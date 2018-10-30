use std::cmp::max;

// TODO: look at these first
use ropey::RopeSlice;
use string_utils::{rope_slice_is_line_ending, rope_slice_is_whitespace};
use utils::{RopeGraphemes,grapheme_width};
use editor::buffer::Buffer;

// line wrap style
#[allow(dead_code)] // haven't implemented NoWrap
pub enum WrapType {
    NoWrap,    // TODO: implement this again
    CharWrap(usize),
    WordWrap(usize),
}

// Maximum graphemes in a line before a soft line break is forced.
// This is necessary to prevent pathological formatting cases which
// could slow down the editor arbitrarily for arbitrarily long
// lines.
pub const LINE_BLOCK_LENGTH: usize = 1 << 12;  // 4096 i.e. 2^12

// ===================================================================
// LineFormatter implementation for terminals/consoles.
// it takes care of converting tabs to spaces, and handling word wrap (if enabled)
// ===================================================================

pub struct LineFormatter {
    pub tab_width: u8,                  // how big are tabs
    pub wrap_type: WrapType,            // do we wrap lines
    pub wrap_additional_indent: usize,  // any extra indentation on wrapped lines
}

impl LineFormatter {

    pub fn new(tab_width: u8) -> LineFormatter {
        LineFormatter {
            tab_width: tab_width,
            wrap_type: WrapType::WordWrap(80),  // a default, really set by set_wrap_width
            wrap_additional_indent: 0,
        }
    }

    // set the value on the wrap_type enums
    pub fn set_wrap_width(&mut self, width: usize) {
        match self.wrap_type {
            WrapType::NoWrap => {}

            WrapType::CharWrap(ref mut w) => {
                *w = width;
            }

            WrapType::WordWrap(ref mut w) => {
                *w = width;
            }
        }
    }

    /// Returns the 2d visual dimensions of the given text when formatted
    /// by the formatter.
    /// The text to be formatted is passed as a grapheme iterator.
    pub fn dimensions<'a, T>(&'a self, g_iter: T) -> (usize, usize)
    where
        T: Iterator<Item = RopeSlice<'a>>,
    {
        let mut maxheight = 0;
        let mut maxwidth = 0;

        for (_g, pos, width, _char_offset) in self.iter(g_iter) {
            maxheight = max(maxheight, pos.0);        // max row
            maxwidth = max(maxwidth, pos.1 + width);  // max col + width
        }

        // we have the max row and col + width
        // to convert to a height we need +=1
        // the width doesn't need that because of the width
        maxheight += 1; // was single_line_height

        return (maxheight,maxwidth);
    }

    /// Converts a char index within a text into a visual 2d position (row,col).
    /// The text to be formatted is passed as a grapheme iterator.
    /// return the (row,col) of pos
    //  if char_idx is out of range, then returns one position beyond end
    pub fn index_to_v2d<'a, T>(&'a self, g_iter: T, char_idx: usize) -> (usize, usize)
    where
        T: Iterator<Item = RopeSlice<'a>>,
    {
        // initialize values so still in scope at end
        let mut pos : (usize,usize) = (0,0);
        let mut width : usize = 0;

        // find the pos for the corresponding char_offset
        for (_g, _pos, _width, char_offset) in self.iter(g_iter) {
            pos = _pos;     
            width = _width;
            // should really be ==
            if char_offset >= char_idx {
                return pos;
            }
        }

        // hit the end
        return (pos.0, pos.1 + width);
    }

    //  TODO: This seems slow
    /// Takes a char index and a visual vertical offset, and returns the char
    /// index after that visual offset is applied.
    pub fn index_offset_vertical_v2d(
        &self,
        buf: &Buffer,
        char_idx: usize,
        offset: isize,
    ) -> usize {
        // TODO: handle rounding modes
        // TODO: do this with bidirectional line iterator

        // Get the line and block index of the given index
        let (mut line_i, col_i) = buf.index_to_line_col(char_idx);

        // Find the right block in the line, and the index within that block
        // let (line_block, col_i_adjusted) = block_index_and_offset(col_i);

        let mut line = buf.get_line(line_i);

        let (mut y, x) = self.index_to_v2d(
            RopeGraphemes::new(&line.slice(..)),
            col_i
        );

        // First, find the right line while keeping track of the vertical offset
        let mut new_y = y as isize + offset;

        loop {
            line = buf.get_line(line_i);
            let (h, _) = self.dimensions(RopeGraphemes::new(&line.slice(..)));

            if new_y >= 0 && new_y < h as isize {
                y = new_y as usize;
                break;
            } else {
                if new_y > 0 {

                    // Check for off-the-end
                    if (line_i + 1) >= buf.line_count() {
                        return buf.char_count();
                    }
                    line_i += 1;
                    new_y -= h as isize;

                } else if new_y < 0 {
                    
                    // Check for off-the-end
                    if line_i == 0 {
                        return 0;
                    }

                    line_i -= 1;
                    line = buf.get_line(line_i);
                    let (h, _) = self.dimensions(RopeGraphemes::new(&line.slice(..)));
                    new_y += h as isize;
                } else {
                    unreachable!();
                }
            }
        }

        // Next, convert the resulting coordinates back into buffer-wide
        // coordinates.
        let col_i = self.v2d_to_index(RopeGraphemes::new(&line.slice(..)), (y, x));

        return buf.line_col_to_index((line_i, col_i));
    }

    /// Takes a char index and a desired visual horizontal position, and
    /// returns an overall char_idx on the same visual line as the given index,
    /// but offset to have the desired horizontal position.
    pub fn index_set_horizontal_v2d(
        &self,
        buf: &Buffer,
        char_idx: usize,
        horizontal: usize
    ) -> usize {

        let (line_i, char_offset) = buf.index_to_line_col(char_idx);
        let line = buf.get_line(line_i);

        // Calculate the horizontal position
        // first find the visual row at char_offset
        let (row, _) = self.index_to_v2d(
            RopeGraphemes::new(&line.slice(..)),
            char_offset
        );
        // and then look up the char_offset at the desired (row,horizontal)
        // within the line
        let mut new_char_offset = self.v2d_to_index(
            RopeGraphemes::new(&line.slice(..)),
            (row, horizontal),
        );

        // Make sure we're not pushing the index off the end of the line
        // if horizontal is too far
        if (line_i + 1) < buf.line_count() && new_char_offset >= line.len_chars() && line.len_chars() > 0
        {
            // clip to last valid character on line
            new_char_offset = line.len_chars() - 1;
        }

        // convert back to a global char_idx
        // new_char_offset - char_offset is the delta on the visual line
        // weird, so the () are necessary to not have underflow in temporary computations
        // in the case we end up at 0, which is a valid usize
        // (8 + 0) - 8 is ok
        // 8 + (0 - 8) underflows on the temporary calculations
        return (char_idx + new_char_offset) - char_offset;
    }

    /// Converts a visual 2d position into a char_offset within a text.
    /// The text to be formatted is passed as a grapheme iterator.
    /// if value doesn't exist, then return value right before
    /// if beyond the end then return one beyond the end
    fn v2d_to_index<'a, T>(
        &'a self,
        g_iter: T,
        v2d: (usize, usize),
    ) -> usize
    where
        T: Iterator<Item = RopeSlice<'a>>,
    {
        let mut char_offset = 0;
        let mut prev_char_offset = 0;
        let mut last_char_cnt = 0;

        for (g, pos, _, _char_offset) in self.iter(g_iter) {
            char_offset = _char_offset;

            // are at or beyond our position
            // found it 
            if pos == v2d {
                return char_offset;
            } else if (pos.0 > v2d.0) || (pos.0 == v2d.0 && pos.1 >= v2d.1) { 
                // beyond what we're looking for, so return previous value
                return prev_char_offset;
            }

            // in case we drop off the end 
            last_char_cnt = g.chars().count();
            // save in case we have to back up
            prev_char_offset = char_offset;
        }

        // character after last in iterator
        return char_offset + last_char_cnt;
    }
    
    /// given on overall char_idx in the buffer, find the col in the 2d (row,col)
    /// position on the line containing char_idx
    pub fn index_to_horizontal_v2d(&self, buf: &Buffer, char_idx: usize) -> usize {

        // get the line index and char_idx offset of the overall char_idx
        let (line_i, col_i) = buf.index_to_line_col(char_idx);
        let line = buf.get_line(line_i);

        let g_iter = RopeGraphemes::new(&line.slice(..));
        return self.index_to_v2d(g_iter, col_i).1;
    }

    // creat iterator to iterate over the graphemes
    // a grapheme cluster is text that should be kept together
    // like a letter and associated accent marks
    pub fn iter<'a, T>(&'a self, g_iter: T) -> LineFormatterVisIter<'a, T>
    where
        T: Iterator<Item = RopeSlice<'a>>,
    {
        LineFormatterVisIter::<'a, T> {
            grapheme_iter: g_iter,
            f: self,        // interesting, contains access to self
            pos: (0, 0),
            char_offset: 0,
            word_buf: Vec::new(),
            word_i: 0,
        }
    }
}


// ===================================================================
// An iterator that iterates over the graphemes in a line in a
// manner consistent with the LineFormatter.
// note: A grapheme is a sequence of one or more code points that are 
// displayed as a single, graphical unit that a reader recognizes as 
// a single element of the writing system. 
// since it is only for a single line, any line ending will only 
// occur at the end
// TODO: should we make a more general iterator after muliple lines?
// ===================================================================
// this keeps the state of the particular iteration
pub struct LineFormatterVisIter<'a, T>
where
    T: Iterator<Item = RopeSlice<'a>>,
{
    grapheme_iter: T,       // iterates over graphemes on line
    f: &'a LineFormatter,   // contains state on how to format lines
    pos: (usize, usize),    // (row,column) of current position, 
                            // updated to next position right before we return
    char_offset : usize,    // offset from first char_idx, so 0 for first grapheme
                            // updated to next position right before we return
                            // note that a graheme may contain various numbers of characters

    word_buf: Vec<RopeSlice<'a>>,   // stores a vector of graphemes for the word
    word_i: usize,                  // next location within word_buf to use
}

impl<'a, T> LineFormatterVisIter<'a, T>
where
    T: Iterator<Item = RopeSlice<'a>>,
{
    // returns the values for next grapheme on line in the case of nowrap
    // so pos is the starting position (row,col) of g, and width is how wide it is
    fn next_nowrap(
        &mut self, 
        g: RopeSlice<'a>
    ) -> Option<(RopeSlice<'a>, (usize, usize), usize, usize)> {

        // width of grapheme, which handles tabs, line endings, and regular characters
        let width = grapheme_vis_width_at_vis_pos(g, self.pos.1, self.f.tab_width as usize);
        // number of characters
        let char_cnt = g.chars().count();

        // save current char_offset and positions to return
        let pos = self.pos;
        let char_offset = self.char_offset;

        // advance to next position by adding width
        self.pos = (self.pos.0, self.pos.1 + width);
        // and next char_offset by adding char_cnt
        self.char_offset += char_cnt;

        return Some((g, pos, width, char_offset));
    }

    // returns the values for next grapheme on line in the case of character wrap
    // also used by word wrap
    fn next_charwrap(
        &mut self,
        g: RopeSlice<'a>,
        wrap_width: usize,
    ) -> Option<(RopeSlice<'a>, (usize, usize), usize, usize)> {

        // get width
        let width = grapheme_vis_width_at_vis_pos(g, self.pos.1, self.f.tab_width as usize);
        // number of characters
        let char_cnt = g.chars().count();
        // save char_offset to return
        let char_offset = self.char_offset;
        // update char_offset by adding char_cnt
        self.char_offset += char_cnt;

        // if starting position of following character is too wide need to wrap
        if (self.pos.1 + width) > wrap_width {
            // remember current position to return
            let pos = (
                // start a new line with just the additional indent for col
                self.pos.0 + 1,  // single_line_height
                self.f.wrap_additional_indent,  
            );
            // advance to next position by adding width
            self.pos = (
                self.pos.0 + 1,  // single_line_height
                self.f.wrap_additional_indent + width,
            );

            return Some((g, pos, width, char_offset));
        } else {

            // normal return stuff
            // remember current position to return
            let pos = self.pos;
            // advance to next position by adding width
            self.pos = (self.pos.0, self.pos.1 + width);
    
            return Some((g, pos, width, char_offset));
        }
    }
}

impl<'a, T> Iterator for LineFormatterVisIter<'a, T>
where
    T: Iterator<Item = RopeSlice<'a>>,
{
    // (grapheme, (row,col), width)
    type Item = (RopeSlice<'a>, (usize, usize), usize, usize);

    fn next(&mut self) -> Option<(RopeSlice<'a>, (usize, usize), usize, usize)> {
        match self.f.wrap_type {

            WrapType::NoWrap => {
                if let Some(g) = self.grapheme_iter.next() {
                    return self.next_nowrap(g);
                } else {
                    return None;
                }
            }

            WrapType::CharWrap(wrap_width) => {
                if let Some(g) = self.grapheme_iter.next() {
                    return self.next_charwrap(g, wrap_width);
                } else {
                    return None;
                }
            }

            WrapType::WordWrap(wrap_width) => {
                // Get next word if necessary
                // includes one whitespace on end
                if self.word_i >= self.word_buf.len() {
                    let mut word_width = 0;   // total width of all word graphemes
                    self.word_buf.truncate(0);
                    while let Some(g) = self.grapheme_iter.next() {
                        self.word_buf.push(g);
                        let width = grapheme_vis_width_at_vis_pos(
                            g,
                            self.pos.1 + word_width, // on next line
                            self.f.tab_width as usize,
                        );
                        word_width += width;
                        if rope_slice_is_whitespace(&g) {
                            break;
                        }
                    }

                    // no word left, so we're done
                    if self.word_buf.len() == 0 {
                        return None;
                    }

                    // Move to next line if necessary if new word doesn't fit
                    if (self.pos.1 + word_width) > wrap_width {
                        if self.pos.1 > 0 {
                            self.pos = (
                                self.pos.0 + 1,  // single_line_height
                                self.f.wrap_additional_indent,
                            );
                        }
                    }

                    // haven't used up any graphemes from word yet
                    self.word_i = 0;
                }
                // have someething left in word_buf to read here

                // Iterate over the word
                let g = self.word_buf[self.word_i];
                self.word_i += 1;
                return self.next_charwrap(g, wrap_width);
            }
        }
    }
}

// ===================================================================
// Helper function
// ===================================================================

/// Returns the visual width of a grapheme given a starting
/// displayed column pos on a line.
/// Need pos and tab_width only in the case of a tab
fn grapheme_vis_width_at_vis_pos(g: RopeSlice, pos: usize, tab_width: usize) -> usize {
    if g == "\t" {
        // put next character on next multiple of tab_width
        let ending_pos = ((pos / tab_width) + 1) * tab_width;
        return ending_pos - pos;  // width is how far we went
    } else if rope_slice_is_line_ending(&g) {
        return 1; // line endings are a single character width, even if multiple chars
    } else {
        return grapheme_width(&g); // otherwise just the width of the grapheme, which is usually 0, 1, or 2
    }
}

#[cfg(test)]
mod tests {
    // #![allow(unused_imports)]
    use super::*;
    use editor::buffer::Buffer;
    // TODO: test Ceiling, Round
    // use self::RoundingBehavior::{Floor};
    use self::LineFormatter;
    use ropey::Rope;
    use utils::RopeGraphemes;

    #[test]
    fn grapheme_vis_width_at_vis_pos_1() {
        // say we have a tab width of 4
        let tab_width = 4;
        // a tab should line next character up with next available 0
        // 0123012301230123012301230123012301230123
        // He  ll      o th    ere, st ranger!
        //    2   2   4    4          1   <- tab width
        let text = Rope::from_str("He\tll\t\to th\tere, st\t税\n"); 
        let iter = RopeGraphemes::new(&text.slice(..));  // grapheme iterator
        let mut pos = 0;  // current pos

        let correct_widths = vec![  1,  // H 
                                    1,  // e 
                                    2,  // \t
                                    1,  // l
                                    1,  // l
                                    2,  // \t
                                    4,  // \t
                                    1,  // o
                                    1,  // space
                                    1,  // t
                                    1,  // h
                                    4,  // \t
                                    1,  // e
                                    1,  // r
                                    1,  // e
                                    1,  // ,
                                    1,  // space
                                    1,  // s
                                    1,  // t
                                    1,  // \t
                                    2,  // 税  width 2
                                    1];  // \n  end of line is width 1

        // note the the position is horizontal on the line, not char_idx
        for (i,g) in iter.enumerate() {
            let mut width = grapheme_vis_width_at_vis_pos(g,pos,tab_width);
            assert_eq!(width,correct_widths[i], "{},{}", i, g);  // H
            pos += width; // need to add width as we go go get pos
        }
    }

    #[test]
    fn dimensions_1() {
        // 123456789012345678901234567890
        // Hello there, stranger!
        let text = Rope::from_str("Hello there, stranger!"); // 22 graphemes long

        let f = LineFormatter {
            tab_width : 4,
            wrap_type : WrapType::CharWrap(80),
            wrap_additional_indent : 0,
        };

        assert_eq!(f.dimensions(RopeGraphemes::new(&text.slice(..))), (1, 22));
    }

    #[test]
    fn dimensions_2() {
        let text = Rope::from_str(
            "Hello there,\
             stranger!  H\
             ow are you d\
             oing this fi\
             ne day?");    // 56 graphemes long

        let f = LineFormatter {
            tab_width : 4,
            wrap_type : WrapType::CharWrap(12),
            wrap_additional_indent : 0,
        };

        // get 5 rows and 12 columns
        assert_eq!(f.dimensions(RopeGraphemes::new(&text.slice(..))), (5, 12));
    }

    #[test]
    fn dimensions_3() {
        // 55 graphemes long
        let text = Rope::from_str(
            "税マイミ文末\
             レ日題イぽじ\
             や男目統ス公\
             身みトしつ結\
             煮ヱマレ断西\
             ロ領視りいぽ\
             凱字テ式重反\
             てす献罪がご\
             く官俵呉嫁ー\
             。",
        );

       let f = LineFormatter {
            tab_width : 4,
            wrap_type : WrapType::CharWrap(12),
            wrap_additional_indent : 0,
        };

        assert_eq!(f.dimensions(RopeGraphemes::new(&text.slice(..))), (10, 12));
    }

    #[test]
    fn dimensions_4() {
        // 55 graphemes long
        let text = Rope::from_str(
            "税マイミ文末\
             レ日題イぽじ\
             や男目統ス公\
             身みトしつ結\
             煮ヱマレ断西\
             ロ領視りいぽ\
             凱字テ式重反\
             てす献罪がご\
             く官俵呉嫁ー\
             。",
        );

        // WordWrap this time
        let f = LineFormatter {
            tab_width : 4,
            wrap_type : WrapType::WordWrap(12),
            wrap_additional_indent : 0,
        };

        assert_eq!(f.dimensions(RopeGraphemes::new(&text.slice(..))), (10, 12));
    }

    #[test]
    fn dimensions_5() {
        let text = Rope::from_str(
            "Hello \
            there, \
            stranger! \
            How are you \
            doing this \
            fine day?");    // 56 graphemes long

        // WordWrap this time
        let f = LineFormatter {
            tab_width : 4,
            wrap_type : WrapType::WordWrap(12),
            wrap_additional_indent : 0,
        };

        // // this prints out the solution with word wraps
        // let g_iter = RopeGraphemes::new(&text.slice(..));
        // for (g, pos, width, char_offset) in f.iter(g_iter) {
        //     println!("{},{:?},{},{}", g, pos, width, char_offset);
        // }

        // note that dimensions could potentially give less than WordWrap width
        assert_eq!(f.dimensions(RopeGraphemes::new(&text.slice(..))), (6, 12));
    }

    #[test]
    fn dimensions_6() {
        // what do we get for a width with 10 letter words
        // and a 15 character wraps
        let text = Rope::from_str(
            "0123456789 \
             0123456789 \
             0123456789 \
             0123456789");

        // WordWrap this time
        let f = LineFormatter {
            tab_width : 4,
            wrap_type : WrapType::WordWrap(15),
            wrap_additional_indent : 0,
        };

        // this prints out the solution with word wraps
        // let mut g_iter = RopeGraphemes::new(&text.slice(..));
        // for (g, pos, width) in f.iter(g_iter) {
        //     println!("{},{:?},{}", g, pos, width);
        // }

        // note that dimensions could potentially give less than WordWrap width
        assert_eq!(f.dimensions(RopeGraphemes::new(&text.slice(..))), (4, 11));
    }

    #[test]
    fn dimensions_7() {
        // easy case is a multiple of 2 so fits double width
        let text = Rope::from_str(
            "税マイミ\
             文末レ日\
             題イぽじ\
             や男目統\
             。",
        );

        // WordWrap this time
        let f = LineFormatter {
            tab_width : 4,
            wrap_type : WrapType::CharWrap(8),
            wrap_additional_indent : 0,
        };

        assert_eq!(f.dimensions(RopeGraphemes::new(&text.slice(..))), (5, 8));
    }

    #[test]
    fn dimensions_8() {
        // check what happens with odd wrap width and double width char
        // should only fit 3 onto a line
        let text = Rope::from_str(
            "税マイ\
             ミ文末\
             レ日題\
             イぽじ\
             や男目\
             統。",
        );

        // WordWrap this time
        let f = LineFormatter {
            tab_width : 4,
            wrap_type : WrapType::CharWrap(7),
            wrap_additional_indent : 0,
        };

        assert_eq!(f.dimensions(RopeGraphemes::new(&text.slice(..))), (6, 6));
    }

    #[test]
    fn dimensions_9() {
        // check what happens with odd wrap width and double width char
        // should only fit 3 onto a line
        let text = Rope::from_str(
            "税マイ\
             ミ文末\
             レ日題\
             イぽじ\
             や男目\
             統。",
        );

        // WordWrap this time
        let f = LineFormatter {
            tab_width : 4,
            wrap_type : WrapType::CharWrap(7),
            wrap_additional_indent : 1,
        };

        // let g_iter = RopeGraphemes::new(&text.slice(..));

        // for (g, pos, width) in f.iter(g_iter) {
        //     println!("{},{:?},{}", g, pos, width);
        // }

        // now uses 7 cols for rows beyond the first
        // from the space at the start of the line
        assert_eq!(f.dimensions(RopeGraphemes::new(&text.slice(..))), (6, 7));
    }


    #[test]
    fn index_to_v2d_1() {
        let text = Rope::from_str("Hello there, stranger!"); // 22 graphemes long

        let f = LineFormatter {
            tab_width: 4,
            wrap_type: WrapType::CharWrap(80),
            wrap_additional_indent: 0,
        };

        // note: each call consumes the grapheme iterator
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 0),(0, 0));
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 5),(0, 5));
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 21),(0, 21));
        // beyond end get one beyone end
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 22),(0, 22));
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 23),(0, 22));  
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 1000),(0, 22));
    }

    #[test]
    fn index_to_v2d_2() {
                                 // cols 0 to 11 are valid
                                 //012345678901
        let text = Rope::from_str("Hello there,\
                                    stranger!  \
                                   How are you \
                                   doing this f\
                                   ine day?"); // 56 graphemes long

       let f = LineFormatter {
            tab_width: 4,
            wrap_type: WrapType::CharWrap(12),
            wrap_additional_indent: 0,
        };

        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 0),(0, 0));
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 5),(0, 5));
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 11),(0, 11));

        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 12),(1, 0));
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 15),(1, 3));
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 23),(1, 11));

        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 24),(2, 0));
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 28),(2, 4));
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 35),(2, 11));

        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 36),(3, 0));
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 43),(3, 7));
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 47),(3, 11));

        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 48),(4, 0)); // i
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 49),(4, 1)); // n
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 50),(4, 2)); // e
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 51),(4, 3)); //  
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 52),(4, 4)); // d 
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 53),(4, 5)); // a
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 54),(4, 6)); // y
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 55),(4, 7)); // ?

        // here beyond the end gives you last valid value
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 56),(4, 7)); // beyond gets last
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 57),(4, 7));
        assert_eq!(f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 1000),(4, 7));
    }

    #[test]
    fn v2d_to_index_1() {
        let text = Rope::from_str("Hello there, stranger!"); // 22 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.wrap_additional_indent = 0;
        f.set_wrap_width(80);

        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 0)),0);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 5)),5);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 22)),22);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 23)),22);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (1, 0)),22);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (1, 1)),22);
    }

    #[test]
    fn v2d_to_index_2() {
        let text = Rope::from_str("Hello there, stranger!  How are you doing this fine day?"); // 56 graphemes long

       let f = LineFormatter {
            tab_width: 4,
            wrap_type: WrapType::CharWrap(12),
            wrap_additional_indent: 0,
        };

        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 0)),0);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 11)),11);
        // if col is beyond the end of the row, return previous valid index
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 12)),11);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 1000)),11);

        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (1, 0)),12);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (1, 11)),23);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (1, 12)),23);

        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (2, 0)),24);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (2, 11)),35);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (2, 12)),35);

        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (3, 0)),36);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (3, 11)),47);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (3, 12)),47);

        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (4, 0)),48);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (4, 7)),55); // last valid index
         
        // beyond end get one beyond last valid index
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (4, 8)),56);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (4, 9)),56);
        assert_eq!(f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (1000, 1000)),56);
    }

    #[test]
    fn index_to_horizontal_v2d_1() {
        let b = Buffer::new_from_str("Hello there, stranger!\nHow are you doing this fine day?"); // 55 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.wrap_additional_indent = 0;
        f.set_wrap_width(80);

        assert_eq!(f.index_to_horizontal_v2d(&b, 0), 0);
        assert_eq!(f.index_to_horizontal_v2d(&b, 5), 5);
        assert_eq!(f.index_to_horizontal_v2d(&b, 26), 3);
        assert_eq!(f.index_to_horizontal_v2d(&b, 55), 32);
        assert_eq!(f.index_to_horizontal_v2d(&b, 56), 32);
    }

    #[test]
    fn index_to_horizontal_v2d_2() {
        let b = Buffer::new_from_str("Hello there, stranger!\nHow are you doing this fine day?"); // 55 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.wrap_additional_indent = 0;
        f.set_wrap_width(12);

        assert_eq!(f.index_to_horizontal_v2d(&b, 0), 0);
        assert_eq!(f.index_to_horizontal_v2d(&b, 11), 11);

        assert_eq!(f.index_to_horizontal_v2d(&b, 12), 0);
        assert_eq!(f.index_to_horizontal_v2d(&b, 22), 10);

        assert_eq!(f.index_to_horizontal_v2d(&b, 23), 0);
        assert_eq!(f.index_to_horizontal_v2d(&b, 34), 11);

        assert_eq!(f.index_to_horizontal_v2d(&b, 35), 0);
        assert_eq!(f.index_to_horizontal_v2d(&b, 46), 11);

        assert_eq!(f.index_to_horizontal_v2d(&b, 47), 0);
        assert_eq!(f.index_to_horizontal_v2d(&b, 55), 8);
        assert_eq!(f.index_to_horizontal_v2d(&b, 56), 8);
    }

    #[test]
    fn index_set_horizontal_v2d_1() {
        let b = Buffer::new_from_str("Hello there, stranger!\nHow are you doing this fine day?"); // 55 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.wrap_additional_indent = 0;
        f.set_wrap_width(80);

        assert_eq!(f.index_set_horizontal_v2d(&b, 0, 0), 0);
        assert_eq!(f.index_set_horizontal_v2d(&b, 0, 22), 22);
        assert_eq!(f.index_set_horizontal_v2d(&b, 0, 23), 22);

        assert_eq!(f.index_set_horizontal_v2d(&b, 8, 0), 0);
        assert_eq!(f.index_set_horizontal_v2d(&b, 8, 22), 22);
        assert_eq!(f.index_set_horizontal_v2d(&b, 8, 23), 22);

        assert_eq!(f.index_set_horizontal_v2d(&b, 22, 0), 0);
        assert_eq!(f.index_set_horizontal_v2d(&b, 22, 22), 22);
        assert_eq!(f.index_set_horizontal_v2d(&b, 22, 23), 22);

        assert_eq!(f.index_set_horizontal_v2d(&b, 23, 0), 23);
        assert_eq!(f.index_set_horizontal_v2d(&b, 23, 32), 55);
        assert_eq!(f.index_set_horizontal_v2d(&b, 23, 33), 55);

        assert_eq!(f.index_set_horizontal_v2d(&b, 28, 0), 23);
        assert_eq!(f.index_set_horizontal_v2d(&b, 28, 32), 55);
        assert_eq!(f.index_set_horizontal_v2d(&b, 28, 33), 55);

        assert_eq!(f.index_set_horizontal_v2d(&b, 55, 0), 23);
        assert_eq!(f.index_set_horizontal_v2d(&b, 55, 32), 55);
        assert_eq!(f.index_set_horizontal_v2d(&b, 55, 33), 55);
    }

    #[test]
    fn index_set_horizontal_v2d_2() {
        let b = Buffer::new_from_str("Hello there, stranger! How are you doing this fine day?"); // 55 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.wrap_additional_indent = 0;
        f.set_wrap_width(12);

        assert_eq!(f.index_set_horizontal_v2d(&b, 0, 0), 0);
        assert_eq!(f.index_set_horizontal_v2d(&b, 0, 11), 11);
        // if col is beyond length of col, then return last valid value
        assert_eq!(f.index_set_horizontal_v2d(&b, 0, 12), 11);

        assert_eq!(f.index_set_horizontal_v2d(&b, 8, 0), 0);
        assert_eq!(f.index_set_horizontal_v2d(&b, 8, 11), 11);
        assert_eq!(f.index_set_horizontal_v2d(&b, 8, 12), 11);

        assert_eq!(f.index_set_horizontal_v2d(&b, 11, 0), 0);
        assert_eq!(f.index_set_horizontal_v2d(&b, 11, 11), 11);
        assert_eq!(f.index_set_horizontal_v2d(&b, 11, 12), 11);

        assert_eq!(f.index_set_horizontal_v2d(&b, 12, 0), 12);
        assert_eq!(f.index_set_horizontal_v2d(&b, 12, 11), 23);
        assert_eq!(f.index_set_horizontal_v2d(&b, 12, 12), 23);

        assert_eq!(f.index_set_horizontal_v2d(&b, 17, 0), 12);
        assert_eq!(f.index_set_horizontal_v2d(&b, 17, 11), 23);
        assert_eq!(f.index_set_horizontal_v2d(&b, 17, 12), 23);

        assert_eq!(f.index_set_horizontal_v2d(&b, 23, 0), 12);
        assert_eq!(f.index_set_horizontal_v2d(&b, 23, 11), 23);
        assert_eq!(f.index_set_horizontal_v2d(&b, 23, 12), 23);
    }

    #[test]
    fn index_offset_vertical_v2d_1() {
        let b = Buffer::new_from_str("Hello there, stranger!\nHow are you doing this fine day?"); // 55 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.wrap_additional_indent = 0;
        f.set_wrap_width(80);

        assert_eq!(f.index_offset_vertical_v2d(&b, 0, 0), 0);
        assert_eq!(f.index_offset_vertical_v2d(&b, 0, 1), 23);
        assert_eq!(f.index_offset_vertical_v2d(&b, 23, -1), 0);

        assert_eq!(f.index_offset_vertical_v2d(&b, 2, 0), 2);
        assert_eq!(f.index_offset_vertical_v2d(&b, 2, 1), 25);
        assert_eq!(f.index_offset_vertical_v2d(&b, 25, -1), 2);

        assert_eq!(f.index_offset_vertical_v2d(&b, 22, 0), 22);
        assert_eq!(f.index_offset_vertical_v2d(&b, 22, 1), 45);
        assert_eq!(f.index_offset_vertical_v2d(&b, 45, -1), 22);

        assert_eq!(f.index_offset_vertical_v2d(&b, 54, 0), 54);
        assert_eq!(f.index_offset_vertical_v2d(&b, 54, 1), 55);
        assert_eq!(f.index_offset_vertical_v2d(&b, 54, -1), 22);
    }

    #[test]
    fn index_offset_vertical_v2d_2() {
        let b = Buffer::new_from_str("Hello there, stranger! How are you doing this fine day?"); // 55 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.wrap_additional_indent = 0;
        f.set_wrap_width(12);

        assert_eq!(f.index_offset_vertical_v2d(&b, 0, 0), 0);
        assert_eq!(f.index_offset_vertical_v2d(&b, 0, 1), 12);
        assert_eq!(f.index_offset_vertical_v2d(&b, 0, 2), 24);

        assert_eq!(f.index_offset_vertical_v2d(&b, 0, 0), 0);
        assert_eq!(f.index_offset_vertical_v2d(&b, 12, -1), 0);
        assert_eq!(f.index_offset_vertical_v2d(&b, 24, -2), 0);

        assert_eq!(f.index_offset_vertical_v2d(&b, 4, 0), 4);
        assert_eq!(f.index_offset_vertical_v2d(&b, 4, 1), 16);
        assert_eq!(f.index_offset_vertical_v2d(&b, 4, 2), 28);

        assert_eq!(f.index_offset_vertical_v2d(&b, 4, 0), 4);
        assert_eq!(f.index_offset_vertical_v2d(&b, 16, -1), 4);
        assert_eq!(f.index_offset_vertical_v2d(&b, 28, -2), 4);

        assert_eq!(f.index_offset_vertical_v2d(&b, 11, 0), 11);
        assert_eq!(f.index_offset_vertical_v2d(&b, 11, 1), 23);
        assert_eq!(f.index_offset_vertical_v2d(&b, 11, 2), 35);

        assert_eq!(f.index_offset_vertical_v2d(&b, 11, 0), 11);
        assert_eq!(f.index_offset_vertical_v2d(&b, 23, -1), 11);
        assert_eq!(f.index_offset_vertical_v2d(&b, 35, -2), 11);
    }
}
