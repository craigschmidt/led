use std::cmp::{min,max};

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

#[allow(dead_code)] // haven't implemented Ceiling
#[derive(Copy, Clone, PartialEq)]
pub enum RoundingBehavior {
    Round,
    Floor,
    Ceiling,
}

// ===================================================================
// LineFormatter implementation for terminals/consoles.
// it takes care of converting tabs to spaces, and handling word wrap (if enabled)
// ===================================================================

pub struct LineFormatter {
    pub tab_width: u8,                  // how big are tabs
    pub wrap_type: WrapType,            // do we wrap lines
    pub maintain_indent: bool,          // TODO: what's this do
    pub wrap_additional_indent: usize,  // any extra indentation on wrapped lines
}

impl LineFormatter {

    pub fn new(tab_width: u8) -> LineFormatter {
        LineFormatter {
            tab_width: tab_width,
            wrap_type: WrapType::WordWrap(80),  // a default, really set by set_wrap_width
            maintain_indent: true,
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

        for (_g, pos, width) in self.iter(g_iter) {

            maxheight = max(maxheight, pos.0);        // max row
            maxwidth = max(maxwidth, pos.1 + width);  // max col + width
        }

        // we have the max row and col + width
        // to convert to a height we need +=1
        // the width doesn't need that because of the width
        // TODO: is this valid for multiwidth at end
        maxheight += 1; // was single_line_height

        return (maxheight,maxwidth);
    }

    /// Converts a char index within a text into a visual 2d position.
    /// The text to be formatted is passed as a grapheme iterator.
    pub fn index_to_v2d<'a, T>(&'a self, g_iter: T, char_idx: usize) -> (usize, usize)
    where
        T: Iterator<Item = RopeSlice<'a>>,
    {
        let mut pos = (0, 0);
        let mut i = 0;
        let mut last_width = 0;

        for (g, _pos, width) in self.iter(g_iter) {
            pos = _pos;
            last_width = width;
            i += g.chars().count();

            if i > char_idx {
                return pos;
            }
        }

        return (pos.0, pos.1 + last_width);
    }

    /// Takes a char index and a visual vertical offset, and returns the char
    /// index after that visual offset is applied.
    pub fn index_offset_vertical_v2d(
        &self,
        buf: &Buffer,
        char_idx: usize,
        offset: isize,
        rounding: (RoundingBehavior, RoundingBehavior),
    ) -> usize {
        // TODO: handle rounding modes
        // TODO: do this with bidirectional line iterator

        // Get the line and block index of the given index
        let (mut line_i, mut col_i) = buf.index_to_line_col(char_idx);

        // Find the right block in the line, and the index within that block
        let (line_block, col_i_adjusted) = block_index_and_offset(col_i);

        let mut line = buf.get_line(line_i);
        let (mut y, x) = self.index_to_v2d(
            RopeGraphemes::new(&line.slice(
                (line_block * LINE_BLOCK_LENGTH)
                    ..min(line.len_chars(), (line_block + 1) * LINE_BLOCK_LENGTH),
            )),
            col_i_adjusted,
        );

        // First, find the right line while keeping track of the vertical offset
        let mut new_y = y as isize + offset;

        let mut block_index: usize = line_block;
        loop {
            line = buf.get_line(line_i);
            let (h, _) = self.dimensions(RopeGraphemes::new(&line.slice(
                (block_index * LINE_BLOCK_LENGTH)
                    ..min(line.len_chars(), (block_index + 1) * LINE_BLOCK_LENGTH),
            )));

            if new_y >= 0 && new_y < h as isize {
                y = new_y as usize;
                break;
            } else {
                if new_y > 0 {
                    let is_last_block = block_index >= last_block_index(line.len_chars());

                    // Check for off-the-end
                    if is_last_block && (line_i + 1) >= buf.line_count() {
                        return buf.char_count();
                    }

                    if is_last_block {
                    line_i += 1;
                        block_index = 0;
                    } else {
                        block_index += 1;
                    }
                    new_y -= h as isize;
                } else if new_y < 0 {
                    // Check for off-the-end
                    if block_index == 0 && line_i == 0 {
                        return 0;
                    }

                    if block_index == 0 {
                    line_i -= 1;
                    line = buf.get_line(line_i);
                        block_index = last_block_index(line.len_chars());
                    } else {
                        block_index -= 1;
                    }
                    let (h, _) = self.dimensions(RopeGraphemes::new(&line.slice(
                        (block_index * LINE_BLOCK_LENGTH)
                            ..min(line.len_chars(), (block_index + 1) * LINE_BLOCK_LENGTH),
                    )));
                    new_y += h as isize;
                } else {
                    unreachable!();
                }
            }
        }

        // Next, convert the resulting coordinates back into buffer-wide
        // coordinates.
        let block_slice = line.slice(
            (block_index * LINE_BLOCK_LENGTH)
                ..min(line.len_chars(), (block_index + 1) * LINE_BLOCK_LENGTH),
        );
        let block_col_i = min(
            self.v2d_to_index(RopeGraphemes::new(&block_slice), (y, x), rounding),
            LINE_BLOCK_LENGTH - 1,
        );
        col_i = (block_index * LINE_BLOCK_LENGTH) + block_col_i;

        return buf.line_col_to_index((line_i, col_i));
    }

    /// Takes a char index and a desired visual horizontal position, and
    /// returns a char index on the same visual line as the given index,
    /// but offset to have the desired horizontal position.
    pub fn index_set_horizontal_v2d(
        &self,
        buf: &Buffer,
        char_idx: usize,
        horizontal: usize,
        rounding: RoundingBehavior,
    ) -> usize {
        let (line_i, col_i) = buf.index_to_line_col(char_idx);
        let line = buf.get_line(line_i);

        // Find the right block in the line, and the index within that block
        let (line_block, col_i_adjusted) = block_index_and_offset(col_i);
        let start_index = line_block * LINE_BLOCK_LENGTH;
        let end_index = min(line.len_chars(), start_index + LINE_BLOCK_LENGTH);

        // Calculate the horizontal position
        let (v, _) = self.index_to_v2d(
            RopeGraphemes::new(&line.slice(start_index..end_index)),
            col_i_adjusted,
        );
        let block_col_i = self.v2d_to_index(
            RopeGraphemes::new(&line.slice(start_index..end_index)),
            (v, horizontal),
            (RoundingBehavior::Floor, rounding),
        );
        let mut new_col_i = start_index + min(block_col_i, LINE_BLOCK_LENGTH - 1);

        // Make sure we're not pushing the index off the end of the line
        if (line_i + 1) < buf.line_count() && new_col_i >= line.len_chars() && line.len_chars() > 0
        {
            new_col_i = line.len_chars() - 1;
        }

        return (char_idx + new_col_i) - col_i;
    }

    /// Converts a visual 2d position into a char index within a text.
    /// The text to be formatted is passed as a grapheme iterator.
    fn v2d_to_index<'a, T>(
        &'a self,
        g_iter: T,
        v2d: (usize, usize),
        _ : (RoundingBehavior, RoundingBehavior),
    ) -> usize
    where
        T: Iterator<Item = RopeSlice<'a>>,
    {
        // TODO: handle rounding modes
        let mut prev_i = 0;
        let mut i = 0;

        for (g, pos, _) in self.iter(g_iter) {
            if pos.0 > v2d.0 {
                i = prev_i;
                break;
            } else if pos.0 == v2d.0 && pos.1 >= v2d.1 {
                break;
            }

            prev_i = i;
            i += g.chars().count();
        }

        return i;
    }
    
    pub fn index_to_horizontal_v2d(&self, buf: &Buffer, char_idx: usize) -> usize {
        let (line_i, col_i) = buf.index_to_line_col(char_idx);
        let line = buf.get_line(line_i);

        // Find the right block in the line, and the index within that block
        let (line_block, col_i_adjusted) = block_index_and_offset(col_i);

        // Get an iter into the right block
        let a = line_block * LINE_BLOCK_LENGTH;
        let b = min(line.len_chars(), (line_block + 1) * LINE_BLOCK_LENGTH);
        let g_iter = RopeGraphemes::new(&line.slice(a..b));
        return self.index_to_v2d(g_iter, col_i_adjusted).1;
    }

    // to iterate over the graphemes
    // a grapheme cluster is text that should be kept together
    // like a letter and associated accent marks
    pub fn iter<'a, T>(&'a self, g_iter: T) -> LineFormatterVisIter<'a, T>
    where
        T: Iterator<Item = RopeSlice<'a>>,
    {
        LineFormatterVisIter::<'a, T> {
            grapheme_iter: g_iter,
            f: self,
            pos: (0, 0),
            indent: 0,
            indent_found: false,
            word_buf: Vec::new(),
            word_i: 0,
        }
    }
}

pub fn block_index_and_offset(index: usize) -> (usize, usize) {
    (index / LINE_BLOCK_LENGTH, index % LINE_BLOCK_LENGTH)
}

pub fn last_block_index(gc: usize) -> usize {
    let mut block_count = gc / LINE_BLOCK_LENGTH;
    if (gc % LINE_BLOCK_LENGTH) > 0 {
        block_count += 1;
    }

    if block_count > 0 {
        return block_count - 1;
    } else {
        return 0;
    }
}


// ===================================================================
// An iterator that iterates over the graphemes in a line in a
// manner consistent with the ConsoleFormatter.
// note: A grapheme is a sequence of one or more code points that are 
// displayed as a single, graphical unit that a reader recognizes as 
// a single element of the writing system. 
// ===================================================================
pub struct LineFormatterVisIter<'a, T>
where
    T: Iterator<Item = RopeSlice<'a>>,
{
    grapheme_iter: T,
    f: &'a LineFormatter,
    pos: (usize, usize),  // row and column of slice

    indent: usize,
    indent_found: bool,

    word_buf: Vec<RopeSlice<'a>>,
    word_i: usize,
}

impl<'a, T> LineFormatterVisIter<'a, T>
where
    T: Iterator<Item = RopeSlice<'a>>,
{
    fn next_nowrap(&mut self, g: RopeSlice<'a>) -> Option<(RopeSlice<'a>, (usize, usize), usize)> {
        let width = grapheme_vis_width_at_vis_pos(g, self.pos.1, self.f.tab_width as usize);

        let pos = self.pos;
        self.pos = (self.pos.0, self.pos.1 + width);
        return Some((g, pos, width));
    }

    fn next_charwrap(
        &mut self,
        g: RopeSlice<'a>,
        wrap_width: usize,
    ) -> Option<(RopeSlice<'a>, (usize, usize), usize)> {
        let width = grapheme_vis_width_at_vis_pos(g, self.pos.1, self.f.tab_width as usize);

        if (self.pos.1 + width) > wrap_width {
            if !self.indent_found {
                self.indent = 0;
                self.indent_found = true;
            }

            if self.f.maintain_indent {
                let pos = (
                    self.pos.0 + 1,  // single_line_height
                    self.indent + self.f.wrap_additional_indent,
                );
                self.pos = (
                    self.pos.0 + 1, // single_line_height
                    self.indent + self.f.wrap_additional_indent + width,
                );
                return Some((g, pos, width));
            } else {
                let pos = (
                    self.pos.0 + 1, // single_line_height
                    self.f.wrap_additional_indent,
                );
                self.pos = (
                    self.pos.0 + 1,  // single_line_height
                    self.f.wrap_additional_indent + width,
                );
                return Some((g, pos, width));
            }
        } else {
            if !self.indent_found {
                if rope_slice_is_whitespace(&g) {
                    self.indent += width;
                } else {
                    self.indent_found = true;
                }
            }

            let pos = self.pos;
            self.pos = (self.pos.0, self.pos.1 + width);
            return Some((g, pos, width));
        }
    }
}

impl<'a, T> Iterator for LineFormatterVisIter<'a, T>
where
    T: Iterator<Item = RopeSlice<'a>>,
{
    type Item = (RopeSlice<'a>, (usize, usize), usize);

    fn next(&mut self) -> Option<(RopeSlice<'a>, (usize, usize), usize)> {
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
                if self.word_i >= self.word_buf.len() {
                    let mut word_width = 0;
                    self.word_buf.truncate(0);
                    while let Some(g) = self.grapheme_iter.next() {
                        self.word_buf.push(g);
                        let width = grapheme_vis_width_at_vis_pos(
                            g,
                            self.pos.1 + word_width,
                            self.f.tab_width as usize,
                        );
                        word_width += width;
                        if rope_slice_is_whitespace(&g) {
                            break;
                        }
                    }

                    if self.word_buf.len() == 0 {
                        return None;
                    } else if !self.indent_found && !rope_slice_is_whitespace(&self.word_buf[0]) {
                        self.indent_found = true;
                    }

                    // Move to next line if necessary
                    if (self.pos.1 + word_width) > wrap_width {
                        if !self.indent_found {
                            self.indent = 0;
                            self.indent_found = true;
                        }

                        if self.pos.1 > 0 {
                            if self.f.maintain_indent {
                                self.pos = (
                                    self.pos.0 + 1,   // single_line_height
                                    self.indent + self.f.wrap_additional_indent,
                                );
                            } else {
                                self.pos = (
                                    self.pos.0 + 1,  // single_line_height
                                    self.f.wrap_additional_indent,
                                );
                            }
                        }
                    }

                    self.word_i = 0;
                }

                // Iterate over the word
                let g = self.word_buf[self.word_i];
                self.word_i += 1;
                return self.next_charwrap(g, wrap_width);
            }
        }
    }
}

// ===================================================================
// Helper functions
// ===================================================================

/// Returns the visual width of a grapheme given a starting
/// position on a line.
fn grapheme_vis_width_at_vis_pos(g: RopeSlice, pos: usize, tab_width: usize) -> usize {
    if g == "\t" {
        let ending_pos = ((pos / tab_width) + 1) * tab_width;
        return ending_pos - pos;  // width is how far we went
    } else if rope_slice_is_line_ending(&g) {
        return 1;
    } else {
        return grapheme_width(&g);
    }
}

#[cfg(test)]
mod tests {
    // #![allow(unused_imports)]
    use super::*;
    use editor::buffer::Buffer;
    // TODO: test Ceiling, Round
    use self::RoundingBehavior::{Floor};
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
            maintain_indent  : false, 
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
            maintain_indent  : false, 
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
            maintain_indent  : false, 
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
            maintain_indent  : false, 
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
            maintain_indent  : false, 
            wrap_additional_indent : 0,
        };

        // this prints out the solution with word wraps
        let g_iter = RopeGraphemes::new(&text.slice(..));
        for (g, pos, width) in f.iter(g_iter) {
            println!("{},{:?},{}", g, pos, width);
        }

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
            maintain_indent  : false, 
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
    fn index_to_v2d_1() {
        let text = Rope::from_str("Hello there, stranger!"); // 22 graphemes long

        let f = LineFormatter {
            tab_width: 4,
            wrap_type: WrapType::CharWrap(80),
            maintain_indent: false,
            wrap_additional_indent: 0,
        };

        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 0),
            (0, 0)
        );
        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 5),
            (0, 5)
        );
        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 22),
            (0, 22)
        );
        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 23),
            (0, 22)
        );
    }

    #[test]
    fn index_to_v2d_2() {
        let text = Rope::from_str("Hello there, stranger!  How are you doing this fine day?"); // 56 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.maintain_indent = false;
        f.wrap_additional_indent = 0;
        f.set_wrap_width(12);

        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 0),
            (0, 0)
        );
        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 5),
            (0, 5)
        );
        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 11),
            (0, 11)
        );

        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 12),
            (1, 0)
        );
        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 15),
            (1, 3)
        );
        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 23),
            (1, 11)
        );

        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 24),
            (2, 0)
        );
        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 28),
            (2, 4)
        );
        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 35),
            (2, 11)
        );

        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 36),
            (3, 0)
        );
        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 43),
            (3, 7)
        );
        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 47),
            (3, 11)
        );

        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 48),
            (4, 0)
        );
        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 50),
            (4, 2)
        );
        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 56),
            (4, 8)
        );

        assert_eq!(
            f.index_to_v2d(RopeGraphemes::new(&text.slice(..)), 57),
            (4, 8)
        );
    }

    #[test]
    fn v2d_to_index_1() {
        let text = Rope::from_str("Hello there, stranger!"); // 22 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.maintain_indent = false;
        f.wrap_additional_indent = 0;
        f.set_wrap_width(80);

        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 0), (Floor, Floor)),
            0
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 5), (Floor, Floor)),
            5
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 22), (Floor, Floor)),
            22
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 23), (Floor, Floor)),
            22
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (1, 0), (Floor, Floor)),
            22
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (1, 1), (Floor, Floor)),
            22
        );
    }

    #[test]
    fn v2d_to_index_2() {
        let text = Rope::from_str("Hello there, stranger!  How are you doing this fine day?"); // 56 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.maintain_indent = false;
        f.wrap_additional_indent = 0;
        f.set_wrap_width(12);

        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 0), (Floor, Floor)),
            0
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 11), (Floor, Floor)),
            11
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (0, 12), (Floor, Floor)),
            11
        );

        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (1, 0), (Floor, Floor)),
            12
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (1, 11), (Floor, Floor)),
            23
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (1, 12), (Floor, Floor)),
            23
        );

        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (2, 0), (Floor, Floor)),
            24
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (2, 11), (Floor, Floor)),
            35
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (2, 12), (Floor, Floor)),
            35
        );

        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (3, 0), (Floor, Floor)),
            36
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (3, 11), (Floor, Floor)),
            47
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (3, 12), (Floor, Floor)),
            47
        );

        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (4, 0), (Floor, Floor)),
            48
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (4, 7), (Floor, Floor)),
            55
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (4, 8), (Floor, Floor)),
            56
        );
        assert_eq!(
            f.v2d_to_index(RopeGraphemes::new(&text.slice(..)), (4, 9), (Floor, Floor)),
            56
        );
    }

    #[test]
    fn index_to_horizontal_v2d_1() {
        let b = Buffer::new_from_str("Hello there, stranger!\nHow are you doing this fine day?"); // 55 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.maintain_indent = false;
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
        f.maintain_indent = false;
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
        f.maintain_indent = false;
        f.wrap_additional_indent = 0;
        f.set_wrap_width(80);

        assert_eq!(f.index_set_horizontal_v2d(&b, 0, 0, Floor), 0);
        assert_eq!(f.index_set_horizontal_v2d(&b, 0, 22, Floor), 22);
        assert_eq!(f.index_set_horizontal_v2d(&b, 0, 23, Floor), 22);

        assert_eq!(f.index_set_horizontal_v2d(&b, 8, 0, Floor), 0);
        assert_eq!(f.index_set_horizontal_v2d(&b, 8, 22, Floor), 22);
        assert_eq!(f.index_set_horizontal_v2d(&b, 8, 23, Floor), 22);

        assert_eq!(f.index_set_horizontal_v2d(&b, 22, 0, Floor), 0);
        assert_eq!(f.index_set_horizontal_v2d(&b, 22, 22, Floor), 22);
        assert_eq!(f.index_set_horizontal_v2d(&b, 22, 23, Floor), 22);

        assert_eq!(f.index_set_horizontal_v2d(&b, 23, 0, Floor), 23);
        assert_eq!(f.index_set_horizontal_v2d(&b, 23, 32, Floor), 55);
        assert_eq!(f.index_set_horizontal_v2d(&b, 23, 33, Floor), 55);

        assert_eq!(f.index_set_horizontal_v2d(&b, 28, 0, Floor), 23);
        assert_eq!(f.index_set_horizontal_v2d(&b, 28, 32, Floor), 55);
        assert_eq!(f.index_set_horizontal_v2d(&b, 28, 33, Floor), 55);

        assert_eq!(f.index_set_horizontal_v2d(&b, 55, 0, Floor), 23);
        assert_eq!(f.index_set_horizontal_v2d(&b, 55, 32, Floor), 55);
        assert_eq!(f.index_set_horizontal_v2d(&b, 55, 33, Floor), 55);
    }

    #[test]
    fn index_set_horizontal_v2d_2() {
        let b = Buffer::new_from_str("Hello there, stranger! How are you doing this fine day?"); // 55 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.maintain_indent = false;
        f.wrap_additional_indent = 0;
        f.set_wrap_width(12);

        assert_eq!(f.index_set_horizontal_v2d(&b, 0, 0, Floor), 0);
        assert_eq!(f.index_set_horizontal_v2d(&b, 0, 11, Floor), 11);
        assert_eq!(f.index_set_horizontal_v2d(&b, 0, 12, Floor), 11);

        assert_eq!(f.index_set_horizontal_v2d(&b, 8, 0, Floor), 0);
        assert_eq!(f.index_set_horizontal_v2d(&b, 8, 11, Floor), 11);
        assert_eq!(f.index_set_horizontal_v2d(&b, 8, 12, Floor), 11);

        assert_eq!(f.index_set_horizontal_v2d(&b, 11, 0, Floor), 0);
        assert_eq!(f.index_set_horizontal_v2d(&b, 11, 11, Floor), 11);
        assert_eq!(f.index_set_horizontal_v2d(&b, 11, 12, Floor), 11);

        assert_eq!(f.index_set_horizontal_v2d(&b, 12, 0, Floor), 12);
        assert_eq!(f.index_set_horizontal_v2d(&b, 12, 11, Floor), 23);
        assert_eq!(f.index_set_horizontal_v2d(&b, 12, 12, Floor), 23);

        assert_eq!(f.index_set_horizontal_v2d(&b, 17, 0, Floor), 12);
        assert_eq!(f.index_set_horizontal_v2d(&b, 17, 11, Floor), 23);
        assert_eq!(f.index_set_horizontal_v2d(&b, 17, 12, Floor), 23);

        assert_eq!(f.index_set_horizontal_v2d(&b, 23, 0, Floor), 12);
        assert_eq!(f.index_set_horizontal_v2d(&b, 23, 11, Floor), 23);
        assert_eq!(f.index_set_horizontal_v2d(&b, 23, 12, Floor), 23);
    }

    #[test]
    fn index_offset_vertical_v2d_1() {
        let b = Buffer::new_from_str("Hello there, stranger!\nHow are you doing this fine day?"); // 55 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.maintain_indent = false;
        f.wrap_additional_indent = 0;
        f.set_wrap_width(80);

        assert_eq!(f.index_offset_vertical_v2d(&b, 0, 0, (Floor, Floor)), 0);
        assert_eq!(f.index_offset_vertical_v2d(&b, 0, 1, (Floor, Floor)), 23);
        assert_eq!(f.index_offset_vertical_v2d(&b, 23, -1, (Floor, Floor)), 0);

        assert_eq!(f.index_offset_vertical_v2d(&b, 2, 0, (Floor, Floor)), 2);
        assert_eq!(f.index_offset_vertical_v2d(&b, 2, 1, (Floor, Floor)), 25);
        assert_eq!(f.index_offset_vertical_v2d(&b, 25, -1, (Floor, Floor)), 2);

        assert_eq!(f.index_offset_vertical_v2d(&b, 22, 0, (Floor, Floor)), 22);
        assert_eq!(f.index_offset_vertical_v2d(&b, 22, 1, (Floor, Floor)), 45);
        assert_eq!(f.index_offset_vertical_v2d(&b, 45, -1, (Floor, Floor)), 22);

        assert_eq!(f.index_offset_vertical_v2d(&b, 54, 0, (Floor, Floor)), 54);
        assert_eq!(f.index_offset_vertical_v2d(&b, 54, 1, (Floor, Floor)), 55);
        assert_eq!(f.index_offset_vertical_v2d(&b, 54, -1, (Floor, Floor)), 22);
    }

    #[test]
    fn index_offset_vertical_v2d_2() {
        let b = Buffer::new_from_str("Hello there, stranger! How are you doing this fine day?"); // 55 graphemes long

        let mut f = LineFormatter::new(4);
        f.wrap_type = WrapType::CharWrap(0);
        f.maintain_indent = false;
        f.wrap_additional_indent = 0;
        f.set_wrap_width(12);

        assert_eq!(f.index_offset_vertical_v2d(&b, 0, 0, (Floor, Floor)), 0);
        assert_eq!(f.index_offset_vertical_v2d(&b, 0, 1, (Floor, Floor)), 12);
        assert_eq!(f.index_offset_vertical_v2d(&b, 0, 2, (Floor, Floor)), 24);

        assert_eq!(f.index_offset_vertical_v2d(&b, 0, 0, (Floor, Floor)), 0);
        assert_eq!(f.index_offset_vertical_v2d(&b, 12, -1, (Floor, Floor)), 0);
        assert_eq!(f.index_offset_vertical_v2d(&b, 24, -2, (Floor, Floor)), 0);

        assert_eq!(f.index_offset_vertical_v2d(&b, 4, 0, (Floor, Floor)), 4);
        assert_eq!(f.index_offset_vertical_v2d(&b, 4, 1, (Floor, Floor)), 16);
        assert_eq!(f.index_offset_vertical_v2d(&b, 4, 2, (Floor, Floor)), 28);

        assert_eq!(f.index_offset_vertical_v2d(&b, 4, 0, (Floor, Floor)), 4);
        assert_eq!(f.index_offset_vertical_v2d(&b, 16, -1, (Floor, Floor)), 4);
        assert_eq!(f.index_offset_vertical_v2d(&b, 28, -2, (Floor, Floor)), 4);

        assert_eq!(f.index_offset_vertical_v2d(&b, 11, 0, (Floor, Floor)), 11);
        assert_eq!(f.index_offset_vertical_v2d(&b, 11, 1, (Floor, Floor)), 23);
        assert_eq!(f.index_offset_vertical_v2d(&b, 11, 2, (Floor, Floor)), 35);

        assert_eq!(f.index_offset_vertical_v2d(&b, 11, 0, (Floor, Floor)), 11);
        assert_eq!(f.index_offset_vertical_v2d(&b, 23, -1, (Floor, Floor)), 11);
        assert_eq!(f.index_offset_vertical_v2d(&b, 35, -2, (Floor, Floor)), 11);
    }
}
