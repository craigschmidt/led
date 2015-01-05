#![allow(dead_code)]

use std::iter::repeat;
use std::mem;
use std::str::Graphemes;
use string_utils::{grapheme_count, grapheme_pos_to_byte_pos, is_line_ending};

const TAB_WIDTH: uint = 4;


/// Returns the visual width of a grapheme given a starting
/// position on a line.
fn grapheme_vis_width_at_vis_pos(g: &str, pos: uint, tab_width: uint) -> uint {
    match g {
        "\t" => {
            let ending_pos = ((pos / tab_width) + 1) * tab_width;
            return ending_pos - pos;
        },
        
        _ => {
            if is_line_ending(g) {
                return 1;
            }
            else {
                return g.width(true);
            }
        }
    }
}



/// A single line of text
pub struct Line {
    text: Vec<u8>, // The text data, stored as UTF8
    pub ending: LineEnding, // The type of line ending, if any
}


impl Line {
    /// Creates a new empty Line
    pub fn new() -> Line {
        Line {
            text: Vec::new(),
            ending: LineEnding::None,
        }
    }
    
    
    /// Creates a new Line from a str.
    pub fn new_from_str(text: &str) -> Line {
        // Initialize Line
        let mut tl = Line {
            text: Vec::with_capacity(text.len()),
            ending: LineEnding::None,
        };
        
        // Copy text data, stopping on a line ending if any is found
        for g in text.graphemes(true) {        
            match g {
                //==============
                // Line endings
                //==============
                
                // CRLF
                "\u{000D}\u{000A}" => {
                    tl.ending = LineEnding::CRLF;
                    break;
                },
                
                // LF
                "\u{000A}" => {
                    tl.ending = LineEnding::LF;
                    break;
                },
                
                // VT
                "\u{000B}" => {
                    tl.ending = LineEnding::VT;
                    break;
                },
                
                // FF
                "\u{000C}" => {
                    tl.ending = LineEnding::FF;
                    break;
                },
                
                // CR
                "\u{000D}" => {
                    tl.ending = LineEnding::CR;
                    break;
                },
                
                // NEL
                "\u{0085}" => {
                    tl.ending = LineEnding::NEL;
                    break;
                },
                
                // LS
                "\u{2028}" => {
                    tl.ending = LineEnding::LS;
                    break;
                },
                
                // PS
                "\u{2029}" => {
                    tl.ending = LineEnding::PS;
                    break;
                },
                
                //==================
                // Other characters
                //==================
                
                _ => {
                    for b in g.bytes() {
                        tl.text.push(b);
                    }
                }
            }
        }
        
        // Done!
        return tl;
    }
    
    
    /// Creates a new Line from a string.
    /// Does not check to see if the string has internal newlines.
    /// This is primarily used for efficient loading of files.
    pub fn new_from_string_unchecked(text: String) -> Line {
        // Initialize Line
        let mut tl = Line {
            text: text.into_bytes(),
            ending: LineEnding::None,
        };
        
        // Check for line ending
        let mut le_size: uint = 0;
        let text_size = tl.text.len();
        if tl.text.len() >= 3 {
            match unsafe{mem::transmute::<&[u8], &str>(tl.text.slice_from(text_size-3))} {
                // LS
                "\u{2028}" => {
                    tl.ending = LineEnding::LS;
                    le_size = 3;
                },
                
                // PS
                "\u{2029}" => {
                    tl.ending = LineEnding::PS;
                    le_size = 3;
                },
                
                _ => {}
            }
        }
        
        if le_size == 0 && tl.text.len() >= 2 {
            match unsafe{mem::transmute::<&[u8], &str>(tl.text.slice_from(text_size-2))} {
                // CRLF
                "\u{000D}\u{000A}" => {
                    tl.ending = LineEnding::CRLF;
                    le_size = 2;
                },
                
                _ => {}
            }
        }
        
        if le_size == 0 && tl.text.len() >= 1 {
            match unsafe{mem::transmute::<&[u8], &str>(tl.text.slice_from(text_size-1))} {
                // LF
                "\u{000A}" => {
                    tl.ending = LineEnding::LF;
                    le_size = 1;
                },
                
                // VT
                "\u{000B}" => {
                    tl.ending = LineEnding::VT;
                    le_size = 1;
                },
                
                // FF
                "\u{000C}" => {
                    tl.ending = LineEnding::FF;
                    le_size = 1;
                },
                
                // CR
                "\u{000D}" => {
                    tl.ending = LineEnding::CR;
                    le_size = 1;
                },
                
                // NEL
                "\u{0085}" => {
                    tl.ending = LineEnding::NEL;
                    le_size = 1;
                },
                
                _ => {}
            }
        }
        
        // Truncate off the line ending, if any
        let trunc_size = text_size - le_size;
        tl.text.truncate(trunc_size);
        
        // Done!
        return tl;
    }
    
    
    /// Returns the total number of unicode graphemes in the line
    pub fn grapheme_count(&self) -> uint {
        let mut count = grapheme_count(self.as_str());
        match self.ending {
            LineEnding::None => {},
            _ => {count += 1;}
        }
        return count;
    }
    
    
    /// Returns the total number of unicode graphemes in the line,
    /// not counting the line ending grapheme, if any.
    pub fn grapheme_count_sans_line_ending(&self) -> uint {
        grapheme_count(self.as_str())
    }
    
    
    /// Returns the visual cell width of the line
    pub fn vis_width(&self, tab_width: uint) -> uint {
        let mut width = 0;
        
        for g in self.as_str().graphemes(true) {
            let w = grapheme_vis_width_at_vis_pos(g, width, tab_width);
            width += w;
        }
        
        return width;
    }
    
    
    pub fn grapheme_at_index<'a>(&'a self, index: uint) -> &'a str {
        let mut iter = self.grapheme_iter();
        let mut i = 0;
        
        for g in iter {
            if i == index {
                return g;
            }
            else {
                i += 1;
            }
        }
        
        // Should never get here
        panic!("Line::grapheme_at_index(): index past end of line.");
    }
    
    
    pub fn grapheme_width_at_index(&self, index: uint, tab_width: uint) -> uint {
        let mut iter = self.grapheme_vis_iter(tab_width);
        let mut i = 0;
        
        for (_, _, width) in iter {
            if i == index {
                return width;
            }
            else {
                i += 1;
            }
        }
        
        // Should never get here
        panic!("Line::grapheme_at_index(): index past end of line.");
    }
    
    
    /// Translates a grapheme index into a visual horizontal position
    pub fn grapheme_index_to_closest_vis_pos(&self, index: uint, tab_width: uint) -> uint {
        let mut pos = 0;
        let mut iter = self.as_str().graphemes(true);
        
        for _ in range(0, index) {
            if let Some(g) = iter.next() {
                let w = grapheme_vis_width_at_vis_pos(g, pos, tab_width);
                pos += w;
            }
            else {
                panic!("Line::grapheme_index_to_vis_pos(): index past end of line.");
            }
        }
        
        return pos;
    }
    
    
    /// Translates a visual horizontal position to the closest grapheme index
    pub fn vis_pos_to_closest_grapheme_index(&self, vis_pos: uint, tab_width: uint) -> uint {
        let mut pos = 0;
        let mut i = 0;
        let mut iter = self.as_str().graphemes(true);
        
        while pos < vis_pos {
            if let Some(g) = iter.next() {
                let w = grapheme_vis_width_at_vis_pos(g, pos, tab_width);
                if (w + pos) > vis_pos {
                    let d1 = vis_pos - pos;
                    let d2 = (pos + w) - vis_pos;
                    if d2 < d1 {
                        i += 1;
                    }
                    break;
                }
                else {
                    pos += w;
                    i += 1;
                }
            }
            else {
                break;
            }
        }
        
        return i;
    }
    
    
    /// Returns an immutable string slice into the text block's memory
    pub fn as_str<'a>(&'a self) -> &'a str {
        unsafe {
            mem::transmute(self.text.as_slice())
        }
    }
    
    
    /// Inserts `text` at grapheme index `pos`.
    /// NOTE: panics if it encounters a line ending in the text.
    pub fn insert_text(&mut self, text: &str, pos: uint) {
        // Find insertion position in bytes
        let byte_pos = grapheme_pos_to_byte_pos(self.as_str(), pos);

        // Grow data size        
        self.text.extend(repeat(0).take(text.len()));
        
        // Move old bytes forward
        let mut from = self.text.len() - text.len();
        let mut to = self.text.len();
        while from > byte_pos {
            from -= 1;
            to -= 1;
            
            self.text[to] = self.text[from];
        }
        
        // Copy new bytes in
        let mut i = byte_pos;
        for g in text.graphemes(true) {
            if is_line_ending(g) {
                panic!("Line::insert_text(): line ending in inserted text.");
            }
            
            for b in g.bytes() {
                self.text[i] = b;
                i += 1
            }
        }
    }
    
    
    /// Appends `text` to the end of line, just before the line ending (if
    /// any).
    /// NOTE: panics if it encounters a line ending in the text.
    pub fn append_text(&mut self, text: &str) {
        let mut i = self.text.len();
    
        // Grow data size        
        self.text.extend(repeat(0).take(text.len()));
        
        // Copy new bytes in
        for g in text.graphemes(true) {
            if is_line_ending(g) {
                panic!("Line::append_text(): line ending in inserted text.");
            }
            
            for b in g.bytes() {
                self.text[i] = b;
                i += 1
            }
        }
    }
    
    
    /// Remove the text between grapheme positions 'pos_a' and 'pos_b'.
    pub fn remove_text(&mut self, pos_a: uint, pos_b: uint) {
        // Bounds checks
        if pos_a > pos_b {
            panic!("Line::remove_text(): pos_a must be less than or equal to pos_b.");
        }
        
        // Find removal positions in bytes
        let byte_pos_a = grapheme_pos_to_byte_pos(self.as_str(), pos_a);
        let byte_pos_b = grapheme_pos_to_byte_pos(self.as_str(), pos_b);
        
        // Move bytes to fill in the gap left by the removed bytes
        let mut from = byte_pos_b;
        let mut to = byte_pos_a;
        while from < self.text.len() {
            self.text[to] = self.text[from];
            
            from += 1;
            to += 1;
        }
        
        // Remove data from the end
        let final_text_size = self.text.len() + byte_pos_a - byte_pos_b;
        self.text.truncate(final_text_size);
    }
    
    
    /// Insert a line break into the line, splitting it into two.
    /// This line stays as the first part of the split.  The second
    /// part is returned.
    pub fn split(&mut self, ending: LineEnding, pos: uint) -> Line {
        let mut other = Line::new();
        
        // Inserting at very beginning: special cased for efficiency
        if pos == 0 {
            mem::swap(self, &mut other);
            self.ending = ending;
        }
        // Otherwise, general case
        else {
            // Find the byte index to split at
            let byte_pos = grapheme_pos_to_byte_pos(self.as_str(), pos);
            
            // Copy the elements after the split index to the second line
            other.text.push_all(self.text.slice_from(byte_pos));
            
            // Truncate the first line
            self.text.truncate(byte_pos);
            
            // Set the line endings appropriately
            other.ending = self.ending;
            self.ending = ending;
        }
        
        return other;
    }
    
    
    /// Returns an iterator over the graphemes of the line
    pub fn grapheme_iter<'a>(&'a self) -> LineGraphemeIter<'a> {
        LineGraphemeIter {
            graphemes: self.as_str().graphemes(true),
            ending: self.ending,
            done: false,
        }
    }
    
    
    /// Returns an iterator over the graphemes of the line
    pub fn grapheme_iter_at_index<'a>(&'a self, index: uint) -> LineGraphemeIter<'a> {
        let temp: &str = unsafe{mem::transmute(self.text.as_slice())};
        
        let mut iter = LineGraphemeIter {
            graphemes: temp.graphemes(true),
            ending: self.ending,
            done: false,
        };
        
        for _ in range(0, index) {
            iter.next();
        }
        
        return iter;
    }
    
    
    /// Returns an iterator over the graphemes of the line
    pub fn grapheme_vis_iter<'a>(&'a self, tab_width: uint) -> LineGraphemeVisIter<'a> {
        LineGraphemeVisIter {
            graphemes: self.grapheme_iter(),
            vis_pos: 0,
            tab_width: tab_width,
        }
    }
}


/// Represents one of the valid Unicode line endings.
/// Also acts as an index into `LINE_ENDINGS`.
#[derive(PartialEq, Copy)]
pub enum LineEnding {
    None = 0,  // No line ending
    CRLF = 1,  // CarriageReturn followed by LineFeed
    LF = 2,    // U+000A -- LineFeed
    VT = 3,    // U+000B -- VerticalTab
    FF = 4,    // U+000C -- FormFeed
    CR = 5,    // U+000D -- CarriageReturn
    NEL = 6,   // U+0085 -- NextLine
    LS = 7,    // U+2028 -- Line Separator
    PS = 8,    // U+2029 -- ParagraphSeparator
}

pub fn str_to_line_ending(g: &str) -> LineEnding {
    match g {
        //==============
        // Line endings
        //==============
        
        // CRLF
        "\u{000D}\u{000A}" => {
            return LineEnding::CRLF;
        },
        
        // LF
        "\u{000A}" => {
            return LineEnding::LF;
        },
        
        // VT
        "\u{000B}" => {
            return LineEnding::VT;
        },
        
        // FF
        "\u{000C}" => {
            return LineEnding::FF;
        },
        
        // CR
        "\u{000D}" => {
            return LineEnding::CR;
        },
        
        // NEL
        "\u{0085}" => {
            return LineEnding::NEL;
        },
        
        // LS
        "\u{2028}" => {
            return LineEnding::LS;
        },
        
        // PS
        "\u{2029}" => {
            return LineEnding::PS;
        },
        
        // Not a line ending
        _ => {
            return LineEnding::None;
        }
    }
}

pub fn line_ending_to_str(ending: LineEnding) -> &'static str {
    LINE_ENDINGS[ending as uint]
}

/// An array of string literals corresponding to the possible
/// unicode line endings.
pub const LINE_ENDINGS: [&'static str; 9] = ["",
                          "\u{000D}\u{000A}",
                          "\u{000A}",
                          "\u{000B}",
                          "\u{000C}",
                          "\u{000D}",
                          "\u{0085}",
                          "\u{2028}",
                          "\u{2029}"
];


/// An iterator over the graphemes of a Line
pub struct LineGraphemeIter<'a> {
    graphemes: Graphemes<'a>,
    ending: LineEnding,
    done: bool,
}

impl<'a> LineGraphemeIter<'a> {
    pub fn skip_graphemes(&mut self, n: uint) {
        for _ in range(0, n) {
            if let None = self.next() {
                break;
            }
        }
    }
}

impl<'a> Iterator for LineGraphemeIter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<&'a str> {
        if self.done {
            return None;
        }
        else {
            let g = self.graphemes.next();
            if let Some(_) = g {
                return g;
            }
            else {
                self.done = true;
                
                if self.ending == LineEnding::None {
                    return None;
                }
                else {
                    return Some(LINE_ENDINGS[self.ending as uint]);
                }
            }
        }
    }
}





/// An iterator over the graphemes of a Line.  This iterator yields not just
/// the grapheme, but also it's beginning visual position in the line and its
/// visual width.
pub struct LineGraphemeVisIter<'a> {
    graphemes: LineGraphemeIter<'a>,
    vis_pos: uint,
    tab_width: uint,
}

impl<'a> LineGraphemeVisIter<'a> {
    pub fn skip_graphemes(&mut self, n: uint) {
        for _ in range(0, n) {
            if let None = self.next() {
                break;
            }
        }
    }
    
    // Skips at least n visual positions, and returns the number of excess
    // skipped visual positions beyond n.
    pub fn skip_vis_positions(&mut self, n: uint) -> uint {
        let mut i = 0;
        while i < n {
            if let Some((_, _, width)) = self.next() {
                i += width;
            }
            else {
                break;
            }
        }
        
        if i > n {
            return i - n;
        }
        else {
            return 0;
        }
    }
}

impl<'a> Iterator for LineGraphemeVisIter<'a> {
    type Item = (&'a str, uint, uint);
    
    fn next(&mut self) -> Option<(&'a str, uint, uint)> {
        if let Some(g) = self.graphemes.next() {
            let pos = self.vis_pos;
            let width = grapheme_vis_width_at_vis_pos(g, self.vis_pos, self.tab_width);
            self.vis_pos += width;
            return Some((g, pos, width));
        }
        else {
            return None;
        }
    }
}




//=========================================================================
// Line tests
//=========================================================================

#[test]
fn new_text_line() {
    let tl = Line::new();
    
    assert!(tl.text.len() == 0);
    assert!(tl.ending == LineEnding::None);
}

#[test]
fn new_text_line_from_str() {
    let tl = Line::new_from_str("Hello!");
    
    assert!(tl.text.len() == 6);
    assert!(tl.text[0] == ('H' as u8));
    assert!(tl.text[1] == ('e' as u8));
    assert!(tl.text[2] == ('l' as u8));
    assert!(tl.text[3] == ('l' as u8));
    assert!(tl.text[4] == ('o' as u8));
    assert!(tl.text[5] == ('!' as u8));
    assert!(tl.ending == LineEnding::None);
}

#[test]
fn new_text_line_from_empty_str() {
    let tl = Line::new_from_str("");
    
    assert!(tl.text.len() == 0);
    assert!(tl.ending == LineEnding::None);
}

#[test]
fn new_text_line_from_str_with_lf() {
    let tl = Line::new_from_str("Hello!\n");
    
    assert!(tl.text.len() == 6);
    assert!(tl.text[0] == ('H' as u8));
    assert!(tl.text[1] == ('e' as u8));
    assert!(tl.text[2] == ('l' as u8));
    assert!(tl.text[3] == ('l' as u8));
    assert!(tl.text[4] == ('o' as u8));
    assert!(tl.text[5] == ('!' as u8));
    assert!(tl.ending == LineEnding::LF);
}

#[test]
fn new_text_line_from_str_with_crlf() {
    let tl = Line::new_from_str("Hello!\r\n");
    
    assert!(tl.text.len() == 6);
    assert!(tl.text[0] == ('H' as u8));
    assert!(tl.text[1] == ('e' as u8));
    assert!(tl.text[2] == ('l' as u8));
    assert!(tl.text[3] == ('l' as u8));
    assert!(tl.text[4] == ('o' as u8));
    assert!(tl.text[5] == ('!' as u8));
    assert!(tl.ending == LineEnding::CRLF);
}

#[test]
fn new_text_line_from_str_with_crlf_and_too_long() {
    let tl = Line::new_from_str("Hello!\r\nLa la la la");
    
    assert!(tl.text.len() == 6);
    assert!(tl.text[0] == ('H' as u8));
    assert!(tl.text[1] == ('e' as u8));
    assert!(tl.text[2] == ('l' as u8));
    assert!(tl.text[3] == ('l' as u8));
    assert!(tl.text[4] == ('o' as u8));
    assert!(tl.text[5] == ('!' as u8));
    assert!(tl.ending == LineEnding::CRLF);
}

#[test]
fn new_text_line_from_string_unchecked() {
    let s = String::from_str("Hello!");
    
    let tl = Line::new_from_string_unchecked(s);
    
    assert!(tl.text.len() == 6);
    assert!(tl.text[0] == ('H' as u8));
    assert!(tl.text[1] == ('e' as u8));
    assert!(tl.text[2] == ('l' as u8));
    assert!(tl.text[3] == ('l' as u8));
    assert!(tl.text[4] == ('o' as u8));
    assert!(tl.text[5] == ('!' as u8));
    assert!(tl.ending == LineEnding::None);
}

#[test]
fn new_text_line_from_string_unchecked_with_lf() {
    let s = String::from_str("Hello!\u{000A}");
    
    let tl = Line::new_from_string_unchecked(s);
    
    assert!(tl.text.len() == 6);
    assert!(tl.text[0] == ('H' as u8));
    assert!(tl.text[1] == ('e' as u8));
    assert!(tl.text[2] == ('l' as u8));
    assert!(tl.text[3] == ('l' as u8));
    assert!(tl.text[4] == ('o' as u8));
    assert!(tl.text[5] == ('!' as u8));
    assert!(tl.ending == LineEnding::LF);
}

#[test]
fn new_text_line_from_string_unchecked_with_crlf() {
    let s = String::from_str("Hello!\u{000D}\u{000A}");
    
    let tl = Line::new_from_string_unchecked(s);
    
    assert!(tl.text.len() == 6);
    assert!(tl.text[0] == ('H' as u8));
    assert!(tl.text[1] == ('e' as u8));
    assert!(tl.text[2] == ('l' as u8));
    assert!(tl.text[3] == ('l' as u8));
    assert!(tl.text[4] == ('o' as u8));
    assert!(tl.text[5] == ('!' as u8));
    assert!(tl.ending == LineEnding::CRLF);
}

#[test]
fn new_text_line_from_string_unchecked_with_ls() {
    let s = String::from_str("Hello!\u{2028}");
    
    let tl = Line::new_from_string_unchecked(s);
    
    assert!(tl.text.len() == 6);
    assert!(tl.text[0] == ('H' as u8));
    assert!(tl.text[1] == ('e' as u8));
    assert!(tl.text[2] == ('l' as u8));
    assert!(tl.text[3] == ('l' as u8));
    assert!(tl.text[4] == ('o' as u8));
    assert!(tl.text[5] == ('!' as u8));
    assert!(tl.ending == LineEnding::LS);
}

#[test]
fn text_line_insert_text() {
    let mut tl = Line::new_from_str("Hello!\r\n");
    
    tl.insert_text(" world", 5);
    
    assert!(tl.text.len() == 12);
    assert!(tl.text[0] == ('H' as u8));
    assert!(tl.text[1] == ('e' as u8));
    assert!(tl.text[2] == ('l' as u8));
    assert!(tl.text[3] == ('l' as u8));
    assert!(tl.text[4] == ('o' as u8));
    assert!(tl.text[5] == (' ' as u8));
    assert!(tl.text[6] == ('w' as u8));
    assert!(tl.text[7] == ('o' as u8));
    assert!(tl.text[8] == ('r' as u8));
    assert!(tl.text[9] == ('l' as u8));
    assert!(tl.text[10] == ('d' as u8));
    assert!(tl.text[11] == ('!' as u8));
    assert!(tl.ending == LineEnding::CRLF);
}

#[test]
fn text_line_append_text() {
    let mut tl = Line::new_from_str("Hello\r\n");
    
    tl.append_text(" world!");
    
    assert!(tl.text.len() == 12);
    assert!(tl.text[0] == ('H' as u8));
    assert!(tl.text[1] == ('e' as u8));
    assert!(tl.text[2] == ('l' as u8));
    assert!(tl.text[3] == ('l' as u8));
    assert!(tl.text[4] == ('o' as u8));
    assert!(tl.text[5] == (' ' as u8));
    assert!(tl.text[6] == ('w' as u8));
    assert!(tl.text[7] == ('o' as u8));
    assert!(tl.text[8] == ('r' as u8));
    assert!(tl.text[9] == ('l' as u8));
    assert!(tl.text[10] == ('d' as u8));
    assert!(tl.text[11] == ('!' as u8));
    assert!(tl.ending == LineEnding::CRLF);
}

#[test]
fn text_line_remove_text() {
    let mut tl = Line::new_from_str("Hello world!\r\n");
    
    tl.remove_text(5, 11);
    
    assert!(tl.text.len() == 6);
    assert!(tl.text[0] == ('H' as u8));
    assert!(tl.text[1] == ('e' as u8));
    assert!(tl.text[2] == ('l' as u8));
    assert!(tl.text[3] == ('l' as u8));
    assert!(tl.text[4] == ('o' as u8));
    assert!(tl.text[5] == ('!' as u8));
    assert!(tl.ending == LineEnding::CRLF);
}

#[test]
fn text_line_split() {
    let mut tl1 = Line::new_from_str("Hello world!\r\n");
    
    let tl2 = tl1.split(LineEnding::LF, 5);
    
    assert!(tl1.text.len() == 5);
    assert!(tl1.text[0] == ('H' as u8));
    assert!(tl1.text[1] == ('e' as u8));
    assert!(tl1.text[2] == ('l' as u8));
    assert!(tl1.text[3] == ('l' as u8));
    assert!(tl1.text[4] == ('o' as u8));
    assert!(tl1.ending == LineEnding::LF);
    
    assert!(tl2.text.len() == 7);
    assert!(tl2.text[0] == (' ' as u8));
    assert!(tl2.text[1] == ('w' as u8));
    assert!(tl2.text[2] == ('o' as u8));
    assert!(tl2.text[3] == ('r' as u8));
    assert!(tl2.text[4] == ('l' as u8));
    assert!(tl2.text[5] == ('d' as u8));
    assert!(tl2.text[6] == ('!' as u8));
    assert!(tl2.ending == LineEnding::CRLF);
}

#[test]
fn text_line_split_beginning() {
    let mut tl1 = Line::new_from_str("Hello!\r\n");
    
    let tl2 = tl1.split(LineEnding::LF, 0);
    
    assert!(tl1.text.len() == 0);
    assert!(tl1.ending == LineEnding::LF);
    
    assert!(tl2.text.len() == 6);
    assert!(tl2.text[0] == ('H' as u8));
    assert!(tl2.text[1] == ('e' as u8));
    assert!(tl2.text[2] == ('l' as u8));
    assert!(tl2.text[3] == ('l' as u8));
    assert!(tl2.text[4] == ('o' as u8));
    assert!(tl2.text[5] == ('!' as u8));
    assert!(tl2.ending == LineEnding::CRLF);
}

#[test]
fn grapheme_index_to_closest_vis_pos_1() {
    let tl = Line::new_from_str("Hello!");
    
    assert!(tl.grapheme_index_to_closest_vis_pos(0) == 0);
}

#[test]
fn grapheme_index_to_closest_vis_pos_2() {
    let tl = Line::new_from_str("\tHello!");
    
    assert!(tl.grapheme_index_to_closest_vis_pos(1) == TAB_WIDTH);
}

#[test]
fn vis_pos_to_closest_grapheme_index_1() {
    let tl = Line::new_from_str("Hello!");
    
    assert!(tl.vis_pos_to_closest_grapheme_index(0) == 0);
}

#[test]
fn vis_pos_to_closest_grapheme_index_2() {
    let tl = Line::new_from_str("\tHello!");
    
    assert!(tl.vis_pos_to_closest_grapheme_index(TAB_WIDTH) == 1);
}


//=========================================================================
// LineGraphemeIter tests
//=========================================================================

#[test]
fn text_line_grapheme_iter() {
    let tl = Line::new_from_str("Hello!");
    let mut iter = tl.grapheme_iter();
    
    assert!(iter.next() == Some("H"));
    assert!(iter.next() == Some("e"));
    assert!(iter.next() == Some("l"));
    assert!(iter.next() == Some("l"));
    assert!(iter.next() == Some("o"));
    assert!(iter.next() == Some("!"));
    assert!(iter.next() == None);
}

#[test]
fn text_line_grapheme_iter_with_lf() {
    let tl = Line::new_from_str("Hello!\n");
    let mut iter = tl.grapheme_iter();
    
    assert!(iter.next() == Some("H"));
    assert!(iter.next() == Some("e"));
    assert!(iter.next() == Some("l"));
    assert!(iter.next() == Some("l"));
    assert!(iter.next() == Some("o"));
    assert!(iter.next() == Some("!"));
    assert!(iter.next() == Some("\n"));
    assert!(iter.next() == None);
}

#[test]
fn text_line_grapheme_iter_with_crlf() {
    let tl = Line::new_from_str("Hello!\r\n");
    let mut iter = tl.grapheme_iter();
    
    assert!(iter.next() == Some("H"));
    assert!(iter.next() == Some("e"));
    assert!(iter.next() == Some("l"));
    assert!(iter.next() == Some("l"));
    assert!(iter.next() == Some("o"));
    assert!(iter.next() == Some("!"));
    assert!(iter.next() == Some("\r\n"));
    assert!(iter.next() == None);
}

#[test]
fn text_line_grapheme_iter_at_index() {
    let tl = Line::new_from_str("Hello!");
    let mut iter = tl.grapheme_iter_at_index(2);
    
    assert!(iter.next() == Some("l"));
    assert!(iter.next() == Some("l"));
    assert!(iter.next() == Some("o"));
    assert!(iter.next() == Some("!"));
    assert!(iter.next() == None);
}

#[test]
fn text_line_grapheme_iter_at_index_past_end() {
    let tl = Line::new_from_str("Hello!");
    let mut iter = tl.grapheme_iter_at_index(10);
    
    assert!(iter.next() == None);
}

#[test]
fn text_line_grapheme_iter_at_index_at_lf() {
    let tl = Line::new_from_str("Hello!\n");
    let mut iter = tl.grapheme_iter_at_index(6);
    
    assert!(iter.next() == Some("\n"));
    assert!(iter.next() == None);
}