#![allow(dead_code)] // TODO: lots of unused stuff

use std::fs::File;
use std::io;
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};
use std::cmp::{min,max};

use self::undo_stack::Operation::*;
use self::undo_stack::UndoStack;
use ropey::{Rope, RopeSlice, iter};
use string_utils::char_count;
use utils::{is_grapheme_boundary, next_grapheme_boundary, prev_grapheme_boundary, RopeGraphemes};

mod undo_stack;

// =============================================================
// Buffer
// =============================================================

/// A text buffer
pub struct Buffer {
    text: Rope,
    file_path: Option<PathBuf>,
    undo_stack: UndoStack,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            text: Rope::new(),
            file_path: None,
            undo_stack: UndoStack::new(),
        }
    }

    pub fn new_from_str(s: &str) -> Buffer {
        Buffer {
            text: Rope::from_str(s),
            file_path: None,
            undo_stack: UndoStack::new(),
        }
    }

    pub fn new_from_file(path: &Path) -> io::Result<Buffer> {

        let f = BufReader::new(File::open(path)?);

        let buf = Buffer {

            text: Rope::from_reader(f)?,
            file_path: Some(path.to_path_buf()),
            undo_stack: UndoStack::new(),
        };

        return Ok(buf);
    }

    pub fn save_to_file(&self, path: &Path) -> io::Result<()> {

        // open file buffer, can fail
        let f = BufWriter::new(File::create(path)?);

        // Write the file back out to disk, can fail
        self.text.write_to(f)?;

        return Ok(());
    }

    // ------------------------------------------------------------------------
    // Functions for getting information about the buffer.
    // ------------------------------------------------------------------------

    pub fn char_count(&self) -> usize {
        self.text.len_chars()
    }

    pub fn is_grapheme(&self, char_idx: usize) -> bool {
        is_grapheme_boundary(&self.text.slice(..), char_idx)
    }

    /// Finds the nth next grapheme boundary after the given char position.
    pub fn nth_next_grapheme(&self, char_idx: usize, n: usize) -> usize {
        let mut char_idx = char_idx;
        for _ in 0..n {
            char_idx = next_grapheme_boundary(&self.text.slice(..), char_idx);
        }
        char_idx
    }

    /// Finds the nth previous grapheme boundary before the given char position.
    pub fn nth_prev_grapheme(&self, char_idx: usize, n: usize) -> usize {
        let mut char_idx = char_idx;
        for _ in 0..n {
            char_idx = prev_grapheme_boundary(&self.text.slice(..), char_idx);
        }
        char_idx
    }

    pub fn line_count(&self) -> usize {
        self.text.len_lines()
    }

    // ------------------------------------------------------------------------
    // Editing operations
    // ------------------------------------------------------------------------

    /// Insert 'text' at grapheme position 'pos'.
    pub fn insert_text(&mut self, text: &str, pos: usize) {
        self.text.insert(pos, text);

        self.undo_stack.push(InsertText(text.to_string(), pos));
    }

    /// Remove the text before grapheme position 'pos' of length 'len'.
    pub fn remove_text_before(&mut self, pos: usize, len: usize) {
        if pos >= len {
            let removed_text = self.text.slice((pos - len)..pos).to_string();

            self.text.remove((pos - len)..pos);

            // Push operation to the undo stack
            self.undo_stack
                .push(RemoveTextBefore(removed_text, pos - len));
        } else {
            panic!(
                "Buffer::remove_text_before(): attempt to remove text before beginning of \
                 buffer."
            );
        }
    }

    /// Remove the text after grapheme position 'pos' of length 'len'.
    pub fn remove_text_after(&mut self, pos: usize, len: usize) {
        let removed_text = self.text.slice(pos..(pos + len)).to_string();

        self.text.remove(pos..(pos + len));

        // Push operation to the undo stack
        self.undo_stack.push(RemoveTextAfter(removed_text, pos));
    }

    /// Moves the text in [pos_a, pos_b) to begin at index pos_to.
    ///
    /// Note that pos_to is the desired index that the text will start at
    /// _after_ the operation, not the index before the operation.  This is a
    /// subtle but important distinction.
    pub fn move_text(&mut self, pos_a: usize, pos_b: usize, pos_to: usize) {
        self._move_text(pos_a, pos_b, pos_to);

        // Push operation to the undo stack
        self.undo_stack.push(MoveText(pos_a, pos_b, pos_to));
    }

    fn _move_text(&mut self, pos_a: usize, pos_b: usize, pos_to: usize) {
        // Nothing to do
        if pos_a == pos_b || pos_a == pos_to {
            return;
        }
        // Bounds error
        else if pos_a > pos_b {
            panic!("Buffer::_move_text(): pos_a must be less than or equal to pos_b.");
        }
        // Bounds error
        else if pos_b > self.text.len_chars() {
            panic!("Buffer::_move_text(): specified text range is beyond end of buffer.");
        }
        // Bounds error
        else if pos_to > (self.text.len_chars() - (pos_b - pos_a)) {
            panic!("Buffer::_move_text(): specified text destination is beyond end of buffer.");
        }
        // Nothing to do, because entire text specified
        else if pos_a == 0 && pos_b == self.text.len_chars() {
            return;
        }
        // All other cases
        else {
            // TODO: a more efficient implementation that directly
            // manipulates the node tree.
            let s = self.text.slice(pos_a..pos_b).to_string();
            self.text.remove(pos_a..pos_b);
            self.text.insert(pos_to, &s);
        }
    }

    /// Removes the lines in line indices [line_a, line_b).
    /// TODO: undo
    pub fn remove_lines(&mut self, line_a: usize, line_b: usize) {
        // Nothing to do
        if line_a == line_b {
            return;
        }
        // Bounds error
        else if line_a > line_b {
            panic!("Buffer::remove_lines(): line_a must be less than or equal to line_b.");
        }
        // Bounds error
        else if line_b > self.line_count() {
            panic!("Buffer::remove_lines(): attempt to remove lines past the last line of text.");
        }
        // All other cases
        else {
            let a = if line_a == 0 {
                0
            } else if line_a == self.text.len_lines() {
                self.text.len_chars()
            } else {
                self.text.line_to_char(line_a) - 1
            };
            let b = if line_b == 0 {
                0
            } else if line_b == self.text.len_lines() {
                self.text.len_chars()
            } else if line_a == 0 {
                self.text.line_to_char(line_b)
            } else {
                self.text.line_to_char(line_b) - 1
            };

            self.text.remove(a..b);
        }
    }

    // ------------------------------------------------------------------------
    // Undo/redo functionality
    // ------------------------------------------------------------------------

    /// Undoes operations that were pushed to the undo stack, and returns a
    /// cursor position that the cursor should jump to, if any.
    pub fn undo(&mut self) -> Option<usize> {
        if let Some(op) = self.undo_stack.prev() {
            match op {
                InsertText(ref s, p) => {
                    let size = char_count(s);
                    self.text.remove(p..(p + size));
                    return Some(p);
                }

                RemoveTextBefore(ref s, p) => {
                    let size = char_count(s);
                    self.text.insert(p, s);
                    return Some(p + size);
                }

                RemoveTextAfter(ref s, p) => {
                    self.text.insert(p, s);
                    return Some(p);
                }

                MoveText(pa, pb, pto) => {
                    let size = pb - pa;
                    self._move_text(pto, pto + size, pa);
                    return Some(pa);
                }

                _ => {
                    return None;
                }
            }
        }

        return None;
    }

    /// Redoes the last undone operation, and returns a cursor position that
    /// the cursor should jump to, if any.
    pub fn redo(&mut self) -> Option<usize> {
        if let Some(op) = self.undo_stack.next() {
            match op {
                InsertText(ref s, p) => {
                    let size = char_count(s);
                    self.text.insert(p, s);
                    return Some(p + size);
                }

                RemoveTextBefore(ref s, p) | RemoveTextAfter(ref s, p) => {
                    let size = char_count(s);
                    self.text.remove(p..(p + size));
                    return Some(p);
                }

                MoveText(pa, pb, pto) => {
                    self._move_text(pa, pb, pto);
                    return Some(pa);
                }

                _ => {
                    return None;
                }
            }
        }

        return None;
    }

    // ------------------------------------------------------------------------
    // Position conversions
    // ------------------------------------------------------------------------

    /// Converts a char index into a line number and char-column number.
    /// i.e. char offset from start of line
    ///
    /// If the index is off the end of the text, returns the line and column
    /// number of the last valid text position.
    pub fn index_to_line_col(&self, pos: usize) -> (usize, usize) {
        if pos < self.text.len_chars() {
            // the line
            let line = self.text.char_to_line(pos);
            // Returns the char index of the start of the given line.
            let line_pos = self.text.line_to_char(line);

            // so the position on the line is the difference with the start of the line
            return (line, pos - line_pos);  // line number, char-column number
        } else {

            // off the end, if for example "move to line" is too large
            let line = self.text.len_lines() - 1;

            // Returns the char index of the start of the given line.
            let line_pos = self.text.line_to_char(line);

            // return one-past-the-end for char offset
            return (line, self.text.len_chars() - line_pos);
        }
    }

    /// Converts a line number and char-column number into a char
    /// index.
    ///
    /// If the column number given is beyond the end of the line, returns the
    /// index of the line's last valid position.  If the line number given is
    /// beyond the end of the buffer, returns the index of the buffer's last
    /// valid position.
    pub fn line_col_to_index(&self, pos: (usize, usize)) -> usize {

        // need to special case this or l_end-1 will underflow
        if self.text.len_chars() == 0 {
            return 0
        }

        if pos.0 < self.text.len_lines() {
            
            // char_idx for start of line 
            let l_start = self.text.line_to_char(pos.0);   
            // char_idx for start of next line
            let l_end = self.text.line_to_char(pos.0 + 1); 
            
            // back up by 1 to get to end of line,
            // if they are not the same (say at the end of text)
            let end_of_line = max(l_start,l_end-1);

            // overall character index is index of start of line, plus char-column number
            let char_idx = l_start + pos.1;
            
            // don't go beyond the end of this line 
            return min(char_idx, end_of_line);

        } else { 
            // is beyond the last line,, so return one beyond
            return self.text.len_chars();
        }
    }

    // ------------------------------------------------------------------------
    // Text reading functions
    // ------------------------------------------------------------------------

    pub fn get_grapheme<'a>(&'a self, index: usize) -> RopeSlice<'a> {
        RopeGraphemes::new(&self.text.slice(index..))
            .nth(0)
            .unwrap()
    }

    pub fn get_line<'a>(&'a self, index: usize) -> RopeSlice<'a> {
        self.text.line(index)
    }

    /// Creates a String from the buffer text in grapheme range [pos_a, posb).
    fn string_from_range(&self, pos_a: usize, pos_b: usize) -> String {
        self.text.slice(pos_a..pos_b).to_string()
    }

    // ------------------------------------------------------------------------
    // Iterator creators
    // ------------------------------------------------------------------------

    /// Creates an iterator at the first character
    pub fn grapheme_iter<'a>(&'a self) -> RopeGraphemes<'a> {
        RopeGraphemes::new(&self.text.slice(..))
    }

    /// Creates an iterator starting at the specified grapheme index.
    /// If the index is past the end of the text, then the iterator will
    /// return None on next().
    pub fn grapheme_iter_at_index<'a>(&'a self, index: usize) -> RopeGraphemes<'a> {
        let len = self.text.len_chars();
        RopeGraphemes::new(&self.text.slice(index..len))
    }

    pub fn line_iter<'a>(&'a self) -> iter::Lines<'a> {
        self.text.lines()
    }

    pub fn line_iter_at_index<'a>(&'a self, line_idx: usize) -> iter::Lines<'a> {
        let start = self.text.line_to_char(line_idx);
        self.text.slice(start..).lines()
    }
}

// ================================================================
// TESTS
// ================================================================

#[cfg(test)]
mod tests {
    #![allow(unused_imports)]
    use super::*;

    #[test]
    fn insert_text() {
        let mut buf = Buffer::new();

        buf.insert_text("Hello 世界!", 0);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 9);
        assert!(buf.line_count() == 1);
        assert!("H" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!(" " == iter.next().unwrap());
        assert!("世" == iter.next().unwrap());
        assert!("界" == iter.next().unwrap());
        assert!("!" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn insert_text_with_newlines() {
        let mut buf = Buffer::new();

        buf.insert_text("Hello\n 世界\r\n!", 0);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 12);
        assert!(buf.line_count() == 3);
        assert!("H" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!(" " == iter.next().unwrap());
        assert!("世" == iter.next().unwrap());
        assert!("界" == iter.next().unwrap());
        assert!("\r\n" == iter.next().unwrap());
        assert!("!" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn insert_text_in_non_empty_buffer_1() {
        let mut buf = Buffer::new();

        buf.insert_text("Hello\n 世界\r\n!", 0);
        buf.insert_text("Again ", 0);

        let mut iter = buf.grapheme_iter();

        assert_eq!(buf.char_count(), 18);
        assert_eq!(buf.line_count(), 3);
        assert_eq!("A", iter.next().unwrap());
        assert_eq!("g", iter.next().unwrap());
        assert_eq!("a", iter.next().unwrap());
        assert_eq!("i", iter.next().unwrap());
        assert_eq!("n", iter.next().unwrap());
        assert_eq!(" ", iter.next().unwrap());
        assert_eq!("H", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("o", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!(" ", iter.next().unwrap());
        assert_eq!("世", iter.next().unwrap());
        assert_eq!("界", iter.next().unwrap());
        assert_eq!("\r\n", iter.next().unwrap());
        assert_eq!("!", iter.next().unwrap());
        assert_eq!(None, iter.next());
    }

    #[test]
    fn insert_text_in_non_empty_buffer_2() {
        let mut buf = Buffer::new();

        buf.insert_text("Hello\n 世界\r\n!", 0);
        buf.insert_text(" again", 5);

        let mut iter = buf.grapheme_iter();

        assert_eq!(buf.char_count(), 18);
        assert_eq!(buf.line_count(), 3);
        assert_eq!("H", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("o", iter.next().unwrap());
        assert_eq!(" ", iter.next().unwrap());
        assert_eq!("a", iter.next().unwrap());
        assert_eq!("g", iter.next().unwrap());
        assert_eq!("a", iter.next().unwrap());
        assert_eq!("i", iter.next().unwrap());
        assert_eq!("n", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!(" ", iter.next().unwrap());
        assert_eq!("世", iter.next().unwrap());
        assert_eq!("界", iter.next().unwrap());
        assert_eq!("\r\n", iter.next().unwrap());
        assert_eq!("!", iter.next().unwrap());
        assert_eq!(None, iter.next());
    }

    #[test]
    fn insert_text_in_non_empty_buffer_3() {
        let mut buf = Buffer::new();

        buf.insert_text("Hello\n 世界\r\n!", 0);
        buf.insert_text("again", 6);

        let mut iter = buf.grapheme_iter();

        assert_eq!(buf.char_count(), 17);
        assert_eq!(buf.line_count(), 3);
        assert_eq!("H", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("o", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!("a", iter.next().unwrap());
        assert_eq!("g", iter.next().unwrap());
        assert_eq!("a", iter.next().unwrap());
        assert_eq!("i", iter.next().unwrap());
        assert_eq!("n", iter.next().unwrap());
        assert_eq!(" ", iter.next().unwrap());
        assert_eq!("世", iter.next().unwrap());
        assert_eq!("界", iter.next().unwrap());
        assert_eq!("\r\n", iter.next().unwrap());
        assert_eq!("!", iter.next().unwrap());
        assert_eq!(None, iter.next());
    }

    #[test]
    fn insert_text_in_non_empty_buffer_4() {
        let mut buf = Buffer::new();

        buf.insert_text("Hello\n 世界\r\n!", 0);
        buf.insert_text("again", 12);

        let mut iter = buf.grapheme_iter();

        assert_eq!(buf.char_count(), 17);
        assert_eq!(buf.line_count(), 3);
        assert_eq!("H", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("o", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!(" ", iter.next().unwrap());
        assert_eq!("世", iter.next().unwrap());
        assert_eq!("界", iter.next().unwrap());
        assert_eq!("\r\n", iter.next().unwrap());
        assert_eq!("!", iter.next().unwrap());
        assert_eq!("a", iter.next().unwrap());
        assert_eq!("g", iter.next().unwrap());
        assert_eq!("a", iter.next().unwrap());
        assert_eq!("i", iter.next().unwrap());
        assert_eq!("n", iter.next().unwrap());
        assert_eq!(None, iter.next());
    }

    #[test]
    fn insert_text_in_non_empty_buffer_5() {
        let mut buf = Buffer::new();

        buf.insert_text("Hello\n 世界\r\n!", 0);
        buf.insert_text("again", 2);

        let mut iter = buf.grapheme_iter();

        assert_eq!(buf.char_count(), 17);
        assert_eq!(buf.line_count(), 3);
        assert_eq!("H", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("a", iter.next().unwrap());
        assert_eq!("g", iter.next().unwrap());
        assert_eq!("a", iter.next().unwrap());
        assert_eq!("i", iter.next().unwrap());
        assert_eq!("n", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("o", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!(" ", iter.next().unwrap());
        assert_eq!("世", iter.next().unwrap());
        assert_eq!("界", iter.next().unwrap());
        assert_eq!("\r\n", iter.next().unwrap());
        assert_eq!("!", iter.next().unwrap());

        assert_eq!(None, iter.next());
    }

    #[test]
    fn insert_text_in_non_empty_buffer_6() {
        let mut buf = Buffer::new();

        buf.insert_text("Hello\n 世界\r\n!", 0);
        buf.insert_text("again", 8);

        let mut iter = buf.grapheme_iter();

        assert_eq!(buf.char_count(), 17);
        assert_eq!(buf.line_count(), 3);
        assert_eq!("H", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("o", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!(" ", iter.next().unwrap());
        assert_eq!("世", iter.next().unwrap());
        assert_eq!("a", iter.next().unwrap());
        assert_eq!("g", iter.next().unwrap());
        assert_eq!("a", iter.next().unwrap());
        assert_eq!("i", iter.next().unwrap());
        assert_eq!("n", iter.next().unwrap());
        assert_eq!("界", iter.next().unwrap());
        assert_eq!("\r\n", iter.next().unwrap());
        assert_eq!("!", iter.next().unwrap());

        assert_eq!(None, iter.next());
    }

    #[test]
    fn insert_text_in_non_empty_buffer_7() {
        let mut buf = Buffer::new();

        buf.insert_text("Hello\n 世界\r\n!", 0);
        buf.insert_text("\nag\n\nain\n", 2);

        let mut iter = buf.grapheme_iter();

        assert_eq!(buf.char_count(), 21);
        assert_eq!(buf.line_count(), 7);
        assert_eq!("H", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!("a", iter.next().unwrap());
        assert_eq!("g", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!("a", iter.next().unwrap());
        assert_eq!("i", iter.next().unwrap());
        assert_eq!("n", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("o", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!(" ", iter.next().unwrap());
        assert_eq!("世", iter.next().unwrap());
        assert_eq!("界", iter.next().unwrap());
        assert_eq!("\r\n", iter.next().unwrap());
        assert_eq!("!", iter.next().unwrap());

        assert_eq!(None, iter.next());
    }

    #[test]
    fn remove_text_1() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);
        assert!(buf.char_count() == 29);
        assert!(buf.line_count() == 6);

        buf.text.remove(0..3);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 26);
        assert!(buf.line_count() == 5);
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("f" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("w" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("d" == iter.next().unwrap());
        assert!("!" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn remove_text_2() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);
        assert!(buf.char_count() == 29);
        assert!(buf.line_count() == 6);

        buf.text.remove(0..12);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 17);
        assert!(buf.line_count() == 4);
        assert!("p" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("f" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("w" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("d" == iter.next().unwrap());
        assert!("!" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn remove_text_3() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);
        assert!(buf.char_count() == 29);
        assert!(buf.line_count() == 6);

        buf.text.remove(5..17);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 17);
        assert!(buf.line_count() == 4);
        assert!("H" == iter.next().unwrap());
        assert!("i" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("f" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("w" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("d" == iter.next().unwrap());
        assert!("!" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn remove_text_4() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);
        assert!(buf.char_count() == 29);
        assert!(buf.line_count() == 6);

        buf.text.remove(23..29);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 23);
        assert!(buf.line_count() == 6);
        assert!("H" == iter.next().unwrap());
        assert!("i" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("f" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn remove_text_5() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);
        assert!(buf.char_count() == 29);
        assert!(buf.line_count() == 6);

        buf.text.remove(17..29);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 17);
        assert!(buf.line_count() == 4);
        assert!("H" == iter.next().unwrap());
        assert!("i" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn remove_text_6() {
        let mut buf = Buffer::new();

        buf.insert_text("Hello\nworld!", 0);
        assert!(buf.char_count() == 12);
        assert!(buf.line_count() == 2);

        buf.text.remove(3..12);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 3);
        assert!(buf.line_count() == 1);
        assert!("H" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn remove_text_7() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\nworld!", 0);
        assert!(buf.char_count() == 15);
        assert!(buf.line_count() == 3);

        buf.text.remove(5..15);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 5);
        assert!(buf.line_count() == 2);
        assert!("H" == iter.next().unwrap());
        assert!("i" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn remove_text_8() {
        let mut buf = Buffer::new();

        buf.insert_text("Hello\nworld!", 0);
        assert!(buf.char_count() == 12);
        assert!(buf.line_count() == 2);

        buf.text.remove(3..11);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 4);
        assert!(buf.line_count() == 1);
        assert!("H" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("!" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn remove_text_9() {
        let mut buf = Buffer::new();

        buf.insert_text("Hello\nworld!", 0);
        assert!(buf.char_count() == 12);
        assert!(buf.line_count() == 2);

        buf.text.remove(8..12);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 8);
        assert!(buf.line_count() == 2);
        assert!("H" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("w" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn remove_text_10() {
        let mut buf = Buffer::new();

        buf.insert_text("12\n34\n56\n78", 0);
        assert!(buf.char_count() == 11);
        assert!(buf.line_count() == 4);

        buf.text.remove(4..11);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 4);
        assert!(buf.line_count() == 2);
        assert!("1" == iter.next().unwrap());
        assert!("2" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("3" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn remove_text_11() {
        let mut buf = Buffer::new();

        buf.insert_text("1234567890", 0);
        assert!(buf.char_count() == 10);
        assert!(buf.line_count() == 1);

        buf.text.remove(9..10);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 9);
        assert!(buf.line_count() == 1);
        assert!("1" == iter.next().unwrap());
        assert!("2" == iter.next().unwrap());
        assert!("3" == iter.next().unwrap());
        assert!("4" == iter.next().unwrap());
        assert!("5" == iter.next().unwrap());
        assert!("6" == iter.next().unwrap());
        assert!("7" == iter.next().unwrap());
        assert!("8" == iter.next().unwrap());
        assert!("9" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn move_text_1() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);

        buf.move_text(0, 3, 2);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 29);
        assert!(buf.line_count() == 6);
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("H" == iter.next().unwrap());
        assert!("i" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("f" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("w" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("d" == iter.next().unwrap());
        assert!("!" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn move_text_2() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);

        buf.move_text(3, 8, 6);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 29);
        assert!(buf.line_count() == 6);
        assert!("H" == iter.next().unwrap());
        assert!("i" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("f" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("w" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("d" == iter.next().unwrap());
        assert!("!" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn move_text_3() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);

        buf.move_text(12, 17, 6);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 29);
        assert!(buf.line_count() == 6);
        assert!("H" == iter.next().unwrap());
        assert!("i" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("f" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("w" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("d" == iter.next().unwrap());
        assert!("!" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn move_text_4() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);

        buf.move_text(23, 29, 20);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 29);
        assert!(buf.line_count() == 6);
        assert!("H" == iter.next().unwrap());
        assert!("i" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("f" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("w" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("d" == iter.next().unwrap());
        assert!("!" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn move_text_5() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);

        buf.move_text(0, 29, 0);

        let mut iter = buf.grapheme_iter();

        assert!(buf.char_count() == 29);
        assert!(buf.line_count() == 6);
        assert!("H" == iter.next().unwrap());
        assert!("i" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("p" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("f" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("w" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("d" == iter.next().unwrap());
        assert!("!" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn remove_lines_1() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);
        assert_eq!(buf.char_count(), 29);
        assert_eq!(buf.line_count(), 6);

        buf.remove_lines(0, 3);

        let mut iter = buf.grapheme_iter();

        assert_eq!(buf.char_count(), 13);
        assert_eq!(buf.line_count(), 3);
        assert_eq!("o", iter.next().unwrap());
        assert_eq!("f", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!("t", iter.next().unwrap());
        assert_eq!("h", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!("w", iter.next().unwrap());
        assert_eq!("o", iter.next().unwrap());
        assert_eq!("r", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("d", iter.next().unwrap());
        assert_eq!("!", iter.next().unwrap());
        assert_eq!(None, iter.next());
    }

    #[test]
    fn remove_lines_2() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);
        assert_eq!(buf.char_count(), 29);
        assert_eq!(buf.line_count(), 6);

        buf.remove_lines(1, 4);

        let mut iter = buf.grapheme_iter();

        assert_eq!(buf.char_count(), 13);
        assert_eq!(buf.line_count(), 3);
        assert_eq!("H", iter.next().unwrap());
        assert_eq!("i", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!("t", iter.next().unwrap());
        assert_eq!("h", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!("w", iter.next().unwrap());
        assert_eq!("o", iter.next().unwrap());
        assert_eq!("r", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("d", iter.next().unwrap());
        assert_eq!("!", iter.next().unwrap());
        assert_eq!(None, iter.next());
    }

    #[test]
    fn remove_lines_3() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);
        assert_eq!(buf.char_count(), 29);
        assert_eq!(buf.line_count(), 6);

        // "Hi\nthere\npeople\nof\nthe\nworld!"

        buf.remove_lines(3, 6);

        let mut iter = buf.grapheme_iter();

        assert_eq!(buf.char_count(), 15);
        assert_eq!(buf.line_count(), 3);
        assert_eq!("H", iter.next().unwrap());
        assert_eq!("i", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!("t", iter.next().unwrap());
        assert_eq!("h", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("r", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!("p", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("o", iter.next().unwrap());
        assert_eq!("p", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!(None, iter.next());
    }

    #[test]
    fn remove_lines_4() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\n", 0);
        assert_eq!(buf.char_count(), 23);
        assert_eq!(buf.line_count(), 6);

        buf.remove_lines(3, 6);

        let mut iter = buf.grapheme_iter();

        assert_eq!(buf.char_count(), 15);
        assert_eq!(buf.line_count(), 3);
        assert_eq!("H", iter.next().unwrap());
        assert_eq!("i", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!("t", iter.next().unwrap());
        assert_eq!("h", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("r", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("\n", iter.next().unwrap());
        assert_eq!("p", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!("o", iter.next().unwrap());
        assert_eq!("p", iter.next().unwrap());
        assert_eq!("l", iter.next().unwrap());
        assert_eq!("e", iter.next().unwrap());
        assert_eq!(None, iter.next());
    }

    #[test]
    fn remove_lines_5() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);
        assert_eq!(buf.char_count(), 29);
        assert_eq!(buf.line_count(), 6);

        buf.remove_lines(0, 6);

        let mut iter = buf.grapheme_iter();

        assert_eq!(buf.char_count(), 0);
        assert_eq!(buf.line_count(), 1);
        assert_eq!(None, iter.next());
    }

    #[test]
    fn remove_lines_6() {
        let mut buf = Buffer::new();

        buf.insert_text("Hi\nthere\npeople\nof\nthe\n", 0);
        assert_eq!(buf.char_count(), 23);
        assert_eq!(buf.line_count(), 6);

        buf.remove_lines(0, 6);

        let mut iter = buf.grapheme_iter();

        assert_eq!(buf.char_count(), 0);
        assert_eq!(buf.line_count(), 1);
        assert_eq!(None, iter.next());
    }

    #[test]
    fn line_col_to_index_1() {
        let mut buf = Buffer::new();
        //  index        0           1           2
        //  index        012 345678 9012345 678 9012 3456789
        //  line         0   1      2       3   4    5
        //  col          012 012345 0123456 012 0123 012345
        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);

        assert_eq!(buf.line_col_to_index((0,  0)),  0);
        assert_eq!(buf.line_col_to_index((0,  1)),  1);
        assert_eq!(buf.line_col_to_index((0,  2)),  2);  // \n
        assert_eq!(buf.line_col_to_index((0,  3)),  2);  // invalid  col
        
        assert_eq!(buf.line_col_to_index((1,  0)),  3);
        assert_eq!(buf.line_col_to_index((1,  1)),  4);
        assert_eq!(buf.line_col_to_index((1,  2)),  5); 
        assert_eq!(buf.line_col_to_index((1,  3)),  6);
        assert_eq!(buf.line_col_to_index((1,  4)),  7);
        assert_eq!(buf.line_col_to_index((1,  5)),  8);  // \n
        assert_eq!(buf.line_col_to_index((1,  6)),  8);  // invalid col

        assert_eq!(buf.line_col_to_index((2,  0)),  9);  
        assert_eq!(buf.line_col_to_index((2,  1)),  10);  
        assert_eq!(buf.line_col_to_index((2,  2)),  11);  
        assert_eq!(buf.line_col_to_index((2,  3)),  12);  
        assert_eq!(buf.line_col_to_index((2,  4)),  13);  
        assert_eq!(buf.line_col_to_index((2,  5)),  14);  
        assert_eq!(buf.line_col_to_index((2,  6)),  15);  // \n 
        assert_eq!(buf.line_col_to_index((2,  7)),  15);  // invalid col 
        assert_eq!(buf.line_col_to_index((2, 10)),  15);  // invalid col
        
        assert_eq!(buf.line_col_to_index((3,  0)),  16);  
        assert_eq!(buf.line_col_to_index((3,  1)),  17);
        assert_eq!(buf.line_col_to_index((3,  2)),  18);  // \n
        assert_eq!(buf.line_col_to_index((3,  3)),  18);  // invalid col

        assert_eq!(buf.line_col_to_index((4,  0)),  19);  
        assert_eq!(buf.line_col_to_index((4,  1)),  20);  
        assert_eq!(buf.line_col_to_index((4,  2)),  21);  
        assert_eq!(buf.line_col_to_index((4,  3)),  22);  // \n  
        assert_eq!(buf.line_col_to_index((4,  4)),  22);  // invalid col

        assert_eq!(buf.line_col_to_index((5,  0)),  23);  // w 
        assert_eq!(buf.line_col_to_index((5,  1)),  24);  // o
        assert_eq!(buf.line_col_to_index((5,  2)),  25);  // r
        assert_eq!(buf.line_col_to_index((5,  3)),  26);  // l
        assert_eq!(buf.line_col_to_index((5,  4)),  27);  // d
        assert_eq!(buf.line_col_to_index((5,  5)),  28);  // !
        assert_eq!(buf.line_col_to_index((5,  6)),  28);  // invalid line
        assert_eq!(buf.line_col_to_index((5,  7)),  28);  // also

        // this is last valid index, which is 28 since
        // there is no /n
        assert_eq!(buf.line_col_to_index((10, 2)), 29);    // invalid line 
    }

    // add a \n at the end
    #[test]
    fn line_col_to_index_2() {
        let mut buf = Buffer::new();
        // remember there is an off the end line when doc ends in \n
        //  index        0           1           2          3
        //  index        012 345678 9012345 678 9012 34567890
        //  line         0   1      2       3   4    5       6
        //  col          012 012345 0123456 012 0123 0123456 0 
        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!\n", 0);
        assert!(buf.char_count() == 30);

        assert_eq!(buf.line_col_to_index((5,  0)),  23);  // w 
        assert_eq!(buf.line_col_to_index((5,  1)),  24);  // o
        assert_eq!(buf.line_col_to_index((5,  2)),  25);  // r
        assert_eq!(buf.line_col_to_index((5,  3)),  26);  // l
        assert_eq!(buf.line_col_to_index((5,  4)),  27);  // d
        assert_eq!(buf.line_col_to_index((5,  5)),  28);  // !
        assert_eq!(buf.line_col_to_index((5,  6)),  29);  // \n
        assert_eq!(buf.line_col_to_index((5,  7)),  29);  // clips to end of line

        assert_eq!(buf.line_col_to_index((6,  0)),  30);  // extra one beyond line
        assert_eq!(buf.line_col_to_index((6,  1)),  30);  // clips to final character

        // this is last valid index, which is 28 since
        // there is no /n
        assert_eq!(buf.line_col_to_index((10, 2)), 30);    // invalid line 
    }


    #[test]
    fn line_col_to_index_3() {
        // try on empty buffer
        let buf = Buffer::new();
        // should always be 0
        assert_eq!(buf.line_col_to_index((0,  0)),  0);  
        assert_eq!(buf.line_col_to_index((5,  0)),  0);  
        assert_eq!(buf.line_col_to_index((5,  1)),  0);  
        assert_eq!(buf.line_col_to_index((10,  10)),  0);  
    }

    #[test]
    fn line_col_to_index_4() {
        let mut buf = Buffer::new();
        //               01234 5012345 6
        //               012345 67890
        buf.insert_text("Hello\nworld!\n", 0); // is 13 characters
        println!("len_chars:{}",buf.char_count());

        assert_eq!(buf.line_col_to_index((0, 0)), 0);
        assert_eq!(buf.line_col_to_index((0, 1)), 1);
        assert_eq!(buf.line_col_to_index((0, 2)), 2);
        assert_eq!(buf.line_col_to_index((0, 3)), 3);
        assert_eq!(buf.line_col_to_index((0, 4)), 4);
        assert_eq!(buf.line_col_to_index((0, 5)), 5);  // last col in row 0 
        assert_eq!(buf.line_col_to_index((0, 6)), 5);  // invalid col in that row, return last
        assert_eq!(buf.line_col_to_index((0, 7)), 5);  // invalid col in that row, return last
        assert_eq!(buf.line_col_to_index((1, 0)), 6);
        assert_eq!(buf.line_col_to_index((1, 1)), 7);
        assert_eq!(buf.line_col_to_index((1, 2)), 8);
        assert_eq!(buf.line_col_to_index((1, 3)), 9);
        assert_eq!(buf.line_col_to_index((1, 4)), 10);
        assert_eq!(buf.line_col_to_index((1, 5)), 11);
        assert_eq!(buf.line_col_to_index((1, 6)), 12);   // last col in row 1
        assert_eq!(buf.line_col_to_index((1, 7)), 12);   // invalid col in that row, return last
        assert_eq!(buf.line_col_to_index((1, 100)), 12); // way beyond end of row 1
        assert_eq!(buf.line_col_to_index((2, 0)), 13);
        assert_eq!(buf.line_col_to_index((2, 1)), 13);
    }

    #[test]
    fn index_to_line_col_1() {
        let mut buf = Buffer::new();
        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);

        let pos = buf.index_to_line_col(5);

        assert!(pos == (1, 2));
    }

    #[test]
    fn index_to_line_col_2() {
        let mut buf = Buffer::new();
        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);

        let pos = buf.index_to_line_col(50);

        assert_eq!(pos, (5, 6));
    }

    #[test]
    fn index_to_line_col_3() {
        let mut buf = Buffer::new();
        buf.insert_text("Hello\nworld!\n", 0);

        assert_eq!(buf.index_to_line_col(0), (0, 0));
        assert_eq!(buf.index_to_line_col(5), (0, 5));
        assert_eq!(buf.index_to_line_col(6), (1, 0));
        assert_eq!(buf.index_to_line_col(12), (1, 6));
        assert_eq!(buf.index_to_line_col(13), (2, 0));
        assert_eq!(buf.index_to_line_col(14), (2, 0));
    }

    #[test]
    fn string_from_range_1() {
        let mut buf = Buffer::new();
        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);

        let s = buf.string_from_range(1, 12);

        assert!(&s[..] == "i\nthere\npeo");
    }

    #[test]
    fn string_from_range_2() {
        let mut buf = Buffer::new();
        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);

        let s = buf.string_from_range(0, 29);

        assert!(&s[..] == "Hi\nthere\npeople\nof\nthe\nworld!");
    }

    #[test]
    fn grapheme_iter_at_index_1() {
        let mut buf = Buffer::new();
        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);

        let mut iter = buf.grapheme_iter_at_index(16);

        assert!("o" == iter.next().unwrap());
        assert!("f" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("t" == iter.next().unwrap());
        assert!("h" == iter.next().unwrap());
        assert!("e" == iter.next().unwrap());
        assert!("\n" == iter.next().unwrap());
        assert!("w" == iter.next().unwrap());
        assert!("o" == iter.next().unwrap());
        assert!("r" == iter.next().unwrap());
        assert!("l" == iter.next().unwrap());
        assert!("d" == iter.next().unwrap());
        assert!("!" == iter.next().unwrap());
        assert!(None == iter.next());
    }

    #[test]
    fn grapheme_iter_at_index_2() {
        let mut buf = Buffer::new();
        buf.insert_text("Hi\nthere\npeople\nof\nthe\nworld!", 0);

        let mut iter = buf.grapheme_iter_at_index(29);

        assert!(None == iter.next());
    }

}
