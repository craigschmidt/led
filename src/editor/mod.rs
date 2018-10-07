use std::collections::HashMap;

use self::cursor::CursorSet;
use self::buffer::Buffer;
use self::formatter::LineFormatter;
use self::formatter::LineFormatterVisIter;
use self::formatter::RoundingBehavior::*;
use self::formatter::LINE_BLOCK_LENGTH;
use self::formatter::block_index_and_offset;
use ropey::{RopeSlice,iter};
use std::cmp::{max, min};
use std::path::{Path, PathBuf};
use string_utils::{char_count, rope_slice_to_line_ending, LineEnding};
use utils::{digit_count, RopeGraphemes};

mod buffer;
mod cursor;
mod formatter;

pub struct Editor {
    buffer: Buffer,
    formatter: LineFormatter,
    file_path: PathBuf,
    line_ending_type: LineEnding,
    soft_tabs: bool,
    soft_tab_width: u8,
    dirty: bool,   // does it need saving?

    // The dimensions of the total editor in screen space, including the
    // header, gutter, etc.
    editor_dim: (usize, usize),   // height, width

    // The dimensions and position of just the text view portion of the editor
    // which changes as the number of digits in the line numbering grows
    view_dim: (usize, usize), // (height, width)

    // TODO: understand these, 
    // is the the first character to display in the view?
    view_pos: (usize, usize), // (char index, visual horizontal offset)

    // The editing cursor positions
    cursors: CursorSet,
}

impl Editor {
    /// Create a new blank editor
    pub fn new() -> Editor {
        Editor {
            buffer: Buffer::new(),
            formatter: LineFormatter::new(4),
            file_path: PathBuf::new(),
            line_ending_type: LineEnding::LF,
            soft_tabs: false,
            soft_tab_width: 4,
            dirty: false,
            editor_dim: (0, 0),
            view_dim: (0, 0),
            view_pos: (0, 0),
            cursors: CursorSet::new(),
        }
    }

    pub fn new_from_file(path: &Path) -> Editor {
        let buf = match Buffer::new_from_file(path) {
            Ok(b) => b,
            // TODO: handle un-openable file better
            _ => panic!("Could not open file!"),
        };

        let mut ed = Editor {
            buffer: buf,
            formatter: LineFormatter::new(4),
            file_path: path.to_path_buf(),
            line_ending_type: LineEnding::LF,
            soft_tabs: false,
            soft_tab_width: 4,
            dirty: false,
            editor_dim: (0, 0),
            view_dim: (0, 0),
            view_pos: (0, 0),
            cursors: CursorSet::new(),
        };

        // For multiple-cursor testing
        // let mut cur = Cursor::new();
        // cur.range.0 = 30;
        // cur.range.1 = 30;
        // ed.update_vis_start(&mut cur);
        // ed.cursors.add_cursor(cur);

        ed.auto_detect_line_ending();
        ed.auto_detect_indentation_style();

        return ed;
    }

    pub fn save_if_dirty(&mut self) {
        if self.dirty && self.file_path != PathBuf::new() {
            let _ = self.buffer.save_to_file(&self.file_path);
            self.dirty = false;
        }
    }

    pub fn auto_detect_line_ending(&mut self) {
        let mut line_ending_histogram: [usize; 8] = [0, 0, 0, 0, 0, 0, 0, 0];

        // Collect statistics on the first 100 lines
        for line in self.buffer.line_iter().take(100) {
            // Get the line ending
            let ending = if line.len_chars() == 1 {
                let g = RopeGraphemes::new(&line.slice((line.len_chars() - 1)..))
                    .last()
                    .unwrap();
                rope_slice_to_line_ending(&g)
            } else if line.len_chars() > 1 {
                let g = RopeGraphemes::new(&line.slice((line.len_chars() - 2)..))
                    .last()
                    .unwrap();
                rope_slice_to_line_ending(&g)
            } else {
                LineEnding::None
            };

            // Record which line ending it is
            match ending {
                LineEnding::None => {}
                LineEnding::CRLF => {
                    line_ending_histogram[0] += 1;
                }
                LineEnding::LF => {
                    line_ending_histogram[1] += 1;
                }
                LineEnding::VT => {
                    line_ending_histogram[2] += 1;
                }
                LineEnding::FF => {
                    line_ending_histogram[3] += 1;
                }
                LineEnding::CR => {
                    line_ending_histogram[4] += 1;
                }
                LineEnding::NEL => {
                    line_ending_histogram[5] += 1;
                }
                LineEnding::LS => {
                    line_ending_histogram[6] += 1;
                }
                LineEnding::PS => {
                    line_ending_histogram[7] += 1;
                }
            }
        }

        // Analyze stats and make a determination
        let mut lei = 0;
        let mut le_count = 0;
        for i in 0usize..8 {
            if line_ending_histogram[i] >= le_count {
                lei = i;
                le_count = line_ending_histogram[i];
            }
        }

        if le_count > 0 {
            self.line_ending_type = match lei {
                0 => LineEnding::CRLF,
                1 => LineEnding::LF,
                2 => LineEnding::VT,
                3 => LineEnding::FF,
                4 => LineEnding::CR,
                5 => LineEnding::NEL,
                6 => LineEnding::LS,
                7 => LineEnding::PS,

                _ => LineEnding::LF,
            };
        }
    }

    pub fn auto_detect_indentation_style(&mut self) {
        let mut tab_blocks: usize = 0;
        let mut space_blocks: usize = 0;
        let mut space_histogram: HashMap<usize, usize> = HashMap::new();

        let mut last_indent = (false, 0usize); // (was_tabs, indent_count)

        // Collect statistics on the first 1000 lines
        for line in self.buffer.line_iter().take(1000) {
            let mut c_iter = line.chars();
            match c_iter.next() {
                Some('\t') => {
                    // Count leading tabs
                    let mut count = 1;
                    for c in c_iter {
                        if c == '\t' {
                            count += 1;
                        } else {
                            break;
                        }
                    }

                    // Update stats
                    if last_indent.0 && last_indent.1 < count {
                        tab_blocks += 1;
                    }

                    // Store last line info
                    last_indent = (true, count);
                }

                Some(' ') => {
                    // Count leading spaces
                    let mut count = 1;
                    for c in c_iter {
                        if c == ' ' {
                            count += 1;
                        } else {
                            break;
                        }
                    }

                    // Update stats
                    if !last_indent.0 && last_indent.1 < count {
                        space_blocks += 1;
                        let amount = count - last_indent.1;
                        *space_histogram.entry(amount).or_insert(0) += 1;
                    }

                    // Store last line info
                    last_indent = (false, count);
                }

                _ => {}
            }
        }

        // Analyze stats and make a determination
        if space_blocks == 0 && tab_blocks == 0 {
            return;
        }

        if space_blocks > (tab_blocks * 2) {
            let mut width = 0;
            let mut width_count = 0;
            for (w, count) in space_histogram.iter() {
                if *count > width_count {
                    width = *w;
                    width_count = *count;
                }
            }

            self.soft_tabs = true;
            self.soft_tab_width = width as u8;
        } else {
            self.soft_tabs = false;
        }
    }

    /// Updates the view dimensions, and returns whether that
    /// actually changed anything.
    pub fn update_dim(&mut self, h: usize, w: usize) -> bool {
        let line_count_digits = digit_count(self.buffer.line_count() as u32, 10) as usize;
        // only need to update if editor_dim changed
        if self.editor_dim.0 != h || self.editor_dim.1 != w {
            self.editor_dim = (h, w);

            // Minus 1 vertically for the header, minus one more than the digits in
            // the line count for the gutter.
            // TODO: generalize this
            self.view_dim = (
                self.editor_dim.0 - 1,
                self.editor_dim.1 - line_count_digits - 1,
            );
            return true;
        } else if self.view_dim.1 != (self.editor_dim.1 - line_count_digits - 1) {
            // or if the line count changes this needs to be updated
            debug!("update_dim:{},{}", self.buffer.line_count(), line_count_digits);
            self.view_dim.1 = self.editor_dim.1 - line_count_digits - 1;
            return true;
        } else {
            return false;
        }
    }

    pub fn undo(&mut self) {
        // TODO: handle multiple cursors properly
        if let Some(pos) = self.buffer.undo() {
            self.cursors.truncate(1);
            self.cursors[0].range.0 = pos;
            self.cursors[0].range.1 = pos;
            self.cursors[0].vis_start = self.formatter.index_to_horizontal_v2d(&self.buffer, self.cursors[0].range.0);
            self.move_view_to_cursor();

            self.dirty = true;

            self.cursors.make_consistent();
        }
    }

    pub fn redo(&mut self) {
        // TODO: handle multiple cursors properly
        if let Some(pos) = self.buffer.redo() {
            self.cursors.truncate(1);
            self.cursors[0].range.0 = pos;
            self.cursors[0].range.1 = pos;
            self.cursors[0].vis_start = self.formatter.index_to_horizontal_v2d(&self.buffer, self.cursors[0].range.0);
            self.move_view_to_cursor();

            self.dirty = true;

            self.cursors.make_consistent();
        }
    }

    /// Moves the editor's view the minimum amount to show the cursor
    pub fn move_view_to_cursor(&mut self) {
        // TODO: account for the horizontal offset of the editor view.

        // TODO: handle multiple cursors properly.  Should only move if
        // there are no cursors currently in view, and should jump to
        // the closest cursor.

        // Find the first and last char index visible within the editor.
        let c_first =
            self.formatter
                .index_set_horizontal_v2d(&self.buffer, self.view_pos.0, 0, Floor);
        let mut c_last = self.formatter.index_offset_vertical_v2d(
            &self.buffer,
            c_first,
            self.view_dim.0 as isize,
            (Floor, Floor),
        );
        c_last =
            self.formatter
                .index_set_horizontal_v2d(&self.buffer, c_last, self.view_dim.1, Floor);

        // Adjust the view depending on where the cursor is
        if self.cursors[0].range.0 < c_first {
            self.view_pos.0 = self.cursors[0].range.0;
        } else if self.cursors[0].range.0 > c_last {
            self.view_pos.0 = self.formatter.index_offset_vertical_v2d(
                &self.buffer,
                self.cursors[0].range.0,
                -(self.view_dim.0 as isize),
                (Floor, Floor),
            );
        }
    }

    pub fn insert_text_at_cursor(&mut self, text: &str) {
        self.cursors.make_consistent();

        let str_len = char_count(text);
        let mut offset = 0;

        for c in self.cursors.iter_mut() {
            // Insert text
            self.buffer.insert_text(text, c.range.0 + offset);
            self.dirty = true;

            // Move cursor
            c.range.0 += str_len + offset;
            c.range.1 += str_len + offset;
            c.vis_start = self.formatter.index_to_horizontal_v2d(&self.buffer, c.range.0);

            // Update offset
            offset += str_len;
        }

        // Adjust view
        self.move_view_to_cursor();
    }

    pub fn insert_tab_at_cursor(&mut self) {
        self.cursors.make_consistent();

        if self.soft_tabs {
            let mut offset = 0;

            for c in self.cursors.iter_mut() {
                // Update cursor with offset
                c.range.0 += offset;
                c.range.1 += offset;

                // Figure out how many spaces to insert
                let vis_pos = self
                    .formatter
                    .index_to_horizontal_v2d(&self.buffer, c.range.0);
                // TODO: handle tab settings
                let next_tab_stop =
                    ((vis_pos / self.soft_tab_width as usize) + 1) * self.soft_tab_width as usize;
                let space_count = min(next_tab_stop - vis_pos, 8);

                // Insert spaces
                let space_strs = [
                    "", " ", "  ", "   ", "    ", "     ", "      ", "       ", "        ",
                ];
                self.buffer.insert_text(space_strs[space_count], c.range.0);
                self.dirty = true;

                // Move cursor
                c.range.0 += space_count;
                c.range.1 += space_count;
                c.vis_start = self.formatter.index_to_horizontal_v2d(&self.buffer, c.range.0);

                // Update offset
                offset += space_count;
            }

            // Adjust view
            self.move_view_to_cursor();
        } else {
            self.insert_text_at_cursor("\t");
        }
    }

    pub fn backspace_at_cursor(&mut self) {
        self.remove_text_behind_cursor(1);
    }

    pub fn remove_text_behind_cursor(&mut self, grapheme_count: usize) {
        self.cursors.make_consistent();

        let mut offset = 0;

        for c in self.cursors.iter_mut() {
            // Update cursor with offset
            c.range.0 -= offset;
            c.range.1 -= offset;

            // Do nothing if there's nothing to delete.
            if c.range.0 == 0 {
                continue;
            }

            let len = c.range.0 - self.buffer.nth_prev_grapheme(c.range.0, grapheme_count);

            // Remove text
            self.buffer.remove_text_before(c.range.0, len);
            self.dirty = true;

            // Move cursor
            c.range.0 -= len;
            c.range.1 -= len;
            c.vis_start = self.formatter.index_to_horizontal_v2d(&self.buffer, c.range.0);

            // Update offset
            offset += len;
        }

        self.cursors.make_consistent();

        // Adjust view
        self.move_view_to_cursor();
    }

    pub fn remove_text_in_front_of_cursor(&mut self, grapheme_count: usize) {
        self.cursors.make_consistent();

        let mut offset = 0;

        for c in self.cursors.iter_mut() {
            // Update cursor with offset
            c.range.0 -= min(c.range.0, offset);
            c.range.1 -= min(c.range.1, offset);

            // Do nothing if there's nothing to delete.
            if c.range.1 == self.buffer.char_count() {
                return;
            }

            let len = self.buffer.nth_next_grapheme(c.range.1, grapheme_count) - c.range.1;

            // Remove text
            self.buffer.remove_text_after(c.range.1, len);
            self.dirty = true;

            // Move cursor
            c.vis_start = self.formatter.index_to_horizontal_v2d(&self.buffer, c.range.0);

            // Update offset
            offset += len;
        }

        self.cursors.make_consistent();

        // Adjust view
        self.move_view_to_cursor();
    }

    // note: unused
    pub fn _remove_text_inside_cursor(&mut self) {
        self.cursors.make_consistent();

        let mut offset = 0;

        for c in self.cursors.iter_mut() {
            // Update cursor with offset
            c.range.0 -= min(c.range.0, offset);
            c.range.1 -= min(c.range.1, offset);

            // If selection, remove text
            if c.range.0 < c.range.1 {
                let len = c.range.1 - c.range.0;

                self.buffer
                    .remove_text_before(c.range.0, c.range.1 - c.range.0);
                self.dirty = true;

                // Move cursor
                c.range.1 = c.range.0;

                // Update offset
                offset += len;
            }

            c.vis_start = self.formatter.index_to_horizontal_v2d(&self.buffer, c.range.0);
        }

        self.cursors.make_consistent();

        // Adjust view
        self.move_view_to_cursor();
    }

    // note: unused
    pub fn _cursor_to_beginning_of_buffer(&mut self) {
        self.cursors = CursorSet::new();

        self.cursors[0].range = (0, 0);
        self.cursors[0].vis_start = self.formatter.index_to_horizontal_v2d(&self.buffer, self.cursors[0].range.0);

        // Adjust view
        self.move_view_to_cursor();
    }

    pub fn _cursor_to_end_of_buffer(&mut self) {
        let end = self.buffer.char_count();

        self.cursors = CursorSet::new();
        self.cursors[0].range = (end, end);
        self.cursors[0].vis_start = self.formatter.index_to_horizontal_v2d(&self.buffer, self.cursors[0].range.0);

        // Adjust view
        self.move_view_to_cursor();
    }

    pub fn cursor_left(&mut self, n: usize) {
        for c in self.cursors.iter_mut() {
            c.range.0 = self.buffer.nth_prev_grapheme(c.range.0, n);
            c.range.1 = c.range.0;
            c.vis_start = self.formatter.index_to_horizontal_v2d(&self.buffer, c.range.0);
        }

        // Adjust view
        self.move_view_to_cursor();
    }

    pub fn cursor_right(&mut self, n: usize) {
        for c in self.cursors.iter_mut() {
            c.range.1 = self.buffer.nth_next_grapheme(c.range.1, n);
            c.range.0 = c.range.1;
            c.vis_start = self.formatter.index_to_horizontal_v2d(&self.buffer, c.range.0);
        }

        // Adjust view
        self.move_view_to_cursor();
    }

    pub fn cursor_up(&mut self, n: usize) {
        for c in self.cursors.iter_mut() {
            let vmove = -1 * (n * self.formatter.single_line_height()) as isize;

            let mut temp_index = self.formatter.index_offset_vertical_v2d(
                &self.buffer,
                c.range.0,
                vmove,
                (Round, Round),
            );
            temp_index = self.formatter.index_set_horizontal_v2d(
                &self.buffer,
                temp_index,
                c.vis_start,
                Round,
            );

            if !self.buffer.is_grapheme(temp_index) {
                temp_index = self.buffer.nth_prev_grapheme(temp_index, 1);
            }

            if temp_index == c.range.0 {
                // We were already at the top.
                c.range.0 = 0;
                c.range.1 = 0;
                c.vis_start = self.formatter.index_to_horizontal_v2d(&self.buffer, c.range.0);

            } else {
                c.range.0 = temp_index;
                c.range.1 = temp_index;
            }
        }

        // Adjust view
        self.move_view_to_cursor();
    }

    pub fn cursor_down(&mut self, n: usize) {
        for c in self.cursors.iter_mut() {
            let vmove = (n * self.formatter.single_line_height()) as isize;

            let mut temp_index = self.formatter.index_offset_vertical_v2d(
                &self.buffer,
                c.range.0,
                vmove,
                (Round, Round),
            );
            temp_index = self.formatter.index_set_horizontal_v2d(
                &self.buffer,
                temp_index,
                c.vis_start,
                Round,
            );

            if !self.buffer.is_grapheme(temp_index) {
                temp_index = self.buffer.nth_prev_grapheme(temp_index, 1);
            }

            if temp_index == c.range.0 {
                // We were already at the bottom.
                c.range.0 = self.buffer.char_count();
                c.range.1 = self.buffer.char_count();
                c.vis_start = self.formatter.index_to_horizontal_v2d(&self.buffer, c.range.0);

            } else {
                c.range.0 = temp_index;
                c.range.1 = temp_index;
            }
        }

        // Adjust view
        self.move_view_to_cursor();
    }

    pub fn page_up(&mut self) {
        let move_amount =
            self.view_dim.0 - max(self.view_dim.0 / 8, self.formatter.single_line_height());
        self.view_pos.0 = self.formatter.index_offset_vertical_v2d(
            &self.buffer,
            self.view_pos.0,
            -1 * move_amount as isize,
            (Round, Round),
        );

        self.cursor_up(move_amount);

        // Adjust view
        self.move_view_to_cursor();
    }

    pub fn page_down(&mut self) {
        let move_amount =
            self.view_dim.0 - max(self.view_dim.0 / 8, self.formatter.single_line_height());
        self.view_pos.0 = self.formatter.index_offset_vertical_v2d(
            &self.buffer,
            self.view_pos.0,
            move_amount as isize,
            (Round, Round),
        );

        self.cursor_down(move_amount);

        // Adjust view
        self.move_view_to_cursor();
    }

    pub fn jump_to_line(&mut self, n: usize) {
        let pos = self.buffer.line_col_to_index((n, 0));
        self.cursors.truncate(1);
        self.cursors[0].range.0 = self.formatter.index_set_horizontal_v2d(
            &self.buffer,
            pos,
            self.cursors[0].vis_start,
            Round,
        );
        self.cursors[0].range.1 = self.cursors[0].range.0;

        // Adjust view
        self.move_view_to_cursor();
    }

    // encapsulate buffer routines

    /// Converts a line number and char-column number into a char index.
    // map to pos tuple
    pub fn line_col_to_index(&self, line_index: usize, line_block_index : usize) -> usize {
        self.buffer.line_col_to_index((line_index, line_block_index * LINE_BLOCK_LENGTH))
    }

    /// Converts a char index into a line number and char-column
    /// number.
    ///
    /// get the line and column of the current view_pos.0
    pub fn index_to_line_col_view_pos_row(&self) -> (usize, usize) {
        self.buffer.index_to_line_col(self.view_pos.0)
    }

    pub fn char_count(&self) -> usize {
        self.buffer.char_count()
    } 

    pub fn line_iter_at_index<'a>(&'a self, line_idx: usize) -> iter::Lines<'a> {
        self.buffer.line_iter_at_index(line_idx)
    }

    // encapsulate routines for both formatter and buffer

    pub fn calc_vis_line_offset(&self, line_index : usize, 
                                       line_block_index : usize, 
                                       char_index : usize) -> usize {

        let temp_line = self.buffer.get_line(line_index);
        
        let (vis_line_offset, _) = self.formatter.index_to_v2d(
            RopeGraphemes::new(&temp_line.slice(
                (line_block_index * LINE_BLOCK_LENGTH)
                    ..min(
                        temp_line.len_chars(),
                        (line_block_index + 1) * LINE_BLOCK_LENGTH,
                    ),
            )),
            self.view_pos.0 - char_index,
        );
        vis_line_offset 
    }

    pub fn index_to_horizontal_v2d(&self, char_idx: usize) -> usize {
        self.formatter.index_to_horizontal_v2d(&self.buffer, char_idx)
    }

    // TODO: what's the deal with the _?
    // TODO: stopped here
    pub fn vis_iter<'a>(&'a self, line_block_index: usize, line : &'a RopeSlice) -> 
    LineFormatterVisIter<'a, RopeGraphemes<'a>> {

        let line_len = line.len_chars();

        self.formatter.iter(RopeGraphemes::new(
                &line.slice((line_block_index * LINE_BLOCK_LENGTH)..line_len),
        ))
    }

    pub fn line_beyond_block_length(&self, line_g_index : usize) -> bool {
        line_g_index >= LINE_BLOCK_LENGTH
    }

    // since formatter is private
    pub fn set_wrap_width_to_view_dim(&mut self) {
        self.formatter.set_wrap_width(self.view_dim.1)
    }

    // getters 
    pub fn get_file_path(&self) -> &PathBuf {
        &self.file_path
    }

    pub fn get_soft_tabs(&self) -> bool { 
        self.soft_tabs
    }

    pub fn get_soft_tab_width(&self) -> u8 {
        self.soft_tab_width
    }

    pub fn get_dirty(&self) -> bool {
        self.dirty
    }

    // LineEnding is copy
    pub fn get_line_ending_type(&self) -> LineEnding {
        self.line_ending_type
    }

    // the left gutter is the difference in space between the editor and view width
    pub fn get_gutter_width(&self) -> usize { 
        self.editor_dim.1 - self.view_dim.1
    }

    // TODO: remove me at some point, or break off from view_pos.0
    // is just always 0
    pub fn get_vis_horizontal_offset(&self) -> usize {
        self.view_pos.1
    }


    // Check if the character is within a cursor
    pub fn at_cursor(&self, char_index : usize) -> bool {
        for c in self.cursors.iter() {
            if char_index >= c.range.0 && char_index <= c.range.1 {
                return true;
            }
        }
        return false;
    }

    // Percentage position in document
    // TODO: use view instead of cursor for calculation if there is more
    // than one cursor.
    pub fn percentage_in_document(&self) -> usize {

        // if document is empty, then show 100
        if self.char_count() == 0 {
            return 100
        } 
        (((self.cursors[0].range.0 as f32) / (self.char_count() as f32)) * 100.0) as usize
    }

    #[allow(dead_code)] // for later
    pub fn line_counts(&self) -> usize { 
        self.buffer.line_count()
    }


    // just a plain function
    pub fn block_index_and_offset(index: usize) -> (usize, usize) {
        block_index_and_offset(index)
    }


}

// 