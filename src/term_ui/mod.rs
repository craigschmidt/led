use std::thread::sleep;
use std::time;

use termion;
use termion::event::{Event, Key};
use termion::input::TermRead;
use editor::Editor;
use string_utils::rope_slice_is_line_ending;
use utils::digit_count;

#[allow(dead_code)] // don't care about unused colors
mod screen;

pub mod smallstring;

use self::screen::{Color, Screen, Style};

/// Generalized ui loop.
macro_rules! ui_loop {
    ($term_ui:ident,draw $draw:block,key_press($key:ident) $key_press:block) => {
        let mut stop = false;

        // Draw the editor to screen for the first time
        {
            $draw
        };
        $term_ui.screen.present();

        // UI loop
        loop {
            let mut should_redraw = false;

            // Handle input
            loop {
                match $term_ui.inp.next() {
                    Some(Ok(Event::Key($key))) => {
                        let (status, state_changed) = || -> (LoopStatus, bool) { $key_press }();
                        should_redraw |= state_changed;
                        if status == LoopStatus::Done {
                            stop = true;
                            break;
                        }
                    }

                    _ => {
                        break;
                    }
                }
            }

            // Check if we're done
            if stop || $term_ui.quit {
                break;
            }

            // Check for screen resize
            let (w, h) = termion::terminal_size().unwrap();
            // TODO: is the -1 for the header row????
            // why check this, why not $term_ui.width == w etc.????
            let needs_update = $term_ui.editor.update_dim(h as usize - 1, w as usize);
            if needs_update {
                $term_ui.width = w as usize;
                $term_ui.height = h as usize;
                $term_ui.screen.resize(w as usize, h as usize);
                $term_ui.editor.set_wrap_width_to_view_dim();
                should_redraw = true;
            }

            // Draw the editor to screen
            if should_redraw {
                {
                    $draw
                };
                $term_ui.screen.present();
            }

            // Sleep for a small bit so we don't just spin on the CPU
            sleep(time::Duration::from_millis(5));
        }
    };
}

pub struct TermUI {
    inp: termion::input::Events<termion::AsyncReader>,
    screen: Screen,
    editor: Editor,
    width: usize,
    height: usize,
    quit: bool,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum LoopStatus {
    Done,
    Continue,
}

impl TermUI {

    // pub fn new() -> TermUI {
    //     TermUI::new_from_editor(Editor::new())
    // }

    pub fn new_from_editor(ed: Editor) -> TermUI {
        let (w, h) = termion::terminal_size().unwrap();
        let mut editor = ed;
        editor.update_dim(h as usize - 1, w as usize);

        TermUI {
            inp: termion::async_stdin().events(),
            screen: Screen::new(),
            editor: editor,
            width: w as usize,
            height: h as usize,
            quit: false,
        }
    }

    pub fn main_ui_loop(&mut self) {
        // Hide cursor
        self.screen.hide_cursor();

        // Set terminal size info
        // TODO: update size if it changes
        let (w, h) = termion::terminal_size().unwrap();
        self.width = w as usize;
        self.height = h as usize;
        // TODO: room for title bar????
        self.editor.update_dim(self.height - 1, self.width);
        self.editor.set_wrap_width_to_view_dim();
        self.screen.resize(w as usize, h as usize);

        // Start the UI
        ui_loop!(
            self,

            // Draw
            draw {
                self.screen.clear(Color::Black);
                self.draw_editor(&self.editor, (0, 0), (self.height - 1, self.width - 1));
            },

            // Handle input
            key_press(key) {
                let mut state_changed = true;
                match key {
                    Key::Ctrl('q') => {
                        self.quit = true;
                        return (LoopStatus::Done, true);
                    }

                    Key::Ctrl('s') => {
                        self.editor.save_if_dirty();
                    }

                    Key::Ctrl('z') => {
                        self.editor.undo();
                    }

                    Key::Ctrl('y') => {
                        self.editor.redo();
                    }

                    Key::Ctrl('l') => {
                        self.go_to_line_ui_loop();
                    }

                    Key::PageUp => {
                        self.editor.page_up();
                    }

                    Key::PageDown => {
                        self.editor.page_down();
                    }

                    Key::Up => {
                        self.editor.cursor_up(1);
                    }

                    Key::Down => {
                        self.editor.cursor_down(1);
                    }

                    Key::Left => {
                        self.editor.cursor_left(1);
                    }

                    Key::Right => {
                        self.editor.cursor_right(1);
                    }

                    // use the desired line ending in 
                    // place of the literal \n
                    Key::Char('\n') => {
                        let nl = self.editor.get_line_ending();
                        self.editor.insert_text_at_cursor(nl);
                    }

                    // TODO: why special case this?
                    Key::Char(' ') => {
                        self.editor.insert_text_at_cursor(" ");
                    }

                    Key::Char('\t') => {
                        self.editor.insert_tab_at_cursor();
                    }

                    Key::Backspace => {
                        self.editor.backspace_at_cursor();
                    }

                    Key::Delete => {
                        self.editor.remove_text_in_front_of_cursor(1);
                    }

                    // Character
                    Key::Char(c) => {
                        self.editor.insert_text_at_cursor(&c.to_string()[..]);
                    }

                    _ => {
                        state_changed = false;
                    }
                }

                (LoopStatus::Continue, state_changed)
            }
        );
    }

    fn go_to_line_ui_loop(&mut self) {
        let style = Style(Color::Black, Color::Cyan);

        let mut cancel = false;
        let prefix = "Jump to line: ";
        let mut line = String::new();

        ui_loop!(
            self,

            // Draw
            draw {
                self.screen.clear(Color::Black);
                self.draw_editor(&self.editor, (0, 0), (self.height - 1, self.width - 1));
                for i in 0..self.width {
                    self.screen.draw(i, 0, " ", style);
                }
                self.screen.draw(1, 0, prefix, style);
                self.screen.draw(
                    prefix.len() + 1,
                    0,
                    &line[..],
                    style,
                );
            },

            // Handle input
            key_press(key) {
                let mut state_changed = true;
                match key {
                    Key::Esc => {
                        cancel = true;
                        return (LoopStatus::Done, true);
                    }

                    Key::Char('\n') => {
                        return (LoopStatus::Done, true);
                    }

                    Key::Backspace => {
                        line.pop();
                    }

                    // Character
                    Key::Char(c) => {
                        if c.is_numeric() {
                            line.push(c);
                        }
                    }

                    _ => {
                        state_changed = false;
                    }
                }

                return (LoopStatus::Continue, state_changed);
            }
        );

        // Jump to line!
        if !cancel {
            if let Ok(n) = line.parse() {
                let n2: usize = n; // Weird work-around: the type of n wasn't being inferred
                if n2 > 0 {
                    self.editor.jump_to_line(n2 - 1);
                } else {
                    self.editor.jump_to_line(0);
                }
            }
        }
    }

    fn draw_editor(
        &self,
        editor: &Editor,
        c1: (usize, usize),  // corner ???
        c2: (usize, usize),
    ) {
        let style = Style(Color::Black, Color::Cyan);

        // Fill in top row with info line color
        for i in c1.1..(c2.1 + 1) {
            self.screen.draw(i, c1.0, " ", style);
        }

        // Filename and dirty marker
        let filename = editor.get_file_path().display();
        let dirty_char = if editor.get_dirty() { "*" } else { "" };
        let name = format!("{}{}", filename, dirty_char);
        self.screen.draw(c1.1 + 1, c1.0, &name[..], style);

        // Percentage position in document
        let pstring = format!("{}%", editor.percentage_in_document());
        self.screen
            .draw(c2.1 - pstring.len().min(c2.1), c1.0, &pstring[..], style);

        // Text encoding info and tab style
        let nl = editor.get_line_ending_name(); // human readable form
        let soft_tabs_str = if editor.get_soft_tabs()
            { "spaces" } 
        else 
            { "tabs" };

        let info_line = format!(
            "UTF8:{}  {}:{}",
            nl, soft_tabs_str, editor.get_soft_tab_width() as usize
        );
        self.screen
            .draw(c2.1 - 30.min(c2.1), c1.0, &info_line[..], style);

        // Draw main text editing area
        self.draw_editor_text(editor, (c1.0 + 1, c1.1), c2);
    }

    fn draw_editor_text(
        &self,
        editor: &Editor,
        c1: (usize, usize),  // upper left corner (1, 0)     (row,col)
        c2: (usize, usize),  // bottom right corner (64, 91) (row,col)
    ) {
        // Calculate all the starting info
        let gutter_width = editor.get_gutter_width();  
        debug!("gutter_width:{}", gutter_width);  // i.e. 3
        
        //let mut char_index = editor.get_view_char_idx();
        //debug!("char_index:{}", char_index);  // 0 

        // get the line and column of the current editor.view_pos.0
        let (line_index, col_i) = editor.index_to_line_col_view_pos_row();  
        debug!("line_index:{}, col_i:{}", line_index, col_i);  // 0, 0
                
        // plain function, not method
        let (mut line_block_index, _huh) = Editor::block_index_and_offset(col_i);
        debug!("line_block_index:{}, _huh:{}", line_block_index, _huh); // 0, 0

        let mut char_index = editor.line_col_to_index(line_index, line_block_index);
        debug!("char_index:{}", char_index);  // 0 
                
        let vis_line_offset = editor.calc_vis_line_offset(line_index, 
                                                          line_block_index, 
                                                          char_index);
                
        debug!("vis_line_offset:{}", vis_line_offset);  // 0 

        let mut screen_line = c1.0 as isize - vis_line_offset as isize;
        debug!("screen_line:{}", screen_line);  // 1

        // column to start drawing screen of text 
        // skip gutter and and the col of c1.1
        let screen_col = c1.1 as isize + gutter_width as isize;
        debug!("screen_col:{}", screen_col); // 3  so the starting column of 

        // Fill in the gutter with the appropriate background
        // since we might not have a line number for all locations 
        // due to wrapping
        for y in c1.0..(c2.0 + 1) {
            for x in c1.1..(c1.1 + gutter_width - 1) {
                self.screen.draw(x, y, " ", Style(Color::White, Color::Blue));
            }
        }

        // the 1 based line number in the file, to print in gutter
        let mut line_num = line_index + 1;

        // line is ropey::RopeSlice
        for line in editor.line_iter_at_index(line_index) {
            // Print line number

            if line_block_index == 0 {

                // let dc = digit_count(line_num as u32, 10);
                debug!("{:?},{},{}", line, line.len_bytes(), line.len_chars());
            // gutter width is supposed to have one extra space at end, 
            // so gutter_width -1 gives max width of digit_count
            let lnx = c1.1 + (gutter_width - 1 - digit_count(line_num));
            let lny = screen_line as usize;
            if lny >= c1.0 && lny <= c2.0 {
                self.screen.draw(
                    lnx,
                    lny,
                    &format!("{}", line_num)[..],
                    Style(Color::White, Color::Blue),
                );
            }
            }

            // Loop through the graphemes of the line and print them to
            // the screen.
            let mut line_g_index: usize = 0;
            // reset these for the line
            let mut last_pos_y = 0;

            // with wrapped lines, this shows how many lines of screen the current line is using
            let mut lines_traversed: usize = 0;
            
            let mut g_iter = editor.vis_iter(line_block_index, &line);
            loop {
                // pos_y is the 0 based row of just the wrapped line by itself
                // pos_x is the 0 based column of just the wrapped line by itself
                // so if a line takes 3 rows to display, pos_y is 0,1,or 2
                if let Some((g, (pos_y, pos_x), width)) = g_iter.next() {
                  
                    let mut g_to_print = g.to_string();
                    // escape tabs and \n
                    // also track if last grapheme of last line is a line ending
                    if rope_slice_is_line_ending(&g) {
                        g_to_print = "LF".to_string();
                    }
                    if g_to_print == "\t".to_string() {
                        g_to_print = "TAB".to_string();
                    }
                    debug!("start: {}, pos_y:{}, pos_x:{}, width:{}, line_g_index:{}, last_pos_y:{}, lines_traversed:{}, line_block_index:{}", 
                    g_to_print, pos_y, pos_x, width, line_g_index, last_pos_y, lines_traversed, line_block_index);

                    // this is true when have a wrapped line
                    if last_pos_y != pos_y {
                        if last_pos_y < pos_y {
                            lines_traversed += pos_y - last_pos_y;
                        }
                        last_pos_y = pos_y;
                    }
                    // Calculate the cell coordinates at which to draw the grapheme
                    let px = pos_x as isize + screen_col - editor.get_vis_horizontal_offset() as isize;
                    let py = lines_traversed as isize + screen_line;

                    // If we're off the bottom, we're done
                    if py > c2.0 as isize {
                        return;
                    }

                    // Draw the grapheme to the screen if it's in bounds
                    if (px >= c1.1 as isize) && (py >= c1.0 as isize) && (px <= c2.1 as isize) {
                        // Check if the character is within a cursor
                        let at_cursor = editor.at_cursor(char_index);

                        // Actually print the character
                        // if off the end of the line, and a cursor
                        if rope_slice_is_line_ending(&g) {
                            if at_cursor {
                                self.screen.draw(
                                    px as usize,
                                    py as usize,
                                    " ",
                                    Style(Color::Black, Color::White),
                                );
                            }
                        } else if g == "\t" {
                            // print the right number of spaces for a tab
                            for i in 0..width {
                                let tpx = px as usize + i;
                                if tpx <= c2.1 {
                                    self.screen.draw(
                                        tpx as usize,
                                        py as usize,
                                        " ",
                                        Style(Color::White, Color::Black),
                                    );
                                }
                            }
                            // and maybe print a cursor
                            if at_cursor {
                                self.screen.draw(
                                    px as usize,
                                    py as usize,
                                    " ",
                                    Style(Color::Black, Color::White),
                                );
                            }
                        } else {
                            // just print a regular character
                            // either with as a cursor or not
                            if at_cursor {
                                self.screen.draw_rope_slice(
                                    px as usize,
                                    py as usize,
                                    &g,
                                    Style(Color::Black, Color::White),
                                );
                            } else {
                                self.screen.draw_rope_slice(
                                    px as usize,
                                    py as usize,
                                    &g,
                                    Style(Color::White, Color::Black),
                                );
                            }
                        }
                    }

                    char_index += g.chars().count();
                    line_g_index += 1;
                } else {
                    break;
                }

                // force a new line if this one is too long
                if editor.line_beyond_block_length(line_g_index) {
                    line_block_index += 1;
                    line_g_index = 0;
                                        
                    // get a new iterator
                    g_iter = editor.vis_iter(line_block_index, &line);
                    lines_traversed += 1;
                }
            }

            line_block_index = 0;
            screen_line += lines_traversed as isize + 1;
            line_num += 1;
        }

        // If we get here, it means we reached the end of the text buffer
        // without going off the bottom of the screen.  So draw the cursor
        // at the end if needed.

        // Check if the character is within a cursor
        let at_cursor = editor.at_cursor(char_index);

        debug!("after at_cursor, {}", at_cursor);

        if at_cursor {
            // Calculate the cell coordinates at which to draw the cursor
            let pos_x = editor
                .index_to_horizontal_v2d(editor.char_count());
            debug!("pos_x:{}", pos_x);
            let px = pos_x as isize + screen_col - editor.get_vis_horizontal_offset() as isize;
            debug!("px:{}",px);
            // TODO: this causes a bug when -1, should be -2
            // but then typing a CR doesn't advance to next line
            // how can it alter the state?
            // back up to the previous value of screen_line
            // note: bug doesn't happen when the screen is full of text!
            let py = screen_line - 1;

            // if last_line_ends_in_line_ending {
            //                 screen_line
            //             } else { 
            //                 screen_line - 1
            //             };
            // debug!("py:{}, {}", py, last_line_ends_in_line_ending);

            if (px >= c1.1 as isize) && (py >= c1.0 as isize) && (px <= c2.1 as isize) && (py <= c2.0 as isize)
            {
                self.screen.draw(
                    px as usize,
                    py as usize,
                    " ",
                    Style(Color::Black, Color::Red),
                );
            }
        }
    }

}
