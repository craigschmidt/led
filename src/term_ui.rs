#![allow(dead_code)]

use rustbox;
use rustbox::Color;
use editor::Editor;
use std::char;
use std::time::duration::Duration;
use string_utils::{is_line_ending};

// Key codes
const K_ENTER: u16 = 13;
const K_TAB: u16 = 9;
const K_SPACE: u16 = 32;
const K_BACKSPACE: u16 = 127;
const K_PAGEUP: u16 = 65519;
const K_PAGEDOWN: u16 = 65518;
const K_UP: u16 = 65517;
const K_DOWN: u16 = 65516;
const K_LEFT: u16 = 65515;
const K_RIGHT: u16 = 65514;
const K_ESC: u16 = 27;
const K_CTRL_Q: u16 = 17;
const K_CTRL_S: u16 = 19;


pub struct TermUI {
    rb: rustbox::RustBox,
    editor: Editor,
}


impl TermUI {
    pub fn new() -> TermUI {
        TermUI {
            rb: rustbox::RustBox::init(&[Some(rustbox::InitOption::BufferStderr)]).unwrap(),
            editor: Editor::new(),
        }
    }
    
    pub fn new_from_editor(editor: Editor) -> TermUI {
        TermUI {
            rb: rustbox::RustBox::init(&[Some(rustbox::InitOption::BufferStderr)]).unwrap(),
            editor: editor,
        }
    }
    
    pub fn ui_loop(&mut self) {
        // Quitting flag
        let mut quit = false;
    
        let mut width = self.rb.width();
        let mut height = self.rb.height();
        self.editor.update_dim(height, width);
    
        loop {
            // Draw the editor to screen
            self.rb.clear();
            self.draw_editor(&self.editor, (0, 0), (height-1, width-1));
            self.rb.present();
            
            
            // Handle events.  We block on the first event, so that the
            // program doesn't loop like crazy, but then continue pulling
            // events in a non-blocking way until we run out of events
            // to handle.
            let mut e = self.rb.poll_event(); // Block until we get an event
            loop {
                match e {
                    Ok(rustbox::Event::KeyEvent(modifier, key, character)) => {
                        //println!("      {} {} {}", modifier, key, character);
                        match key {
                            K_CTRL_Q | K_ESC => {
                                quit = true;
                                break;
                            },
                            
                            K_CTRL_S => {
                                self.editor.save_if_dirty();
                            },
                            
                            K_PAGEUP => {
                                self.editor.page_up();
                            },
                            
                            K_PAGEDOWN => {
                                self.editor.page_down();
                            },
                            
                            K_UP => {
                                self.editor.cursor_up(1);
                            },
                            
                            K_DOWN => {
                                self.editor.cursor_down(1);
                            },
                            
                            K_LEFT => {
                                self.editor.cursor_left(1);
                            },
                            
                            K_RIGHT => {
                                self.editor.cursor_right(1);
                            },
                            
                            K_ENTER => {
                                self.editor.insert_text_at_cursor("\n");
                            },
                            
                            K_SPACE => {
                                self.editor.insert_text_at_cursor(" ");
                            },
                            
                            K_TAB => {
                                self.editor.insert_text_at_cursor("\t");
                            },
                            
                            K_BACKSPACE => {
                                self.editor.remove_text_behind_cursor(1);
                            },
                            
                            // Character
                            0 => {
                                if let Option::Some(c) = char::from_u32(character) {
                                    self.editor.insert_text_at_cursor(c.to_string().as_slice());
                                }
                            },
                            
                            _ => {}
                        }
                    },
                    
                    Ok(rustbox::Event::ResizeEvent(w, h)) => {
                        width = w as uint;
                        height = h as uint;
                        self.editor.update_dim(height, width);
                    }
                    
                    _ => {
                        break;
                    }
                }
    
                e = self.rb.peek_event(Duration::milliseconds(0)); // Get next event (if any)
            }
            
            
            // Quit if quit flag is set
            if quit {
                break;
            }
        }
    }


    pub fn draw_editor(&self, editor: &Editor, c1: (uint, uint), c2: (uint, uint)) {
        let mut line_iter = editor.buffer.line_iter_at_index(editor.view_pos.0);
        
        let mut grapheme_index;
        
        let mut vis_line_num = editor.view_pos.0;
        let mut vis_col_num = editor.view_pos.1;
        
        let mut print_line_num = c1.0;
        let mut print_col_num = c1.1;
        
        let max_print_line = c2.0 - c1.0;
        let max_print_col = c2.1 - c1.1;
        
        loop {
            if let Some(line) = line_iter.next() {
                let mut g_iter = line.grapheme_vis_iter();
                let excess = g_iter.skip_vis_positions(editor.view_pos.1);
                
                vis_col_num += excess;
                print_col_num += excess;
                
                grapheme_index = editor.buffer.pos_vis_2d_to_closest_1d((vis_line_num, vis_col_num));
                
                for (g, pos, width) in g_iter {
                    print_col_num = pos - editor.view_pos.1;
                    
                    if is_line_ending(g) {
                        if grapheme_index == editor.cursor.range.0 {
                            self.rb.print(print_col_num, print_line_num, rustbox::RB_NORMAL, Color::Black, Color::White, " ");
                        }
                    }
                    else if g == "\t" {
                        for i in range(print_col_num, print_col_num + width) {
                            self.rb.print(i, print_line_num, rustbox::RB_NORMAL, Color::White, Color::Black, " ");
                        }
                        
                        if grapheme_index == editor.cursor.range.0 {
                            self.rb.print(print_col_num, print_line_num, rustbox::RB_NORMAL, Color::Black, Color::White, " ");
                        }
                    }
                    else {
                        if grapheme_index == editor.cursor.range.0 {
                            self.rb.print(print_col_num, print_line_num, rustbox::RB_NORMAL, Color::Black, Color::White, g);
                        }
                        else {
                            self.rb.print(print_col_num, print_line_num, rustbox::RB_NORMAL, Color::White, Color::Black, g);
                        }
                    }
                    
                    vis_col_num += width;
                    grapheme_index += 1;
                    print_col_num += width;
                    
                    if print_col_num > max_print_col {
                        break;
                    }
                }
            }
            else {
                break;
            }
            
            vis_line_num += 1;
            print_line_num += 1;
            vis_col_num = editor.view_pos.1;
            
            if print_line_num > max_print_line {
                break;
            }
        }
        
        // Print cursor if it's at the end of the text, and thus wasn't printed
        // already.
        if editor.cursor.range.0 >= editor.buffer.len() {
            let vis_cursor_pos = editor.buffer.pos_1d_to_closest_vis_2d(editor.cursor.range.0);
                if (vis_cursor_pos.0 >= editor.view_pos.0) && (vis_cursor_pos.1 >= editor.view_pos.1) {
                let print_cursor_pos = (vis_cursor_pos.0 - editor.view_pos.0, vis_cursor_pos.1 - editor.view_pos.1);
                
                if print_cursor_pos.0 >= c1.0 && print_cursor_pos.0 <= c2.0 && print_cursor_pos.1 >= c1.1 && print_cursor_pos.1 <= c2.1 {
                    self.rb.print(print_cursor_pos.1, print_cursor_pos.0, rustbox::RB_NORMAL, Color::Black, Color::White, " ");
                }
            }
        }
    }
    
    
}