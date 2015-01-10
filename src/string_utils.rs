#![allow(dead_code)]
//! Misc helpful utility functions for TextBuffer related stuff.

pub fn is_line_ending(text: &str) -> bool {
    match text {
        "\u{000D}\u{000A}"
        | "\u{000A}"
        | "\u{000B}"
        | "\u{000C}"
        | "\u{000D}"
        | "\u{0085}"
        | "\u{2028}"
        | "\u{2029}" => true,
        
        _ => false
    }
}

pub fn newline_count(text: &str) -> usize {
    let mut count = 0;
    for c in text.chars() {
        if c == '\n' {
            count += 1;
        }
    }
    return count;
}

pub fn char_count(text: &str) -> usize {
    let mut count = 0;
    for _ in text.chars() {
        count += 1;
    }
    return count;
}

pub fn grapheme_count(text: &str) -> usize {
    let mut count = 0;
    for _ in text.graphemes(true) {
        count += 1;
    }
    return count;
}

pub fn char_and_newline_count(text: &str) -> (usize, usize) {
    let mut char_count = 0;
    let mut newline_count = 0;
    
    for c in text.chars() {
        char_count += 1;
        if c == '\n' {
            newline_count += 1;
        }
    }
    
    return (char_count, newline_count);
}

pub fn char_pos_to_byte_pos(text: &str, pos: usize) -> usize {
    let mut i: usize = 0;
    
    for (offset, _) in text.char_indices() {
        if i == pos {
            return offset;
        }
        i += 1;
    }
    
    if i == pos {
        return text.len();
    }
    
    panic!("char_pos_to_byte_pos(): char position off the end of the string.");
}

pub fn grapheme_pos_to_byte_pos(text: &str, pos: usize) -> usize {
    let mut i: usize = 0;
    
    for (offset, _) in text.grapheme_indices(true) {
        if i == pos {
            return offset;
        }
        i += 1;
    }
    
    if i == pos {
        return text.len();
    }
    
    panic!("grapheme_pos_to_byte_pos(): grapheme position off the end of the string.");
}