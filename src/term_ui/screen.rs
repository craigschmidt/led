// use std;
use std::cell::RefCell;
use std::io;
use std::io::{BufWriter, Write};
use std::iter;
use std::fmt;

use super::smallstring::SmallString;
use ropey::RopeSlice;
use termion;
use termion::color;
use termion::color::Rgb;
use termion::raw::{IntoRawMode, RawTerminal};
use termion::screen::AlternateScreen;
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;
use utils::{grapheme_width, RopeGraphemes};

pub(crate) struct Screen {
    // TODO: convert this for the mouse version
    out: RefCell<AlternateScreen<RawTerminal<BufWriter<io::Stdout>>>>,
    // store the style and grapheme and an Option(usize) for the 
    // char_idx in the current buffer if the start of the grapheme
    buf: RefCell<Vec<Option<(Style, SmallString, Option<usize>)>>>,
    w: usize,   // width of screen
    h: usize,   // height of screen
}

impl Screen {
    pub(crate) fn new() -> Self {
        let (w, h) = termion::terminal_size().unwrap();
        let buf = iter::repeat(Some((Style(Color::Background, Color::Background), " ".into(), None)))
            .take(w as usize * h as usize)
            .collect();
        Screen {
            out: RefCell::new(AlternateScreen::from(
                // TODO: magic number
                BufWriter::with_capacity(1 << 14, io::stdout())
                    .into_raw_mode()
                    .unwrap(),
            )),
            buf: RefCell::new(buf),
            w: w as usize,
            h: h as usize,
        }
    }

    pub(crate) fn clear(&self, col: Color) {
        for cell in self.buf.borrow_mut().iter_mut() {
            match *cell {
                Some((ref mut style, ref mut text, ref mut char_idx)) => {
                    *style = Style(col, col);
                    text.clear();
                    text.push_str(" ");
                    *char_idx = None;
                }
                _ => {
                    *cell = Some((Style(col, col), " ".into(), None));
                }
            }
        }
    }

    pub(crate) fn resize(&mut self, w: usize, h: usize) {
        self.w = w;
        self.h = h;
        self.buf
            .borrow_mut()
            .resize(w * h, Some((Style(Color::Background, Color::Background), " ".into(), None)));
    }

    pub(crate) fn present(&self) {
        let mut out = self.out.borrow_mut();
        let buf = self.buf.borrow();

        let mut last_style = Style(Color::Background, Color::Background);
        write!(out, "{}", last_style).unwrap();

        // Write everything to the tmp_string first.
        for y in 0..self.h {
            let mut x = 0;
            // Goto coordinates are 1 based so + 1
            write!(out, "{}", termion::cursor::Goto(1, y as u16 + 1)).unwrap();
            while x < self.w {
                if let Some((style, ref text, _char_idx)) = buf[y * self.w + x] {
                    if style != last_style {
                        write!(out, "{}", style).unwrap();
                        last_style = style;
                    }
                    write!(out, "{}", text).unwrap();
                    x += 1;
                } else {
                    x += 1;
                }
            }
        }

        // Make sure everything is written out
        out.flush().unwrap();
    }

    // draw a string slice, like the numbers in the gutter, or the header
    // this is for non-buffer text, so set the char_idx to None always 
    pub(crate) fn draw(&self, x: usize, y: usize, text: &str, style: Style) {
        if y < self.h {
            let mut buf = self.buf.borrow_mut();
            let mut x = x;
            for g in UnicodeSegmentation::graphemes(text, true) {
                let width = UnicodeWidthStr::width(g);
                // skip zero width characters
                if width > 0 {
                    if x < self.w {
                        buf[y * self.w + x] = Some((style, g.into(), None));
                    }
                    x += 1;
                    // skip the double wide characters that would be covered up, 
                    // putting None into the buffer
                    for _ in 0..(width - 1) {
                        if x < self.w {
                            buf[y * self.w + x] = None;
                        }
                        x += 1;
                    }
                }
            }
        }
    }

    // draw the main body text from a rope slice
    // buffer text remembers the corresponding char_idx where it came from
    pub(crate) fn draw_rope_slice(&self, x: usize, y: usize, text: &RopeSlice, style: Style, char_idx : usize) {
        if y < self.h {
            let mut buf = self.buf.borrow_mut();
            let mut x = x;
            for g in RopeGraphemes::new(&text) {
                let width = grapheme_width(&g);
                // skip zero width characters
                if width > 0 {
                    if x < self.w {
                        buf[y * self.w + x] = Some((style, SmallString::from_rope_slice(&g),Some(char_idx)));
                    }
                    x += 1;
                    // skip the double wide characters that would be covered up, 
                    // putting None into the buffer
                    for _ in 0..(width - 1) {
                        if x < self.w {
                            buf[y * self.w + x] = None;
                        }
                        x += 1;
                    }
                }
            }
        }
    }

    pub(crate) fn hide_cursor(&self) {
        write!(self.out.borrow_mut(), "{}", termion::cursor::Hide).unwrap();
    }

    pub(crate) fn show_cursor(&self) {
        write!(self.out.borrow_mut(), "{}", termion::cursor::Show).unwrap();
    }
}

impl Drop for Screen {
    fn drop(&mut self) {
        write!(
            self.out.borrow_mut(),
            "{}{}{}",
            color::Fg(color::Reset),
            color::Bg(color::Reset),
            termion::clear::All,
        ).unwrap();
        self.show_cursor();
    }
}

// Monokai colors from tmtheme-editor.herokuapp.com 
// theme.dark.monokai

// backgrounds
pub const BACKGROUND : Rgb            = Rgb(0x27, 0x28, 0x22); // "#272822"  // default background
pub const CARET_BG : Rgb              = Rgb(0xF8, 0xF8, 0xF0); // "#F8F8F0"
pub const INVISIBLES_BG : Rgb         = Rgb(0x3B, 0x3A, 0x32); // "#3B3A32"
pub const LINE_HIGHLIGHT_BG : Rgb     = Rgb(0x3E, 0x3D, 0x32); // "#3E3D32"
pub const SELECTION_BG : Rgb          = Rgb(0x49, 0x48, 0x3E); // "#49483E"
pub const INVALID_BG  : Rgb           = Rgb(0xF9, 0x26, 0x72); // "#F92672"
pub const INVALID_DEPRECATED_BG : Rgb = Rgb(0xAE, 0x81, 0xFF); // "#AE81FF"

// foreground
pub const FOREGROUND : Rgb            = Rgb(0xF8, 0xF8, 0xF2); // "#F8F8F2"  // default foreground
pub const COMMENT_FG : Rgb            = Rgb(0x75, 0x71, 0x5E); // "#75715E"
pub const STRING_FG : Rgb             = Rgb(0xE6, 0xDB, 0x74); // "#E6DB74"
pub const CONSTANT_FG : Rgb           = Rgb(0xE6, 0xDB, 0x74); // "#E6DB74"
pub const CONSTANT_NUMERIC_FG : Rgb   = Rgb(0xAE, 0x81, 0xFF); // "#AE81FF"
pub const VARIABLE_FG : Rgb           = Rgb(0xF8, 0xF8, 0xF2); // "#F8F8F2"
pub const KEYWORD_FG : Rgb            = Rgb(0xF9, 0x26, 0x72); // "#F92672"
pub const STORAGE_FG : Rgb            = Rgb(0x66, 0xD9, 0xEF); // "#66D9EF"
pub const CLASS_FUNCTION_FG : Rgb     = Rgb(0xA6, 0xE2, 0x2E); // "#A6E22E"
pub const VARIABLE_PARAMETER_FG : Rgb = Rgb(0xFD, 0x97, 0x1F); // "#FD971F"
pub const TAG_FG : Rgb                = Rgb(0xF9, 0x26, 0x72); // "#F92672"
pub const FUNCTION_CONSTANT_FG : Rgb  = Rgb(0x66, 0xD9, 0xEF); //"#66D9EF"
pub const INVALID_FG : Rgb            = Rgb(0xF8, 0xF8, 0xF0); //"#F8F8F0"
pub const INVALID_DEPRECATED_FG : Rgb = Rgb(0xF8, 0xF8, 0xF0); //"#F8F8F0"

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum Color {
    // Black,
    // Blue,
    // Cyan,
    // Green,
    // LightBlack,
    // LightBlue,
    // LightCyan,
    // LightGreen,
    // LightMagenta,
    // LightRed,
    // LightWhite,
    // LightYellow,
    // Magenta,
    // Red,
    Rgb(u8, u8, u8),
    // White,
    // Yellow,

    // Monokai colors
    // backgrounds 
    Background,   // default background
    CaretBg,
    InvisiblesBg, 
    LineHighlightBg,
    SelectionBg,
    InvalidBg,  
    InvalidDeprecatedBg, 

    // foregrounds
    Foreground, // default foreground
    CommentFg,
    StringFg,
    ConstantFg,
    ConstantNumericFg,
    VariableFg,
    KeywordFg,
    StorageFg,
    ClassFunctionFg,
    VariableParameterFg,
    TagFg,
    FunctionConstantFg,
    InvalidFg,
    InvalidDeprecatedFg,
}


#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct Style(pub Color, pub Color); // Fg, Bg

impl fmt::Display for Style {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            // Color::Black => write!(f, "{}", color::Fg(color::Black)),
            // Color::Blue => write!(f, "{}", color::Fg(color::Blue)),
            // Color::Cyan => write!(f, "{}", color::Fg(color::Cyan)),
            // Color::Green => write!(f, "{}", color::Fg(color::Green)),
            // Color::LightBlack => write!(f, "{}", color::Fg(color::LightBlack)),
            // Color::LightBlue => write!(f, "{}", color::Fg(color::LightBlue)),
            // Color::LightCyan => write!(f, "{}", color::Fg(color::LightCyan)),
            // Color::LightGreen => write!(f, "{}", color::Fg(color::LightGreen)),
            // Color::LightMagenta => write!(f, "{}", color::Fg(color::LightMagenta)),
            // Color::LightRed => write!(f, "{}", color::Fg(color::LightRed)),
            // Color::LightWhite => write!(f, "{}", color::Fg(color::LightWhite)),
            // Color::LightYellow => write!(f, "{}", color::Fg(color::LightYellow)),
            // Color::Magenta => write!(f, "{}", color::Fg(color::Magenta)),
            // Color::Red => write!(f, "{}", color::Fg(color::Red)),
            Color::Rgb(r, g, b) => write!(f, "{}", color::Fg(color::Rgb(r, g, b))),
            // Color::White => write!(f, "{}", color::Fg(color::White)),
            // Color::Yellow => write!(f, "{}", color::Fg(color::Yellow)),

            // foregrounds
            Color::Foreground => write!(f, "{}", color::Fg(FOREGROUND)),
            Color::CommentFg => write!(f, "{}", color::Fg(COMMENT_FG)),
            Color::StringFg => write!(f, "{}", color::Fg(STRING_FG)),
            Color::ConstantFg => write!(f, "{}", color::Fg(CONSTANT_FG)),
            Color::ConstantNumericFg => write!(f, "{}", color::Fg(CONSTANT_NUMERIC_FG)),
            Color::VariableFg => write!(f, "{}", color::Fg(VARIABLE_FG)),
            Color::KeywordFg => write!(f, "{}", color::Fg(KEYWORD_FG)),
            Color::StorageFg => write!(f, "{}", color::Fg(STORAGE_FG)),
            Color::ClassFunctionFg => write!(f, "{}", color::Fg(CLASS_FUNCTION_FG)),
            Color::VariableParameterFg => write!(f, "{}", color::Fg(VARIABLE_PARAMETER_FG)),
            Color::TagFg => write!(f, "{}", color::Fg(TAG_FG)),
            Color::FunctionConstantFg => write!(f, "{}", color::Fg(FUNCTION_CONSTANT_FG)),
            Color::InvalidFg => write!(f, "{}", color::Fg(INVALID_FG)),
            Color::InvalidDeprecatedFg => write!(f, "{}", color::Fg(INVALID_DEPRECATED_FG)),

            // backgrounds, allow as foregrounds too 
            Color::Background => write!(f, "{}", color::Fg(BACKGROUND)),
            Color::CaretBg => write!(f, "{}", color::Fg(CARET_BG)),
            Color::InvisiblesBg => write!(f, "{}", color::Fg(INVISIBLES_BG)),
            Color::LineHighlightBg => write!(f, "{}", color::Fg(LINE_HIGHLIGHT_BG)),
            Color::SelectionBg => write!(f, "{}", color::Fg(SELECTION_BG)),
            Color::InvalidBg => write!(f, "{}", color::Fg(INVALID_BG)),
            Color::InvalidDeprecatedBg => write!(f, "{}", color::Fg(INVALID_DEPRECATED_BG)),

        }?;

        match self.1 {
            // Color::Black => write!(f, "{}", color::Bg(color::Black)),
            // Color::Blue => write!(f, "{}", color::Bg(color::Blue)),
            // Color::Cyan => write!(f, "{}", color::Bg(color::Cyan)),
            // Color::Green => write!(f, "{}", color::Bg(color::Green)),
            // Color::LightBlack => write!(f, "{}", color::Bg(color::LightBlack)),
            // Color::LightBlue => write!(f, "{}", color::Bg(color::LightBlue)),
            // Color::LightCyan => write!(f, "{}", color::Bg(color::LightCyan)),
            // Color::LightGreen => write!(f, "{}", color::Bg(color::LightGreen)),
            // Color::LightMagenta => write!(f, "{}", color::Bg(color::LightMagenta)),
            // Color::LightRed => write!(f, "{}", color::Bg(color::LightRed)),
            // Color::LightWhite => write!(f, "{}", color::Bg(color::LightWhite)),
            // Color::LightYellow => write!(f, "{}", color::Bg(color::LightYellow)),
            // Color::Magenta => write!(f, "{}", color::Bg(color::Magenta)),
            // Color::Red => write!(f, "{}", color::Bg(color::Red)),
            Color::Rgb(r, g, b) => write!(f, "{}", color::Bg(color::Rgb(r, g, b))),
            // Color::White => write!(f, "{}", color::Bg(color::White)),
            // Color::Yellow => write!(f, "{}", color::Bg(color::Yellow)),

            // backgrounds 
            Color::Background => write!(f, "{}", color::Bg(BACKGROUND)),
            Color::CaretBg => write!(f, "{}", color::Bg(CARET_BG)),
            Color::InvisiblesBg => write!(f, "{}", color::Bg(INVISIBLES_BG)),
            Color::LineHighlightBg => write!(f, "{}", color::Bg(LINE_HIGHLIGHT_BG)),
            Color::SelectionBg => write!(f, "{}", color::Bg(SELECTION_BG)),
            Color::InvalidBg => write!(f, "{}", color::Bg(INVALID_BG)),
            Color::InvalidDeprecatedBg => write!(f, "{}", color::Bg(INVALID_DEPRECATED_BG)),
            
            // foregrounds, allow as backgrounds too
            Color::Foreground => write!(f, "{}", color::Bg(FOREGROUND)),
            Color::CommentFg => write!(f, "{}", color::Bg(COMMENT_FG)),
            Color::StringFg => write!(f, "{}", color::Bg(STRING_FG)),
            Color::ConstantFg => write!(f, "{}", color::Bg(CONSTANT_FG)),
            Color::ConstantNumericFg => write!(f, "{}", color::Bg(CONSTANT_NUMERIC_FG)),
            Color::VariableFg => write!(f, "{}", color::Bg(VARIABLE_FG)),
            Color::KeywordFg => write!(f, "{}", color::Bg(KEYWORD_FG)),
            Color::StorageFg => write!(f, "{}", color::Bg(STORAGE_FG)),
            Color::ClassFunctionFg => write!(f, "{}", color::Bg(CLASS_FUNCTION_FG)),
            Color::VariableParameterFg => write!(f, "{}", color::Bg(VARIABLE_PARAMETER_FG)),
            Color::TagFg => write!(f, "{}", color::Bg(TAG_FG)),
            Color::FunctionConstantFg => write!(f, "{}", color::Bg(FUNCTION_CONSTANT_FG)),
            Color::InvalidFg => write!(f, "{}", color::Bg(INVALID_FG)),
            Color::InvalidDeprecatedFg => write!(f, "{}", color::Bg(INVALID_DEPRECATED_FG)),
        }
    }
}
