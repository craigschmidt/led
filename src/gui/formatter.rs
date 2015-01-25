use string_utils::{is_line_ending};
use buffer::line::{Line, LineGraphemeIter};
use buffer::line_formatter::{LineFormatter, RoundingBehavior};

//===================================================================
// LineFormatter implementation for terminals/consoles.
//===================================================================

pub struct GUILineFormatter {
    pub tab_width: u8,
}


impl GUILineFormatter {
    pub fn new(tab_width: u8) -> GUILineFormatter {
        GUILineFormatter {
            tab_width: tab_width,
        }
    }


    /// Returns the visual cell width of a line
    pub fn vis_width(&self, line: &Line) -> usize {
        let mut width = 0;
        
        for g in line.grapheme_iter() {
            let w = grapheme_vis_width_at_vis_pos(g, width, self.tab_width as usize);
            width += w;
        }
        
        return width;
    }


    pub fn vis_grapheme_iter<'b>(&'b self, line: &'b Line) -> ConsoleLineFormatterVisIter<'b> {
        ConsoleLineFormatterVisIter {
            grapheme_iter: line.grapheme_iter(),
            f: self,
            pos: (0, 0),
        }
    }
}


impl<'a> LineFormatter for GUILineFormatter {
    fn single_line_height(&self) -> usize {
        return 1;
    }

    fn dimensions(&self, line: &Line) -> (usize, usize) {
        return (1, self.vis_width(line));
    }
    
    
    fn index_to_v2d(&self, line: &Line, index: usize) -> (usize, usize) {
        let mut pos = 0;
        let mut iter = line.grapheme_iter();
        
        for _ in range(0, index) {
            if let Some(g) = iter.next() {
                let w = grapheme_vis_width_at_vis_pos(g, pos, self.tab_width as usize);
                pos += w;
            }
            else {
                panic!("GUILineFormatter::index_to_v2d(): index past end of line.");
            }
        }
        
        return (0, pos);
    }
    
    
    fn v2d_to_index(&self, line: &Line, v2d: (usize, usize), rounding: (RoundingBehavior, RoundingBehavior)) -> usize {
        let mut pos = 0;
        let mut i = 0;
        let mut iter = line.grapheme_iter();
        
        while pos < v2d.1 {
            if let Some(g) = iter.next() {
                let w = grapheme_vis_width_at_vis_pos(g, pos, self.tab_width as usize);
                if (w + pos) > v2d.1 {
                    let d1 = v2d.1 - pos;
                    let d2 = (pos + w) - v2d.1;
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
}


//===================================================================
// An iterator that iterates over the graphemes in a line in a
// manner consistent with the ConsoleFormatter.
//===================================================================
pub struct ConsoleLineFormatterVisIter<'a> {
    grapheme_iter: LineGraphemeIter<'a>,
    f: &'a GUILineFormatter,
    pos: (usize, usize),
}



impl<'a> Iterator for ConsoleLineFormatterVisIter<'a> {
    type Item = (&'a str, (usize, usize), usize);

    fn next(&mut self) -> Option<(&'a str, (usize, usize), usize)> {
        if let Some(g) = self.grapheme_iter.next() {
            let pos = self.pos;
            
            let width = grapheme_vis_width_at_vis_pos(g, self.pos.1, self.f.tab_width as usize);
            self.pos = (self.pos.0, self.pos.1 + width);
            
            return Some((g, (pos.0, pos.1), width));
        }
        else {
            return None;
        }
    }
}



//===================================================================
// Helper functions
//===================================================================

/// Returns the visual width of a grapheme given a starting
/// position on a line.
fn grapheme_vis_width_at_vis_pos(g: &str, pos: usize, tab_width: usize) -> usize {
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
