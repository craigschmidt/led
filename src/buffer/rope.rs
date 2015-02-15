use std::cmp::{min, max};
use std::mem;
use std::str::Graphemes;
use std::ops::Index;
use string_utils::{grapheme_count, insert_text_at_grapheme_index, remove_text_between_grapheme_indices, split_string_at_grapheme_index, grapheme_pos_to_byte_pos};

const MIN_NODE_SIZE: usize = 2048;
const MAX_NODE_SIZE: usize = MIN_NODE_SIZE * 2;


/// A rope data structure for storing text in a format that is efficient
/// for insertion and removal even for extremely large strings.
pub struct Rope {
    data: RopeData,
    grapheme_count_: usize,
    tree_height: u32,
}


enum RopeData {
    Leaf(String),
    Branch(Box<Rope>, Box<Rope>),
}


impl Rope {
    /// Creates a new empty rope
    pub fn new() -> Rope {
        Rope {
            data: RopeData::Leaf(String::new()),
            grapheme_count_: 0,
            tree_height: 1,
        }
    }

    /// Creates a new rope from a string slice    
    pub fn new_from_str(s: &str) -> Rope {
        let mut rope = Rope {
            data: RopeData::Leaf(String::from_str(s)),
            grapheme_count_: grapheme_count(s),
            tree_height: 1,
        };
        
        rope.split_if_too_large();
        
        return rope;
    }
    
    /// Creates a new rope from a string, consuming the string
    pub fn new_from_string(s: String) -> Rope {
        let gc = grapheme_count(s.as_slice());
        
        let mut rope = Rope {
            data: RopeData::Leaf(s),
            grapheme_count_: gc,
            tree_height: 1,
        };
        
        rope.split_if_too_large();
        
        return rope;
    }
    
    pub fn grapheme_count(&self) -> usize {
        return self.grapheme_count_;
    }
    
    pub fn insert_text_at_grapheme_index(&mut self, text: &str, pos: usize) {
        match self.data {
            // Find node for text to be inserted into
            RopeData::Branch(ref mut left, ref mut right) => {
                if pos < left.grapheme_count_ {
                    left.insert_text_at_grapheme_index(text, pos);
                }
                else {
                    right.insert_text_at_grapheme_index(text, pos - left.grapheme_count_);
                }
            },
            
            // Insert the text
            RopeData::Leaf(ref mut s_text) => {
                insert_text_at_grapheme_index(s_text, text, pos);
            },
        }
        
        self.update_stats();
        self.split_if_too_large();
        self.rebalance();
    }
    
    pub fn remove_text_between_grapheme_indices(&mut self, pos_a: usize, pos_b: usize) {
        // Bounds checks
        if pos_a > pos_b {
            panic!("Rope::remove_text_between_grapheme_indices(): pos_a must be less than or equal to pos_b.");
        }
        if pos_b > self.grapheme_count_ {
            panic!("Rope::remove_text_between_grapheme_indices(): attempt to remove text after end of node text.");
        }
        
        match self.data {
            RopeData::Leaf(ref mut text) => {
                remove_text_between_grapheme_indices(text, pos_a, pos_b);
            },
            
            RopeData::Branch(ref mut left, ref mut right) => {
                let lgc = left.grapheme_count_;
                
                if pos_a < lgc {
                    left.remove_text_between_grapheme_indices(pos_a, min(pos_b, lgc));
                }
                
                if pos_b > lgc {
                    right.remove_text_between_grapheme_indices(pos_a - min(pos_a, lgc), pos_b - lgc);
                }
            }
        }
        
        self.update_stats();
        self.merge_if_too_small();
        self.rebalance();
    }
    
    /// Splits a rope into two pieces from the given grapheme index.
    /// The first piece remains in this rope, the second piece is returned
    /// as a new rope.
    pub fn split(&mut self, pos: usize) -> Rope {
        // TODO: make more efficient.
        let s = self.to_string();
        let gc = self.grapheme_count();
        let bp = grapheme_pos_to_byte_pos(s.as_slice(), pos);
        self.remove_text_between_grapheme_indices(pos, gc);
        
        Rope::new_from_str(&s.as_slice()[bp..])
    }

    /// Appends another rope to the end of this one, consuming the other rope.    
    pub fn append(&mut self, rope: Rope) {
        // TODO: make more efficient.  Converting to a string and then
        // inserting is pretty slow...
        let s = rope.to_string();
        let gc = self.grapheme_count();
        self.insert_text_at_grapheme_index(s.as_slice(), gc);
    }
    
    /// Makes a copy of the rope as a string
    pub fn to_string(&self) -> String {
        let mut s = String::new();

        for chunk in self.chunk_iter() {
            s.push_str(chunk);
        }
        
        return s;
    }
    
    
    /// Creates a chunk iterator for the rope
    pub fn chunk_iter<'a>(&'a self) -> RopeChunkIter<'a> {
        self.chunk_iter_at_index(0).1
    }
    
    
    /// Creates a chunk iter starting at the chunk containing the given
    /// grapheme index.  Returns the chunk and its starting grapheme index.
    pub fn chunk_iter_at_index<'a>(&'a self, index: usize) -> (usize, RopeChunkIter<'a>) {
        let mut node_stack: Vec<&'a Rope> = Vec::new();
        let mut cur_node = self;
        let mut grapheme_i = index;
        
        // Find the right rope node, and populate the stack at the same time
        loop {
            match cur_node.data {
                RopeData::Leaf(_) => {
                    node_stack.push(cur_node);
                    break;
                },
                
                RopeData::Branch(ref left, ref right) => {
                    if grapheme_i < left.grapheme_count_ {
                        node_stack.push(&(**right));
                        cur_node = &(**left);
                    }
                    else {
                        cur_node = &(**right);
                        grapheme_i -= left.grapheme_count_;
                    }
                }
            }
        }
        
        (index - grapheme_i, RopeChunkIter {node_stack: node_stack})
    }
    
    
    /// Creates an iterator at the first grapheme of the rope
    pub fn grapheme_iter<'a>(&'a self) -> RopeGraphemeIter<'a> {
        self.grapheme_iter_at_index(0)
    }
    
    
    /// Creates an iterator at the given grapheme index
    pub fn grapheme_iter_at_index<'a>(&'a self, index: usize) -> RopeGraphemeIter<'a> {
        let (grapheme_i, mut chunk_iter) = self.chunk_iter_at_index(index);
        
        // Create the grapheme iter for the current node
        let mut giter = if let Some(text) = chunk_iter.next() {
            text.as_slice().graphemes(true)
        }
        else {
            unreachable!()
        };
        
        // Get to the right spot in the iter
        for _ in grapheme_i..index {
            giter.next();
        }
        
        // Create the rope grapheme iter
        return RopeGraphemeIter {
            chunk_iter: chunk_iter,
            cur_chunk: giter,
        };
    }
    
    
    //================================================================
    // Private utility functions
    //================================================================
    
    fn is_leaf(&self) -> bool {
        if let RopeData::Leaf(_) = self.data {
            true
        }
        else {
            false
        }
    }
    

    /// Non-recursively updates the stats of a node    
    fn update_stats(&mut self) {
        match self.data {
            RopeData::Leaf(ref text) => {
                self.grapheme_count_ = grapheme_count(text);
                self.tree_height = 1;
            },
            
            RopeData::Branch(ref left, ref right) => {
                self.grapheme_count_ = left.grapheme_count_ + right.grapheme_count_;
                self.tree_height = max(left.tree_height, right.tree_height) + 1;
            }
        }
    }


    /// Splits a leaf node into pieces if it's too large
    // TODO: find a way to do this that's more algorithmically efficient
    // if lots of splits need to happen.  This version ends up re-scanning
    // the text quite a lot, as well as doing quite a few unnecessary
    // allocations.
    fn split_if_too_large(&mut self) {
        if self.grapheme_count_ > MAX_NODE_SIZE && self.is_leaf() {
            
            // Calculate split position and how large the left and right
            // sides are going to be
            let split_pos = self.grapheme_count_ / 2;
            let new_gc_l = split_pos;
            let new_gc_r = self.grapheme_count_ - split_pos;

            // Do the split
            let mut nl = Box::new(Rope::new());
            let mut nr = Box::new(Rope::new());
            mem::swap(self, &mut (*nl));
            if let RopeData::Leaf(ref mut text) = nl.data {
                nr.data = RopeData::Leaf(split_string_at_grapheme_index(text, split_pos));
                text.shrink_to_fit();
            }
            
            // Recursively split
            nl.grapheme_count_ = new_gc_l;
            nr.grapheme_count_ = new_gc_r;
            nl.split_if_too_large();
            nr.split_if_too_large();
            
            // Update the new left and right node's stats
            nl.update_stats();
            nr.update_stats();
            
            // Create the new branch node with the new left and right nodes
            self.data = RopeData::Branch(nl, nr);
            self.update_stats();
        }
    }
    
    
    /// Merges a non-leaf node into a leaf node if it's too small
    fn merge_if_too_small(&mut self) {
        if self.grapheme_count_ < MIN_NODE_SIZE && !self.is_leaf() {
            let mut merged_text = String::new();
            
            if let RopeData::Branch(ref mut left, ref mut right) = self.data {
                // First, recursively merge the children
                left.merge_if_too_small();
                right.merge_if_too_small();
                
                // Then put their text into merged_text
                if let RopeData::Leaf(ref mut text) = left.data {
                    mem::swap(&mut merged_text, text);
                }        
                if let RopeData::Leaf(ref mut text) = right.data {
                    merged_text.push_str(text.as_slice());
                }
            }
            
            // Make this a leaf node with merged_text as its data
            self.data = RopeData::Leaf(merged_text);
            self.tree_height = 1;
            // Don't need to update grapheme count, because it should be the
            // same as before.
        }
    }
    
    
    /// Rotates the tree under the node left
    fn rotate_left(&mut self) {
        let mut temp = Rope::new();
        
        if let RopeData::Branch(_, ref mut right) = self.data {
            mem::swap(&mut temp, &mut (**right));
            
            if let RopeData::Branch(ref mut left, _) = temp.data {   
                mem::swap(&mut (**left), &mut (**right));
            }
            else {
                panic!("Rope::rotate_left(): attempting to rotate node without branching right child.");
            }
        }
        else {
            panic!("Rope::rotate_left(): attempting to rotate leaf node.");
        }
        
        if let RopeData::Branch(ref mut left, _) = temp.data {
            mem::swap(&mut (**left), self);
            left.update_stats();
        }
        
        mem::swap(&mut temp, self);
        self.update_stats();
    }
    
    
    /// Rotates the tree under the node right
    fn rotate_right(&mut self) {
        let mut temp = Rope::new();
        
        if let RopeData::Branch(ref mut left, _) = self.data {
            mem::swap(&mut temp, &mut (**left));
            
            if let RopeData::Branch(_, ref mut right) = temp.data {   
                mem::swap(&mut (**right), &mut (**left));
            }
            else {
                panic!("Rope::rotate_right(): attempting to rotate node without branching left child.");
            }
        }
        else {
            panic!("Rope::rotate_right(): attempting to rotate leaf node.");
        }
        
        if let RopeData::Branch(_, ref mut right) = temp.data {
            mem::swap(&mut (**right), self);
            right.update_stats();
        }
        
        mem::swap(&mut temp, self);
        self.update_stats();
    }
    
    
    /// Balances the tree under this node
    fn rebalance(&mut self) {
        loop {
            let mut rot: isize;
            
            if let RopeData::Branch(ref mut left, ref mut right) = self.data {
                let height_diff = (left.tree_height as isize) - (right.tree_height as isize);

                // Left side higher than right side
                if height_diff > 1 {
                    let mut child_rot = false;
                    if let RopeData::Branch(ref lc, ref rc) = left.data {
                        if lc.tree_height < rc.tree_height {
                            child_rot = true;
                        }
                    }
                    
                    if child_rot {
                        left.rotate_left();
                    }
                    
                    rot = 1;
                }
                // Right side higher then left side
                else if height_diff < -1 {
                    let mut child_rot = false;
                    if let RopeData::Branch(ref lc, ref rc) = right.data {
                        if lc.tree_height > rc.tree_height {
                            child_rot = true;
                        }
                    }
                    
                    if child_rot {
                        right.rotate_right();
                    }
                    
                    rot = -1;
                }
                // Balanced, stop
                else {
                    break;
                }
            }
            else {
                // Leaf node, stop
                break;
            }
            
            if rot == 1 {
                self.rotate_right();
            }
            else if rot == -1 {
                self.rotate_left();
            }
        }
    }
}


// Direct indexing to graphemes in the rope
impl Index<usize> for Rope {
    type Output = str;
    
    fn index<'a>(&'a self, index: &usize) -> &'a str {
        if *index >= self.grapheme_count() {
            panic!("Rope::Index: attempting to fetch grapheme that outside the bounds of the text.");
        }
        
        match self.data {
            RopeData::Leaf(ref text) => {
                let mut i: usize = 0;
                for g in text.graphemes(true) {
                    if i == *index {
                        return &g;
                    }
                    i += 1;
                }
                unreachable!();
            },
            
            RopeData::Branch(ref left, ref right) => {
                if *index < left.grapheme_count() {
                    return &left[*index];
                }
                else {
                    return &right[*index - left.grapheme_count()];
                }
            },
        }
    }
}




//=============================================================
// Rope iterators
//=============================================================

/// An iterator over a rope's string chunks
pub struct RopeChunkIter<'a> {
    node_stack: Vec<&'a Rope>,
}

impl<'a> Iterator for RopeChunkIter<'a> {
    type Item = &'a str;
    
    fn next(&mut self) -> Option<&'a str> {
        if let Some(next_chunk) = self.node_stack.pop() {
            loop {
                if let Option::Some(node) = self.node_stack.pop() {
                    match node.data {
                        RopeData::Leaf(_) => {
                            self.node_stack.push(node);
                            break;
                        },
                      
                        RopeData::Branch(ref left, ref right) => {
                            self.node_stack.push(&(**right));
                            self.node_stack.push(&(**left));
                            continue;
                        }
                    }
                }
                else {
                    break;
                }
            }
            
            if let RopeData::Leaf(ref text) = next_chunk.data {
                return Some(text.as_slice());
            }
            else {
                unreachable!();
            }
        }
        else {
            return None;
        }
    }
}



/// An iterator over a rope's graphemes
pub struct RopeGraphemeIter<'a> {
    chunk_iter: RopeChunkIter<'a>,
    cur_chunk: Graphemes<'a>,
}


impl<'a> Iterator for RopeGraphemeIter<'a> {
    type Item = &'a str;
    
    fn next(&mut self) -> Option<&'a str> {
        loop {
            if let Some(g) = self.cur_chunk.next() {
                return Some(g);
            }
            else {   
                if let Some(s) = self.chunk_iter.next() {
                    self.cur_chunk = s.graphemes(true);
                    continue;
                }
                else {
                    return None;
                }
            }
        }
    }
}




//===================================================================
// Unit test
//===================================================================

#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn new_1() {
        let rope = Rope::new();
        let mut iter = rope.grapheme_iter();
        
        assert_eq!(None, iter.next());
    }
    
    
    #[test]
    fn new_2() {
        let rope = Rope::new_from_str("Hello world!");
        let mut iter = rope.grapheme_iter();
        
        assert_eq!(Some("H"), iter.next());
        assert_eq!(Some("e"), iter.next());
        assert_eq!(Some("l"), iter.next());
        assert_eq!(Some("l"), iter.next());
        assert_eq!(Some("o"), iter.next());
        assert_eq!(Some(" "), iter.next());
        assert_eq!(Some("w"), iter.next());
        assert_eq!(Some("o"), iter.next());
        assert_eq!(Some("r"), iter.next());
        assert_eq!(Some("l"), iter.next());
        assert_eq!(Some("d"), iter.next());
        assert_eq!(Some("!"), iter.next());
        assert_eq!(None, iter.next());
    }
    
    
    #[test]
    fn new_3() {
        let s = String::from_str("Hello world!");
        let rope = Rope::new_from_string(s);
        let mut iter = rope.grapheme_iter();
        
        assert_eq!(Some("H"), iter.next());
        assert_eq!(Some("e"), iter.next());
        assert_eq!(Some("l"), iter.next());
        assert_eq!(Some("l"), iter.next());
        assert_eq!(Some("o"), iter.next());
        assert_eq!(Some(" "), iter.next());
        assert_eq!(Some("w"), iter.next());
        assert_eq!(Some("o"), iter.next());
        assert_eq!(Some("r"), iter.next());
        assert_eq!(Some("l"), iter.next());
        assert_eq!(Some("d"), iter.next());
        assert_eq!(Some("!"), iter.next());
        assert_eq!(None, iter.next());
    }
    
    
    #[test]
    fn index() {
        let rope = Rope::new_from_str("Hel世界lo world!");
        
        assert_eq!("H", &rope[0]);
        assert_eq!("界", &rope[4]);
    }
    
    
    #[test]
    fn to_string() {
        let rope = Rope::new_from_str("Hello there good people of the world!");
        let s = rope.to_string();
        
        assert_eq!("Hello there good people of the world!", s.as_slice());
    }
    
    
    #[test]
    fn split_1() {
        let mut rope1 = Rope::new_from_str("Hello there good people of the world!");
        let rope2 = rope1.split(18);
        
        assert_eq!("Hello there good p", rope1.to_string().as_slice());
        assert_eq!("eople of the world!", rope2.to_string().as_slice());
    }
    
    
    #[test]
    fn split_2() {
        let mut rope1 = Rope::new_from_str("Hello there good people of the world!");
        let rope2 = rope1.split(37);
        
        assert_eq!("Hello there good people of the world!", rope1.to_string().as_slice());
        assert_eq!("", rope2.to_string().as_slice());
    }
    
    
    #[test]
    fn split_3() {
        let mut rope1 = Rope::new_from_str("Hello there good people of the world!");
        let rope2 = rope1.split(0);
        
        assert_eq!("", rope1.to_string().as_slice());
        assert_eq!("Hello there good people of the world!", rope2.to_string().as_slice());
    }
    
    
    #[test]
    fn append_1() {
        let mut rope1 = Rope::new_from_str("Hello there good p");
        let rope2 = Rope::new_from_str("eople of the world!");
        
        rope1.append(rope2);
        
        assert_eq!("Hello there good people of the world!", rope1.to_string().as_slice());
    }
    
    
    #[test]
    fn append_2() {
        let mut rope1 = Rope::new_from_str("Hello there good people of the world!");
        let rope2 = Rope::new_from_str("");
        
        rope1.append(rope2);
        
        assert_eq!("Hello there good people of the world!", rope1.to_string().as_slice());
    }
    
    
    #[test]
    fn append_3() {
        let mut rope1 = Rope::new_from_str("");
        let rope2 = Rope::new_from_str("Hello there good people of the world!");
        
        rope1.append(rope2);
        
        assert_eq!("Hello there good people of the world!", rope1.to_string().as_slice());
    }
    
    
    #[test]
    fn insert_text() {
        let mut rope = Rope::new();
        
        rope.insert_text_at_grapheme_index("Hello 世界!", 0);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 9);
        assert!(Some("H") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some(" ") == iter.next());
        assert!(Some("世") == iter.next());
        assert!(Some("界") == iter.next());
        assert!(Some("!") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn insert_text_in_non_empty_buffer_1() {
        let mut rope = Rope::new_from_str("Hello\n 世界\r\n!");
        
        rope.insert_text_at_grapheme_index("Again ", 0);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 17);
        assert!(Some("A") == iter.next());
        assert!(Some("g") == iter.next());
        assert!(Some("a") == iter.next());
        assert!(Some("i") == iter.next());
        assert!(Some("n") == iter.next());
        assert!(Some(" ") == iter.next());
        assert!(Some("H") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some(" ") == iter.next());
        assert!(Some("世") == iter.next());
        assert!(Some("界") == iter.next());
        assert!(Some("\r\n") == iter.next());
        assert!(Some("!") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn insert_text_in_non_empty_buffer_2() {
        let mut rope = Rope::new_from_str("Hello\n 世界\r\n!");
        
        rope.insert_text_at_grapheme_index(" again", 5);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 17);
        assert!(Some("H") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some(" ") == iter.next());
        assert!(Some("a") == iter.next());
        assert!(Some("g") == iter.next());
        assert!(Some("a") == iter.next());
        assert!(Some("i") == iter.next());
        assert!(Some("n") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some(" ") == iter.next());
        assert!(Some("世") == iter.next());
        assert!(Some("界") == iter.next());
        assert!(Some("\r\n") == iter.next());
        assert!(Some("!") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn insert_text_in_non_empty_buffer_3() {
        let mut rope = Rope::new_from_str("Hello\n 世界\r\n!");
        
        rope.insert_text_at_grapheme_index("again", 6);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 16);
        assert!(Some("H") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("a") == iter.next());
        assert!(Some("g") == iter.next());
        assert!(Some("a") == iter.next());
        assert!(Some("i") == iter.next());
        assert!(Some("n") == iter.next());
        assert!(Some(" ") == iter.next());
        assert!(Some("世") == iter.next());
        assert!(Some("界") == iter.next());
        assert!(Some("\r\n") == iter.next());
        assert!(Some("!") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn insert_text_in_non_empty_buffer_4() {
        let mut rope = Rope::new_from_str("Hello\n 世界\r\n!");        

        rope.insert_text_at_grapheme_index("again", 11);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 16);
        assert!(Some("H") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some(" ") == iter.next());
        assert!(Some("世") == iter.next());
        assert!(Some("界") == iter.next());
        assert!(Some("\r\n") == iter.next());
        assert!(Some("!") == iter.next());
        assert!(Some("a") == iter.next());
        assert!(Some("g") == iter.next());
        assert!(Some("a") == iter.next());
        assert!(Some("i") == iter.next());
        assert!(Some("n") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn insert_text_in_non_empty_buffer_5() {
        let mut rope = Rope::new_from_str("Hello\n 世界\r\n!");
        
        rope.insert_text_at_grapheme_index("again", 2);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 16);
        assert!(Some("H") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("a") == iter.next());
        assert!(Some("g") == iter.next());
        assert!(Some("a") == iter.next());
        assert!(Some("i") == iter.next());
        assert!(Some("n") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some(" ") == iter.next());
        assert!(Some("世") == iter.next());
        assert!(Some("界") == iter.next());
        assert!(Some("\r\n") == iter.next());
        assert!(Some("!") == iter.next());
        
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn insert_text_in_non_empty_buffer_6() {
        let mut rope = Rope::new_from_str("Hello\n 世界\r\n!");
        
        rope.insert_text_at_grapheme_index("again", 8);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 16);
        assert!(Some("H") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some(" ") == iter.next());
        assert!(Some("世") == iter.next());
        assert!(Some("a") == iter.next());
        assert!(Some("g") == iter.next());
        assert!(Some("a") == iter.next());
        assert!(Some("i") == iter.next());
        assert!(Some("n") == iter.next());
        assert!(Some("界") == iter.next());
        assert!(Some("\r\n") == iter.next());
        assert!(Some("!") == iter.next());
        
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn insert_text_in_non_empty_buffer_7() {
        let mut rope = Rope::new_from_str("Hello\n 世界\r\n!");
        
        rope.insert_text_at_grapheme_index("\nag\n\nain\n", 2);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 20);
        assert!(Some("H") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("a") == iter.next());
        assert!(Some("g") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("a") == iter.next());
        assert!(Some("i") == iter.next());
        assert!(Some("n") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some(" ") == iter.next());
        assert!(Some("世") == iter.next());
        assert!(Some("界") == iter.next());
        assert!(Some("\r\n") == iter.next());
        assert!(Some("!") == iter.next());
        
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn remove_text_1() {
        let mut rope = Rope::new_from_str("Hi\nthere\npeople\nof\nthe\nworld!");
        
        rope.remove_text_between_grapheme_indices(0, 3);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 26);
        assert!(Some("t") == iter.next());
        assert!(Some("h") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("r") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("p") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("p") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("f") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("t") == iter.next());
        assert!(Some("h") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("w") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("r") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("d") == iter.next());
        assert!(Some("!") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn remove_text_2() {
        let mut rope = Rope::new_from_str("Hi\nthere\npeople\nof\nthe\nworld!");
        
        rope.remove_text_between_grapheme_indices(0, 12);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 17);
        assert!(Some("p") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("f") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("t") == iter.next());
        assert!(Some("h") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("w") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("r") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("d") == iter.next());
        assert!(Some("!") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn remove_text_3() {
        let mut rope = Rope::new_from_str("Hi\nthere\npeople\nof\nthe\nworld!");
        
        rope.remove_text_between_grapheme_indices(5, 17);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 17);
        assert!(Some("H") == iter.next());
        assert!(Some("i") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("t") == iter.next());
        assert!(Some("h") == iter.next());
        assert!(Some("f") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("t") == iter.next());
        assert!(Some("h") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("w") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("r") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("d") == iter.next());
        assert!(Some("!") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn remove_text_4() {
        let mut rope = Rope::new_from_str("Hi\nthere\npeople\nof\nthe\nworld!");
        
        rope.remove_text_between_grapheme_indices(23, 29);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 23);
        assert!(Some("H") == iter.next());
        assert!(Some("i") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("t") == iter.next());
        assert!(Some("h") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("r") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("p") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("p") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("f") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("t") == iter.next());
        assert!(Some("h") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn remove_text_5() {
        let mut rope = Rope::new_from_str("Hi\nthere\npeople\nof\nthe\nworld!");
        
        rope.remove_text_between_grapheme_indices(17, 29);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 17);
        assert!(Some("H") == iter.next());
        assert!(Some("i") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("t") == iter.next());
        assert!(Some("h") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("r") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("p") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("p") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn remove_text_6() {
        let mut rope = Rope::new_from_str("Hello\nworld!");
        
        rope.remove_text_between_grapheme_indices(3, 12);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 3);
        assert!(Some("H") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn remove_text_7() {
        let mut rope = Rope::new_from_str("Hi\nthere\nworld!");
        
        rope.remove_text_between_grapheme_indices(5, 15);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 5);
        assert!(Some("H") == iter.next());
        assert!(Some("i") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("t") == iter.next());
        assert!(Some("h") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn remove_text_8() {
        let mut rope = Rope::new_from_str("Hello\nworld!");
        
        rope.remove_text_between_grapheme_indices(3, 11);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 4);
        assert!(Some("H") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("!") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn remove_text_9() {
        let mut rope = Rope::new_from_str("Hello\nworld!");
        
        rope.remove_text_between_grapheme_indices(8, 12);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 8);
        assert!(Some("H") == iter.next());
        assert!(Some("e") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("l") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("w") == iter.next());
        assert!(Some("o") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn remove_text_10() {
        let mut rope = Rope::new_from_str("12\n34\n56\n78");
        
        rope.remove_text_between_grapheme_indices(4, 11);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 4);
        assert!(Some("1") == iter.next());
        assert!(Some("2") == iter.next());
        assert!(Some("\n") == iter.next());
        assert!(Some("3") == iter.next());
        assert!(None == iter.next());
    }
    
    
    #[test]
    fn remove_text_11() {
        let mut rope = Rope::new_from_str("1234567890");
        
        rope.remove_text_between_grapheme_indices(9, 10);
        
        let mut iter = rope.grapheme_iter();
        
        assert!(rope.grapheme_count() == 9);
        assert!(Some("1") == iter.next());
        assert!(Some("2") == iter.next());
        assert!(Some("3") == iter.next());
        assert!(Some("4") == iter.next());
        assert!(Some("5") == iter.next());
        assert!(Some("6") == iter.next());
        assert!(Some("7") == iter.next());
        assert!(Some("8") == iter.next());
        assert!(Some("9") == iter.next());
        assert!(None == iter.next());
    }
    
}