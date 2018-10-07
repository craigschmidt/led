// command line argument parsing
extern crate docopt;
// the rope data structure used by a buffer
extern crate ropey;
// a generic serialization/deserialization framework
extern crate serde;
// macros 1.1 implementation of #[derive(Serialize, Deserialize)]
#[macro_use]
extern crate serde_derive;
// 'Small vector' optimization: store up to a small number of items on the stack
extern crate smallvec;
// terminal library
extern crate termion;
// This crate provides Grapheme Cluster and Word boundaries according to Unicode Standard Annex #29 rules. 
extern crate unicode_segmentation;
// Determine displayed width of `char` and `str` types according to Unicode Standard Annex #11 rules. 
extern crate unicode_width;
// logging code 
extern crate flexi_logger;
#[macro_use]
extern crate log;

use docopt::Docopt;
use editor::Editor;
use std::path::Path;
use term_ui::TermUI;
use flexi_logger::Logger;  // opt_format

mod editor;
mod string_utils;
mod term_ui;
mod utils;

// do docopt actuall turns the doc string into code 
// that parses thar argments, which is really weird and kind of nifty
// TODO: maybe use `clap` instead
// Usage documentation string
static USAGE: &'static str = "
Usage: led [options] [<file>]
       led --help

Options:
    -h, --help  Show this message
";

// Struct for storing command-line arguments
#[derive(Debug, Deserialize)]
struct Args {
    arg_file: Option<String>,
    flag_help: bool,
}

fn main() {
    // Get command-line arguments
    // explain: unwrap_or_else returns the value of Ok(x) from Result
    // or exit the program with an error message if it fails. 
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());

    // start logging:  one of error, warn, info, debug, trace
    Logger::with_env_or_str("debug")
            .log_to_file()
            .directory("log_files")
            // .format(opt_format)  try default now
            .start()
            .unwrap_or_else(|e| panic!("Logger initialization failed with {}", e));

    // Load file, if specified
    // TODO: stopped here ....
    let editor = if let Option::Some(s) = args.arg_file {
        Editor::new_from_file(&Path::new(&s[..]))
    } else {
        Editor::new()
    };

    // Initialize and start UI
    let mut ui = TermUI::new_from_editor(editor);
    ui.main_ui_loop();
}
