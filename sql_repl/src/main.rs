mod repl;

use std::io;

fn main() {
    repl::start(&io::stdin());
}
