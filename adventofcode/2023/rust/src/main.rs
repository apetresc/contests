mod day1;
mod util;

use std::fs::File;
use std::io::{self, BufReader};

fn main() -> io::Result<()> {
    let day1_input = util::parse_input(BufReader::new(File::open("1.in")?))?;
    println!("Day 1A: {}", day1::day1a(&day1_input));
    println!("Day 1B: {}", day1::day1b(&day1_input));

    Ok(())
}
