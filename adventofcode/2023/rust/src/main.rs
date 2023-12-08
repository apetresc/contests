mod day1;
mod day2;
mod util;

use std::fs::File;
use std::io::{self, BufReader};

fn main() -> io::Result<()> {
    let day1_input = util::parse_input(BufReader::new(File::open("1.in")?))?;
    println!("Day 1A: {}", day1::day1a(&day1_input));
    println!("Day 1B: {}", day1::day1b(&day1_input));

    let day2_input = util::parse_input(BufReader::new(File::open("2.in")?))?;
    println!("Day 2A: {}", day2::day2a(&day2_input));
    println!("Day 2B: {}", day2::day2b(&day2_input));

    Ok(())
}
