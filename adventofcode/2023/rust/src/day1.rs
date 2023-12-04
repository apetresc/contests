use regex::Regex;

static DIGIT_WORDS: [&str; 10] = [
    "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

pub fn day1a(input: &[String]) -> u32 {
    input
        .iter()
        .map(|line| {
            let digits: Vec<u32> = line.chars().filter_map(|c| c.to_digit(10)).collect();
            let (first, last) = (digits.first(), digits.last());
            match (first, last) {
                (Some(first), Some(last)) => 10 * first + last,
                _ => panic!("Invalid input: {}", line),
            }
        })
        .sum()
}

fn find_substring_occurrences(haystack: &str, needles: &[&str]) -> Vec<usize> {
    let pattern = needles.join("|");
    let re = Regex::new(&pattern).unwrap();

    let mut occurrences = vec![];
    let mut start = 0;

    while let Some(mat) = re.find_at(haystack, start) {
        occurrences.push(mat.start());
        start = mat.start() + 1;
    }

    occurrences
}

pub fn day1b(input: &[String]) -> u32 {
    input
        .iter()
        .map(|line| {
            let digit_indices: Vec<usize> = line
                .chars()
                .enumerate()
                .filter(|(_, c)| c.is_digit(10))
                .map(|(i, _)| i)
                .collect();
            let word_indices: Vec<usize> = find_substring_occurrences(line, &DIGIT_WORDS);
            let match_indices: Vec<usize> = digit_indices
                .into_iter()
                .chain(word_indices.into_iter())
                .collect();
            let (first_index, last_index) = (
                *match_indices.iter().min().unwrap(),
                *match_indices.iter().max().unwrap(),
            );
            let first = match line.chars().nth(first_index).unwrap().to_digit(10) {
                Some(digit) => digit,
                None => DIGIT_WORDS
                    .iter()
                    .enumerate()
                    .find(|&(_, word)| line[first_index..].starts_with(word))
                    .map(|(index, _)| index)
                    .unwrap() as u32,
            };
            let last = match line.chars().nth(last_index).unwrap().to_digit(10) {
                Some(digit) => digit,
                None => DIGIT_WORDS
                    .iter()
                    .enumerate()
                    .find(|&(_, word)| line[last_index..].starts_with(word))
                    .map(|(index, _)| index)
                    .unwrap() as u32,
            };
            10 * first + last
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util;
    use std::fs::File;
    use std::io::{self, BufReader};

    #[test]
    fn test_find_substring_occurrences() {
        assert_eq!(
            find_substring_occurrences("two1nine", &DIGIT_WORDS),
            vec![0, 4]
        );
        assert_eq!(
            find_substring_occurrences("12oneighthree33", &DIGIT_WORDS),
            vec![2, 4, 8]
        );
    }

    #[test]
    fn test_day1b_substring_occurrences() {
        assert_eq!(day1b(&vec!["two1nine".to_string()]), 29);
        assert_eq!(day1b(&vec!["eightwothree".to_string()]), 83);
        assert_eq!(day1b(&vec!["abcone2threexyz".to_string()]), 13);
        assert_eq!(day1b(&vec!["xtwone3four".to_string()]), 24);
        assert_eq!(day1b(&vec!["4nineeightseven2".to_string()]), 42);
        assert_eq!(day1b(&vec!["zoneight234".to_string()]), 14);
        assert_eq!(day1b(&vec!["7pqrstsixteen".to_string()]), 76);
    }

    #[test]
    fn test_day1a() -> io::Result<()> {
        assert_eq!(
            day1a(&util::parse_input(BufReader::new(File::open(
                "1a.sample.in"
            )?))?),
            142
        );
        Ok(())
    }

    #[test]
    fn test_day1b() -> io::Result<()> {
        assert_eq!(
            day1b(&util::parse_input(BufReader::new(File::open(
                "1b.sample.in"
            )?))?),
            281
        );
        Ok(())
    }
}
