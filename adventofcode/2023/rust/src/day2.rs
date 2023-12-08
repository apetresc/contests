use std::cmp::max;

fn parse_pull(pull: &str) -> (u32, u32, u32) {
    let (mut red, mut green, mut blue) = (0, 0, 0);
    for part in pull.split(",") {
        let mut part = part.split_whitespace();
        let (count, color) = (
            part.next().unwrap().parse::<u32>().unwrap(),
            part.next().unwrap(),
        );
        match color {
            "red" => red += count,
            "green" => green += count,
            "blue" => blue += count,
            _ => (),
        }
    }
    (red, green, blue)
}

fn parse_game(line: &str) -> (u32, Vec<(u32, u32, u32)>) {
    let game_id = line
        .split_whitespace()
        .nth(1)
        .unwrap()
        .split(":")
        .nth(0)
        .unwrap()
        .parse::<u32>()
        .unwrap();
    let line = line.split(": ").nth(1).unwrap();

    let pulls = line.split("; ");
    (game_id, pulls.map(parse_pull).collect())
}

fn valid_game(line: &str) -> (u32, bool) {
    let (game_id, pulls) = parse_game(line);

    (
        game_id,
        pulls
            .iter()
            .all(|(r, g, b)| *r <= 12 && *g <= 13 && *b <= 14),
    )
}

pub fn day2a(input: &[String]) -> u32 {
    input
        .iter()
        .filter_map(|line| match valid_game(line) {
            (game_id, true) => Some(game_id),
            _ => None,
        })
        .sum()
}

fn min_cubes(line: &str) -> (u32, u32, u32) {
    let (_, pulls) = parse_game(line);
    pulls.iter().fold((0, 0, 0), |(r, g, b), (r2, g2, b2)| {
        (max(r, *r2), max(g, *g2), max(b, *b2))
    })
}

pub fn day2b(input: &[String]) -> u32 {
    input
        .iter()
        .map(|line| min_cubes(line))
        .map(|(r, g, b)| r * g * b)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util;
    use std::fs::File;
    use std::io::{self, BufReader};

    #[test]
    fn test_parse_pull() {
        assert_eq!(parse_pull("1 red, 2 green, 3 blue"), (1, 2, 3));
        assert_eq!(parse_pull("1 red"), (1, 0, 0));
        assert_eq!(parse_pull("2 green, 3 blue"), (0, 2, 3));
    }

    #[test]
    fn test_parse_game() {
        assert_eq!(
            parse_game("Game 1: 1 red, 2 green, 3 blue; 2 red, 3 green, 4 blue"),
            (1, vec![(1, 2, 3), (2, 3, 4)])
        );
        assert_eq!(
            parse_game("Game 2: 1 red, 2 blue; 2 red"),
            (2, vec![(1, 0, 2), (2, 0, 0)])
        );
    }

    #[test]
    fn test_day2a() -> io::Result<()> {
        let day2_input = util::parse_input(BufReader::new(File::open("2.sample.in")?))?;
        assert_eq!(day2a(&day2_input), 8);

        Ok(())
    }

    #[test]
    fn test_min_cubes() {
        assert_eq!(
            min_cubes("Game 1: 1 red, 2 green, 3 blue; 2 red, 3 green, 4 blue"),
            (2, 3, 4)
        );
        assert_eq!(min_cubes("Game 2: 1 red, 2 blue; 2 red"), (2, 0, 2));
    }

    #[test]
    fn test_day2b() -> io::Result<()> {
        let day2_input = util::parse_input(BufReader::new(File::open("2.sample.in")?))?;
        assert_eq!(day2b(&day2_input), 2286);

        Ok(())
    }
}
