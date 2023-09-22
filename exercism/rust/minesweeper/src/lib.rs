/*
 * Minesweeper Library in Rust
 */

fn mine_at(minefield: &[&str], x: i64, y: i64) -> u8 {
    /* Return 1 if there is a mine at (x, y), 0 otherwise */
    if x < 0 || y < 0 {
        return 0;
    }
    if x >= minefield.len() as i64 || y >= minefield[0].len() as i64 {
        return 0;
    }
    if minefield[x as usize].chars().nth(y as usize).unwrap() == '*' {
        return 1;
    }
    0
}

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    if minefield.len() == 0 {
        return vec![];
    }
    let mut adj: Vec<Vec<u8>> = vec![vec![0; minefield[0].len()]; minefield.len()];

    for (i, row) in minefield.iter().enumerate() {
        for (j, c) in row.chars().enumerate() {
            adj[i][j] = match (i, j, c) {
                (_, _, '*') => 9,
                _ => {
                    mine_at(minefield, (i - 1) as i64, (j - 1) as i64)
                        + mine_at(minefield, (i - 1) as i64, j as i64)
                        + mine_at(minefield, (i - 1) as i64, (j + 1) as i64)
                        + mine_at(minefield, i as i64, (j - 1) as i64)
                        + mine_at(minefield, i as i64, j as i64)
                        + mine_at(minefield, i as i64, (j + 1) as i64)
                        + mine_at(minefield, (i + 1) as i64, (j - 1) as i64)
                        + mine_at(minefield, (i + 1) as i64, j as i64)
                        + mine_at(minefield, (i + 1) as i64, (j + 1) as i64)
                }
            }
            /*
            if *c == ('*' as u8) {
                adj[i][j] = 9;
            }
            */
        }
    }

    println!("{:?}", adj);

    //adj.iter()
    adj.iter()
        .map(|v| {
            v.iter()
                .map(|c| match c {
                    0 => " ".to_string(),
                    1..=8 => c.to_string(),
                    9 => "*".to_string(),
                    _ => " ".to_string(),
                })
                .collect()
        })
        .collect()
}
