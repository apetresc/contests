pub fn check(candidate: &str) -> bool {
    let mut char_count = 0;
    for c in candidate.chars().map(|c| c.to_ascii_lowercase()).filter(|c| c.is_alphabetic()) {
        let c = c as u8 - 'a' as u8;
        if char_count & (1 << c) != 0 {
            return false;
        } else {
            char_count |= 1 << c;
        }
    }

    true
}
