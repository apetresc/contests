pub fn abbreviate(phrase: &str) -> String {
    phrase
        .replace("-", " ")
        .replace(|c: char| !c.is_alphabetic() && !c.is_whitespace(), "")
        .split_whitespace()
        .map(|word| word.chars().next().unwrap().to_ascii_uppercase())
        .collect()
}
