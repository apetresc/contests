pub fn abbreviate(phrase: &str) -> String {
    phrase
        .replace("-", " ")
        .replace(|c: char| !c.is_alphabetic() && !c.is_whitespace(), "")
        .split_whitespace()
        .map(|word|
            if word.chars().all(|c| !c.is_alphabetic() || c.is_uppercase()) {
                &word[0..1]
            } else { word })
        .flat_map(|word| {
            word.chars().enumerate().filter_map(|(i, c)| {
                if i == 0 || c.is_ascii_uppercase() { Some(c) }
                else { None }
            })
        })
        .collect::<String>()
        .to_ascii_uppercase()
}
