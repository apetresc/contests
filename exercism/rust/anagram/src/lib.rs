use std::collections::HashSet;

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let mut anagrams = HashSet::<&str>::new();
    let lower_word = word.to_lowercase();
    let mut sorted_word: Vec<char> = lower_word.chars().collect();
    sorted_word.sort_unstable();

    for candidate in possible_anagrams {
        if candidate.len() != word.len() {
            continue;
        };
        let lower_candidate = candidate.to_lowercase();
        let mut chars: Vec<char> = lower_candidate.chars().collect();
        chars.sort_unstable();
        if chars == sorted_word && lower_word != lower_candidate {
            anagrams.insert(&candidate);
        }
    }

    anagrams
}
