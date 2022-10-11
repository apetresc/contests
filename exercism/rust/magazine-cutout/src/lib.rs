use std::collections::HashMap;

pub fn can_construct_note(magazine: &[&str], note: &[&str]) -> bool {
    let mut words = HashMap::<&str, u32>::new();
    for word in magazine {
        *words.entry(word).or_insert(0) += 1;
    }
    for word in note {
        match words.get(word) {
            Some(w) if *w > 0 => words.entry(word).and_modify(|e| {*e -= 1} ),
            Some(_) => return false,
            None => return false
        };
    }

    true
}

