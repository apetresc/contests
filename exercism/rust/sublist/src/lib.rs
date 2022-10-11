#[derive(Debug, PartialEq, Eq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

fn prefix<T: PartialEq>(first_list: &[T], second_list: &[T]) -> bool {
    for (i, e) in first_list.iter().enumerate() {
        if second_list[i] != *e {
            return false;
        }
    }
    true
}

pub fn sublist<T: PartialEq>(first_list: &[T], second_list: &[T]) -> Comparison {
    let swapped = first_list.len() > second_list.len();
    let (first_list, second_list) = if swapped {
        (second_list, first_list)
    } else {
        (first_list, second_list)
    };
    let offset = second_list.len() - first_list.len();

    for i in 0..=offset {
        if prefix(first_list, &second_list[i..i + first_list.len()]) {
            return if offset == 0 {
                Comparison::Equal
            } else if swapped {
                Comparison::Superlist
            } else {
                Comparison::Sublist
            };
        }
    }

    if first_list.len() == 0 && second_list.len() == 0 {
        Comparison::Equal
    } else {
        Comparison::Unequal
    }
}
