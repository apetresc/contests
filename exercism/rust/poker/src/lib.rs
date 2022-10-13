#[derive(Copy, Clone, Debug, PartialEq)]
struct Card {
    val: u8,
    suit: u8,
}

#[derive(Debug)]
pub struct PokerHand<'a> {
    rep: &'a str,
    cards: [Card; 5],
}

impl<'a> PokerHand<'a> {
    pub fn new(s: &'a str) -> Self {
        PokerHand {
            rep: s,
            cards: s
                .split(' ')
                .map(|card| Card {
                    val: match &card[0..&card.len() - 1] {
                        "A" => 14,
                        "J" => 11,
                        "Q" => 12,
                        "K" => 13,
                        v => v.parse::<u8>().unwrap(),
                    },
                    suit: card[card.len() - 1..].as_bytes()[0],
                })
                .collect::<Vec<Card>>()
                .try_into()
                .unwrap(),
        }
    }

    fn rank(&self) -> (u8, Vec<u8>) {
        let mut cards = self.cards;
        cards.sort_by_key(|c| u8::MAX - c.val);

        if cards.windows(2).all(|cs| {
            cs[0].suit == cs[1].suit
                && (cs[0].val - cs[1].val == 1 || (cs[0].val == 14 && cs[0].val - cs[1].val == 9))
        }) {
            // Straight flush
            (
                9,
                vec![match cards[0].val {
                    14 if cards[1].val == 5 => 1, // Aces are low when starting a straight
                    _ => cards[0].val,
                }],
            )
        } else if let Some(run) = cards.windows(4).find(|cs| cs[0].val == cs[3].val) {
            // 4 of a kind
            (
                8,
                vec![
                    run[0].val,
                    match run[0].val {
                        _low if _low == cards[4].val => cards[0].val,
                        _ => cards[4].val,
                    },
                ],
            )
        } else if (cards[0].val == cards[1].val)
            && (cards[3].val == cards[4].val)
            && (cards[2].val == cards[1].val || cards[2].val == cards[3].val)
        {
            // Full house
            (
                7,
                vec![
                    cards[2].val,
                    match cards[2].val {
                        _low if _low == cards[4].val => cards[0].val,
                        _ => cards[4].val,
                    },
                ],
            )
        } else if cards.windows(2).all(|cs| cs[0].suit == cs[1].suit) {
            // Flush
            (6, cards.map(|c| c.val).to_vec())
        } else if cards
            .windows(2)
            .all(|cs| cs[0].val - cs[1].val == 1 || (cs[0].val == 14 && cs[0].val - cs[1].val == 9))
        {
            // Straight
            (
                5,
                vec![match cards[0].val {
                    14 if cards[1].val == 5 => 1, // Aces are low when starting a straight
                    _ => cards[0].val,
                }],
            )
        } else if cards
            .windows(3)
            .any(|cs| cs[0].val == cs[1].val && cs[0].val == cs[2].val)
        {
            // Three of a kind
            (
                4,
                vec![
                    cards[2].val,
                    match cards[2].val {
                        _high if _high == cards[0].val => cards[3].val,
                        _ => cards[0].val,
                    },
                    match cards[2].val {
                        _low if _low == cards[4].val => cards[1].val,
                        _ => cards[4].val,
                    },
                ],
            )
        } else if cards
            .windows(2)
            .map(|cs| if cs[0].val == cs[1].val { 1 } else { 0 })
            .sum::<i8>()
            == 2
        {
            // Two pairs
            (
                3,
                vec![
                    cards[1].val,
                    cards[3].val,
                    match cards[2].val {
                        _right if _right == cards[2].val => cards[4].val,
                        _left if _left == cards[1].val => cards[0].val,
                        _ => cards[2].val,
                    },
                ],
            )
        } else if let Some(pair) = cards.windows(2).find(|cs| cs[0].val == cs[1].val) {
            // One pair
            (
                2,
                [
                    vec![pair[0].val],
                    cards
                        .iter()
                        .filter_map(|c| match c.val {
                            _p if _p == pair[0].val => None,
                            v => Some(v),
                        })
                        .collect(),
                ]
                .concat(),
            )
        } else {
            // High card
            (1, cards.map(|c| c.val).to_vec())
        }
    }
}

impl<'a> PartialEq for PokerHand<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.cards
            .iter()
            .zip(other.cards.iter())
            .all(|(c1, c2)| c1.val == c2.val && c1.suit == c2.suit)
    }
}

impl<'a> PartialOrd for PokerHand<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.rank().partial_cmp(&other.rank())
    }
}

/// Given a list of poker hands, return a list of those hands which win.
pub fn winning_hands<'a>(hands: &[&'a str]) -> Vec<&'a str> {
    let mut hands = hands
        .iter()
        .map(|hand| PokerHand::new(hand))
        .collect::<Vec<PokerHand>>();
    hands.sort_by(|a, b| b.partial_cmp(a).unwrap());
    hands
        .iter()
        .filter_map(|hand| match hand.rank() {
            _high if _high == hands[0].rank() => Some(hand.rep),
            _ => None,
        })
        .collect()
}
