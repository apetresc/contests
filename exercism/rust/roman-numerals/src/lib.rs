pub struct Roman;

const NUMERALS: [(i32, &'static str); 13] = [
    (1000, "M"),
    (900, "CM"),
    (500, "D"),
    (400, "CD"),
    (100, "C"),
    (90, "XC"),
    (50, "L"),
    (40, "XL"),
    (10, "X"),
    (9, "IX"),
    (5, "V"),
    (4, "IV"),
    (1, "I"),
];


impl Roman {
    pub fn from(num: u32) -> String {
        let mut roman = String::new();
        let mut num = num as i32;
        for &(n, s) in NUMERALS.iter() {
            while num >= n {
                roman.push_str(s);
                num -= n;
            }
        }
        roman
    }
}
