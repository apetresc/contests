use core::fmt::Display;
use std::fmt::Result;

#[derive(Debug, Eq, PartialEq)]
pub struct Clock {
    pub minutes: i32,
}

impl Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        write!(
            f,
            "{:02}:{:02}",
            self.minutes.div_euclid(60),
            self.minutes.rem_euclid(60)
        )
    }
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        Self::resolve(hours * 60 + minutes)
    }

    fn resolve(minutes: i32) -> Self {
        Clock {
            minutes: minutes.rem_euclid(24 * 60),
        }
    }

    pub fn add_minutes(&mut self, minutes: i32) -> Self {
        Self::resolve(self.minutes + minutes)
    }
}
