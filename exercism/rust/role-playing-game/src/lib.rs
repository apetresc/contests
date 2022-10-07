use std::cmp::max;

pub struct Player {
    pub health: u32,
    pub mana: Option<u32>,
    pub level: u32,
}

impl Player {
    pub fn revive(&self) -> Option<Player> {
        match self.health {
            0 => Some(Player {
                    health: 100,
                    mana: match self.level { 0..=9 => None, 10.. => Some(100) },
                    level: self.level }),
            _ => None
        }
    }

    // TODO: These subtractions here don't do the best job at checking for
    // over/underflows. I need to grok the difference between `saturating_sub`,
    // `checked_sub`, et al.
    pub fn cast_spell(&mut self, mana_cost: u32) -> u32 {
        match self.mana {
            Some(v) if v < mana_cost => 0,
            Some(v) => {
                self.mana = Some(v - mana_cost);
                2 * mana_cost
            },
            None => {
                self.health = max(0, self.health as i32 - mana_cost as i32) as u32;
                0
            }
        }
    }
}
