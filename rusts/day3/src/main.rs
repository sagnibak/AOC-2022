#![feature(array_chunks)]

use std::collections::HashSet;
use std::fs;

struct Rucksack {
    comp_1: HashSet<u8>,
    comp_2: HashSet<u8>,
}

impl Rucksack {
    fn get_common_entry_in_both_comps(&self) -> u8 {
        *self
            .comp_1
            .intersection(&self.comp_2)
            .take(1)
            .next()
            .unwrap()
    }
}

impl From<&&str> for Rucksack {
    fn from(str_bag: &&str) -> Self {
        let mut comp_1 = HashSet::new();
        let mut comp_2 = HashSet::new();

        for (i, c) in str_bag.as_bytes().into_iter().enumerate() {
            if i < str_bag.len() / 2 {
                comp_1.insert(*c);
            } else {
                comp_2.insert(*c);
            }
        }
        Rucksack { comp_1, comp_2 }
    }
}

struct ThreeSacks {
    sack_1: HashSet<u8>,
    sack_2: HashSet<u8>,
    sack_3: HashSet<u8>,
}

impl ThreeSacks {
    fn get_badge(&self) -> u8 {
        *self
            .sack_1
            .intersection(&self.sack_2)
            .map(|x| *x)
            .collect::<HashSet<_>>()
            .intersection(&self.sack_3)
            .take(1)
            .next()
            .unwrap()
    }
}

impl From<&[&str; 3]> for ThreeSacks {
    fn from(bags: &[&str; 3]) -> Self {
        let sack_1 = bags[0].bytes().collect();
        let sack_2 = bags[1].bytes().collect();
        let sack_3 = bags[2].bytes().collect();
        ThreeSacks {
            sack_1,
            sack_2,
            sack_3,
        }
    }
}

fn get_priority(c: u8) -> u64 {
    if c >= b'a' && c <= b'z' {
        (c - b'a' + 1).into()
    } else if c >= b'A' && c <= b'Z' {
        (c - b'A' + 27).into()
    } else {
        unreachable!()
    }
}

fn main() -> Result<(), std::io::Error> {
    let raw_input = fs::read_to_string("../../inputs/day3.input")?;
    let lines = raw_input.split_whitespace().collect::<Vec<_>>();

    let part_1 = lines
        .iter()
        .map(Rucksack::from)
        .map(|rucksack| rucksack.get_common_entry_in_both_comps())
        .map(get_priority)
        .sum::<u64>();

    let part_2 = lines
        .array_chunks::<3>()
        .map(ThreeSacks::from)
        .map(|sacks| sacks.get_badge())
        .map(get_priority)
        .sum::<u64>();

    println!("Part 1: {}", part_1);
    println!("Part 2: {}", part_2);
    Ok(())
}
