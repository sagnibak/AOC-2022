use std::fs;
use std::collections::HashSet;

struct Rucksack {
    comp_1: HashSet<u8>,
    comp_2: HashSet<u8>,
}

impl Rucksack {
    fn get_common_entry_in_both_comps(&self) -> u8 {
        *self.comp_1.intersection(&self.comp_2).take(1).next().unwrap()
    }
}

impl From<&str> for Rucksack {
    fn from(str_bag: &str) -> Self {
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
    let result = fs::read_to_string("../../inputs/day3.input")?
        .split_whitespace()
        .map(Rucksack::from)
        .map(|rucksack| rucksack.get_common_entry_in_both_comps())
        .map(get_priority)
        .sum::<u64>();
    println!("Part 1: {}", result);
    Ok(())
}
