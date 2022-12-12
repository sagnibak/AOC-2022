use lazy_static::lazy_static;
use regex::Regex;
use std::fs;

lazy_static! {
    static ref RE: Regex = Regex::new("([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)").unwrap();
}

struct IntervalPair {
    first_interval: Interval,
    second_interval: Interval,
}

fn parse(raw_input: &str) -> Vec<IntervalPair> {
    todo!()
}

impl IntervalPair {
    fn from_line(line: &str) -> Self {
        let caps = RE.captures_iter(line);
        IntervalPair {
            first_interval: Interval(
                caps.next()
                    .unwrap()
                    .get(0)
                    .unwrap()
                    .as_str()
                    .parse()
                    .unwrap(),
                caps.next()
                    .unwrap()
                    .get(0)
                    .unwrap()
                    .as_str()
                    .parse()
                    .unwrap(),
            ),
            second_interval: Interval(
                caps.next()
                    .unwrap()
                    .get(0)
                    .unwrap()
                    .as_str()
                    .parse()
                    .unwrap(),
                caps.next()
                    .unwrap()
                    .get(0)
                    .unwrap()
                    .as_str()
                    .parse()
                    .unwrap(),
            ),
        }
    }
}

struct Interval(i32, i32);

fn main() -> Result<(), std::io::Error> {
    let raw_input = fs::read_to_string("../../inputs/day4.input")?;
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
