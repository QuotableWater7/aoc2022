use std::collections::HashSet;

type Rucksack = str;

fn parse_rucksacks(input: &str) -> Vec<(&Rucksack, &Rucksack)> {
  input.split("\n").map(|r| r.split_at(r.len() / 2)).collect()
}

fn compute_first_match((r1, r2): &(&Rucksack, &Rucksack)) -> char {
  let mut r1Set: HashSet<char> = HashSet::new();

  for char in r1.chars() {
    r1Set.insert(char);
  }

  r2.chars().find(|char| r1Set.contains(&char)).unwrap()
}

fn get_priority(char: char) -> u32 {
  let ascii_value = char as u32;

  if ascii_value >= 97 {
    ascii_value - 97 + 1
  } else {
    ascii_value - 65 + 27
  }
}

fn main() {
    let file_contents = include_str!("input.txt");
    let rucksacks = parse_rucksacks(file_contents);
    let first_matches = rucksacks.iter().map(|pair| compute_first_match(pair));
    let priorities = first_matches.map(get_priority);
    let sum = priorities.sum::<u32>();

    println!("{:?}", sum)
}