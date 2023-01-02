use std::fs;

fn part1() {
    let contents = fs::read_to_string("/Users/josephbowler/agora/aoc2022/01/input.txt")
        .expect("Should have been able to read the file");
        
    let groups = contents.split("\n\n");
    let mut max: u32 = 0;

    for group in groups {
      let values = group.lines();

      let total: u32 = values.map(|x| x.parse::<u32>().expect("should be a number")).sum();

      if total.gt(&max) {
        max = total
      }
    }

    println!("Max:\t{max}");
}

fn part2() {
  let contents = fs::read_to_string("/Users/josephbowler/agora/aoc2022/01/input.txt")
    .expect("Should have been able to read the file");

  let groups = contents.split("\n\n").collect::<Vec<&str>>();

  let mut totals = groups
    .into_iter()
    .map(
      |group| group
        .lines()
        .map(|x| x.parse::<u32>().unwrap())
        .sum::<u32>()
    ).collect::<Vec<u32>>();

  totals.sort();

  let top_3: Vec<u32> = totals.into_iter().rev().take(3).collect();
  let sum: u32 = top_3.into_iter().sum();

  println!("{sum}")
}

fn main() {
  part1();

  println!("********************");

  part2();
}