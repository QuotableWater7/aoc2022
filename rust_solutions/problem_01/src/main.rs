use std::{fs};

fn get_input_filepath() -> String {
  let split_file = file!();
  let index_of_last_slash = split_file.rfind(|x| x == '/').unwrap();
  let (directory, _filename) = split_file.split_at(index_of_last_slash);

  format!("{directory}/input.txt")
}

fn part1() {
  let input_filepath = get_input_filepath();
  let contents = fs::read_to_string(input_filepath)
      .expect("Should have been able to read the file");
      
  let groups = contents.split("\n\n");

  let max: u32 = groups.map(|g| g.lines()
      .map( |line| line.parse::<u32>().unwrap() ).sum()
    ).max().unwrap();

  println!("Part 1: {max}");
}

fn part2() {
  let contents = fs::read_to_string(get_input_filepath())
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

  println!("Part 2: {sum}")
}

fn main() {
  part1();
  part2();
}