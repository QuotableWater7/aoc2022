use std::str::FromStr;

#[derive(Debug)]
struct Timespan {
  start: u64,
  end: u64,
}

impl Timespan {
  pub fn is_fully_overlapping(&self, timespan: &Timespan) -> bool {
    (self.start <= timespan.start && self.end <= timespan.end) || 
    timespan.start >= self.start && timespan.end <= self.end
  }

  pub fn is_partially_overlapping(&self, timespan: &Timespan) -> bool {
    (self.start <= timespan.start && self.end >= timespan.start) ||
    timespan.start <= self.start && timespan.end >= self.start
  }
}

#[derive(Debug)]
struct ParseTimespanError;

impl FromStr for Timespan {
  type Err = ParseTimespanError;

  fn from_str(string: &str) -> Result<Self, Self::Err> {
    let (start, end) = string
      .split_once('-')
      .map(|(start, end)| {
        (start.parse::<u64>().unwrap(), end.parse::<u64>().unwrap())
      })
        .unwrap();
  
    Ok(Timespan { start, end })
  }
}

fn parse_timespan(string: &str) -> (Timespan, Timespan) {
  let (a, b) = string.split_once(',').unwrap();

  (a.parse::<Timespan>().unwrap(), b.parse::<Timespan>().unwrap())
}

fn main() {
  let timespans = include_str!("input.txt").lines().map(parse_timespan);

  // part 1
  let count = timespans
    .clone()
    .filter(|(span1, span2)| span1.is_fully_overlapping(&span2))
    .count();

  println!("part 1: {count}");

  // part 2
  let count = timespans
    .clone()
    .filter(|(span1, span2)| span1.is_partially_overlapping(&span2))
    .count();

  println!("part 2: {count}");
}
