#[derive(Debug)]
struct Timespan {
  start: u64,
  end: u64,
}

fn is_fully_overlapping(timespan1: &Timespan, timespan2: &Timespan) -> bool {
  (timespan1.start <= timespan2.start && timespan1.end <= timespan2.end) || 
  timespan2.start >= timespan1.start && timespan2.end <= timespan1.end
}

fn is_partially_overlapping(timespan1: &Timespan, timespan2: &Timespan) -> bool {
  if timespan1.start <= timespan2.start {
    return timespan1.end >= timespan2.start
  }

  if timespan2.start <= timespan1.start {
    return timespan2.end >= timespan1.start
  }

  false
}

fn parse_line(string: &str) -> (Timespan, Timespan) {
  let (a, b) = string.split_once(',').unwrap();
  let (a_start, a_end) = a.split_once('-').map(|(a_start, a_end)| (a_start.parse::<u64>().unwrap(), a_end.parse::<u64>().unwrap())).unwrap();
  let (b_start, b_end) = b.split_once('-').map(|(b_start, b_end)| (b_start.parse::<u64>().unwrap(), b_end.parse::<u64>().unwrap())).unwrap();

  (Timespan { start: a_start, end: a_end }, Timespan { start: b_start, end: b_end })
}

fn main() {
  let timespans = include_str!("input.txt").lines().map(parse_line);

  // part 1
  let count = timespans
    .clone()
    .filter(|(span1, span2)| is_fully_overlapping(&span1, &span2))
    .count();

  println!("part 1: {count}");

  // part 2
  let count = timespans
    .clone()
    .filter(|(span1, span2)| is_partially_overlapping(&span1, &span2))
    .count();

  println!("part 2: {count}");
}
