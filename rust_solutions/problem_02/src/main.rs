struct Turn {
  first_move: TurnAction,
  second_move: TurnAction,
}

#[derive(Debug)]
enum TurnAction {
  ROCK,
  PAPER,
  SCISSORS,
}

fn compute_score_for_turn(game_turn: &Turn) -> u32 {
  match game_turn.first_move {
    TurnAction::ROCK => {
      match game_turn.second_move {
        TurnAction::ROCK => 1 + 3,
        TurnAction::PAPER => 2 + 6,
        TurnAction::SCISSORS => 3 + 0,
      }
    }
    TurnAction::PAPER => {
      match game_turn.second_move {
        TurnAction::ROCK => 1 + 0,
        TurnAction::PAPER => 2 + 3,
        TurnAction::SCISSORS => 3 + 6,
      }
    }
    TurnAction::SCISSORS => {
      match game_turn.second_move {
        TurnAction::ROCK => 1 + 6,
        TurnAction::PAPER => 2 + 0,
        TurnAction::SCISSORS => 3 + 3,
      }
    }
  }
}

fn parse_game_move(value: &str) -> TurnAction {
  match value {
    "A" | "X" => TurnAction::ROCK,
    "B" | "Y" => TurnAction::PAPER,
    "C" | "Z" => TurnAction::SCISSORS,
    _ => panic!("Invalid input: {}", value)
  }
}

fn parse_turn(input_line: &str) -> Turn {
  let (turn1, turn2) = input_line.split_once(" ").unwrap();

  Turn {
    first_move: parse_game_move(turn1),
    second_move: parse_game_move(turn2),
  }
}

fn parse_turns_from_string() -> Vec<Turn> {
  let contents = include_str!("input.txt");

  contents
    .lines()
    .into_iter()
    .map(parse_turn)
    .collect()
}

fn part1(turns: Vec<Turn>) {
  let scores = turns.iter().map(compute_score_for_turn);
  let total_score: u32 = scores.sum();

  println!("{}", total_score)
}

fn main() {
  let turns = parse_turns_from_string();
  part1(turns);
}
