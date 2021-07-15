/* 
   Member 1: Hai Vu - 500963706
   Member 2: Xuan Le - 500962159
*/

#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct Card{
  Rank: u32,
  Suit: u32,
}

fn convert_card(val: u32) -> Card
{
  let mut rank = val % 13;
  let mut suit = val / 13;
  if rank == 0 {
    rank = 13;
    suit = suit - 1;
  }
  if rank == 1 {
    rank = 14;
  }
  Card {
    Rank: rank,
    Suit: suit,
  }
}

pub fn deal(perm: [u32;9]) -> Vec<String>
{
  let mut cards_hand_1 : Vec<Card> = Vec::new();
  let mut cards_hand_2 : Vec<Card> = Vec::new();
  let mut i = 0;

  //split cards
  while i < 9 {
    if i < 4 {
      cards_hand_1.push(convert_card(perm[i]));
      cards_hand_2.push(convert_card(perm[i+1]));
      i += 2;
    }
    else {
      cards_hand_1.push(convert_card(perm[i]));
      cards_hand_2.push(convert_card(perm[i]));
      i += 1
    }
  }

  let mut hand_1 : Vec<Card>;
  let mut hand_2 : Vec<Card>;
  let mut best_hand_1= vec![cards_hand_1[0], cards_hand_1[1], cards_hand_1[2], cards_hand_1[3], cards_hand_1[4]];
  let mut best_hand_2= vec![cards_hand_2[0], cards_hand_2[1], cards_hand_2[2], cards_hand_2[3], cards_hand_2[4]];

  for i in 0..7{
    for j in (i+1)..7{
      hand_1 = Vec::new();
      hand_2 = Vec::new();
      for z in 0..7 {
        if z == i || z == j {
          continue;
        }
        hand_1.push(cards_hand_1[z]);
        hand_2.push(cards_hand_2[z]);
      }
      best_hand_1 = better_hand(best_hand_1, hand_1);
      best_hand_2 = better_hand(best_hand_2, hand_2);
    }
  }
  let mut best_hand_string : Vec<String> = Vec::new();
  for card in better_hand(best_hand_1, best_hand_2).iter(){
    best_hand_string.push(concat(&rank_to_string(card.Rank), &suit_to_string(card.Suit)));
  }

  best_hand_string

}

fn better_hand(hand1: Vec<Card>, hand2: Vec<Card>) -> Vec<Card> {
  let mut hand1_new : Vec<Card> = Vec::new();
  let mut hand2_new : Vec<Card> = Vec::new();
  let hand1_sort = sort(hand1);
  let hand2_sort = sort(hand2);

  for card in hand1_sort.iter(){
    hand1_new.push(*card);
  }
  for card in hand2_sort.iter(){
    hand2_new.push(*card);
  }
  match (evaluate(hand1_sort), evaluate(hand2_sort)) {
    (x, y) if x.0 > y.0 => hand1_new,
    (x, y) if x.0 < y.0 => hand2_new,

    (x, y) if x.0 == y.0 && x.1 > y.1 => hand1_new,
    (x, y) if x.0 == y.0 && x.1 < y.1 => hand2_new,

    (x, y) if x.0 == y.0 && x.1 == y.1 && x.2 > y.2 => hand1_new,
    (x, y) if x.0 == y.0 && x.1 == y.1 && x.2 < y.2 => hand2_new,

    (x, y) if x.0 == y.0 && x.1 == y.1 && x.2 == y.2 && x.3 > y.3 => hand1_new,
    (x, y) if x.0 == y.0 && x.1 == y.1 && x.2 == y.2 && x.3 < y.3 => hand2_new,

    (x, y) if x.0 == y.0 && x.1 == y.1 && x.2 == y.2 && x.3 == y.3 && x.4 > y.4 => hand1_new,
    (x, y) if x.0 == y.0 && x.1 == y.1 && x.2 == y.2 && x.3 == y.3 && x.4 < y.4 => hand2_new,

    (x, y) if x.0 == y.0 && x.1 == y.1 && x.2 == y.2 && x.3 == y.3 && x.4 == y.4 && x.5 > y.5 => hand1_new,
    (x, y) if x.0 == y.0 && x.1 == y.1 && x.2 == y.2 && x.3 == y.3 && x.4 == y.4 && x.5 < y.5 => hand2_new,

    _ => hand1_new,
  }
}


fn evaluate(hand: Vec<Card>) -> (u32, u32, u32, u32, u32, u32) {
  let c1 = hand[0];
  let c2 = hand[1];
  let c3 = hand[2];
  let c4 = hand[3];
  let c5 = hand[4];
  match (c1.Rank, c2.Rank, c3.Rank, c4.Rank, c5.Rank){
    (14, 13, 12, 11, 10) if is_flush(c1, c2, c3, c4, c5) => (10, 0, 0, 0, 0, 0),

    (14, 5, 4, 3, 2) if is_flush(c1, c2, c3, c4, c5) => (9, 5, 0, 0, 0, 0),
    (a, b, c, d, e) if a == b + 1 && b == c + 1 && c == d + 1 && d == e + 1 && is_flush(c1, c2, c3, c4, c5) => (9, a, 0, 0,0, 0),

    (a, b, c, d, e) if (a == b && b == c && c == d) => (8, a, e, 0, 0, 0),
    (a, b, c, d, e) if (b == c && c == d && d == e) => (8, b, a, 0, 0, 0),

    (a, b, c, d, e) if (a == b && b == c && d == e) => (7, a, d,0, 0, 0),
    (a, b, c, d, e) if (a == b && c == d && d == e) => (7, c, a, 0, 0, 0),

    (a, b, c, d, e) if is_flush(c1, c2, c3, c4, c5) => (6, a, b, c, d, e),

    (14, 13, 12, 11, 10) => (5, 14, 0, 0, 0, 0),
    (14, 5, 4, 3, 2) => (5, 5, 0, 0, 0, 0),
    (a, b, c, d, e) if a == b + 1 && b == c + 1 && c == d + 1 && d == e + 1 => (5, a, 0, 0, 0, 0),

    (a, b, c, d, e) if a == b && b == c => (4, a, d, e, 0, 0),
    (a, b, c, d, e) if b == c && c == d => (4, b, a, e, 0, 0),
    (a, b, c, d, e) if c == d && d == e => (4, c, a, b, 0, 0),

    (a, b, c, d, e) if (a == b && c == d) => (3, a, c, e, 0, 0),
    (a, b, c, d, e) if (a == b && d == e) => (3, a, d, c, 0, 0),
    (a, b, c, d, e) if (b == c && d == e) => (3, b, d, a, 0, 0),

    (a, b, c, d, e) if a == b => (2, a, c, d, e, 0),
    (a, b, c, d, e) if b == c => (2, b, a, d, e, 0),
    (a, b, c, d, e) if c == d => (2, c, a, b, e, 0),
    (a, b, c, d, e) if d == e => (2, d, a, b, c, 0),

    (a, b, c, d, e) => (1, a, b, c, d, e),
  }

}

fn is_flush (c1: Card, c2: Card, c3: Card, c4: Card, c5: Card) -> bool {
  match (c1.Suit, c2.Suit, c3.Suit, c4.Suit, c5.Suit) {
    (a, b, c, d, e) if a == b && b == c && c == d && d == e => true,
    _ => false,
  }
}


fn sort(mut hand: Vec<Card>) -> Vec<Card> {
  hand.sort_by(|a, b|  b.Rank.cmp(&a.Rank));
  hand
}


fn suit_to_string(suit: u32) -> String {
  match suit {
    0 => String::from("C"),
    1 => String::from("D"),
    2 => String::from("H"),
    3 => String::from("S"),
    _ => String::from(""),
  }
}

fn rank_to_string(rank: u32) -> String {
  match rank {
    14 => String::from("1"),
    2 => String::from("2"),
    3 => String::from("3"),
    4 => String::from("4"),
    5 => String::from("5"),
    6 => String::from("6"),
    7 => String::from("7"),
    8 => String::from("8"),
    9 => String::from("9"),
    10 => String::from("10"),
    11 => String::from("11"),
    12 => String::from("12"),
    13 => String::from("13"),
    _ => String::from(""),
  }
}

fn concat(w1: &str, w2: &str) -> String {
    let mut result = String::from(w1);
    result.push_str(w2); 
    result = String::from(result);
    return result;
}