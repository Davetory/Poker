-- Member 1: Hai Vu - 500963706
-- Member 2: Ngoc Huyen Oanh Phung - 500976963

module Poker where
  import Data.List (sortBy)
  import Data.Function (on)

  type Hand = [Card]
  type Card = (Int, Int)

  suit_to_string :: Int -> String  
  suit_to_string s = case s of
    0 -> "C"
    1 -> "D"
    2 -> "H"
    3 -> "S"
    _ -> ""
  
  rank_to_string :: Int -> String
  rank_to_string r = case r of
    14 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "10"
    11 -> "11"
    12 -> "12"
    13 -> "13"
    _  -> ""

  parse_card :: Integer -> Card
  parse_card c = case c of
    x | mod x 13 == 0 -> (13, fromIntegral (div x 13) - 1)
    y | mod y 13 == 1 -> (14, fromIntegral (div y 13))
    z                   -> (fromIntegral (mod z 13), fromIntegral (div z 13))

  deal :: [Integer] -> [String]
  deal i = let hand1 = best_hand $ get_comb 5 $ split_card i 0 2
               hand2 = best_hand $ get_comb 5 $ split_card i 1 3
            in parse_hand $ better_hand hand1 hand2


  parse_hand :: Hand -> [String]
  parse_hand [] = []
  parse_hand h = map (\(x,y) -> rank_to_string x ++ suit_to_string y) h


  best_hand :: [Hand] -> Hand
  best_hand [] = []
  best_hand (xh:xt) = better_hand xh (best_hand xt)


  get_comb :: Int -> Hand -> [Hand]
  get_comb 0 _ = [[]]
  get_comb _ [] = []
  get_comb n (xh:xt) = get_comb n xt ++ map (xh:) (get_comb (n-1) xt)


  split_card :: [Integer] -> Int -> Int -> Hand
  split_card d s s1 = map parse_card [get_index d s] ++ map parse_card [get_index d s1] ++ map parse_card (drop 4 d)

  get_index :: [Integer] -> Int -> Integer
  get_index (xh:xt) s | s == 0 = xh
                     | otherwise = get_index xt (s-1)


  sort_hand :: Hand -> Hand
  sort_hand h = reverse (sortBy (compare `on` fst) h)


  better_hand :: Hand -> Hand -> Hand
  better_hand h1 h2 = if evaluate(sort_hand h1) > evaluate(sort_hand h2) then h1
                      else if evaluate(sort_hand h1) < evaluate(sort_hand h2) then h2
                      else h1


  evaluate :: Hand -> [Int]
  evaluate h = case h of 
    [(14,_),(13,_),(12,_),(11,_),(10,_)] | isFlush h -> [10, 0, 0, 0, 0, 0]

    [(14,_),(5,_),(4,_),(3,_),(2,_)] |isFlush h -> [9, 5, 0, 0, 0, 0]
    [(c1,_),(_,_),(_,_),(_,_),(_,_)] | isStraight h && isFlush h -> [9, c1, 0, 0, 0, 0]

    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c1, c2, c3, c4] -> [8, c1, c5, 0, 0, 0]
    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c2, c3, c4, c5] -> [8, c2, c1, 0, 0, 0]

    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c1, c2, c3] && isEqual [c4, c5] -> [7, c1, c4, 0, 0, 0]
    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c1, c2] && isEqual [c3, c4, c5] -> [7, c3, c1, 0, 0, 0]

    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isFlush h -> [6, c1, c2, c3, c4, c5]

    [(14,_),(13,_),(12,_),(11,_),(10,_)] ->  [5, 14, 0, 0, 0, 0]
    [(14,_),(5,_),(4,_),(3,_),(2,_)] ->  [5, 5, 0, 0, 0, 0]
    [(c1,_),(_,_),(_,_),(_,_),(_,_)] | isStraight h ->  [5, c1, 0, 0, 0, 0]

    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c1, c2, c3] -> [4, c1, c4, c5, 0, 0]
    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c2, c3, c4] -> [4, c2, c1, c5, 0, 0]
    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c3, c4, c5] -> [4, c3, c1, c2, 0, 0]

    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c1, c2] && isEqual [c3, c4] -> [3, c1, c3, c5, 0, 0]
    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c1, c2] && isEqual [c4, c5] -> [3, c1, c4, c3, 0, 0]
    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c2, c3] && isEqual [c4, c5] -> [3, c2, c4, c1, 0, 0]

    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c1, c2] -> [2, c1, c3, c4, c5, 0]
    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c2, c3] -> [2, c2, c1, c4, c5, 0]
    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c3, c4] -> [2, c3, c1, c2, c5, 0]
    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] | isEqual [c4, c5] -> [2, c4, c1, c2, c3, 0]

    [(c1,_),(c2,_),(c3,_),(c4,_),(c5,_)] -> [1, c1, c2, c3, c4, c5]
    
    _ ->  [0, 0, 0, 0, 0]


  isStraight :: Hand -> Bool
  isStraight = isConsecutive . map fst

  isConsecutive :: [Int] -> Bool
  isConsecutive (x:x1:xs) = x == x1+1 && isConsecutive (x1:xs)
  isConsecutive _ = True

  isFlush :: Hand -> Bool
  isFlush = isEqual . map snd

  isEqual :: [Int] -> Bool
  isEqual (x:x1:xs) = x == x1 && isEqual(x1:xs)
  isEqual _ = True