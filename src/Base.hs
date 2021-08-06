module Base where
    
    -- gameState is a 3-Tuple:
    --     1. Int => Who I am?
    --     2. [[Int]] => A list of rows per player where rows represent card ownage (definitely holds, does not hold, unknown but possible) of respective player
    --     3. [[[Int]]] => A list of rows per player where each column has a list of clue groups
    type Clue = (Int, ([Int], [Int], [Int]))
    type Claim = (Int, Int, Int)
    data GameState = GameState {myid :: Int, possessions :: [[Int]], clues :: [Clue]} deriving (Show)

    cImpossible :: Int
    cImpossible = 0
    cDefinitely :: Int
    cDefinitely = 1
    cPossible :: Int
    cPossible = 2