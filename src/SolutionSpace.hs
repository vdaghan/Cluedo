module SolutionSpace where

    import Data.List
    import Debug.Trace

    import Base
    import MClues
    import MPossessions

    solutionSpaceUpperBound :: GameState -> Integer
    solutionSpaceUpperBound gameState = upperBound
        where p = possessions gameState
              addRows r1 r2 = map (\r -> ((fst r) + (snd r))) (zip r1 r2)
              mapRow r = map (\c -> if c == cPossible then 1 else 0) r
              numPossibles = foldl (\p r -> addRows p (mapRow r)) (replicate 21 0) p
              numPossibilities = map (\n -> if n == 0 then 1 else n+1) numPossibles
              upperBound = product numPossibilities

    chooseNRecursion :: [Int] -> Int -> [[Int]] -> [[Int]]
    chooseNRecursion ls n choices
        | n <= 0 = choices
        | otherwise = foldl (\p x -> p ++ chooseNRecursion (dropWhile (<= x) ls) (n-1) [c ++ [x] | c <- choices]) [] ls
    
    -- all n-element sublists of the list ls
    chooseN :: [Int] -> Int -> [[Int]]
    chooseN ls n = chooseNRecursion ls n [[]]

    -- replace elements indexed by choice in list ls with i
    replaceChoices :: [Int] -> [Int] -> Int -> [Int]
    replaceChoices ls choice i = map (\indexed -> if (elem (fst indexed) choice) then i else (snd indexed)) (zip [1..length(ls)] ls)
    
    deleteAllFromList :: [Int] -> [Int] -> [Int]
    deleteAllFromList toDelete fromList
        | null toDelete = fromList
        | otherwise = deleteAllFromList (tail toDelete) (delete (head toDelete) fromList)
    
    deleteFromTupleList :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    deleteFromTupleList toDelete = foldl (\p t -> if t == toDelete then p else p ++ [t]) []

    -- repeatedly choose n elements from list ls and replace them with i, where (n, i) <- directives
    chooseRepeatedlyRecursion :: [Int] -> [(Int, Int)] -> [[Int]] -> [[Int]]
    chooseRepeatedlyRecursion ls directives choices
        | null directives = choices
        | otherwise = [ c | let d = head directives,
                            let n = fst d,
                            let i = snd d,
                            chosen <- chooseN ls n,
                            let ls' = deleteAllFromList chosen ls,
                            let choices' = [ replaceChoices c chosen i | c <- choices ],
                            let d' = deleteFromTupleList d directives,
                            c <- chooseRepeatedlyRecursion ls' d' choices']
    chooseRepeatedly ls unknown known = chooseRepeatedlyRecursion refinedList unknown [replicate numChoices 0]
        where numChoices = length known + foldl (\p d -> p + fst d) 0 unknown
              refinedList = deleteAllFromList known ls

    --getPersonListFromGameState gameState
    computeSolutionSpace :: GameState -> [[Int]]
    computeSolutionSpace gameState = [ c | person <- personList,
                                           weapon <- weaponList,
                                           location <- locationList,
                                           c <- chooseRepeatedly [1..21] unknown [person, weapon, location] ]
        where p = possessions gameState
              --possibilities = m
              unknown = [(3, 1),(3, 2),(3, 3),(3, 4),(3, 5),(3, 6)]
              personList = [1..6]
              weaponList = [7..12]
              locationList = [13..21]
    gpr :: [[Int]] -> [Int] -> [Int]
    gpr (x:xs) (y:ys)
        | null xs = get y x
        | otherwise = get y x ++ gpr xs ys
        where get ind ls
                  | length ls >= ind = [snd ( head (filter (\t -> fst t == ind) (zip [0..length ls-1] ls)))]
                  | otherwise = []
    enumLength = foldl (\p n -> p * length n) 1
    enumToIndexList :: [Int] -> Int -> [Int]
    enumToIndexList ls ind
        | null ls = []
        | otherwise = r:enumToIndexList (tail ls) ind'
        where r = rem ind (head ls)
              ind' = div (ind - r) (head ls)
    
    cSS :: GameState -> [[Int]]
    cSS gameState = filteredPossibilities
        where p = possessions gameState
              encodeColumn cI = encoded
                where col = getCol gameState cI
                      numState s = foldl (\p c -> if c == s then p + 1 else p) 0 col
                      numDefinites = numState cDefinitely
                      numImpossibles = numState cImpossible
                      possibles = map fst (filter (\t -> snd t == cPossible) (zip [1..6] col))
                      definites = map fst (filter (\t -> snd t == cDefinitely) (zip [1..6] col))
                      encoded = possibles ++ (if numDefinites == 0 then [0] else definites)
              encodedPossibilities = map encodeColumn [1..21]
              numPossibilities = enumLength encodedPossibilities
              encodedNumPossibilities = map length encodedPossibilities
              possibilities = map (gpr encodedPossibilities . enumToIndexList encodedNumPossibilities) [0..numPossibilities-1]
              -- Filter helpers
              personGroup = [1..6]
              weaponGroup = [7..12]
              locationGroup = [13..21]
              findGroup g ls = map snd (filter (\t -> elem (fst t) g) (zip [1..21] ls))
              exactlyOneAnswerInGroup g ls = 1 == foldl (\p e -> if e == 0 then p+1 else p) 0 (findGroup g ls)
              usefulClues = filter (isClueUseful gameState) (clues gameState)
              unmapClue c = (pl, (unmapPersons ps, unmapWeapons ws, unmapLocations ls))
                where (pl, (ps,ws,ls)) = c
              respectsClue c lst = any (\e -> (snd e == pl) && elem (fst e) (concat [ps,ws,ls]) ) (zip  [1..21] lst)
                where (pl, (ps,ws,ls)) = unmapClue c
              rc c ls = respectsClue c ls
              -- Filters
              exactlyOneAnswerPerGroup ls = all (\g -> exactlyOneAnswerInGroup g ls) [personGroup, weaponGroup, locationGroup]
              exactlyThreeAnswersPerPlayer ls = all (\p -> 3 == foldl (\s pI -> if pI == p then s+1 else s) 0 ls) [1..6]
              respectsClues ls = all (\c -> rc c ls) usefulClues
              --atMostOneAnswerPerClue is satisfied by design
              -- Application of filters
              filters = [ exactlyThreeAnswersPerPlayer, exactlyOneAnswerPerGroup, respectsClues ]
              filteredPossibilities = foldl (flip filter) possibilities filters

    solutionSpaceProbabilities :: GameState -> [[Int]] -> [[Int]]
    solutionSpaceProbabilities gameState sS = probabilities
        where sSLen = length sS
              emptyBin = replicate 7 0
              emptyProbabilities = replicate 21 emptyBin
              addToBin bin ind = map (\t -> if fst t == ind then (snd t)+1 else (snd t)) (zip [0..6] bin)
              processSolution sol prob = map (\t -> addToBin (snd t) (fst t)) (zip sol prob)
              occurences = foldl (flip processSolution) emptyProbabilities sS
              probabilities = map (map (\p -> div (p*100) sSLen)) occurences
              --probabilities = [ | solution <- sS ]
    
    answerProbabilities = map (head . take 1)
    
    solutionSpace :: GameState -> (Integer, Int, [[Int]], [[Int]], [Int])
    solutionSpace gameState
        | sSUB > 105000 = (sSUB, 0, [[]], [[]], [])
        | otherwise = (sSUB, length sS, sS, sSP, aP)
        where sSUB = solutionSpaceUpperBound gameState
              sS = cSS gameState
              sSP = solutionSpaceProbabilities gameState sS
              aP = answerProbabilities sSP

    --solutionSpaceProbabilities :: GameState -> (Integer, [[Int]]) -> 