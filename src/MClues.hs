module MClues where

    import Data.List
    import Debug.Trace

    import Base
    import MPossessions
    
    isClueUseful :: GameState -> Clue -> Bool
    isClueUseful gameState clue = result
        where (player, (persons, weapons, locations)) = clue
              playerRow = getRow gameState player
              persons' = unmapPersons persons
              weapons' = unmapWeapons weapons
              locations' = unmapLocations locations
              isIndexOfPerson = \t -> ([] /= persons') && (t == head persons')
              isIndexOfWeapon = \t -> ([] /= weapons') && (t == head weapons')
              isIndexOfLocation = \t -> ([] /= locations') && (t == head locations')
              isIndexOfClue = \t -> isIndexOfPerson t || isIndexOfWeapon t || isIndexOfLocation t
              indexedTuplesCorrespondingToAnswer = filter (isIndexOfClue . fst) (zip [1..length playerRow] playerRow)
              columnCorrespondingToAnswer = map snd indexedTuplesCorrespondingToAnswer
              coincidesWithADefinite = elem cDefinitely columnCorrespondingToAnswer
              result = not coincidesWithADefinite
    noteClue :: GameState -> Clue -> GameState
    noteClue gameState clue
        | isClueUseful gameState clue = GameState {myid = myid gameState, possessions = possessions gameState, clues = c'}
        | otherwise = gameState
        where (player, (persons, weapons, locations)) = clue
              c = clues gameState
              c' = c ++ [clue]
    claimToClue :: Int -> Claim -> Clue
    claimToClue player claim = (player, ([person],[weapon],[location]))
        where (person, weapon, location) = claim

    unresolvedPartOfClue :: GameState -> Clue -> [Int]
    unresolvedPartOfClue gameState clue = unresolvedPart
        where (player, (persons, weapons, locations)) = clue
              playerRow = getRow gameState player
              persons' = unmapPersons persons
              weapons' = unmapWeapons weapons
              locations' = unmapLocations locations
              isIndexOfPerson = \t -> ([] /= persons') && (t == head persons')
              isIndexOfWeapon = \t -> ([] /= weapons') && (t == head weapons')
              isIndexOfLocation = \t -> ([] /= locations') && (t == head locations')
              isIndexOfClue = \t -> isIndexOfPerson t || isIndexOfWeapon t || isIndexOfLocation t
              indexedTuplesCorrespondingToAnswer = filter (isIndexOfClue . fst) (zip [1..length playerRow] playerRow)
              indexedUnresolvedPart = filter (\t -> snd t == cPossible) indexedTuplesCorrespondingToAnswer
              unresolvedPart = map fst indexedUnresolvedPart
    groupClueSetsByPlayer :: GameState -> [(Int, [([Int], [Int], [Int])])]
    groupClueSetsByPlayer gameState = result
        where usefulClues = filter (isClueUseful gameState) (clues gameState)
              cluesFromPlayer player = (player, [ snd c | c <- usefulClues, fst c == player ])
              result = [ cluesFromPlayer p | p <- [1..6] ]
    doCluesIntersect :: ([Int], [Int], [Int]) -> ([Int], [Int], [Int]) -> Bool
    doCluesIntersect c1 c2 = personsIntersect || weaponsIntersect || locationsIntersect
        where (p1, w1, l1) = c1
              (p2, w2, l2) = c2
              p1' = unmapPersons p1
              w1' = unmapWeapons w1
              l1' = unmapLocations l1
              p2' = unmapPersons p2
              w2' = unmapWeapons w2
              l2' = unmapLocations l2
              personsIntersect = ([] /= p1') && ([] /= p2') && (head p1' == head p2')
              weaponsIntersect = ([] /= w1') && ([] /= w2') && (head w1' == head w2')
              locationsIntersect = ([] /= l1') && ([] /= l2') && (head l1' == head l2')
    findDisjointClueSets gameState = disjointClues
        where clueSetsByPlayer = groupClueSetsByPlayer gameState
              mapDisjointClues clueSet = map (\c -> c:filter (not . doCluesIntersect c) clueSet) clueSet
              disjointCluesFromPlayer player = (player, head [ mapDisjointClues (snd cS) | cS <- clueSetsByPlayer, fst cS == player ])
              disjointClues = [ disjointCluesFromPlayer p | p <- [1..6] ]
    --maxDisjointClueSets :: GameState -> [(Int, Int)]
    safeMaximum ls
        | null ls = 0
        | otherwise = maximum ls
    maxDisjointClueSets gameState = result
        where disjointClueSets = findDisjointClueSets gameState
              numDisjointClues clueSet = map length clueSet
              result = [ (p, safeMaximum (numDisjointClues cS)) | dC <- disjointClueSets, let (p, cS) = dC ]
    knownsForClueSet :: [([Int], [Int], [Int])] -> [Int]
    knownsForClueSet = foldl (\p c -> p ++ getPWL c) []
        where getP (p,w,l) = if null p then [] else unmapPersons p
              getW (p,w,l) = if null w then [] else unmapWeapons w
              getL (p,w,l) = if null l then [] else unmapLocations l
              getPWL (p,w,l) = getP (p,w,l) ++ getW (p,w,l) ++ getL (p,w,l)
    powerSet cS
        | null cS = [[]]
        | otherwise = [ x:ps | let (x:xs) = cS, ps <- powerSet xs] ++ powerSet (tail cS)
    findDisjointClues cS = filter isClueSetDisjoint pS
        where doesClueIntersectWithClueSet clue clueSet = any (doCluesIntersect clue) clueSet
              isClueSetDisjoint clueSet = notElem True [ doCluesIntersect c' c'' | c' <- clueSet, c'' <- clueSet, c' /= c'' ]
              pS = filter (not . null) (powerSet cS)
    deleteElemsFromList elems ls = foldl (flip delete) ls elems
    processDisjointClues :: GameState -> [(Int, [Int])]
    processDisjointClues gameState = result
        where clueSetsByPlayer = {-trace ("clueSetsByPlayer: " ++ show (groupClueSetsByPlayer gameState))-} groupClueSetsByPlayer gameState
              disjointCluesFromPlayer player = (player, head [ findDisjointClues (snd cS) | cS <- clueSetsByPlayer, fst cS == player ])
              disjointClueSets = {-trace ("disjointCluesFromPlayer: " ++ show ([ disjointCluesFromPlayer p | p <- [1..6] ]))-} [ disjointCluesFromPlayer p | p <- [1..6] ]
              numDisjointClues clueSet = map length clueSet
              maxDisjointClueLengths :: [(Int, Int)]
              maxDisjointClueLengths = [ (p, safeMaximum (numDisjointClues cS)) | dC <- disjointClueSets, let (p, cS) = dC ]
              maxDisjointClueLengthForPlayer :: Int -> Int
              maxDisjointClueLengthForPlayer p = snd (head (filter (\t -> fst t == p) maxDisjointClueLengths))
              unknownsForClueSet clueSet = deleteElemsFromList (knownsForClueSet clueSet) [1..21]
              definitesInRow row = foldl (\p t -> if snd t == cDefinitely then p ++ [fst t] else p) [] (zip [1..21] row)
              impossiblesInRow row = foldl (\p t -> if snd t == cImpossible then p ++ [fst t] else p) [] (zip [1..21] row)
              impossiblesForClueSets = [ (p, properUnknowns) | dC <- disjointClueSets,
                                                         let (p, cS) = dC,
                                                         c <- cS,
                                                         length c == maxDisjointClueLengthForPlayer p,
                                                         let row = getRow gameState p,
                                                         let unknowns = deleteElemsFromList (definitesInRow row) (unknownsForClueSet c),
                                                         let properUnknowns = deleteElemsFromList (impossiblesInRow row) unknowns,
                                                         length c + length(definitesInRow row) == 3 ]
              result = {-trace ("impossibles: " ++ show impossiblesForClueSets)-} impossiblesForClueSets

    processClue :: GameState -> Clue -> GameState
    processClue gameState clue
        | isClueUseful gameState clue && (length unresolvedPart == 1) = gS'
        | otherwise = gameState
        where p = possessions gameState
              (player, (persons, weapons, locations)) = clue
              unresolvedPart = unresolvedPartOfClue gameState clue
              p' = markInPossessions player (head unresolvedPart) cDefinitely p
              gS' = GameState {myid = myid gameState, possessions = p', clues = clues gameState}
    processClues :: GameState -> GameState
    processClues gameState = gS''
        where gS' = foldl processClue gameState (clues gameState)
              impossibles = processDisjointClues gS'
              p = possessions gS'
              markMultipleInPossessions pI cIList state pos = foldl (\pp cI -> markInPossessions pI cI state pp) pos cIList
              p' = foldl (\pp i -> uncurry markMultipleInPossessions i cImpossible pp) p impossibles
              gS'' = GameState {myid = myid gameState, possessions = p', clues = clues gameState}