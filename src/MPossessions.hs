module MPossessions where

    import Base

    type Row = [Int]
    type Possessions = [Row]

    emptyRow :: Row
    emptyRow = replicate (6+6+9) cPossible
    
    -- delete this
    listToIndexedList row = zip [1..(length row)] row

    markStateInRow :: Int -> Int -> Row -> Row
    markStateInRow index state row = [ if i == index then state else x | (i, x) <- (listToIndexedList row) ]
    
    markStateInPossessions :: Int -> Int -> Int -> Possessions -> Possessions
    markStateInPossessions pI cI s p = [ if i == pI then (markStateInRow cI s r) else r | (i, r) <- (listToIndexedList p) ]
    
    markDefiniteInPossessions :: Int -> Int -> Possessions -> Possessions
    markDefiniteInPossessions playerIndex clueIndex possessions = foldl (\p pI -> if pI == playerIndex then markStateInPossessions pI clueIndex cDefinitely p else markStateInPossessions pI clueIndex cImpossible p) possessions [1..6]

    markInRowMultiple :: [Int] -> Int -> Row -> Row
    markInRowMultiple indices state row = foldl (\r i -> markStateInRow i state r ) row indices

    markInPossessions :: Int -> Int -> Int -> Possessions -> Possessions
    markInPossessions playerIndex clueIndex state possessions
        | state == cImpossible = markStateInPossessions playerIndex clueIndex state possessions
        | state == cDefinitely = markDefiniteInPossessions playerIndex clueIndex possessions

    mapIndex i = if i <= 6 then i else if i <= 12 then i-6 else i-12
    mapIndices ls = map (\i -> if i <= 6 then i else if i <= 12 then i-6 else i-12) ls
    unmapIndex i offset = i + offset
    unmapPersons ls = map (\i -> i) ls
    unmapWeapons ls = map (\i -> i+6) ls
    unmapLocations ls = map (\i -> i+12) ls
    myRow persons weapons locations = foldl (\p ls -> markInRowMultiple ls cDefinitely p) emptyRow [(unmapPersons persons), (unmapWeapons weapons), (unmapLocations locations)]
    markMultipleDefinite playerIndex persons weapons locations possessions
        | length persons > 0 = markMultipleDefinite playerIndex [] weapons locations (mark possessions p')
        | length weapons > 0 = markMultipleDefinite playerIndex persons [] locations (mark possessions w')
        | length locations > 0 = markMultipleDefinite playerIndex persons weapons [] (mark possessions l')
        | otherwise = possessions
        where p' = unmapPersons persons
              w' = unmapWeapons weapons
              l' = unmapLocations locations
              mark possessions indices = foldl (\p i -> markInPossessions playerIndex i cDefinitely p) possessions indices
    getPlayersBetween asker responder
        | (asker < responder) && (responder-asker > 1) = [(asker+1)..(responder-1)]
        | (asker < responder) && (responder-asker == 1) = []
        | (asker > responder) && (asker-responder < 5) = [(asker+1)..6] ++ [1..(responder-1)]
        | (asker > responder) && (asker-responder == 5) = []
        | otherwise = error "getPlayersBetween"

    getNth ls ind = snd (head (filter (\t -> if (fst t) == ind then True else False) (zip [1..length(ls)] ls)))

    getRow gameState ind = getNth p ind
        where p = possessions gameState

    getCol gameState ind = map (\r -> getNth (row r) ind) [1..6]
        where p = possessions gameState
              row r = getRow gameState r
    
    numStateInColumn state column p = [ imp | row <- p,
                                              iCol <- (zip [1..21] row),
                                              (fst iCol == column),
                                              let imp = if (snd iCol) == state then 1 else 0 ]