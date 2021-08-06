module Cluedo where

    import Data.List
    import Debug.Trace

    import Base
    import MClues
    import MPossessions
    import SolutionSpace
    import Utilities
    
    clueToChar clue
        | clue == cImpossible = "X "
        | clue == cDefinitely = "O "
        | clue == cPossible = ". "
        | otherwise = "E "
    clueBreak ind = if (ind == 7) || (ind == 13) then " | " else ""
    printRow row = Prelude.print (concatMap (\i -> clueBreak (fst i) ++ clueToChar (snd i)) (zip [1..length row] row))
    printGameState gameState = mapM_ (\r -> printRow r) gameState -- hlint hints at something called Eta reduce?

    combination n k = round( product [n-k+1..n] / product [1..k] )
    numSolutions = c 6 1 * c 6 1 * c 9 1 * c 15 3 * c 12 3 * c 9 3 * c 6 3 where c = combination

    answers = [ [(1, x), (2, y), (3, z)] | x <- [1..6], y <- [1..6], z <- [1..9] ]
    


    newGame :: GameState
    newGame = GameState { myid = -1, possessions = replicate 6 emptyRow, clues = [] }
    iAm :: Int -> GameState -> GameState
    iAm myid gameState = GameState {myid = myid, possessions = possessions gameState, clues = clues gameState}
    iHold :: ([Int], [Int], [Int]) -> GameState -> GameState
    iHold cards gameState = GameState { myid = myid gameState, possessions = p', clues = clues gameState }
        where (persons, weapons, locations) = cards
              p = possessions gameState
              p' = markMultipleDefinite (myid gameState) persons weapons locations p
    -- If I claim and there is no answer, then those cards which I do not hold are parts of solution
    iClaimNoAnswer :: Claim -> GameState -> GameState
    iClaimNoAnswer claim gameState = checkGameState gS'
        where (person, weapon, location) = claim
              gS' = gameState
    -- If I claim and they answer, then the answer is a part of solution
    --     Also, players between me and responder does not hold any of the claimed cards
    iClaimTheyAnswer :: Claim -> Int -> Int -> GameState -> GameState
    iClaimTheyAnswer claim responder answer gameState = checkGameState gS'
        where playersBetween = getPlayersBetween (myid gameState) responder
              (person, weapon, location) = claim
              cIList = [head (unmapPersons [person]), head (unmapWeapons [weapon]), head (unmapLocations [location])]
              p = possessions gameState
              plcITuples = [ (pl, cI) | pl <- playersBetween, cI <- cIList ]
              p' = foldl (\pos plcI -> uncurry markStateInPossessions plcI cImpossible pos) p plcITuples
              cI
                  | answer == 1 = head cIList
                  | answer == 2 = head (tail cIList)
                  | otherwise = head (tail (tail cIList))
              p'' = markDefiniteInPossessions responder cI p'
              gS' = GameState {myid = myid gameState, possessions = p'', clues = clues gameState}
    -- If they claim and there is no answer, then either asker holds claimed cards or they are parts of solution
    theyClaimNoAnswer :: Int -> Claim -> GameState -> GameState
    theyClaimNoAnswer asker claim gameState = checkGameState gS'
        where playersBetween = delete (myid gameState) (delete asker [1..6])
              (person, weapon, location) = claim
              cIList = [head (unmapPersons [person]), head (unmapWeapons [weapon]), head (unmapLocations [location])]
              p = possessions gameState
              plcITuples = [ (pl, cI) | pl <- playersBetween, cI <- cIList ]
              p' = foldl (\pos plcI -> uncurry markStateInPossessions plcI cImpossible pos) p plcITuples
              gS' = GameState {myid = myid gameState, possessions = p', clues = clues gameState}
    -- If they claim and I answer, then the players between asker and me does not hold any of the claimed cards
    theyClaimIAnswer :: Int -> Claim -> Int -> GameState -> GameState
    theyClaimIAnswer asker claim answer gameState = checkGameState gS'
        where playersBetween = getPlayersBetween asker (myid gameState)
              (person, weapon, location) = claim
              cIList = [head (unmapPersons [person]), head (unmapWeapons [weapon]), head (unmapLocations [location])]
              p = possessions gameState
              plcITuples = [ (pl, cI) | pl <- playersBetween, cI <- cIList ]
              p' = foldl (\pos plcI -> uncurry markStateInPossessions plcI cImpossible pos) p plcITuples
              gS' = GameState {myid = myid gameState, possessions = p', clues = clues gameState}
    -- If they claim and they answer, then responder holds at least one of the claimed cards
    --     Also, players between asker and responder does not hold any of the claimed cards
    theyClaimTheyAnswer :: Int -> Claim -> Int -> GameState -> GameState
    theyClaimTheyAnswer asker claim responder gameState = checkGameState gS''
        where playersBetween = getPlayersBetween asker responder
              (person, weapon, location) = claim
              cIList = [head (unmapPersons [person]), head (unmapWeapons [weapon]), head (unmapLocations [location])]
              p = possessions gameState
              plcITuples = [ (pl, cI) | pl <- playersBetween, cI <- cIList ]
              p' = foldl (\pos plcI -> uncurry markStateInPossessions plcI cImpossible pos) p plcITuples
              gS' = GameState {myid = myid gameState, possessions = p', clues = clues gameState}
              clue = claimToClue responder claim
              gS'' = noteClue gS' clue
    -- If they accuse and the answer is wrong, then at least one of the claimed cards is not a solution
    theyAccuse :: Int -> Claim -> Bool -> GameState -> GameState
    theyAccuse asker claim truth gameState = checkGameState gS'
        where (person, weapon, location) = claim
              gS' = gameState
    

    checkSolutionPerGroup gameState = gS'
        where p = possessions gameState
              personIndices = [1..6]
              weaponIndices = [7..12]
              locationIndices = [13..21]
              statesInColumn state column = [ imp | row <- p,
                                                    iCol <- zip [1..21] row,
                                                    fst iCol == column,
                                                    let imp = if snd iCol == state then 1 else 0 ]
              numImpossiblesInColumn column = sum (statesInColumn cImpossible column)
              numImpossiblesInColumns columns = map numImpossiblesInColumn columns
              solvedForPersons = elem 6 (numImpossiblesInColumns personIndices)
              solvedForWeapons = elem 6 (numImpossiblesInColumns weaponIndices)
              solvedForLocations = elem 6 (numImpossiblesInColumns locationIndices)
              numDefinitesInColumn column = sum (statesInColumn cDefinitely column)
              numDefinitesInColumns columns = map numDefinitesInColumn columns
              numPossiblesInColumn column = sum (statesInColumn cPossible column)
              numPossiblesInColumns columns = map numPossiblesInColumn columns
              pruneIncomplete pp solved indices
                  | solved = [ [ c | iCol <- zip [1..21] row,
                                     let col = snd iCol,
                                     let indCol = fst iCol,
                                     let c = if elem indCol indices && (numImpossiblesInColumn indCol == 5) && (numPossiblesInColumn indCol == 1) && (col == cPossible) then cDefinitely else col ] | row <- pp ]
                  | otherwise = pp
              p1 = pruneIncomplete p solvedForPersons personIndices
              p2 = pruneIncomplete p1 solvedForWeapons weaponIndices
              p3 = pruneIncomplete p2 solvedForLocations locationIndices
              solvedForGroup' indices = (length indices - 1) == sum (numDefinitesInColumns indices)
              markAnswerInColumn col pos = foldl (\pos' r -> markInPossessions r col cImpossible pos' ) pos [1..6]
              markOnly pp indices
                  | solvedForGroup' indices = foldl (\pp' i -> if elem 1 (statesInColumn cDefinitely i) then pp' else markAnswerInColumn i pp' ) pp indices
                  | otherwise = pp
              p4 = markOnly p3 personIndices
              p5 = markOnly p4 weaponIndices
              p6 = markOnly p5 locationIndices
              gS' =  GameState {myid = myid gameState, possessions = p6, clues = clues gameState}
    numDefiniteInRow row = foldl (\n col -> if col == cDefinitely then n+1 else n) 0 row
    checkNumDefiniteInRow row
        | numDefinite == 3 = map (\col -> if col /= cDefinitely then cImpossible else cDefinitely) row
        | otherwise = row
        where numDefinite = numDefiniteInRow row
    checkNumDefinitely possessions = map (\row -> checkNumDefiniteInRow row) possessions
    checkGameState gameState = gS''''
        where p = possessions gameState
              p' = checkNumDefinitely p
              gS' = GameState {myid = myid gameState, possessions = p', clues = clues gameState}
              gS'' = processClues gS'
              gS''' = checkSolutionPerGroup gS''
              changed = p /= possessions gS'''
              gS'''' = if changed then checkGameState gS''' else gS'''
    
    print :: GameState -> IO()
    print gameState = do
        printGameState (possessions gameState)
        putStr "Useful Clues: "
        Prelude.print usefulClues
        putStr "Outdated Clues: "
        Prelude.print outdatedClues
        putStr "Solution space length (upperbound): "
        Prelude.print upperBound
        putStr "Solution space length: "
        Prelude.print nSS
        putStr "Solution space: "
        Prelude.print sS
        putStr "Solution space probabilities: "
        Prelude.print sSP
        putStr "Answer probabilities: "
        Prelude.print aP
        where usefulClues = filter (isClueUseful gameState) (clues gameState)
              outdatedClues = filter (isClueUseful gameState) (clues gameState)
              sST = solutionSpace gameState
              (upperBound, nSS, sS, sSP, aP) = sST

        