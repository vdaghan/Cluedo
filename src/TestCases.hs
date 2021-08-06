module TestCases where

    import Cluedo

    testCase1 = [   iAm 4,
                    iHold ([4], [], [3,4]),
                    theyClaimTheyAnswer 6 (6,5,1) 1,
                    theyClaimTheyAnswer 1 (6,5,1) 5,
                    theyClaimTheyAnswer 2 (2,5,8) 5,
                    theyClaimIAnswer 3 (1,1,3) 3,
                    iClaimTheyAnswer (1,1,1) 5 3,
                    theyClaimTheyAnswer 5 (4,3,9) 1,
                    theyClaimTheyAnswer 6 (2,1,3) 2,
                    theyAccuse 1 (4,4,8) False,
                    theyClaimTheyAnswer 2 (4,1,2) 3,
                    theyClaimTheyAnswer 3 (3,6,8) 6,
                    iClaimTheyAnswer (1,2,7) 6 1,
                    theyClaimTheyAnswer 5 (2,4,6) 6,
                    theyClaimTheyAnswer 6 (4,5,8) 2,
                    theyClaimIAnswer 2 (4,3,4) 1,
                    theyClaimIAnswer 3 (2,6,4) 3,
                    iClaimTheyAnswer (5,1,4) 2 1,
                    theyClaimTheyAnswer 5 (5,1,8) 2,
                    theyClaimTheyAnswer 6 (2,5,5) 5,
                    theyClaimTheyAnswer 2 (2,4,5) 5,
                    theyClaimTheyAnswer 3 (5,2,6) 1{-,
                    iClaimTheyAnswer (5,3,5) 5 3,
                    theyClaimTheyAnswer 5 (4,2,7) 1,
                    theyClaimTheyAnswer 6 (1,6,7) 3,
                    theyClaimTheyAnswer 2 (3,4,9) 3-}
                ]
                --"X X X X X O  | X O . X X X  | X X X X X X X X . "
                --"X X X X O X  | O X X X X X  | X X X X X X X O X "
                --"X X . X X X  | X X X X X X  | X O X X X X O X . "
                --"X X X O X X  | X X X X X X  | X X O O X X X X X "
                --"X . X X X X  | X X X X . X  | O X X X O X X X X "
                --"O . . X X X  | X X X . X .  | X X X X X X X X X "
    
    testCase2 = [   iAm 5,
                    iHold ([2], [3], [5]),
                    theyClaimTheyAnswer 3 (6,3,7) 4,
                    theyClaimTheyAnswer 4 (5,6,1) 6,
                    iClaimTheyAnswer (1,1,2) 1 1,
                    theyClaimTheyAnswer 6 (6,4,2) 2,
                    theyClaimTheyAnswer 1 (4,3,3) 3,
                    theyClaimTheyAnswer 2 (1,2,7) 3,
                    theyClaimTheyAnswer 3 (3,6,3) 1,
                    theyClaimTheyAnswer 4 (1,4,6) 1,
                    iClaimTheyAnswer (4,5,7) 2 2,
                    theyClaimTheyAnswer 6 (2,1,3) 3,
                    theyClaimTheyAnswer 1 (3,2,6) 2,
                    theyClaimTheyAnswer 2 (2,1,2) 4,
                    theyClaimTheyAnswer 3 (5,2,2) 4,
                    theyClaimIAnswer 4 (6,6,5) 3,
                    iClaimTheyAnswer (4,1,7) 4 3,
                    theyClaimIAnswer 6 (6,3,8) 2,
                    theyClaimTheyAnswer 1 (4,1,9) 4,
                    theyClaimIAnswer 2 (2,3,1) 2,
                    theyClaimTheyAnswer 3 (5,2,3) 6{-,
                    theyClaimTheyAnswer 4 (3,1,6) 2,
                    iClaimTheyAnswer (2,3,8) 6 3,
                    theyClaimTheyAnswer 6 (3,1,4) 2,
                    theyClaimTheyAnswer 1 (2,1,6) 3,
                    theyClaimIAnswer 2 (2,4,4) 1,
                    theyAccuse 2 (4,1,4) False,
                    theyClaimNoAnswer 3 (6,2,4),
                    theyClaimNoAnswer 4 (6,1,4)-}
                ]
                --"O X X X X X  | X X X X X O  | . X X X X X X X . "
                --"X X O X X X  | X X X O O X  | X X X X X X X X X "
                --"X X X X X X  | X O X X X X  | X X O X X O X X X "
                --"X X X O X X  | X X X X X X  | X O X X X X O X X "
                --"X O X X X X  | X X O X X X  | X X X X O X X X X "
                --"X X X X O X  | X X X X X X  | . X X X X X X O . "
    
    testCase3 = [   iAm 2,
                    iHold ([4], [], [4,5]),
                    theyClaimTheyAnswer 4 (6,5,1) 5,
                    theyClaimTheyAnswer 5 (5,1,9) 6,
                    theyClaimTheyAnswer 6 (6,2,1) 4,
                    theyClaimTheyAnswer 1 (1,3,9) 4,
                    iClaimTheyAnswer (2,4,2) 3 2,
                    theyClaimTheyAnswer 4 (1,1,2) 5,
                    theyClaimTheyAnswer 5 (3,4,5) 1,
                    theyClaimIAnswer 6 (4,4,6) 1,
                    theyClaimTheyAnswer 1 (5,5,3) 3,
                    iClaimTheyAnswer (2,6,7) 4 1,
                    theyClaimTheyAnswer 3 (3,6,4) 1,
                    theyClaimTheyAnswer 4 (4,6,6) 6,
                    theyClaimTheyAnswer 5 (4,4,2) 6,
                    theyClaimTheyAnswer 6 (5,5,9) 4,
                    theyClaimTheyAnswer 1 (6,3,8) 3{-,
                    iClaimTheyAnswer (1,6,5) 5 1,
                    theyClaimTheyAnswer 3 (6,5,1) 4,
                    theyClaimTheyAnswer 4 (3,2,2) 5,
                    theyClaimTheyAnswer 5 (2,3,7) 1,
                    theyClaimTheyAnswer 6 (2,3,6) 4,
                    theyClaimIAnswer 1 (1,6,4) 3-}
                ]-- iAccuse (5,3,1) True
                -- X X O X X X  | X X X X X O  | X X X X X X O X X
                -- X X X O X X  | X X X X X X  | X X X O O X X X X
                -- X X X X X X  | X X X O X X  | X X O X X X X O X
                -- X O X X X O  | X X X X X X  | X X X X X X X X O
                -- O X X X X X  | X O X X O X  | X X X X X X X X X
                -- X X X X X X  | O X X X X X  | X O X X X O X X X
    
    testCase4 = [   iAm 3,
                    iHold ([1], [2], [8]),
                    theyClaimIAnswer 2 (1,1,1) 1,
                    theyClaimTheyAnswer 4 (1,5,1) 6,
                    theyClaimTheyAnswer 5 (4,6,1) 6,
                    theyClaimTheyAnswer 6 (4,1,1) 5,
                    theyClaimTheyAnswer 1 (6,2,4) 2,
                    theyClaimTheyAnswer 2 (4,3,6) 5,
                    iClaimTheyAnswer (2,4,2) 5 3,
                    theyClaimTheyAnswer 4 (2,3,6) 5,
                    theyClaimTheyAnswer 5 (4,5,6) 4,
                    theyClaimIAnswer 6 (4,2,6) 2,
                    theyClaimIAnswer 1 (5,6,8) 3,
                    theyClaimTheyAnswer 2 (6,3,9) 4,
                    iClaimTheyAnswer (3,4,3) 1 3,
                    theyClaimTheyAnswer 4 (5,3,4) 6,
                    theyClaimTheyAnswer 5 (4,6,5) 4,
                    theyClaimTheyAnswer 6 (3,6,6) 2,
                    theyClaimTheyAnswer 1 (6,6,5) 4,
                    theyClaimTheyAnswer 2 (4,4,7) 6,
                    iClaimTheyAnswer (1,4,9) 4 3
                ]-- iAccuse (4,4,5) True
    
    testCase5 = [   iAm 3,
                    iHold ([4], [2], [1]),
                    iClaimTheyAnswer (1,1,7) 4 1,
                    theyClaimTheyAnswer 5 (5,4,7) 1,
                    theyClaimTheyAnswer 6 (5,2,1) 1,
                    theyClaimTheyAnswer 2 (6,3,9) 5,
                    iClaimTheyAnswer (2,1,2) 4 3,
                    theyClaimTheyAnswer 4 (4,1,6) 5,
                    theyClaimTheyAnswer 5 (2,1,6) 6,
                    theyClaimTheyAnswer 6 (2,1,5) 1,
                    theyClaimTheyAnswer 1 (3,4,3) 2,
                    theyClaimTheyAnswer 2 (6,6,5) 5,
                    iClaimTheyAnswer (3,5,8) 4 3,
                    theyClaimTheyAnswer 4 (4,5,9) 5,
                    theyClaimTheyAnswer 5 (3,5,4) 1,
                    theyClaimTheyAnswer 6 (6,2,4) 1,
                    theyClaimIAnswer 1 (3,2,2) 2,
                    theyClaimTheyAnswer 2 (5,1,7) 1,
                    iClaimTheyAnswer (1,2,2) 4 3,
                    -- iAccuse (3,4,7) False,
                    theyClaimTheyAnswer 4 (4,6,5) 5,
                    theyClaimTheyAnswer 5 (4,4,7) 2,
                    theyClaimTheyAnswer 6 (4,4,5) 2,
                    theyClaimTheyAnswer 1 (3,6,3) 2,
                    theyClaimTheyAnswer 2 (3,3,3) 6,
                    theyAccuse 2 (3,6,7) True
                ]
    
    testCase6 = [   iAm 3,
                    iHold ([1], [3,4], []),
                    theyClaimIAnswer 2 (3,3,3) 2,
                    iClaimTheyAnswer (6,5,1) 6 2,
                    theyClaimIAnswer 6 (2,3,1) 2,
                    theyClaimTheyAnswer 1 (1,4,8) 2,
                    theyClaimTheyAnswer 2 (2,5,6) 4,
                    iClaimTheyAnswer (3,1,4) 4 2,
                    theyClaimTheyAnswer 4 (1,4,2) 5,
                    theyClaimTheyAnswer 5 (5,6,5) 1,
                    theyClaimTheyAnswer 6 (4,1,9) 4,
                    theyClaimIAnswer 1 (1,2,2) 1,
                    theyClaimTheyAnswer 2 (4,6,1) 5,
                    iClaimTheyAnswer (3,6,8) 1 2,
                    theyClaimTheyAnswer 4 (6,4,6) 2,
                    theyClaimTheyAnswer 5 (5,4,8) 2,
                    theyClaimTheyAnswer 6 (5,2,4) 2,
                    theyClaimIAnswer 1 (4,4,7) 2,
                    theyClaimTheyAnswer 2 (2,2,2) 5,
                    iClaimTheyAnswer (3,4,5) 1 3,
                    theyClaimTheyAnswer 4 (3,2,7) 6,
                    theyClaimTheyAnswer 5 (2,2,1) 6,
                    theyClaimTheyAnswer 6 (3,2,5) 1,
                    theyClaimNoAnswer 1 (2,2,5),
                    theyAccuse 2 (2,2,3) True
                ]
