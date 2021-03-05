module Hanoi where

-- Disks of different sizes
type Disk = Int
-- A rod can have several disks on it
type Rod  = [Disk]
-- A Hanoi problem consists of three rods
type Problem = (Rod, Rod, Rod)
-- Identifier for the rods: 0,1,2
type RodID = Int
-- Move the topmost disk from one rod to another
type Move = (RodID, RodID)



initial :: Int -> Problem
initial n = ([1..n],[],[])


validateRod :: Rod -> Bool
validateRod [] = True
validateRod [a] = True
validateRod (a : b : list) = a < b && validateRod (b : list)


validateProblem :: Problem -> Bool
validateProblem (r1, r2, r3) = validateRod r1 && validateRod r2 && validateRod r3


move :: RodID -> RodID -> Problem -> Problem

move 0 1 ((r : r1), r2, r3) = (r1, (r : r2), r3)
move 0 2 ((r : r1), r2, r3) = (r1, r2, (r : r3))

move 1 0 (r1, (r : r2), r3) = ((r : r1), r2, r3)
move 1 2 (r1, (r : r2), r3) = (r1, r2, (r : r3))

move 2 0 (r1, r2, (r : r3)) = ((r : r1), r2, r3)
move 2 1 (r1, r2, (r : r3)) = (r1, (r : r2), r3)


executeMove :: Move -> Problem -> Problem
executeMove (id1, id2) prob
	| id1 == 0 && id2 == 1  = move 0 1 prob 
	| id1 == 0 && id2 == 2  = move 0 2 prob
	| id1 == 1 && id2 == 2  = move 1 2 prob
	| id1 == 1 && id2 == 0  = move 1 0 prob
	| id1 == 2 && id2 == 0  = move 2 0 prob
	| id1 == 2 && id2 == 1  = move 2 1 prob
	

executeMoves :: [Move] -> Problem -> Problem
executeMoves [] prob = prob
executeMoves (m1 : ms) (r1, r2, r3) = executeMoves ms (executeMove m1 (r1, r2, r3))


freeRod :: RodID -> RodID -> RodID
freeRod x y = 3 - x - y


genMoves :: Int -> RodID -> RodID -> [Move] -> [Move]
genMoves 0 _ _ moves = moves
genMoves n honnan hova (m1 : ms) = genMoves (n-1) honnan (freeRod honnan hova) ((honnan,hova) : genMoves (n-1) (freeRod honnan hova) hova (m1 : ms))


solve :: Problem -> [Move]
solve (r1, [], []) = genMoves (length r1) 0 2 []
solve _ = error "Hibás probléma bemenet!" 
