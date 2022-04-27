{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

type TilePos = (Int, Int)
type State = [TilePos]

e :: State
e = [ (2, 2), (1, 1), (3, 2), (3, 1), (2, 3), (2, 1), (3, 3), (1, 3), (1, 2) ]

e1 :: State
e1 = [ (2, 1), (1, 1), (2, 2), (3, 1), (3, 2), (3, 3), (2, 3), (1, 3), (1, 2) ]

ef :: State
ef = [(2, 2), (1, 1), (2, 1), (3, 1), (3, 2), (3, 3), (2, 3), (1 ,3), (1, 2)]

whichTileAt :: TilePos -> State -> Int
whichTileAt (x1,x2) ((y1,y2):ys)
    | x1 == y1 && x2 == y2 = 0
    | otherwise = 1 + whichTileAt (x1,x2)  ys


posTile :: Int -> State -> TilePos
posTile x ((y1,y2):ys)
    |x == 0 = (y1,y2)
    |otherwise = posTile (x-1) ys

posEmpty :: State -> TilePos
posEmpty ((y1,y2):ys) = (y1, y2)


toString :: State -> String
toString s = row 1 s ++ row 2 s ++ row 3 s ++ "\n"
row :: Int -> State -> String
row n s = " " ++ t 1 ++ " " ++ t 2 ++ " " ++ t 3 ++ "\n"
    where t m = show (whichTileAt (m, n) s)


toStr :: [State] -> String
toStr = concatMap toString


distHamming :: TilePos -> TilePos -> Int
distHamming (x1,x2) (y1,y2) = abs(x1 -y1) + abs(x2+y2)

distManhattan :: TilePos -> TilePos -> Int
distManhattan (x1,x2) (y1,y2)
    | x1 == y1 && x2 == y2 = 0
    | x1 == y1 = abs (x2 - y2)
    | x2 == y2 = abs (x1 - y1)
    | otherwise = abs (x1-y1) + abs(x2-y2)


s :: State
s = [(2,3), (1,2), (1,1), (3,1), (3,2), (3,3), (2,2), (1,3), (2,1)]

h1 :: State -> Int
h1 (s:ss) = aux (s:ss) 0
      where aux [] _ = 0
            aux (s:ss) n = distManhattan s (posTile n ef) + aux ss (n+1)


h2 :: State -> Int
h2 (s:ss) = aux (s:ss) 0
      where aux [] _ = 0
            aux (s:ss) n = distHamming s (posTile n ef) + aux ss (n+1)


successeurs :: State -> [State]
successeurs s = [swap i s | i <- [1 .. (length s) - 1], valide i s]
    where
        valide i s = distHamming (head s)(s!!i) == 1
        swap i s =[s!!i] ++ take (i-1) (tail s) ++ [head s] ++ drop (i+1) s

test :: State -> Bool
test s = successeurs s == successeursBis s


successeursBis :: State -> [State]
successeursBis (e:ts) = inter [e] ts
    where inter (e:ts) [] = []
          inter (e:ts1) (t:ts2)
            | distManhattan e t == 1 = (t:ts1 ++ e:ts2) : inter (e:ts1++[t]) ts2
            | otherwise = inter (e:ts1++[t]) ts2


bfs :: State -> [State]
bfs s = bfsSolv [s] []

bfsSolv :: [State] -> [State] -> [State]
bfsSolv (s : ss) visited
    | s == ef = reverse (s : visited)
    | otherwise = bfsSolv (add_Bfs (remAlreadyVisited (successeurs s) visited) ss) (s : visited)
        where
            remAlreadyVisited xs ys = [x | x <- xs, not (elem x ys) ]
            add_Bfs xs ys = ys ++ xs


a :: State
a = [(3,2),(1,1),(2,2),(2,1),(3,1),(3,3),(2,3),(1,3),(1,2)]



parent :: State -> [(State,State)] -> State 
parent s ((x, p) : cs)
    | s == x = p
    | otherwise = parent s cs


nil :: State
nil = []


findSolnPath :: State -> [(State,State)] -> State 
findSolnPath s ((x, p) : cs)
    | p == nil = s 
    | otherwise = parent s cs 

--bfsSolv2 :: [(State, State)] -> [(State, State)] -> [State]
--bfsSolv2 [] _ = []
--bfsSolv2 ((s,p):os) ((x, p) : cs)
--   |s == ef = reverse ((x,_) : cs)
--    |otherwise = bfsSolv2 (add_Bfs2 ) ((s,p) : cs)
--        where remAlreadyVisited xs ys = [x | x <- xs, not (elem x ys)]
--        add_Bfs2 xs ys = ys ++ xs