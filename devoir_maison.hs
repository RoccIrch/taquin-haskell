
type TilePos = (Int, Int)
type State = [TilePos]

e :: State
e = [ (2, 2), (1, 1), (3, 2), (3, 1), (2, 3), (2, 1), (3, 3), (1, 3), (1, 2) ]


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

--posEmpty :: State -> TilePos
--posEmpty ((y1,y2):ys) 
--    |whichTileAt (y1,y2) ((y1,y2):ys)  



toString :: State -> String
toString s = row 1 s ++ row 2 s ++ row 3 s ++ "\n"
row :: Int -> State -> String
row n s = " " ++ t 1 ++ " " ++ t 2 ++ " " ++ t 3 ++ "\n"
    where t m = show (whichTileAt (m, n) s)


toStr :: [State] -> String
toStr [] = ""
toStr (x : xs) = (toString x) ++ (toStr xs)


distHamming :: TilePos -> TilePos -> Int 
distHamming (x1,x2) (y1,y2) = if x1 == y1 && x2 == y2 then 0 else 1 
    
distManhattan :: TilePos -> TilePos -> Int 
distManhattan (x1,x2) (y1,y2) 
    | x1 == y1 && x2 == y2 = 0 
    | x1 == y1 = abs (x2 - y2)
    | x2 == y2 = abs (x1 - y1)
    | otherwise = abs (x1-y1) + abs(x2-y2)


s :: State
s = [(2,3), (1,2), (1,1), (3,1), (3,2), (3,3), (2,2), (1,3), (2,1)]

--h1 :: State -> Int 
--h1 ((x1,x2): xs) 
  --  |
   -- |otherwise = distHamming (x1,x2)(1,1) + h1 xs


--h2 :: State -> Int 
--h2 ((x1,x2): xs) 
--    | 
--    |otherwise = distManhattan (x1,x2)(1,1) + h1 xs