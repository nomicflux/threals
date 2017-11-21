module Threal.Impartial where

import Threal.Base
import Threal.Threals

data ThrealTree = TThunk | TNode [(Threal, ThrealTree, ThrealTree, ThrealTree)]

data ImpartialTree = IThunk | INode [(Threal, ImpartialTree)]

data Winner = C | P | N | U deriving (Show, Eq)
data WinnerTree = WLeaf Winner | WNode [WinnerTree] deriving (Eq)

rotateWinnerBack :: Winner -> Winner
rotateWinnerBack C = N
rotateWinnerBack N = P
rotateWinnerBack P = C
rotateWinnerBack U = U

rotateWinnerTree :: WinnerTree -> WinnerTree
rotateWinnerTree (WLeaf x) = WLeaf (rotateWinnerBack x)
rotateWinnerTree (WNode []) = WNode []
rotateWinnerTree (WNode trees) = WNode (nub $ map rotateWinnerTree trees)

concatWTrees :: WinnerTree -> WinnerTree -> WinnerTree
concatWTrees (WNode []) x = x
concatWTrees x (WNode []) = x
concatWTrees (WNode (x : [])) y@(WLeaf _) = WNode [y, x]
concatWTrees y@(WLeaf _) (WNode (x : [])) = WNode [y, x]
concatWTrees tree1 tree2 = WNode [tree1, tree2]

pruneWTree :: WinnerTree -> WinnerTree
pruneWTree x@(WLeaf _) = x
pruneWTree (WNode (x@(WLeaf _) : [])) = x
pruneWTree y = y

makeTern :: Int -> Int -> [Int]
makeTern num 0   = [num]
makeTern num pow = rem : makeTern (num - rem*base) (pow - 1)
    where base = 3^pow
          rem = num `div` base

ternAdd :: Int -> Int -> Int
ternAdd x y = modSum
    where maxNum = max x y
          maxPow = fst $ head $ dropWhile (\(a,b) -> b < maxNum) $ map (\n -> (n, 3 ^ n)) [0..]
          ternX = makeTern x maxPow
          ternY = makeTern y maxPow
          modAdd acc (a,b) = 3*acc + ((a+b) `mod` 3)
          modSum = foldl modAdd 0 (zip ternX ternY)

makeWLeaf :: Winner -> WinnerTree
makeWLeaf w = WLeaf w

makeWBranch :: [Winner] -> WinnerTree
makeWBranch ws = WNode (map WLeaf ws)

makeWBush :: [[Winner]] -> WinnerTree
makeWBush wss = WNode (map makeWBranch wss)

concatITrees :: ImpartialTree -> ImpartialTree -> ImpartialTree
concatITrees IThunk x = x
concatITrees x IThunk = x
concatITrees (INode as) (INode bs) = INode (as++bs)

singleNode :: Threal -> ThrealTree
singleNode t = TNode [(t, TThunk, TThunk, TThunk)]

multiNode :: [Threal] -> ThrealTree
multiNode ts = TNode $ concatMap (\x -> [(x, TThunk, TThunk, TThunk)]) ts

singleNodeWithLeaves :: Threal -> ThrealTree
singleNodeWithLeaves t@(Threal r g b) = TNode [(t, multiNode r, multiNode g, multiNode b)]

concatTrees :: ThrealTree -> ThrealTree -> ThrealTree
concatTrees TThunk x = x
concatTrees x TThunk = x
concatTrees (TNode a) (TNode b) = TNode (a++b)

treeMap :: ThrealTree -> (ThrealTree -> ThrealTree) -> ThrealTree
treeMap TThunk _ = TThunk
treeMap (TNode []) _ = TNode []
treeMap (TNode ((t, x, y, z) : nxt)) f = concatTrees (TNode [(t, f x, f y, f z)]) (treeMap (TNode nxt) f)

impartial :: [Threal] -> Threal
impartial t = Threal t t t

turnTree :: Threal -> ThrealTree
turnTree = singleNodeWithLeaves

nextTurnInTree :: ThrealTree -> ThrealTree
nextTurnInTree TThunk = TThunk
nextTurnInTree (TNode []) = TNode []
nextTurnInTree (TNode (h@(t, TThunk, TThunk, TThunk) : nxt)) = concatTrees thisTree nextTree
    where thisTree = turnTree t
          nextTree = nextTurnInTree (TNode nxt)
nextTurnInTree (TNode (h@(t, r, g, b) : nxt)) = concatTrees thisTree thatTree
    where thisTree = treeMap (TNode [h]) nextTurnInTree
          thatTree = nextTurnInTree (TNode nxt)

anyThunk :: ThrealTree -> Bool
anyThunk TThunk = True
anyThunk (TNode []) = False
anyThunk (TNode ((_, TThunk, _, _) : _)) = True
anyThunk (TNode ((_, _, TThunk, _) : _)) = True
anyThunk (TNode ((_, _, _, TThunk) : _)) = True
anyThunk (TNode ((_, r, g, b) : nxt)) = any anyThunk [r,g,b,(TNode nxt)]

gameTree t n = head $ drop (n-1) $ iterate nextTurnInTree $ turnTree t

fullGameTree t = head $ dropWhile anyThunk $ iterate nextTurnInTree $ turnTree t

impartializeTree :: ThrealTree -> ImpartialTree
impartializeTree TThunk = IThunk
impartializeTree (TNode []) = INode []
impartializeTree (TNode ((t, r, g, b) : nxt)) = concatITrees thisTree nextTree
    where thisTree = INode [(t, impartializeTree r)]
          nextTree = impartializeTree (TNode nxt)

impartialGame :: Threal -> ImpartialTree
impartialGame = impartializeTree . fullGameTree

calculateWinner :: ImpartialTree -> WinnerTree
calculateWinner IThunk = makeWLeaf U
calculateWinner (INode []) = makeWBranch []
calculateWinner (INode ((n, h) : t))
    | n === tzero = makeWLeaf P
    | otherwise   = allWin
    where thisWin = rotateWinnerTree $ calculateWinner h
          restWin = calculateWinner (INode t)
--          allWin  = concatWTrees thisWin restWin
          allWin = if thisWin == WLeaf P || restWin == WLeaf P then WLeaf P else concatWTrees thisWin restWin

calculateImpartial stacks = timberStack `seq` foldedVal `seq` gameTree `seq` calculateWinner gameTree
    where timberStack = map timber stacks
          foldedVal = foldl (+) tzero timberStack
          gameTree = impartialGame foldedVal

instance Show WinnerTree where
    show (WLeaf x) = show x
    show (WNode xs) = "[" ++ (intercalate ", " (map show xs)) ++ "]"

instance Show ThrealTree where
    show t = showTTree t 1

showTTree TThunk _ = "_"
showTTree (TNode []) _ = ""
showTTree (TNode ((t, r, g, b) : nxt)) depth
    | t === tzero = show t ++ nxtStr nxt
    | otherwise = show t ++ " " ++ arrow ++ indent ++
                  (colorify redColour   $ "\n" ++ indent ++ "Red:   ")   ++ (showTTree r ndepth) ++
                  (colorify greenColour $ "\n" ++ indent ++ "Green: ") ++ (showTTree g ndepth) ++
                  (colorify blueColour  $ "\n" ++ indent ++ "Blue:  ")  ++ (showTTree b ndepth) ++
                  "\n" ++ nxtStr nxt
    where ndepth = succ depth
          indent = take (2 * depth) $ repeat '\t'
          arrow = (take depth $ repeat '-') ++ ">"
          nxtStr [] = ""
          nxtStr s  = tail (tail indent) ++ "        &" ++ showTTree (TNode s) depth

instance Show ImpartialTree where
    show t = showITree t 1

showITree IThunk _ = "_"
showITree (INode []) _ = ""
showITree (INode ((t, r) : nxt)) depth
    | t === tzero = show t ++ nxtStr nxt
    | otherwise = show t ++ " " ++ arrow ++ indent ++
                  "\n" ++ indent ++  (showITree r ndepth) ++
                  "\n" ++ nxtStr nxt
    where ndepth = succ depth
          indent = take depth $ repeat '\t'
          arrow = (take depth $ repeat '-') ++ ">"
          nxtStr [] = ""
          nxtStr s  = tail indent ++ "&" ++ showITree (INode s) depth

compactNode :: (Threal, ThrealTree, ThrealTree, ThrealTree) -> String
compactNode (t,r,g,b)
    | t === tzero = ""
    | otherwise = ('r':(compactTTree r)) ++ ('g':compactTTree g) ++ ('b':compactTTree b)

compactTTree :: ThrealTree -> String
compactTTree TThunk = "_"
compactTTree (TNode []) = ""
compactTTree (TNode nodes)
    | nodeStr == "" = ""
    | otherwise = '[' : nodeStr ++ "]"
    where nodeStr = (concatMap compactNode nodes)
