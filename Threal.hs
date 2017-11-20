module Threal where

import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Control.Applicative( (<$>), (<*>), pure )
import Control.Monad (when, mapM, mapM_)
import Control.Monad.State (State, get, put, runState, evalState, StateT, liftIO, evalStateT)

data Threal = Threal [Threal] [Threal] [Threal]

instance Hashable Threal where
  hashWithSalt salt (Threal reds greens blues) = ((redHash + greenHash + blueHash) * salt) `mod` 34321347
    where
      hasher n arr = n + 3 * (sum $ map ((+ (length arr)) . hashWithSalt (salt + n)) arr)
      redHash = hasher 0 reds
      greenHash = hasher 1 greens
      blueHash = hasher 2 blues

data ThrealTree = TThunk | TNode [(Threal, ThrealTree, ThrealTree, ThrealTree)]

data ImpartialTree = IThunk | INode [(Threal, ImpartialTree)]

data Winner = C | P | N | U deriving (Show, Eq)
data WinnerTree = WLeaf Winner | WNode [WinnerTree] deriving (Eq)

data Comps = Comps { redComp :: (Threal -> Threal -> Bool)
                   , greenComp :: (Threal -> Threal -> Bool)
                   , blueComp :: (Threal -> Threal -> Bool)
                   }

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

tzero :: Threal
tzero = Threal [] [] []

red = Threal [tzero] [] []
green = Threal [] [tzero] []
blue = Threal [] [] [tzero]

negRed = Threal [] [tzero] [tzero]
negGreen = Threal [tzero] [] [tzero]
negBlue = Threal [tzero] [tzero] []
--negRed = negate red
--negGreen = negate green
--negBlue = negate blue

star = Threal [tzero] [tzero] [tzero]
fullStar = Threal [star] [star] [star]

rainbowStar = red + green + blue
negRainbowStar = negRed + negGreen + negBlue

threalNum :: Comps -> Threal -> Integer -> Threal
threalNum c t n = completelyUnique c $ foldl' (\acc n -> threalAdd acc t) tzero [1..n]

redNum :: Integer -> Threal
redNum = threalNum allComp red

greenNum :: Integer -> Threal
greenNum = threalNum allComp green

blueNum :: Integer -> Threal
blueNum = threalNum allComp blue

timber :: Integer -> Threal
timber 0 = tzero
timber 1 = star
timber n = Threal opts opts opts
    where opts = map timber [n-1, n-2 .. 0]

impartial :: [Threal] -> Threal
impartial t = Threal t t t

redStar = Threal [star] [] []
greenStar = Threal [] [star] []
blueStar = Threal [] [] [star]

redArrow = Threal [star] [tzero] [tzero]
greenArrow = Threal [tzero] [star] [tzero]
blueArrow = Threal [tzero] [tzero] [star]
negRedArrow = Threal [tzero] [star] [star]
negGreenArrow = Threal [star] [tzero] [star]
negBlueArrow = Threal [star] [star] [tzero]

allRed = Threal [red] [red] [red]
allGreen = Threal [green] [green] [green]
allBlue = Threal [blue] [blue] [blue]

staleRed = Threal [negRed] [red] [red]
staleGreen = Threal [green] [negGreen] [green]
staleBlue = Threal [blue] [blue] [negBlue]

redSink = Threal [negGreen, negBlue] [negGreen] [negBlue]
greenSink = Threal [negRed] [negRed, negBlue] [negBlue]
blueSink = Threal [negRed] [negGreen] [negRed, negGreen]

redSource = Threal [green, blue] [green] [blue]
greenSource = Threal [red] [red, blue] [blue]
blueSource = Threal [red] [green] [red, green]

notRed = Threal [] [green] [blue]
notGreen = Threal [red] [] [blue]
notBlue = Threal [red] [green] []

swapRG = Threal [green] [red] []
swapGB = Threal [] [blue] [green]
swapBR = Threal [blue] [] [red]

redGoneGreen = Threal [] [green] [green]
redGoneBlue = Threal [] [blue] [blue]
greenGoneRed = Threal [red] [] [red]
greenGoneBlue = Threal [blue] [] [blue]
blueGoneRed = Threal [red] [red] []
blueGoneGreen = Threal [green] [green] []

revenge = Threal [swapGB] [swapBR] [swapRG]
sophiesStar = Threal [notRed] [notGreen] [notBlue]
anticlockwiseStar = Threal [green] [blue] [red]
clockwiseStar = Threal [blue] [red] [green]

-- Num Instance

threalAdd :: Threal -> Threal -> Threal
threalAdd x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = Threal redMapped greenMapped blueMapped
    where redMapped = (map (\z -> threalAdd x z) r2) ++ (map (\z -> threalAdd z y) r1)
          greenMapped = (map (\z -> threalAdd x z) g2) ++ (map (\z -> threalAdd z y) g1)
          blueMapped = (map (\z -> threalAdd x z) b2) ++ (map (\z -> threalAdd z y) b1)

threalStraightNegate :: Threal -> Threal
threalStraightNegate (Threal r g b) = Threal redMapped greenMapped blueMapped
    where redMapped = (map threalStraightNegate g) ++ (map threalStraightNegate b)
          greenMapped = (map threalStraightNegate b) ++ (map threalStraightNegate r)
          blueMapped = (map threalStraightNegate r) ++ (map threalStraightNegate g)

turnClockwise :: Threal -> Threal
turnClockwise (Threal r g b) = Threal bTurned rTurned gTurned
    where bTurned = map turnClockwise b
          rTurned = map turnClockwise r
          gTurned = map turnClockwise g

turnAnticlockwise :: Threal -> Threal
turnAnticlockwise (Threal r g b) = Threal gTurned bTurned rTurned
    where bTurned = map turnAnticlockwise b
          rTurned = map turnAnticlockwise r
          gTurned = map turnAnticlockwise g


instance Num Threal where
  (+) x y = simplify $ threalAdd x y
  negate x = simplify $ threalAdd (turnClockwise x) (turnAnticlockwise x)
  (-) x y = x + (negate y)
  (*) x y = tzero
  abs x = x
  signum x = firstPart + secondPart
      where (rcomp:gcomp:bcomp:arcomp:agcomp:abcomp:[]) = x <|> tzero
            firstPart
                | rcomp && gcomp && bcomp = tzero
                | rcomp && gcomp = negBlue
                | rcomp && bcomp = negGreen
                | gcomp && bcomp = negRed
                | rcomp = red
                | gcomp = green
                | bcomp = blue
                | otherwise = star
            secondPart
                | arcomp && agcomp && abcomp = tzero
                | arcomp && agcomp = blue
                | arcomp && abcomp = green
                | agcomp && abcomp = red
                | arcomp = negRed
                | agcomp = negGreen
                | abcomp = negBlue
                | otherwise = star
  fromInteger n = (redNum n) + (greenNum n) + (blueNum n)

--(*) x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = Threal redMapped greenMapped blueMapped
--    where redMapped = [r1*y + x*r2 - r1*r2, g1*y + x*b2 - g1*b2, b1*y + x*g2 - b1*g2]
--          greenMapped = [g1*y + x*g2 - g1*g2, b1*y + x*r2 - b1*r2, r1*y + x*b2 - r1*b2]
--          blueMapped = [b1*y + x*b2 - b1*b2, r1*y + x*g2 - r1*g2, g1*y + x*r2 - g1*r2]


-- Comparisons

none :: (a -> Bool) -> [a] -> Bool
none f l = and $ (map (not . f)) l

--

fullCompRedGreen :: Threal -> Threal -> Bool
fullCompRedGreen x@(Threal r1 g1 b1) y@(Threal r2 g2 b2)  = r1++b1 `seq` g2++b2 `seq` none (y `fullCompRedGreen`) (r1++b1) && none (`fullCompRedGreen` x) (g2++b2)

fullCompGreenBlue :: Threal -> Threal -> Bool
fullCompGreenBlue x@(Threal r1 g1 b1) y@(Threal r2 g2 b2)  = g1++r1 `seq` b2++r2 `seq` none (y `fullCompGreenBlue`) (g1++r1) && none (`fullCompGreenBlue` x) (b2++r2)

fullCompBlueRed :: Threal -> Threal -> Bool
fullCompBlueRed x@(Threal r1 g1 b1) y@(Threal r2 g2 b2)  = b1++g1 `seq` r2++g2 `seq` none (y `fullCompBlueRed`) (b1++g1) && none (`fullCompBlueRed` x) (r2++g2)

fullEqRedGreen x y = fullCompRedGreen x y && fullCompRedGreen y x
fullEqGreenBlue x y = fullCompGreenBlue x y && fullCompGreenBlue y x
fullEqBlueRed x y = fullCompBlueRed x y && fullCompBlueRed y x

fullCompRedder :: Threal -> Threal -> Bool
fullCompRedder x y = y `fullCompRedGreen` x && x `fullCompBlueRed` y

fullCompGreener :: Threal -> Threal -> Bool
fullCompGreener x y = y `fullCompGreenBlue` x && x `fullCompRedGreen` y

fullCompBluer :: Threal -> Threal -> Bool
fullCompBluer x y = y `fullCompBlueRed` x && x `fullCompGreenBlue` y

--

compRedGreen :: Threal -> Threal -> Bool
compRedGreen x@(Threal r1 g1 _) y@(Threal r2 g2 _)  = none (y `compRedGreen`) r1 && none (`compRedGreen` x) g2

compGreenBlue :: Threal -> Threal -> Bool
compGreenBlue x@(Threal _ g1 b1) y@(Threal _ g2 b2)  = none (y `compGreenBlue`) g1 && none (`compGreenBlue` x) b2

compBlueRed :: Threal -> Threal -> Bool
compBlueRed x@(Threal r1 _ b1) y@(Threal r2 _ b2)  = none (y `compBlueRed`) b1 && none (`compBlueRed` x) r2

compRedder :: Threal -> Threal -> Bool
compRedder x y = y `compRedGreen` x && x `compBlueRed` y

compGreener :: Threal -> Threal -> Bool
compGreener x y = y `compGreenBlue` x && x `compRedGreen` y

compBluer :: Threal -> Threal -> Bool
compBluer x y = y `compBlueRed` x && x `compGreenBlue` y

eqRedGreen x y = compRedGreen x y && compRedGreen y x
eqGreenBlue x y = compGreenBlue x y && compGreenBlue y x
eqBlueRed x y = compBlueRed x y && compBlueRed y x

cfRedGreen x y = not $ compRedGreen x y || compRedGreen y x
cfGreenBlue x y = not $ compGreenBlue x y || compGreenBlue y x
cfBlueRed x y = not $ compBlueRed x y || compBlueRed y x

notCompRedGreen x y = eqRedGreen x y || cfRedGreen x y
notCompGreenBlue x y = eqGreenBlue x y || cfGreenBlue x y
notCompBlueRed x y = eqBlueRed x y || cfBlueRed x y

--

allRedderThan :: Threal -> Threal -> Bool
allRedderThan x@(Threal r1 g1 b1) y@(Threal r2 g2 b2)  = none (\z -> y `allRedderThan` z && z `allGreenerThan` y && z `allBluerThan` y) r1 &&
                                                         none (\z -> z `allRedderThan` x && x `allGreenerThan` z && x `allBluerThan` z) (g2++b2)
allGreenerThan :: Threal -> Threal -> Bool
allGreenerThan x@(Threal r1 g1 b1) y@(Threal r2 g2 b2)  = none (\z -> y `allGreenerThan` z && z `allBluerThan` y && z `allRedderThan` y) g1 &&
                                                          none (\z -> z `allGreenerThan` x && x `allBluerThan` z && x `allRedderThan` z) (b2++r2)
allBluerThan :: Threal -> Threal -> Bool
allBluerThan x@(Threal r1 g1 b1) y@(Threal r2 g2 b2)  = none (\z -> y `allBluerThan` z && z `allRedderThan` y && z `allGreenerThan` y) b1 &&
                                                        none (\z -> z `allBluerThan` x && x `allRedderThan` z && x `allGreenerThan` z) (r2++g2)
--

anyRedderThan :: Threal -> Threal -> Bool
anyRedderThan x@(Threal r1 g1 b1) y@(Threal r2 g2 b2)  = none (\z -> y `anyRedderThan` z || z `anyGreenerThan` y || z `anyBluerThan` y) r1 &&
                                                         none (\z -> z `anyRedderThan` x || x `anyGreenerThan` z || x `anyBluerThan` z) (g2++b2)
anyGreenerThan :: Threal -> Threal -> Bool
anyGreenerThan x@(Threal r1 g1 b1) y@(Threal r2 g2 b2)  = none (\z -> y `anyGreenerThan` z || z `anyBluerThan` y || z `anyRedderThan` y) g1 &&
                                                          none (\z -> z `anyGreenerThan` x || x `anyBluerThan` z || x `anyRedderThan` z) (b2++r2)
anyBluerThan :: Threal -> Threal -> Bool
anyBluerThan x@(Threal r1 g1 b1) y@(Threal r2 g2 b2)  = none (\z -> y `anyBluerThan` z || z `anyRedderThan` y || z `anyGreenerThan` y) b1 &&
                                                        none (\z -> z `anyBluerThan` x || x `anyRedderThan` z || x `anyGreenerThan` z) (r2++g2)



revFullyRedderThan :: Threal -> Threal -> Bool
revFullyRedderThan x@(Threal r1 g1 b1) y@(Threal r2 g2 b2)  = none (\z -> z `revFullyGreenerThan` y || z `revFullyBluerThan` y) r1 && none (\z -> z `revFullyRedderThan` x) (g2++b2)

revFullyGreenerThan :: Threal -> Threal -> Bool
revFullyGreenerThan x@(Threal r1 g1 b1) y@(Threal r2 g2 b2)  = none (\z -> z `revFullyBluerThan` y || z `revFullyRedderThan` y) g1 && none (\z -> z `revFullyGreenerThan` x) (b2++r2)

revFullyBluerThan :: Threal -> Threal -> Bool
revFullyBluerThan x@(Threal r1 g1 b1) y@(Threal r2 g2 b2)  = none (\z -> z `revFullyRedderThan` y || z `revFullyGreenerThan` y) b1 && none (\z -> z `revFullyBluerThan` x) (r2++g2)



revRedderThanCross :: Threal -> Threal -> Bool
revRedderThanCross x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = none (\z -> z `revGreenerThanCross` y || z `revBluerThanCross` y) r1 && none (`revRedderThanCross` x) (g2++b2)

revGreenerThanCross :: Threal -> Threal -> Bool
revGreenerThanCross x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = none (\z -> z `revBluerThanCross` y || z `revRedderThanCross` y) g1 && none (`revGreenerThanCross` x) (b2++r2)

revBluerThanCross :: Threal -> Threal -> Bool
revBluerThanCross x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = none (\z -> z `revRedderThanCross` y || z `revGreenerThanCross` y) b1 && none (`revBluerThanCross` x) (r2++g2)

revRedderThanSame :: Threal -> Threal -> Bool
revRedderThanSame x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = none (\z -> z `revGreenerThanSame` y || z `revBluerThanSame` y) r1 && none (\z -> x `revGreenerThanSame` z || x `revBluerThanSame` z) (g2++b2)

revGreenerThanSame :: Threal -> Threal -> Bool
revGreenerThanSame x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = none (\z -> z `revBluerThanSame` y || z `revRedderThanSame` y) g1 && none (\z -> x `revBluerThanSame` z || x `revRedderThanSame` z) (b2++r2)

revBluerThanSame :: Threal -> Threal -> Bool
revBluerThanSame x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = none (\z -> z `revRedderThanSame` y || z `revGreenerThanSame` y) b1 && none (\z -> x `revRedderThanSame` z || x `revGreenerThanSame` z) (r2++g2)


fullyRedderThan :: Threal -> Threal -> Bool
fullyRedderThan x@(Threal r1 g1 b1) y@(Threal r2 g2 b2)  = none (y `fullyRedderThan`) r1 && none (\z -> x `fullyGreenerThan` z && x `fullyBluerThan` z) (g2++b2)

fullyGreenerThan :: Threal -> Threal -> Bool
fullyGreenerThan x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = none (y `fullyGreenerThan`) g1 && none (\z -> x `fullyBluerThan` z && x `fullyRedderThan` z) (b2++r2)

fullyBluerThan :: Threal -> Threal -> Bool
fullyBluerThan x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = none (y `fullyBluerThan`) b1 && none (\z -> x `fullyRedderThan` z && x `fullyGreenerThan` z) (r2++g2)

redderThanCross :: Threal -> Threal -> Bool
redderThanCross x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = none (y `redderThanCross`) r1 && none (x `greenerThanCross`) g2 && none (x `bluerThanCross`) b2

greenerThanCross :: Threal -> Threal -> Bool
greenerThanCross x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = none (y `greenerThanCross`) g1 && none (x `bluerThanCross`) b2 && none (x `redderThanCross`) r2

bluerThanCross :: Threal -> Threal -> Bool
bluerThanCross x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = none (y `bluerThanCross`) b1 && none (x `redderThanCross`) r2 && none (x `greenerThanCross`) g2

redderThanSame :: Threal -> Threal -> Bool
redderThanSame x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = none (y `redderThanSame`) r1 && none (`redderThanSame` x) g2 && none (`redderThanSame` x) b2

greenerThanSame :: Threal -> Threal -> Bool
greenerThanSame x@(Threal r1 g1 b1)  y@(Threal r2 g2 b2) = none (y `greenerThanSame`) g1 && none (`greenerThanSame` x) b2 && none (`greenerThanSame` x) r2

bluerThanSame :: Threal -> Threal -> Bool
bluerThanSame x@(Threal r1 g1 b1)  y@(Threal r2 g2 b2) = none (y `bluerThanSame`) b1 && none (`bluerThanSame` x) r2 && none (`bluerThanSame` x) g2

redderThan :: Threal -> Threal -> Bool
redderThan = compRedder

greenerThan :: Threal -> Threal -> Bool
greenerThan = compGreener

bluerThan :: Threal -> Threal -> Bool
bluerThan = compBluer

(>^) = redderThan
(>-) = greenerThan
(>\) = bluerThan

equalSets :: [Threal] -> [Threal] -> Bool
equalSets x y = x \\ y == [] && y \\ x == []

(=|=) :: Threal -> Threal -> Bool
(=|=) x y = redderThan x y && greenerThan x y && bluerThan x y

instance Eq Threal where
  (==) = (=|=)

exactIn [] y = False
exactIn (x:xs) y
    | x === y = True
    | otherwise = exactIn xs y

exactDiff x [] = x
exactDiff [] _ = []
exactDiff (x:xs) y
    | exactIn y x = exactDiff xs y
    | otherwise = x : exactDiff xs y

exactNub [] = []
exactNub (x:xs)
    | exactIn xs x = exactNub xs
    | otherwise = x : exactNub xs

(===) (Threal [] [] []) (Threal [] [] []) = True
(===) (Threal [] [] []) _ = False
(===) _ (Threal [] [] []) = False
(===) (Threal r1 g1 b1) (Threal r2 g2 b2) =  lengthsEq && redsEq && greensEq && bluesEq
    where nubR1 = exactNub r1
          nubR2 = exactNub r2
          nubG1 = exactNub g1
          nubG2 = exactNub g2
          nubB1 = exactNub b1
          nubB2 = exactNub b2
          lengthsEq = length nubR1 == length nubR2 && length nubG1 == length nubG2 && length nubB1 == length nubB2
          redsEq = null $ exactDiff nubR1 nubR2
          greensEq = null $ exactDiff nubG1 nubG2
          bluesEq = null $ exactDiff nubB1 nubB2

(|||) :: Threal -> Threal -> Bool
(|||) x y = (not $ redderThan x y) && (not $ greenerThan x y) && (not $ bluerThan x y)

fullComp :: Threal -> Threal -> IO ()
fullComp x y = putStrLn $
               "Cross: \n" ++
               show x ++ " >Red> "++ show y ++": " ++ (show $ redderThanCross y x) ++ "\t\t" ++ show x ++" <Red< "++ show y ++": " ++ (show $ redderThanCross x y) ++ "\n" ++
               show x ++ " >Green> "++ show y ++": " ++ (show $ greenerThanCross y x) ++ "\t\t"++ show x ++" <Green< "++ show y++ ": " ++ (show $ greenerThanCross x y) ++ "\n" ++
               show x ++" >Blue> "++ show y ++": " ++ (show $ bluerThanCross y x) ++ "\t\t"++ show x++" <Blue< "++ show y ++": " ++ (show $ bluerThanCross x y) ++  "\n" ++
               --"==: " ++ (show $ x =|= y) ++ "\t\t||: " ++ (show $ x ||| y) ++
               "Same: \n" ++
               show x ++ " >Red> "++ show y ++": " ++ (show $ redderThanSame y x) ++ "\t\t" ++ show x ++" <Red< "++ show y ++": " ++ (show $ redderThanSame x y) ++ "\n" ++
               show x ++ " >Green> "++ show y ++": " ++ (show $ greenerThanSame y x) ++ "\t\t"++ show x ++" <Green< "++ show y++ ": " ++ (show $ greenerThanSame x y) ++ "\n" ++
               show x ++" >Blue> "++ show y ++": " ++ (show $ bluerThanSame y x) ++ "\t\t"++ show x++" <Blue< "++ show y ++": " ++ (show $ bluerThanSame x y) ++  "\n" ++
               "Full: \n" ++
               show x ++ " >Red> "++ show y ++": " ++ (show $ fullyRedderThan y x) ++ "\t\t" ++ show x ++" <Red< "++ show y ++": " ++ (show $ fullyRedderThan x y) ++ "\n" ++
               show x ++ " >Green> "++ show y ++": " ++ (show $ fullyGreenerThan y x) ++ "\t\t"++ show x ++" <Green< "++ show y++ ": " ++ (show $ fullyGreenerThan x y) ++ "\n" ++
               show x ++" >Blue> "++ show y ++": " ++ (show $ fullyBluerThan y x) ++ "\t\t"++ show x++" <Blue< "++ show y ++": " ++ (show $ fullyBluerThan x y) ++  "\n"++
               "Rev Cross: \n" ++
               show x ++ " >Red> "++ show y ++": " ++ (show $ revRedderThanCross y x) ++ "\t\t" ++ show x ++" <Red< "++ show y ++": " ++ (show $ revRedderThanCross x y) ++ "\n" ++
               show x ++ " >Green> "++ show y ++": " ++ (show $ revGreenerThanCross y x) ++ "\t\t"++ show x ++" <Green< "++ show y++ ": " ++ (show $ revGreenerThanCross x y) ++ "\n" ++
               show x ++" >Blue> "++ show y ++": " ++ (show $ revBluerThanCross y x) ++ "\t\t"++ show x++" <Blue< "++ show y ++": " ++ (show $ revBluerThanCross x y) ++  "\n" ++
               "Rev Same: \n" ++
               show x ++ " >Red> "++ show y ++": " ++ (show $ revRedderThanSame y x) ++ "\t\t" ++ show x ++" <Red< "++ show y ++": " ++ (show $ revRedderThanSame x y) ++ "\n" ++
               show x ++ " >Green> "++ show y ++": " ++ (show $ revGreenerThanSame y x) ++ "\t\t"++ show x ++" <Green< "++ show y++ ": " ++ (show $ revGreenerThanSame x y) ++ "\n" ++
               show x ++" >Blue> "++ show y ++": " ++ (show $ revBluerThanSame y x) ++ "\t\t"++ show x++" <Blue< "++ show y ++": " ++ (show $ revBluerThanSame x y) ++  "\n" ++
               "Rev Full: \n" ++
               show x ++ " >Red> "++ show y ++": " ++ (show $ revFullyRedderThan y x) ++ "\t\t" ++ show x ++" <Red< "++ show y ++": " ++ (show $ revFullyRedderThan x y) ++ "\n" ++
               show x ++ " >Green> "++ show y ++": " ++ (show $ revFullyGreenerThan y x) ++ "\t\t"++ show x ++" <Green< "++ show y++ ": " ++ (show $ revFullyGreenerThan x y) ++ "\n" ++
               show x ++" >Blue> "++ show y ++": " ++ (show $ revFullyBluerThan y x) ++ "\t\t"++ show x++" <Blue< "++ show y ++": " ++ (show $ revFullyBluerThan x y) ++  "\n"


(<>) :: Threal -> Threal -> IO ()
(<>) = fullComp

signature :: Threal -> Threal -> [Bool]
signature x y = [x `redderThan` y, x `greenerThan` y, x `bluerThan` y, y `redderThan` x, y `greenerThan` x, y `bluerThan` x]

(<|>) = signature

compSig x y = [[compRedGreen x y, compGreenBlue x y, compBlueRed x y, compRedGreen y x, compGreenBlue y x, compBlueRed y x],
               [fullCompRedGreen x y, fullCompGreenBlue x y, fullCompBlueRed x y, fullCompRedGreen y x, fullCompGreenBlue y x, fullCompBlueRed y x]]

-- Access

redPart :: Threal -> [Threal]
redPart (Threal r _ _) = r

greenPart :: Threal -> [Threal]
greenPart (Threal _ g _) = g

bluePart :: Threal -> [Threal]
bluePart (Threal _ _ b) = b

replaceRed :: Threal -> [Threal] -> Threal
replaceRed (Threal _ g b) r = Threal r g b

replaceGreen :: Threal -> [Threal] -> Threal
replaceGreen (Threal r _ b) g = Threal r g b

replaceBlue :: Threal -> [Threal] -> Threal
replaceBlue (Threal r g _) b = Threal r g b

nextTurn :: Threal -> [Threal]
nextTurn (Threal r g b) = nub [Threal x y z | x <- redParts, y <- greenParts, z <- blueParts]
    where greenParts = map greenPart r
          blueParts = map bluePart g
          redParts = map redPart b

nextTurns :: [Threal] -> [Threal]
nextTurns = nub . (concatMap nextTurn )

fastForward :: Threal -> Int -> [[Threal]]
fastForward t n = take n $ iterate nextTurns [t]

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

-- Impartial Games

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

-- Simplifications

nubComps :: Comps -> [Threal] -> [Threal]
nubComps c ts = nubBy eqC ts
  where eqC t s = (redComp c) t s && (greenComp c) t s && (blueComp c) t s

uniqueFields :: Comps -> Threal -> Threal
uniqueFields c (Threal r g b) = Threal (nubC r) (nubC g) (nubC b)
  where nubC = nubComps c

completelyUnique :: Comps -> Threal -> Threal
completelyUnique c (Threal r g b) = Threal (nubC (map cuc r)) (nubC (map cuc g)) (nubC (map cuc b))
  where cuc = completelyUnique c
        nubC = nubComps c

-- dominate :: Threal -> Threal
-- dominate x = Threal redOpts greenOpts blueOpts
--   where (Threal uniqueR uniqueG uniqueB) = uniqueFields x
--         dR = map dominate uniqueR
--         dB = map dominate uniqueB
--         dG = map dominate uniqueG
--         noneLess comp l x = none (\z -> comp z x) (l \\ [x])
--         redOpts = dR `seq` filter (noneLess redderThan dR) dR
--         greenOpts = dG `seq` filter (noneLess greenerThan dG) dG
--         blueOpts = dB `seq` filter (noneLess bluerThan dB) dB

dominate :: Threal -> Threal
dominate x = evalState (dominateState relComp x) M.empty

dominateC :: Comps -> Threal -> Threal
dominateC c x = evalState (dominateState c x) M.empty

dominateState :: Monad m => Comps -> Threal -> StateT (HashMap Threal Threal) m Threal
dominateState c x = do
  cache <- get
  case M.lookup x cache of
    Just y -> return y
    Nothing -> do
      let (Threal uniqueR uniqueG uniqueB) = uniqueFields c x
          (dR, redState) = runState (traverse dsc uniqueR) cache
          (dG, greenState) = runState (traverse dsc uniqueG) redState
          (dB, blueState) = runState (traverse dsc uniqueB) greenState
      let redOpts = dR `seq` filter (noneLess (redComp c) dR) dR
          greenOpts = dG `seq` filter (noneLess (greenComp c) dG) dG
          blueOpts = dB `seq` filter (noneLess (blueComp c) dB) dB
      let res = Threal redOpts greenOpts blueOpts
          updatedCache = M.insert x res blueState
      put updatedCache
      return res
  where noneLess comp l a = none (a `comp`) (l \\ [a])
        dsc = dominateState c

--nand = not . and
--nor = not . or

compReversible :: (Threal -> Threal -> Bool) -> (Threal -> [Threal]) -> (Threal -> [Threal]) -> (Threal -> [Threal] -> Threal) -> (Threal -> [Threal] -> Threal) -> Threal -> Threal
compReversible comp fstGet sndGet fstSet sndSet x = sndSet (fstSet x mapped1) mapped2
    where compOpts1 = map (\z -> (z, sndGet z)) $ fstGet x
          mapped1 = concatMap (\(orig, revved) -> if any (`comp` x) revved then revved else [orig]) compOpts1
          compOpts2 = map (\z -> (z, fstGet z)) $ sndGet x
          mapped2 = concatMap (\(orig, revved) -> if any (x `comp`) revved then revved else [orig]) compOpts2

rgReversible = compReversible compRedGreen redPart greenPart replaceRed replaceGreen
gbReversible = compReversible compGreenBlue greenPart bluePart replaceGreen replaceBlue
brReversible = compReversible compBlueRed bluePart redPart replaceBlue replaceRed

rgbReversible = brReversible . gbReversible . rgReversible

reversible :: Threal -> Threal
reversible x = Threal redOpts greenOpts blueOpts
  where ux@(Threal uniqueR uniqueG uniqueB) = uniqueFields allComp x
        rR = map reversible uniqueR
        rG = map reversible uniqueG
        rB = map reversible uniqueB
        redComp r@(Threal rr rg rb)
            | not $ null gbBetter = concatMap redPart gbBetter
            | otherwise = [r]
            where gbBetter = filter (\z -> z `compGreener` x && z `compBluer` x) (rg++rb)
        greenComp g@(Threal gr gg gb)
            | not $ null brBetter = concatMap greenPart brBetter
            | otherwise = [g]
            where brBetter = filter (\z -> z `compBluer` x && z `compRedder` x) (gb++gr)
        blueComp b@(Threal br bg bb)
            | not $ null rgBetter = concatMap bluePart rgBetter
            | otherwise = [b]
            where rgBetter = filter (\z -> z `compRedder` x && z `compGreener` x) (br++bg)
        redOpts = rR `seq` concatMap redComp rR
        greenOpts = rG `seq` concatMap greenComp rG
        blueOpts = rB `seq` concatMap blueComp rB

reversibleC :: Comps -> Threal -> Threal
reversibleC c x = evalState (reversibleState c x) M.empty

reversibleState :: Monad m => Comps -> Threal -> StateT (HashMap Threal Threal) m Threal
reversibleState c x = do
  cache <- get
  case M.lookup x cache of
    Just y -> return y
    Nothing -> do
      let
        (Threal uniqueR uniqueG uniqueB) = uniqueFields c x
        (rR, redState) = runState (traverse rsc uniqueR) cache
        (rG, greenState) = runState (traverse rsc uniqueG) redState
        (rB, blueState) = runState (traverse rsc uniqueB) greenState
        rComp r@(Threal rr rg rb)
            | not $ null gbBetter = concatMap redPart gbBetter
            | otherwise = [r]
            where
              --gbBetter = filter ((redComp c) x) (rg++rb)
              gbBetter = filter (\z -> greenComp c z x && blueComp c z x && redComp c x z) (rg++rb)
        gComp g@(Threal gr gg gb)
            | not $ null brBetter = concatMap greenPart brBetter
            | otherwise = [g]
            where
              -- brBetter = filter ((greenComp c) x) (gb++gr)
              brBetter = filter (\z -> blueComp c z x && redComp c z x && greenComp c x z) (gb++gr)
        bComp b@(Threal br bg bb)
            | not $ null rgBetter = concatMap bluePart rgBetter
            | otherwise = [b]
            where
              --rgBetter = filter ((blueComp c) x) (br++bg)
              rgBetter = filter (\z -> redComp c z x && greenComp c z x && blueComp c x z) (br++bg)
        redOpts = rR `seq` concatMap rComp rR
        greenOpts = rG `seq` concatMap gComp rG
        blueOpts = rB `seq` concatMap bComp rB
        res = Threal redOpts greenOpts blueOpts
        updatedCache = M.insert x res blueState
      put updatedCache
      return res
  where rsc = reversibleState c

        --rtx z = z `redderThan` x
        --gtx z = z `greenerThan` x
        --btx z = z `bluerThan` x
        --redOpts = rR `seq` filter (\r -> nor (map (\z -> x `redderThan` z && z `greenerThan` x && z `bluerThan` x) (nub $ greenPart r ++ bluePart r)) ) rR
        --greenOpts = rG `seq` filter (\g -> nor (map (\z -> x `greenerThan` z && z `bluerThan` x && z `redderThan` x) (nub $ bluePart g ++ redPart g)) ) rG
        --blueOpts = rB `seq` filter (\b -> nor (map (\z -> x `bluerThan` z && z `redderThan` x && z `greenerThan` x) (nub $ redPart b ++ greenPart b)) ) rB
        -- redOpts = rR `seq` filter (\r -> (nor $ map (\z -> z `fullCompRedder` ux && z `eqGreenBlue` ux) $ rrest r)) rR
        --     where rrest r = greenPart r ++ bluePart r
        -- greenOpts = rG `seq` filter (\g -> (nor $ map (\z -> z `fullCompGreener` ux && z `eqBlueRed` ux) $ grest g)) rG
        --     where grest g = bluePart g ++ redPart g
        -- blueOpts = rB `seq` filter (\b -> (nor $ map (\z -> z `fullCompBluer` ux && z `eqRedGreen` ux) $ brest b)) rB
        --     where brest b = redPart b ++ greenPart b

simplify :: Threal -> Threal
simplify x = x `seq` completelyUnique allComp x

simplifyC :: Comps -> Threal -> Threal
simplifyC c x = completelyUnique c x

addC :: Comps -> Threal -> Threal -> Threal
addC c x y = simplifyC c $ threalAdd x y

  -- Show Instance

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

instance Show Threal where
  show = showThreal

noColour      = "\x1b[;0m"
redColour     = "\x1b[;31m"
greenColour   = "\x1b[;32m"
blueColour    = "\x1b[;34m"
magentaColour = "\x1b[;35m"
yellowColour  = "\x1b[;33m"
cyanColour    = "\x1b[;36m"

colorify colour s = colour ++ s ++ noColour

inspect :: Threal -> IO ()
inspect = putStrLn . inspectThreal

inspectThreal :: Threal -> String
inspectThreal x = (colorify noColour "{ ") ++ shownR ++ (colorify yellowColour " \\ ") ++ shownG ++ (colorify cyanColour " / ") ++ shownB ++ (colorify noColour " }")
  where shownR = concatMap (colorify redColour . show) (redPart x)
        shownG = concatMap (colorify greenColour . show) (greenPart x)
        shownB = concatMap (colorify blueColour .  show) (bluePart x)

showThreal :: Threal -> String
showThreal (Threal [] [] []) = " 0 "
showThreal (Threal [(Threal [] [] [])] [] []) = " r "
showThreal (Threal [] [(Threal [] [] [])] []) = " g "
showThreal (Threal [] [] [(Threal [] [] [])]) = " b "
showThreal (Threal [] [(Threal [] [] [])] [(Threal [] [] [])]) = " ~R~ "
showThreal (Threal [(Threal [] [] [])] [] [(Threal [] [] [])]) = " ~G~ "
showThreal (Threal [(Threal [] [] [])] [(Threal [] [] [])] []) = " ~B~ "
showThreal (Threal [(Threal [] [] [])] [(Threal [] [] [])] [(Threal [] [] [])]) = " * "

showThreal (Threal [Threal [(Threal [] [] [])] [(Threal [] [] [])] [(Threal [] [] [])]] [(Threal [] [] [])] [(Threal [] [] [])]) = " ^r^ "
showThreal (Threal [(Threal [] [] [])] [Threal [(Threal [] [] [])] [(Threal [] [] [])] [(Threal [] [] [])]] [(Threal [] [] [])]) = " ^g^ "
showThreal (Threal [(Threal [] [] [])] [(Threal [] [] [])] [Threal [(Threal [] [] [])] [(Threal [] [] [])] [(Threal [] [] [])]]) = " ^b^ "

showThreal (Threal [(Threal [] [] [])] [Threal [(Threal [] [] [])] [(Threal [] [] [])] [(Threal [] [] [])]] [Threal [(Threal [] [] [])] [(Threal [] [] [])] [(Threal [] [] [])]]) = " ^~R~^ "
showThreal (Threal [Threal [(Threal [] [] [])] [(Threal [] [] [])] [(Threal [] [] [])]] [(Threal [] [] [])] [Threal [(Threal [] [] [])] [(Threal [] [] [])] [(Threal [] [] [])]]) = " ^~G~^ "
showThreal (Threal [Threal [(Threal [] [] [])] [(Threal [] [] [])] [(Threal [] [] [])]] [Threal [(Threal [] [] [])] [(Threal [] [] [])] [(Threal [] [] [])]] [(Threal [] [] [])]) = " ^~B~^ "

showThreal (Threal [Threal [Threal [] [] []] [] []] [] []) = " 2r "
showThreal (Threal [] [Threal [] [Threal [] [] []] []] []) = " 2g "
showThreal (Threal [] [] [Threal [] [] [Threal [] [] []]]) = " 2b "

showThreal (Threal [Threal [Threal [Threal [] [] []] [] []] [] []] [] []) = " 3r "
showThreal (Threal [] [Threal [] [Threal [] [Threal [] [] []] []] []] []) = " 3g "
showThreal (Threal [] [] [Threal [] [] [Threal [] [] [Threal [] [] []]]]) = " 3b "

showThreal (Threal [] [Threal [] [(Threal [] [] [])] []] [Threal [] [] [(Threal [] [] [])]]) = " -R "
showThreal (Threal [Threal [(Threal [] [] [])] [] []] [] [Threal [] [] [(Threal [] [] [])]]) = " -G "
showThreal (Threal [Threal [(Threal [] [] [])] [] []] [Threal [] [(Threal [] [] [])] []] []) = " -B "

showThreal (Threal [Threal [] [Threal [] [] [(Threal [] [] [])]] [Threal [] [(Threal [] [] [])] []]] [Threal [Threal [] [] [(Threal [] [] [])]] [] [Threal [(Threal [] [] [])] [] []]] [Threal [Threal [] [(Threal [] [] [])] []] [Threal [(Threal [] [] [])] [] []] []]) = " r<>g<>b "

showThreal (Threal [Threal [(Threal [] [] [])] [] []] [Threal [(Threal [] [] [])] [] []] [Threal [(Threal [] [] [])] [] []]) = " r* "
showThreal (Threal [Threal [] [(Threal [] [] [])] []] [Threal [] [(Threal [] [] [])] []] [Threal [] [(Threal [] [] [])] []]) = " g* "
showThreal (Threal [Threal [] [] [(Threal [] [] [])]] [Threal [] [] [(Threal [] [] [])]] [Threal [] [] [(Threal [] [] [])]]) = " b* "

showThreal (Threal [] [Threal [] [(Threal [] [] [])] []] [Threal [] [(Threal [] [] [])] []]) = " g~R "
showThreal (Threal [] [Threal [] [] [(Threal [] [] [])]] [Threal [] [] [(Threal [] [] [])]]) = " b~R "
showThreal (Threal [Threal [(Threal [] [] [])] [] []] [] [Threal [(Threal [] [] [])] [] []]) = " r~G "
showThreal (Threal [Threal [] [] [(Threal [] [] [])]] [] [Threal [] [] [(Threal [] [] [])]]) = " b~G "
showThreal (Threal [Threal [(Threal [] [] [])] [] []] [Threal [(Threal [] [] [])] [] []] []) = " r~B "
showThreal (Threal [Threal [] [(Threal [] [] [])] []] [Threal [] [(Threal [] [] [])] []] []) = " g~B "

showThreal (Threal [Threal [] [(Threal [] [] [])] []] [Threal [(Threal [] [] [])] [] []] []) = " r<>g "
showThreal (Threal [Threal [] [] [(Threal [] [] [])]] [] [Threal [(Threal [] [] [])] [] []]) = " b<>r "
showThreal (Threal [] [Threal [] [] [(Threal [] [] [])]] [Threal [] [(Threal [] [] [])] []]) = " g<>b "

showThreal (Threal [Threal [] [(Threal [] [] [])] [(Threal [] [] [])]] [Threal [(Threal [] [] [])] [] []] [Threal [(Threal [] [] [])] [] []]) = " #R# "
showThreal (Threal [Threal [] [(Threal [] [] [])] []] [Threal [(Threal [] [] [])] [] [(Threal [] [] [])]] [Threal [] [(Threal [] [] [])] []]) = " #G# "
showThreal (Threal [Threal [] [] [(Threal [] [] [])]] [Threal [] [] [(Threal [] [] [])]] [Threal [(Threal [] [] [])] [(Threal [] [] [])] []]) = " #B# "

showThreal (Threal [Threal [] [(Threal [] [] [])] [], Threal [] [] [(Threal [] [] [])]] [Threal [] [(Threal [] [] [])] []] [Threal [] [] [(Threal [] [] [])]]) = " R! "
showThreal (Threal [Threal [] [] [(Threal [] [] [])], Threal [] [(Threal [] [] [])] []] [Threal [] [(Threal [] [] [])] []] [Threal [] [] [(Threal [] [] [])]]) = " R! "
showThreal (Threal [Threal [(Threal [] [] [])] [] []] [Threal [(Threal [] [] [])] [] [], Threal [] [] [(Threal [] [] [])]] [Threal [] [] [(Threal [] [] [])]]) = " G! "
showThreal (Threal [Threal [(Threal [] [] [])] [] []] [Threal [] [] [(Threal [] [] [])], Threal [(Threal [] [] [])] [] []] [Threal [] [] [(Threal [] [] [])]]) = " G! "
showThreal (Threal [Threal [(Threal [] [] [])] [] []] [Threal [] [(Threal [] [] [])] []] [Threal [(Threal [] [] [])] [] [], Threal [] [(Threal [] [] [])] []]) = " B! "
showThreal (Threal [Threal [(Threal [] [] [])] [] []] [Threal [] [(Threal [] [] [])] []] [Threal [] [(Threal [] [] [])] [], Threal [(Threal [] [] [])] [] []]) = " B! "

showThreal (Threal [Threal [(Threal [] [] [])] [] [(Threal [] [] [])], Threal [(Threal [] [] [])] [(Threal [] [] [])] []] [Threal [(Threal [] [] [])] [] [(Threal [] [] [])]] [Threal [(Threal [] [] [])] [(Threal [] [] [])] []]) = " $r "
showThreal (Threal [Threal [(Threal [] [] [])] [(Threal [] [] [])] [], Threal [(Threal [] [] [])] [] [(Threal [] [] [])]] [Threal [(Threal [] [] [])] [] [(Threal [] [] [])]] [Threal [(Threal [] [] [])] [(Threal [] [] [])] []]) = " $r "
showThreal (Threal [Threal [] [(Threal [] [] [])] [(Threal [] [] [])]] [Threal [] [(Threal [] [] [])] [(Threal [] [] [])], Threal [(Threal [] [] [])] [(Threal [] [] [])] []] [Threal [(Threal [] [] [])] [(Threal [] [] [])] []]) = " $g "
showThreal (Threal [Threal [] [(Threal [] [] [])] [(Threal [] [] [])]] [Threal [(Threal [] [] []), Threal [] [(Threal [] [] [])] [(Threal [] [] [])]] [(Threal [] [] [])] []] [Threal [(Threal [] [] [])] [(Threal [] [] [])] []]) = " $g "
showThreal (Threal [Threal [] [(Threal [] [] [])] [(Threal [] [] [])]] [Threal [(Threal [] [] [])] [] [(Threal [] [] [])]] [Threal [] [(Threal [] [] [])] [(Threal [] [] [])], Threal [(Threal [] [] [])] [] [(Threal [] [] [])]]) = " $b "
showThreal (Threal [Threal [] [(Threal [] [] [])] [(Threal [] [] [])]] [Threal [(Threal [] [] [])] [] [(Threal [] [] [])]] [Threal [(Threal [] [] [])] [] [(Threal [] [] [])], Threal [] [(Threal [] [] [])] [(Threal [] [] [])]]) = " $b "

showThreal t@(Threal r g b)
    | t === (timber 2) = " 2* "
    | t === (timber 3) = " 3* "
    | t === (timber 4) = " 4* "
    | t === (timber 5) = " 5* "
    | otherwise = (colorify noColour "{ ") ++ shownR ++ (colorify yellowColour " \\ ") ++ shownG ++ (colorify cyanColour " / ") ++ shownB ++ (colorify noColour " }")
    where shownR = concatMap (colorify redColour . showThreal) r
          shownG = concatMap (colorify greenColour . showThreal) g
          shownB = concatMap (colorify blueColour .  showThreal) b

-- Tests

options = [tzero, red, green, blue, star, negRed, negGreen, negBlue, fullStar, rainbowStar, negRainbowStar, redArrow, greenArrow, blueArrow, negRedArrow, negGreenArrow, negBlueArrow, redNum 2, greenNum 2, blueNum 2, redStar, greenStar, blueStar, allRed, allGreen, allBlue, swapRG, swapGB, swapBR, redSink, greenSink, blueSink, redSource, greenSource, blueSource, staleRed, staleGreen, staleBlue, notRed, notGreen, notBlue, redGoneGreen, redGoneBlue, greenGoneRed, greenGoneBlue, blueGoneRed, blueGoneGreen, revenge, sophiesStar, clockwiseStar, anticlockwiseStar, timber 2, timber 3]

allOpts = [ x + y | x <- options, y <- options, x /= y]
shortOpts = [tzero, red, green, blue, star, rainbowStar, fullStar, negRed, negGreen, negBlue]
moreOpts = options `seq` options ++ ((+) <$> options <*> pure red) ++ ((+) <$> options <*> pure green)

allThreeComp :: Comps -> Threal -> Threal -> Bool
allThreeComp comps x y = redComp comps x y && greenComp comps x y && blueComp comps x y

notComp :: Comps -> Threal -> Threal -> Bool
notComp c x y = not $ allThreeComp c x y

crossComp = Comps redderThanCross greenerThanCross bluerThanCross
sameComp = Comps redderThanSame  greenerThanSame  bluerThanSame
fullyComp = Comps fullyRedderThan fullyGreenerThan fullyBluerThan
revCrossComp = Comps revRedderThanCross revGreenerThanCross revBluerThanCross
revSameComp = Comps revRedderThanSame revGreenerThanSame revBluerThanSame
revFullyComp = Comps revFullyRedderThan revFullyGreenerThan revFullyBluerThan
anyComp = Comps anyRedderThan anyGreenerThan anyBluerThan
allComp = Comps allRedderThan allGreenerThan allBluerThan
relComp = Comps eqRedGreen eqGreenBlue eqBlueRed
fullRelComp = Comps fullEqRedGreen fullEqGreenBlue fullEqBlueRed

allComps :: [Comps]
allComps = [relComp, fullRelComp, crossComp, sameComp, fullyComp, allComp]

workingComps :: [Comps]
workingComps = [fullRelComp, crossComp, fullyComp]

zeroEquivalent compList = [x | x <- options, allThreeComp compList x tzero]

testZeroEquivalents = map (\c -> [[x, y] | x <- zeroEquivalent c, y <- options, notComp c (threalAdd x y) y || notComp c (threalAdd y x) y]) allComps

--genEquivs compList comparand = filter (allThreeComp compList comparand) allOpts

compElem _ [] _ = False
compElem compList (x:xs) c
    | allThreeComp compList x c = True
    | otherwise = compElem compList xs c

compNub _ [] = []
compNub compList (x:xs)
    | compElem compList xs x = compNub compList xs
    | otherwise = x : compNub compList xs

setEqual x y = exactDiff x y == []

someSetEqual [] y = False
someSetEqual (x:xs) y
  | setEqual x y = True
  | otherwise = someSetEqual xs y

compSubset _ [] _ = True
compSubset _ _ [] = False
compSubset compList (x:xs) y
    | compElem compList y x = compSubset compList xs y
    | otherwise = False

compEqual compList x y = compSubset compList x y && compSubset compList y x

compDisparate _ [] _ = True
compDisparate _ _ [] = True
compDisparate compList (x:xs) y
    | compElem compList y x = False
    | otherwise = compDisparate compList xs y

sameOrNothing compList x y = compEqual compList x y || compDisparate compList x y

getAllEquivClasses compList = [filter (compare x) allOpts | x <- allOpts]
    where compare = allThreeComp compList

compareAllGroups compList [] = True
compareAllGroups compList (x:xs) = (and (map (sameOrNothing compList x) xs)) && (compareAllGroups compList xs)

getFullEquivClasses compList = foldl' (\acc z -> if any (compEqual compList z) acc then acc else z:acc) [] equivList
    where equivList = getAllEquivClasses compList

allEquivClassesSame compList = compareAllGroups compList $ getAllEquivClasses compList

equivalenceClasses _ [] = [[]]
equivalenceClasses f (x:xs) = (x:same) : equivalenceClasses f diff
    where (same, diff) = partition (f x) xs

getCompsEquivs compList = allOpts `seq` equivalenceClasses (allThreeComp compList) allOpts
--getCompsEquivs compList =  map (genEquivs compList) allOpts
--getCompsEquivs compList = foldl' (\acc x -> if someSetEqual acc x then acc else x : acc) [] compOpts
--  where compOpts = map (genEquivs compList) allOpts

getAllEquivs = map getCompsEquivs allComps

genTestEquivs compList = compsEquivs `seq` concatMap (\e -> map (\x -> [threalAdd x eq | eq <- e]) allOpts) compsEquivs
    where compsEquivs = getCompsEquivs compList

--reduceTestEquivs compList = equivList `seq` foldl' (\acc e -> if someSetEqual acc e then acc else e:acc) [] (map (compNub compList) equivList)
--    where equivList = genTestEquivs compList

reduceTestEquivs compList = equivList `seq` map (compNub compList) equivList
    where equivList = genTestEquivs compList

testCompEquivs compList = allOpts `seq` filter (\l -> length l > 1) (reduceTestEquivs compList)

testAllEquivs = map testCompEquivs allComps

testIdentities compList = (\c -> [x | x <- options, not $ allThreeComp c x x]) compList

--relDComp = [(\x y -> compGreener x y && compBluer x y), (\x y -> compBluer x y && compRedder x y), (\x y -> compRedder x y && compGreener x y)]
relDComp = map flip [compRedder, compGreener, compBluer]
fullRelDComp = map flip [fullCompRedder, fullCompGreener, fullCompBluer]

uniqueCL compList (Threal r g b) = Threal (compNub compList $ map (uniqueCL compList) r) (compNub compList $ map (uniqueCL compList) g) (compNub compList $ map (uniqueCL compList) b)

dominateCL compList@(rt:gt:bt:[]) (Threal r g b) = Threal redOpts greenOpts blueOpts
  where uniqueR = exactNub r
        uniqueG = exactNub g
        uniqueB = exactNub b
        --dR = uniqueR `seq` map (dominateCL compList) uniqueR
        --dG = uniqueG `seq` map (dominateCL compList) uniqueG
        --dB = uniqueB `seq` map (dominateCL compList) uniqueB
        noneLess comp l x = none (\z -> comp x z)  $ filter (not . (=== x)) l
        redOpts = uniqueR `seq` filter (noneLess rt uniqueR) uniqueR
        greenOpts = uniqueG `seq` filter (noneLess gt uniqueG) uniqueG
        blueOpts = uniqueB `seq` filter (noneLess bt uniqueB) uniqueB

-- testAddition compList domList x y = if (and res) then ""
--                             else "Failed: " ++ show x ++ ", " ++ show y ++ ": " ++ show res ++ "\n"
--   where compare = allThreeComp compList
--         us = threalAdd x y
--         cu = uniqueCL compList us
--         d  = dominateCL domList cu
--         dus = dominateCL domList us
--         --rus = reversible us
--         --rcu = reversible cu
--         --rd  = reversible d
--         res = [compare us cu] ++ [compare cu d, compare us dus] -- ++ [compare rus us, compare rcu cu, compare rd d, compare rd us]

-- testCompAddition compList domList = allOpts `seq` [compAddition x y | x <- allOpts, y <- allOpts]
--     where compAddition = testAddition compList domList

-- printTestComp compList domList = putStrLn $ concat $ testCompAddition compList domList

testAddition :: Comps -> Int -> Threal -> Threal -> StateT (HashMap Threal Threal, HashMap Threal Threal) IO ()
testAddition fs n x y = do
    (domCache, revCache) <- get
    let
      us = threalAdd x y
      cu = cuc us
      (d, dCache)  = runState (dsc cu) domCache
      (dus, dusCache) = runState (dsc us) dCache
      (rus, rusCache) = runState (rsc us) revCache
      (rcu, rcuCache) = runState (rsc cu) rusCache
      (rd, rdCache)  = runState (rsc d) rcuCache
      res = [compare us cu] ++ [compare cu d, compare us dus] ++ [compare rus us, compare rcu cu, compare rd d, compare rd us]
      -- fullRes = map fst $ filter snd $ zip [[us, cu], [cu, d], [us, dus], [rus, us], [rcu, cu], [rd, d], [rd, us]] res
    when (not $ and res) (liftIO $ putStr $ pointer ++ "Failed: " ++ show x ++ ", " ++ show y ++ ": " ++ show res ++ "\n")
    put (dusCache, rdCache)
  where compare = allThreeComp fs
        stars = replicate n '*'
        pointer = stars ++ "-> "
        cuc = completelyUnique fs
        dsc = dominateState fs
        rsc = reversibleState fs

testCompAddition :: Int -> Comps -> IO [()]
testCompAddition n fs = allOpts `seq` mapM (\(x,y) -> evalStateT (testAddition fs n x y) (M.empty, M.empty)) [(x, y) | x <- shortOpts, y <- options]

testAllCompsAddition :: IO ()
testAllCompsAddition = mapM_ (\(f, i) -> liftIO (putStrLn "-------") >> testCompAddition i f) indexedComps >> return ()
  where indexedComps = zip workingComps [1..]

-- printTestComps = traverse (putStrLn . concat . testCompAddition $ fs) allComps

-- printTestComp fs = putStrLn $ concat $ testCompAddition fs

-- printAllTestComps = traverse (\fs -> putStrLn "---" >> printTestComp fs) allComps

-- printWorkingTestComps = traverse (\fs -> putStrLn "---" >> printTestComp fs) workingComps

testAssociativity x y z = ((x + y) + z) =|= (x + (y + z))

testAllAssociative = [testAssociativity x y z | x <- options, y <- options, z <- options]
