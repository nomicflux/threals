module Threal.Comps where

import Threal.Base
import Data.List ((\\))

none :: (a -> Bool) -> [a] -> Bool
none f l = and $ (map (not . f)) l

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
signature :: Threal -> Threal -> [Bool]
signature x y = [x `redderThan` y, x `greenerThan` y, x `bluerThan` y, y `redderThan` x, y `greenerThan` x, y `bluerThan` x]

(<|>) = signature

compSig x y = [[compRedGreen x y, compGreenBlue x y, compBlueRed x y, compRedGreen y x, compGreenBlue y x, compBlueRed y x],
               [fullCompRedGreen x y, fullCompGreenBlue x y, fullCompBlueRed x y, fullCompRedGreen y x, fullCompGreenBlue y x, fullCompBlueRed y x]]
