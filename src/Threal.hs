module Threal where

import Data.List
import Data.Hashable
import Control.Applicative( (<$>), (<*>), pure )

data Threal = Threal [Threal] [Threal] [Threal]

instance Hashable Threal where
  hashWithSalt salt (Threal reds greens blues) = ((redHash + greenHash + blueHash) * salt) `mod` 34321347
    where
      hasher n arr = n + 3 * (sum $ map ((+ (length arr)) . hashWithSalt (salt + n)) arr)
      redHash = hasher 0 reds
      greenHash = hasher 1 greens
      blueHash = hasher 2 blues

--threalNum :: Comps -> Threal -> Integer -> Threal
--threalNum c t n = completelyUnique c $ foldl' (\acc n -> threalAdd acc t) tzero [1..n]

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

---
---
-- instance Num Threal where
--   (+) x y = threalAdd x y
--   negate x = threalAdd (turnClockwise x) (turnAnticlockwise x)
--   (-) x y = x + (negate y)
--   (*) x y = tzero
--   abs x = x
--   signum x = firstPart + secondPart
--       where (rcomp:gcomp:bcomp:arcomp:agcomp:abcomp:[]) = x <|> tzero
--             firstPart
--                 | rcomp && gcomp && bcomp = tzero
--                 | rcomp && gcomp = negBlue
--                 | rcomp && bcomp = negGreen
--                 | gcomp && bcomp = negRed
--                 | rcomp = red
--                 | gcomp = green
--                 | bcomp = blue
--                 | otherwise = star
--             secondPart
--                 | arcomp && agcomp && abcomp = tzero
--                 | arcomp && agcomp = blue
--                 | arcomp && abcomp = green
--                 | agcomp && abcomp = red
--                 | arcomp = negRed
--                 | agcomp = negGreen
--                 | abcomp = negBlue
--                 | otherwise = star
--   fromInteger n = (redNum n) + (greenNum n) + (blueNum n)

--(*) x@(Threal r1 g1 b1) y@(Threal r2 g2 b2) = Threal redMapped greenMapped blueMapped
--    where redMapped = [r1*y + x*r2 - r1*r2, g1*y + x*b2 - g1*b2, b1*y + x*g2 - b1*g2]
--          greenMapped = [g1*y + x*g2 - g1*g2, b1*y + x*r2 - b1*r2, r1*y + x*b2 - r1*b2]
--          blueMapped = [b1*y + x*b2 - b1*b2, r1*y + x*g2 - r1*g2, g1*y + x*r2 - g1*r2]


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

-- Simplifications

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

-- dominate :: Threal -> Threal
-- dominate x = evalState (dominateState relComp x) M.empty

--nand = not . and
--nor = not . or

-- compReversible :: (Threal -> Threal -> Bool) -> (Threal -> [Threal]) -> (Threal -> [Threal]) -> (Threal -> [Threal] -> Threal) -> (Threal -> [Threal] -> Threal) -> Threal -> Threal
-- compReversible comp fstGet sndGet fstSet sndSet x = sndSet (fstSet x mapped1) mapped2
--     where compOpts1 = map (\z -> (z, sndGet z)) $ fstGet x
--           mapped1 = concatMap (\(orig, revved) -> if any (`comp` x) revved then revved else [orig]) compOpts1
--           compOpts2 = map (\z -> (z, fstGet z)) $ sndGet x
--           mapped2 = concatMap (\(orig, revved) -> if any (x `comp`) revved then revved else [orig]) compOpts2

-- rgReversible = compReversible compRedGreen redPart greenPart replaceRed replaceGreen
-- gbReversible = compReversible compGreenBlue greenPart bluePart replaceGreen replaceBlue
-- brReversible = compReversible compBlueRed bluePart redPart replaceBlue replaceRed

-- rgbReversible = brReversible . gbReversible . rgReversible

-- reversible :: Threal -> Threal
-- reversible x = Threal redOpts greenOpts blueOpts
--   where ux@(Threal uniqueR uniqueG uniqueB) = uniqueFields allComp x
--         rR = map reversible uniqueR
--         rG = map reversible uniqueG
--         rB = map reversible uniqueB
--         redComp r@(Threal rr rg rb)
--             | not $ null gbBetter = concatMap redPart gbBetter
--             | otherwise = [r]
--             where gbBetter = filter (\z -> z `compGreener` x && z `compBluer` x) (rg++rb)
--         greenComp g@(Threal gr gg gb)
--             | not $ null brBetter = concatMap greenPart brBetter
--             | otherwise = [g]
--             where brBetter = filter (\z -> z `compBluer` x && z `compRedder` x) (gb++gr)
--         blueComp b@(Threal br bg bb)
--             | not $ null rgBetter = concatMap bluePart rgBetter
--             | otherwise = [b]
--             where rgBetter = filter (\z -> z `compRedder` x && z `compGreener` x) (br++bg)
--         redOpts = rR `seq` concatMap redComp rR
--         greenOpts = rG `seq` concatMap greenComp rG
--         blueOpts = rB `seq` concatMap blueComp rB

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

-- simplify :: Threal -> Threal
-- simplify x = x `seq` completelyUnique allComp x

  -- Show Instance
