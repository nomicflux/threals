module Threal.Tests where

import Threal.Base
import Threal.Comps
import Threal.ThrealM

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
