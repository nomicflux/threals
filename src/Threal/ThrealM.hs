module Threal.ThrealM where

import Threal.Base

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Control.Monod.Reader
import Control.Monad.State (State, get, put, runState, evalState, StateT, liftIO, evalStateT)
import Control.Monad (when, mapM, mapM_)

data Comps = Comps { redComp :: (Threal -> Threal -> Bool)
                   , greenComp :: (Threal -> Threal -> Bool)
                   , blueComp :: (Threal -> Threal -> Bool)
                   }

data ThrealCache = { dominatedCache :: HashMap Threal Threal
                   , reversedCache :: HashMap Threal Threal
                   , uniqueCache :: HashMap Threal Threal
                   }

type ThrealM a = StateT ThrealCache (ReaderT IO Comps) a

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

dominate :: Comps -> Threal -> Threal
dominate c x = evalState (dominateState c x) M.empty

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

reversible :: Comps -> Threal -> Threal
reversible c x = evalState (reversibleState c x) M.empty

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

simplify :: Comps -> Threal -> Threal
simplify c x = completelyUnique c x

add :: Comps -> Threal -> Threal -> Threal
add c x y = simplifyC c $ threalAdd x y
