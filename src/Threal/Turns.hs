module Threal.Turns where

import Threal.Base
import Threal.Comps

nextTurn :: Threal -> [Threal]
nextTurn (Threal r g b) = nub [Threal x y z | x <- redParts, y <- greenParts, z <- blueParts]
    where greenParts = map greenPart r
          blueParts = map bluePart g
          redParts = map redPart b

nextTurns :: [Threal] -> [Threal]
nextTurns = nub . (concatMap nextTurn )

fastForward :: Threal -> Int -> [[Threal]]
fastForward t n = take n $ iterate nextTurns [t]
