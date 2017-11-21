module Threal.Show where

import Threal
import Threal.Threals
import Threal.Comps

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
