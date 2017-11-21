module Threal.Threals where

import Threal

tzero :: Threal
tzero = Threal [] [] []

star = Threal [tzero] [tzero] [tzero]

timber :: Integer -> Threal
timber 0 = tzero
timber 1 = star
timber n = Threal opts opts opts
    where opts = map timber [n-1, n-2 .. 0]

red = Threal [tzero] [] []
green = Threal [] [tzero] []
blue = Threal [] [] [tzero]

negRed = Threal [] [tzero] [tzero]
negGreen = Threal [tzero] [] [tzero]
negBlue = Threal [tzero] [tzero] []
--negRed = negate red
--negGreen = negate green
--negBlue = negate blue

fullStar = Threal [star] [star] [star]

rainbowStar = threalAdd (threalAdd red green) blue
negRainbowStar = threalAdd (threalAdd negRed negGreen) negBlue

threalNum :: Threal -> Integer -> Threal
threalNum t n = foldl (\acc n -> threalAdd acc t) tzero [1..n]

redNum :: Integer -> Threal
redNum = threalNum red

greenNum :: Integer -> Threal
greenNum = threalNum green

blueNum :: Integer -> Threal
blueNum = threalNum blue

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
