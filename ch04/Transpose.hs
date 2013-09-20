module Transpose
where

transposeRec [] = []

transposeRec l =
	(map head l) : (transposeRec $ filter (/= "" ) (map tail l))

transpose input =
    let l = lines input
    in unlines $ transposeRec l