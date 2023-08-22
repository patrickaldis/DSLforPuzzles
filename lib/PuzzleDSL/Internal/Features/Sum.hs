module PuzzleDSL.Internal.Features.Sum (cellSum) where

import Data.SBV (SBV, SWord8, SymVal)
import qualified Data.SBV.List as L

cellSum :: [[SWord8]] -> SWord8
cellSum = L.foldl (+) 0 . toSBVList

toSBVList :: SymVal a => [[SBV a]] -> SBV [a]
toSBVList = L.implode . concat
