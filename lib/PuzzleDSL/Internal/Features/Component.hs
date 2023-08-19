module PuzzleDSL.Internal.Features.Component
  ( neighbors,
    slookup,
    Label,
    isConnected,
    isConnectedIx,
  )
where

import Data.List ()
import Data.SBV
import qualified Data.SBV.List as L
import Data.SBV.Tuple (tuple, untuple)
import PuzzleDSL.Internal.Spec
import PuzzleDSL.Internal.Utils
import Prelude hiding (lookup)

type Label = (Index, Word8)

-- | Gets the dimensions of a nested list
dim :: [[a]] -> (Int, Int)
dim r@(c : _) = (length r, length c)
dim _ = (0, 0)

-- | Determines if two cellvars are connected with a path
isConnected :: [[SBV Bool]] -> CellVar -> CellVar -> SBV Bool
isConnected bs x' y' =
  let x = tuple (row x', col x')
      y = tuple (row y', col y')
   in isConnectedIx bs L.nil x y

-- | Deteremines if two indices are connected with a path
isConnectedIx :: [[SBV Bool]] -> SBV [Index] -> SBV Index -> SBV Index -> SBV Bool
isConnectedIx bs blacklist x y =
  let nbs x' y' = y' `L.elem` neighbors (dim bs) x'
      val = flip slookup bs
      cnd = isConnectedIx bs (x L..: blacklist)
   in sAnd
        [ val x .== val y,
          sOr
            [ x .== y,
              x `nbs` y,
              quantifiedBool
                ( \(Exists z) ->
                    sAnd
                      [ z `nbs` x,
                        z `L.notElem` blacklist,
                        z `cnd` y
                      ]
                )
            ]
        ]

-- | Function to produce the possible neighbors of an index
-- given the board size. Recall indices are 1-based.
neighbors :: (Int, Int) -> SBV Index -> SBV [Index]
neighbors dims ix =
  let (h, w) = toEnum <$$> dims
      validNeighbors :: SBV [Index]
      validNeighbors =
        L.filter
          ( \x ->
              let (i, j) = untuple x
               in sNot . sOr $
                    [ i .< 1,
                      j .< 1,
                      i .> h,
                      j .> w
                    ]
          )
          ( L.map
              (ix +:)
              ( literal
                  [ (1, 0),
                    (0, 1),
                    (-1, 0),
                    (0, -1)
                  ]
              )
          )
   in validNeighbors

-- | Helper function for index combination
(+:) :: SBV Index -> SBV Index -> SBV Index
(+:) x y =
  let (a, b) = untuple x
      (c, d) = untuple y
   in tuple (a + c, b + d)

-- | Function to look up the cell value in a grid, given an SBV Index
slookup :: SBV Index -> [[SBool]] -> SBool
slookup ix a =
  let (i, j) = untuple ix
      xs !..! n = select xs (head a) n
      x !.! n = select x sTrue n
   in (a !..! (i - 1)) !.! (j - 1)
