module PuzzleDSL.Internal.Component (compRelation, dfs, dfs', neighbors, slookup, sFst, Label) where

import Data.List ()
import Data.SBV
import qualified Data.SBV.List as L
import Data.SBV.Tuple (tuple, untuple)
import PuzzleDSL.Internal.Spec
import PuzzleDSL.Internal.Utils
import Prelude hiding (lookup)

type Label = (Index, Word8)

-- | `sbv` fst
sFst :: (SymVal a, SymVal b) => SBV (a, b) -> SBV a
sFst t = let (t1, _) = untuple t in t1

-- | `sbv` snd
sSnd :: (SymVal a, SymVal b) => SBV (a, b) -> SBV b
sSnd t = let (_, t2) = untuple t in t2

-- | Gets the dimensions of a nested list
dim :: [[a]] -> (Int, Int)
dim r@(c : _) = (length r, length c)
dim _ = (0, 0)

-- | `dfs bs x`
-- Calls a depth first search on `bs` :: [[SBool]]
-- starting at index `x`, and returns a list of indices
-- in the connected component containing `x`

-- Note: this function calls the helper function `dfs'`
dfs :: [[SBool]] -> SBV Index -> SBV [Index]
dfs bs x =
  let ls = sFst $ dfs' bs x ixs 1
      ixs =
        let (h, w) = dim bs
         in literal
              [ toEnum <$$> (i, j)
                | i <- [1 .. h],
                  j <- [1 .. w]
              ] ::
              SBV [Index]
      compLs = L.filter (\l -> sSnd l .== 1) ls
      compIxs = L.map sFst compLs
   in compIxs

-- | Function to create an equivalence relation based on
-- the connected components of a grid of `SBool`.
compRelation :: [[SBool]] -> CellVar -> CellVar -> SBool
compRelation bs x y =
  let x' = literal (rawRow x, rawCol x)
      y' = literal (rawRow y, rawCol y)
      component = dfs bs x'
   in x' `L.elem` component .&& y' `L.elem` component

-- | Depth first search main procedure:
-- `dfs' bs ix unv num`
-- Performs a depth first search on `bs` :: [SBool],
-- starting at index `ix`, given unvisisted cells `unv`,
-- and the current component number `num`.
-- The procedure outputs:
-- (labels, visited)
-- Where:
-- - *labels* is a list of (Index, component#)
-- - *visisted* is a list of the indices visited
--   during the search
dfs' :: [[SBool]] -> SBV Index -> SBV [Index] -> SBV Word8 -> SBV ([Label], [Index])
dfs' bs ix unvisited num =
  ite
    -- check whether the current node has been visited.
    -- If so, then return ([], [])
    -- I.e. (no additional labels, no additional visits)
    -- Otherwise proceed
    (ix `L.elem` unvisited)
    ( ite
        -- branch based on the current value of the node
        (slookup ix bs)
        -- TRUE
        ( L.foldr
            f
            z
            (neighbors dims ix)
        )
        -- FALSE
        ( let labels = L.singleton . tuple $ (ix, 0)
              visited = L.singleton ix
           in tuple (labels, visited)
        )
    )
    (literal ([], []))
  where
    -- function to combine dfs search results.
    --                    |       OLD      |         |      NEW      |
    --   searchpoint       labels   visited          labels   visited
    f :: SBV Index -> SBV ([Label], [Index]) -> SBV ([Label], [Index])
    f x l = tuple (ls L.++ ls', vs L.++ vs')
      where
        (ls, vs) = untuple l
        (ls', vs') = untuple $ dfs' bs x unvisited' num
        unvisited' = unvisited \\ vs

    -- seed value for fold should be ([ix, num], [ix])
    z :: SBV ([Label], [Index])
    z =
      let labels = L.singleton . tuple $ (ix, num)
          visited = L.singleton ix
       in tuple (labels, visited)
    dims = (length bs, length . head $ bs)

-- | The set difference operation, rewritten for SBV lists
(\\) :: (SymVal a, Eq a) => SBV [a] -> SBV [a] -> SBV [a]
x \\ b = L.filter (`L.notElem` b) x

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
(+:) :: SBV (Word8, Word8) -> SBV (Word8, Word8) -> SBV (Word8, Word8)
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

-- labels2Array :: SBV [Label] -> SArray Index Word8
-- labels2Array ls = insertLabels ls (sListArray 0 [])

-- insertLabels :: SBV [Label] -> SArray Index Word8 -> SArray Index Word8
-- insertLabels bigL accum =
--   let (l, ls) = L.uncons bigL
--    in ite
--         (sNot $ L.null ls)
--         ( let accum' = writeArray accum ix n
--               (ix, n) = untuple l
--            in insertLabels ls accum'
--         )
--         accum

-- getComponent :: SBV [Label] -> SBV Index -> SBV Word8
-- getComponent ls ix = sSnd $ L.head (L.filter (\l -> sFst l .== ix) ls)

-- --           Labels  Unvisited    Labels  Unvisited
-- components :: [[SBool]] -> SBV [Label]
-- components bs =
-- let ixs = let (h, w) = dim bs in [toEnum <$$> (i, j) | i <- [1 .. h], j <- [1 .. w]] :: [Index]
--       (ls, _, _) = untuple $ applyDFS bs $ literal ([], ixs, 1)
--    in ls

-- --          Grid          Labs     Unexp    C#         Labs     Unex     C#
-- applyDFS :: [[SBool]] -> SBV ([Label], [Index], Word8) -> SBV ([Label], [Index], Word8)
-- applyDFS bs x =
--   -- Explore
--   let (ls, unexp, num) = untuple x
--    in ite
--         (sNot . L.null $ unexp)
--         ( let next = L.head unexp
--               unvs = L.tail unexp
--               (ls', vs) = untuple $ dfs bs next (next L..: unvs) num
--               x' = tuple (ls L.++ ls', unvs \\ vs, num + 1)
--            in applyDFS bs x'
--         )
--         x
