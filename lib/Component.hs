module Component (components, dfs, neighbors, applyDFS) where

import Data.List ((\\))
import qualified Data.Map.Strict as M
import Data.SBV
import qualified Data.Set as S
import Spec
import Utils
import Prelude hiding (lookup)

type Label = (Index, SWord8)

reconstruct :: [Label] -> [[SWord8]]
reconstruct ls =
  let ixNMap = M.fromList ls
      ixs = map fst ls
      (h, w) = maximum ixs
   in [[ixNMap M.! (j, i) | i <- [1 .. h]] | j <- [1 .. w]]

--           Labels  Unvisited    Labels  Unvisited
components :: [[SBool]] -> [[SWord8]]
components bs =
  let ixs = let (h, w) = dim bs in [toEnum <$$> (i, j) | i <- [1 .. h], j <- [1 .. w]] :: [Index]
      (ls, _, _) = applyDFS bs ([], ixs, 1)
      ns = reconstruct ls
   in ns

dim :: [[a]] -> (Int, Int)
dim r@(c : _) = (length r, length c)
dim _ = (0, 0)

--          Grid          Labs     Unexp    C#         Labs     Unex     C#
applyDFS :: [[SBool]] -> ([Label], [Index], Word8) -> ([Label], [Index], Word8)
applyDFS bs (ls, next : unvs, num) =
  -- Explore
  let (ls', vs) = dfs bs next (S.fromList (next : unvs)) num
   in applyDFS bs (ls ++ ls', unvs \\ vs, num + 1)
applyDFS _ x = x

-- | Depth first search
-- Returns (, )
--     Grid         Current  Unvisited    C#        newLab     Visited
dfs :: [[SBool]] -> Index -> S.Set Index -> Word8 -> ([Label], [Index])
dfs bs ix unvisited num =
  if not $ S.member ix unvisited
    then ([], [])
    else
      iteLazy
        (lookup ix bs) -- Current Value of ix
        -- TRUE
        ( foldl
            f
            z
            (neighbors dims ix)
        )
        -- FALSE
        ( let labels = [(ix, literal 0)]
              visited = [ix]
           in (labels, visited)
        )
  where
    --    labels   visited     searchpoint
    f :: ([Label], [Index]) -> Index -> ([Label], [Index])
    f (ls, vs) x =
      let (ls', vs') = dfs bs x unvisited' num
          unvisited' = S.difference unvisited (S.fromList vs)
       in (ls ++ ls', vs ++ vs')
    z =
      let labels = [(ix, literal num)]
          visited = [ix]
       in (labels, visited)
    dims = (length bs, length . head $ bs)

neighbors :: (Int, Int) -> Index -> [Index]
neighbors (h, w) ix =
  let x = fromEnum <$$> ix
      validNeighbors =
        filter
          ( \(i, j) ->
              not . or $
                [ i < 1,
                  j < 1,
                  i > h,
                  j > w
                ]
          )
          ( map
              (x +:)
              [ (1, 0),
                (0, 1),
                (-1, 0),
                (0, -1)
              ]
          )
   in map (toEnum <$$>) validNeighbors

(+:) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(+:) (a, b) (c, d) = (a + c, b + d)
