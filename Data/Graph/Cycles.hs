module Data.Graph.Cycles (
  cyclicVertices, cycles, cyclesN, cyclicEdges
, cyclicSubgraph, d2Forest
) where

import Control.Monad (unless, when)
import Control.Monad.ST (runST)
import Data.Graph.Simple.Graph
import Data.Graph.Simple.Util
import Data.STRef.Strict
import Data.Tree (Forest)

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MVU

cyclicEdges ∷ Graph → [Edge]
cyclicEdges = snd . cvs

cyclicSubgraph ∷ Graph → Graph
cyclicSubgraph g = fromList (order g) $ cyclicEdges g

cyclicVertices ∷ Graph → [Vertex]
cyclicVertices g = let cs = fst $ cvs g
                   in  filter ((cs UV.!) . unVertex) (vertices g)

cycles ∷ Graph → Vertex → [[Vertex]]
cycles g v = fmap (v:) . filter (keepCycle g v) $ paths g v

cyclesN ∷ Int → Graph → Vertex → [[Vertex]]
cyclesN n g v = fmap (v:) . filter (keepCycle g v) $ pathsN n g v

d2Forest ∷ Graph → Forest Vertex
d2Forest g = let ds    = degreesV g
                 valid = (2 ==) . (ds UV.!) . unVertex 
             in  dfsFiltered g valid
----------------------------------------------------------------------
-- Algorithms

keepCycle ∷ Graph → Vertex → [Vertex] → Bool
keepCycle g v (v':t@(_:_:_)) | adjacent g v v'  = keep t
                          where keep (v'':_:[]) = v'' > v'
                                keep (_:t')     = keep t'
                                keep _          = error "not possible"
keepCycle _ _ _                                 = False

cvs ∷ Graph → (UV.Vector Bool, [Edge])
cvs g = let o = order g
        in
        runST $ do vs ← MVU.replicate o False     -- visited vertices
                   mk ← MVU.replicate o (0 ∷ Int) -- vertices marked as cycle endpoints
                   cs ← MVU.replicate o False     -- vertices in cycles
                   bs ← newSTRef []

                   let visited'   = unsafeReadVU vs
                   let visit' v   = unsafeWriteVU vs v True
                   let marked'    = unsafeReadVU mk
                   let mark' v    = unsafeModVU mk v (+1)
                   let unmark' v  = unsafeWriteVU mk v 0
                   let cycle' v   = unsafeWriteVU cs v True
                   let ns v       = neighbors g v
                   let ns' p v    = filter (p /=) $ ns v
                   let addE p v   = modifySTRef bs (edge v p :)

                   let check' p v = do vis ← visited' v
                                       if vis 
                                       then do
                                         mark' v            
                                         addE p v
                                         return 1
                                       else do
                                         visit' v
                                         is ← mapM (check' v) $ ns' p v
                                         let s = sum is
                                         if s > 0    -- we are within a cycle
                                         then do
                                           cycle'  v
                                           m ← marked' v
                                           let res = s - 2*m
                                           if m > 0  -- we are at the end of some cycles
                                           then do
                                             unmark' v
                                             when (res > 0) $ addE p v
                                             return $ res
                                           else addE p v >> return s
                                         else return s

                   let check v = do vis ← visited' v
                                    unless vis $ check' v v >> return ()

                   mapM_ check $ vertices g

                   (,) <$> UV.unsafeFreeze cs <*> readSTRef bs

-- cvs ∷ Graph → UV.Vector Bool
-- cvs g = let o = order g
--         in
--         runST $ do vs ← MVU.replicate o False     -- visited vertices
--                    mk ← MVU.replicate o (0 ∷ Int) -- vertices marked as cycle endpoints
--                    cs ← MVU.replicate o False     -- vertices in cycles
-- 
--                    let visited'  = unsafeReadVU vs
--                    let visit' v  = unsafeWriteVU vs v True
--                    let marked'   = unsafeReadVU mk
--                    let mark' v   = unsafeModVU mk v (+1)
--                    let unmark' v = unsafeWriteVU mk v 0
--                    let cycle' v  = unsafeWriteVU cs v True
--                    let ns v      = neighbors g v
--                    let ns' p v   = filter (p /=) $ ns v
-- 
--                    let check' p v = do vis ← visited' v
--                                        traceM $ "Checking " <> show v <> "; parent: " <> show p <> "; visited: " <> show vis
--                                        if vis 
--                                        then do
--                                          mark' v            
--                                          traceM $ "Cycle found at " <> show v
--                                          return 1
--                                        else do
--                                          visit' v
--                                          traceM $ "Marked " <> show v <> " as visited"
--                                          is ← mapM (check' v) $ ns' p v
--                                          let s = sum is
--                                          if s > 0    -- we are within a cycle
--                                          then do
--                                            traceM $ show v <> " within " <> show s <> " cycles"
--                                            cycle'  v
--                                            m ← marked' v
--                                            if m > 0  -- we are at the end of some cycles
--                                            then do
--                                              traceM $ "End of cycle found at " <> show v
--                                              traceM $ "Unmarking and returning " <> show (s - 2*m)
--                                              unmark' v
--                                              return $ s-2*m
--                                            else return s
--                                          else return s
-- 
--                    let check v = do vis ← visited' v
--                                     unless vis $ check' v v >> return ()
-- 
--                    mapM_ check $ vertices g
-- 
--                    UV.unsafeFreeze cs
