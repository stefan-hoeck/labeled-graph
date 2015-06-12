{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module        : Data.Graph.Simple.Vertex
Description   : Newtype wrapper for graph vertices
Copyright     : Stefan Höck
Maintainer    : Stefan Höck
Stability     : experimental

This module provides an abstract newtype wrapper for
graph vertices. This gives certain guaranties about vertices,
mainly that they stay within certain bounds. Vertices
can range from 0 to an upper bound which depends on the
system on which the application runs (see 'maxVertex').
-}
module Data.Graph.Simple.Vertex (
-- * Class
  Vertex, unVertex

-- * Constructors  
, vertex, vertexMay, unsafeVertex

-- * Bounds
, minVAsInt, maxVAsInt, minVertex, maxVertex
) where

import Control.DeepSeq (NFData)

-- | Newtype representing vertices in a graph
--
--   Internally, a vertex is just an Int but to make
--   sure that only valid vertices are created, the
--   newtype's constructor is not made publi.
newtype Vertex = Vertex { unVertex ∷ Int }
  deriving (Eq, Ord, NFData)

instance Show Vertex where
  show = show . unVertex

instance Bounded Vertex where
  minBound = minVertex
  maxBound = maxVertex

instance Enum Vertex where
  fromEnum = unVertex
  toEnum   = vertex

-- This is a hack for uncluttering the syntax when using
-- vertices. Not sure whether we should really do this.
instance Num Vertex where
  (Vertex a) + (Vertex b) = vertex $ a + b
  (Vertex a) * (Vertex b) = vertex $ a * b
  abs      = id
  negate   = id
  signum _ = Vertex 1
  fromInteger = vertex . fromInteger

-- | Create a vertex from an Int
--   Returns Nothing if the Int is out of
--   valid bounds.
vertexMay ∷ Int → Maybe Vertex
vertexMay v | isValidVertex v = Just $ Vertex v
            | otherwise       = Nothing

-- | Create a vertex from an Int
--   Used i `mod` maxVAsInt if i is out of bounds.
vertex ∷ Int → Vertex
vertex v | isValidVertex v = Vertex v
         | otherwise       = Vertex $ v `mod` maxVAsInt

-- | Create a vertex from an Int
--   No bounds checking for maximum performance. Highly
--   unsafe! USE AT YOUR OWN RISK
{-# INLINE unsafeVertex #-}
unsafeVertex ∷ Int → Vertex
unsafeVertex = Vertex


-- | Min and max integer bounds for vertices
minVAsInt, maxVAsInt ∷ Int
minVAsInt = 0
maxVAsInt = (floor . sqrt $ maxD) - 1
    where maxD ∷ Double
          maxD = fromIntegral (maxBound ∷ Int)


-- | The minimum vertex
minVertex ∷ Vertex
minVertex = Vertex minVAsInt

-- | The maximal allowed vertex
--
--   Note that this corresponds to √(maxBound ∷ Int), so
--   on 32-Bit systems, this is 46'339, while on 64-Bit
--   systems, it is 3'037'000'498.
maxVertex ∷ Vertex
maxVertex = Vertex maxVAsInt


isValidVertex ∷ Int → Bool
isValidVertex v = minVAsInt <= v && v <= maxVAsInt
