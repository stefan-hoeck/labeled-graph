{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Graph.Simple.Vertex (
  Vertex, unVertex

, vertex, vertexMay
, minVAsInt, maxVAsInt, minVertex, maxVertex
) where

import Data.Bool (Bool, (&&), otherwise)
import Data.Eq (Eq)
import Data.Function (($), (.), id)
import Data.Int (Int)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Ord (Ord, (<=))
import Data.String (String)
import GHC.Enum (Enum, toEnum, fromEnum, Bounded, minBound, maxBound)
import GHC.Float (sqrt, Double)
import GHC.Num (Num(..))
import GHC.Real (floor, fromIntegral)
import Prelude (error)
import Text.Show (Show, show)

-- | Newtype representing vertices in a graph
newtype Vertex = Vertex { unVertex ∷ Int }
  deriving (Eq, Ord)

instance Show Vertex where
  show (Vertex v) = 'V' : show v

instance Bounded Vertex where
  minBound = minVertex
  maxBound = maxVertex

instance Enum Vertex where
  fromEnum = unVertex
  toEnum   = vertex

instance Num Vertex where
  (Vertex a) + (Vertex b) = vertex $ a + b
  (Vertex a) * (Vertex b) = vertex $ a * b
  abs      = id
  negate   = id
  signum _ = Vertex 1
  fromInteger = vertex . fromInteger

vertexMay ∷ Int → Maybe Vertex
vertexMay v | isValidVertex v = Just $ Vertex v
            | otherwise       = Nothing

vertex ∷ Int → Vertex
vertex v | isValidVertex v = Vertex v
         | otherwise       = error $ invalidVertexMsg v

isValidVertex ∷ Int → Bool
isValidVertex v = minVAsInt <= v && v <= maxVAsInt

minVAsInt, maxVAsInt ∷ Int
minVAsInt = 0
maxVAsInt = (floor . sqrt $ maxD) - 1
    where maxD ∷ Double
          maxD = fromIntegral (maxBound ∷ Int)

minVertex ∷ Vertex
minVertex = Vertex minVAsInt

-- | The maximal allowed vertex
--
--   Note that this corresponds to √(maxBound ∷ Int), so
--   on 32-Bit systems, this is 46'339, while on 64-Bit
--   systems, it is 3'037'000'498.
maxVertex ∷ Vertex
maxVertex = Vertex maxVAsInt

invalidVertexMsg ∷ Int → String
invalidVertexMsg v = "Invalid vertex: " 
                     <> show v
                     <> "; Vertices must lie in the interval ["
                     <> show minVAsInt
                     <> ","
                     <> show maxVAsInt
                     <> "]"
