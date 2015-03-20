{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Graph.Simple.Vertex (
  Vertex, unVertex

, vertex, vertexMay, unsafeVertex
, minVAsInt, maxVAsInt, minVertex, maxVertex
) where

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
--   unsafe!
{-# INLINE unsafeVertex #-}
unsafeVertex ∷ Int → Vertex
unsafeVertex = Vertex

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
