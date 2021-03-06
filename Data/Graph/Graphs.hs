--   Samples of graphs useful for testing
--   Many examples come from organic chemistry. These were generated
--   from SMILES strings using a smiles parser which generates
--   LGraphs
module Data.Graph.Graphs (
  methane, ethane, propane, butane, alkane

, isobutane, isopentane, neopentane

, cycloalkane, cyclopropane, cyclobutane, cyclopentane, cyclohexane

, ocimene, limonene, pinene
, viagra, xanax, phentermine, valium, ambien, nexium, vioxx
, paxil, lipitor, cialis, strychnine, cocaine, quinine, lsd
, morphine, heroine, nicotine, vitamine_a, caffeine

, module Data.Graph.Simple
) where

import Data.Graph.Simple

-- * Alkanes
methane, ethane, propane, butane ∷ Graph
methane = alkane 1
ethane  = alkane 2
propane = alkane 3
butane  = alkane 4

alkane ∷ Int → Graph
alkane n = chain n

-- * Cycles

cyclopropane, cyclobutane, cyclopentane, cyclohexane ∷ Graph
cyclopropane = cycloalkane 3
cyclobutane  = cycloalkane 4
cyclopentane = cycloalkane 5
cyclohexane  = cycloalkane 6

cycloalkane ∷ Int → Graph
cycloalkane n = fromList n $ (0 <-> (n-1)) : [x <-> (x+1) | x ← [0..n-2]]


-- * Branched alkanes
isobutane, isopentane, neopentane ∷ Graph
isobutane  = fromList 4 [0 <-> 1,1 <-> 2,1 <-> 3]
isopentane = fromList 5 [0 <-> 1,1 <-> 2,2 <-> 3,2 <-> 4]
neopentane = fromList 5 [0 <-> 1,1 <-> 2,1 <-> 3,1 <-> 4]


-- * Monoterpenes
ocimene,limonene,pinene ∷ Graph
ocimene  = fromList 10 [0 <-> 1,1 <-> 2,1 <-> 3,3 <-> 4,4 <-> 5,5 <-> 6,6 <-> 7,6 <-> 8,8 <-> 9]
limonene = fromList 10 [0 <-> 1,1 <-> 2,1 <-> 3,3 <-> 4,3 <-> 9,4 <-> 5,5 <-> 6,6 <-> 7,6 <-> 8,8 <-> 9]
pinene   = fromList 10 [0 <-> 1,0 <-> 3,0 <-> 6,1 <-> 2,1 <-> 8,1 <-> 9,2 <-> 3,2 <-> 4,4 <-> 5,5 <-> 6,6 <-> 7]


-- * Alkaloids
viagra,xanax,phentermine,valium,ambien,nexium,vioxx,paxil,lipitor,cialis,strychnine,cocaine,quinine,lsd,morphine,heroine,nicotine,vitamine_a,caffeine ∷ Graph
viagra        = fromList 32 [0 <-> 1,1 <-> 2,2 <-> 3,2 <-> 12,3 <-> 4,4 <-> 5,4 <-> 6,6 <-> 7,6 <-> 12,7 <-> 8,7 <-> 9,9 <-> 10,10 <-> 11,10 <-> 13,11 <-> 12,13 <-> 14,13 <-> 18,14 <-> 15,15 <-> 16,15 <-> 22,16 <-> 17,17 <-> 18,18 <-> 19,19 <-> 20,20 <-> 21,22 <-> 23,22 <-> 24,22 <-> 25,25 <-> 26,25 <-> 31,26 <-> 27,27 <-> 28,28 <-> 29,28 <-> 30,30 <-> 31]
xanax         = fromList 22 [0 <-> 1,1 <-> 2,1 <-> 21,2 <-> 3,3 <-> 4,4 <-> 5,4 <-> 21,5 <-> 6,6 <-> 7,7 <-> 8,7 <-> 14,8 <-> 9,8 <-> 13,9 <-> 10,10 <-> 11,11 <-> 12,12 <-> 13,14 <-> 15,14 <-> 20,15 <-> 16,16 <-> 17,16 <-> 18,18 <-> 19,19 <-> 20,20 <-> 21]
phentermine   = fromList 11 [0 <-> 1,1 <-> 2,1 <-> 3,1 <-> 4,4 <-> 5,5 <-> 6,5 <-> 10,6 <-> 7,7 <-> 8,8 <-> 9,9 <-> 10]
valium        = fromList 20 [0 <-> 1,1 <-> 2,1 <-> 19,2 <-> 3,2 <-> 4,4 <-> 5,5 <-> 6,6 <-> 7,6 <-> 13,7 <-> 8,7 <-> 12,8 <-> 9,9 <-> 10,10 <-> 11,11 <-> 12,13 <-> 14,13 <-> 19,14 <-> 15,15 <-> 16,15 <-> 17,17 <-> 18,18 <-> 19]
ambien        = fromList 23 [0 <-> 1,1 <-> 2,1 <-> 3,3 <-> 4,3 <-> 5,5 <-> 6,6 <-> 7,6 <-> 15,7 <-> 8,7 <-> 16,8 <-> 9,9 <-> 10,9 <-> 15,10 <-> 11,11 <-> 12,12 <-> 13,12 <-> 14,14 <-> 15,16 <-> 17,16 <-> 22,17 <-> 18,18 <-> 19,19 <-> 20,19 <-> 21,21 <-> 22]
nexium        = fromList 24 [0 <-> 1,1 <-> 2,2 <-> 3,2 <-> 10,3 <-> 4,4 <-> 5,5 <-> 6,5 <-> 9,6 <-> 7,7 <-> 8,7 <-> 11,8 <-> 9,9 <-> 10,11 <-> 12,11 <-> 13,13 <-> 14,14 <-> 15,14 <-> 22,15 <-> 16,16 <-> 17,17 <-> 18,17 <-> 19,19 <-> 20,19 <-> 22,20 <-> 21,22 <-> 23]
vioxx         = fromList 22 [0 <-> 1,1 <-> 2,1 <-> 3,1 <-> 4,4 <-> 5,4 <-> 9,5 <-> 6,6 <-> 7,7 <-> 8,7 <-> 10,8 <-> 9,10 <-> 11,10 <-> 15,11 <-> 12,11 <-> 16,12 <-> 13,12 <-> 14,14 <-> 15,16 <-> 17,16 <-> 21,17 <-> 18,18 <-> 19,19 <-> 20,20 <-> 21]
paxil         = fromList 24 [0 <-> 1,1 <-> 2,1 <-> 6,2 <-> 3,3 <-> 4,4 <-> 5,4 <-> 7,5 <-> 6,7 <-> 8,7 <-> 12,8 <-> 9,9 <-> 10,10 <-> 11,11 <-> 12,12 <-> 13,13 <-> 14,14 <-> 15,15 <-> 16,15 <-> 23,16 <-> 17,17 <-> 18,18 <-> 19,18 <-> 22,19 <-> 20,20 <-> 21,21 <-> 22,22 <-> 23]
lipitor       = fromList 40 [0 <-> 1,1 <-> 2,1 <-> 3,3 <-> 4,3 <-> 23,4 <-> 5,4 <-> 14,5 <-> 6,5 <-> 7,7 <-> 8,8 <-> 9,8 <-> 13,9 <-> 10,10 <-> 11,11 <-> 12,12 <-> 13,14 <-> 15,14 <-> 34,15 <-> 16,15 <-> 23,16 <-> 17,16 <-> 22,17 <-> 18,18 <-> 19,19 <-> 20,19 <-> 21,21 <-> 22,23 <-> 24,24 <-> 25,25 <-> 26,26 <-> 27,26 <-> 33,27 <-> 28,28 <-> 29,28 <-> 30,30 <-> 31,31 <-> 32,31 <-> 33,34 <-> 35,34 <-> 39,35 <-> 36,36 <-> 37,37 <-> 38,38 <-> 39]
cialis        = fromList 29 [0 <-> 1,1 <-> 2,1 <-> 18,2 <-> 3,3 <-> 4,3 <-> 5,5 <-> 6,5 <-> 17,6 <-> 7,6 <-> 20,7 <-> 8,7 <-> 15,8 <-> 9,9 <-> 10,9 <-> 14,10 <-> 11,11 <-> 12,12 <-> 13,13 <-> 14,14 <-> 15,15 <-> 16,16 <-> 17,17 <-> 18,18 <-> 19,20 <-> 21,20 <-> 28,21 <-> 22,22 <-> 23,23 <-> 24,23 <-> 27,24 <-> 25,25 <-> 26,26 <-> 27,27 <-> 28]
strychnine    = fromList 25 [0 <-> 1,1 <-> 2,1 <-> 18,2 <-> 3,3 <-> 4,3 <-> 16,4 <-> 5,5 <-> 6,6 <-> 7,7 <-> 8,7 <-> 15,8 <-> 9,9 <-> 10,9 <-> 13,10 <-> 11,11 <-> 12,12 <-> 13,12 <-> 17,12 <-> 24,13 <-> 14,14 <-> 15,15 <-> 16,16 <-> 17,17 <-> 18,18 <-> 19,19 <-> 20,19 <-> 24,20 <-> 21,21 <-> 22,22 <-> 23,23 <-> 24]
cocaine       = fromList 22 [0 <-> 1,1 <-> 2,2 <-> 3,2 <-> 4,4 <-> 5,4 <-> 10,5 <-> 6,5 <-> 20,6 <-> 7,7 <-> 8,8 <-> 9,8 <-> 20,9 <-> 10,10 <-> 11,11 <-> 12,12 <-> 13,12 <-> 14,14 <-> 15,14 <-> 19,15 <-> 16,16 <-> 17,17 <-> 18,18 <-> 19,20 <-> 21]
quinine       = fromList 24 [0 <-> 1,1 <-> 2,2 <-> 3,2 <-> 23,3 <-> 4,4 <-> 5,5 <-> 6,5 <-> 22,6 <-> 7,7 <-> 8,8 <-> 9,9 <-> 10,9 <-> 22,10 <-> 11,10 <-> 12,12 <-> 13,12 <-> 17,13 <-> 14,14 <-> 15,14 <-> 19,15 <-> 16,16 <-> 17,17 <-> 18,18 <-> 19,19 <-> 20,20 <-> 21,22 <-> 23]
lsd           = fromList 24 [0 <-> 1,1 <-> 2,2 <-> 3,2 <-> 5,3 <-> 4,5 <-> 6,5 <-> 7,7 <-> 8,7 <-> 22,8 <-> 9,9 <-> 10,9 <-> 11,11 <-> 12,11 <-> 21,12 <-> 13,13 <-> 14,13 <-> 23,14 <-> 15,15 <-> 16,16 <-> 17,16 <-> 23,17 <-> 18,18 <-> 19,19 <-> 20,20 <-> 21,20 <-> 23,21 <-> 22]
morphine      = fromList 21 [0 <-> 1,1 <-> 2,1 <-> 11,2 <-> 3,3 <-> 4,4 <-> 5,4 <-> 8,4 <-> 12,5 <-> 6,5 <-> 15,6 <-> 7,7 <-> 8,7 <-> 19,8 <-> 9,9 <-> 10,9 <-> 17,10 <-> 11,11 <-> 12,12 <-> 13,13 <-> 14,14 <-> 15,15 <-> 16,17 <-> 18,18 <-> 19,19 <-> 20]
heroine       = fromList 27 [0 <-> 1,1 <-> 2,1 <-> 11,2 <-> 3,3 <-> 4,4 <-> 5,4 <-> 8,4 <-> 12,5 <-> 6,5 <-> 15,6 <-> 7,7 <-> 8,7 <-> 22,8 <-> 9,9 <-> 10,9 <-> 20,10 <-> 11,11 <-> 12,12 <-> 13,13 <-> 14,14 <-> 15,15 <-> 16,16 <-> 17,17 <-> 18,17 <-> 19,20 <-> 21,21 <-> 22,22 <-> 23,23 <-> 24,24 <-> 25,24 <-> 26]
nicotine      = fromList 12 [0 <-> 1,1 <-> 2,1 <-> 5,2 <-> 3,3 <-> 4,4 <-> 5,5 <-> 6,6 <-> 7,6 <-> 11,7 <-> 8,8 <-> 9,9 <-> 10,10 <-> 11]
caffeine      = fromList 14 [0 <-> 1,1 <-> 2,1 <-> 13,2 <-> 3,3 <-> 4,4 <-> 5,4 <-> 13,5 <-> 6,5 <-> 7,7 <-> 8,7 <-> 9,9 <-> 10,9 <-> 11,11 <-> 12,11 <-> 13]
vitamine_a    = fromList 21 [0 <-> 1,1 <-> 2,1 <-> 5,2 <-> 3,3 <-> 4,5 <-> 6,6 <-> 7,7 <-> 8,8 <-> 9,8 <-> 10,10 <-> 11,11 <-> 12,12 <-> 13,12 <-> 18,13 <-> 14,13 <-> 15,15 <-> 16,16 <-> 17,17 <-> 18,18 <-> 19,18 <-> 20]
