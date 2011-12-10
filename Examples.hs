module Examples where
import Triangulation
import TriangulationCxtObject
import StandardCoordinates
import Data.TrieSet as Set
import FaceLattice
import ParseJvx


-- | Figure 1 in "Normal surfaces in topologically finite 3-manifolds (Tillmann)"
fig1 ::  Triangulation
fig1 = triang [( 0 ./ tABD,  0 ./ oBCD)]

eightComplement = triang [ ( 0 ./ tABC, 1 ./ oDBC) 
                         , ( 0 ./ tABD, 1 ./ oACD)
                         , ( 0 ./ tACD, 1 ./ oACB)
                         , ( 0 ./ tBCD, 1 ./ oBAD)
                         ]

tt2 ::  Triangulation
tt2 = triang [( 0 ./ tACD,  1 ./ oCBD)]

tt3 ::  Triangulation
tt3 = triang [( 1 ./ tABD,  1 ./ oDBC)] 

standard3Sphere ::  Triangulation
standard3Sphere = convertToTriangulation [(1,2,3,4),(0,2,3,4),(0,1,3,4),(0,1,2,4),(0,1,2,3)]

-- standard3Sphere = fromRight $ mkTriangulationG tets gluings
--     where
--       tets = subsetsSized 4 (Set.fromList [1::Int .. 5])  
-- 
-- 
--       gluings = [ (tet,tri,tet',g,tri') |
-- 
--                     tet <- tets,
--                     tet' <- tets,
--                     tet /= tet',
--                     tri <- allTriangles,
--                     let verts = [ Set.elemAt (fromEnum v) tet | v <- vertexList tri ],
--                     g <- allS3,
--                     tri' <- allTriangles,
--                     let verts' = [ Set.elemAt (fromEnum v) tet' | v <- vertexList (packOrderedFace g tri') ],
--                     Set.fromList verts == Set.fromList verts'
--                 ]



tt10 = triang [ ( 0 ./ tABD,  0 ./ oBCD) , 
                ( 0 ./ tABC,  0 ./ oACD) ]

tt11 = triang [ ( 0 ./ tABD,  0 ./ oBCD) , 
                ( 0 ./ tABC,  0 ./ oDAC) ]



phs = fromRight $ mkTriangulation [0..89] 
      [
        ( 1 ./ tABC,  0 ./ oABC)
      , ( 3 ./ tABC,  2 ./ oABC)
      , ( 4 ./ tABC,  0 ./ oABD)
      , ( 4 ./ tABD,  2 ./ oABD)
      , ( 3 ./ tABD,  1 ./ oABD)
      , ( 6 ./ tABC,  5 ./ oABC)
      , ( 8 ./ tABC,  7 ./ oABC)
      , ( 9 ./ tABC,  7 ./ oABD)
      , ( 8 ./ tABD,  5 ./ oABD)
      , ( 9 ./ tABD,  6 ./ oABD)
      , (10 ./ tABC,  0 ./ oACD)
      , (10 ./ tABD,  5 ./ oACD)
      , ( 6 ./ tACD,  1 ./ oACD)
      , (12 ./ tABC, 11 ./ oABC)
      , (14 ./ tABC, 13 ./ oABC)
      , (15 ./ tABC, 13 ./ oABD)
      , (14 ./ tABD, 11 ./ oABD)
      , (15 ./ tABD, 12 ./ oABD)
      , (16 ./ tABC, 11 ./ oACD)
      , (12 ./ tACD,  2 ./ oACD)
      , (16 ./ tABD,  3 ./ oACD)
      , (18 ./ tABC, 17 ./ oABC)
      , (17 ./ tABD,  7 ./ oACD)
      , (19 ./ tABC, 18 ./ oABD)
      , (19 ./ tABD,  8 ./ oACD)
      , (20 ./ tABC, 17 ./ oACD)
      , (18 ./ tACD, 13 ./ oACD)
      , (20 ./ tABD, 14 ./ oACD)
      , (22 ./ tABC, 21 ./ oABC)
      , (21 ./ tABD, 10 ./ oACD)
      , (22 ./ tABD,  4 ./ oACD)
      , (23 ./ tABC, 20 ./ oACD)
      , (23 ./ tABD,  9 ./ oACD)
      , (21 ./ tACD, 19 ./ oACD)
      , (22 ./ tACD, 15 ./ oACD)
      , (23 ./ tACD, 16 ./ oACD)
      , (25 ./ tABC, 24 ./ oABC)
      , (27 ./ tABC, 26 ./ oABC)
      , (26 ./ tABD, 24 ./ oABD)
      , (28 ./ tABC, 25 ./ oABD)
      , (28 ./ tABD, 27 ./ oABD)
      , (29 ./ tABC,  0 ./ oBCD)
      , (31 ./ tABC, 30 ./ oABC)
      , (30 ./ tABD, 29 ./ oABD)
      , (31 ./ tABD,  1 ./ oBCD)
      , (33 ./ tABC, 32 ./ oABC)
      , (34 ./ tABC, 24 ./ oACD)
      , (32 ./ tABD, 25 ./ oACD)
      , (34 ./ tABD, 33 ./ oABD)
      , (36 ./ tABC, 35 ./ oABC)
      , (37 ./ tABC, 35 ./ oABD)
      , (36 ./ tABD,  2 ./ oBCD)
      , (37 ./ tABD,  3 ./ oBCD)
      , (39 ./ tABC, 38 ./ oABC)
      , (40 ./ tABC, 26 ./ oACD)
      , (38 ./ tABD, 27 ./ oACD)
      , (40 ./ tABD, 39 ./ oABD)
      , (41 ./ tABC, 32 ./ oACD)
      , (42 ./ tABC, 33 ./ oACD)
      , (42 ./ tABD, 41 ./ oABD)
      , (38 ./ tACD, 29 ./ oACD)
      , (39 ./ tACD,  4 ./ oBCD)
      , (35 ./ tACD, 34 ./ oACD)
      , (40 ./ tACD, 36 ./ oACD)
      , (30 ./ tACD, 28 ./ oACD)
      , (41 ./ tACD, 31 ./ oACD)
      , (42 ./ tACD, 37 ./ oACD)
      , (44 ./ tABC, 43 ./ oABC)
      , (45 ./ tABC,  5 ./ oBCD)
      , (45 ./ tABD, 43 ./ oABD)
      , (44 ./ tABD,  6 ./ oBCD)
      , (46 ./ tABC, 24 ./ oBCD)
      , (47 ./ tABC, 25 ./ oBCD)
      , (47 ./ tABD, 43 ./ oACD)
      , (46 ./ tABD, 44 ./ oACD)
      , (26 ./ tBCD,  7 ./ oBCD)
      , (48 ./ tABC,  8 ./ oBCD)
      , (48 ./ tABD, 27 ./ oBCD)
      , (46 ./ tACD,  9 ./ oBCD)
      , (49 ./ tABC, 28 ./ oBCD)
      , (49 ./ tABD, 47 ./ oACD)
      , (50 ./ tABC, 48 ./ oACD)
      , (50 ./ tABD, 45 ./ oACD)
      , (50 ./ tACD, 49 ./ oACD)
      , (52 ./ tABC, 51 ./ oABC)
      , (53 ./ tABC, 51 ./ oABD)
      , (52 ./ tABD, 43 ./ oBCD)
      , (53 ./ tABD, 44 ./ oBCD)
      , (54 ./ tABC, 51 ./ oACD)
      , (56 ./ tABC, 55 ./ oABC)
      , (55 ./ tABD, 54 ./ oABD)
      , (56 ./ tABD, 52 ./ oACD)
      , (57 ./ tABC, 54 ./ oACD)
      , (57 ./ tABD, 53 ./ oACD)
      , (59 ./ tABC, 58 ./ oABC)
      , (61 ./ tABC, 60 ./ oABC)
      , (62 ./ tABC, 58 ./ oABD)
      , (60 ./ tABD, 59 ./ oABD)
      , (62 ./ tABD, 61 ./ oABD)
      , (58 ./ tACD, 10 ./ oBCD)
      , (59 ./ tACD, 29 ./ oBCD)
      , (63 ./ tABC, 55 ./ oACD)
      , (63 ./ tABD, 60 ./ oACD)
      , (61 ./ tACD, 56 ./ oACD)
      , (63 ./ tACD, 30 ./ oBCD)
      , (57 ./ tACD, 31 ./ oBCD)
      , (62 ./ tACD, 45 ./ oBCD)
      , (64 ./ tABC, 51 ./ oBCD)
      , (64 ./ tABD, 11 ./ oBCD)
      , (52 ./ tBCD, 12 ./ oBCD)
      , (66 ./ tABC, 65 ./ oABC)
      , (65 ./ tABD, 64 ./ oACD)
      , (66 ./ tABD, 53 ./ oBCD)
      , (68 ./ tABC, 67 ./ oABC)
      , (32 ./ tBCD, 13 ./ oBCD)
      , (67 ./ tABD, 33 ./ oBCD)
      , (68 ./ tABD, 14 ./ oBCD)
      , (70 ./ tABC, 69 ./ oABC)
      , (69 ./ tABD, 67 ./ oACD)
      , (68 ./ tACD, 65 ./ oACD)
      , (70 ./ tABD, 66 ./ oACD)
      , (69 ./ tACD, 34 ./ oBCD)
      , (70 ./ tACD, 46 ./ oBCD)
      , (47 ./ tBCD, 15 ./ oBCD)
      , (71 ./ tABC, 54 ./ oBCD)
      , (72 ./ tABC, 71 ./ oABD)
      , (72 ./ tABD, 64 ./ oBCD)
      , (73 ./ tABC, 55 ./ oBCD)
      , (73 ./ tABD, 35 ./ oBCD)
      , (56 ./ tBCD, 36 ./ oBCD)
      , (73 ./ tACD, 71 ./ oACD)
      , (74 ./ tABC, 72 ./ oACD)
      , (74 ./ tABD, 37 ./ oBCD)
      , (74 ./ tACD, 16 ./ oBCD)
      , (75 ./ tABC, 17 ./ oBCD)
      , (76 ./ tABC, 18 ./ oBCD)
      , (77 ./ tABC, 75 ./ oABD)
      , (77 ./ tABD, 76 ./ oABD)
      , (65 ./ tBCD, 38 ./ oBCD)
      , (78 ./ tABC, 39 ./ oBCD)
      , (78 ./ tABD, 66 ./ oBCD)
      , (75 ./ tACD, 40 ./ oBCD)
      , (71 ./ tBCD, 19 ./ oBCD)
      , (76 ./ tACD, 57 ./ oBCD)
      , (72 ./ tBCD, 48 ./ oBCD)
      , (78 ./ tACD, 77 ./ oACD)
      , (67 ./ tBCD, 58 ./ oBCD)
      , (68 ./ tBCD, 59 ./ oBCD)
      , (60 ./ tBCD, 20 ./ oBCD)
      , (75 ./ tBCD, 61 ./ oBCD)
      , (76 ./ tBCD, 41 ./ oBCD)
      , (79 ./ tABC, 62 ./ oBCD)
      , (79 ./ tABD, 42 ./ oBCD)
      , (79 ./ tACD, 77 ./ oBCD)
      , (81 ./ tABC, 80 ./ oABC)
      , (80 ./ tABD, 69 ./ oBCD)
      , (82 ./ tABC, 70 ./ oBCD)
      , (82 ./ tABD, 81 ./ oABD)
      , (80 ./ tACD, 21 ./ oBCD)
      , (83 ./ tABC, 22 ./ oBCD)
      , (83 ./ tABD, 81 ./ oACD)
      , (84 ./ tABC, 78 ./ oBCD)
      , (84 ./ tABD, 83 ./ oACD)
      , (84 ./ tACD, 82 ./ oACD)
      , (80 ./ tBCD, 73 ./ oBCD)
      , (85 ./ tABC, 63 ./ oBCD)
      , (85 ./ tABD, 81 ./ oBCD)
      , (86 ./ tABC, 23 ./ oBCD)
      , (86 ./ tABD, 85 ./ oACD)
      , (86 ./ tACD, 82 ./ oBCD)
      , (87 ./ tABC, 49 ./ oBCD)
      , (87 ./ tABD, 85 ./ oBCD)
      , (87 ./ tACD, 83 ./ oBCD)
      , (88 ./ tABC, 50 ./ oBCD)
      , (88 ./ tABD, 74 ./ oBCD)
      , (88 ./ tACD, 79 ./ oBCD)
      , (89 ./ tABC, 88 ./ oBCD)
      , (89 ./ tABD, 87 ./ oBCD)
      , (89 ./ tACD, 86 ./ oBCD)
      , (89 ./ tBCD, 84 ./ oBCD)
      ]
