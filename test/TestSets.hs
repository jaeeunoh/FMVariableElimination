module TestSets where

import Types
import Util
import qualified Data.Map as Map
import Data.Ratio

startSet = [
      ReducibleInequality (Map.fromList [('x', 1), ('y', 2)]) 0 GreaterEqual -- x + 2y > 0
    , ReducibleInequality (Map.fromList [('x', 1), ('y', -2)]) 10 LessEqual  -- x - 2y + 10 < 0
    ]

--orig
-- x > -2y
-- x < 2y - 10

-- 2y - 10 > -2y
-- 4y - 10 > 0

endSet = [ReducibleInequality (Map.fromList [('y', 4)]) (-10) GreaterEqual]     -- 4y - 10 < 0

----------------


-- x - 5y + 2z >= 7 
-- 3x - 2y - 6z >= -12 
-- -2x + 5y -4z >= -10 
-- -3x + 6y -3z >= -9 

startSet2 = [
      ReducibleInequality (Map.fromList [('x', 1), ('y', -5), ('z', 2)]) (-7) GreaterEqual
    , ReducibleInequality (Map.fromList [('x', 3), ('y', -2), ('z', -6)]) 12 GreaterEqual
    , ReducibleInequality (Map.fromList [('x', -2), ('y', 5), ('z', -4)]) 10 GreaterEqual
    , ReducibleInequality (Map.fromList [('x', -3), ('y', 6), ('z', -3)]) 9 GreaterEqual
    -- ReducibleInequality (Map.fromList [('y', -10), ('z', 3)]) 15 GreaterEqual
    ]

-- 5/2 y - 2z + 5 >= 5y - 2z + 7 
-- 5/2 y -2z + 5 >= 2/3y + 2z -4 
-- 2y - z + 3 >= 5y - 2z + 7
-- 2y - z + 3 >= 2/3y + 2z -4 

-- -5/2y +0z -2 >= 0
-- 11/6y -4z + 9 >= 0
-- -3y +z - 4 >= 0
-- 4/3y -3z + 7 >= 0


endSet2 = [
     ReducibleInequality (Map.fromList [('y', -5 % 2), ('z', 0)]) (-2) GreaterEqual
    ,ReducibleInequality (Map.fromList [('y', 11 % 6), ('z', -4)]) (9) GreaterEqual
    ,ReducibleInequality (Map.fromList [('y', -3), ('z', 1)]) (-4) GreaterEqual
    ,ReducibleInequality (Map.fromList [('y', 4 % 3), ('z', -3)]) 7 GreaterEqual
    ]


-- -5y +0z -4 >= 0
-- 11y -24z + 54 >= 0
-- -3y +z - 4 >= 0
-- 4y -9z + 21 >= 0

-- -y +0z -4/5 >= 0
-- y -24/11z + 54/11 >= 0
-- -y +z/3 - 4/3 >= 0
-- y -9/4z + 21/4 >= 0

-- y <= 0z - 4/5
-- y >= 24/11z - 54/11
-- y <= +z/3 - 4/3
-- y >= 9/4z - 21/4

-- 24/11z - 54/11 <= 0z - 4/5
-- 24/11z - 54/11 <= +z/3 - 4/3
-- 9/4z - 21/4 <= 0z - 4/5
-- 9/4z - 21/4 <= +z/3 - 4/3

-- -24/11z + 226/55 >= 0
-- -61/33z + 118/33 >= 0
-- -9/4z + 89/20 >= 0
-- -23/12z + 47/12 >= 0

endEndSet2 = [
    ReducibleInequality (Map.fromList [('z', (-24) % 11)]) (226 % 55) GreaterEqual
    ,ReducibleInequality (Map.fromList [('z', (-9) % 4)]) (89 % 20) GreaterEqual
    ,ReducibleInequality (Map.fromList [('z', (-61) % 33)]) (118 % 33) GreaterEqual    
    ,ReducibleInequality (Map.fromList [('z', (-23) % 12)]) (47 % 12) GreaterEqual
    ]


----------------

startSet3 = [
     prs "1y <= 15"
    ,prs "1y >= 1x -10"
    ,prs "1y >= 20 -1x"
    ]

endSet3 = [
     prs "-1x +25 >= 0"
    ,prs "1x -5 >= 0"
    ]

-----
-- A failing set

startSet4 = [
     prs "1y < 10"
    ,prs "2x < 2y"
    ,prs "1x >= 15"
    ]