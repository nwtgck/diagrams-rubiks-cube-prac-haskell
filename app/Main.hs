-- {-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

-- module Main where

-- import Diagrams.RubiksCube
-- import Control.Lens
-- import Diagrams.RubiksCube.Move (Move (..))
-- import Diagrams.RubiksCube.Model

-- import Control.Lens hiding ((#))
-- import Diagrams.Prelude hiding (center, cube)
-- import Diagrams.TwoD.Arrow (arrowFromLocatedTrail')
-- import Diagrams.Trail (trailPoints)
-- import qualified Diagrams.Prelude as P

-- import Data.Function (on)
-- import Data.List (sortBy, mapAccumL)
-- import Data.Typeable (Typeable)

-- drawMovesExample' =
--   let moves = [B, R, F', R', D', F, F]
--       endPos = solvedRubiksCube
--       settings = with & showStart .~ True
--   in drawMovesBackward settings endPos moves

-- main :: IO ()
-- main = do
--   -- let 
--       -- c   = solvedRubiksCube ^. undoMoves [R,U,R',U']
--   -- print $ drawMoves (with & showStart .~ True) solvedRubiksCube []
--   return ()

-- (from: https://archives.haskell.org/projects.haskell.org/diagrams/doc/quickstart.html)
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

myCircle :: Diagram B
myCircle = circle 1

main = mainWith myCircle
-- stack build && stack exec diagrams-rubiks-cube-prac-exe -- -o circle.svg -w 400