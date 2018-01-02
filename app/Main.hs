{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Diagrams.RubiksCube
import Control.Lens
import Diagrams.RubiksCube.Move (Move (..))
import Diagrams.RubiksCube.Model

import Control.Lens hiding ((#))
import Diagrams.Prelude hiding (center, cube)
import Diagrams.TwoD.Arrow (arrowFromLocatedTrail')
import Diagrams.Trail (trailPoints)
import qualified Diagrams.Prelude as P

import Data.Function (on)
import Data.List (sortBy, mapAccumL)
import Data.Typeable (Typeable)

import qualified Diagrams.Backend.SVG as Backend.SVG
-- import qualified Diagrams.Backend.SVG.CmdLine as SVG.CmdLine
-- import Diagrams.Backend.Cairo

-- drawMovesExample' =
--   let moves = [B, R, F', R', D', F, F]
--       endPos = solvedRubiksCube
--       settings = with & showStart .~ True
--   in drawMovesBackward settings endPos moves

-- stack build && stack exec diagrams-rubiks-cube-prac-exe
main :: IO ()
main = do
  let 
      c   = solvedRubiksCube ^. undoMoves [R,U,R',U']
      settings :: MovesSettings Double
      settings = (with & showStart .~ True) 
      d :: Diagram (Backend.SVG.B)
      -- d   = drawMoves settings solvedRubiksCube []
      d   = drawRubiksCube def solvedRubiksCube
  -- renderDia Backend.SVG.SVG (CairoOptions "foo.svg" (Width 250) Backend.SVG.SVG False) (d )
  Backend.SVG.renderSVG "hoge.svg" (mkWidth 250) d
  -- SVG.CmdLine.mainWith d
  return ()

-- -- (from: https://archives.haskell.org/projects.haskell.org/diagrams/doc/quickstart.html)
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE FlexibleContexts          #-}
-- {-# LANGUAGE TypeFamilies              #-}

-- import Diagrams.Prelude
-- import qualified Diagrams.Backend.SVG.CmdLine as SVG.CmdLine

-- myCircle :: Diagram (SVG.CmdLine.B)
-- myCircle = circle 1

-- main = SVG.CmdLine.mainWith myCircle
-- -- stack build && stack exec diagrams-rubiks-cube-prac-exe -- -o circle.svg -w 400