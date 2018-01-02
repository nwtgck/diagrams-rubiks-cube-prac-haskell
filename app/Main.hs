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

main :: IO ()
main = do
  let 
      c   = solvedRubiksCube ^. undoMoves [R,U,R',U']
      settings :: MovesSettings Double
      settings = (with & showStart .~ True) 
      d :: Diagram (Backend.SVG.B)
      -- d   = drawMoves settings solvedRubiksCube []
      d   = drawRubiksCube def solvedRubiksCube
  Backend.SVG.renderSVG "hoge.svg" (mkWidth 250) d
  return ()