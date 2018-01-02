{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Diagrams.RubiksCube
import qualified Diagrams.RubiksCube.Move as RubiksCube.Move
import qualified Diagrams.RubiksCube.Model
import qualified Data.Colour.RGBSpace

import Control.Lens hiding ((#))
import qualified Diagrams.Prelude

import qualified Diagrams.Backend.SVG

main :: IO ()
main = do
  let cube :: Diagrams.RubiksCube.Model.RubiksCube (Data.Colour.RGBSpace.Colour Double)
      cube    = Diagrams.RubiksCube.solvedRubiksCube ^. Diagrams.RubiksCube.Model.undoMoves [RubiksCube.Move.R, RubiksCube.Move.U, RubiksCube.Move.R', RubiksCube.Move.U']
      settings :: Diagrams.RubiksCube.MovesSettings Double
      settings = (Diagrams.Prelude.with & Diagrams.RubiksCube.showStart .~ True)
      d :: Diagrams.Prelude.Diagram (Diagrams.Backend.SVG.B)
      -- d   = drawMoves settings solvedRubiksCube []
      d   = Diagrams.RubiksCube.drawRubiksCube Diagrams.Prelude.def cube
  Diagrams.Backend.SVG.renderSVG "demo_images/demo1.svg" (Diagrams.Prelude.mkWidth 250) d
  return ()