{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Diagrams.RubiksCube
import qualified Diagrams.RubiksCube.Move as RubiksCube.Move
import qualified Diagrams.RubiksCube.Model

import Control.Lens hiding ((#))
import qualified Diagrams.Prelude

import qualified Diagrams.Backend.SVG

main :: IO ()
main = do
  let 
      c   = Diagrams.RubiksCube.solvedRubiksCube ^. Diagrams.RubiksCube.Model.undoMoves [RubiksCube.Move.R, RubiksCube.Move.U, RubiksCube.Move.R', RubiksCube.Move.U']
      settings :: Diagrams.RubiksCube.MovesSettings Double
      settings = (Diagrams.Prelude.with & Diagrams.RubiksCube.showStart .~ True)
      d :: Diagrams.Prelude.Diagram (Diagrams.Backend.SVG.B)
      -- d   = drawMoves settings solvedRubiksCube []
      d   = Diagrams.RubiksCube.drawRubiksCube Diagrams.Prelude.def Diagrams.RubiksCube.solvedRubiksCube
  Diagrams.Backend.SVG.renderSVG "hoge.svg" (Diagrams.Prelude.mkWidth 250) d
  return ()