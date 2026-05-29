{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (forM_)
import Data.List (last)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy qualified as Ez
import Optics
import Text.Printf (printf)

type NetWeights = ((Double, Double), (Double, Double))

dataset :: [(Double, Double)]
dataset = [(-2.0, 2.0), (-1.0, 1.0), (0.0, 0.0), (1.0, 1.0), (2.0, 2.0)]

linearNeuron :: ParaLens Double Double Double Double Double Double
linearNeuron = MkLens (\w x -> w * x) (\w x r -> (r * w, r * x))

relu :: Lens Double Double Double Double
relu = nonPara (\x -> if x > 0 then x else 0) (\x r -> if x > 0 then r else 0)

copy :: Lens Double Double (Double, Double) (Double, Double)
copy = nonPara (\x -> (x, x)) (\_ (r1, r2) -> r1 + r2)

sumNeuron :: ParaLens (Double, Double) (Double, Double) (Double, Double) (Double, Double) Double Double
sumNeuron =
  MkLens
    (\(w1, w2) (x1, x2) -> w1 * x1 + w2 * x2)
    (\(w1, w2) (x1, x2) r -> ((r * w1, r * w2), (r * x1, r * x2)))

-- Input -> Copy -> (Neuron x Neuron) -> (ReLU x ReLU) -> Sum
network :: ParaLens NetWeights NetWeights Double Double Double Double
network =
  ( ( (copy >->> (linearNeuron #### linearNeuron))
        >>-> (relu #--# relu)
    )
      >>>> sumNeuron
  )

--------------------------------------------------------------------------------
-- LEARNING

-- "Stochastic" Gradient Descent
sgdAdapter :: Double -> Lens NetWeights NetWeights NetWeights NetWeights
sgdAdapter lr =
  nonPara
    id
    ( \((w1, w2), (w3, w4)) ((q1, q2), (q3, q4)) ->
        ( (w1 - lr * q1, w2 - lr * q2),
          (w3 - lr * q3, w4 - lr * q4)
        )
    )

-- Momentum
type MomentumState = NetWeights

momentumAdapter :: Double -> Double -> Lens (NetWeights, MomentumState) (NetWeights, MomentumState) NetWeights NetWeights
momentumAdapter lr gamma =
  nonPara
    (\(w, _v) -> w)
    ( \(((w1, w2), (w3, w4)), ((v1, v2), (v3, v4))) ((g1, g2), (g3, g4)) ->
        let v1' = gamma * v1 + lr * g1
            v2' = gamma * v2 + lr * g2
            v3' = gamma * v3 + lr * g3
            v4' = gamma * v4 + lr * g4
            w1' = w1 - v1'
            w2' = w2 - v2'
            w3' = w3 - v3'
            w4' = w4 - v4'
         in (((w1', w2'), (w3', w4')), ((v1', v2'), (v3', v4')))
    )

--------------------------------------------------------------------------------

trainableNetwork :: Double -> ParaLens NetWeights NetWeights Double Double Double Double
trainableNetwork lr = sgdAdapter lr *** network

trainableNetworkWithMomentum :: Double -> Double -> ParaLens (NetWeights, MomentumState) (NetWeights, MomentumState) Double Double Double Double
trainableNetworkWithMomentum lr gamma = momentumAdapter lr gamma *** network

--------------------------------------------------------------------------------

trainStep :: ParaLens NetWeights NetWeights Double Double Double Double -> NetWeights -> (Double, Double) -> NetWeights
trainStep (MkLens play coplay) weights (x, target) =
  let y = play weights x
      lossGrad = y - target
      (_, updatedWeights) = coplay weights x lossGrad
   in updatedWeights

trainHistory :: Int -> ParaLens NetWeights NetWeights Double Double Double Double -> NetWeights -> [(Double, Double)] -> [NetWeights]
trainHistory 0 _ w _ = [w]
trainHistory epochs model w ds =
  let w' = foldl (trainStep model) w ds
   in w : trainHistory (epochs - 1) model w' ds

predict :: NetWeights -> Double -> Double
predict weights x = let (MkLens play _) = network in play weights x

--------------------------------------------------------------------------------

-- VISUALIZZAZIONE
xRangeDense :: [Double]
xRangeDense = [-5, -4.9999 .. 5]

renderSnapshot :: Int -> NetWeights -> IO ()
renderSnapshot epoch weights = do
  let fileName = printf "snapshot_epoch_%03d.png" epoch

  toFile (Ez.def {_fo_size = (1200, 800)}) fileName $ do
    Ez.layout_title Ez..= printf "Epoch %d: Learning y = |x| with Optics.hs" epoch

    Ez.plot $
      Ez.line "Target (abs x)" [[(x, abs x) | x <- xRangeDense]]
        >>= return . (Ez.plot_lines_style . Ez.line_width Ez..~ 4)
        >>= return . (Ez.plot_lines_style . Ez.line_color Ez..~ Ez.opaque Ez.lightgrey)

    Ez.plot $
      Ez.line "Learned (ParaLens)" [[(x, predict weights x) | x <- xRangeDense]]
        >>= return . (Ez.plot_lines_style . Ez.line_width Ez..~ 2)
        >>= return . (Ez.plot_lines_style . Ez.line_color Ez..~ Ez.opaque Ez.blue)

    Ez.plot $ Ez.points "Training Data" dataset

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let initialWeights = ((0.6, -0.2), (0.3, 0.7))
  let totalEpochs = 120
  let snapshotInterval = 10

  let history = trainHistory totalEpochs (trainableNetwork 0.005) initialWeights dataset

  forM_ (zip [0 ..] history) $ \(e, weights@((w1, w2), (w3, w4))) -> do
    printf "Epoch %3d | w1:%-10.6f w2:%-10.6f | w3:%-10.6f w4:%-10.6f\n" e w1 w2 w3 w4

    -- Print PNGs
    if e `mod` snapshotInterval == 0
      then renderSnapshot e weights
      else return ()

  let finalW = last history
  putStrLn "---------------------------------------------------------------------"
  putStrLn $ "Final weights: " ++ show finalW
  putStrLn $ "Final prediction for x=-5.0: " ++ show (predict finalW (-5.0))
