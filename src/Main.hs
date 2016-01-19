module Main where

import Linear                 ( V3(..)
                              )
import Vis                    ( VisObject(..)
                              , Flavour(..)
                              , makeColor
                              , playIO
                              , defaultOpts
                              , optWindowName
                              )
import Control.Concurrent.STM ( atomically
                              )
import qualified MicChan as MC 

data World = World { time :: Float
                   , mic :: MC.Mic
                   , r :: AvgAmplitude
                   }
type AvgAmplitude = Double

sampleTime :: Double
sampleTime = 0.2

amplificationFactor :: Double
amplificationFactor = 800

main :: IO ()
main = do
  initialWorld <- initWorld
  let m = mic initialWorld
  playIO
    (defaultOpts {optWindowName = "zkouška animace co reaguje na zvuk"})
    sampleTime
    initialWorld
    drawFun
    updateWorld
    (const $ return ())
    Nothing
    Nothing
    Nothing
  MC.closeMic m

initWorld :: IO World
initWorld = do
  m <- MC.openMic
  return $ World { time = 0
                 , mic = m
                 , r = 0
                 }

updateWorld :: Float -> World -> IO World
updateWorld _ w = do
  dta <- atomically (MC.readSoundData $ mic w)
  return w { r = processSoundData dta
           }
  where
    processSoundData (MC.SD []) = 0.0
    processSoundData (MC.SD s) = let d = fmap ((*amplificationFactor) . abs) s
                                 in min 1 $ sum d / fromIntegral (length d)

drawFun w = return (VisObjects [plane, sphere], Nothing)
  where
    sphere = Trans (V3 0 0 (-1)) $ Sphere (r w) Solid (makeColor 1 0 0 1)
    plane = Plane (V3 0 0 1) (makeColor 1 1 1 1) (makeColor 0.4 0.6 0.65 0.4)
