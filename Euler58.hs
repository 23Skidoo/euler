module Main
       where

import Common (isPrime)

data SpiralState = SpiralState {
  latestVal  :: !Int,
  sideLength :: !Int,
  numPrimes  :: !Int,
  numDiags   :: !Int
  }
  deriving (Show, Eq)

initialState :: SpiralState
initialState = SpiralState {
  latestVal  = 1,
  sideLength = 1,
  numPrimes  = 0,
  numDiags   = 1
}

wrapSpiral :: SpiralState -> SpiralState
wrapSpiral state =
  let sideLength' = sideLength state + 2
      step        = sideLength' - 1
      cur         = latestVal state
      new         = cur + step * 4
      diags       = [cur + step*i | i <- [1..4]]
      -- NB: This requires an approx 1GB large sieve. See the Common module.
      numPrimes'  = length . filter isPrime $ diags
  in SpiralState {
    latestVal  = new,
    sideLength = sideLength',
    numPrimes  = numPrimes state + numPrimes',
    numDiags   = numDiags state  + length diags
    }

main :: IO ()
main = print . sideLength . go $ initialState
  where
    go state =
      let state' = wrapSpiral state
          np     = numPrimes state'
          nd     = numDiags  state'
          ratio = (fromIntegral np / fromIntegral nd) :: Double
      in if ratio < 0.1 then state' else go state'
