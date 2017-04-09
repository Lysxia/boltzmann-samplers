module Boltzmann.Options where

data Options = Options
  { averageSize :: Maybe Double
  , points :: Int
  } deriving (Eq, Show)

sizedOptions :: Double -> Options
sizedOptions size = Options (Just size) 0

singularOptions :: Options
singularOptions = Options Nothing 0
