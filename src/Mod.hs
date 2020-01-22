module Mod (Setting(..), Mod(..)) where

data Setting = Float Float
             | Bool Bool
             | String String
             | Int Int
             | List [Setting]
             deriving (Eq, Show)

data Mod = Mod
  { modName :: String
  , modSettings :: [(String, Setting)]
  , enabled :: Bool
  } deriving (Eq, Show)

