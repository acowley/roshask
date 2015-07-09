{-# LANGUAGE OverloadedStrings #-}
-- |Generate a Lens instances for ROS msg types.
module Instances.Lens (lensImport, lensInstance) where
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as B
import Types

lensImport :: ByteString
lensImport = B.concat ["import Lens.Family.TH (makeLenses)\n",
                       "import Lens.Family (view, set)\n"]

lensInstance :: String -> ByteString
lensInstance name = B.concat ["$(makeLenses ''", pack name, ")"]

