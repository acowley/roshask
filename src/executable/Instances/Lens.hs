{-# LANGUAGE OverloadedStrings #-}
-- |Generate a Lens instances for ROS msg types.
module Instances.Lens (lensImport, lensInstance) where
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as B
import Types

lensImport :: ByteString
lensImport = "import Control.Lens (makeLenses, view, set)\n"

lensInstance :: String -> ByteString
lensInstance name = B.append "makeLenses ''" (pack name)

