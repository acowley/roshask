{-# LANGUAGE ForeignFunctionInterface #-}
module SDLWrapper where
import ShowVideo
foreign export ccall "haskell_main" main :: IO ()
