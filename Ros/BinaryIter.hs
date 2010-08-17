{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, BangPatterns, 
             TypeSynonymInstances #-}
-- |Binary iteratee-style serialization helpers for working with ROS
-- message types. This module is used by the automatically-generated
-- code for ROS .msg types.
module Ros.BinaryIter (streamIn) where
import Control.Applicative
import Control.Concurrent (myThreadId, killThread, forkIO)
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.IO (Handle)
import System.IO.Unsafe (unsafeInterleaveIO)
import Unsafe.Coerce (unsafeCoerce)
import Ros.RosTypes
import Ros.RosBinary (RosBinary(get))
import Ros.Stream (fromList)
import Ros.Util.RingChan

-- Get the specified number of bytes from a 'Handle'. Returns a
-- wrapped-up 'Nothing' if the client shutdown (indicated by receiving
-- a message of zero length).
hGetAll :: Handle -> Int -> IOMaybe BL.ByteString
hGetAll h n = IOMaybe $ go n []
    where go n' acc = do bs <- BS.hGet h n'
                         case BS.length bs of
                           0 -> return Nothing
                           x | x < n' -> go (n' - x) (bs:acc)
                             | otherwise -> return . Just $ 
                                            BL.fromChunks (reverse (bs:acc))

newtype IOMaybe a = IOMaybe { unIOM :: IO (Maybe a) }
instance Monad IOMaybe where
    return = IOMaybe . return . Just
    IOMaybe ma >>= f = IOMaybe $ do x <- ma
                                    case x of
                                      Just x' -> unIOM $ f x'
                                      Nothing -> return Nothing

instance Functor IOMaybe where
    fmap f (IOMaybe m) = IOMaybe $
                         do x <- m
                            case x of 
                              Just x' -> return . Just . f $ x'
                              Nothing -> return Nothing

-- |The function that does the work of streaming members of the
-- 'RosBinary' class in from a 'Handle'. The first parameter specifies
-- the maximum number of items that will be buffered.
streamIn :: RosBinary a => Int -> Handle -> IO (Stream a)
streamIn sz h = do c <- newRingChan sz
                   parent <- myThreadId
                   forkIO $ go c parent
                   items <- getChanContents c
                   return $ fromList items
    where go c p = let aux = 
                           do item <- unIOM $
                                      do len <- runGet getInt <$> hGetAll h 4
                                         runGet get <$> hGetAll h len
                              case item of
                                Nothing -> putStrLn "Publisher stopped" >>
                                           myThreadId >>= killThread >>
                                           killThread p >> return undefined
                                Just item' -> writeChan c item' >> aux
                   in aux
-- streamIn sz h = go 
--     where go = do item <- unIOM $
--                           do len <- runGet getInt <$> hGetAll h 4
--                              runGet get <$> hGetAll h len
--                   case item of
--                     Nothing -> putStrLn "Publisher stopped" >>
--                                myThreadId >>= killThread >>
--                                return undefined
--                     Just item' -> Cons item' <$> unsafeInterleaveIO go

getInt :: Get Int
getInt = unsafeCoerce <$> getWord32host
