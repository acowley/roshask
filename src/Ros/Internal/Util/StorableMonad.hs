-- |An applicative interface for working with Storable values. The idea
-- is that the underlying pointer is threaded through the computation
-- to make reading and writing consecutive values easier.
module Ros.Internal.Util.StorableMonad (peek, poke, runStorable, 
                                        StorableM) where
import Control.Monad.State.Strict
import Foreign.Ptr
import Foreign.Storable hiding (peek, poke)
import qualified Foreign.Storable as S

-- |A state monad that threads a pointer through a computation.
type StorableM a = StateT (Ptr ()) IO a

-- |Action that pokes a value into the current pointer location, then
-- moves the pointer to just after the poked value.
poke :: Storable a => a -> StorableM ()
poke x = do ptr <- get
            liftIO $ S.poke (castPtr ptr) x
            put (plusPtr ptr (sizeOf x))

-- |Action that peeks a value from the current pointer location, then
-- moves the pointer to just after the peeked value.
peek :: Storable a => StorableM a
peek = do ptr <- get
          x <- liftIO $ S.peek (castPtr ptr)
          put (plusPtr ptr (sizeOf x))
          return x

-- |Run a StorableM action with the supplied initial pointer location.
runStorable :: StorableM a -> Ptr b -> IO a
runStorable s p = evalStateT s (castPtr p)
