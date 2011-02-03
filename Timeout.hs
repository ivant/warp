module Timeout
    ( Manager
    , Handle
    , initialize
    , register
    , tickle
    , pause
    , resume
    ) where
import qualified Control.Concurrent.Chan as C
import Control.Concurrent (forkIO, threadDelay)
import Network.Socket (Socket, sClose)
import Control.Monad (forever)
import qualified Data.IORef as I

data Event = Marker | Timeout Socket (I.IORef Bool)
newtype Manager = Manager (C.Chan Event)

timeout = 30 * 1000000

initialize :: IO Manager
initialize = do
    c <- C.newChan
    C.writeChan c Marker -- make sure everyone has at least 30 seconds
    _ <- forkIO $ forever $ do
        threadDelay timeout
        _ <- forkIO $ do
            C.writeChan c Marker
            loop c
        return ()
    return $ Manager c
  where
    loop c = do
        x <- C.readChan c
        case x of
            Marker -> return ()
            Timeout s ref -> do
                b <- I.readIORef ref
                if b
                    then sClose s
                    else return ()
                loop c

data Handle = Handle Manager Socket (I.IORef (I.IORef Bool))

register :: Manager -> Socket -> IO Handle
register (Manager c) s = do
    i <- I.newIORef True
    i' <- I.newIORef i
    C.writeChan c $ Timeout s i
    return $ Handle (Manager c) s i'

tickle :: Handle -> IO ()
tickle (Handle (Manager c) s ref) = do
    i <- I.readIORef ref
    I.writeIORef i False
    i' <- I.newIORef True
    I.writeIORef ref i'
    C.writeChan c $ Timeout s i'

pause :: Handle -> IO ()
pause (Handle _ _ ref) = do
    i <- I.readIORef ref
    I.writeIORef i False

resume :: Handle -> IO ()
resume (Handle (Manager c) s ref) = do
    i' <- I.newIORef True
    I.writeIORef ref i'
    C.writeChan c $ Timeout s i'
