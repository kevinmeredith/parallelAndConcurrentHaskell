import Control.Concurrent
import Control.Exception

data Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  forkIO (do r <- try action; putMVar var r)
  return (Async var)

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async var) = readMVar var

wait :: Async a -> IO a
wait a = do
  r <- waitCatch a
  case r of 
   Left e  -> throwIO e
   Right a -> return a

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = do
  m <- newEmptyMVar
  forkIO $ do r <- try (fmap Left (wait a)); putMVar m r
  forkIO $ do r <- try (fmap Right (wait b)); putMVar m r
  wait (Async m)

slow :: MVar a -> a -> IO (MVar a)
slow m a = threadDelay 5000000 >> putMVar m a >> return m

fast :: MVar a -> a -> IO (MVar a)
fast m a = threadDelay 2000000 >> putMVar m a >> return m

race :: IO ()
race = do
  turtle <- async (threadDelay 5000000 >> return "turtle")
  hare   <- async (threadDelay 1000000 >> return "hare")
  winner <- waitEither turtle hare
  putStrLn $ show winner