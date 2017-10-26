import Control.Concurrent
import Control.Concurrent.MVar

timeDownload :: String -> IO ()
timeDownload url = do
  (page, time) <- timeit $ getURL url   -- 1
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time