module Net where

import Control.Monad
import Control.Exception
import Control.Concurrent (forkFinally)

import Data.Kind
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.C8
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.Random

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

import qualified Database.SQLite.Simple as DB

import DES
import DH

type Keys = [(String, String)]

newtype NetworkError = NetworkError String deriving Show
deriving anyclass instance Exception NetworkError

resolve :: Maybe HostName -> ServiceName -> IO AddrInfo
resolve host port = do
  let hints = defaultHints { addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) host (Just port)
  pure addr

host :: AddrInfo -> IO Socket
host addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock $ addrAddress addr
  listen sock 10
  pure sock

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  pure sock

die :: forall (a :: Type). String -> IO a
die s = putStrLn s >> throwIO (NetworkError s)

generateNonce :: IO String
generateNonce = show <$> getStdRandom (random :: StdGen -> (Int, StdGen))

generateTimestamp :: IO String
generateTimestamp = show . round <$> getPOSIXTime

generateSessionKey :: IO String
generateSessionKey = go 10
  where go :: Int -> IO String
        go 0 = pure []
        go n = do
          bit <- randomBit
          (bit:) <$> go (pred n)
        randomBit :: IO Char
        randomBit = do
          b <- getStdRandom (randomR (True, False))
          pure $ if b then '0' else '1'

ensureTables :: DB.Connection -> IO ()
ensureTables conn = DB.withTransaction conn $ DB.execute_ conn "create table if not exists keys (identity text primary key, shared_key text)"

lookupKey :: DB.Connection -> String -> IO (Maybe String)
lookupKey conn idA = ensureTables conn >> parse <$> DB.withTransaction conn (DB.query conn "select shared_key from table where identity = ?" (DB.Only idA))
  where parse :: [[String]] -> Maybe String
        parse ((x:_):_) = Just x
        parse _ = Nothing

updateKey :: DB.Connection -> String -> String -> IO ()
updateKey conn idA keyA = do
  ensureTables conn
  k <- lookupKey conn idA
  case k of
    Just _ -> DB.withTransaction conn $ DB.execute conn "insert into keys (identity, shared_key) values (?, ?)" (idA, keyA) 
    Nothing -> DB.withTransaction conn $ DB.execute conn "update keys set shared_key = ? where identity = ?" (keyA, idA)

hostDiffieHellman :: DB.Connection -> Integer -> IO ()
hostDiffieHellman keys keyPriv = withSocketsDo $ do
  addr <- resolve Nothing "3000"
  bracket (host addr) close $ \sock -> forever $ do
    (conn, peer) <- accept sock
    putStrLn $ mconcat ["Accepted connection from \"", show peer, "\""]
    void . flip forkFinally (const $ close conn) $ do
      msg <- recv conn 4096
      putStrLn $ mconcat ["Received message \"", BS.C8.unpack msg, "\""]
      unless (BS.null msg) $ do
        let (idA:skeyA:_) = BS.C8.split ';' msg
            keyA = read $ BS.C8.unpack skeyA
            key = intToString10 $ computeSessionKey keyA keyPriv
        putStrLn $ mconcat ["Computed key \"", key, "\" for \"", BS.C8.unpack idA, "\"!"]
        updateKey keys (BS.C8.unpack idA) key
        sendAll conn $ BS.C8.pack $ show keyPub
        putStrLn $ mconcat ["Sent public key \"", show keyPub, "\""]
  where keyPub :: Integer
        keyPub = computePublicKey keyPriv

requestDiffieHellman :: String -> String -> Integer -> IO String
requestDiffieHellman kdc idA keyPriv = withSocketsDo $ do
  addr <- resolve (Just kdc) "3001"
  bracket (open addr) close $ \sock -> do
    sendAll sock . BS.C8.pack $ mconcat [idA, ";", show keyPub]
    intToString10 . flip computeSessionKey keyPriv . read . BS.C8.unpack <$> recv sock 4096
  where keyPub :: Integer
        keyPub = computePublicKey keyPriv

hostSessionKey :: DB.Connection -> IO ()
hostSessionKey keys = withSocketsDo $ do
  addr <- resolve Nothing "3000"
  bracket (host addr) close $ \sock -> forever $ do
    (conn, peer) <- accept sock
    putStrLn $ mconcat ["Accepted connection from \"", show peer, "\""]
    void . flip forkFinally (const $ close conn) $ do
      msg <- recv conn 4096
      putStrLn $ mconcat ["Received message \"", BS.C8.unpack msg, "\""]
      unless (BS.null msg) $ do
        let (idA:idB:n:_) = BS.C8.split ';' msg
        kA <- lookupKey keys (BS.C8.unpack idA)
        keyA <- case kA of
          Just k -> pure k
          _ -> die $ mconcat ["Unknown ID \"", BS.C8.unpack idA, "\""]
        kB <- lookupKey keys (BS.C8.unpack idB)
        keyB <- case kB of
          Just k -> pure k
          _ -> die $ mconcat ["Unknown ID \"", BS.C8.unpack idB, "\""]
        keyS <- BS.C8.pack <$> generateSessionKey
        putStrLn $ mconcat ["Generated session key \"", BS.C8.unpack keyS, "\""]
        ts <- BS.C8.pack <$> generateTimestamp
        putStrLn $ mconcat ["Current time is ", show ts]
        let envB = encryptByteString (buildKey keyB) $ mconcat [keyS, ";", idA, ";", ts]
            envA = encryptByteString (buildKey keyA) $ mconcat [keyS, ";", idB, ";", n, ";", envB]
        sendAll conn envA
        putStrLn "Sent encrypted envelope!"

requestSessionKey :: String -> String -> String -> String -> IO (String, BS.ByteString)
requestSessionKey kdc keyA idA idB  = withSocketsDo $ do
  addr <- resolve (Just kdc) "3000"
  bracket (open addr) close $ \sock -> do
    n <- generateNonce
    sendAll sock . BS.C8.pack $ mconcat [idA, ";", idB, ";", n]
    envA <- recv sock 4096
    let (keyS:idB':n':envB:_) = BS.C8.split ';' $ decryptByteString (buildKey keyA) envA
    putStrLn $ mconcat ["Received session key \"", BS.C8.unpack keyS, "\""]
    if | idB /= BS.C8.unpack idB' ->
         die $ mconcat ["Tried to communicate with \"", idB, "\" but received envelope for \"", BS.C8.unpack idB', "\""]
       | n /= BS.C8.unpack n' ->
         die $ mconcat ["Expected nonce \"", n, "\" but received nonce \"", BS.C8.unpack n', "\""]
       | otherwise -> pure (BS.C8.unpack keyS, envB)

hostSession :: String -> IO ()
hostSession keyB = withSocketsDo $ do
  addr <- resolve Nothing "3001"
  bracket (host addr) close $ \sock -> forever $ do
    (conn, peer) <- accept sock
    putStrLn $ mconcat ["Accepted connection from \"", show peer, "\""]
    void . flip forkFinally (const $ close conn) $ do
      msg <- recv conn 4096
      unless (BS.null msg) $ do
        let (keyS:idA:sts':_) = BS.C8.split ';' $ decryptByteString (buildKey keyB) msg
            ts' = (read $ BS.C8.unpack sts') :: Integer
        putStrLn $ mconcat ["Received message from \"", BS.C8.unpack idA, "\""]
        ts <- round <$> getPOSIXTime
        putStrLn $ mconcat ["Received timestamp \"", show ts', "\", current time is \"", show ts, "\""]
        when (ts - ts' > 10) $ die "Timestamp difference too large - replay attack detected."
        putStrLn $ mconcat ["Received session key \"", BS.C8.unpack keyS, "\""]
        n <- generateNonce
        sendAll conn . encryptByteString (buildKey (BS.C8.unpack keyS)) $ BS.C8.pack n
        putStrLn "Sent encrypted nonce."
        n' <- decryptByteString (buildKey (BS.C8.unpack keyS)) <$> recv conn 4096
        if '!':n == BS.C8.unpack n'
          then putStrLn "Successfully negotiated session key!"
          else die $ mconcat ["Expected nonce \"", '!':n, "\" but received nonce \"", BS.C8.unpack n', "\""]

joinSession :: String -> (String, BS.ByteString) -> IO ()
joinSession remote (keyS, envB) = withSocketsDo $ do
  addr <- resolve (Just remote) "3001"
  bracket (open addr) close $ \sock -> do
    sendAll sock envB
    n <- decryptByteString (buildKey keyS) <$> recv sock 4096
    sendAll sock . encryptByteString (buildKey keyS) $ BS.C8.pack ('!':BS.C8.unpack n)
