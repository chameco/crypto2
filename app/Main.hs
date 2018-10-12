module Main where

import qualified Data.ByteString as BS

import System.Environment (getProgName, getArgs)
import System.IO (hPutStrLn, stderr)

import Options.Applicative

import Vec
import Bits
import DES
import Net

data Options = Options { key :: Bits Ten
                       , idA :: String
                       , kdcAddr :: String
                       , remote :: String
                       , idB :: String
                       , database :: String
                       , keyPriv :: Integer
                       , cmd :: Options -> IO ()
                       } 

encrypt :: Options -> IO ()
encrypt o = encryptByteString (key o) <$> BS.getContents >>= BS.putStr

decrypt :: Options -> IO ()
decrypt o = decryptByteString (key o) <$> BS.getContents >>= BS.putStr

kdcDiffieHellman :: Options -> IO ()
kdcDiffieHellman o = hostDiffieHellman (keyPriv o)

kdc :: Options -> IO ()
kdc o = hostSessionKey undefined

client :: Options -> IO ()
client o = do
  keyA <- requestDiffieHellman (kdcAddr o) (idA o) (keyPriv o)
  resp <- requestSessionKey (kdcAddr o) keyA (idA o) (idB o)
  joinSession (remote o) resp

server :: Options -> IO ()
server o = do
  keyB <- requestDiffieHellman (kdcAddr o) (idA o) (keyPriv o)
  hostSession keyB

main :: IO ()
main = do
  opts <- execParser . flip info idm . (<**>helper) $ Options . buildKey
    <$> strOption (long "key" <> short 'k' <> metavar "KEY" <> value (error "Key not given!") <> help "Encryption/decryption key")
    <*> strOption (long "id" <> short 'i' <> metavar "ID" <> value (error "Identity not given!") <> help "Identifying string")
    <*> strOption (long "kdc" <> short 'c' <> metavar "KDC" <> value (error "KDC address not given!") <> help "KDC address")
    <*> strOption (long "remote" <> short 'r' <> metavar "REMOTE" <> value (error "Remote address not given!") <> help "Remote address")
    <*> strOption (long "remote-id" <> short 'I' <> metavar "REMOTEID" <> value (error "Remote identity not given!") <> help "Remote identifying string")
    <*> strOption (long "db" <> short 'd' <> metavar "DB" <> value "keystore.sqlite3" <> help "SQLite3 key store path")
    <*> option auto (long "dhkey" <> short 'K' <> metavar "DHKEY" <> value (error "Diffie-Hellman private key not given!") <> help "Diffie-Hellman private key")
    <*> subparser ( mconcat [ command "encrypt" (info (pure encrypt) idm)
                            , command "decrypt" (info (pure decrypt) idm)
                            , command "kdcdh" (info (pure kdcDiffieHellman) idm)
                            , command "kdc" (info (pure kdc) idm)
                            , command "client" (info (pure client) idm)
                            , command "server" (info (pure server) idm)
                            ])
  cmd opts opts
