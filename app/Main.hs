module Main where

import qualified Data.ByteString as BS

import System.Environment (getProgName, getArgs)
import System.IO (hPutStrLn, stderr)

import Options.Applicative

import Vec
import Bits
import DES

data Options = Options { key :: Bits Ten
                       , kdcAddr :: String
                       , serverAddr :: String
                       , cmd :: Options -> IO ()
                       } 

encrypt :: Options -> IO ()
encrypt o = encryptByteString (key o) <$> BS.getContents >>= BS.putStr

decrypt :: Options -> IO ()
decrypt o = decryptByteString (key o) <$> BS.getContents >>= BS.putStr

kdc :: Options -> IO ()
kdc = undefined

client :: Options -> IO ()
client = undefined

server :: Options -> IO ()
server = undefined

main :: IO ()
main = do
  opts <- execParser . flip info idm . (<**>helper) $ Options . buildKey
    <$> strOption (long "key" <> short 'k' <> metavar "KEY" <> help "Encryption/decryption key")
    <*> strOption (long "kdc" <> short 'K' <> metavar "KDC" <> help "KDC address")
    <*> strOption (long "server" <> short 's' <> metavar "SERVER" <> help "Server address")
    <*> subparser ( mconcat [ command "encrypt" (info (pure encrypt) idm)
                            , command "decrypt" (info (pure decrypt) idm)
                            ])
  cmd opts opts
