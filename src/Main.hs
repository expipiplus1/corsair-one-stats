module Main where

import           Data.Bits
import           Control.Exception
import           Control.Concurrent
import           Data.Foldable                  ( traverse_ )
import           Data.List
import           Data.Word
import           System.Exit
import           System.IO
import           Text.Printf
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as T
import qualified Data.Vector                   as V
import           System.USB
import           Control.Monad
import           Data.Digest.CRC
import           Data.Digest.CRC8

main :: IO ()
main = do
  ctx <- newCtx
  setDebug ctx PrintInfo
  dev <- findMyDevice ctx 0x1b1c 0x0c14
  go dev

go :: Device -> IO ()
go dev = do
  putStrLn "Opening device..."
  withDeviceHandle dev $ \devHndl -> do
    putStrLn "Detaching kernel driver..."
    withDetachedKernelDriver devHndl 0 $ do
      putStrLn "Claiming interface..."
      withClaimedInterface devHndl 0 $ do
        initialize devHndl
        putStrLn "initialized"
        traverse_
          (\i ->
            print i >> poll (B.singleton (fromInteger i)) devHndl >> threadDelay
              1000000
          )
          [0, 8 ..]

-- | Mimic pretty much what iCue does
initialize :: DeviceHandle -> IO ()
initialize devHandle = do
  -- Without this the device can get into a bad state, although it does respond
  -- all the responses are empty
  resetDevice devHandle

  let setIdle                      = 0x0a
      setConfiguration             = 0x09
      getDescriptor                = 0x06
      getDescDevice                = 0x01
      getDescConfig                = 0x02
      getDescHIDReport             = 0x22
      manufacturerStringIndexIndex = 14
      productStringIndexIndex      = 15

  deviceDesc <- getDesc devHandle getDescDevice 0 0 18 1000
  let manufacturerStringIndex = bsWord8 manufacturerStringIndexIndex deviceDesc
      productStringIndex      = bsWord8 productStringIndexIndex deviceDesc
  totalConfigLength <- bsWord16 2 <$> getDesc devHandle getDescConfig 0 0 9 1000
  _ <- getDesc devHandle getDescConfig 0 0 (fromIntegral totalConfigLength) 1000
  control devHandle (ControlSetup Standard ToDevice setConfiguration 0x1 0) 1000

  T.putStrLn =<< readString devHandle manufacturerStringIndex
  T.putStrLn =<< readString devHandle productStringIndex

  -- This returns -EPIPE for iCue too, catch and ignore it here
  control devHandle (ControlSetup Class ToInterface setIdle 0 0) 1000
    `catch` (\e -> print (e :: USBException))

  _ <- readControl
    devHandle
    (ControlSetup Standard
                  ToInterface
                  getDescriptor
                  (getDescHIDReport `shiftL` 8)
                  0
    )
    91
    1000
  pure ()

-- | Send a `0x3f` packet with payload of prefixNonsense, 0, suffixNonsense and
-- print out the returned data.
poll :: B.ByteString -> DeviceHandle -> IO ()
poll prefixNonsense devHandle = do
  let
    setReport  = 0x09
    reportType = 0x02
    payload =
      prefixNonsense <> B.replicate (64 - 1 - B.length prefixNonsense - 1) 0x00
  writeControlExact
    devHandle
    (ControlSetup Class ToInterface setReport (reportType `shiftL` 8) 0)
    (B.singleton 0x3f <> payload <> B.singleton (crc8 (digest payload)))
    1000
  (b, _) <- readInterrupt devHandle (EndpointAddress 1 In) 64 1000
  printf "Fan Speed   : %d RPM\n" (bsWord16 0x0f b)
  printf "CPU Pump    : %d RPM\n" (bsWord16 0x1d b)
  printf "GPU Pump    : %d RPM\n" (bsWord16 0x16 b)
  printf "CPU Coolant : %.02f °C\n"
         (realToFrac (bsWord16 0x07 b) / 0x100 :: Float)
  printf "GPU Coolant : %.02f °C\n"
         (realToFrac (bsWord16 0x31 b) / 0x100 :: Float)
  putStr "\n"
  pure ()

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- | Construct a request for some configuration data
getDesc :: DeviceHandle -> Word8 -> Word8 -> Word16 -> ReadExactAction
getDesc devHandle desc index value =
  let getDescriptor = 0x06
  in  readControlExact
        devHandle
        (ControlSetup Standard
                      ToDevice
                      getDescriptor
                      (fromIntegral desc `shiftL` 8 .|. fromIntegral index)
                      value
        )

-- | Read a string from a device given a language and index
readString :: DeviceHandle -> Word8 -> IO T.Text
readString devHandle index = do
  let descString = 0x03
      noLanguage = 0x0000
  -- Use the first language
  lang <- bsWord16 2 <$> getDesc devHandle descString 0 noLanguage 4 1000
  len  <- bsWord8 0 <$> getDesc devHandle descString index lang 2 1000
  s    <- getDesc devHandle descString index lang (fromIntegral len) 1000
  pure (T.takeWhile (/= '\NUL') . T.decodeUtf16LE . B.drop 2 $ s)

-- | Print some bytes in rows of 8 in hex.
printBytes :: B.ByteString -> IO ()
printBytes = traverse_ line . chunks 0x10
 where
  line = putStrLn . unwords . map (printf "%02x") . B.unpack
  chunks n b =
    let (x, y) = B.splitAt n b in x : if B.null y then [] else chunks n y

-- | Enumerate all devices and find the right one.
findMyDevice :: Ctx -> VendorId -> ProductId -> IO Device
findMyDevice ctx vendorId productId = do
  devs        <- V.toList <$> getDevices ctx
  deviceDescs <- mapM getDeviceDesc devs
  case fmap fst $ find (match . snd) $ zip devs deviceDescs of
    Nothing  -> hPutStrLn stderr "Device not found" >> exitFailure
    Just dev -> return dev
 where
  match :: DeviceDesc -> Bool
  match devDesc =
    deviceVendorId devDesc == vendorId && deviceProductId devDesc == productId

-- | Extract a little-endian Word16 starting at a particular index
bsWord16 :: Int -> B.ByteString -> Word16
bsWord16 i b = (b ! (i + 1) `shiftL` 8) .|. b ! i
 where
  (!)  = fromIntegral .: B.index
  (.:) = (.) . (.)

-- | Extract a Word8 starting at a particular index
bsWord8 :: Int -> B.ByteString -> Word8
bsWord8 = flip B.index
