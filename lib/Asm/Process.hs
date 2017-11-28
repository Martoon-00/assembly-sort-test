{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Utils to work with OS processes.

module Asm.Process
    ( ProgramInput (..)
    , ProgramStdout
    , ProgramStderr
    , ProgramOutput (..)
    , readCreateProcess
    ) where

import           Control.Concurrent       (forkIO, killThread)
import qualified Control.Concurrent.Async as A
import           Control.Exception        (SomeException, evaluate, handle, onException,
                                           throwIO, try)
import           Control.Monad            (unless)
import           Data.String              (IsString)
import           Foreign.C.Error          (Errno (..), ePIPE)
import           GHC.IO.Exception         (IOErrorType (..), IOException (..))
import           System.Exit              (ExitCode (..))
import           System.IO                (Handle, hClose, hGetContents, hPutStr)
import           System.Process           (CreateProcess (..), ProcessHandle,
                                           StdStream (..), createProcess, proc,
                                           terminateProcess, waitForProcess)
import           System.Process.Internals (ProcessHandle (..), stopDelegateControlC)
import           Universum

-- TODO: type or newtype?
newtype ProgramInput = ProgramInput Text
    deriving (IsString)
type ProgramStdout = Text
type ProgramStderr = Text

data ProgramOutput = ProgramOutput
    { poStdout :: !ProgramStdout
    , poStderr :: !ProgramStderr
    } deriving (Show)

-- | Copy-pasted `readCreateProcess` functions, with slight differences:
-- 1. Fetches both stdout and stderr
-- 2. Decouples initialization from interaction phase.
--
-- Note that this passes input strictly, and fetches output strictly as well,
-- blocking until stdout and stderr are entirely evaluated.
readCreateProcess
    :: FilePath
    -> IO (ProgramInput -> IO (ExitCode, ProgramOutput))
readCreateProcess executable = do
    let cp_opts = (proc executable [])
            { std_in  = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }

    inputBox <- newEmptyMVar

    let startProcess = withCreateProcess cp_opts $
          \(Just inh) (Just outh) (Just errh) ph -> do

            -- wait for permission to start interaction phase
            ProgramInput input <- takeMVar inputBox

            -- fork off a thread to start consuming the output
            poStdout <- toText <$> hGetContents outh
            poStderr <- toText <$> hGetContents errh
            let result = ProgramOutput{..}
            withForkWait (evaluate $ seq result ()) $ \waitOut -> do

                -- now write any input
                unless (null input) $
                    ignoreSigPipe $ hPutStr inh (toString input)
                -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
                ignoreSigPipe $ hClose inh

                -- wait on the output
                waitOut
                hClose outh
                hClose errh

            -- wait on the process
            ex <- waitForProcess ph
            return (ex, result)

    process <- A.async startProcess
    return $ \input -> do
        putMVar inputBox input
        A.wait process


-- * Following functions are exported by /process/ library in latest lts only.

withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let doWait = takeMVar waitVar >>= either throwIO return
    restore (body doWait) `onException` killThread tid

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> throwIO e

withCreateProcess
  :: CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess c action =
    bracket (createProcess c) cleanupProcess $
        \(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph

cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
               -> IO ()
cleanupProcess (mb_stdin, mb_stdout, mb_stderr,
                ph@(ProcessHandle _ delegating_ctlc)) = do
    terminateProcess ph
    -- Note, it's important that other threads that might be reading/writing
    -- these handles also get killed off, since otherwise they might be holding
    -- the handle lock and prevent us from closing, leading to deadlock.
    maybe (return ()) (ignoreSigPipe . hClose) mb_stdin
    maybe (return ()) hClose mb_stdout
    maybe (return ()) hClose mb_stderr
    -- terminateProcess does not guarantee that it terminates the process.
    -- Indeed on Unix it's SIGTERM, which asks nicely but does not guarantee
    -- that it stops. If it doesn't stop, we don't want to hang, so we wait
    -- asynchronously using forkIO.

    -- However we want to end the Ctl-C handling synchronously, so we'll do
    -- that synchronously, and set delegating_ctlc as False for the
    -- waitForProcess (which would otherwise end the Ctl-C delegation itself).
    when delegating_ctlc
      stopDelegateControlC
    _ <- forkIO (waitForProcess (resetCtlcDelegation ph) >> return ())
    return ()
  where
    resetCtlcDelegation (ProcessHandle m _) = ProcessHandle m False

