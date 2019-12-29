module Main where

import Parser as Parser
import qualified System.Process as Process
import Sound.Tidal.Context
import System.Environment
import System.Exit
import GHC.IO.Handle
import Options.Applicative
import System.IO (hPutStr,hPutStrLn, hFlush,hWaitForInput,stdin)




-- optionsFromList (x:y:xs) = case x of
--   "bootTidal" ->

-- to construct custom options, just use
-- customOptions = defaultOptions {bootTidal = False, sclangPath = "some/other/path"}
-- etc...

exit_   = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

createTidalProcess :: String -> Process.CreateProcess
createTidalProcess ghciCommand = (Process.proc firstArg otherArgs) -- ["BootTidal.hs"]
                  { Process.std_in  = Process.CreatePipe
                  , Process.std_out = Process.Inherit
                  , Process.std_err = Process.Inherit
                  , Process.delegate_ctlc = True
                  }
                  where (firstArg:otherArgs) = words ghciCommand

createSupercolliderProcess:: String -> Process.CreateProcess
createSupercolliderProcess sclangCommand = (Process.proc firstArg otherArgs ) -- ["app/initScripts/startup.scd"]
                              { Process.std_in  = Process.CreatePipe
                              , Process.std_out = Process.Inherit
                              , Process.std_err = Process.Inherit
                              , Process.create_new_console = True
                              , Process.delegate_ctlc = False
                              }
                              where (firstArg:otherArgs) = words sclangCommand

main :: IO ()
main = do
  optionsIO <- Parser.optionParser
  options <- optionsIO
  args_ <- getArgs
  let tidalBootPath' = Parser.tidalBootPath options
      superColliderBootPath' = Parser.superColliderBootPath options
      ghciPath' = Parser.ghciPath options
      sclangPath' = Parser.sclangPath options
      supercolliderProcess = createSupercolliderProcess $ sclangPath' ++ " " ++ superColliderBootPath'
      tidalProcess = createTidalProcess $ ghciPath'
  st@(stIn, stOut, stErr, stDel) <- doubleWithCreateProcess supercolliderProcess tidalProcess (stInteraction options)
  Process.cleanupProcess st

type ProcessReturns = (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle)
type ProcessHandler = Maybe Handle -> Maybe Handle -> Maybe Handle -> Process.ProcessHandle -> IO ProcessReturns
type DualProcessHandler = Process.CreateProcess -> Process.CreateProcess -> (ProcessReturns -> ProcessReturns -> IO ProcessReturns) -> IO ProcessReturns


doubleWithCreateProcess :: DualProcessHandler -- this function allows any 2 different Processes to interact with eachother via the passed function `interaction`
                                              -- it allows both the supercollider (sclang) and tidalcycles (ghci) processes to be controlled via one terminal
doubleWithCreateProcess process1 process2 interaction = do
  p1_ <- Process.withCreateProcess process1 (interaction_ process2)
  return p1_
    where interaction_ p a b c d = do
            p2_ <- Process.withCreateProcess p (interaction__ (a,b,c,d))
            return p2_
              where interaction__ abcd_ a' b' c' d' = interaction abcd_ (a',b',c',d')


stInteraction :: Parser.Options -> ProcessReturns -> ProcessReturns -> IO ProcessReturns
stInteraction options s'' t'' = do
  let (stdin',stdout',stderr',ph') = t''
  interaction_ stdin' stdout' stderr' ph'
    where interaction_ stdin' stdout' stderr' ph = do
            let bootPath = Parser.tidalBootPath options
                bootFile = ":script " ++ bootPath ++ " \n" -- command to laod BootTidal.hs in the ghci
                mb = maybe (error "Maybe.fromJust: Nothing") id
            _ <- hWaitForInput stdin 1000
            hPutStr (mb stdin') bootFile -- passes BootTidal.hs to ghci as a script
            hFlush (mb stdin') -- makes sure it evaluates (just in case)
            let pipeUserInput = do -- this function takes user input and pipes it into the running tidalcycles process
                                   -- until the user calls ":quit"
                                   -- TODO: let the user input ctrl-d to stop the process
                      command_ <- getLine
                      sendInput command_
                      if (command_ == ":quit")
                        then do
                          return (stdout')
                        else do
                          pipeUserInput
                            where sendInput cmd = do { hPutStrLn (mb stdin') cmd ; hFlush (mb stdin')}
            stdout__ <- pipeUserInput
            _ <- Process.waitForProcess ph
            return (stdin', stdout__,stderr',ph)
