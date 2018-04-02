module Main where
import System.Environment
import Core
import Syntax
import Parser
import LamE
import Terms
import System.Console.GetOpt
import Data.Maybe
import System.IO


data Options = Options 
  { optVerbose :: Bool
  , optIn :: Maybe String
  , optOut :: Maybe String
  , optReduce :: Maybe (Term -> Term)
  }

defaultOptions :: Options
defaultOptions = Options
  { optVerbose  = False
  , optIn = Nothing
  , optOut = Nothing
  , optReduce = Nothing
  }

options :: [OptDescr (Options -> Options)]
options = 
  [ Option ['v'] ["verbose"] (NoArg (\o -> o{optVerbose=True})) "print intermediate parse trees to stderr"
  , Option ['i'] ["input"] (ReqArg ((\f o -> o{optIn=Just f}) ) "FILE") "input FILE" 
  , Option ['o'] ["output"] (OptArg (\f o -> o{optOut= f}  ) "FILE") "output FILE" 
  , Option [] ["bnf"] (NoArg (\o -> o{optReduce= Just bnf}  ) ) "attempts to reduce result to beta-normal-form" 
  , Option [] ["hnf"] (NoArg (\o -> o{optReduce= Just hnf}  ) ) "attempts to reduce result to head-normal-form" 
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
 where header = "Usage: LamE [OPTION...] INPUT"

main :: IO ()
main = do
  (opts, args) <- (getArgs >>= compilerOpts) 
  source <- case optIn opts of
    Just path -> readFile path
    Nothing -> return . head $ args
  let prog = parseStrP source
      report = if optVerbose opts then hPutStr stderr else const (return ())
      output = case optOut opts of
        Just out -> writeFile out . show
        Nothing -> print 
  report $ show prog ++ "\n"
  let a = act' prog prim
  report $ show a ++ "\n"
  let p = partial a
  report $ show p ++ "\n"
  let r = church p
  output r
  case optReduce opts of
    Nothing -> return ()
    Just f -> output $ f r
