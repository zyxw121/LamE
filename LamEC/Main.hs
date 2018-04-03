{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment
import Core
import Syntax
import Parser
import Eval
import Terms
import System.Console.GetOpt
import Data.Maybe
import System.IO


data Options = Options 
  { optReport ::String -> IO ()
  , optIn :: Maybe String
  , optOut :: String -> IO () 
  , optReduce :: Maybe (Term -> Term)
  , optDecode :: Maybe (Term -> String)
  }

defaultOptions :: Options
defaultOptions = Options
  { optReport = const $ return ()
  , optIn = Nothing
  , optOut = putStrLn 
  , optReduce = Nothing
  , optDecode = Nothing
  }

options :: [OptDescr (Options -> Options)]
options = 
  [ Option ['v'] ["verbose"] (NoArg (\o -> o{optReport=hPutStrLn stderr })) "print intermediate parse trees to stderr"
  , Option ['i'] ["input"] (ReqArg ((\f o -> o{optIn=Just f}) ) "FILE") "input FILE" 
  , Option ['o'] ["output"] (OptArg ((\f o -> o{optOut= writeFile f }) . fromMaybe "output" )  "FILE") "output FILE" 
  , Option [] ["bnf"] (NoArg (\o -> o{optReduce= Just bnf}  ) ) "attempts to reduce result to beta-normal-form" 
  , Option [] ["hnf"] (NoArg (\o -> o{optReduce= Just hnf}  ) ) "attempts to reduce result to head-normal-form" 
  , Option [] ["int"] (NoArg (\o -> o{optDecode= Just $ show . (unchurch :: Term -> Int)}  ) ) "attempts to decode the result as an integer" 
  , Option [] ["bool"] (NoArg (\o -> o{optDecode= Just $ show . (unchurch :: Term -> Bool)}  ) ) "attempts to decode the result as a boolean" 
  , Option [] ["char"] (NoArg (\o -> o{optDecode= Just $ show . (unchurch :: Term -> Char)}  ) ) "attempts to decode the result as a char" 
  , Option [] ["string"] (NoArg (\o -> o{optDecode= Just $ show . (unchurch :: Term -> String)}  ) ) "attempts to decode the result as a string" 
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
      report = optReport opts 
      output =  optOut opts 
  report $ show prog ++"\n" 
  let a = act' prog prim
  report $ show a ++"\n"
  let p = partial a
  report $ show p ++"\n"
  let r = church p
  output $ show r++"\n"
  case optReduce opts of
    Nothing -> return ()
    Just f -> output $ show (f r) ++ "\n"
  case optDecode opts of
    Nothing -> return ()
    Just f -> output $ f r
