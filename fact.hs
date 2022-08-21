import System.Environment
import System.Exit
import System.IO
import Text.Read

main = getArgs >>= parse >> exit

parse []       = usage
parse (n:nums) = firstarg n >> mapM_ dofact nums

args "-h"        = usage   >> exit
args "--help"    = usage   >> exit
args "-v"        = version >> exit
args "--version" = version >> exit
args str         = hPutStrLn stderr ("Invalid argument " ++ str) >> abend

usage = do
            putStrLn "fact [-hv] [--help|--version] [nums...]"
            putStrLn "Prints the factorials of its arguments"

version = putStrLn "fact version 1.0"

exit  = exitWith ExitSuccess
abend = exitWith (ExitFailure 1)

firstarg n = case (readMaybe n) of
                 Just x  -> putStrLn (show (fact x))
                 Nothing -> args n

dofact str = case (readMaybe str) of
                 Just x  -> putStrLn (show (fact x))
                 Nothing -> hPutStrLn stderr ("Invald number " ++ str)

fact n = inner 1 n
         where
             inner a 0 = a
             inner a 1 = a
             inner a x = if (x < 0) then a else inner (a * x) (x - 1)
