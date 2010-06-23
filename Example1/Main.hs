module Main (main) where
import qualified Ros.Std_msgs.String as S

main = let s = S.String "'erro"
       in do putStrLn (S.data s)