module Lib where
import Data.List
import Data.Char
import System.IO
import Data.List
import qualified System.Process as SP
import Control.Monad
data MarkType = Cross | Circle  deriving (Show, Eq)
data MarkPosition  = MarkPosition {
    x :: Int,
    y :: Int
} deriving (Show, Eq)

data Mark =  Mark {
    markType :: MarkType,
    position :: MarkPosition
} deriving (Show, Eq)

createMarkPosition :: (Int,Int) -> Maybe MarkPosition
createMarkPosition (x,y) | (x < 1 || x > 3) || (y < 1 || y > 3) = Nothing
                         | otherwise                            = Just (MarkPosition x y)

createMark :: MarkType -> MarkPosition -> Mark
createMark t p = Mark t p

checkWin :: [Mark] -> (Bool,[Mark])
checkWin m  = (False, m)

compareToWin :: Mark -> (Int,Int) -> MarkType -> Bool
compareToWin m (x1,y1) t | (pX == x1 && pY  == y1 && markT == t) = True
                         | otherwise                             = False
                         where markT  = markType m
                               pX    = x $ position m
                               pY    = y $ position m

playMark ::  Mark ->  [Mark] -> Maybe [Mark]
playMark k  [] = Just [k]
playMark k (mh:marks) | markType k == markType mh = Nothing
                      | otherwise = let markToAddPosition = (position k)
                                        existValue = find (\a-> filterMark (position a) markToAddPosition ) (mh : marks)
                                        executeAction (Just _) = Nothing
                                        executeAction Nothing = Just (k : (mh : marks))
                                    in executeAction existValue

                                     
                           
filterMark :: MarkPosition -> MarkPosition -> Bool
filterMark (MarkPosition x y) (MarkPosition xx yy) = x == xx && y == yy

showMarkPosition (MarkPosition x y) = show x

createNextMarkType :: MarkType -> MarkType
createNextMarkType Cross = Circle
createNextMarkType Circle = Cross

startGame :: IO ()
startGame = welcome where

    welcome :: IO()
    welcome = do
        play [] Cross
        putStrLn $ "Vamos a Jugar al tateti!" 
    play :: [Mark] -> MarkType -> IO()
    play marks nextMarkType= do
        mark <- getMark nextMarkType
        case playMark mark marks of
            Nothing  -> play marks Cross
            (Just m) -> do putStrLn $ show m
                           play m (createNextMarkType nextMarkType)
        return ()

    getMark :: MarkType -> IO Mark
    getMark m = do
        putStrLn $ "---------------------------------"
        putStrLn $ "Elija la x donde colocar la Cruz"
        moveX <- digitToInt <$> getChar
        putStrLn $ "Elija la y donde colocar la Cruz"
        moveY <- digitToInt <$> getChar
        case (createMarkPosition (moveX,moveY)) of
            Nothing  -> getMark m
            (Just p) -> return $ (createMark m p)


clearScreen :: IO ()
clearScreen = do
    _ <- SP.system "reset"
    return ()  