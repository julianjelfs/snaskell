{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Control.Applicative       (pure)
import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forever)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State (StateT, evalStateT, get, modify)
import           Data.Functor              ((<$>))
import           Data.Monoid               ((<>))
import           Prelude                   hiding (Left, Right)
import qualified System.Console.ANSI       as Term
import           System.IO                 (Handle, hReady, stdin)

main :: IO ()
main = evalStateT (forever loop) initialState

dimensions :: (Int, Int)
dimensions = (19, 39)

data Direction
    = Up
    | Down
    | Left
    | Right
    deriving (Show)

newtype Snake = Snake [(Int, Int)] deriving Show

newtype Food = Food (Int, Int) deriving Show

data State = State
    { direction :: Direction
    , snake     :: Snake
    , food      :: Food
    } deriving Show

initialState :: State
initialState =
  State Up (Snake [(10, 6), (10, 7), (10, 8), (10, 9), (10, 10)]) (Food (5, 5))

loop :: StateT State IO ()
loop = do
  k <- liftIO $ stdin `ifReadyDo` getChar
  _ <- modify (moveSnake . changeDirection k)
  get >>= liftIO . drawGrid

moveSnake :: State -> State
moveSnake s@State { snake, direction } =
    case direction of
        Up    -> move snake (\(x, y) -> (x, y - 1))
        Down  -> move snake (\(x, y) -> (x, y + 1))
        Left  -> move snake (\(x, y) -> (x - 1, y))
        Right -> move snake (\(x, y) -> (x + 1, y))
    where
        move :: Snake -> ((Int, Int) -> (Int, Int)) -> State
        move (Snake l@(h : _)) mover = s { snake = Snake (mover h : allButLast l) }
        move _ _ = s

        allButLast :: [a] -> [a]
        allButLast xs = take (length xs - 1) xs

drawGrid :: State -> IO ()
drawGrid s =
    clearScreen >> putStrLn (showGrid s) >> threadDelay 100000

showPoint :: State -> Int -> Int -> Char
showPoint State { snake = Snake segments, food = Food (fx, fy) } y x
  | any (\(sx, sy) -> sx == x && sy == y) segments = '*'
  | fx == x && fy == y = '#'
  | otherwise          = '.'

showRow :: State -> Int -> String
showRow s y = (showPoint s y <$> [0 .. (snd dimensions)]) <> ['\n']

showGrid :: State -> String
showGrid s = concat $ showRow s <$> [0 .. (fst dimensions)]

changeDirection :: Maybe Char -> State -> State
changeDirection (Just 'a') s = s { direction = Left }
changeDirection (Just 'd') s = s { direction = Right }
changeDirection (Just 'w') s = s { direction = Up }
changeDirection (Just 's') s = s { direction = Down }
changeDirection _          s = s

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd action = hReady hnd >>= \case
  True  -> Just <$> action
  False -> pure Nothing

clearScreen :: IO ()
clearScreen = Term.clearScreen

