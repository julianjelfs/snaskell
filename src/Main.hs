{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Control.Applicative            ( pure )
import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.State      ( StateT
                                                , evalStateT
                                                , get
                                                , put
                                                )
import           Data.Functor                   ( (<$>) )
import           Data.Monoid                    ( (<>) )
import           Prelude                 hiding ( Left
                                                , Right
                                                )
import qualified System.Console.ANSI           as Term
import           System.IO                      ( Handle
                                                , hReady
                                                , stdin
                                                )
import           System.Random                  ( randomRIO )

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
initialState = State Up (Snake [(10, 6)]) (Food (5, 5))

loop :: StateT State IO ()
loop = do
  k <- liftIO $ stdin `ifReadyDo` getChar
  s <- get
  let e  = eat s
      s' = moveSnake e . changeDirection k $ s
  s'' <- if e then liftIO (moveFood s' <$> randomPoint) else pure s'
  _   <- put s''
  liftIO . drawGrid $ s''

crash :: State -> Bool
crash State { snake = Snake (h : t) } = h `elem` t

eat :: State -> Bool
eat State { snake = Snake (h : _), food = Food f } = h == f

moveFood :: State -> (Int, Int) -> State
moveFood s f = s { food = Food f }

moveSnake :: Bool -> State -> State
moveSnake grow s@State { snake, direction } = case direction of
  Up    -> move snake (\(x, y) -> (x, clampY (y - 1)))
  Down  -> move snake (\(x, y) -> (x, clampY (y + 1)))
  Left  -> move snake (\(x, y) -> (clampX (x - 1), y))
  Right -> move snake (\(x, y) -> (clampX (x + 1), y))
 where
  move :: Snake -> ((Int, Int) -> (Int, Int)) -> State
  move (Snake l@(h : _)) mover = s { snake = Snake (mover h : allButLast l) }
  move _                 _     = s

  allButLast :: [a] -> [a]
  allButLast xs = if grow then xs else take (length xs - 1) xs

  clampX :: Int -> Int
  clampX = clamp 0 (snd dimensions)

  clampY :: Int -> Int
  clampY = clamp 0 (fst dimensions)

  clamp :: Int -> Int -> Int -> Int
  clamp lower upper val | val < lower = upper
                        | val > upper = lower
                        | otherwise   = val

drawGrid :: State -> IO ()
drawGrid s = Term.clearScreen >> putStrLn (showGrid s) >> threadDelay 100000

showPoint :: State -> Int -> Int -> Char
showPoint State { snake = Snake segments, food = Food (fx, fy) } y x
  | (x, y) `elem` segments = '@'
  | fx == x && fy == y     = '#'
  | otherwise              = '.'

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

randomPoint :: IO (Int, Int)
randomPoint = do
  x <- randomRIO (0, snd dimensions)
  y <- randomRIO (0, fst dimensions)
  pure (x, y)
