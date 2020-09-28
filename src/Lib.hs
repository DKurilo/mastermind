{-# LANGUAGE LambdaCase #-}
-- https://en.wikipedia.org/wiki/Mastermind_(board_game)
-- Knuth algorithm for computer as codebreaker
module Lib
    ( GamesAmount(..)
    , play
    ) where

import           Control.Monad                (forM, mapM, (>>=))
import           Data.Char                    (isSpace)
import           Data.List                    (intercalate, intersect)
import qualified Data.Map                     as M
import           GHC.Read
import           System.Random
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read
import qualified Text.Read.Lex                as L

maxGuesses :: Int
maxGuesses = 12

newtype GamesAmount = GA Int deriving (Show)

instance Read GamesAmount where
    readPrec = prec minPrec $ do
      n <- step readPrec
      if n `mod` 2 == 1
         then pfail
         else (return . GA . (`div` 2)) n

gameAmount :: GamesAmount -> Int
gameAmount (GA n) = 2 * n

pegReader :: (Char -> ReadPrec a) -> ReadPrec a
pegReader p = prec minPrec $ do
        let isSpaceOrDelimiter c =  isSpace c || c == ',' || c == ';' || c == '|'
        (lift . P.skipMany) $ P.satisfy isSpaceOrDelimiter
        get >>= p

data Color = C1 | C2 | C3 | C4 | C5 | C6 deriving (Eq, Ord)

getColor :: Int -> Maybe Color
getColor 1 = Just C1
getColor 2 = Just C2
getColor 3 = Just C3
getColor 4 = Just C4
getColor 5 = Just C5
getColor 6 = Just C6
getColor _ = Nothing

instance Read Color where
    readPrec = pegReader $ \case
          '1' -> return C1
          '2' -> return C2
          '3' -> return C3
          '4' -> return C4
          '5' -> return C5
          '6' -> return C6
          _   -> pfail

instance Show Color where
    show C1 = "1"
    show C2 = "2"
    show C3 = "3"
    show C4 = "4"
    show C5 = "5"
    show C6 = "6"

allColors = [C1, C2, C3, C4, C5, C6]

data Code = Code Color Color Color Color deriving (Eq, Ord)

instance Read Code where
    readPrec = prec minPrec $ do
        c1 <- reset readPrec
        c2 <- reset readPrec
        c3 <- reset readPrec
        c4 <- reset readPrec
        return $ Code c1 c2 c3 c4

instance Show Code where
    show (Code c1 c2 c3 c4) = show c1 <> " " <>  show c2 <> " " <> show c3 <> " " <> show c4

type Guess = Code

initialGuess = Code C1 C1 C2 C2

buildCode :: [Int] -> Maybe Code
buildCode ns = mapM getColor ns >>= buildCodeFromColors

buildCodeFromColors :: [Color] -> Maybe Code
buildCodeFromColors cs@(c1:c2:c3:c4:_)
  | length cs == 4 = Just (Code c1 c2 c3 c4)
buildCodeFromColors _ = Nothing

getCodeColors :: Code -> [Color]
getCodeColors (Code c1 c2 c3 c4) = [c1, c2, c3, c4]

type Result = (Int, Int)

data Response = Resp Int Int deriving (Eq, Ord)

instance Semigroup Response where
    (Resp c p) <> (Resp c' p') = Resp (c + c') (p + p')

instance Monoid Response where
    mempty = Resp 0 0

instance Read Response where
    readPrec = prec minPrec (do
            c <- get
            resp <- readPrec :: ReadPrec Response
            let Resp col p = resp
            if col + p < 4
               then return ((if c == '1' then Resp 1 0 else Resp 0 1) <> resp)
               else pfail)
           +++ prec minPrec (return (Resp 0 0))

instance Show Response where
    show (Resp c p) = intercalate "\n" (replicate c "Color" <> replicate p "Color & Position")

play :: GamesAmount -> IO ()
play ga = do
    points <- forM [1..gameAmount ga] $ \n ->
        if n `mod` 2 == 0
           then masterCoder
           else masterBreaker
    let (me, player) = foldl (\(m, p) (m', p') -> (m + m', p + p')) (0, 0) points
    if me > player
       then putStrLn "I won this time!"
       else if me < player
               then putStrLn "You won!"
               else putStrLn "Draw!"

masterCoder :: IO Result
masterCoder = do
    ns <- mapM (\_ -> getStdRandom (randomR (1, 6))) [1..4]
    let code = case buildCode ns of
            Just c -> c
            _      -> error "Oops! I can't figure out any code!"
    putStrLn ("I have code! (4 numbers from 1 to 6) Break it in " <> show maxGuesses <> " gueses!")
    masterCoderPlay maxGuesses code

masterCoderPlay :: Int -> Code -> IO Result
masterCoderPlay 0 code = do
    putStrLn ("I won this game! My code is: " <> show code)
    return (2, 0)
masterCoderPlay turns code = do
    putStrLn $ "Type your guess! " <> show turns <> if turns == 1 then " guess" else " guesses" <> " left."
    guess <- read <$> getLine
    let resp = getResponse code guess
        Resp _ p = resp
    if p == 4
       then do
           putStrLn "You won this game!"
           return (1, 0)
       else do
           putStrLn ("Result:\n" <> show resp)
           (me, player) <- masterCoderPlay (turns - 1) code
           return (me + 1, player)

getResponse :: Code -> Guess -> Response
getResponse code guess = Resp col pos
    where codeColors = getCodeColors code
          guessColors = getCodeColors guess
          pos = length . filter (==True) $ zipWith (==) codeColors guessColors
          allColorsMap = (M.fromList . zip allColors . repeat) 0
          listToCountMap vs = (M.unionWith (+) allColorsMap . M.fromListWith (+) . zip vs . repeat) 1
          codeMap = listToCountMap codeColors
          guessMap = listToCountMap guessColors
          col = (sum . M.elems) (M.unionWith min codeMap guessMap) - pos

masterBreaker :: IO Result
masterBreaker = do
    putStrLn "Figure out your code (4 numbers from 1 to 6) and press any key"
    _ <- getChar
    putChar '\n'
    let allcodes = [let (Just code) = buildCodeFromColors [c1, c2, c3, c4] in code | c1 <- allColors
                                                                                   , c2 <- allColors
                                                                                   , c3 <- allColors
                                                                                   , c4 <- allColors
                   ]
    masterBreakerPlay maxGuesses allcodes allcodes initialGuess

masterBreakerPlay :: Int -> [Code] -> [Code] -> Guess -> IO Result
masterBreakerPlay 0 _ _ _ = do
    putStrLn "You won this game! Are you a cheater?!"
    return (0,2)
masterBreakerPlay turns unused possible guess = do
    putStrLn $ show turns <> if turns == 1 then " guess" else " guesses" <> " left! \nMy guess: " <> show guess
            <> "\nEnter hint, please (1 for each right color with wrong position, 2 for each right color and right position):"
    resp <- read <$> getLine
    let Resp _ p = resp
    if p == 4
       then do
           putStrLn "I won!"
           return (0, 1)
       else do
           let unused' = filter (/=guess) unused
               possible' = filter (\guess' -> getResponse guess guess' == resp) possible
               guess' = getNewGuess unused' possible'
           (me, player) <- masterBreakerPlay (turns - 1) unused' possible' guess'
           return (me, player + 1)

getNewGuess :: [Code] -> [Code] -> Code
getNewGuess unused possible
  | null foundInPossible = head bestGuesses
  | otherwise = head foundInPossible
  where hits = [(getResponse codeU codeP, codeU) | codeU <- unused, codeP <- possible]
        hitMap = foldl (\m (r, _) -> M.insertWith (+) r 1 m) M.empty hits
        guessHitMap = foldl (\m (r, g) -> let Just h = M.lookup r hitMap in M.insertWith min g h m) M.empty hits
        minHighHitCount = (minimum . M.elems) hitMap
        bestGuesses = (map fst . filter (\(_, v) -> v == minHighHitCount) . M.assocs) guessHitMap
        foundInPossible = possible `intersect` bestGuesses
