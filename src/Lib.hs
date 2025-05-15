module Lib
    ( decodeMessage
    ) where

-- | Decode an intergalactic message from a string.
-- The message is a sequence of integers separated by spaces.
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- Nothing
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- Nothing
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 8"
-- Just 3
-- 
-- >>> decodeMessage "5 5 5 1 2 3 4 8 2 3"
-- Nothing

count :: [Int] -> Int -> Int
count x y = length([a| a <- x, a == y])

intList :: String -> [Int]
intList a = read <$> (words a)

decodeMessage :: String -> Maybe Int
decodeMessage msg =
    Just (intList msg) >>= \ms ->
    if count ms (minimum ms) /= 1
    then Nothing
    else if count ms (maximum ms) /= 1
    then Nothing
    else if (minimum ms + maximum ms) `mod` 2 == 1
    then Nothing
    else Just (count ms $ (minimum ms + maximum ms) `div` 2)


-- | Decode an intergalactic message from a string.
-- The message is a sequence of integers separated by spaces.
-- This is an improved version of the previous function, with a more
-- informative error messages.
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- Left "Communication interference detected: minimum number not Unique"
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- Left "Communication interference detected: maximum number not Unique"
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 8"
-- Right 3
--
-- >>> decodeMessageImproved "5 5 5 1 2 3 4 8 2 3"
-- Left "Communication interference detected: midPoint not even"
decodeMessageImproved :: String -> Either String Int
decodeMessageImproved msg =
    Right (intList msg) >>= \ms ->
    if count ms (minimum ms) /= 1
    then Left "Communication interference detected: minimum number not Unique"
    else if count ms (maximum ms) /= 1 
    then Left "Communication interference detected: maximum number not Unique"
    else if (minimum ms + maximum ms) `mod` 2 == 1
    then Left "Communication interference detected: midPoint not even"
    else Right (count ms $ (minimum ms + maximum ms) `div` 2)
