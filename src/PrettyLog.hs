{-# LANGUAGE OverloadedStrings #-}

import Data.Text as T
import Data.Text.Encoding as E
import Data.Text.Encoding.Error as EE
import Data.List as L
import Data.ByteString as B
import System.IO as S
import Text.XML.Light
import Text.Regex.PCRE.Light

data LogEntry = LogEntry Text [Text]
                deriving Show

-- One word in square brackets
token :: Regex
token = compile "^\\[[^ ]+\\]" []

tokenFound :: Text -> Bool
tokenFound t = case match token (E.encodeUtf8 t) [] of
               Just _  -> True
               Nothing -> False

-- How many tokens to read at most
tokenCount :: Int
tokenCount = 8

-- How many bytes to read in one step.
readStep :: Integer
readStep = 100

makeAlfredOutput :: [LogEntry] -> String
makeAlfredOutput values = ppElement $ unode "items" items
                          where items = makeAlfredItems values

makeAlfredItems :: [LogEntry] -> [Element]
makeAlfredItems = L.map makeAlfredItem

makeAlfredItem :: LogEntry -> Element
makeAlfredItem (LogEntry _ x) = unode "item" (attribute, title)
                   where first = L.head x
                         attribute = [Attr (QName "arg" Nothing Nothing) (T.unpack $ T.intercalate "\r\n" x)]
                         title = [unode "title" (CData CDataText (T.unpack first) Nothing)]

lastN :: Int -> [a] -> [a]
lastN n xs = L.drop (L.length xs - n) xs

parseGroups :: [Text] -> ([Text], [LogEntry])
parseGroups x = groupByToken x [] [] []

groupByToken :: [Text] -> [Text] -> [LogEntry] -> [Text] -> ([Text], [LogEntry])
groupByToken [] temp groups leftover = (leftover, groups ++ [LogEntry "" temp])

groupByToken (x:xs) [] [] leftover = if tokenFound x
                                     then groupByToken xs [x] [] leftover
                                     else groupByToken xs [] [] (leftover ++ [x])

groupByToken (x:xs) temp groups leftover = if tokenFound x
                                           then groupByToken xs [x] (groups ++ [LogEntry "" temp]) leftover
                                           else groupByToken xs (temp ++ [x]) groups leftover

parseStep :: Handle -> (Text, [LogEntry]) -> IO [LogEntry]
parseStep h parsed = do
  currentPosition <- S.hTell h

  if L.length (snd parsed) >= tokenCount
  then return $ lastN tokenCount $ snd parsed
  else if currentPosition == 0
       then return $ lastN tokenCount $ LogEntry "" (T.lines (fst parsed)) : snd parsed
       else do
         let step = min currentPosition readStep

         S.hSeek h RelativeSeek (-step)

         str <- B.hGet h $ fromInteger step
         let decodedStr = E.decodeUtf8With EE.ignore str

         let goodHead = getGoodHead currentPosition decodedStr

         let goodSize = B.length $ E.encodeUtf8 decodedStr
           in S.hSeek h RelativeSeek $ toInteger (-goodSize)

         if T.length goodHead == 0
         then parseStep h (T.append decodedStr (fst parsed), snd parsed)
         else do
           let newParsed = parseGroups $ T.lines $ T.append goodHead (fst parsed)

           let dropped = L.head (T.lines decodedStr)

           if L.length (snd newParsed) == 0
           then parseStep h (T.unlines $ dropped : fst newParsed, snd parsed)
           else parseStep h (T.unlines $ dropped : fst newParsed, snd newParsed ++ snd parsed)

getGoodHead :: Integer -> Text -> Text
getGoodHead pos str
  | pos < readStep = str
  | otherwise = if "\n" `T.isInfixOf` str
                then dropBadHead str
                else T.empty

dropBadHead :: Text -> Text
dropBadHead s = T.tail $ T.dropWhile (/= '\n') s

main :: IO ()
main = do
  h <- S.openFile "data.txt" ReadMode
  S.hSeek h SeekFromEnd 0
  values <- parseStep h (T.empty, [])

  let xml = makeAlfredOutput values
  S.putStrLn $ xml_header ++ "\n" ++ xml
