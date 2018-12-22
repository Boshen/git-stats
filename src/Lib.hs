{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Applicative                       (empty)
import           Control.Concurrent.Async                  (mapConcurrently)
import           Control.Monad
import           Data.List                                 (sortOn)
import qualified Data.Map.Strict                           as Map
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Void
import           System.Process
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer                as L

data Blame = Blame
  { sha          :: Text
  , originalLine :: Int
  , finalLine    :: Int
  , author       :: Text
  , filename     :: Text
  } deriving (Show)

type Parser = Parsec Void Text

countLines :: String -> IO ()
countLines dir = do
  files <- runCmd "git ls-files"
  blames <-
    mapConcurrently
      (\file -> runCmd $ "git blame --line-porcelain " ++ file)
      (Prelude.lines files)
  let names = Map.unionsWith (+) (map (parseBlame . T.pack) blames)
  printBlames names
  putStrLn "\n"
  where
    runCmd cmd = readCreateProcess (shell cmd) {cwd = Just dir} ""

parseBlame :: Text -> Map.Map Text Integer
parseBlame blame =
  case parse (many blameParser) "" blame of
    Left e       -> Map.empty -- TODO error handling
    Right blames -> Map.fromListWith (+) (map (\b -> (author b, 1)) blames)

printBlames :: Map.Map Text Integer -> IO ()
printBlames names =
  putDoc . vcat . map f . reverse . sortOn snd $ Map.toList names
  where
    f (author, count) =
      annotate
        (color Yellow)
        (pretty . T.justifyLeft 6 ' ' . T.pack $ show count) <+>
      annotate (color Red) (pretty author)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer = lexeme L.decimal

str = lexeme . string

takeAll = lexeme $ takeWhile1P Nothing (/= '\n')

blameParser :: Parser Blame
blameParser
  -- THE PORCELAIN FORMAT
  -- In this format, each line is output after a header; the header at the minimum has the first line which has:
  -- 40-byte SHA-1 of the commit the line is attributed to
 = do
  sha <- lexeme $ replicateM 40 asciiChar
  -- the line number of the line in the original file;
  originalLine <- integer
  -- the line number of the line in the final file;
  finalLine <- integer
  -- on a line that starts a group of lines from a different commit than the previous one,
  -- the number of lines in this group. On subsequent lines this field is absent.
  option 1 integer
  -- This header line is followed by the following information at least once for each commit:
  -- the author name ("author"), email ("author-mail"), time ("author-time"), and time zone ("author-tz"); similarly for committer.
  author <- str "author" >> takeAll
  str "author-mail" >> takeAll
  str "author-time" >> takeAll
  str "author-tz" >> takeAll
  str "committer" >> takeAll
  str "committer-mail" >> takeAll
  str "committer-time" >> takeAll
  str "committer-tz" >> takeAll
  str "summary" >> takeAll
  -- "previous" on non-commited files, "boundary" otherwise
  optional ((str "previous" >> takeAll) <|> str "boundary")
  -- the filename in the commit that the line is attributed to.
  filename <- str "filename" >> takeWhile1P Nothing (/= '\n')
  newline
  -- The contents of the actual line is output after the above header, prefixed by a TAB. This is to allow adding more header elements later.
  tab
  line <- lexeme $ takeWhileP Nothing (/= '\n')
  return $
    Blame
      { sha = T.pack sha
      , originalLine = originalLine
      , finalLine = finalLine
      , author = author
      , filename = filename
      }
