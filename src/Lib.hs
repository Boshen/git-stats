{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Applicative                       (empty)
import           Control.Concurrent.Async                  (mapConcurrently)
import           Control.Monad
import           Data.Either                               (fromRight)
import           Data.List                                 (concatMap, sortOn)
import qualified Data.Map.Strict                           as Map
import           Data.Maybe
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Read                            (decimal)
import           Data.Void
import           System.Process                            hiding (readCreateProcessWithExitCode)
import           System.Process.Text                       (readCreateProcessWithExitCode)
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

data Author = Author
  { authorLines   :: Int
  , authorCommits :: Int
  } deriving (Show)

type Parser = Parsec Void Text

countCommits :: String -> IO (Map.Map Text Author)
countCommits dir =
  Map.fromList . map (f . T.words) . T.lines <$>
  runCmd dir "git shortlog -sn HEAD"
  where
    f (count:names) =
      ( T.unwords names
      , Author
          { authorLines = 0
          , authorCommits = fst . fromRight (0, "0") . decimal $ count
          })

countLines :: String -> IO (Map.Map Text Author)
countLines dir = do
  files <- T.lines <$> runCmd dir "git ls-files"
  blameFiles <-
    mapConcurrently
      (\file -> do
         filetype <- runCmd dir $ "file -b -I " ++ T.unpack file
         if fst (T.breakOn ";" filetype) == "text/plain"
           then do
             blameFile <-
               runCmd dir $ "git blame --line-porcelain " ++ T.unpack file
             return $ Just blameFile
           else return Nothing)
      files
  let blames = concatMap parseBlame (catMaybes blameFiles)
      counts = Map.fromListWith (+) $ map (\b -> (author b, 1)) blames
  return $
    Map.map (\count -> Author {authorLines = count, authorCommits = 0}) counts

mergeAuthors ::
     Map.Map Text Author -> Map.Map Text Author -> Map.Map Text Author
mergeAuthors = Map.unionWith f
  where
    f a b =
      Author
        { authorLines = authorLines a + authorLines b
        , authorCommits = authorCommits a + authorCommits b
        }

runCmd :: String -> String -> IO Text
runCmd dir cmd = do
  (_, out, _) <- readCreateProcessWithExitCode (shell cmd) {cwd = Just dir} ""
  return out

parseBlame :: Text -> [Blame]
parseBlame blame =
  case parse (many blameParser) "" blame of
    Left e       -> [] -- TODO error handling
    Right blames -> blames

printLines :: Map.Map Text Author -> IO ()
printLines authors = do
  let docs = map f . reverse . sortOn (authorLines . snd) $ Map.toList authors
  putDoc . vcat . map g $ ("Lines", "Commits", "Author") : docs
  where
    f (author, Author authorLines authorCommits) =
      (T.pack . show $ authorLines, T.pack . show $ authorCommits, author)
    g (lines, commits, author) =
      annotate (color Yellow) (pretty . T.justifyRight 6 ' ' $ lines) <+>
      annotate (color Blue) (pretty . T.justifyRight 6 ' ' $ commits) <+>
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
