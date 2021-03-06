{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib where

import           Control.Applicative                       (empty)
import           Control.Concurrent.Async                  (mapConcurrently)
import           Control.Monad                             (replicateM)
import           Control.Parallel.Strategies               (parList, rseq,
                                                            using)
import           Data.Either                               (fromRight)
import           Data.List                                 (sortOn)
import           Data.Map.Strict                           (Map)
import qualified Data.Map.Strict                           as Map
import           Data.Maybe                                (catMaybes)
import qualified Data.Set                                  as Set
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Read                            (decimal)
import           Data.Void                                 (Void)
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
  { authorLines     :: Int
  , authorCommits   :: Int
  , authorFiles     :: Int
  , authorAdditions :: Int
  , authorDeletions :: Int
  } deriving (Show)

type Parser = Parsec Void Text

defaultAuthor :: Author
defaultAuthor = Author 0 0 0 0 0

instance Semigroup Author where
  a <> b =
    Author
      { authorLines = authorLines a + authorLines b
      , authorCommits = authorCommits a + authorCommits b
      , authorFiles = authorFiles a + authorFiles b
      , authorAdditions = authorAdditions a + authorAdditions b
      , authorDeletions = authorDeletions a + authorDeletions b
      }

instance Monoid Author where
  mempty = defaultAuthor

countCommits :: String -> IO (Map Text Author)
countCommits dir = Map.fromList . map (f . T.words) . T.lines <$> runCmd dir "git shortlog -s HEAD"
  where
    f (count:names) = (T.unwords names, defaultAuthor {authorCommits = textToInt count})
    f _ = error "git shortlog is not giving the correct result"

countChanges :: String -> IO (Map Text Author)
countChanges dir = do
  authors <- map (T.unwords . tail . T.words) . T.lines <$> runCmd dir "git shortlog -s HEAD"
  logs <-
    flip mapConcurrently authors $ \author ->
      map (f author . T.words) . T.lines <$>
      runCmd dir ("git log --pretty=tformat: --numstat --author=\"" ++ T.unpack author ++ "\"")
  return $ Map.fromListWith mappend $ concat logs
  where
    f author (addition:deletion:_) =
      (author, defaultAuthor {authorAdditions = textToInt addition, authorDeletions = textToInt deletion})
    f _ _ = error "git log error"

countLines :: String -> IO (Map Text Author)
countLines dir = do
  files <- T.lines <$> runCmd dir "git ls-files"
  blameFiles <-
    flip mapConcurrently files $ \file -> do
      filetype <- runCmd dir ("file -b -I " ++ T.unpack file)
      if fst (T.breakOn ";" filetype) == "text/plain" -- ignore non-text files, e.g. binary files
        then Just <$> runCmd dir ("git blame --line-porcelain " ++ T.unpack file)
        else return Nothing
  let blames = concat (parseBlame <$> catMaybes blameFiles `using` parList rseq)
      counts =
        Map.fromListWith
          (\a b -> (fst a + fst b, Set.union (snd a) (snd b)))
          (map (\b -> (author b, (1, Set.singleton $ filename b))) blames)
  return $ Map.map (\(count, fileSet) -> defaultAuthor {authorLines = count, authorFiles = Set.size fileSet}) counts

mergeAuthors :: [Map Text Author] -> Map Text Author
mergeAuthors = Map.unionsWith mappend

runCmd :: String -> String -> IO Text
runCmd dir cmd = do
  (_, out, _) <- readCreateProcessWithExitCode (shell cmd) {cwd = Just dir} ""
  return out

parseBlame :: Text -> [Blame]
parseBlame blame = either (const []) id (parse (many blameParser) "" blame)

printLines :: Map Text Author -> IO ()
printLines authors = do
  let title = ("Lines", "Adds", "Dels", "Commits", "Files", "Author")
      docs = title : (map f . sortOn (negate . authorLines . snd) $ Map.toList authors)
  putDoc . vcat . map g $ docs
  where
    f (author, Author {..}) =
      (ss authorLines, ss authorAdditions, ss authorDeletions, ss authorCommits, ss authorFiles, author)
    g (lines, additions, deletions, commits, files, author) =
      annotate (color Yellow) (pp lines) <+>
      annotate (color Yellow) (pp additions) <+>
      annotate (color Yellow) (pp deletions) <+>
      annotate (color Cyan) (pp commits) <+> annotate (color Blue) (pp files) <+> annotate (color Red) (pretty author)
    ss = T.pack . show
    pp = pretty . T.justifyRight 8 ' '

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

str :: Text -> Parser Text
str = lexeme . string

takeAll :: Parser Text
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
  lexeme $ takeWhileP Nothing (/= '\n')
  return $
    Blame {sha = T.pack sha, originalLine = originalLine, finalLine = finalLine, author = author, filename = filename}

textToInt :: Text -> Int
textToInt = fst . fromRight (0, "0") . decimal
