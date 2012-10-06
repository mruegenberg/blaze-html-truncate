module Text.Blaze.Truncate(truncateHtml) where

import Text.Blaze(Markup)
import Text.Blaze.Internal(MarkupM(..),ChoiceString(..),StaticString(..))
import Data.Char
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import GHC.Exts (IsString (..))
import Data.List(dropWhileEnd)
import Prelude
import Data.List(head)
import qualified Text.HTML.TagSoup as TS
import qualified Text.StringLike as SL

data Tagged a = Tagged Int a
instance Functor Tagged where
    fmap f (Tagged n a) = Tagged n (f a)

-- (inefficient `take` for StringLike)
splitAtSL :: SL.StringLike a => Int -> a -> (a,a)
splitAtSL i t | i <= 0 = (SL.empty,t)
splitAtSL i t = case SL.uncons t of
    Nothing -> (t,SL.empty)
    Just (chr,rst) -> case (splitAtSL (i - 1) rst) of
        (r,r') -> (SL.cons chr r,r')
        
dropWhileEndSL :: SL.StringLike a => (Char -> Bool) -> a -> a
dropWhileEndSL p = SL.fromString . (dropWhileEnd p) . SL.toString
    
lengthSL :: SL.StringLike a => a -> Int
lengthSL t' = lengthSL' t' 0
    where
        lengthSL' t n = case SL.uncons t of
            Nothing -> n
            Just (_,rst) -> lengthSL' rst (n + 1)

-- FIXME: ugly and slow
splitAtPreEscapedHtml :: SL.StringLike str => Int -> str -> (str,str)
splitAtPreEscapedHtml n txt = case go n 0 (TS.parseTags txt) of (a,b) -> (TS.renderTags a, TS.renderTags b)
   where
       go :: (SL.StringLike str) => Int -> Int -> [TS.Tag str] -> ([TS.Tag str],[TS.Tag str])
       go i openTgs tags | i <= 0 && openTgs <= 0 = ([],tags) -- keep running until all tags are closed
       go _ _ [] = ([],[])
       go i openTgs (t@(TS.TagPosition _ _) : ts) = case (go i openTgs ts) of (ts',ts'') -> (t: ts',ts'')
       go i openTgs (t@(TS.TagWarning _) : ts) = case (go i openTgs ts) of (ts',ts'') -> (t: ts',ts'')
       go i openTgs (t@(TS.TagComment _) : ts) = case (go i openTgs ts) of (ts',ts'') -> (t: ts',ts'')
       go i openTgs (t@(TS.TagOpen _ _)  : ts)	= case (go i (openTgs + 1) ts) of (ts',ts'') -> (t: ts',ts'')
       go i openTgs (t@(TS.TagClose _) : ts)	= case (go i (openTgs - 1) ts) of (ts',ts'') -> (t: ts',ts'')
       go i openTgs ((TS.TagText str) : ts) = case splitAtSL i str of
           (str',str'') -> case (go (i - (lengthSL str')) openTgs ts) of 
               (ts',ts'') -> ((TS.TagText str') : ts', (TS.TagText str'') : ts'')
               
dropWhileEndPreEscapedHtml :: SL.StringLike str => (Char -> Bool) -> str -> str
dropWhileEndPreEscapedHtml p txt = (TS.renderTags . reverse . (go 0) . reverse . TS.parseTags) txt
   where
       go :: (SL.StringLike str) => Int -> [TS.Tag str] -> [TS.Tag str]
       go _ [] = []
       go openTgs (t@(TS.TagPosition _ _) : ts) = t : (go openTgs ts)
       go openTgs (t@(TS.TagWarning _) : ts) = t : (go openTgs ts)
       go openTgs (t@(TS.TagComment _) : ts) = t : (go openTgs ts)
       go openTgs (t@(TS.TagOpen _ _)  : ts)	= t : (go (openTgs - 1) ts)
       go openTgs (t@(TS.TagClose _) : ts)	= t : (go (openTgs + 1) ts)
       go openTgs ((TS.TagText str) : ts) = 
         if SL.strNull str then ts
         else case dropWhileEndSL p str of
           str' -> if not (SL.strNull str') then (TS.TagText str') : ts
                  else go openTgs ts

-- | Truncate the given HTML to a certain length, preserving tags. Returns the truncated Html or `Nothing` if no truncation occured.
--   Words are preserved, so if the truncated text ends within some word, that whole word is cut.
truncateHtml :: Int    -- ^ The amount of characters (not counting tags) which the truncated text should have at most
             -> Markup   -- ^ The HTML to truncate
             -> Maybe Markup  -- ^ `Just` the truncated HTML or `Nothing` if no truncation occured
truncateHtml n html = case go n html of Tagged n' html' -> if n' <= 0 then Just html' else Nothing
    where
        go :: Int -> MarkupM b -> Tagged (MarkupM b)
        go i (Parent t open close content) = fmap (Parent t open close) (go i content)
        go i (Leaf t begin end) = Tagged i (Leaf t begin end)
        go i (AddAttribute t key value h) = fmap (AddAttribute t key value) (go i h)
        go i (AddCustomAttribute key value h) = fmap (AddCustomAttribute key value) (go i h)
        go i (Append h1 h2) = case go i h1 of
          Tagged j h1' | j <= 0 -> Tagged j (Append h1' Empty) -- FIXME: we actually want to return just Tagged j h1', but can't due to a type error
          Tagged j h1' -> fmap (Append h1') (go j h2)
        go i Empty = Tagged i Empty
        go i (Content content) = fmap Content (truncateChoiceString i content)
        
splitAtPreEscaped' :: Int -> ChoiceString -> (ChoiceString, ChoiceString)
splitAtPreEscaped' i str | i <= 0 = (EmptyChoiceString, str)
splitAtPreEscaped' i (Static str) = case splitAtPreEscapedHtml i ((getString str) "") of (str',str'') -> (Static (fromString str'),Static (fromString str''))
splitAtPreEscaped' i (String str) = case splitAtPreEscapedHtml i str of (str',str'') -> (String str',String str'')
splitAtPreEscaped' i (Text str) = case splitAtPreEscapedHtml i str of (str',str'') -> (Text str',Text str'')
splitAtPreEscaped' i (ByteString str) = case splitAtPreEscapedHtml i str of (str',str'') -> (ByteString str',ByteString str'')
splitAtPreEscaped' i (PreEscaped str) = case splitAt' i str of (str',str'') -> (PreEscaped str',PreEscaped str'')
splitAtPreEscaped' _ (External str) = (External str,External EmptyChoiceString) -- note: these should not be truncated, so the behavior is a bit special here
splitAtPreEscaped' i (AppendChoiceString str1 str2) = case splitAt' i str1 of
  (str1',str1'') -> if not (empty' str1'') then (str1', AppendChoiceString str1'' str2)
                   else case splitAt' (i - (length' str1)) str2 of
                     (str2',str2'') -> (AppendChoiceString str1' str2', str2'')
splitAtPreEscaped' _ EmptyChoiceString = (EmptyChoiceString,EmptyChoiceString)
        
splitAt' :: Int -> ChoiceString -> (ChoiceString, ChoiceString)
splitAt' i str | i <= 0 = (EmptyChoiceString, str)
splitAt' i (Static str) = case splitAt i ((getString str) "") of (str',str'') -> (Static (fromString str'),Static (fromString str''))
splitAt' i (String str) = case splitAt i str of (str',str'') -> (String str',String str'')
splitAt' i (Text str) = case T.splitAt i str of (str',str'') -> (Text str',Text str'')
splitAt' i (ByteString str) = case B.splitAt i str of (str',str'') -> (ByteString str',ByteString str'')
splitAt' i (PreEscaped str) = case splitAtPreEscaped' i str of (str',str'') -> (PreEscaped str',PreEscaped str'')
splitAt' _ (External str) = (External str,External EmptyChoiceString) -- note: these should not be truncated, so the behavior is a bit special here
splitAt' i (AppendChoiceString str1 str2) = case splitAt' i str1 of
  (str1',str1'') -> if not (empty' str1'') then (str1', AppendChoiceString str1'' str2)
                   else case splitAt' (i - (length' str1)) str2 of
                     (str2',str2'') -> (AppendChoiceString str1' str2', str2'')
splitAt' _ EmptyChoiceString = (EmptyChoiceString,EmptyChoiceString)

length' :: ChoiceString -> Int
length' (Static str) = length ((getString str) "")
length' (String str) = length str
length' (Text str) = T.length str
length' (ByteString str) = B.length str
length' (PreEscaped str) = length' str
length' (External _) = 0 -- note: these should not be truncated, so the behavior is a bit special here
length' (AppendChoiceString str1 str2) = length' str1 + length' str2
length' EmptyChoiceString = 0

empty' :: ChoiceString -> Bool
empty' (Static str) = null ((getString str) "")
empty' (String str) = null str
empty' (Text str) = T.null str
empty' (ByteString str) = B.null str
empty' (PreEscaped str) = empty' str
empty' (External _) = True -- note: these should not be truncated, so the behavior is a bit special here
empty' (AppendChoiceString str1 str2) = empty' str1 && empty' str2
empty' EmptyChoiceString = True

head' :: ChoiceString -> Char
head' (Static str) = head ((getString str) "")
head' (String str) = head str
head' (Text str) = T.head str
head' (ByteString str) = (head . show . B.head) str
head' (PreEscaped str) = head' str
head' (External _) = undefined -- note: these should not be truncated, so the behavior is a bit special here
head' (AppendChoiceString str1 str2) = if empty' str1 then head' str2 else head' str1
head' EmptyChoiceString = undefined

dropWhileEndPreEscaped' :: (Char -> Bool) -> ChoiceString -> ChoiceString
dropWhileEndPreEscaped' f (Static str) = Static (fromString (dropWhileEndPreEscapedHtml f ((getString str) "")))
dropWhileEndPreEscaped' f (String str) = String (dropWhileEndPreEscapedHtml f str)
dropWhileEndPreEscaped' f (Text str) = Text (dropWhileEndPreEscapedHtml f str)
dropWhileEndPreEscaped' f (ByteString str) = ByteString (dropWhileEndPreEscapedHtml f str) -- FIXME: inefficient
dropWhileEndPreEscaped' f (PreEscaped str) = PreEscaped (dropWhileEndPreEscaped' f str)
dropWhileEndPreEscaped' f (External str) = External str -- note: these should not be truncated, so the behavior is a bit special here
dropWhileEndPreEscaped' f (AppendChoiceString str1 str2) = case dropWhileEndPreEscaped' f str2 of 
  str2' -> if empty' str2' then dropWhileEndPreEscaped' f str1
          else (AppendChoiceString str1 str2')
dropWhileEndPreEscaped' _ EmptyChoiceString = EmptyChoiceString

dropWhileEnd' :: (Char -> Bool) -> ChoiceString -> ChoiceString
dropWhileEnd' f (Static str) = Static (fromString (dropWhileEnd f ((getString str) "")))
dropWhileEnd' f (String str) = String (dropWhileEnd f str)
dropWhileEnd' f (Text str) = Text (T.dropWhileEnd f str)
dropWhileEnd' f (ByteString str) = ByteString (fst $ B.spanEnd (f . head . show) str) -- FIXME: inefficient
dropWhileEnd' f (PreEscaped str) = PreEscaped (dropWhileEndPreEscaped' f str)
dropWhileEnd' _ (External str) = External str -- note: these should not be truncated, so the behavior is a bit special here
dropWhileEnd' f (AppendChoiceString str1 str2) = case dropWhileEnd' f str2 of 
  str2' -> if empty' str2' then dropWhileEnd' f str1
          else (AppendChoiceString str1 str2')
dropWhileEnd' _ EmptyChoiceString = EmptyChoiceString

truncateChoiceString :: Int -> ChoiceString -> Tagged ChoiceString
truncateChoiceString i str = case splitAt' i str of 
  (str',rst) -> if (empty' rst) || (isSpace $ head' rst)
               then Tagged (i - (length' str')) str'
               else case dropWhileEnd' (not. isSpace) (dropWhileEnd' isSpace str') of
                 str'' -> Tagged (i - length' str'') str''
