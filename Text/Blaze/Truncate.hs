module Text.Blaze.Truncate(truncateHtml) where

-- from Blaze 0.4 to 0.5: Html -> Markup; HtmlM -> MarkupM; AddCustomAttribute has an additional first argument
import Text.Blaze(Html)
import Text.Blaze.Internal(HtmlM(..),ChoiceString(..),StaticString(..))
import Data.Char
import qualified Data.Text as T
import qualified Data.ByteString as B
import GHC.Exts (IsString (..))
import Data.List(dropWhileEnd)

data Tagged a = Tagged Int a
instance Functor Tagged where
    fmap f (Tagged n a) = Tagged n (f a)
    
-- type Html = Markup
-- type HtmlM a = MarkupM a

-- | Truncate the given HTML to a certain length, preserving tags. Returns the truncated Html or `Nothing` if no truncation occured.
--   Words are preserved, so if the truncated text ends within some word, that whole word is cut.
truncateHtml :: Int    -- ^ The amount of characters (not counting tags) which the truncated text should have at most
             -> Html   -- ^ The HTML to truncate
             -> Maybe Html  -- ^ `Just` the truncated HTML or `Nothing` if no truncation occured
truncateHtml n html = case go n html of Tagged n' html' -> if n' /= n then Just html' else Nothing
    where
        go :: Int -> HtmlM b -> Tagged (HtmlM b)
        go i (Parent t open close content) = fmap (Parent t open close) (go i content)
        go i (Leaf t begin end) = Tagged i (Leaf t begin end)
        go i (AddAttribute t key value h) = fmap (AddAttribute t key value) (go i h)
        go i (AddCustomAttribute t key value h) = fmap (AddCustomAttribute t key value) (go i h)
        go i (Append h1 h2) = case go i h1 of
            Tagged j h1' -> fmap (Append h1') (go j h2)
        go i Empty = Tagged i Empty
        go i (Content content) = fmap Content (truncateChoiceString i content)
        
splitAt' :: Int -> ChoiceString -> (ChoiceString, ChoiceString)
splitAt' i (Static str) = case splitAt i ((getString str) "") of (str',str'') -> (Static (fromString str'),Static (fromString str''))
splitAt' i (String str) = case splitAt i str of (str',str'') -> (String str',String str'')
splitAt' i (Text str) = case T.splitAt i str of (str',str'') -> (Text str',Text str'')
splitAt' i (ByteString str) = case B.splitAt i str of (str',str'') -> (ByteString str',ByteString str'')
splitAt' i (PreEscaped str) = case splitAt' i str of (str',str'') -> (PreEscaped str',PreEscaped str'')
splitAt' _ (External str) = (External str,External EmptyChoiceString) -- note: these should not be truncated, so the behavior is a bit special here
splitAt' i (AppendChoiceString str1 str2) = case splitAt' i str1 of
    (str1',str1'') -> 
        if empty' str1'' then case splitAt' (i - (length' str1')) str2 of
            (str2',str2'') -> (AppendChoiceString str1' str2',str2'')
        else (str1',AppendChoiceString str1'' str2)
splitAt' _ EmptyChoiceString = (EmptyChoiceString,EmptyChoiceString)

length' :: ChoiceString -> Int
length' (Static str) = length ((getString str) "")
length' (String str) = length str
length' (Text str) = T.length str
length' (ByteString str) = B.length str
length' (PreEscaped str) = length' str
length' (External str) = 0 -- note: these should not be truncated, so the behavior is a bit special here
length' (AppendChoiceString str1 str2) = length' str1 + length' str2
length' EmptyChoiceString = 0

empty' :: ChoiceString -> Bool
empty' (Static str) = null ((getString str) "")
empty' (String str) = null str
empty' (Text str) = T.null str
empty' (ByteString str) = B.null str
empty' (PreEscaped str) = empty' str
empty' (External str) = True -- note: these should not be truncated, so the behavior is a bit special here
empty' (AppendChoiceString str1 str2) = empty' str1 && empty' str2
empty' EmptyChoiceString = True

head' :: ChoiceString -> Char
head' (Static str) = head ((getString str) "")
head' (String str) = head str
head' (Text str) = T.head str
head' (ByteString str) = (head . show . B.head) str
head' (PreEscaped str) = head' str
head' (External str) = undefined -- note: these should not be truncated, so the behavior is a bit special here
head' (AppendChoiceString str1 str2) = if empty' str1 then head' str2 else head' str1
head' EmptyChoiceString = undefined

dropWhileEnd' :: (Char -> Bool) -> ChoiceString -> ChoiceString
dropWhileEnd' f (Static str) = Static (fromString (dropWhileEnd f ((getString str) "")))
dropWhileEnd' f (String str) = String (dropWhileEnd f str)
dropWhileEnd' f (Text str) = Text (T.dropWhileEnd f str)
dropWhileEnd' f (ByteString str) = ByteString (fst $ B.spanEnd (f . head . show) str) -- FIXME: inefficient
dropWhileEnd' f (PreEscaped str) = dropWhileEnd' f str
dropWhileEnd' f (External str) = External str -- note: these should not be truncated, so the behavior is a bit special here
dropWhileEnd' f (AppendChoiceString str1 str2) = case dropWhileEnd' f str2 of 
  str2' -> if empty' str2' then dropWhileEnd' f str1
          else (AppendChoiceString str1 str2')
dropWhileEnd' f EmptyChoiceString = EmptyChoiceString

truncateChoiceString :: Int -> ChoiceString -> Tagged ChoiceString
truncateChoiceString i str = case splitAt' i str of 
  (str',rst) -> if (empty' rst) || (isSpace $ head' rst)
               then Tagged (i - (length' str')) str'
               else case dropWhileEnd' (not. isSpace) (dropWhileEnd' isSpace str') of
                 str'' -> Tagged (i - length' str'') str''

