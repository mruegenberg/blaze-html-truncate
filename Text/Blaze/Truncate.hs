module Text.Blaze.Truncate(truncateHtml) where

import Text.Blaze(Markup)
import Text.Blaze.Internal(MarkupM(..),ChoiceString(..),StaticString(..))
import qualified Data.Text as T
import qualified Data.ByteString as B
import GHC.Exts (IsString (..))
import Prelude
import qualified Text.HTML.Truncate as HTML

data Tagged a = Tagged Int a
instance Functor Tagged where
    fmap f (Tagged n a) = Tagged n (f a)

-- | Truncate the given HTML to a certain length, preserving tags. Returns the truncated Html or `Nothing` if no truncation occured.
--   Words are preserved, so if the truncated text ends within some word, that whole word is cut.
truncateHtml :: Int    -- ^ The amount of characters (not counting tags) which the truncated text should have at most
             -> Markup   -- ^ The HTML to truncate
             -> Maybe Markup  -- ^ `Just` the truncated HTML or `Nothing` if no truncation occured
truncateHtml n html = case go n html of Tagged n' html' -> if n' <= 0 then filterEmptyTags html' else Nothing
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
        
        -- filter _trailing_ empty tags
        filterEmptyTags :: MarkupM a -> Maybe (MarkupM b)
        filterEmptyTags (Parent t open close content) = fmap (Parent t open close) (filterEmptyTags content)
        filterEmptyTags (Leaf _ _ _) = Nothing
        filterEmptyTags (AddAttribute t key value h) = fmap (AddAttribute t key value) (filterEmptyTags h)
        filterEmptyTags (AddCustomAttribute key value h) = fmap (AddCustomAttribute key value) (filterEmptyTags h)
        filterEmptyTags (Append h1 h2) = case filterEmptyTags h2 of
          Nothing  -> filterEmptyTags h1
          Just h2' -> Just (Append h1 h2')
        filterEmptyTags Empty = Nothing
        filterEmptyTags (Content content) = if (length' content) == 0 then Nothing else Just (Content content)

length' :: ChoiceString -> Int
length' (Static str) = length ((getString str) "")
length' (String str) = length str
length' (Text str) = T.length str
length' (ByteString str) = B.length str
length' (PreEscaped str) = length' str
length' (External _) = 0 -- note: these should not be truncated, so the behavior is a bit special here
length' (AppendChoiceString str1 str2) = length' str1 + length' str2
length' EmptyChoiceString = 0

truncateChoiceString :: Int -> ChoiceString -> Tagged ChoiceString
truncateChoiceString i _ | i <= 0 = Tagged 0 EmptyChoiceString
truncateChoiceString i (Static str) = case HTML.truncateStringLike i ((getString str) "") of
  (i',str') -> Tagged (max 0 i') $ Static (fromString str')
truncateChoiceString i (String str)  = case HTML.truncateStringLike i str of 
  (i',str') -> Tagged (max 0 i') $ String str'
truncateChoiceString i (Text str)  = case HTML.truncateStringLike i str of 
  (i',str') -> Tagged (max 0 i') $ Text str'
truncateChoiceString i (ByteString str)  = case HTML.truncateStringLike i str of 
  (i',str') -> Tagged (max 0 i') $ ByteString str'
-- truncateChoiceString i (ByteString str) = case B.take i str of
--    str' -> Tagged (max 0 (i - B.length str')) $ ByteString str'
truncateChoiceString i (PreEscaped str) = case truncateChoiceStringPreEscaped i str of
  Tagged i' str' -> Tagged (max 0 i') $ PreEscaped str'
truncateChoiceString i str@(External _)   = Tagged i str
truncateChoiceString i (AppendChoiceString str1 str2) = 
  case truncateChoiceString i str1 of 
    Tagged i' str1' -> case truncateChoiceString i' str2 of
      Tagged i'' str2' -> Tagged (max 0 i'') $ AppendChoiceString str1' str2'
truncateChoiceString i EmptyChoiceString = Tagged i EmptyChoiceString

truncateChoiceStringPreEscaped :: Int -> ChoiceString -> Tagged ChoiceString
truncateChoiceStringPreEscaped i _ | i <= 0 = Tagged 0 EmptyChoiceString
truncateChoiceStringPreEscaped i (Static str) = case HTML.truncateHtml' i ((getString str) "") of
   (i',str') -> Tagged (max 0 i') $ Static (fromString str')
truncateChoiceStringPreEscaped i (String str)  = case HTML.truncateHtml' i str of 
  (i',str') -> Tagged (max 0 i') $ String str'
truncateChoiceStringPreEscaped i (Text str)  = case HTML.truncateHtml' i str of 
  (i',str') -> Tagged (max 0 i') $ Text str'
truncateChoiceStringPreEscaped i (ByteString str)  = case HTML.truncateHtml' i str of 
  (i',str') -> Tagged (max 0 i') $ ByteString str'
-- truncateChoiceStringPreEscaped i (ByteString str) = case B.take i str of
--    str' -> Tagged (max 0 (i - B.length str')) $ ByteString str'
truncateChoiceStringPreEscaped i (PreEscaped str) = case truncateChoiceStringPreEscaped i str of
  Tagged i' str' -> Tagged (max 0 i') $ PreEscaped str'
truncateChoiceStringPreEscaped i str@(External _)   = Tagged i str
truncateChoiceStringPreEscaped i (AppendChoiceString str1 str2) = 
  case truncateChoiceStringPreEscaped i str1 of 
    Tagged i' str1' -> case truncateChoiceStringPreEscaped i' str2 of
      Tagged i'' str2' -> Tagged (max 0 i'') $ AppendChoiceString str1' str2'
truncateChoiceStringPreEscaped i EmptyChoiceString = Tagged i EmptyChoiceString