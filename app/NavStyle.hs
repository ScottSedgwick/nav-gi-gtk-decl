module NavStyle (styles, TextShow, tshow, CssClasses(..)) where

import Data.ByteString    (ByteString)
import Data.Char          (toLower)
import Data.Text.Encoding (encodeUtf8)
import Data.Text          (Text, pack)
import Data.Text.Lazy     (unpack)
import Clay

styles :: ByteString
styles = getStyle navStyle

getStyle :: Css -> ByteString
getStyle = encodeUtf8 . pack . unpack . render

class TextShow a where
  tshow :: a -> Text

data CssClasses = Selected | Red | Green | Blue | Yellow deriving (Eq, Show)

instance TextShow CssClasses where
  tshow = pack . fmap toLower . show

navStyle :: Css
navStyle = do
  button ? do
    border solid (px 2) gray 
    fontWeight (weight 800)
  button # byClass (tshow Selected) ? do
    background white
    border solid (px 2) black
  -- -- Specific color classes:
  button # byClass (tshow Red) ? 
    color red
  button # byClass (tshow Green) ? 
    color green
  button # byClass (tshow Blue) ? 
    color blue
  button # byClass (tshow Yellow) ? 
    color goldenrod