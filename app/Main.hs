{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Async ( async )
import Control.Monad ( forM_, void )
import Data.ByteString ( ByteString )
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified GI.Gdk as Gdk
import GI.Gtk ( Box (..)
              , Button(..)
              , Label (..)
              , MenuBar (..)
              , MenuItem (..)
              , Orientation (..)
              , Window (..)
              )
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

-- import Navlib
import NavStyle

data Screen = DR 
            | GC
            deriving (Eq, Show)

data State = State { stMessage :: Text
                   , stSelected :: Int 
                   , stScreen :: Screen
                   } deriving (Eq, Show)

initState :: State
initState = State { stMessage = "Click a button in the menu."
                  , stSelected = 0 
                  , stScreen = DR
                  }

data Event = Open 
           | Save 
           | Help 
           | Closed 
           | MoveTo Int
           | ToScreen Screen

colors :: [CssClasses]
colors = [Red, Green, Blue, Yellow]

view' :: State -> AppView Event
view' (State msg si scr) = do
  let winSettings = [ #title := "Navigation"
                    , on #deleteEvent (const (True, Closed))
                    , #widthRequest := 400
                    , #heightRequest := 300
                    ]
  bin Window winSettings $ container Box [#orientation := OrientationVertical] $ do
        boxChild False False 0 $ container MenuBar [] $ do
          subMenu "File" $ do
            menuItem MenuItem [on #activate Open] $ widget Label [#label := "Open"]
            menuItem MenuItem [on #activate Save] $ widget Label [#label := "Save"]
          subMenu "Screen" $ do
            menuItem MenuItem [on #activate (ToScreen DR)] $ widget Label [#label := "Dead Reckoning"]
            menuItem MenuItem [on #activate (ToScreen GC)] $ widget Label [#label := "Great Circle"]
          subMenu "Help" $ 
            menuItem MenuItem [on #activate Help] $ widget Label [#label := "About"]
        boxChild True False 0 $ widget Label [#label := msg]
        case scr of
          DR -> 
            boxChild True False 10
              $ container Box [#orientation := OrientationHorizontal]
              $ forM_ (zip [0 ..] colors)
              $ \(i, color) ->
                  boxChild True False 10
                    $ let cs = if i == si then ["selected", tshow color] else [tshow color]
                      in  widget Button
                                [ #label := tshow color
                                --, on #enter (MoveTo i)
                                , on #clicked (MoveTo i)
                                , classes cs
                                ]
          GC -> 
            boxChild True False 10
              $ container Box [#orientation := OrientationHorizontal]
              $ boxChild True False 10
                  $ widget Button [ #label := "Great Circle"
                                  , on #clicked (ToScreen DR) ]


update' :: State -> Event -> Transition State Event
update' s Open   = Transition (s {stMessage = "Opening file..."}) (return Nothing)
update' s Save   = Transition (s {stMessage = "Saving file..."}) (return Nothing)
update' s Help   = Transition (s {stMessage = "There is no help"}) (return Nothing)
update' _ Closed = Exit
update' s (MoveTo i) 
  | i >= 0 && i < length colors = Transition (s {stSelected = i}) (return Nothing)
  | otherwise                   = Transition s (return Nothing)
update' s (ToScreen scr) = Transition (s {stScreen = scr}) (return Nothing)


runWithStyle :: Typeable event => ByteString -> App state event -> IO ()
runWithStyle style app = do
    void $ Gtk.init Nothing
    -- Set up screen and CSS provider
    screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
    p      <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromData p style
    Gtk.styleContextAddProviderForScreen screen p (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
    void . async $ do
      runLoop app
      Gtk.mainQuit
    Gtk.main

main :: IO ()
main = runWithStyle styles app
  where
    app = App
          { view         = view'
          , update       = update'
          , inputs       = []
          , initialState = initState
          }
