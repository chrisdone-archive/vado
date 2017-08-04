{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Char
import qualified Data.HashMap.Strict as HM
import           Data.Int
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.String
import           Data.Text ( Text)
import qualified Data.Text as T
import           Linear.V2 (V2(..))
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Client.Internal
import qualified Network.HTTP.Client.TLS as TLS
import           Network.URI
import qualified SDL as SDL
import qualified SDL.Cairo as Cairo
import qualified SDL.Cairo.Canvas as Canvas
import           SDL.Event as SDL
import           SDL.Vect
import           System.Environment
import qualified Text.HTML.DOM as DOM
import qualified Text.XML as XML

--------------------------------------------------------------------------------
-- Types

-- | Some content, either a block or inline element, or some text.
data Content
  = ElementContent !Text !Events !Style ![Content]
  | TextContent !Text

-- | Style for an element. Inheritable values are Maybes.
data Style = Style
  { styleMargin :: !Double
  , stylePadding :: !Double
  , styleDisplay :: !Display
  , styleWidth :: !(Maybe Double)
  , styleBackgroundColor :: !(Maybe Canvas.Color)
  , styleColor :: !(Maybe Canvas.Color)
  , styleFontWeight :: !(Maybe FontWeight)
  , styleFontSize :: !(Maybe Double)
  , styleLineHeight :: !(Maybe Double)
  , styleFontStyle :: !(Maybe FontStyle)
  } deriving (Show)

-- | Display style.
data Display = BlockDisplay | InlineDisplay
  deriving Show

-- | Font style.
data FontStyle = NormalStyle | ItalicStyle
  deriving (Show)

-- | Font weight.
data FontWeight = NormalWeight | BoldWeight
  deriving (Show)

-- | A box to be displayed.
data Box
  = RectBox !Canvas.Dim !(Maybe Canvas.Color)
  | TextBox !Events !TextBox

-- | A set of events that an element may handle.
data Events = Events
  { eventsClick :: Maybe ((URI -> IO ()) -> IO () -> IO ())
    -- ^ A handler accepting two arguments: either a loadUrl or a "do
    -- nothing" action. This was thrown together at the last minute to
    -- support clicking links.
  }

-- | A box of text to be rendered on the screen at the given
-- coordinates with the given style.
data TextBox = Text
  { textXY :: !(V2 Double)
  , textWH :: !(V2 Double)
  , textColor :: !Canvas.Color
  , textWeight :: !FontWeight
  , textStyle :: !FontStyle
  , textSize :: !Double
  , textText :: !Text
  } deriving (Show)

-- | A text size measurer.
newtype Measuring a =
  Measuring {runMeasuring :: Canvas.Canvas a}
  deriving (Monad, Functor, Applicative)

-- | Line-state. The state of rendering line-broken text.
data LS = LS
  { lsX :: Double
  , lsY :: Double
  , lsLineHeight :: Double
    -- ^ The line height gets increased every time we render a font
    -- larger than the last rendered thing.
  , lsMaxHeight :: Double
    -- ^ The maximum rendering height. This is just a small
    -- optimization to not render more than needed.
  }

--------------------------------------------------------------------------------
-- Main entry point and event handler

-- | Main entry point.
--
-- * Read in commandline argument with the URL.
-- * Make a request to the URL.
-- * Create an SDL window.
-- * Render the content.
--
main :: IO ()
main = do
  url:_ <- getArgs -- Get the URL.
  request0 <- HTTP.parseRequest (fromString url)
  content0 <- getContent request0
  -- Initialize SDL and create a window.
  SDL.initialize [SDL.InitVideo, SDL.InitTimer, SDL.InitEvents]
  window <-
    SDL.createWindow
      "Wish"
      SDL.defaultWindow
      { SDL.windowHighDPI = True
      , SDL.windowResizable = True
      , SDL.windowInitialSize = defaultWindowSize
      }
  -- Setup Cairo rendering on the window.
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.defaultRenderer {SDL.rendererType = SDL.SoftwareRenderer}
  texture0 <- Cairo.createCairoTexture renderer defaultWindowSize
  -- Render the page immediately.
  boxes0 <- rerender renderer texture0 content0 0
  -- Setup an event loop to handle events or quit. Re-render on each event.
  -- This set of arguments would be better collapsed into a record.
  let eloop texture scrollY boxes content request = do
        e <- waitEvent
        case eventPayload e of
          QuitEvent -> return ()
          WindowClosedEvent {} -> return ()
          MouseButtonEvent ev ->
            case mouseButtonEventMotion ev of
              Released -> do
                case find
                       (overlaps (mouseButtonEventPos ev) . snd)
                       (reverse (mapMaybe getClickEvent boxes)) of
                  Just (handler, _) ->
                    let loadUrl uri = do
                          request' <- setUriRelative request uri
                          putStrLn ("Downloading: " ++ show request')
                          content' <- getContent request'
                          let scrollY' = 0
                          boxes' <- rerender renderer texture content' scrollY'
                          eloop texture scrollY' boxes' content' request'
                        continue = eloop texture scrollY boxes content request
                    in handler loadUrl continue
                  _ -> eloop texture scrollY boxes content request
              _ -> eloop texture scrollY boxes content request
          MouseWheelEvent ev -> do
            let scrollY' =
                  min
                    0
                    (scrollY +
                     (let V2 _ y = mouseWheelEventPos ev
                      in fromIntegral y))
            boxes' <- rerender renderer texture content scrollY'
            eloop texture scrollY' boxes' content request
          WindowResizedEvent ev -> do
            texture' <-
              Cairo.createCairoTexture
                renderer
                (fmap fromIntegral (windowResizedEventSize ev))
            boxes' <- rerender renderer texture' content scrollY
            eloop texture' scrollY boxes' content request
          _ -> do
            eloop texture scrollY boxes content request
  eloop texture0 0 boxes0 content0 request0

--------------------------------------------------------------------------------
-- Web request

-- | Make a web request and then parse the HTML, convert it to the
-- more normalized Content type.
getContent :: HTTP.Request -> IO Content
getContent request = do
  -- Make a blocking request to the URL.
  manager <- HTTP.newManager TLS.tlsManagerSettings
  response <- HTTP.httpLbs request manager
  -- Parse the response body as possibly malformed HTML and convert that to an XML tree.
  let doc = XML.documentRoot (DOM.parseLBS (HTTP.responseBody response))
      content = elementToContent doc
  pure content

--------------------------------------------------------------------------------
-- Mouse events

-- | Does the point overlap the rectangle of text? Text is rendered
-- above the y, not below it. So that explains the calculation below.
overlaps :: Point V2 Int32 -> Canvas.Dim -> Bool
overlaps (P (V2 x0 y0)) (Canvas.D px py0 pw ph) =
  x >= px && y >= py && x <= px + pw && y <= py + ph
  where
    x = fromIntegral x0
    y = fromIntegral y0
    py = py0 - ph

-- | If an element has a click event, i.e. anchor elements, extract that.
getClickEvent :: Box -> Maybe (((URI -> IO ()) -> IO () -> IO ()), Canvas.Dim)
getClickEvent =
  \case
    TextBox events t -> do
      handler <- eventsClick events
      let V2 x y = textXY t
          V2 w h = textWH t
      pure (handler, Canvas.D x y w h)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Converting XML tree to a normalized content tree

-- | Normalize an XML tree of elements and text, possibly with
-- attributes like style and event handlers.
xmlToContent :: XML.Node -> Maybe Content
xmlToContent =
  \case
    XML.NodeElement element ->
      if elem
           (T.map toLower (XML.nameLocalName (XML.elementName element)))
           ignoreElements
        then Nothing
        else Just (elementToContent element)
    XML.NodeContent t ->
      if T.null (T.strip t)
        then Nothing
        else Just (TextContent (T.unwords (T.words t)))
    XML.NodeInstruction {} -> Nothing
    XML.NodeComment {} -> Nothing
  where
    ignoreElements = ["head", "script", "style", "br", "hr", "img", "input"]

-- | Convert an element to some content.
elementToContent :: XML.Element -> Content
elementToContent element =
  case HM.lookup name elementStyles of
    Nothing ->
      ElementContent
        (T.map toLower (XML.nameLocalName (XML.elementName element)))
        defaultEvents
        defaultStyle
        (mapMaybe xmlToContent (XML.elementNodes element))
    Just style ->
      ElementContent
        (T.map toLower (XML.nameLocalName (XML.elementName element)))
        defaultEvents
        { eventsClick =
            if name == "a"
              then Just
                     (\loadUrl continue ->
                        (case M.lookup "href" (XML.elementAttributes element) of
                           Nothing -> do
                             continue
                           Just url ->
                             case parseURIReference (T.unpack url) of
                               Nothing -> do
                                 continue
                               Just uri -> do
                                 loadUrl uri))
              else Nothing
        }
        style
        (mapMaybe xmlToContent (XML.elementNodes element))
  where
    name = XML.nameLocalName (XML.elementName element)

--------------------------------------------------------------------------------
-- Laying out content to a list of absolutely-positioned boxes

-- | Convert an element to boxes.
blockToBoxes :: LS -> Double -> Events -> Style -> [Content] -> Measuring (LS, [Box])
blockToBoxes ls0 maxWidth events0 inheritedStyle nodes0 =
  if lsY ls0 > lsMaxHeight ls0
    then pure (ls0, [])
    else fmap
           (second concat)
           (mapAccumM
              (\ls content ->
                 case content of
                   TextContent t ->
                     textToBoxes ls events0 inheritedStyle maxWidth t
                   ElementContent _ events style nodes ->
                     case styleDisplay style of
                       InlineDisplay ->
                         inlineToBoxes
                           ls
                           maxWidth
                           events
                           (mergeStyles inheritedStyle style)
                           nodes
                       BlockDisplay ->
                         blockToBoxes
                           ls
                           { lsY = lsY ls + lsLineHeight ls
                           , lsLineHeight = 0
                           , lsX = 0
                           }
                           maxWidth
                           events
                           (mergeStyles inheritedStyle style)
                           nodes)
              ls0
              nodes0)

-- | Convert an element to boxes.
inlineToBoxes :: LS -> Double -> Events -> Style -> [Content] -> Measuring (LS, [Box])
inlineToBoxes ls0 maxWidth events0 inheritedStyle nodes0 = do
  if lsY ls0 > lsMaxHeight ls0
    then pure (ls0, [])
    else fmap
           (second concat)
           (mapAccumM
              (\ls content ->
                 case content of
                   TextContent t ->
                     textToBoxes ls events0 inheritedStyle maxWidth t
                   ElementContent _ events style nodes ->
                     inlineToBoxes
                       ls
                       maxWidth
                       events
                       (mergeStyles inheritedStyle style)
                       nodes)
              ls0
              nodes0)

-- | Layout text word-by-word with line-breaking.
textToBoxes :: LS -> Events -> Style -> Double -> Text -> Measuring (LS, [Box])
textToBoxes ls0 events style maxWidth t = do
  mapAccumM
    (\ls word -> do
       wh@(V2 width _) <- measure word
       let ls' =
             if lsX ls + width > maxWidth
               then ls
                    {lsX = 0, lsY = lsY ls + lsLineHeight ls, lsLineHeight = lineHeight}
               else ls {lsLineHeight = max (lsLineHeight ls) lineHeight}
       fe <- extents
       pure
         ( ls' {lsX = (lsX ls' + width + space), lsY = lsY ls'}
         , TextBox
             events
             (Text
              { textXY = V2 (lsX ls') (lsY ls' + Canvas.fontExtentsHeight fe)
              , textWH = wh
              , textColor = color
              , textWeight = weight
              , textSize = fontSize
              , textText = word
              , textStyle = fontStyle
              })))
    ls0
    (T.words t)
  where
    fontStyle = fromMaybe defaultFontStyle (styleFontStyle style)
    space = (fontSize / 2)
    color = fromMaybe defaultColor (styleColor style)
    fontSize = fromMaybe defaultFontSize (styleFontSize style)
    lineHeight =
      fontSize * (fromMaybe defaultLineHeight (styleLineHeight style))
    weight = fromMaybe defaultWeight (styleFontWeight style)
    bold =
      case weight of
        NormalWeight -> False
        BoldWeight -> True
    italic =
      case fontStyle of
        ItalicStyle -> True
        _ -> False
    measure w =
      Measuring
        (do Canvas.textFont (Canvas.Font defaultFontFace fontSize bold italic)
            Canvas.textSize (T.unpack w))
    extents = Measuring Canvas.fontExtents

--------------------------------------------------------------------------------
-- SDL rendering to the canvas

-- | Re-render the canvas. The boxes are computed by blockToBoxes,
-- this function simply renders the boxes that it's told to render.
--
-- Later, it might calculate the height of the document, then create a
-- fresh texture which it could scroll simply by passing extra
-- arguments to copy of a region to copy offset by some Y.
rerender :: SDL.Renderer -> SDL.Texture -> Content -> Double -> IO [Box]
rerender renderer texture content scrollY = do
  boxes <-
    Canvas.withCanvas texture $ do
      Canvas.background $ Canvas.rgb 255 255 255
      (V2 width height) <- Canvas.getCanvasSize
      (_, boxes) <-
        runMeasuring
          (blockToBoxes
             (LS
              {lsX = 0, lsY = scrollY, lsLineHeight = 0, lsMaxHeight = height})
             width
             defaultEvents
             defaultStyle {styleWidth = Just width}
             [content])
      mapM_
        (\box ->
           case box of
             RectBox dim mcolor -> do
               case mcolor of
                 Just color -> do
                   Canvas.fill color
                   Canvas.rect dim
                 Nothing -> pure ()
             TextBox _ text -> do
               Canvas.stroke (textColor text)
               Canvas.textFont
                 (Canvas.Font
                    "Arial"
                    (textSize text)
                    (case textWeight text of
                       BoldWeight -> True
                       NormalWeight -> False)
                    (case textStyle text of
                       ItalicStyle -> True
                       NormalStyle -> False))
               Canvas.textBaseline (T.unpack (textText text)) (textXY text))
        boxes
      pure boxes
  SDL.copy renderer texture Nothing Nothing
  SDL.present renderer
  pure boxes

--------------------------------------------------------------------------------
-- Styles

-- | Merge the inherited style and the element style.
mergeStyles :: Style -> Style -> Style
mergeStyles inherited element =
  Style
  { styleMargin = styleMargin element
  , stylePadding = stylePadding element
  , styleBackgroundColor =
      styleBackgroundColor element <|> styleBackgroundColor inherited
  , styleColor = styleColor element <|> styleColor inherited
  , styleFontWeight = styleFontWeight element <|> styleFontWeight inherited
  , styleDisplay = styleDisplay element
  , styleFontSize = styleFontSize element <|> styleFontSize inherited
  , styleFontStyle = styleFontStyle element <|> styleFontStyle inherited
  , styleWidth = styleWidth element <|> styleWidth inherited
  , styleLineHeight = styleLineHeight element <|> styleLineHeight inherited
  }

-- | Default style.
defaultStyle :: Style
defaultStyle =
  Style
  { styleMargin = 0
  , stylePadding = 0
  , styleBackgroundColor = Nothing
  , styleColor = Nothing
  , styleFontWeight = Nothing
  , styleDisplay = BlockDisplay
  , styleFontSize = Nothing
  , styleWidth = Nothing
  , styleLineHeight = Nothing
  , styleFontStyle = Nothing
  }

-- | Default stylings for standard HTML elements.
elementStyles :: HM.HashMap Text Style
elementStyles =
  HM.fromList
    ([ ( T.pack ("h" ++ show n)
       , defaultStyle
         {styleFontSize = Just size, styleFontWeight = Just BoldWeight})
     | (n :: Int, size :: Double) <- zip [1 .. 6] [40, 35, 20, 25, 20, 18]
     ] ++
     [ inline' "a" (\s -> s {styleColor = Just (Canvas.rgb 0 0 255)})
     , inline "b"
     , inline "big"
     , inline "i"
     , inline "small"
     , inline "tt"
     , inline "abbr"
     , inline "acronym"
     , inline "cite"
     , inline "code"
     , inline "dfn"
     , inline' "em" (\s -> s {styleFontStyle = Just ItalicStyle})
     , inline "kbd"
     , inline' "strong" (\s -> s {styleFontWeight = Just BoldWeight})
     , inline "samp"
     , inline "time"
     , inline "var"
     , inline "bdo"
     , inline "br"
     , inline "img"
     , inline "map"
     , inline "object"
     , inline "q"
     , inline "script"
     , inline "span"
     , inline "sub"
     , inline "sup"
     , inline "button"
     , inline "input"
     , inline "label"
     , inline "select"
     , inline "textarea"
     ])
  where
    inline name = (name, defaultStyle {styleDisplay = InlineDisplay})
    inline' name f = (name, f (defaultStyle {styleDisplay = InlineDisplay}))

-- | Default text color.
defaultColor :: Canvas.Color
defaultColor = Canvas.rgb 0 0 0

-- | Default font-size.
defaultFontSize :: Double
defaultFontSize = 15

defaultWeight :: FontWeight
defaultWeight = NormalWeight

defaultFontStyle :: FontStyle
defaultFontStyle = NormalStyle

defaultLineHeight :: Double
defaultLineHeight = 1.5

defaultWindowSize :: Num n => V2 n
defaultWindowSize = V2 800 600

defaultFontFace :: String
defaultFontFace = "Arial"

defaultEvents :: Events
defaultEvents = Events Nothing

--------------------------------------------------------------------------------
-- Utilities

-- | Map over the list, accumulating a state.
mapAccumM :: Monad m => (state -> a -> m (state, b)) -> state -> [a] -> m (state, [b])
mapAccumM f state0 xs =
  fmap
    (second reverse)
    (foldM
       (\(state, ys) x -> do
          (state', y) <- f state x
          pure (state', y : ys))
       (state0, [])
       xs)
