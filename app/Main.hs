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
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import           Data.Char
import qualified Data.HashMap.Strict as HM
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
import qualified SDL.Image as Image
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
  | ImageContent !Text !(Maybe (V2 Double)) !(Maybe SDL.Surface) -- !(Maybe Text)

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
  | ImageBox !Canvas.Dim SDL.Surface
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
  Measuring {runMeasuring :: ReaderT Double Canvas.Canvas a}
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

-- | State for the event handler.
data EV = EV
  { evRequest :: Request
  , evContent :: Content
  , evBoxes :: [Box]
  , evScrollY :: Double
  , evTexture :: SDL.Texture
  , evRenderer :: SDL.Renderer
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
      (T.concat ["Vado - ", T.pack url])
      SDL.defaultWindow
      { SDL.windowHighDPI = True
      , SDL.windowResizable = True
      , SDL.windowInitialSize = defaultWindowSize
      }
  -- Setup Cairo rendering on the window.
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  Just (SDL.Rectangle _ viewport@(V2 w h)) <-
    SDL.get (SDL.rendererViewport renderer)
  texture0 <- Cairo.createCairoTexture renderer viewport
  -- Render the page immediately.
  let ev0 = EV request0 content0 [] 0 texture0 renderer
  let scale@(_, scaley) =
        let (V2 w0 h0) = defaultWindowSize
        in (fromIntegral w / w0, fromIntegral h / h0)
  boxes0 <- rerender scaley ev0
  -- Setup an event loop to handle events or quit. Re-render on each event.
  -- This set of arguments would be better collapsed into a record.
  eloop scale ev0 {evBoxes = boxes0}

-- | Event loop.
eloop :: (Double, Double) -> EV -> IO ()
eloop scale@(scalex, scaley) ev = do
  event <- waitEvent
  case eventPayload event of
    QuitEvent -> return ()
    WindowClosedEvent {} -> return ()
    MouseButtonEvent e ->
      case mouseButtonEventMotion e of
        Released -> do
          case find
                 (overlaps
                    (let P (V2 w h) = mouseButtonEventPos e
                     in P
                          (V2
                             (fromIntegral w * scalex)
                             (fromIntegral h * scaley))) .
                  snd)
                 (reverse (mapMaybe getClickEvent (evBoxes ev))) of
            Just (handler, _) ->
              let loadUrl uri = do
                    request' <- setUriRelative (evRequest ev) uri
                    putStrLn ("Downloading: " ++ show request')
                    content' <- getContent request'
                    let scrollY' = 0
                    boxes' <-
                      rerender
                        scaley
                        ev {evContent = content', evScrollY = scrollY'}
                    eloop
                      scale
                      ev
                      { evScrollY = scrollY'
                      , evBoxes = boxes'
                      , evContent = content'
                      , evRequest = request'
                      }
                  continue = eloop scale ev
              in handler loadUrl continue
            _ -> eloop scale ev
        _ -> eloop scale ev
    MouseWheelEvent e -> do
      let ev' =
            ev
            { evScrollY =
                min
                  0
                  (evScrollY ev +
                   (let V2 _ y = mouseWheelEventPos e
                    in fromIntegral y * scaley * 5))
            }
      boxes' <- rerender scaley ev'
      eloop scale ev' {evBoxes = boxes'}
    WindowResizedEvent e -> do
      texture' <-
        Cairo.createCairoTexture
          (evRenderer ev)
          (fmap fromIntegral (windowResizedEventSize e))
      let ev' = ev {evTexture = texture'}
      boxes' <- rerender scaley ev'
      eloop scale ev' {evBoxes = boxes'}
    _ -> do
      eloop scale ev

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
  fetchImages (manager, request) content

fetchImages :: (HTTP.Manager, HTTP.Request) -> Content -> IO Content
fetchImages http (ElementContent a b c elements) = do
  elements' <- mapM (fetchImages http) elements
  return (ElementContent a b c elements')
fetchImages (manager, req) (ImageContent src dim0 Nothing) = do
  let Just uri = parseURIReference (T.unpack src)
  req' <- setUriRelative req uri
  putStrLn ("Downloading: " ++ show req')
  resp <- HTTP.httpLbs req' manager
  let body = B.toStrict $ HTTP.responseBody resp
  case Image.format body of
    Just _ -> do
      img <- Image.decode body
      V2 dx dy <- SDL.surfaceDimensions img
      let dim = dim0 <|> Just (V2 (fromIntegral dx) (fromIntegral dy))
      return $ ImageContent src dim (Just img)
    _ ->
      return $ ImageContent src dim0 Nothing
fetchImages _ t = return t

--------------------------------------------------------------------------------
-- Mouse events

-- | Does the point overlap the rectangle of text? Text is rendered
-- above the y, not below it. So that explains the calculation below.
overlaps :: Point V2 Double -> Canvas.Dim -> Bool
overlaps (P (V2 x y)) (Canvas.D px py0 pw ph) =
  x >= px && y >= py && x <= px + pw && y <= py + ph
  where
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
      if T.toLower (XML.nameLocalName (XML.elementName element)) == "img"
      then do
        let lookupAttribute attr node =
              M.lookup (XML.Name attr Nothing Nothing) (XML.elementAttributes node)
        src <- lookupAttribute "src" element
        let w = (read . T.unpack) <$> lookupAttribute "width" element
        let h = (read . T.unpack) <$> lookupAttribute "height" element
        let dim = V2 <$> w <*> h
        Just (ImageContent src dim Nothing)
      else if elem
           (T.toLower (XML.nameLocalName (XML.elementName element)))
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
    ignoreElements = ["head", "script", "style", "br", "hr", "input"]

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
                   ImageContent _ (Just (V2 dx dy)) (Just img) -> do
                     let y = lsY ls
                     let x = lsX ls
                     let dim = Canvas.D x y dx dy
                     let ls' = ls { lsX = x + dx, lsLineHeight = max (lsLineHeight ls) dy }
                     pure (ls', [ImageBox dim img])
                   ImageContent _ _ _ ->
                     pure (ls, [])
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
                   ImageContent _ (Just (V2 dx dy)) (Just img) -> do
                     let ls' = ls { lsX = lsX ls + dx, lsLineHeight = dy }
                     let dim = Canvas.D (lsX ls) (lsY ls) dx dy
                     pure (ls', [ImageBox dim img])
                   ImageContent _ _ _ ->
                     pure (ls, [])
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
       scale <- Measuring ask
       wh@(V2 width _) <- measure word
       let ls' =
             if lsX ls + width > maxWidth
               then ls
                    {lsX = 0, lsY = lsY ls + lsLineHeight ls, lsLineHeight = (lineHeight scale)}
               else ls {lsLineHeight = max (lsLineHeight ls) (lineHeight scale)}
       fe <- extents
       pure
         ( ls' {lsX = (lsX ls' + width + space scale), lsY = lsY ls'}
         , TextBox
             events
             (Text
              { textXY = V2 (lsX ls') (lsY ls' + Canvas.fontExtentsHeight fe)
              , textWH = wh
              , textColor = color
              , textWeight = weight
              , textSize = fontSize scale
              , textText = word
              , textStyle = fontStyle
              })))
    ls0
    (T.words t)
  where
    fontStyle = fromMaybe defaultFontStyle (styleFontStyle style)
    space scale = (fontSize scale / 2)
    color = fromMaybe defaultColor (styleColor style)
    fontSize scale = fromMaybe defaultFontSize (styleFontSize style) * scale
    lineHeight scale =
      fontSize scale * (fromMaybe defaultLineHeight (styleLineHeight style))
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
        (do scale <- ask
            lift (Canvas.textFont (Canvas.Font defaultFontFace (fontSize scale) bold italic))
            lift (Canvas.textSize (T.unpack w)))
    extents = Measuring (lift Canvas.fontExtents)

--------------------------------------------------------------------------------
-- SDL rendering to the canvas

-- | Re-render the canvas. The boxes are computed by blockToBoxes,
-- this function simply renders the boxes that it's told to render.
--
-- Later, it might calculate the height of the document, then create a
-- fresh texture which it could scroll simply by passing extra
-- arguments to copy of a region to copy offset by some Y.
rerender :: Double -> EV -> IO [Box]
rerender scale ev = do
  boxes <-
    Canvas.withCanvas (evTexture ev) $ do
      Canvas.background $ Canvas.rgb 255 255 255
      (V2 width height) <- Canvas.getCanvasSize
      (_, boxes) <-
        runReaderT
          (runMeasuring
             (blockToBoxes
                (LS
                 { lsX = 0
                 , lsY = (evScrollY ev)
                 , lsLineHeight = 0
                 , lsMaxHeight = height
                 })
                width
                defaultEvents
                defaultStyle {styleWidth = Just width}
                [(evContent ev)]))
          scale
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
               Canvas.textBaseline (T.unpack (textText text)) (textXY text)
             _ -> return ())
        boxes
      pure boxes
  SDL.copy (evRenderer ev) (evTexture ev) Nothing Nothing
  forM_ boxes $ \case
    ImageBox (Canvas.D x y dx dy) img -> do
      texture <- SDL.createTextureFromSurface (evRenderer ev) img
      let pos = V2 (round x) (round y)
      let size = V2 (round  dx) (round dy)
      let dim = SDL.Rectangle (P pos) size
      SDL.copy (evRenderer ev) texture Nothing (Just dim)
      return ()
    _ -> return ()
  SDL.present (evRenderer ev)
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
