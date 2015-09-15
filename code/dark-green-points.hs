import Graphics.Rendering.Cairo

data Point  = Point Double Double
  deriving Show
data Vector = Vector Double Double
  deriving Show
data RGBA = RGB  Double Double Double
          | RGBA Double Double Double Double

tau = 6.28318530717958647692

uncurry3 f (a,b,c) = f a b c

mkVector (Point x0 y0) (Point x1 y1) =
  Vector (x1 - x0) (y1 - y0)

toPoint (Point x0 y0) (Vector dx dy) =
  Point (x0 + dx) (y0 + dy)
toPoint1 (Vector dx dy) = 
  Point dx dy

normal (Vector dx dy) = 
  Vector (-dy) dx

dist (Point x0 y0) (Point x1 y1) = 
  sqrt ((sqr dx) + (sqr dy))
  where
    sqr x = x * x
    dx = x1 - x0
    dy = y1 - y0

magnitude (Vector dx dy) =
  dist (Point 0 0) (Point dx dy)

darkPoly = [Point 200 50, Point 55 180, Point 75 340, Point 345 210]

unit r (Vector dx dy) = 
  Vector (r * dx / mag) (r * dy / mag)
  where
    mag = magnitude (Vector dx dy)

white  = RGBA 1.00 1.00 1.00 1.00
red    = RGB  0.88 0.29 0.22
orange = RGB  0.98 0.63 0.15
yellow = RGB  0.97 0.85 0.39
green  = RGB  0.38 0.74 0.43
darkGreen = RGB 0.00 0.66 0.52

fileName = "dark-green-points.png"

vectorAngle (Vector x y) 
  | y >= 0   = acos x
  | otherwise = -(acos x)

setColor (RGBA r g b a) = setSourceRGBA r g b a
setColor (RGB r g b) = setColor (RGBA r g b 0.8)

drawMarkerAt color (Point x y) = do
  let bw = 10.0
  save
  translate x y
  setColor color
  rectangle (-0.5*bw) (-0.5*bw) bw bw
  fill
  restore

drawArc color r (Point x y) angle1 angle2 = do
  setColor color
  arc x y r angle1 angle2
  stroke

drawLine color (Point x0 y0) (Point x1 y1) = do
  setColor color
  moveTo x0 y0
  lineTo x1 y1
  stroke

drawVector color (Point x y) (Vector dx dy) = do
  setColor color
  moveTo x y
  relLineTo dx dy
  stroke

paintCanvas = do
  setSourceRGB 1 1 1
  paint
  mapM_ (drawMarkerAt darkGreen) darkPoly

createPng fileName = do
  let w = 400
      h = 400
  img <- createImageSurface FormatARGB32 w h
  renderWith img paintCanvas
  surfaceWriteToPNG img fileName

main = do
  createPng fileName

