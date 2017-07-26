import Graphics.Rendering.Cairo

data Point  = Point Double Double
data Vector = Vector Double Double
data RGBA = RGB  Double Double Double
          | RGBA Double Double Double Double

tau = 6.28318530717958647692

uncurry3 f (a,b,c) = f a b c

intToDouble :: Int -> Double
intToDouble i = fromRational (toRational i)

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

unit r (Vector dx dy) = 
  Vector (r * dx / mag) (r * dy / mag)
  where
    mag = magnitude (Vector dx dy)

white  = RGBA 1.00 1.00 1.00 1.00
black  = RGB  0.00 0.00 0.00
red    = RGB  0.88 0.29 0.22
orange = RGB  0.98 0.63 0.15
yellow = RGB  0.97 0.85 0.39
green  = RGB  0.38 0.74 0.43
blue   = RGB  0.33 0.67 0.82
violet = RGB  0.57 0.40 0.72
grey n = RGB n n n
darkGreen  = RGB 0.00 0.66 0.52
darkBlue   = RGB 0.17 0.51 0.79
darkViolet = RGB 0.28 0.34 0.47

fileName = "circle-area.png"

vectorAngle (Vector x y) 
  | y >= 0   = acos x
  | otherwise = -(acos x)

setColor (RGBA r g b a) = setSourceRGBA r g b a
setColor (RGB r g b) = setColor (RGBA r g b 1.0)

drawMarkerAt (Point x y) = do
  let bw = 10.0
  save
  translate x y
  setColor red
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
   
angles1 n = [i * (tau/n) | i <- [0..n-1]]
angles2 n = [i * (tau/n) | i <- [1..n]]
angles n = zip (angles1 n) (angles2 n)

drawSegment color r (Point x y) angle1 angle2 =  do
  drawVector color (Point x y) vec1
  drawVector color (Point x y) vec2
  drawArc color r (Point x y) angle1 angle2 
  where
    vec1 = Vector (r * cos angle1)(r * sin angle1)
    vec2 = Vector (r * cos angle2)(r * sin angle2)

drawSegmentCircle i = do
  mapM_ (\(a1,a2) -> (drawSegment green r pt a1 a2)) alfas
  where
    alfas = angles i
    pt
      | i < 9.5   = Point midmargin (midlineY i)
      | otherwise = Point ((intToDouble pageW/2) + midmargin) (midlineY (i-7))

drawSegmentZigZag n = do
  mapM_ (\(pt,(a1,a2)) -> (drawSegment violet r pt a1 a2)) (zip points angles)
  where
    centreXs = [r * (i + 2) | i <- [1..n]]
    angles = [(
      extra i - 0.5 * (tau/n),
      extra i + 0.5 * (tau/n) 
        ) | i <- [1..n]]
    extra i = if (round i `mod` 2 == 0) then 0.25 * tau else 0.75 * tau
    angle4 =  0.5 * (tau/n)
    angle1 = -tau/4 + angle4
    angle2 = tau/4 - angle4
    vec1 = Vector (r * cos angle1)(r * sin angle1)
    vec2 = Vector (r * cos angle2)(r * sin angle2)
    vec i = if (round i `mod` 2 == 0) then vec1 else vec2
    point 0 
      | n < 9.5   = Point (3*r) (midlineY n)
      | otherwise = Point ((intToDouble pageW/2) + 3*r) (midlineY (n-7))
    point n = toPoint (point (n-1)) (vec n)
    points = tail [point i | i <- [0..n]]

drawCircles n = do
  mapM_ (\i -> drawSegmentCircle i) [i | i <- [3..n]]
  mapM_ (\i -> drawSegmentZigZag i) [i | i <- [3..n]]
    
midlineY i = 2.5 * r * (i-3) + midmargin  -- y coordinate of circle centre
midmargin = 1.5 * r            -- from edge of picture to centre of circle
r = 50                                                    -- circle radius
n = 16                                                        -- divisions

paintCanvas = do
  setSourceRGB 1 1 1
  paint
  drawCircles n

(pageW,pageH) = (700,900)

createPng fileName = do
  img <- createImageSurface FormatARGB32 pageW pageH
  renderWith img paintCanvas
  surfaceWriteToPNG img fileName
  liftIO ( do 
    putStrLn ("Created: " ++ fileName)
    putStrLn ("Size: " ++ show pageW ++ "x" ++ show pageH ++ " pixels")
    )

main = do
  createPng fileName

