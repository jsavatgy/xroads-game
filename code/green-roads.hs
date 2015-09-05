import Graphics.Rendering.Cairo

data Point  = Point Double Double
  deriving Show
data Vector = Vector Double Double
  deriving Show
data RGBA = RGB  Double Double Double
          | RGBA Double Double Double Double
  deriving Show

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

testPolygon = [Point 200 100, Point 100 200, Point 100 300, Point 300 200]
testDodecagon = 
  [Point (200 + radius * cos a)(200 - radius * sin a)| a <- angles]
  where
   radius = 100
   corners = 12
   angles = [i * (tau/corners) | i <- [0..corners-1]]

unit r (Vector dx dy) = 
  Vector (r * dx / mag) (r * dy / mag)
  where
    mag = magnitude (Vector dx dy)

points = testPolygon
segments = zip points (tail (cycle points))
startPoints = map fst segments
endPoints = map snd segments
vectors = map (uncurry mkVector) segments
normals = map normal vectors
units50 = map (unit 50) normals
units = map (unit 1.0) normals

white  = RGBA 1.00 1.00 1.00 1.00
red    = RGB  0.88 0.29 0.22
orange = RGB  0.98 0.63 0.15
yellow = RGB  0.97 0.85 0.39
green  = RGB  0.38 0.74 0.43
darkGreen = RGB 0.00 0.66 0.52

fileName = "green-roads.png"

plateW = 100
roadMarks = [0.286, 0.307, 0.491]
r1 = plateW * roadMarks !! 0
r2 = plateW - r1

vectorAngle (Vector x y) 
  | y >= 0   = acos x
  | otherwise = -(acos x)

arcs1 = zip3 endPoints units (tail (cycle units))
vecAngles = map vectorAngle units
arcs2 = zip3 (tail (cycle startPoints)) (tail (cycle vecAngles)) vecAngles

setColor (RGBA r g b a) = setSourceRGBA r g b a
setColor (RGB r g b) = setColor (RGBA r g b 0.8)

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

paintRoadLine r = do
  mapM_ (uncurry (drawLine green)) (zip rStart rEnd)
  mapM_ (uncurry3 (drawArc green r)) arcs2
  where
    rVec = map (unit r) normals
    rStart = map (uncurry toPoint) (zip startPoints rVec)
    rEnd = map (uncurry toPoint) (zip endPoints rVec)

paintRoad rs = do
  mapM_ paintRoadLine rs

paintCanvas = do
  setSourceRGB 1 1 1
  paint
  mapM_ drawMarkerAt points
  mapM_ (uncurry (drawVector orange)) (zip points vectors)
  mapM_ (uncurry (drawVector yellow)) (zip startPoints units50)
  mapM_ (uncurry (drawVector yellow)) (zip endPoints units50)
  paintRoad [r1,r2]

createPng fileName = do
  let w = 400
      h = 400
  img <- createImageSurface FormatARGB32 w h
  renderWith img paintCanvas
  surfaceWriteToPNG img fileName
  liftIO ( do 
    putStrLn "points = "
    print points
    putStrLn "segments = "
    print segments
    putStrLn "vectors = "
    print vectors
    putStrLn "normals = "
    print normals
    putStrLn "50xunits = "
    print units
    putStrLn "arcs2 = "
    print arcs2
    putStrLn ("Created: " ++ fileName)
    putStrLn ("Size: " ++ show w ++ "x" ++ show h ++ " pixels")
    )

main = do
  createPng fileName

