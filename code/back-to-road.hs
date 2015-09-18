import Graphics.Rendering.Cairo
import Data.List (transpose)

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

vectorFromAngle r a =
  Vector (r * cos a) (r * sin a)

darkPoly = [Point 170 50, Point 55 180, Point 75 340, 
            Point 150 180, Point 345 110]

unit r (Vector dx dy) = 
  Vector (r * dx / mag) (r * dy / mag)
  where
    mag = magnitude (Vector dx dy)

rotList n xs = take size (drop (n `mod` size) (cycle xs))
  where size = length xs

pairwise f (x:y:zs) = f x y : pairwise f zs
pairwise f _ = []

plateW = 100
roadMarks = [0.286, 0.307, 0.491]
r1 = plateW * roadMarks !! 0
r2 = plateW - r1
dirPoints = darkPoly
dirTriplets = transpose [rotList r dirPoints | r <- [-1,0,1]]
dirVecs2 = map (\[a,b,c] -> [(b,mkVector b a),(b,mkVector b c)]) dirTriplets
dirVecs = concat dirVecs2
dirUnits50 = map (\(s,v) -> (s, unit 50 v)) dirVecs
dirUnits60 = map (\(s,v) -> (s, unit 65 v)) dirVecs
dirUnits = map (\(s,v) -> unit 1.00 v) dirVecs
dirAngles = map (vectorAngle axisX) dirUnits
dirBetween = pairwise vectorAngle dirUnits
dirHalves = map 
  (\(a,b) -> b + 0.5*a) 
  (zip dirBetween (pairwise (\a b -> a) dirAngles))
dirHVecs = map (vectorFromAngle (0.5*plateW)) dirHalves
points = map (uncurry toPoint) (zip darkPoly dirHVecs)
segments = zip points (tail (cycle points))
startPoints = map fst segments
endPoints = map snd segments
vectors = map (uncurry mkVector) segments
normals = map normal vectors
units50 = map (unit 50) normals
units = map (unit 1.0) normals
arcs1 = zip3 endPoints units (tail (cycle units))
vecAngles = map (vectorAngle axisX) units
arcs2 = zip3 (tail (cycle startPoints)) (tail (cycle vecAngles)) vecAngles

white  = RGBA 1.00 1.00 1.00 1.00
black  = RGB  0.00 0.00 0.00
red    = RGB  0.88 0.29 0.22
orange = RGB  0.98 0.63 0.15
yellow = RGB  0.97 0.85 0.39
green  = RGB  0.38 0.74 0.43
blue   = RGB  0.33 0.67 0.82
darkGreen = RGB 0.00 0.66 0.52
darkBlue  = RGB 0.17 0.51 0.79

fileName = "back-to-road.png"

axisX = Vector 1.0 0.0

vectorAngle (Vector x1 y1) (Vector x2 y2) =
  atan2 (x1*y2 - y1*x2) (x1*x2 + y1*y2)

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

drawPair color (s,v) = do
  drawVector color s v

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
  mapM_ (drawMarkerAt darkGreen) darkPoly
  mapM_ (drawPair blue) dirUnits50
  mapM_ (drawPair darkBlue) (zip darkPoly dirHVecs)
  mapM_ (drawMarkerAt red) points
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

main = do
  createPng fileName

