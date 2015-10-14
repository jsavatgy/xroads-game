import Graphics.Rendering.Cairo
import Data.List (transpose)
import Data.Fixed (mod')

type Curv = Char
data Segment = Straight Point Angle Curv Point Angle Curv
             | Curve Point Angle Angle Curv
type Angle = Double
data Vec = Vec Point Angle Curv | None
  deriving Show
data Curving = Convex | Concave
  deriving Show
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

darkPoly = [Point 100 60, Point 60 350, Point 160 200, 
            Point 240 200, Point 340 350, Point 300 60]

unit r (Vector dx dy) = 
  Vector (r * dx / mag) (r * dy / mag)
  where
    mag = magnitude (Vector dx dy)

rotList n xs = take size (drop (n `mod` size) (cycle xs))
  where size = length xs

curved angle 
 | angle >= 0  = Convex
 | otherwise   = Concave

ox Convex  = 'x'
ox Concave = 'o'

cycledTriples xs = 
  zipWith3 (\a b c -> a:b:[c]) (rotList (-1) xs) xs (rotList 1 xs)
cycledPairs xs = zipWith (\a b -> a:[b]) xs (rotList 1 xs)
pairwise f (x:y:zs) = f x y : pairwise f zs
pairwise f _ = []

plateW = 100
--roadMarks = [0.286, 0.307, 0.491]
--r1 = plateW * roadMarks !! 0
--r2 = plateW - r1
dirPoints = darkPoly
dirTriplets = transpose [rotList r dirPoints | r <- [-1,0,1]]
dirVecs2 = map (\[a,b,c] -> [(b,mkVector b a),(b,mkVector b c)]) dirTriplets
dirVecs = concat dirVecs2
dirUnits = map (\(s,v) -> unit 1.00 v) dirVecs
dirAngles = map (vectorAngle axisX) dirUnits
dirBetween = pairwise vectorAngle dirUnits
dirCurved  = map curved dirBetween
dirOx = map ox dirCurved
dirCycledPairs = cycledPairs dirOx
dirCycledTriples = cycledTriples dirOx
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
units = map (unit 1.0) normals
vecAngles = map (vectorAngle axisX) units
arcs2 = zip3 startPoints vecAngles (rotList (-1) vecAngles)
ioVecs = concat (calcVecs dirCurved points dirUnits)
ioSegs = calcSegments ioVecs

opposite angle = (angle + 0.5*tau) `mod'` tau

angleRS p0 p1 op = 
  (beta `op` alpha) `mod'` tau
  where
    r = dist p0 p1
    alpha = acos (plateW / r)
    beta = vectorAngle axisX (mkVector p0 p1)

calcVec crv pts ags =
  [prevNew,nextNew] 
  where
    prevNew = case prevOx of
      "xx" -> Vec thisPt prevAg 'x'
      "oo" -> Vec thisPt (opposite prevAg) 'o'
      "xo" -> Vec thisPt (opposite (angleRS prevPt thisPt (+))) 'o'
      "ox" -> Vec thisPt (opposite (angleRS prevPt thisPt (-))) 'x'
      _    -> None
    nextNew = case nextOx of
      "xx" -> Vec thisPt thisAg 'x'
      "oo" -> Vec thisPt (opposite thisAg) 'o'
      "xo" -> Vec thisPt (angleRS thisPt nextPt (+)) 'x'
      "ox" -> Vec thisPt (angleRS thisPt nextPt (-)) 'o'
      _    -> None
    [prevOx,nextOx] = [init crv, tail crv]
    [prevPt,thisPt,nextPt] = pts
    [prevAg,thisAg,nextAg] = ags
  
calcVecs curves points units =
  map (uncurry3 calcVec) z
  where
  z = zip3 
    (cycledTriples (map ox curves))
    (cycledTriples points)
    (cycledTriples vecAngles)

mkCurve (Vec p0 a0 c0) (Vec p1 a1 c1) =
  Curve p0 a0 a1 c0

mkStraight (Vec p0 a0 c0) (Vec p1 a1 c1) =
  Straight p0 a0 c0 p1 a1 c1
  
calcSegments vecs = 
  pairwise mkCurve vecs ++
  pairwise mkStraight (rotList 1 vecs)

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

fileName = "io-segs.png"

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

drawMarkerCrv Convex  p = drawMarkerAt red p
drawMarkerCrv Concave p = drawMarkerAt violet p
  
drawArcNegative color r (Point x y) angle1 angle2 = do
  setColor color
  arcNegative x y r angle1 angle2
  stroke

drawArc color r (Point x y) angle1 angle2 = do
  setColor color
  arc x y r angle1 angle2
  stroke

drawCircle color r (Point x y) = do
  setColor color
  arc x y r 0 tau
  fill

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

drawPair color (s,v) = drawVector color s v

drawCircles2 color r = do
  mapM_ (drawCircle color r) points

drawCircles3 = do
  mapM_ (\(c,r) -> drawCircles2 c r) (zip grs rss)
  where
    rss = [n*plateW | n <- [0.75,0.50,0.25]] 
    grs = [grey 0.95, grey 0.90, grey 0.85]

alongAngle ratio angle curved =
  case curved of
    'x' -> vectorFromAngle (ratio*plateW) angle
    'o' -> vectorFromAngle ((1.00-ratio)*plateW) angle

drawSeg ratio (Curve p0 a0 a1 curved) =
  case curved of
    'x' -> drawArcNegative green (ratio*plateW) p0 a0 a1
    'o' -> drawArc green ((1.00-ratio)*plateW) p0 a0 a1

drawSeg ratio (Straight p0 a0 c0 p1 a1 c1) = 
  drawVector green point0 (mkVector point0 point1)
  where
    point0 = toPoint p0 (alongAngle ratio a0 c0)
    point1 = toPoint p1 (alongAngle ratio a1 c1)

drawSeg1 x = do
  setDash [] 0
  mapM_ (\r -> drawSeg r x) [0.30,0.70]
  setDash [7,2] 0
  drawSeg 0.50 x
drawSegs xs = mapM_ drawSeg1 xs

drawVec (Vec pt ag cu) =
  drawVector yellow pt (vectorFromAngle plateW ag)
drawVec None = return ()

drawVecs xs = mapM_ drawVec xs

paintCanvas = do
  setSourceRGB 1 1 1
  paint
  drawCircles3
  mapM_ (drawMarkerAt darkGreen) darkPoly
  mapM_ (drawPair darkBlue) (zip darkPoly dirHVecs)
  mapM_ (uncurry drawMarkerCrv) (zip dirCurved points)
  drawVecs ioVecs
  drawSegs ioSegs

createPng fileName = do
  let w = 400
      h = 400
  img <- createImageSurface FormatARGB32 w h
  renderWith img paintCanvas
  surfaceWriteToPNG img fileName
  liftIO ( do 
    putStrLn "ioVecs = "
    print ioVecs
    putStrLn ("Created: " ++ fileName)
    putStrLn ("Size: " ++ show w ++ "x" ++ show h ++ " pixels")
    )

main = do
  createPng fileName

