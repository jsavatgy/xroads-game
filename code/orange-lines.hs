import Graphics.Rendering.Cairo

data Point  = Point Double Double
data Vector = Vector Double Double
data RGBA = RGB  Double Double Double
          | RGBA Double Double Double Double


mkVector (Point x0 y0) (Point x1 y1) =
  Vector (x1 - x0) (y1 - y0)

points = [Point 200 100, Point 100 300, Point 300 200]
segments = zip points (tail (cycle points))
vectors = map (uncurry mkVector) segments

white  = RGBA 1.00 1.00 1.00 1.00
red    = RGB  0.88 0.29 0.22
orange = RGB  0.98 0.63 0.15

fileName = "orange-lines.png"

setColor (RGBA r g b a) = do
  setSourceRGBA r g b a
setColor (RGB r g b) = do
  setColor (RGBA r g b 0.8)

drawOneMarker bw = do
  rectangle (-0.5*bw) (-0.5*bw) bw bw
  fill

drawMarkerAt (Point x y) = do
  save
  translate x y
  setColor red
  drawOneMarker 20.0
  restore

drawVector (Point x y) (Vector dx dy) = do
  setColor orange
  moveTo x y
  relLineTo dx dy
  stroke

paintCanvas = do
  setSourceRGB 1 1 1
  paint
  mapM_ drawMarkerAt points
  mapM_ (uncurry drawVector) (zip points vectors)

createPng fileName = do
  let w = 400
      h = 400
  img <- createImageSurface FormatARGB32 w h
  renderWith img paintCanvas
  surfaceWriteToPNG img fileName
  liftIO ( do 
    putStrLn ("Created: " ++ fileName)
    putStrLn ("Size: " ++ show w ++ "x" ++ show h ++ " pixels")
    )

main = do
  createPng fileName

