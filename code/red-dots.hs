import Graphics.Rendering.Cairo

data Point = Point Double Double

points = [Point 200 100, Point 100 300, Point 300 200]

red = (0.88, 0.29, 0.22)

fileName = "red-dots.png"

drawOneMarker bw (r,g,b) = do
  rectangle (-0.5*bw) (-0.5*bw) bw bw
  setSourceRGBA r g b 0.8
  fill

drawMarkerAt (Point x y) = do
  save
  translate x y
  drawOneMarker 20.0 red
  restore

paintCanvas = do
  setSourceRGB 1 1 1
  paint
  mapM_ drawMarkerAt points

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

