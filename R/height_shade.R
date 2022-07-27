

height_shade <- function (heightmap, texture1, texture2, range = NULL, keep_user_par = TRUE) 
{
  
  t1 <- t1
  t2 <- t2
  # these were death valley
  # range1 <- c(0, max(heightmap, na.rm = TRUE))
  # range2 <- c(min(heightmap, na.rm = TRUE), 0)
  
  # these were uluru
  range1 <- c(550, max(heightmap, na.rm = TRUE))
  range2 <- c(min(heightmap, na.rm = TRUE), 550)
  
  # Sea level and above
              
  tempfilename = tempfile()

  grDevices::png(tempfilename, width = nrow(heightmap), height = ncol(heightmap))
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::image(rayshader:::fliplr(heightmap), axes = FALSE, col = t1, 
                  useRaster = TRUE, zlim = range1)
  graphics::image(rayshader:::fliplr(heightmap), axes = FALSE, col = t2, 
                  useRaster = TRUE, zlim = range2, add = TRUE)
  grDevices::dev.off()
  tempmap = png::readPNG(tempfilename)

  return(tempmap)
}
