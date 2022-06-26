

height_shade <- function (heightmap, texture1, texture2, range = NULL, keep_user_par = TRUE) 
{
  
  t1 <- t1
  t2 <- t2
  range1 <- c(0, max(heightmap, na.rm = TRUE))
  range2 <- c(min(heightmap, na.rm = TRUE), 0)
  
  # Sea level and above
              
  tempfilename1 = tempfile()

  grDevices::png(tempfilename, width = nrow(heightmap), height = ncol(heightmap))
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::image(rayshader:::fliplr(heightmap), axes = FALSE, col = t1, 
                  useRaster = TRUE, zlim = range1)
  graphics::image(rayshader:::fliplr(heightmap), axes = FALSE, col = t2, 
                  useRaster = TRUE, zlim = range2, add = TRUE)
  grDevices::dev.off()
  tempmap = png::readPNG(tempfilename)
  
  # Below sea level
  
  tempfilename2 = tempfile()
  
  grDevices::png(tempfilename, width = nrow(heightmap), height = ncol(heightmap))
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::image(rayshader:::fliplr(heightmap), axes = FALSE, col = t2, 
                  useRaster = TRUE, zlim = range2)
  grDevices::dev.off()
  tempmap2 = png::readPNG(tempfilename)
  
  x <- list(tempmap, tempmap2)
  full <- raster::merge(tempmap, tempmap2)
  return(full)
}
