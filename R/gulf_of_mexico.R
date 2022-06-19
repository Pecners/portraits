library(marmap)
library(rayshader)
library(scico)
library(MetBrewer)
library(terra)
library(glue)

data <- rast("gulf.tif")

data <- getNOAA.bathy(lat1 = 33, lat2 = 17, lon1 = -98, lon2 = -75,
                      resolution = 1) %>%
  as.raster()


dd <- raster_to_matrix(data)

small <- resize_matrix(dd, .25)

fix <- function(x) {
  for(row in 1:nrow(x)) {
    for(col in 1:ncol(x)) {
      if(!is.na(x[row, col])) {
        if (x[row, col] > 0) {
          x[row, col] <- NA
        } else {
          x[row, col] <- abs(x[row, col])
        }
      }
      
    }
  }
  return(x)
}

d <- fix(dd)
small <- fix(small)


pal <- "lajolla"
colors <- met.brewer(pal)
colors <- scico(10, palette = pal)


rgl::rgl.close()

small %>%
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  plot_3d(heightmap = small, solid = FALSE, zscale = 10,
          #windowsize = c(800*wr,800*hr), 
          #shadowwidth = 100, 
          shadowcolor = colors[1],
          phi = 90, zoom = 1, theta = 0, background = "white")

render_highquality(
  glue("test_gulf.png"), parallel = TRUE, 
  samples = 300,
  light = FALSE, interactive = FALSE,
  #ambient_light = TRUE, backgroundhigh = colors[1],
  environment_light = "bathybase/env/phalzer_forest_01_4k.hdr",
  intensity_env = 2,
  rotate_env = 140,
  #width = round(6000 * wr), height = round(6000 * hr)
)
