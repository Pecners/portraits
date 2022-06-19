library(elevatr)
library(scico)
library(sf)
library(rayshader)
library(MetBrewer)
library(magick)
library(glue)
library(tidyverse)
library(scales)
library(fontawesome)
library(grid)
library(svgparser)

map <- "yellowstone"

data <- st_read("data/YELL_tracts/YELL_boundary.shp")

area <- as.numeric(st_area(data)) / 2.59e6

zelev <- get_elev_raster(data, z = 12, clip = "location")

hm <- raster_to_matrix(zelev)

# elevation range in ft, converted from meters

elev_range <- (max(hm, na.rm = TRUE) - min(hm, na.rm = TRUE)) * 3.281

small <- rayshader::resize_matrix(hm, .25)
hm <- hm
w <- nrow(hm)
h <- ncol(hm)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

if (min(c(wr, hr)) < .75) {
  if (wr < .75) {
    wr <- .75
  } else {
    hr <- .75
  }
}

# turku, Benedictus
pal <- "Demuth"
pal <- "Archambault"
colors <- met.brewer(pal)
colors <- scico(10, palette = pal)[4:10]

rgl::rgl.close()


hm %>%
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  #height_shade(texture = (grDevices::colorRampPalette(colors))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow() %>%
  #add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE, zscale = 10,
          windowsize = c(800*wr,800*hr), shadowdepth = -300,
          #shadowwidth = 100, 
          shadowcolor = colors[1],
          phi = 90, zoom = 1, theta = 0, background = "white")

render_camera(theta = 0, phi = 90, zoom = 1.1)

render_highquality(
  glue("plots/{map}_{pal}_deepshad.png"), parallel = TRUE, 
  samples = 300,
  light = FALSE, interactive = FALSE,
  #ambient_light = TRUE, backgroundhigh = colors[1],
  environment_light = "../bathybase/env/phalzer_forest_01_4k.hdr",
  intensity_env = 1.5,
  rotate_env = 180,
  width = round(6000 * wr), height = round(6000 * hr)
)

states <- spData::us_states 

spot <- st_buffer(st_centroid(data), 100000)
text_color <- colors[1]



add_stuff <- function(pal, map, c, c_fun, t) {
  if (c_fun == "scico") {
    colors <- scico(n = 10, palette = pal)
  } else {
    colors <- rev(met.brewer(pal))
  }
  
  text_color <- colors[t]
  
  img <- image_read(glue("plots/{map}_{pal}_deepshad.png"))
  
  # Title
  img_ <- image_annotate(img, "A Portrait of", font = "Cinzel Decorative",
                         color = text_color, size = 125, gravity = "north",
                         location = "+0+200")
  # Subtitle
  img_ <- image_annotate(img_, "Yellowstone National Park", weight = 700, 
                         font = "Cinzel Decorative", location = "+0+400",
                         color = text_color, size = 200, gravity = "north")
  
  twitter <- fa("twitter", fill = text_color, fill_opacity = .5)
  grid.newpage()
  
  tmp <- tempfile()
  png(tmp, bg = "transparent")
  grid.draw(read_svg(twitter))
  dev.off()
  
  tw <- image_read(tmp)
  tw <- image_scale(tw, "x75")
  
  # Caption
  img_ <- image_annotate(img_, glue("Graphic by Spencer Schien (     @MrPecners) | ", 
                         "Data from AWS Terrain Tiles and USGS"), 
                         font = "Cinzel Decorative", location = "+0+50",
                         color = alpha(text_color, .5), size = 75, gravity = "south")
  # Twitter
  img_mosaic <- image_composite(img_, tw, gravity = "south",
                                offset = "-530+65")
  
  magick::image_write(img_mosaic, glue("plots/{map}_titled_{pal}_deep.png"))
}

add_stuff(pal = "Demuth", map = "yellowstone", c = 8, c_fun = "met", t = 1)

add_stuff(pal = "Archambault", map = "yellowstone", c = 8, c_fun = "met", t = 6)
