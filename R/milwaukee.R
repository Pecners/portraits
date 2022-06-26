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

map <- "milwaukee"

data <- st_read("../Shapefiles/Milwaukee/City Limits/citylimit.shp")

zelev <- get_elev_raster(data, z = 14, clip = "location")

# Square miles

area <- as.numeric(st_area(data)) / (5280^2)
mat <- raster_to_matrix(zelev)

# elevation range in ft, converted from meters

elev_range <- (max(mat, na.rm = TRUE) - min(mat, na.rm = TRUE)) * 3.281

small <- rayshader::resize_matrix(hm, .25)

hm <- mat
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
#pal <- "Archambault"
colors <- met.brewer(pal)
colors <- scico(10, palette = pal)[4:10]

# People's flag colors

pal <- "peoples_flag"
gold <- "#F6A700"
light_blue <- "#00A6E1"
dark_blue <- "#0D213F"
colors <- c(dark_blue, light_blue, gold, "white")


rgl::rgl.close()


hm %>%
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  #height_shade(texture = (grDevices::colorRampPalette(colors))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow() %>%
  #add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE, zscale = .3,
          windowsize = c(800*wr,800*hr), shadowdepth = -300,
          #shadowwidth = 100, 
          shadowcolor = colors[1],
          phi = 90, zoom = .7, theta = 0, background = "white") 

outfile <- glue("plots/{map}_{pal}_highres.png")

{
  outfile
  start_time <- Sys.time()
  render_highquality(
    outfile, parallel = TRUE, 
    samples = 300,
    light = FALSE, interactive = FALSE,
    #ambient_light = TRUE, backgroundhigh = colors[1],
    environment_light = "../bathybase/env/phalzer_forest_01_4k.hdr",
    intensity_env = 1.5,
    rotate_env = -90,
    width = round(6000 * wr), height = round(6000 * hr)
  )
  end_time <- Sys.time()
}



add_stuff <- function(pal, map, c, c_fun, t) {
  if (c_fun == "scico") {
    colors <- scico(n = 10, palette = pal)
  } else if (c_fun == "met") {
    colors <- met.brewer(pal)
  } else {
    colors <- c_fun
  }
  
  text_color <- colors[t]
  
  img <- image_read(glue("plots/{map}_{pal}_highres.png"))
  
  # Title
  img_ <- image_annotate(img, "A Portrait of", font = "Cinzel Decorative",
                         color = text_color, size = 100, gravity = "north",
                         location = "+1000+200")
  # Subtitle
  img_ <- image_annotate(img_, "Milwaukee, Wisconsin", weight = 700, 
                         font = "Cinzel Decorative", location = "+1000+350",
                         color = text_color, size = 150, gravity = "north")
  
  twitter <- fa("twitter", fill = text_color, fill_opacity = .5)
  grid.newpage()
  
  tmp <- tempfile()
  png(tmp, bg = "transparent")
  grid.draw(read_svg(twitter))
  dev.off()
  
  tw <- image_read(tmp)
  tw <- image_scale(tw, "x50")
  
  # Caption
  img_ <- image_annotate(img_, glue("Graphic by Spencer Schien (     @MrPecners) | ", 
                                    "Data from AWS Terrain Tiles"), 
                         font = "Cinzel Decorative", location = "+0+50",
                         color = alpha(text_color, .5), size = 50, gravity = "south")
  
  # Area
  img_ <- image_annotate(img_, glue("Area: {label_comma()(round(area))} sq mi"),
                         font = "Cinzel Decorative", location = "+1000+600",
                         color = text_color, size = 90, gravity = "north")
  
  # Elevation range
  img_ <- image_annotate(img_, glue("Elevation Range: {label_comma()(round(elev_range))} ft"),
                         font = "Cinzel Decorative", location = "+1000+750",
                         color = text_color, size = 90, gravity = "north")
  
  states <- spData::us_states 
  
  spot <- st_buffer(st_centroid(data), 200000)
  
  loc_plot <- ggplot() + 
    geom_sf(data = states, fill = "transparent", color = text_color, size = 0.2) + 
    geom_sf(data = spot, fill = colors[3], color = colors[3]) +
    theme_void() + 
    coord_sf(crs = 3347)
  
  loc_plot
  ggsave(loc_plot, filename = glue("plots/{map}_inset.png"), w = 4*1.5, h = 3*1.5)
  
  
  # Twitter
  img_ <- image_composite(img_, tw, gravity = "south",
                          offset = "-215+65")
  
  inset <- image_read(glue("plots/{map}_inset.png"))
  new_inset <- image_scale(inset, "x750")
  
  img_mosaic <- image_composite(img_, new_inset, gravity = "north",
                                offset = "+1000-900")
  
  image_write(img_mosaic, glue("plots/{map}_titled_{pal}_highres.png"))
  smimg <- image_scale(img_mosaic, "x3500")
  image_write(smimg, glue("plots/{map}_titled_{pal}_insta_small.png"))
  
}

add_stuff(pal = "peoples_flag", map = "milwaukee", c = 8, c_fun = colors, t = 1)
add_stuff(pal = "Demuth", map = "milwaukee", c = 8, c_fun = "met", t = 10)
