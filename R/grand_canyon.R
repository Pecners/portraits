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

map <- "grandcanyon"

data <- st_read("data/grca_tracts/GRCA_boundary.shp")

# Square miles

area <- as.numeric(st_area(data)) / 2.59e6

zelev <- get_elev_raster(data, z = 12, clip = "location")

mat <- raster_to_matrix(zelev)

# elevation range in ft, converted from meters

elev_range <- (max(mat, na.rm = TRUE) - min(mat, na.rm = TRUE)) * 3.281

small <- rayshader::resize_matrix(hm, .25)

strip <- function(m, h) {
  for (nrow in 1:nrow(m)) {
    for (ncol in 1:ncol(m)) {
      if (!is.na(m[nrow,ncol])) {
        m[nrow,ncol] <- m[nrow,ncol] * -1
      } 
    }
  }
  return(m)
}

new <- strip(small, 1750)

hm <- new
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



rgl::rgl.close()


hm %>%
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  #height_shade(texture = (grDevices::colorRampPalette(colors))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow() %>%
  #add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE, zscale = 10,
          windowsize = c(800*wr,800*hr), 
          #shadowdepth = -300,
          #shadowwidth = 100, 
          shadowcolor = colors[1],
          phi = 90, zoom = .7, theta = 0, background = "white") 


{
  glue("plots/{map}_{pal}_deepshad.png")
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
  
}



states <- spData::us_states 

spot <- st_buffer(st_centroid(data), 100000)
text_color <- colors[1]



add_stuff <- function(pal, map, c, c_fun, t) {
  if (c_fun == "scico") {
    colors <- scico(n = 10, palette = pal)
  } else {
    colors <- met.brewer(pal)
  }
  
  text_color <- colors[t]
  
  img <- image_read(glue("plots/{map}_{pal}_deepshad.png"))
  
  # Title
  img_ <- image_annotate(img, "A Portrait of", font = "Cinzel Decorative",
                         color = text_color, size = 125, gravity = "north",
                         location = "+0+200")
  # Subtitle
  img_ <- image_annotate(img_, "Grand Canyon National Park", weight = 700, 
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
  
  # Area
  img_ <- image_annotate(img_, glue("Area: {label_comma()(round(area))} sq mi"),
                         font = "Cinzel Decorative", location = "+1200-1000",
                         color = text_color, size = 110, gravity = "west")
  
  # Elevation range
  img_ <- image_annotate(img_, glue("Elevation Range: {label_comma()(round(elev_range))} ft"),
                         font = "Cinzel Decorative", location = "+1200-1300",
                         color = text_color, size = 110, gravity = "west")
  
  states <- spData::us_states 
  
  spot <- st_buffer(st_centroid(data), 100000)
  text_color <- colors[length(colors)]
  
  
  loc_plot <- ggplot() + 
    geom_sf(data = states, fill = "transparent", color = text_color, size = 0.2) + 
    geom_sf(data = spot, fill = NA, color = colors[2]) +
    theme_void() + 
    coord_sf(crs = 3347)
  
  loc_plot
  ggsave(loc_plot, filename = glue("plots/{map}_inset.png"), w = 4*1.5, h = 3*1.5)
  
  
  # Twitter
  img_ <- image_composite(img_, tw, gravity = "south",
                                offset = "-530+65")
  
  inset <- image_read(glue("plots/{map}_inset.png"))
  new_inset <- image_scale(inset, "x1000")
  
  img_mosaic <- image_composite(img_, new_inset, gravity = "east",
                                offset = "+1200-1000")
  
  magick::image_write(img_mosaic, glue("plots/{map}_titled_{pal}_deep.png"))
}

add_stuff(pal = "Benedictus", map = "grandcanyon", c = 8, c_fun = "met", t = 1)

add_stuff(pal = "Demuth", map = "grandcanyon", c = 8, c_fun = "met", t = 10)

img <- image_read(glue("plots/{map}_titled_{pal}_deep.png"))
smimg <- image_scale(img, "x2500")

image_write(smimg, glue("plots/{map}_titled_{pal}_small.png"))
