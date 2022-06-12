library(elevatr)
library(scico)
library(sf)
library(rayshader)
library(MetBrewer)
library(magick)
library(glue)
library(tidyverse)
library(scales)

map <- "yosemite"

yose <- st_read("data/yose_tracts/yose_boundary.shp")

zelev <- get_elev_raster(yose, z = 12, clip = "location")

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

pal <- "oleron"
colors <- met.brewer(pal)
colors <- scico(10, palette = pal)[4:10]

rgl::rgl.close()


hm %>%
  height_shade(texture = grDevices::colorRampPalette(colors, bias = 1.5)(256)) %>%
  #height_shade(texture = (grDevices::colorRampPalette(colors))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow() %>%
  #add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE, zscale = 20,
          windowsize = c(800*wr,800*hr), 
          #shadowwidth = 100, 
          shadowcolor = colors[1],
          phi = 90, zoom = 1, theta = 0, background = "white")

render_camera(theta = 0, phi = 90, zoom = 1)

render_highquality(
  glue("plots/{map}_{pal}.png"), parallel = TRUE, 
  samples = 300,
  light = FALSE, interactive = FALSE,
  #ambient_light = TRUE, backgroundhigh = colors[1],
  environment_light = "../bathybase/env/phalzer_forest_01_4k.hdr",
  intensity_env = 2,
  rotate_env = 140,
  width = round(6000 * wr), height = round(6000 * hr)
)

states <- spData::us_states 

spot <- st_buffer(st_centroid(yose), 100000)
text_color <- colors[7]


loc_plot <- ggplot() + 
  geom_sf(data = states, fill = "transparent", color = text_color, size = 0.2) + 
  geom_sf(data = spot, fill = NA, color = colors[1]) +
  theme_void() + 
  coord_sf(crs = 3347)

loc_plot
ggsave(loc_plot, filename = glue("plots/{map}_inset.png"), w = 4*1.5, h = 3*1.5)



img <- image_read(glue("plots/{map}_{pal}.png"))


# Title
img_ <- image_annotate(img, "A Portrait of", font = "Cinzel Decorative",
                       color = text_color, size = 125, gravity = "north",
                       location = "-1500+250")
# Subtitle
img_ <- image_annotate(img_, "Yosemite National Park", weight = 700, 
                       font = "Cinzel Decorative", location = "-1500+450",
                       color = text_color, size = 175, gravity = "north")

# Caption
img_ <- image_annotate(img_, "Graphic by Spencer Schien (@MrPecners) | Data from AWS Terrain Tiles via {elevatr}", 
                       font = "Cinzel Decorative", location = "+0+50",
                       color = alpha(text_color, .5), size = 75, gravity = "south")

# Elevation range
img_ <- image_annotate(img_, glue("Elevation Range: {label_comma()(round(elev_range))} ft"),
                       font = "Cinzel Decorative", location = "-1500+800",
                       color = text_color, size = 90, gravity = "north")


inset <- image_read(glue("plots/{map}_inset.png"))
new_inset <- image_scale(inset, "x100")

img_mosaic <- image_composite(img_, inset, gravity = "north",
                              offset = "+1700+150")

magick::image_write(img_mosaic, glue("plots/{map}1_titled_{pal}.png"))

