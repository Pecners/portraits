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

map <- "death"

data <- st_read("data/deva_tracts/deva_boundary.shp")

zelev <- get_elev_raster(data, z = 10, clip = "location")
formax <- get_elev_raster(data, z = 13, clip = "location")

# Square miles

area <- as.numeric(st_area(data)) / 2.59e6
mat <- raster_to_matrix(zelev)
fmat <- raster_to_matrix(formax)


# elevation range in ft, converted from meters
high <- max(fmat, na.rm = TRUE) * 3.281
low <- min(mat, na.rm = TRUE)
elev_range <- high - low

small <- rayshader::resize_matrix(mat, .25)

hm <- mat
w <- nrow(hm)
h <- ncol(hm)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))


# turku, Benedictus
pal <- "Hiroshige"
#pal <- "Archambault"
colors <- met.brewer(pal)
colors <- scico(10, palette = pal)[4:10]

rgl::rgl.close()

t1 <- grDevices::colorRampPalette(c(colors[1:5], "white"))(256)
t2 <- grDevices::colorRampPalette(colors[10:6], bias = .5)(256)


plot_3d(hillshade = tempmap, heightmap = hm, solid = FALSE, zscale = 10,
        windowsize = c(800*wr,800*hr), 
        shadowdepth = -2000,
        #shadowwidth = 100, 
        shadowcolor = colors[1],
        phi = 90, zoom = .9, theta = 0, background = "white") 


#render_camera(phi = 90, zoom = .9, theta = 0)

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
    #rotate_env = 90,
    width = round(6000 * wr), height = round(6000 * hr)
  )
  end_time <- Sys.time()
}

###################
# Post-processing #
###################

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
                         color = "black", size = 125, gravity = "north",
                         location = "+1000+400")
  # Subtitle
  img_ <- image_annotate(img_, "Death Valley National Park", weight = 700, 
                         font = "Cinzel Decorative", location = "+1000+600",
                         color = "black", size = 200, gravity = "north")
  
  twitter <- fa("twitter", fill = "black", fill_opacity = .5)
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
                         color = alpha("black", .5), size = 75, gravity = "south")
  
  # Area
  img_ <- image_annotate(img_, glue("Area: {label_comma()(round(area))} sq mi"),
                         font = "Cinzel Decorative", location = "+1000+1400", weight = 700,
                         color = "black", size = 100, gravity = "north")
  
  # Highest point
  img_ <- image_annotate(img_, glue("Highest Point: {label_comma()(abs(round(high)))} ft"),
                         font = "Cinzel Decorative", location = "+500+1000", weight = 700,
                         color = "black", size = 100, gravity = "north")
  
  img_ <- image_annotate(img_, "above sea level",
                         font = "Cinzel Decorative", location = "+1680+1000", weight = 700,
                         color = colors[1], size = 100, gravity = "north")
  
  # Lowest
  
  img_ <- image_annotate(img_, glue("Lowest Point: 281 ft"),
                         font = "Cinzel Decorative", location = "+500+1200", weight = 700,
                         color = "black", size = 100, gravity = "north", )
  
  img_ <- image_annotate(img_, "below sea level",
                         font = "Cinzel Decorative", location = "+1600+1200", weight = 700, 
                         color = text_color, size = 100, gravity = "north")
  
  
  states <- spData::us_states 
  
  spot <- st_buffer(st_centroid(data), 100000)
  
  loc_plot <- ggplot() + 
    geom_sf(data = states, fill = "transparent", color = text_color, size = 0.2) + 
    geom_sf(data = spot, fill = NA, color = colors[c]) +
    theme_void() + 
    coord_sf(crs = 3347)
  
  loc_plot
  ggsave(loc_plot, filename = glue("plots/{map}_inset.png"), w = 4*1.5, h = 3*1.5)
  
  
  # Twitter
  img_ <- image_composite(img_, tw, gravity = "south",
                          offset = "-530+65")
  
  inset <- image_read(glue("plots/{map}_inset.png"))
  new_inset <- image_scale(inset, "x1500")
  
  img_mosaic <- image_composite(img_, new_inset, gravity = "west",
                                offset = "+400+1500")
  
  image_write(img_mosaic, glue("plots/{map}_titled_{pal}_highres.png"))
  smimg <- image_scale(img_mosaic, "x3000")
  image_write(smimg, glue("plots/{map}_titled_{pal}_insta_small.png"))
  
}

add_stuff(pal = "Hiroshige", map = "death", c = 2, c_fun = "met", t = 9)
#add_stuff(pal = "Demuth", map = "milwaukee", c = 2, c_fun = "met", t = 10)
