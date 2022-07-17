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
library(rnaturalearth)
library(rnaturalearthdata)
library(ggstar)

map <- "katmai"

data <- st_read("data/katm_tracts/KATM_boundary.shp") %>%
  st_transform(crs = 3338)

data[1,] %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = data[2,], fill = "red") +
  coord_sf(crs = 4326)

# Plotted with z = 11, got max elev with z = 12

zelev <- get_elev_raster(data, z = 12, clip = "location")

# Square miles from square meters

area <- as.numeric(st_area(st_union(data))) / 2.59e+6
mat <- raster_to_matrix(zelev)
sum(mat < 0, na.rm = TRUE)
sum(mat == 0, na.rm = TRUE)
mat[which(mat < 0)] <- 0
sum(mat < 0, na.rm = TRUE)
sum(mat == 0, na.rm = TRUE)

# elevation range in ft, converted from meters

elev_range <- (max(mat, na.rm = TRUE) - min(mat, na.rm = TRUE)) * 3.281

high <- max(mat, na.rm = TRUE) * 3.281

#small <- rayshader::resize_matrix(hm, .25)


hm <- mat
rm(mat)

w <- nrow(hm)
h <- ncol(hm)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

pal <- "flag"
#3871ef light blue
#061B4A true blue from flag
colors <- c("#061B4A", "#3871ef", "#FFB70B", "white")

rgl::rgl.close()

hm %>%
  height_shade(texture = grDevices::colorRampPalette(colors, bias = 2.5)(256)) %>%
  #height_shade(texture = (grDevices::colorRampPalette(colors))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow() %>%
  #add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE, zscale = 5,
          shadowdepth = -1000,
          windowsize = c(800*wr,800*hr), 
          #shadowwidth = 100, 
          #shadowcolor = colors[1],
          phi = 90, zoom = .8, theta = 0, background = "white") 

render_camera(phi = 90, zoom = .9, theta = 0)

outfile <- glue("plots/{map}_{pal}_z10.png")

{
  outfile
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"))
  render_highquality(
    outfile, parallel = TRUE, 
    samples = 300, 
    light = FALSE, interactive = FALSE,
    #ambient_light = TRUE, backgroundhigh = colors[1],
    environment_light = "../bathybase/env/phalzer_forest_01_4k.hdr",
    intensity_env = 2,
    rotate_env = 80,
    width = round(6000 * wr), height = round(6000 * hr)
  )
  end_time <- Sys.time()
}



add_stuff <- function(pal, map, c, c_fun, t, markups = TRUE) {
  if (length(c_fun) == 1) {
    if (c_fun == "scico") {
      colors <- scico(n = 10, palette = pal)
    } else if (c_fun == "met") {
      colors <- met.brewer(pal)
    } 
  } else {
    colors <- c_fun
  }
  
  text_color <- colors[t]
  
  img <- image_read(glue("plots/{map}_{pal}_z10.png"))
  
  # Title
  img_ <- image_annotate(img, "Katmai", weight = 700, 
                         font = "Cinzel Decorative", location = "-1950+300",
                         color = text_color, size = 400, gravity = "north")
  
  # Subtitle
  img_ <- image_annotate(img_, "National Park\nand Preserve", font = "Cinzel Decorative",
                         color = text_color, size = 125, gravity = "north",
                         location = "-1950+900")
  
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
                                    "Data from USGS and AWS Terrain Tiles"), 
                         font = "Cinzel Decorative", location = "+0+50",
                         color = alpha(text_color, .5), size = 75, gravity = "south")
  
  # Twitter
  img_ <- image_composite(img_, tw, gravity = "south",
                          offset = "-530+65")
  
  if (markups) {
    # # Area
    # img_ <- image_annotate(img_, glue("Area: {label_comma()(round(area))} sq mi"),
    #                        font = "Cinzel Decorative", location = "-1750+800",
    #                        color = text_color, size = 80, gravity = "north")
    # 
    # # Highest Point
    # img_ <- image_annotate(img_, glue("Highest Point: {label_comma()(round(elev_range))} ft"),
    #                        font = "Cinzel Decorative", location = "-1750+1000",
    #                        color = text_color, size = 80, gravity = "north")
    # 
    na <- world %>%
      filter(continent == "North America")
    
    water <- st_sfc(st_point(c(50, -150)), crs = "+proj=ortho +lat_0=50 +lon_0=-150") %>%
      st_buffer(., 6400000)
    
    
    loc_plot <- ggplot(data = world) +
      geom_sf(data = water, color = NA, fill = alpha(colors[2], .75)) +
      geom_sf(fill = "#ffd46d", size = .1, color = "grey40") +
      geom_sf(data = spot, fill = colors[1], stroke = 0,
              size = 5, shape = 21) +
      geom_sf(data = spot, fill = colors[2], stroke = 0,
              size = 4, shape = 21) +
      geom_sf(data = spot, fill = colors[3], stroke = 0,
              size = 3, shape = 21) +
      geom_sf(data = spot, fill = colors[4], stroke = 0,
              size = 2, shape = 21) +
      coord_sf(crs = "+proj=ortho +lat_0=50 +lon_0=-150") +
      theme_void() 
    
    loc_plot
    ggsave(loc_plot, filename = glue("plots/{map}_inset.png"), w = 4*1.5, h = 3*1.5)
    inset <- image_read(glue("plots/{map}_inset.png"))
    new_inset <- image_scale(inset, "x1000")
    img_ <- image_composite(img_, new_inset, gravity = "south",
                            offset = "+1850+600")
  }
  
  image_write(img_, glue("plots/{map}_titled_{pal}_highres.png"))
  smimg <- image_scale(img_, "x3500")
  image_write(smimg, glue("plots/{map}_titled_{pal}_insta_small.png"))
  
}

add_stuff(pal = "flag", map = "katmai", c = 8, c_fun = colors, t = 1, markups = TRUE)
#add_stuff(pal = "custom", map = "seattle", c = 8, c_fun = colors, t = 1, markups = FALSE)
