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

map <- "big_bend"


data <- st_read("data/nps_boundary/nps_boundary.shp") %>%
  filter(str_detect(PARKNAME, "Big Bend"))

data %>%
  ggplot() +
  geom_sf() +
  coord_sf(crs = 4326)

z <- 12

zelev <- get_elev_raster(data, z = z, clip = "location")

# Square miles from square meters

area <- as.numeric(st_area(st_union(data))) / 2.59e+6
mat <- raster_to_matrix(zelev)

# elevation range in ft, converted from meters

elev_range <- (max(mat, na.rm = TRUE) - min(mat, na.rm = TRUE)) * 3.281

high <- max(mat, na.rm = TRUE) * 3.281

#small <- rayshader::resize_matrix(hm, .25)


hm <- mat

w <- nrow(hm)
h <- ncol(hm)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

pal <- "tam"
#3871ef light blue
#061B4A true blue from flag
colors <- met.brewer("Tam")

rgl::rgl.close()

hm %>%
  height_shade(texture = grDevices::colorRampPalette(rev(c("white", colors[1:6], "white")), bias = .5)(256)) %>%
  #height_shade(texture = (grDevices::colorRampPalette(colors))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow() %>%
  #add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE, zscale = 3,
          shadowdepth = -1000,
          windowsize = c(800*wr,800*hr), 
          #shadowwidth = 100, 
          #shadowcolor = colors[1],
          phi = 90, zoom = 1, theta = 0, background = "white") 

render_camera(phi = 90, zoom = .7, theta = 0)

outfile <- glue("plots/{map}_{pal}_z{z}.png")

{
  outfile
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"))
  render_highquality(
    outfile, parallel = TRUE, 
    samples = 300, 
    light = FALSE, interactive = FALSE,
    # backgroundhigh = colors[1],
    #ambient_light = TRUE, backgroundhigh = colors[1],
    environment_light = "../bathybase/env/phalzer_forest_01_4k.hdr",
    intensity_env = 1.75,
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
  
  img <- image_read(glue("plots/{map}_{pal}_z{z}.png"))
  
  # Title
  img_ <- image_annotate(img, "Big Bend", weight = 700, 
                         font = "Cinzel Decorative", location = "-1700+300",
                         color = text_color, size = 300, gravity = "north")
  
  # Subtitle
  img_ <- image_annotate(img_, "National Park", font = "Cinzel Decorative",
                         color = text_color, size = 150, gravity = "north",
                         location = "-1700+800")
  
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
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    prj <- "+proj=ortho +lat_0=20 +lon_0=-100"
    
    water <- st_sfc(st_point(c(50, -150)), crs = prj) %>%
      st_buffer(., 6400000)
    
    spot <- st_centroid(data[1,])
    
    loc_plot <- ggplot(data = world) +
      geom_sf(data = water, color = NA, fill = alpha(text_color, .75)) +
      geom_sf(fill = "white", size = .1, color = "grey40") +
      geom_sf(fill = alpha(colors[2], .75), size = .1, color = "grey40") +
      geom_sf(data = spot, fill = colors[4], stroke = 0,
              size = 6, shape = 21) +
      geom_sf(data = spot, fill = colors[3], stroke = 0,
              size = 5, shape = 21) +
      geom_sf(data = spot, fill = colors[2], stroke = 0,
              size = 4, shape = 21) +
      geom_sf(data = spot, fill = colors[1], stroke = 0,
              size = 3, shape = 21) +
      coord_sf(crs = prj) +
      theme_void() 
    
    loc_plot
    ggsave(loc_plot, filename = glue("plots/{map}_inset.png"), w = 4*1.5, h = 3*1.5)
    inset <- image_read(glue("plots/{map}_inset.png"))
    new_inset <- image_scale(inset, "x750")
    img_ <- image_composite(img_, new_inset, gravity = "north",
                            offset = "-1700+1300")
  }
  
  image_write(img_, glue("plots/{map}_titled_{pal}_highres.png"))
  smimg <- image_scale(img_, "x3500")
  image_write(smimg, glue("plots/{map}_titled_{pal}_insta_small.png"))
  
}

add_stuff(pal = "Tam", map = "big_bend", c = 8, c_fun = "met", t = 6, markups = TRUE)
#add_stuff(pal = "custom", map = "seattle", c = 8, c_fun = colors, t = 1, markups = FALSE)
