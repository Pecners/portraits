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
library(NatParksPalettes)

map <- "eye"

data <- st_sfc(st_point(c(-11.401515762405115, 21.127630356438505)), crs = 4326) %>%
  st_transform(crs = 3081) %>%
  st_buffer(50000)

world %>%
  ggplot() + 
  geom_sf() +
  geom_sf(data = data, color = "red", size = 3)


z <- 12

zelev <- get_elev_raster(data, z = z, clip = "location")

# Square miles from square meters

area <- as.numeric(st_area(st_union(data))) / 2.59e+6
mat <- raster_to_matrix(zelev)

# elevation range in ft, converted from meters

elev_range <- (max(mat, na.rm = TRUE) - min(mat, na.rm = TRUE)) * 3.281

high <- max(mat, na.rm = TRUE) * 3.281

#small <- rayshader::resize_matrix(hm, .25)

for (nrow in 1:round(nrow(mat) * .3)) {
  for (ncol in 1:ncol(mat)) {
    if (!is.na(mat[nrow,ncol]) & mat[nrow,ncol] < 500) {
      mat[nrow,ncol] <- NA
    }
  }
}

for (nrow in 1:round(nrow(mat) * .4)) {
  for (ncol in 1:round(ncol(mat) * .2)) {
    if (!is.na(mat[nrow,ncol]) & mat[nrow,ncol] < 500) {
      mat[nrow,ncol] <- NA
    }
  }
}

for (nrow in 1:round(nrow(mat) * .3)) {
  for (ncol in 1:round(ncol(mat) * .2)) {
      mat[nrow,ncol] <- NA
  }
}


hm <- mat

w <- nrow(hm)
h <- ncol(hm)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

pal <- "Acadia"
#3871ef light blue
#061B4A true blue from flag
colors <- natparks.pals(pal)

rgl::rgl.close()

hm %>%
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  #height_shade(texture = (grDevices::colorRampPalette(colors))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow() %>%
  #add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE, zscale = 1,
          #shadowdepth = 0,
          windowsize = c(800*wr,800*hr), 
          #shadowwidth = 100, 
          #shadowcolor = colors[1],
          phi = 90, zoom = 1, theta = 0, background = "white") 

render_camera(phi = 90, zoom = .85, theta = 0)

outfile <- glue("plots/{map}_{pal}_z{z}.png")
outfile

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
  cat(glue("Time elapsed: {end_time - start_time}"))
}

sf::sf_use_s2(FALSE)

add_stuff <- function(pal, map, c, c_fun, w, t, lim, markups = TRUE) {
  if (length(c_fun) == 1) {
    if (c_fun == "scico") {
      colors <- scico(n = 10, palette = pal)
    } else if (c_fun == "met") {
      colors <- met.brewer(pal)
    } else if (c_fun == "natparks") {
      colors <- natparks.pals(pal)
    }
  } else {
    colors <- c_fun
  }
  
  water_color <- colors[w]
  text_color <- colors[t]
  
  img <- image_read(glue("plots/{map}_{pal}_z{z}.png"))
  
  # Title
  img_ <- image_annotate(img, "Eye of the Sahara", weight = 700, 
                         font = "Cinzel Decorative", location = "-1900+650",
                         color = text_color, size = 150, gravity = "north")
  
  # Subtitle
  img_ <- image_annotate(img_, "A portrait of the", font = "Cinzel Decorative",
                         color = text_color, size = 100, gravity = "north",
                         location = "-1900+500")
  
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
                                    "Data from AWS Terrain Tiles"), 
                         font = "Cinzel Decorative", location = "+0+50",
                         color = alpha(text_color, .5), size = 75, gravity = "south")
  
  # Twitter
  img_ <- image_composite(img_, tw, gravity = "south",
                          offset = "-325+65")
  
  if (markups) {
    # # Area
    # img_ <- image_annotate(img_, glue("Area: {label_comma()(round(area))} sq mi"),
    #                        font = "Cinzel Decorative", location = "-1750+800",
    #                        color = text_color, size = 80, gravity = "north")
    # 
    # Highest Point
    # img_ <- image_annotate(img_, glue("Filled to {label_comma()(lim_lab)} ft"),
    #                        font = "Cinzel Decorative", location = "-1400+1500",
    #                        color = text_color, size = 125, gravity = "north")
    
    world <- ne_countries(scale = "small", returnclass = "sf")
    
    lat <- 10
    long <- -10
    
    prj <- glue("+proj=ortho +lat_0={lat} +lon_0={long} +x_0=0 +y_0=0 +a=6375000 +b=6375000 +units=m +no_defs")
    
    water <- st_sfc(st_point(c(0, 0)), crs = prj) %>%
      st_buffer(., 6371000) %>%
      st_transform(crs = 4326)
    
    circle_coords <- st_coordinates(water)[, c(1,2)]
    circle_coords <- circle_coords[order(circle_coords[, 1]),]
    circle_coords <- circle_coords[!duplicated(circle_coords),]
    
    rectangle <- list(rbind(circle_coords,
                            c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                            c(X = 180, Y = 90),
                            c(X = -180, Y = 90),
                            c(X = -180, circle_coords[1, 'Y']),
                            circle_coords[1, c('X','Y')])) %>% 
      st_polygon() %>% st_sfc(crs = 4326)
    
    
    rectangle %>%
      ggplot()+
      geom_sf(data = world) +
      geom_sf(color = "red", fill = alpha("red", .5)) 
    
    w <- st_intersection(world, rectangle)
    w %>%
      ggplot() +
      geom_sf()
    
    spot <- st_centroid(data)
    
    loc_plot <- ggplot(data = w) +
      geom_sf(data = water, color = NA, fill = alpha(water_color, .75)) +
      geom_sf(fill = text_color, size = .1, color = "white") +
      geom_sf(data = spot, fill = colors[7], stroke = 0,
              size = 6, shape = 21) +
      geom_sf(data = spot, fill = colors[6], stroke = 0,
              size = 5, shape = 21) +
      geom_sf(data = spot, fill = "white", stroke = 0,
              size = 3, shape = 21) + 
      coord_sf(crs = prj) +
      theme_void() 
    
    loc_plot
    ggsave(loc_plot, filename = glue("plots/{map}_inset.png"), w = 4*1.5, h = 3*1.5)
    inset <- image_read(glue("plots/{map}_inset.png"))
    new_inset <- image_scale(inset, "x750")
    img_ <- image_composite(img_, new_inset, gravity = "north",
                            offset = "-1900+1000")
  }
  
  image_write(img_, glue("plots/{map}_titled_{pal}_highres.png"))
  smimg <- image_scale(img_, "x3000")
  image_write(smimg, glue("plots/{map}_titled_{pal}_insta_small.png"))
  
}

add_stuff(pal = pal, map = map, c = 8, c_fun = "natparks", w = 6, t = 1, lim = lim, markups = TRUE)
#add_stuff(pal = "custom", map = "seattle", c = 8, c_fun = colors, t = 1, markups = FALSE)
