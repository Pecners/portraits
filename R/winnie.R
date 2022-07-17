library(glue)
library(terra)
library(jsonlite)
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

dir <- "../bathybase/data/BathybaseDb"

f <- list.files(dir)
ff <- list.files(glue("{dir}/{f[1]}"))

j <- jsonlite::fromJSON(glue("{dir}/{f[1]}/{ff[1]}/info.json"))

all_lakes <- map_df(1:length(f), function(i) {
  ff <- list.files(glue("{dir}/{f[i]}"))
  
  map_df(1:length(ff), function(k) {
    j <- jsonlite::fromJSON(glue("{dir}/{f[i]}/{ff[k]}/info.json"))
    
    tibble(
      name = j$Name,
      lat = j$Lat,
      long = j$Lon,
      mean_depth = j$`Mean Depth`,
      max_depth = j$`Max Depth`,
      volume = j$Volume,
      main_dir = f[i],
      sub_dir = ff[k],
      source = j$Source
    )
  })
})

ind <- "300-399/370"

d <- rast(glue("../bathybase/data/BathybaseDb/{ind}/bathy.tiff"))
lake_info <- fromJSON(glue("../bathybase/data/BathybaseDb/{ind}/info.json"))
max_depth <- lake_info$`Max Depth`
# resolution is in meters, convert to feet
volume <- abs(sum(dd * ((5*3.281) ^ 2), na.rm = TRUE) / 43560)
data <- st_point(x = c(lake_info$Lon, lake_info$Lat)) %>%
  st_sfc(crs = 4326)

dd <- raster_to_matrix(d)
small <- rayshader::resize_matrix(dd, .25)

hm <- abs(dd)

w <- nrow(hm)
h <- ncol(hm)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

# Colors based on MN Wild colors
colors <- c(
  "#154734",
  "#EAAA00",
  "#DDCBA4",
  "#FFFFFF"
)


rgl::rgl.close()

hm %>%
  height_shade(texture = grDevices::colorRampPalette(rev(colors), bias = 2)(256)) %>%
  #height_shade(texture = (grDevices::colorRampPalette(colors))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow() %>%
  #add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE, zscale = .5,
          shadowdepth = -200,
          windowsize = c(800*wr,800*hr), 
          #shadowwidth = 100, 
          #shadowcolor = colors[1],
          phi = 90, zoom = 1, theta = 0, background = "white") 

render_camera(phi = 90, zoom = .8, theta = 0)

outfile <- glue("plots/{lake_info$Name}_wild_rev.png")

{
  outfile
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"))
  render_highquality(
    outfile, parallel = TRUE, 
    samples = 300, 
    light = FALSE, interactive = FALSE,
    backgroundhigh = "#FFD700",
    #ambient_light = TRUE, backgroundhigh = colors[1],
    environment_light = "../bathybase/env/phalzer_forest_01_4k.hdr",
    intensity_env = 1.75,
    rotate_env = -80,
    width = round(6000 * wr), height = round(6000 * hr)
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"))
}

add_stuff <- function(name, c, c_fun, t, markups = TRUE) {
  if (length(c_fun) == 1) {
    if (c_fun == "scico") {
      colors <- scico(n = 10, palette = pal)
    } else if (c_fun == "met") {
      colors <- met.brewer(pal)
    } 
  } else {
    colors <- c_fun
  }
  
  text_color <- colors[1]
  
  img <- image_read(glue("plots/{name}.png"))
  
  # Title
  img_ <- image_annotate(img, "Lake Winnibigoshish", weight = 700, 
                         font = "Cinzel Decorative", location = "+800+500",
                         color = text_color, size = 200, gravity = "north")
  
  # Subtitle
  img_ <- image_annotate(img_, "A Portrait of", font = "Cinzel Decorative",
                         color = text_color, size = 100, gravity = "north",
                         location = "+800+300")
  
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
                                    "Data from Minnesota DNR"), 
                         font = "Cinzel Decorative", location = "+0+50",
                         color = alpha(text_color, .5), size = 75, gravity = "south")
  
  # Twitter
  img_ <- image_composite(img_, tw, gravity = "south",
                          offset = "-300+65")
  
  if (markups) {
    # Area
    img_ <- image_annotate(img_, glue("Max Depth: {label_comma()(round(max_depth))} ft"),
                           font = "Cinzel Decorative", location = "+50+900",
                           color = text_color, size = 100, gravity = "north")

    # Highest Point
    img_ <- image_annotate(img_, glue("Volume: {round((volume)/1000000, 1)}M ac-ft"),
                           font = "Cinzel Decorative", location = "+50+1200",
                           color = text_color, size = 100, gravity = "north")

    states <- spData::us_states 
    
    b <- st_point(c(lake_info$Lon, lake_info$Lat)) %>%
      st_sfc(crs = 4326) %>% 
      st_buffer(dist = 100000)
    
    loc_plot <- ggplot() + 
      geom_sf(data = states, fill = "transparent", color = text_color, size = 0.2) + 
      geom_sf(data = b, fill = colors[2], color = text_color) +
      theme_void() + 
      coord_sf(crs = 3347) 
    
    ggsave(loc_plot, filename = glue("plots/{map}_inset.png"), w = 4*1.5, h = 3*1.5)
    inset <- image_read(glue("plots/{map}_inset.png"))
    new_inset <- image_scale(inset, "x1000")
    img_ <- image_composite(img_, new_inset, gravity = "north",
                            offset = "+1450+800")
  }
  
  image_write(img_, glue("plots/{name}_titled_highres.png"))
  smimg <- image_scale(img_, "x3500")
  image_write(smimg, glue("plots/{name}_titled_insta_small.png"))
  
}

add_stuff(name = "Winnibigoshish_wild_rev", c = 8, c_fun = colors, t = 6, markups = TRUE)
#add_stuff(pal = "custom", map = "seattle", c = 8, c_fun = colors, t = 1, markups = FALSE)