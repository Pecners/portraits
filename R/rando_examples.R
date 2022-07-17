colors <- c("#061B4A", "#3871EF", "#FFB70B", "#FFFFFF")

data.frame(
  c = colors,
  ind = 1:4
) %>%
  ggplot(aes(fill = c)) +
  geom_rect(aes(xmin = ind - 1, xmax = ind, ymin = 0, ymax = 1)) +
  geom_text(aes(label = c, y = .5, x = ind - .5,
                color = ifelse(ind == 1, "white", "black")),
            size = 6) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_void()

ggsave("alaska_colors.png", width = 5, height = 3)


library(rnaturalearth)
library(sf)
library(ggplot2)

world <- ne_countries(scale = "medium", returnclass = "sf")

prj <- "+proj=ortho +lat_0=50 +lon_0=-150"

water <- st_sfc(st_point(c(50, -150)), crs = prj) |>
  st_buffer(6400000)

ggplot(data = world) +
  geom_sf(data = water, color = NA, fill = alpha(colors[2], .75)) +
  geom_sf(fill = "#ffd46d", size = .1, color = "grey40") +
  coord_sf(crs = prj) +
  theme_void() 

ggsave("inset.png")
