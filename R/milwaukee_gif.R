library(magick)

imgs <- c("plots/milwaukee_titled_Demuth_deep.png",
          "plots/milwaukee_titled_peoples_flag_deep.png")

imgs <- imgs[c(1, 1, 2, 2, 1)]

img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_resize(img_joined, '700') %>%
  image_morph(frames = 20) %>%
  image_animate(fps = 10, optimize = TRUE)

## view animated image
#img_animated

## save to disk
image_write(image = img_animated,
            path = "plots/milwaukee.gif")
