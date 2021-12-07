library(tidyverse)
library(ggplot2)
library(purrr)
library(reshape2)
library(raster)
library(numbers)
library(rayshader)
library(rayrender)
library(rgl)
library(terrainmeshr)

#import data
importData <- read.csv(file.choose())

#run for 3D plotting
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

#data wrangling
dt <- importData %>% 
  dplyr::select(-reflection, -specific) %>% 
  melt(id.vars = c("day", "hour")) %>% 
  arrange(day, hour) %>%
  filter(value != 0) %>% 
  rename(d = day, h = hour) %>%
  mutate(
    v = case_when(
      variable == "joy" ~ 20,
      variable == "interest" ~ 19,
      variable == "gratitude" ~ 15,
      variable == "amusement" ~14,
      variable == "calm" ~ 8,
      variable == "nostalgia" ~ 7,
      variable == "anxiety" ~ 3,
      variable == "boredom" ~ 3,
      variable == "annoyance" ~ 3,
      variable == "loneliness" ~ 2,
      variable == "anger" ~ 1,
      variable == "sadness" ~ 0
    ),
    t = 1
  ) %>%
  dplyr::select(d, h, v, t)

#shift and iterate functions
shift <- function(df, dx, dy) {
  rx = range(df$h)[2] - range(df$h)[1] + 2
  ry = range(df$d)[2] - range(df$d)[1] + 1.5
  df <- df %>% mutate(
    h = h + dx*rx,
    d = d + dy*ry,
    v = mod(v + 3, 21),
    t = t*.9
  )
  return(df)
}

iterate <- function(df, x, y) {
  df <- bind_rows(
    accumulate2(
      .x = x,
      .y = y,
      .f = shift,
      .init = df
    )
  )
  return(df)
}

#create grid
xlist <- c(0, 1, 0, 0, -1, -1, 0, 0,
           0, 1, 1, 1, 0, 0, 0, 0, -1, -1, -1, -1, 0, 0, 0, 0)
ylist <- c(-1, 0, 1, 1, 0, 0, -1, -1,
           -1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, -1, -1, -1, -1)
dg <- iterate(dt, xlist, ylist)

#create raster, disaggregate, and revert to data frame
dr <- rasterFromXYZ(dg) %>% disaggregate(10)
df <- data.frame(rasterToPoints(dr)) 

tiff("grid.tiff", height = 6000, width = 6000, units = "px", pointsize = 12, res = 300, compression = 'lzw')

ggplot(data = df) +
  geom_raster(aes(x = y, y = -x, fill = v,
                  alpha = t)) +
  scale_fill_gradientn(colours = colorPalette) +
  scale_alpha_identity() +
  xlim(min(df$y) - 20, max(df$y) + 20) +
  ylim(min(df$x) - 30, max(df$x) + 10) +
  theme_void() +
  theme(legend.position = "none")

dev.off()

#color palette
colorPalette = c("#222222",
                 "#45668f",
                 "#31c539",
                 "#e1a52d", 
                 "#cd2826")

#create TIFF
tiff("1week12d.tiff", height = 6000, width = 6000, units = "px", pointsize = 12, res = 300, compression = 'lzw')

#plot
theme_set(theme_void())
ggraster <- ggplot(data = df) +
  geom_raster(aes(x = y, y = -x, 
                  fill = v, alpha = t)) +
  scale_fill_gradientn(colours = colorPalette) +
  scale_alpha_identity() +
  xlim(min(df$y) - 20, max(df$y) + 20) +
  ylim(min(df$x) - 30, max(df$x) + 20) +
  theme(legend.position = "none",
        panel.background = element_rect("white"))

#plot 3D
plot_gg(ggraster, width = 6, height = 4, raytrace = TRUE, multicore = TRUE, scale = 300,
        zoom = 0.3, theta = 180, phi = 0, fov = 0,
        solid = FALSE,
        sunangle = 225,
        max_error = 0.01,
        windowsize = c(1200, 800))
render_highquality(samples = 400, aperture = 30,
                   light = FALSE, ambient = TRUE)
#render_snapshot(clear = TRUE)
rgl.close()
dev.off()

