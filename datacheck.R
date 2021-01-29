require(tidyverse)
require(maps)
require(mapdata)
require(ggplot2)

# set directory and read data -----------------------------------------------------
dir1 = "/Users/Yuki/Dropbox/same"
setwd(dir1)

month = c(1,2,3,4,5,6,9,10,11,12)

temp = NULL
for(i in month){
  df = read.csv(paste0("same", i, ".csv"))
  temp = rbind(temp, df)
}
summary(temp)

temp = temp %>% filter(year < 2012) 
temp = temp %>% filter(lon < 143) %>% filter(lat < 42) %>% filter(lon > 135) %>% filter(lat > 37)
summary(temp)
unique(temp$month)

df = temp

map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
region2 = subset(world_map, world_map$region == region)
local_map = map + geom_polygon(data = region2, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + coord_map(xlim = c(min(df$lon)-0.2, max(df$lon)+0.2), ylim = c(min(df$lat)-0.2, max(df$lat)+0.2))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1)),
           axis.text.y = element_text(size = rel(1)),
           axis.title.x = element_text(size = rel(1)),
           axis.title.y = element_text(size = rel(1)),
           legend.title = element_text(size = 10))
p = geom_point(data = df , aes(x = lon, y = lat, colour = kg), shape = 16, size = 1)
# p = geom_point(data = same %>% filter(kg > 10000), aes(x = lon, y = lat, colour = kg), shape = 16, size = 1)
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
labs = labs(x = "Longitude", y = "Latitude", colour = "kg")
f = facet_wrap(~ month, ncol = 5)

fig = local_map+theme_bw()+th+p+c+labs+f
ggsave(filename = "map_same.pdf", plot = fig, units = "in", width = 11.69, height = 8.27)




# ---------------------------------------------------------------
# make the mesh with barrier ---------------------------------------------------------------
# ---------------------------------------------------------------
# setting for barrier model -------------------------------------------------------
setwd('/Users/Yuki/FRA/INLAren/spde-book-files')

## ----opts, echo = FALSE, results = 'hide', message = FALSE, warning = FALSE----
source('R/initial_setup.R')
opts_chunk$set(
  fig.path = 'figs/barrier-'
)
source('R/spde-book-functions.R')

library(scales)
library(rgeos)
## High resolution maps when using map()
library(mapdata) 
## Map features, map2SpatialPolygons()
library(maptools)


# North Japan -----------------------------------------------------------
# Select region 
# map <- map("world", "Japan", fill = TRUE,
#            col = "transparent", plot = TRUE)Russia
map <- map("world", c("Japan", "Russia"), fill = TRUE,
           col = "transparent", plot = TRUE)
IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
map.sp <- map2SpatialPolygons(
  map, IDs = IDs,
  proj4string = CRS("+proj=longlat +datum=WGS84")) #緯度経度データ
summary(map.sp)



# make a polygon ------------------------------------------------------------

### 小さくした時
pl.sel <- SpatialPolygons(list(Polygons(list(Polygon(
  cbind(c(135, 138, 139, 139, 143, 143, 142), # x-axis
        c(37,  38.5,  40.5, 43,  43,  39,  37)), # y-axis
  FALSE)), '0')), proj4string = CRS(proj4string(map.sp))) #緯度経度データ

summary(pl.sel)
poly.water <- gDifference(pl.sel, map.sp) #緯度経度
summary(poly.water)
plot(pl.sel)
plot(map.sp)
plot(poly.water)

## ------------------------------------------------------------------------
mesh.not <- inla.mesh.2d(boundary = poly.water, max.edge = 0.5,
                         cutoff = 0.2)
plot(mesh.not)
summary(mesh.not) #緯度経度


## ----label = "plot-barr-mesh1", fig = TRUE, echo = FALSE, fig.align = "center", fig.width = 10, heigh = 4.5, width = '97%', fig.cap = "The left plot shows the polygon for land in grey and the manually constructed polygon for our study area in light blue. The right plot shows the simple mesh, constructed only in the water."----
par(mfrow = c(1, 1), mar = c(3, 3, 0.5, 0.5), mgp = c(2, 0.7, 0), las = 1)
par(mar = c(0, 0, 0, 0))

plot(pl.sel, col = alpha("skyblue", 0.5), asp = 1)
plot(map.sp, add = TRUE, col = alpha(gray(0.9), 0.5))

plot(pl.sel, asp = 1)
plot(map.sp, add = TRUE, col = gray(0.9))
plot(mesh.not, add = TRUE) #ここが変！=>修正済

## ------------------------------------------------------------------------
mesh <- inla.mesh.2d(boundary = poly.water,
                     max.edge = 0.5,
                     cutoff = 0.2,
                     offset = c(1, 1))
plot(mesh)


## ------------------------------------------------------------------------
water.tri = inla.over_sp_mesh(poly.water, y = mesh, 
                              type = "centroid", ignore.CRS = TRUE)
# ignore.CRS: whether to ignore the coordinate system information in x and y (default FALSE)
summary(water.tri)
num.tri = length(mesh$graph$tv[, 1])
barrier.tri = setdiff(1:num.tri, water.tri)
poly.barrier = inla.barrier.polygon(mesh, 
                                    barrier.triangles = barrier.tri)
plot(poly.barrier)

## ----label = "plot-barr-mesh2", fig = TRUE, echo = FALSE, fig.align = "center", fig.width = 6, heigh = 4.5, width = '97%', fig.cap = "The mesh constructed both over water and land. The grey region is the original land map. The inner red outline marks the coastline barrier."----

plot(mesh, lwd = 0.5, add = FALSE)
plot(pl.sel, add = TRUE)
plot(map.sp, add = TRUE, col = gray(.9))
plot(poly.barrier, border = "red", add = TRUE)


# ---------------------------------------------------------------
# spde ----------------------------------------------------------
# ---------------------------------------------------------------
range <- 200
barrier.model <- inla.barrier.pcmatern(mesh, 
                                       barrier.triangles = barrier.tri)
Q <- inla.rgeneric.q(barrier.model, "Q", theta = c(0, log(range)))




# ---------------------------------------------------------------
# projector matrix ----------------------------------------------
# ---------------------------------------------------------------
# lonlat
# loc = as.matrix(cbind(m1$lon, m1$lat))
par(mfrow = c(2, 5), mar = c(2, 2, 1, 1), mgp = c(2, 0.7, 0), las = 1, oma = c(0, 0, 0.5, 1))
for(i in month){
  loc = as.matrix(cbind(df %>% filter(month == i) %>% select(lon), df %>% filter(month == i) %>% select(lat)))
  
  # projector matrix
  A = inla.spde.make.A(mesh, loc = loc)
  dim(A) 
  table(rowSums(A > 0)) 
  table(rowSums(A)) 
  table(colSums(A) > 0) 
  
  plot(mesh)
  points(loc, col = "red", pch = 16, cex = 0.5)
}


df$pa = (df$kg > 0) + 0

# monthly
pa = df %>% dplyr::group_by(month) %>% dplyr::summarize(pre = sum(pa))
check = df %>% dplyr::group_by(month) %>% dplyr::summarize(total = n()) 
check = left_join(check, pa, by = "month") %>% mutate(freq = pre/total)

# year-month
pa = df %>% dplyr::group_by(year, month) %>% dplyr::summarize(pre = sum(pa))
check = df %>% dplyr::group_by(year, month) %>% dplyr::summarize(total = n()) 
check = left_join(check, pa, by = c("year", "month")) %>% mutate(freq = pre/total)

for(i in month){
  print(summary(check %>% filter(month == i)))
  
}

check = check %>% arrange(month)



map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
region2 = subset(world_map, world_map$region == region)
local_map = map + geom_polygon(data = region2, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + coord_map(xlim = c(min(df$lon)-0.2, max(df$lon)+0.2), ylim = c(min(df$lat)-0.2, max(df$lat)+0.2))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1)),
           axis.text.y = element_text(size = rel(1)),
           axis.title.x = element_text(size = rel(1)),
           axis.title.y = element_text(size = rel(1)),
           legend.title = element_text(size = 10))
# c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
f = facet_wrap(~ month, ncol = 5)

p = geom_point(data = df, aes(x = lon, y = lat, colour = as.character(pa)), shape = 16, size = 1)
labs = labs(x = "Longitude", y = "Latitude", colour = "occurrance")
  
local_map+theme_bw()+th+p+labs+f



