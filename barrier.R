require(tidyverse)
require(maps)
require(mapdata)
require(ggplot2)

dir1 = "/Users/Yuki/Dropbox/same"

# 5.2 barrier model -------------------------------------------------------
setwd('/Users/Yuki/FRA/INLAren/spde-book-files')

## ----opts, echo = FALSE, results = 'hide', message = FALSE, warning = FALSE----
source('R/initial_setup.R')
opts_chunk$set(
  fig.path = 'figs/barrier-'
)
library(scales)
library(rgeos)
## High resolution maps when using map()
library(mapdata) 
## Map features, map2SpatialPolygons()
library(maptools)


# North Japan -----------------------------------------------------------
# Select region 
map <- map("world", "Japan", fill = TRUE,
           col = "transparent", plot = TRUE)
IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
map.sp <- map2SpatialPolygons(
  map, IDs = IDs,
  proj4string = CRS("+proj=longlat +datum=WGS84"))
summary(map.sp)


# make a polygon ------------------------------------------------------------
pl.sel <- SpatialPolygons(list(Polygons(list(Polygon(
  cbind(c(135, 137, 139, 143, 144, 144), # x-axis 
        c(35,  39,  42,  42,  38,  36.2)), # y-axis
  FALSE)), '0')), proj4string = CRS(proj4string(map.sp)))
poly.water <- gDifference(pl.sel, map.sp)
plot(pl.sel)
plot(map.sp)
plot(poly.water)

## ------------------------------------------------------------------------
# Define UTM projection
kmproj <- CRS("+proj=utm +zone=53 ellps=WGS84 +units=km")
# Project data
poly.water = spTransform(poly.water, kmproj)
pl.sel = spTransform(pl.sel, kmproj)
map.sp = spTransform(map.sp, kmproj)

## ------------------------------------------------------------------------
mesh.not <- inla.mesh.2d(boundary = poly.water, max.edge = 30,
                         cutoff = 2)

## ----label = "plot-barr-mesh1", fig = TRUE, echo = FALSE, fig.align = "center", fig.width = 10, heigh = 4.5, width = '97%', fig.cap = "The left plot shows the polygon for land in grey and the manually constructed polygon for our study area in light blue. The right plot shows the simple mesh, constructed only in the water."----
par(mfrow = c(1, 2), mar = c(3, 3, 0.5, 0.5), mgp = c(2, 0.7, 0), las = 1)
par(mar = c(0, 0, 0, 0))

plot(pl.sel, col = alpha("skyblue", 0.5), asp = 1)
plot(map.sp, add = TRUE, col = alpha(gray(0.9), 0.5))

plot(pl.sel, asp = 1)
plot(map.sp, add = TRUE, col = gray(0.9))
plot(mesh.not, add = TRUE)

## ------------------------------------------------------------------------
max.edge = 30
bound.outer = 80
mesh <- inla.mesh.2d(boundary = poly.water,
                     max.edge = c(1, 5) * max.edge,
                     cutoff = 1,
                     offset = c(max.edge, bound.outer))
plot(mesh)


## ------------------------------------------------------------------------
water.tri = inla.over_sp_mesh(poly.water, y = mesh, 
                              type = "centroid", ignore.CRS = TRUE)
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


## ---- warning = FALSE, message = FALSE-----------------------------------
range <- 200
barrier.model <- inla.barrier.pcmatern(mesh, 
                                       barrier.triangles = barrier.tri)
Q <- inla.rgeneric.q(barrier.model, "Q", theta = c(0, log(range)))

## ---- warning = FALSE, message = FALSE-----------------------------------
stationary.model <- inla.spde2.pcmatern(mesh, 
                                        prior.range = c(1, 0.1), prior.sigma = c(1, 0.1))
Q.stat <- inla.spde2.precision(stationary.model, 
                               theta = c(log(range), 0))

