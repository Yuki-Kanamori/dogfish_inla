require(tidyverse)
require(maps)
require(mapdata)
require(ggplot2)

dir1 = "/Users/Yuki/Dropbox/same"
setwd(dir1)

m1 = read.csv('same1.csv')
summary(m1)
m1 = m1 %>% mutate(pa = ifelse(kg > 0, 1, 0)) %>% select(year, lon, lat, pa)

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
  proj4string = CRS("+proj=longlat +datum=WGS84")) #緯度経度データ
summary(map.sp)


# make a polygon ------------------------------------------------------------
# pl.sel <- SpatialPolygons(list(Polygons(list(Polygon(
#   cbind(c(135, 137, 139, 143, 144, 144), # x-axis 
#         c(35,  39,  42,  42,  38,  36.2)), # y-axis
#   FALSE)), '0')), proj4string = CRS(proj4string(map.sp)))
pl.sel <- SpatialPolygons(list(Polygons(list(Polygon(
  cbind(c(128, 132, 138, 144, 144, 144), # x-axis 
        c(34,  39,  43,  43,  38,  34)), # y-axis
  FALSE)), '0')), proj4string = CRS(proj4string(map.sp))) #緯度経度データ
summary(pl.sel)
poly.water <- gDifference(pl.sel, map.sp) #緯度経度
summary(poly.water)
plot(pl.sel)
plot(map.sp)
plot(poly.water)


# ------------------------------------------------------------------------
# Define UTM projection
# ここでUTMに変換しており，緯度経度でなくなっている
# kmproj <- CRS("+proj=utm +zone=53 ellps=WGS84 +units=km")
# # Project data
# poly.water = spTransform(poly.water, kmproj)
# pl.sel = spTransform(pl.sel, kmproj)
# map.sp = spTransform(map.sp, kmproj)

## ------------------------------------------------------------------------
mesh.not <- inla.mesh.2d(boundary = poly.water, max.edge = 0.5,
                         cutoff = 0.2)
plot(mesh.not)
summary(mesh.not) #緯度経度


## ----label = "plot-barr-mesh1", fig = TRUE, echo = FALSE, fig.align = "center", fig.width = 10, heigh = 4.5, width = '97%', fig.cap = "The left plot shows the polygon for land in grey and the manually constructed polygon for our study area in light blue. The right plot shows the simple mesh, constructed only in the water."----
par(mfrow = c(1, 2), mar = c(3, 3, 0.5, 0.5), mgp = c(2, 0.7, 0), las = 1)
par(mar = c(0, 0, 0, 0))

plot(pl.sel, col = alpha("skyblue", 0.5), asp = 1)
plot(map.sp, add = TRUE, col = alpha(gray(0.9), 0.5))

plot(pl.sel, asp = 1)
plot(map.sp, add = TRUE, col = gray(0.9))
plot(mesh.not, add = TRUE) #ここが変！=>修正済

## ------------------------------------------------------------------------
# max.edge = 30
# bound.outer = 80
# mesh <- inla.mesh.2d(boundary = poly.water,
#                      max.edge = c(1, 5) * max.edge,
#                      cutoff = 1,
#                      offset = c(max.edge, bound.outer))
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

# lonlat
loc = as.matrix(cbind(m1$lon, m1$lat))

# projector matrix
A = inla.spde.make.A(mesh, loc = loc)
dim(A) # 185229, 1738
table(rowSums(A > 0)) #3が185229
table(rowSums(A)) #1が185229
table(colSums(A) > 0) #FALSEが1368 TRUEが370

plot(mesh)
points(loc, col = "red", pch = 16, cex = 0.5)

# for prediction ------------------------------------------------
# Select region 
map <- map("world", "Japan", fill = TRUE,
           col = "transparent", plot = TRUE)
IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
map.sp <- map2SpatialPolygons(
  map, IDs = IDs,
  proj4string = CRS("+proj=longlat +datum=WGS84"))
summary(map.sp)


pl.sel2 <- SpatialPolygons(list(Polygons(list(Polygon(
  cbind(c(128, 132, 138, 144, 144, 144), # x-axis 
        c(34,  39,  43,  43,  38,  34)), # y-axis
  FALSE)), '0')), proj4string = CRS(proj4string(map.sp))) #緯度経度データ
summary(pl.sel2)
poly.water2 <- gDifference(pl.sel2, map.sp)
plot(pl.sel2)
plot(map.sp)
plot(poly.water2)
summary(poly.water2) #xmax = 144

tok_bor = poly.water2@polygons[[1]]@Polygons[[1]]@coords
summary(tok_bor) #xmax = 141 ここが変！=>直った？

bb_tok = poly.water@bbox
summary(bb_tok)
x = seq(bb_tok[1, "min"] - 5, bb_tok[1, "max"] + 5, length.out = 100)
y = seq(bb_tok[2, "min"] - 5, bb_tok[2, "max"] + 5, length.out = 100)
coop = as.matrix(expand.grid(x, y))
summary(coop)
ind = point.in.polygon(coop[, 1], coop[, 2],
                       tok_bor[, 1], tok_bor[, 2])
coop = coop[which(ind == 1), ]
plot(coop, asp = 1)

Ap = inla.spde.make.A(mesh = mesh, loc = coop)
dim(Ap) #1518, 1738

A.pred = inla.spde.make.A(mesh, mesh$loc[, 1:2])

# stack ---------------------------------------------------------
stk = inla.stack(
  data = list(y = m1$pa),
  A = list(A, 1),
  effects = list(s = 1:mesh$n, intercept = rep(1, nrow(m1))),
  tag = 'est'
)

stk.pred = inla.stack(
  data = list(y = NA),
  A = list(A.pred, 1),
  effects = list(s = 1:mesh$n, intercept = rep(1, nrow(A.pred))),
  tag = 'pred'
)

joint.stk = inla.stack(stk, stk.pred)

form.barrier = y ~ 0 + intercept + f(s, barrier.model)

stdev.pcprior = list(prior = 'pc.prec', param = c(2, 0.01))

res = inla(
  form.barrier,
  data = inla.stack.data(joint.stk),
  control.predictor = list(A = inla.stack.A(joint.stk),
                           compute = TRUE),
  family = 'binomial',
  control.inla = list(int.strategy = 'eb')
)

res = inla(form.barrier, 
                data = inla.stack.data(joint.stk), 
                family = c("binomial"), 
                control.predictor = list(compute = TRUE, A = inla.stack.A(joint.stk)), 
                control.results = list(return.marginals.random = FALSE, return.marginals.predictor = FALSE), 
                control.compute = list(waic = TRUE, dic = TRUE))
