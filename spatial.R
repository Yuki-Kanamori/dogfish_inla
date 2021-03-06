require(tidyverse)
require(maps)
require(mapdata)
require(ggplot2)


# set directory and read data -----------------------------------------------------
dir1 = "/Users/Yuki/Dropbox/same"
setwd(dir1)

m1 = read.csv('same1.csv')
summary(m1)
m1 = m1 %>% select(year, lon, lat, kg)

# 予備解析のためデータを小さくする
m1 = m1 %>% filter(year < 1982)
summary(m1)
catch = (m1$kg > 0) + 0 #バイナリーデータに変換


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
loc = as.matrix(cbind(m1$lon, m1$lat))

# projector matrix
A = inla.spde.make.A(mesh, loc = loc)
dim(A) 
table(rowSums(A > 0)) 
table(rowSums(A)) 
table(colSums(A) > 0) 

plot(mesh)
points(loc, col = "red", pch = 16, cex = 0.5)



# ---------------------------------------------------------------
# set the prediction points -------------------------------------
# ---------------------------------------------------------------
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
coop = coop[which(ind == 1), ] #1516
plot(coop, asp = 1)

Ap = inla.spde.make.A(mesh = mesh, loc = coop)
dim(Ap) #1518, 1738




# ---------------------------------------------------------------
# stack ---------------------------------------------------------
# ---------------------------------------------------------------
stk = inla.stack(
  data = list(y = catch),
  A = list(A, 1),
  effects = list(s = 1:mesh$n, intercept = rep(1, length(catch))),
  tag = 'est'
)

na = rep(NA, nrow(coop))
# stk.pred = inla.stack(
#   data = list(y = NA),
#   A = list(A.pred, 1),
#   effects = list(s = 1:mesh$n, intercept = rep(1, nrow(A.pred))),
#   tag = 'pred'
# )
stk.pred = inla.stack(
  data = list(y = na),
  A = list(Ap, 1),
  effects = list(s = 1:mesh$n, intercept = rep(1, nrow(coop))),
  tag = 'pred'
)
joint.stk = inla.stack(stk, stk.pred)


# ---------------------------------------------------------------
# formula -------------------------------------------------------
# ---------------------------------------------------------------
form.barrier = y ~ 0 + intercept + f(s, model = barrier.model)


# ---------------------------------------------------------------
# fitting -------------------------------------------------------
# ---------------------------------------------------------------
res = inla(form.barrier, data = inla.stack.data(joint.stk),
                    control.predictor = list(A = inla.stack.A(joint.stk)),
                    family = 'binomial', 
                    control.inla = list(int.strategy = "eb"),
                    control.compute = list(waic = TRUE, dic = TRUE))
res$waic$waic; res$dic$dic #28156, 28165
summary(res)


# plot the fitted values on a map -------------------------------
best_kono = res

index_cp = inla.stack.index(joint.stk, tag = "pred")$data

pred_mean_c = best_kono$summary.fitted.values[index_cp, "mean"]
pred_ll_c = best_kono$summary.fitted.values[index_cp, "0.025quant"]
pred_ul_c = best_kono$summary.fitted.values[index_cp, "0.975quant"]

dpm_c = rbind(data.frame(east = coop[, 1], north = coop[, 2],
                         value = pred_mean_c, variable = "pred_mean_catch"),
              data.frame(east = coop[, 1], north = coop[, 2],
                         value = pred_ll_c, variable = "pred_ll_catch"),
              data.frame(east = coop[, 1], north = coop[, 2],
                         value = pred_ul_c, variable = "pred_ul_catch"))

dpm_c$variable = as.factor(dpm_c$variable)

g2 = ggplot(data = dpm_c, aes(east, north, fill = value))
t = geom_tile()
f = facet_wrap(~ variable)
c = coord_fixed(ratio = 1)
s = scale_fill_gradient(name = "encounter prob. (logit)", low = "blue", high = "orange")
g2+t+f+c+s+theme_bw()


# with map
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 33 & jap$lat < 43 & jap$long > 127 & jap$long < 145, ]
pol = geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")
c_map = coord_map(xlim = c(127, 145), ylim = c(33, 43))

m_dpm = dpm_c %>% filter(str_detect(variable, "mean"))
unique(m_dpm$variable)

# g = ggplot(data = m_dpm, aes(east, north, fill = value))
# t = geom_tile()
# f = facet_wrap(~ variable)
# c = coord_fixed(ratio = 1)
# s = scale_fill_gradient(name = "encounter prob. (logit)", low = "blue", high = "orange")
# g+t+f+c+s+pol+c_map+theme_bw()+labs(x = "", y = "", title = "Jan. 1972-1981", colour = "Logit (encounter prob.)")
# 
g = ggplot(data = m_dpm, aes(east, north, colour = value))
p = geom_point()
g+p+pol+theme_bw()+labs(x = "", y = "", title = "Jan. 1972-1981", colour = "Logit\n (encounter prob.)") + scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))

