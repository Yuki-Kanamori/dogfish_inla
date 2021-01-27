require(tidyverse)
require(maps)
require(mapdata)
require(ggplot2)


# set directory and read data -----------------------------------------------------
dir1 = "/Users/Yuki/Dropbox/same"
setwd(dir1)

### for all data
temp = NULL
for(i in 1:12){
  df = read.csv(paste0("same", i, ".csv"))
  temp = rbind(temp, df)
}

summary(temp)


# make time series
times = data.frame(year = rep(min(temp$year):max(temp$year), each = length(unique(temp$month))), month = rep(1:max(temp$month)))
times = times %>% filter(month != 7) %>% filter(month != 8) %>% filter(year != 2012)
times = times %>% mutate(time = rep(1:nrow(times)))

# combine
temp = temp %>% select(year, month, lon, lat, kg) %>% filter(month != 7) %>% filter(month != 8) %>% filter(year != 2012)
unique(temp$month)
temp = left_join(temp, times, by = c("year", "month"))
summary(temp)

temp = temp %>% filter(lon < 145) %>% filter(lat < 42) %>% filter(year < 1988)
summary(temp)
catch = (temp$kg > 0) + 0 #バイナリーデータに変換
# 
# 



### for minimum data
m1 = read.csv('same1.csv')
summary(m1)
m1 = m1 %>% select(year, month, lon, lat, kg)

# 予備解析のためデータを小さくする
m1 = m1 %>% filter(year < 1975)
summary(m1)

temp = m1

# make time series
times = data.frame(year = rep(min(temp$year):max(temp$year)), month = rep(unique(temp$month)))
times = times %>% mutate(time = rep(1:nrow(times)))

temp = left_join(temp, times, by = c("year", "month"))
temp = temp %>% filter(lon < 145) %>% filter(lat < 42)
summary(temp)
catch = (temp$kg > 0) + 0 #バイナリーデータに変換
summary(temp)


### for some year using all month data
m1 = NULL
for(i in 1:12){
  data = read.csv(paste0("same", i, ".csv"))
  m1 = rbind(m1, data)
}

summary(m1)
temp = m1 %>% dplyr::filter(year == 1990)
summary(temp$year)

# make time series
times = data.frame(year = rep(min(temp$year):max(temp$year)), month = rep(unique(temp$month)))
times = times %>% mutate(time = rep(1:nrow(times)))

temp = left_join(temp, times, by = c("year", "month"))
temp = temp %>% filter(lon < 145) %>% filter(lat < 42)
summary(temp)
catch = (temp$kg > 0) + 0 #バイナリーデータに変換

summary(temp)


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
## lat < 42で北海道を除去した場合
pl.sel <- SpatialPolygons(list(Polygons(list(Polygon(
  cbind(c(128, 132, 138, 144, 144, 144), # x-axis
        c(34,  39,  43,  43,  38,  34)), # y-axis
  FALSE)), '0')), proj4string = CRS(proj4string(map.sp))) #緯度経度データ

# ### 北海道を除去しなかった場合
# pl.sel <- SpatialPolygons(list(Polygons(list(Polygon(
#   cbind(c(128, 132, 138, 151, 151, 144, 144, 144), # x-axis 
#         c(34,  39,  43,  47, 43.5,  41, 38,  34)), # y-axis
#   FALSE)), '0')), proj4string = CRS(proj4string(map.sp))) #緯度経度データ
# 
# ### 北海道の日本海側・オホーツクも推定する場合
# pl.sel <- SpatialPolygons(list(Polygons(list(Polygon(
#   cbind(c(128, 132, 138, 140, 151, 151, 144, 144, 144), # x-axis 
#         c(34,  39,  43,  46, 47, 43.5,  41, 38,  34)), # y-axis
#   FALSE)), '0')), proj4string = CRS(proj4string(map.sp))) #緯度経度データ

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
loc = as.matrix(cbind(temp$lon, temp$lat))

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


## lat < 42で北海道を除去した場合
pl.sel2 <- SpatialPolygons(list(Polygons(list(Polygon(
  cbind(c(128, 132, 138, 144, 144, 144), # x-axis 
        c(34,  39,  43,  43,  38,  34)), # y-axis
  FALSE)), '0')), proj4string = CRS(proj4string(map.sp))) #緯度経度データ

# ### 北海道を除去しなかった場合
# pl.sel2 <- SpatialPolygons(list(Polygons(list(Polygon(
#   cbind(c(128, 132, 138, 151, 151, 144, 144, 144), # x-axis 
#         c(34,  39,  43,  47, 43.5,  41, 38,  34)), # y-axis
#   FALSE)), '0')), proj4string = CRS(proj4string(map.sp))) #緯度経度データ
# 
# ### 北海道の日本海側・オホーツクも推定する場合
# pl.sel2 <- SpatialPolygons(list(Polygons(list(Polygon(
#   cbind(c(128, 132, 138, 140, 151, 151, 144, 144, 144), # x-axis 
#         c(34,  39,  43,  46, 47, 43.5,  41, 38,  34)), # y-axis
#   FALSE)), '0')), proj4string = CRS(proj4string(map.sp))) #緯度経度データ

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

loc2 = NULL
for(i in 1:length(unique(temp$time))){
  loc2 = rbind(loc2, coop)
}

Ap = inla.spde.make.A(mesh = mesh, loc = loc2)
dim(Ap) #1518, 1738


# ---------------------------------------------------------------
# stack ---------------------------------------------------------
# ---------------------------------------------------------------
# stk = inla.stack(
#   data = list(y = catch),
#   A = list(A, 1, 1),
#   effects = list(s = 1:mesh$n, intercept = rep(1, length(catch)*length(unique(temp$time))), time = rep(1:length(unique(temp$time)), each = length(catch))),
#   tag = 'est'
# )
stk = inla.stack(
  data = list(y = catch),
  A = list(A, 1, 1),
  effects = list(s = 1:mesh$n, intercept = rep(1, length(catch)), time = temp$time),
  tag = 'est'
)

na = rep(NA, nrow(coop)*length(unique(temp$time)))
# stk.pred = inla.stack(
#   data = list(y = NA),
#   A = list(A.pred, 1),
#   effects = list(s = 1:mesh$n, intercept = rep(1, nrow(A.pred))),
#   tag = 'pred'
# )
stk.pred = inla.stack(
  data = list(y = na),
  A = list(Ap, 1, 1),
  effects = list(s = 1:mesh$n, intercept = rep(1, nrow(coop)*length(unique(temp$time))), time = rep(1:length(unique(temp$time)), each = nrow(coop))),
  tag = 'pred'
)
joint.stk = inla.stack(stk, stk.pred)


# ---------------------------------------------------------------
# formula -------------------------------------------------------
# ---------------------------------------------------------------
form.barrier = y ~ 0 + intercept + f(s, model = barrier.model) + f(time, model = "rw1", scale.model = TRUE)


# ---------------------------------------------------------------
# fitting -------------------------------------------------------
# ---------------------------------------------------------------
res = inla(form.barrier, data = inla.stack.data(joint.stk),
           control.predictor = list(A = inla.stack.A(joint.stk)),
           family = 'binomial', 
           control.inla = list(int.strategy = "eb"),
           control.compute = list(waic = TRUE, dic = TRUE))
res$waic$waic; res$dic$dic
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
recheck = data.frame(east = coop[, 1], north = coop[, 2],
                     value = pred_mean_c, variable = "pred_mean_catch")

dpm_c$variable = as.factor(dpm_c$variable)

g2 = ggplot(data = dpm_c, aes(east, north, fill = value))
t = geom_tile()
f = facet_wrap(~ variable)
c = coord_fixed(ratio = 1)
s = scale_fill_gradient(name = "encounter prob. (logit)", low = "blue", high = "orange")
g2+t+f+c+s+theme_bw()


# with map
world_map <- map_data("world")

### 北海道を除く場合
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 33 & jap$lat < 43 & jap$long > 127 & jap$long < 145, ]

### 北海道を入れる場合
jap_cog <- world_map[world_map$lat > 33 & world_map$lat < 47 & world_map$long > 127 & world_map$long < 152, ]

pol = geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")
c_map = coord_map(xlim = c(127, 145), ylim = c(33, 43))

m_dpm = dpm_c %>% filter(str_detect(variable, "mean"))
unique(m_dpm$variable)
summary(m_dpm)

g = ggplot(data = m_dpm, aes(east, north, colour = value))
p = geom_point()
g+p+pol+theme_bw()+labs(x = "", y = "", title = "Jan. 1972-1981", colour = "Logit\n (encounter prob.)") + scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))




# re-make maps --------------------------------------------------
local.plot.field <- function(field, ...){
  # xlim = c(127, 145)
  # ylim = c(33, 43)
  xlim = c(127, 152)
  ylim = c(33, 47)
  proj = inla.mesh.projector(mesh, xlim = xlim,
                             ylim = ylim, dims=c(300, 300))
  field.proj = inla.mesh.project(proj, field)
  image.plot(list(x = proj$x, y = proj$y, z = field.proj),
             xlim = xlim, ylim = ylim, ...)
  plot(poly.barrier, add = TRUE, col = 'grey')
}


### year trend
# par(mfrow = c(9, 5), mar = c(2, 2, 1, 1), mgp = c(2, 0.7, 0), las = 1, oma = c(0, 0, 0.5, 1))
par(mfrow = c(1, 3), mar = c(2, 2, 1, 1), mgp = c(2, 0.7, 0), las = 1, oma = c(0, 0, 0.5, 1))
for(i in 1:length(unique(temp$time))) {
  # Rough estimate of posterior mean
  local.plot.field(
    res$summary.random$s$mean + res$summary.fixed$mean[1] +
      res$summary.random$time$mean[i],
    main = paste0(i+1971), zlim = c(-7, 4), asp = 1,
    col = book.color.c(100),
    axes = FALSE)
}

### seasonality
par(mfrow = c(4, 3), mar = c(2, 2, 1, 1), mgp = c(2, 0.7, 0), las = 1, oma = c(0, 0, 0.5, 1))
for(i in 1:length(unique(temp$time))) {
  # Rough estimate of posterior mean
  local.plot.field(
    res$summary.random$s$mean + res$summary.fixed$mean[1] +
      res$summary.random$time$mean[i],
    main = paste0(times[i, "month"]), zlim = c(-1, 1), asp = 1,
    col = book.color.c(100),
    axes = FALSE)
}


par(mfrow = c(2, 3), mar = c(2, 2, 1, 1), mgp = c(2, 0.7, 0), las = 1, oma = c(0, 0, 0.5, 1))
for(i in 1:6) {
  # Rough estimate of posterior mean
  local.plot.field(
    res$summary.random$s$mean + res$summary.fixed$mean[1] +
      res$summary.random$time$mean[i],
    main = paste0(times[i, "month"]), zlim = c(-7, 4), asp = 1,
    col = book.color.c(100),
    axes = FALSE)
}

space = res$summary.random$s$mean #vector 1738
intercept = res$summary.fixed$mean[1] #scaler
time = res$summary.random$time$mean[1] #scaler

test = space+intercept+time #長さ1738


test2 = data.frame(east = coop[, 1], north = coop[, 2], #1518
                   value = test, variable = "pred_mean_time1")


