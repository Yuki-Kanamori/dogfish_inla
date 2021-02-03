require(tidyverse)
require(maps)
require(mapdata)
require(ggplot2)


# set directory and read data -----------------------------------------------------
dir1 = "/Users/Yuki/Dropbox/same"
setwd(dir1)

month = c(1,2,3,4,5,6,9,10,11,12)

for(m in month){
  ### for minimum data
  m1 = read.csv(paste0("same", m, ".csv"))
  summary(m1)
  m1 = m1 %>% select(year, month, lon, lat, kg)
  
  # 予備解析のためデータを小さくする
  m1 = m1 %>% filter(year < 2012)
  summary(m1)
  
  temp = m1
  
  # make time series
  times = data.frame(year = rep(min(temp$year):max(temp$year)), month = rep(unique(temp$month)))
  times = times %>% mutate(time = rep(1:nrow(times)))
  
  temp = left_join(temp, times, by = c("year", "month"))
  # temp = temp %>% filter(lon < 145) %>% filter(lat < 42)
  temp = temp %>% filter(lon < 143) %>% filter(lat < 42) %>% filter(lon > 135) %>% filter(lat > 37)
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
  
  ### 小さくした時
  pl.sel2 <- SpatialPolygons(list(Polygons(list(Polygon(
    cbind(c(135, 138, 139, 139, 143, 143, 142), # x-axis
          c(37,  38.5,  40.5, 43,  43,  39,  37)), # y-axis
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
  
  loc2 = NULL
  for(i in 1:length(unique(temp$time))){
    loc2 = rbind(loc2, coop)
  }
  
  Ap = inla.spde.make.A(mesh = mesh, loc = loc2)
  dim(Ap) #1518, 1738
  
  
  # ---------------------------------------------------------------
  # stack ---------------------------------------------------------
  # ---------------------------------------------------------------
  stk = inla.stack(
    data = list(y = catch),
    A = list(A, 1, 1),
    effects = list(s = 1:mesh$n, intercept = rep(1, length(catch)), time = temp$time),
    tag = 'est'
  )
  
  na = rep(NA, nrow(coop)*length(unique(temp$time)))
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
  
  
  reso = 100
  xlim = c(135, 143)
  ylim = c(37, 43)
  est = NULL
  for(i in 1:length(unique(temp$time))){
    # 推定値の緯度経度情報を引っ張り出す with 解像度
    proj = inla.mesh.projector(mesh, xlim = xlim,
                               ylim = ylim, dims=c(reso, reso))
    # 推定値を引っ張り出し，選択した解像度に変換する
    field.proj = inla.mesh.project(proj, res$summary.random$s$mean + res$summary.fixed$mean[1] +
                                     res$summary.random$time$mean[i]) #100*100の行列．
    
    z = data.frame(field.proj) 
    colnames(z) = proj$y
    z$x = proj$x
    z2 = z %>% gather(key = lat, value = prob, 1:(ncol(z)-1)) %>% dplyr::rename(lon = x) %>% 
      mutate(lat = as.numeric(str_sub(lat, 1, 6)), time = paste0(times[i, "year"], "_", times[i, "month"]))
    
    est = rbind(est, z2)
  }
  
  setwd(dir1)
  write.csv(est, paste0("est", m, ".csv"))
  
  summary(est)
  unique(est$time)
  # with map
  world_map <- map_data("world")
  jap <- subset(world_map, world_map$region == "Japan")
  jap_cog <- jap[jap$lat > 35 & jap$lat < 45 & jap$long > 130 & jap$long < 145, ]
  pol = geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="black", fill="black")
  c_map = coord_map(xlim = c(134.5, 143), ylim = c(36.5, 43))
  
  g = ggplot(est %>% na.omit() %>% filter(prob > log(0.3/0.7)), aes(x = lon, y = lat, fill = prob))
  # r = geom_raster()
  t = geom_tile()
  # v = scale_fill_viridis(na.value = "transparent")
  c = coord_fixed(ratio = 1)
  f = facet_wrap(~ time, ncol = 8)
  labs = labs(x = "Longitude", y = "Latitude", fill = "Logit \n (encounter probability)")
  th = theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.x = element_text(size = rel(1)),
             axis.text.y = element_text(size = rel(1)),
             axis.title.x = element_text(size = rel(1)),
             axis.title.y = element_text(size = rel(1)),
             legend.title = element_text(size = 10))
  fig = g+t+c+pol+labs+c_map+theme_bw()+scale_fill_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red", "darkred"))+f
  
  setwd(dir1)
  ggsave(filename = paste0("temp", m, ".pdf"), plot = fig, units = "in", width = 11.69, height = 8.27)
  ggsave(filename = paste0("temp", m, ".png"), plot = fig, units = "in", width = 11.69, height = 8.27)
  
}




# animation -----------------------------------------------------
# require(devtools)
# devtools::install_github("dgrtwo/gganimate")

require(gganimate)
require(ggplot2)
require(lubridate)

dir1 = "/Users/Yuki/Dropbox/same"
setwd(dir1)

month = c(1,2,3,4,5,6,9,10,11,12)
df = NULL
for(i in month){
  data = read.csv(paste0("est", i, ".csv"))
  df = rbind(df, data)
}
summary(df)
unique(df$time)
df = df %>% mutate(year = as.numeric(str_sub(time, 1, 4)), month = as.numeric(str_sub(time, 6, 7))) %>% select(-X, -time)

times = data.frame(year = rep(min(df$year):max(df$year), each = length(unique(df$month))), month = rep(unique(df$month)))
times = times %>% mutate(time = rep(1:nrow(times)))

df2 = left_join(df, times, by = c("year", "month"))

# map inf.
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 35 & jap$lat < 45 & jap$long > 130 & jap$long < 145, ]
pol = geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="black", fill="black")
c_map = coord_map(xlim = c(134.5, 143), ylim = c(36.5, 43))

# ggplot2
g = ggplot(df2 %>% na.omit() %>% filter(prob > log(0.3/0.7), year == 1972), aes(x = lon, y = lat, fill = prob))
t = geom_tile()
c = coord_fixed(ratio = 1)
labs = labs(x = "Longitude", y = "Latitude", fill = "Logit \n (encounter probability)")
# , title = "[{as.integer(frame_time)}] : {frame_time}"
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1)),
           axis.text.y = element_text(size = rel(1)),
           axis.title.x = element_text(size = rel(1)),
           axis.title.y = element_text(size = rel(1)),
           legend.title = element_text(size = 10))
fig = g+t+c+pol+labs+c_map+theme_bw()+scale_fill_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red", "darkred"))+transition_time(time)

fig = g+t+c+pol+labs+c_map+theme_bw()+scale_fill_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red", "darkred"))+transition_reveal(time)+ease_aes("cubic-in-out")

anim = animate(fig, fps = 20)
# fps: The framerate of the animation in frames/sec (default 10)

gganimate(anim)

# animate(fig, nframes = 24, renderer = gifski_renderer("year1972.gif"))
anim_save(anim, filenane = "year1972.gif")


# 一枚づつgifでプロット
gg_animate(fig)





