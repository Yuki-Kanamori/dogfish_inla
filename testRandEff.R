require(tidyverse)
require(maps)
require(mapdata)
require(ggplot2)


# INLA -------------------------------------------------------
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


### for some year using all month data
m1 = NULL
for(i in 1:12){
  data = read.csv(paste0("same", i, ".csv"))
  m1 = rbind(m1, data)
}

summary(m1)
temp = m1 %>% dplyr::filter(year < 1975)
summary(temp$year)

# make time series
times = data.frame(year = rep(min(temp$year):max(temp$year)), month = rep(unique(temp$month)))
times = times %>% mutate(time = rep(1:nrow(times)))

temp = left_join(temp, times, by = c("year", "month"))
temp = temp %>% filter(lon < 145) %>% filter(lat < 42)
summary(temp)
catch = (temp$kg > 0) + 0 #バイナリーデータに変換

summary(temp)



# create mesh -----------------------------------------------------
loc = as.matrix(cbind(temp$lon, temp$lat))

mesh <- inla.mesh.2d(loc,
                     max.edge = 0.5,
                     cutoff = 0.2,
                     offset = c(1, 1))

plot(mesh)

## ----spde----------------------------------------------------------------
spde <- inla.spde2.pcmatern(mesh = mesh, 
                            prior.range = c(0.5, 0.01), # P(range < 0.05) = 0.01
                            prior.sigma = c(1, 0.01)) # P(sigma > 1) = 0.01

## ----rfindex-------------------------------------------------------------
iset <- inla.spde.make.index('i', n.spde = spde$n.spde,
                             n.group = length(unique(temp$time)))
# i = 2244

## ----apred---------------------------------------------------------------
A <- inla.spde.make.A(mesh = mesh,
                      loc = loc, group = temp$time) 
As <- inla.spde.make.A(mesh = mesh,
                      loc = loc) 

stk = inla.stack(
  data = list(y = catch),
  A = list(A, As, 1),
  effects = list(i = iset, s = spde$n.spde, intercept = rep(1, length(catch))),
  tag = 'est'
)

h.spec <- list(theta = list(prior = 'pccor1', param = c(0, 0.9)))
formulae <- y ~ 0 + intercept + f(i, model = spde, group = i.group, 
                          control.group = list(model = 'rw1', hyper = h.spec)) + f(s, model = spde, control.group = list(model = 'rw1', hyper = h.spec))

# PC prior on the autoreg. param.
prec.prior <- list(prior = 'pc.prec', param = c(1, 0.01))
# Model fitting
res <- inla(formulae,  data = inla.stack.data(stk), 
            control.predictor = list(compute = TRUE,
                                     A = inla.stack.A(stk)), 
            control.family = list(hyper = list(theta = prec.prior)), 
            control.fixed = list(expand.factor.strategy = 'inla'))

