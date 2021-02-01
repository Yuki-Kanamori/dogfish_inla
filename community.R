require(tidyverse)
require(ggplot2)
require(maps)


dir1 = "/Users/Yuki/Dropbox/migration"


# -------------------------------------------------------------------------
# ikarui ------------------------------------------------------------------
# -------------------------------------------------------------------------

# JS2 ------------------------------------------------------------
dir_js = "/Users/Yuki/Dropbox/same/JS2"
setwd(dir_js)
path = dir_js
files = list.files(path)

##### for reading all species data
# 
# data = read.csv(paste0(files[1]), fileEncoding = "CP932")
# col = colnames(data)
# col = col[34:length(col)]
# js = NULL
# for(j in 1:length(col)){
#   for(i in 1:length(files)){
#     data = read.csv(paste0(files[i]), fileEncoding = "CP932")
#     
#     data2 = data %>% select(CatchDate, Effort_tow, col[j], Latitude, Longitude) %>% mutate(sp = paste0(col[j])) %>% dplyr::rename(kg = paste0(col[j]))
#     kg = data2$kg
#     kg[is.na(kg)] = 0
#     data2$kg = kg
#     
#     js = rbind(js, data2)
#   }
# }
# summary(js)
# 
# js2 = js %>% mutate(year = as.numeric(str_sub(CatchDate, 1, 4)), month = as.numeric(str_sub(CatchDate, 5, 6)), day = as.numeric(str_sub(CatchDate, 7, 8)))
# summary(js2)
# unique(js2$sp)

js = NULL
for(i in 1:length(files)){
  data = read.csv(paste0(files[i]), fileEncoding = "CP932")

  data2 = data %>% select(CatchDate, Effort_tow, Ikarui, Latitude, Longitude) %>% mutate(sp = "Ikarui") %>% dplyr::rename(kg = Ikarui)
  kg = data2$kg
  kg[is.na(kg)] = 0
  data2$kg = kg

  js = rbind(js, data2)
}
summary(js)

js2 = js %>% mutate(year = as.numeric(str_sub(CatchDate, 1, 4)), month = as.numeric(str_sub(CatchDate, 5, 6)), day = as.numeric(str_sub(CatchDate, 7, 8)))
summary(js2)
unique(js2$sp)


# PO ------------------------------------------------------------
dir_po = "/Users/Yuki/Dropbox/same/PO"
setwd(dir_po)
path = dir_po
files = list.files(path)

ika = NULL
surume = NULL
yari = NULL
for(i in 1:length(files)){
  data = read.csv(paste0(files[i]), fileEncoding = "CP932")
  
  if("Ika" %in% colnames(data)){
    data2 = data %>% select(CatchDate, Effort_tow, Ika, Latitude, Longitude) %>% mutate(sp = "Ika") %>% dplyr::rename(kg = Ika)
    kg = data2$kg
    kg[is.na(kg)] = 0
    data2$kg = kg
    
    ika = rbind(ika, data2)
  }
  if("Surumeika" %in% colnames(data)){
    data2 = data %>% select(CatchDate, Effort_tow, Surumeika, Latitude, Longitude) %>% mutate(sp = "Surumeika") %>% dplyr::rename(kg = Surumeika)
    kg = data2$kg
    kg[is.na(kg)] = 0
    data2$kg = kg
    
    surume = rbind(surume, data2)
  }
  if("Yariika" %in% colnames(data)){
    data2 = data %>% select(CatchDate, Effort_tow, Yariika, Latitude, Longitude) %>% mutate(sp = "Yariika") %>% dplyr::rename(kg = Yariika)
    kg = data2$kg
    kg[is.na(kg)] = 0
    data2$kg = kg
    
    yari = rbind(yari, data2)
  }
}

summary(ika)
summary(surume)
summary(yari)

po = rbind(ika, surume)
po = rbind(po, yari)

po2 = po %>% mutate(year = as.numeric(str_sub(CatchDate, 1, 4)), month = as.numeric(str_sub(CatchDate, 5, 6)), day = as.numeric(str_sub(CatchDate, 7, 8)))
summary(po2)
unique(po2$year)
unique(po2$sp)

ikarui = rbind(js2, po2) %>% dplyr::rename(lon = Longitude, lat = Latitude)
summary(ikarui)

setwd(dir1)
write.csv(ikarui, "ikarui.csv")


# 北海道も推定してみる
# same = same %>% filter(lat < 42) #うまいこと北海道のデータは除去できる

summary(ikarui %>% filter(kg > 0))

map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
region2 = subset(world_map, world_map$region == region)
local_map = map + geom_polygon(data = region2, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + coord_map(xlim = c(min(ikarui$lon)-0.2, max(ikarui$lon)+0.2), ylim = c(min(ikarui$lat)-0.2, max(ikarui$lat)+0.2))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.5)),
           axis.text.y = element_text(size = rel(1.5)),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = 13))
p = geom_point(data = ikarui %>% filter(kg > 2000), aes(x = lon, y = lat, colour = kg), shape = 16, size = 1)
# p = geom_point(data = same %>% filter(kg > 10000), aes(x = lon, y = lat, colour = kg), shape = 16, size = 1)
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
labs = labs(x = "Longitude", y = "Latitude", colour = "kg")
# f = facet_wrap(~ year, ncol = 8)

fig = local_map+theme_bw()+th+p+c+labs
ggsave(filename = "map_ikarui_500.pdf", plot = fig, units = "in", width = 11.69, height = 8.27)







# -------------------------------------------------------------------------
# madara ------------------------------------------------------------------
# -------------------------------------------------------------------------

# JS2 ------------------------------------------------------------
dir_js = "/Users/Yuki/Dropbox/same/JS2"
setwd(dir_js)
path = dir_js
files = list.files(path)


js = NULL
for(i in 1:length(files)){
  data = read.csv(paste0(files[i]), fileEncoding = "CP932")
  
  data2 = data %>% select(CatchDate, Effort_tow, Madara, Latitude, Longitude) %>% mutate(sp = "Madara") %>% dplyr::rename(kg = Madara)
  kg = data2$kg
  kg[is.na(kg)] = 0
  data2$kg = kg
  
  js = rbind(js, data2)
}
summary(js)

js2 = js %>% mutate(year = as.numeric(str_sub(CatchDate, 1, 4)), month = as.numeric(str_sub(CatchDate, 5, 6)), day = as.numeric(str_sub(CatchDate, 7, 8)))
summary(js2)
unique(js2$sp)


# PO ------------------------------------------------------------
dir_po = "/Users/Yuki/Dropbox/same/PO"
setwd(dir_po)
path = dir_po
files = list.files(path)

po = NULL
for(i in 1:length(files)){
  data = read.csv(paste0(files[i]), fileEncoding = "CP932")
  
  data2 = data %>% select(CatchDate, Effort_tow, Madara, Latitude, Longitude) %>% mutate(sp = "Madara") %>% dplyr::rename(kg = Madara)
  kg = data2$kg
  kg[is.na(kg)] = 0
  data2$kg = kg
  
  po = rbind(po, data2)
}
summary(po)

po2 = po %>% mutate(year = as.numeric(str_sub(CatchDate, 1, 4)), month = as.numeric(str_sub(CatchDate, 5, 6)), day = as.numeric(str_sub(CatchDate, 7, 8)))
summary(po2)
unique(po2$sp)


madara = rbind(js2, po2) %>% dplyr::rename(lon = Longitude, lat = Latitude)
summary(madara)

setwd(dir1)
write.csv(madara, "madara.csv")

summary(madara %>% filter(kg > 0))

map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
region2 = subset(world_map, world_map$region == region)
local_map = map + geom_polygon(data = region2, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + coord_map(xlim = c(min(madara$lon)-0.2, max(madara$lon)+0.2), ylim = c(min(madara$lat)-0.2, max(madara$lat)+0.2))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.5)),
           axis.text.y = element_text(size = rel(1.5)),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = 13))
p = geom_point(data = madara %>% filter(kg > 400), aes(x = lon, y = lat, colour = kg), shape = 16, size = 1)
# p = geom_point(data = same %>% filter(kg > 10000), aes(x = lon, y = lat, colour = kg), shape = 16, size = 1)
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
labs = labs(x = "Longitude", y = "Latitude", colour = "kg")
# f = facet_wrap(~ year, ncol = 8)

fig = local_map+theme_bw()+th+p+c+labs
ggsave(filename = "map_madara.pdf", plot = fig, units = "in", width = 11.69, height = 8.27)


