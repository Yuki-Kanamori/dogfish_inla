require(tidyverse)
require(maps)
require(mapdata)
require(ggplot2)

dir1 = "/Users/Yuki/Dropbox/same"

# JS ------------------------------------------------------------
dir_js = "/Users/Yuki/Dropbox/same/JS"
setwd(dir_js)
path = dir_js
files = list.files(path)

js = NULL
for(i in 1:length(files)){
  data = read.csv(paste0(files[i]), fileEncoding = "CP932")
  
  if("Same" %in% colnames(data)){
    data2 = data %>% select(CatchDate, Effort_tow, Same) %>% mutate(sp = "Same") %>% dplyr::rename(kg = Same)
    kg = data2$kg
    kg[is.na(kg)] = 0
    data2$kg = kg
  }
  if("SameEi" %in% colnames(data)){
    data2 = data %>% select(CatchDate, Effort_tow, SameEi) %>% mutate(sp = "SameEi") %>% dplyr::rename(kg = SameEi)
    kg = data2$kg
    kg[is.na(kg)] = 0
    data2$kg = kg
  }
  js = rbind(js, data2)
}
unique(js$CatchDate)
summary(js$CatchDate)
js2 = js %>% mutate(year = as.numeric(str_sub(CatchDate, 1, 4)), month = as.numeric(str_sub(CatchDate, 5, 6)), day = as.numeric(str_sub(CatchDate, 7, 8)))
summary(js2)
unique(js2$year)

setwd(dir1)
write.csv(js2, "js.csv", fileEncoding = "CP932")
