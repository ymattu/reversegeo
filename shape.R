setwd("~/Desktop/reversegeo")

library(tidyverse)
library(sf)
library(plyr)


shpdir <- "/Volumes/Transcend/reversegeo/shpdir"
shapes <- list.files(shpdir,
                     pattern = ".shp$",
                     full.names = TRUE)

shplist <- list()

for (i in 1:length(shapes)) {
  shplist[[i]] <- read_sf(shapes[i])
}

load("shplist.RData")


sa <- rbind(shplist[[1]], shplist[[2]])
zenkoku <- rbind.fill(shplist) %>%
  select(1:31) %>%
  st_as_sf()

save(zenkoku, file = "zenkoku.RData")

write_sf(zenkoku, "./zenkoku/shp/zenkoku.shp", layer_options = "ENCODING=UTF-8")
