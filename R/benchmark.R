setwd("~/Desktop/reversegeo")
library(sf)
library(tidyverse)
library(pforeach)

df <- read_csv('./dat1000.csv') %>%

system.time(
  res <- pforeach(row = rows(df), .c = rbind)({
    row %>%
      mutate(place = find_pref_place(pref_polygons, dfsbind, longitude, latitude))
      #separate(place, c("pref", "city", "town", "banchi"), sep = ",")
}))

