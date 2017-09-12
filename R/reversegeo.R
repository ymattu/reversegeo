# 準備 #
########
setwd("~/Desktop/reversegeo")
options(digits = 15)

library(tidyverse)
library(sf)
library(pforeach) #githubinstall::githubinstall("pforeach")
library(haven)

############
# 関数定義 #
############
# 関数
#' @param polygon list
#' @param lon longitude
#' @param lat latitude
## 都道府県の判定
find_pref2 <- function(polygon, lon, lat){
  point <- st_point(c(lon, lat))
  for(p in pref){
    if(length(st_contains(polygon[[p]], point)[[1]])){
      return(p)
    }
  }
  return(NA)
}

## 住所(何丁目レベル)判定
#' @param sp_polygon object of class sf, sfc or sfg
#' @param pref prefecture
#' @param lon longitude
#' @param lat latitude
find_place <- function(sp_polygon, pref, lon, lat) {
  
  if(is.na(pref) == TRUE) {
    return(NA)
  }
  
  sp_pref <- sp_polygon %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    filter(KEN_NAME == pref)
  
  which.row <- suppressMessages(sf::st_contains(sp_pref, sf::st_point(c(lon, lat)), sparse = FALSE)) %>%
    grep(TRUE, .)
  
  if (identical(which.row, integer(0)) == TRUE) {
    return(NA)
  } else {
    geos <- sp_pref[which.row, ]
    
    place_name = paste(geos$GST_NAME,
                       geos$CSS_NAME,
                       geos$MOJI,
                       sep = ",") %>%
      stringr::str_replace_all("NA", "")
    return(place_name)
  }
}

## 県と市区町村、番地を検索
##' @param polygon polygon list
##' @param sp 番地レベルshapefile
##' @param longitude 経度
##' @param latitude 緯度
find_pref_place <- function(polygon, sp2, longitude, latitude) {
  pref <- find_pref2(polygon, longitude, latitude)
  if(is.na(pref) == TRUE) {
    return(NA)
  }
  city_banchi <- find_place(sp2, pref, longitude, latitude)
  return(paste(pref, city_banchi, sep = ","))
}



# 都道府県のshapefile読み込み
load("./R/jp.RData")
japan_p <- select(jp, KEN)
pref <- unique(jp$KEN)
# 都道府県ごとにポリゴンをまとめる
pref_polygons <- tapply(japan_p$geometry,
                        japan_p$KEN, # 都道府県名でグループ化
                        st_combine)

# 市区町村レベルのshapefileの読み込み
load("./R/dfsbind.RData")
st_write(dfsbind, "dfsbind.shp", layer_options = "ENCODING=UTF-8")
