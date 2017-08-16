逆ジオコーディング
================

準備
----

### ディレクトリの設定、桁数の設定

``` r
setwd("~/Desktop/reversegeo")
options(digits = 15)
```

### パッケージのインストール

``` r
# パッケージが入っている場合は飛ばして良い
install.packages(c("tidyverse", "sf", "devtools", "githubinstall", "knitr"))
githubinstall::githubinstall("pforeeach")
```

### パッケージの読み込み

``` r
library(tidyverse) # データ処理とか色々
library(sf) # 地理情報データのため
library(pforeach) # マルチコア処理のため
library(knitr) # きれいな出力のkable()関数のため
```

関数を定義
----------

### 都道府県の判定

``` r
#' @param polygon list
#' @param lon longitude
#' @param lat latitude
find_pref2 <- function(polygon, lon, lat){
  point <- st_point(c(lon, lat))
  for(p in pref){
    if(length(st_contains(polygon[[p]], point)[[1]])){
      return(p)
    }
  }
  return(NA)
}
```

### 住所(何丁目レベル)判定

``` r
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
```

### 県と市区町村、番地を検索

``` r
#' @param polygon polygon list
#' @param sp 番地レベルshapefile
#' @param longitude 経度
#' @param latitude 緯度
find_pref_place <- function(polygon, sp2, longitude, latitude) {
  pref <- find_pref2(polygon, longitude, latitude)
  if(is.na(pref) == TRUE) {
    return(NA)
  }
  city_banchi <- find_place(sp2, pref, longitude, latitude)
  return(paste(pref, city_banchi, sep = ","))
}
```

　シェープファイルなど必要なデータセットづくり
----------------------------------------------

### 都道府県検索のためのデータ

全国47都道府県の検索が可能

``` r
# 都道府県のshapefile読み込み
load("jp.RData")
japan_p <- select(jp, KEN)
pref <- unique(jp$KEN)
# 都道府県ごとにポリゴンをまとめる
pref_polygons <- tapply(japan_p$geometry,
                        japan_p$KEN, # 都道府県名でグループ化
                        st_combine)
```

### 市区町村レベルまで探すためのデータ

検索可能なのは以下の11都府県(全国に拡充予定) - 福島県 - 茨城県 - 栃木県 - 群馬県 - 埼玉県 - 千葉県 - 東京都 - 神奈川県 - 山梨県 - 長野県 - 静岡県'

``` r
# 市区町村レベルのshapefileの読み込み
load("dfsbind.rdata")
```

逆ジオコーディング
------------------

### 単純な1地点(例: よこはま動物園ズーラシア)

よこはま動物園ズーラシアの住所は **神奈川県横浜市旭区上白根町1175−1**

``` r
find_pref_place(pref_polygons, dfsbind, 139.526510, 35.494865)
#> [1] "神奈川県,横浜市,旭区,上白根町"

# 時間を計測
system.time(find_pref_place(pref_polygons, dfsbind, 139.526510, 35.494865))
#>              user            system           elapsed 
#> 0.520000000000000 0.013000000000000 0.574000000000001
```

1件0.5秒ほど

### データフレームの場合

#### サンプルデータの作成

``` r
dat <- data_frame(id = c(1:4),
                  long = c(139.884678, 139.742946, 135.728965, 139.526510),
                  lat = c(35.626065, 35.649114, 35.039200, 35.494865),
                  location = c("東京ディズニーシー", "慶應義塾大学",　"金閣寺", "よこはま動物園ズーラシア"))
kable(dat)
```

|   id|        long|        lat| location                 |
|----:|-----------:|----------:|:-------------------------|
|    1|  139.884678|  35.626065| 東京ディズニーシー       |
|    2|  139.742946|  35.649114| 慶應義塾大学             |
|    3|  135.728965|  35.039200| 金閣寺                   |
|    4|  139.526510|  35.494865| よこはま動物園ズーラシア |

#### 逆ジオコーディング

一件にそれなりの時間がかかるので、データフレームで逆ジオコーディングをするとき、特に行数が多いときはマルチコア処理をしたほうがよい。

``` r
 res <- pforeach(row = rows(dat), .c = rbind)({
   row %>%
     mutate(place = find_pref_place(pref_polygons, dfsbind, long, lat)) %>%
     separate(place, c("pref", "city", "town", "banchi"), sep = ",")
 })
res[res == ""] <- NA
res[res == "NA"] <- NA
kable(res)
```

|   id|        long|        lat| location                 | pref     | city   | town | banchi     |
|----:|-----------:|----------:|:-------------------------|:---------|:-------|:-----|:-----------|
|    1|  139.884678|  35.626065| 東京ディズニーシー       | 千葉県   | 浦安市 | NA   | 舞浜       |
|    2|  139.742946|  35.649114| 慶應義塾大学             | 東京都   | 港区   | NA   | 三田２丁目 |
|    3|  135.728965|  35.039200| 金閣寺                   | 京都府   | NA     | NA   | NA         |
|    4|  139.526510|  35.494865| よこはま動物園ズーラシア | 神奈川県 | 横浜市 | 旭区 | 上白根町   |

都道府県以上のレベルでは上記11都府県以外は現状できないので NA になる。

都道府県だけでいいなら下記。

``` r
res <- pforeach(row = rows(dat), .c = rbind)({
   row %>%
     mutate(pref = find_pref(pref_polygons, long, lat))
 })
```

#### 速度は？

1000行で4分弱。都道府県だけなら1分ほど。

検証環境
--------

3.1GHz Core i7, メモリ16ギガ, 2コア

``` r
devtools::session_info()
#>  setting  value                       
#>  version  R version 3.4.1 (2017-06-30)
#>  system   x86_64, darwin15.6.0        
#>  ui       X11                         
#>  language (EN)                        
#>  collate  ja_JP.UTF-8                 
#>  tz       Asia/Tokyo                  
#>  date     2017-08-17                  
#> 
#>  package    * version date       source                          
#>  assertthat * 0.2.0   2017-04-11 CRAN (R 3.4.0)                  
#>  backports  * 1.1.0   2017-05-22 CRAN (R 3.4.0)                  
#>  base       * 3.4.1   2017-07-07 local                           
#>  bindr      * 0.1     2016-11-13 cran (@0.1)                     
#>  bindrcpp   * 0.2     2017-06-17 cran (@0.2)                     
#>  broom      * 0.4.2   2017-02-13 CRAN (R 3.4.0)                  
#>  cellranger * 1.1.0   2016-07-27 CRAN (R 3.4.0)                  
#>  codetools  * 0.2-15  2016-10-05 CRAN (R 3.4.1)                  
#>  colorspace * 1.3-2   2016-12-14 CRAN (R 3.4.0)                  
#>  compiler   * 3.4.1   2017-07-07 local                           
#>  datasets   * 3.4.1   2017-07-07 local                           
#>  DBI        * 0.7     2017-06-18 cran (@0.7)                     
#>  devtools     1.13.2  2017-06-02 CRAN (R 3.4.0)                  
#>  digest     * 0.6.12  2017-01-27 CRAN (R 3.4.0)                  
#>  doParallel * 1.0.10  2015-10-14 CRAN (R 3.4.0)                  
#>  doRNG      * 1.6.6   2017-04-10 cran (@1.6.6)                   
#>  dplyr      * 0.7.2   2017-07-20 CRAN (R 3.4.1)                  
#>  evaluate   * 0.10.1  2017-06-24 CRAN (R 3.4.1)                  
#>  forcats    * 0.2.0   2017-01-23 CRAN (R 3.4.0)                  
#>  foreach    * 1.4.3   2015-10-13 cran (@1.4.3)                   
#>  foreign    * 0.8-69  2017-06-22 CRAN (R 3.4.1)                  
#>  ggplot2    * 2.2.1   2016-12-30 CRAN (R 3.4.0)                  
#>  glue       * 1.1.1   2017-06-21 cran (@1.1.1)                   
#>  graphics   * 3.4.1   2017-07-07 local                           
#>  grDevices  * 3.4.1   2017-07-07 local                           
#>  grid       * 3.4.1   2017-07-07 local                           
#>  gtable     * 0.2.0   2016-02-26 CRAN (R 3.4.0)                  
#>  haven      * 1.1.0   2017-07-09 CRAN (R 3.4.1)                  
#>  highr      * 0.6     2016-05-09 CRAN (R 3.4.0)                  
#>  hms        * 0.3     2016-11-22 CRAN (R 3.4.0)                  
#>  htmltools  * 0.3.6   2017-04-28 cran (@0.3.6)                   
#>  httr       * 1.2.1   2016-07-03 CRAN (R 3.4.0)                  
#>  iterators  * 1.0.8   2015-10-13 cran (@1.0.8)                   
#>  jsonlite   * 1.5     2017-06-01 cran (@1.5)                     
#>  knitr      * 1.16    2017-05-18 CRAN (R 3.4.0)                  
#>  lattice    * 0.20-35 2017-03-25 CRAN (R 3.4.1)                  
#>  lazyeval   * 0.2.0   2016-06-12 CRAN (R 3.4.0)                  
#>  lubridate  * 1.6.0   2016-09-13 CRAN (R 3.4.0)                  
#>  magrittr   * 1.5     2014-11-22 CRAN (R 3.4.0)                  
#>  memoise      1.1.0   2017-04-21 CRAN (R 3.4.0)                  
#>  methods    * 3.4.1   2017-07-07 local                           
#>  mnormt     * 1.5-5   2016-10-15 CRAN (R 3.4.0)                  
#>  modelr     * 0.1.0   2016-08-31 CRAN (R 3.4.0)                  
#>  munsell    * 0.4.3   2016-02-13 CRAN (R 3.4.0)                  
#>  nlme       * 3.1-131 2017-02-06 CRAN (R 3.4.1)                  
#>  parallel   * 3.4.1   2017-07-07 local                           
#>  pforeach   * 1.3     2017-08-05 Github (hoxo-m/pforeach@2c44f3b)
#>  pkgconfig  * 2.0.1   2017-03-21 cran (@2.0.1)                   
#>  pkgmaker   * 0.22    2014-05-14 CRAN (R 3.4.0)                  
#>  plyr       * 1.8.4   2016-06-08 CRAN (R 3.4.0)                  
#>  psych      * 1.7.5   2017-05-03 CRAN (R 3.4.1)                  
#>  purrr      * 0.2.2.2 2017-05-11 CRAN (R 3.4.0)                  
#>  R6         * 2.2.2   2017-06-17 cran (@2.2.2)                   
#>  Rcpp       * 0.12.12 2017-07-15 cran (@0.12.12)                 
#>  readr      * 1.1.1   2017-05-16 cran (@1.1.1)                   
#>  readxl     * 1.0.0   2017-04-18 CRAN (R 3.4.0)                  
#>  registry   * 0.3     2015-07-08 CRAN (R 3.4.0)                  
#>  reshape2   * 1.4.2   2016-10-22 CRAN (R 3.4.0)                  
#>  rlang      * 0.1.1   2017-05-18 cran (@0.1.1)                   
#>  rmarkdown  * 1.6     2017-06-15 CRAN (R 3.4.0)                  
#>  rngtools   * 1.2.4   2014-03-06 CRAN (R 3.4.0)                  
#>  rprojroot  * 1.2     2017-01-16 CRAN (R 3.4.0)                  
#>  rvest      * 0.3.2   2016-06-17 CRAN (R 3.4.0)                  
#>  scales     * 0.4.1   2016-11-09 CRAN (R 3.4.0)                  
#>  sf         * 0.5-2   2017-07-12 CRAN (R 3.4.1)                  
#>  stats      * 3.4.1   2017-07-07 local                           
#>  stringi    * 1.1.5   2017-04-07 CRAN (R 3.4.0)                  
#>  stringr    * 1.2.0   2017-02-18 CRAN (R 3.4.0)                  
#>  tibble     * 1.3.3   2017-05-28 cran (@1.3.3)                   
#>  tidyr      * 0.6.3   2017-05-15 CRAN (R 3.4.0)                  
#>  tidyverse  * 1.1.1   2017-01-27 CRAN (R 3.4.0)                  
#>  tools      * 3.4.1   2017-07-07 local                           
#>  udunits2   * 0.13    2016-11-17 CRAN (R 3.4.0)                  
#>  units      * 0.4-5   2017-06-15 cran (@0.4-5)                   
#>  utils      * 3.4.1   2017-07-07 local                           
#>  withr        1.0.2   2016-06-20 CRAN (R 3.4.0)                  
#>  xml2       * 1.1.1   2017-01-24 CRAN (R 3.4.0)                  
#>  xtable     * 1.8-2   2016-02-05 CRAN (R 3.4.0)                  
#>  yaml       * 2.1.14  2016-11-12 CRAN (R 3.4.0)
```

### 参考

-   <http://qiita.com/nozma/items/808bce2f496eabd50ff1>
-   <http://y-mattu.hatenablog.com/entry/2017/07/18/020123>
