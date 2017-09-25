import geopandas as gpd
from geopandas.geoseries import *


def reverse_geo(shdf, lon, lat):
    """
    逆ジオコーディング
    shdfは、geopandasでshapefileを読み込んだデータフレーム
    lon, latは経度、緯度
    """
    polygon = shdf['geometry']
    point = Point(lon, lat)

    whichtrue = polygon.contains(point)
    whichrow = whichtrue[whichtrue == True].index

    geos = shdf.ix[whichrow]

    if geos.empty == True:
        return(None)

    reslist = [geos['KEN_NAME'].to_string(index=False),
               geos['GST_NAME'].to_string(index=False),
               geos['CSS_NAME'].to_string(index=False),
               geos['MOJI'].to_string(index=False)]

    res = ','.join(reslist)

    return(res)

shdf2 = gpd.read_file('~/Desktop/A002005212015DDSWC12227/h27ka12227.shp')

reverse_geo(shdf2, 139.884678, 35.626065)
