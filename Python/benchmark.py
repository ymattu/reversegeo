import time
import timeit
import geopandas as gpd
from geopandas.geoseries import *
import pandas as pd
import numpy as np
from multiprocessing import Pool
import multiprocessing


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

df = pd.read_csv('/Users/matsumurayuuya/Desktop/reversegeo/data/latlong_sample.csv')
df1000 = df.head(1000)

sh = gpd.read_file('/Users/matsumurayuuya/Desktop/reversegeo/shp/dfsbind.shp')

# 普通に計算
df1000['place'] = df1000.apply(lambda x: reverse_geo(sh, x['longitude'], x['latitude']), axis = 1)

# 並列化
def split_parallel(df, num_split, map_func):
    p = Pool(multiprocessing.cpu_count())
    df_split = np.array_split(df,num_split)
    result = p.map(map_func,df_split)
    p.close()
    return pd.concat(result)

def revgeo_df(df):
    df['place'] = df.apply(lambda x: reverse_geo(sh, x['longitude'], x['latitude']), axis = 1)
    return(df)


df1000_2 = split_parallel(df1000, 100, revgeo_df)
