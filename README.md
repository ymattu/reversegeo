逆ジオコーディング
===========================

# これは何
R と Python による逆ジオコーディングについて、特に shapefile を使った方法についてまとめたものです。

# ファイル構成
- benchmark: **実行時間などを含めて解析に必要なコードをまとめたもの。一番のメイン。**
    - benchmark_Python.ipynb: Python 版
    - benchmark_R.ipynb: R 版
- Python: Python に関するスクリプト
    - `benchmark.py`: ベンチマークを取るためのファイル(下書き)
    - `reversegeo.py`: Python による逆ジオコーディングの関数と使用例をまとめたファイル
- R: R に関するスクリプトとデータ
    - `benchmark.R`: ベンチマークを取るためのファイル(下書き)
    - `reversegeo.R`: R による逆ジオコーディングの関数と使用例をまとめたファイル
    - `jp.RData` 市区町村境界のシェイプファイルを読み込んだ sf オブジェクト([こちら](https://www.esrij.com/products/japan-shp/)からダウンロードした)
    - `dfsbind.RData` 首都圏(とその周辺の県)の番地レベルの境界データ(sf オブジェクト)
        -   福島県
        -   茨城県
        -   栃木県
        -   群馬県
        -   埼玉県
        -   千葉県
        -   東京都
        -   神奈川県
        -   山梨県
        -   長野県
        -   静岡県
- `dat1000.csv`: 1000 行のサンプルデータ
- `shape.R`: ダウンロードしたシェイプファイルを加工するためのスクリプト

## GitHub にはないファイル
容量の関係で Github には上げられないけど必要、もしくは場合に応じて必要なファイル

[こちら](https://drive.google.com/drive/folders/0B08TCnGdQZDWU2RoR0VBTW8yRHM?usp=sharing)からダウンロードする

- `R/zenkoku.RData`: 全国の番地レベルのシェイプファイル(`zenkoku_shp/zenkoku.shp`)を読み込んだもの(sf オブジェクト)
- shpdir: 全国市区町村の番地レベルシェイプファイルおよび関連ファイルお収めたフォルダ
- shpzip: 全国市区町村の番地レベルシェイプファイルをダウンロードした時の zip ファイルを収めたフォルダ。`unzip_all.py`で解凍する。
- shutoken_shp: 首都圏とその周辺の県(上述)の番地レベルのシェイプファイルおよび関連ファイルを収めたフォルダ
- zenkoku_shp: shpdir の中身をひとまとめにしたシェイプファイルを収めたフォルダ

# 所感
シェイプファイルからの逆ジオコーディングは、全国でやろうとするとかなり時間がかかるので、件数が少ない時は Google Map の API とかを使った方が良さそう。
どうしても件数が多くて API ではできない時は、札束でクラウドを叩くか、ポリゴンの重心を求めて最も近いところを取ってくる、などの方法をとるしかなさそうです。

# Future Work
- ポリゴンの重心を求めてもっとも近いとところを取る、がどのくらい精度が下がるか検証
- Elastic Search でポリゴン包含を判定
- PostgreGIS でポリゴン包含を判定
