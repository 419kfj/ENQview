# ENQview

## 履歴
2024/12/22　一応、うごくようになった。
2024/12/19　ENQviewをpackage化することにした。

## install方法

devtools::install_github("419kfj/ENQview", upgrade="never")

## 呼び出し手順

- インストールのあと

library(ENQview)

# データを使えるようにする。

load("ENQview_data/Bunka2")
ENQview(Bunka2)
