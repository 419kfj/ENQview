#’ 層化MAplotのための層化MAtableの構築
#'
#' Input　
#' df  入力データフレーム（全体）df
#' selected_vars 選択したMA変数（Q7#1...）chars
#' layer_val 層化変数名 文字列 char
#'
#' Output
#' df format
#' @export
make_grouped_MA_tbl <- function(df,selected_vars,layer_val,...){
  data_for_plot <- df
  selected_data <- data_for_plot[, selected_vars, drop = FALSE]
  gp_vari <- layer_val # 層化変数
  data_for_plot %>% group_by(!!!rlang::syms(gp_vari)) %>%
    dplyr::summarise(度数=n(),across(all_of(selected_vars),
                                   ~ sum(. == 1,na.rm = TRUE)/n(),
                                   .names="ratio_{col}")) -> MA_group_tbl # ここで、行を選択すればいよい
  return(MA_group_tbl)
}

#----

#' ベイス補正をするためのベータ分布のパラメータをEとVから求める
#' @export
get_Be_pram <- function(E,V){
  alpha <- E * (E*(1-E)/V - 1)
  beta <- (1-E) * (E*(1-E)/V -1)
  return(list(alpha,beta))
}

#Freq　vari2
#Rate　vari1
#' @export
make_bayes_vec　<- function(度数,Freq,...){
  data.frame(度数,Freq) %>% mutate(weight=度数/sum(度数),Rate = Freq/度数) %>%
    mutate(E = sum(Rate * weight), V = sum((Rate - E)^2 * Rate)) %>%
    mutate(alpha=get_Be_pram(E,V)[[1]],beta =get_Be_pram(E,V)[[2]]) %>%
    mutate(pre = alpha/(alpha+beta))　%>%
    mutate(post = (alpha+Freq)/(alpha+beta+度数)) -> res.bayes

  return(res.bayes$post)
}

#' @export
make_grouped_MA_tbl2 <- function(df,selected_vars,layer_val,...){　# bayes補正付きの表作成
  data_for_plot <- df
  selected_data <- data_for_plot[, selected_vars, drop = FALSE]
  gp_vari <- layer_val # 層化変数
  data_for_plot %>% group_by(!!!rlang::syms(gp_vari)) -> .tbl0
  .tbl0　%>%
    dplyr::summarise(度数=n(),weight=1,across(all_of(selected_vars),
                                            ~ sum(. == 1,na.rm = TRUE),
                                            .names="freq_{col}")) %>%
    ungroup %>%
    mutate(weight=度数/sum(度数)) -> .tbl1 # 度数、頻度集計の表
  .tbl1 %>%
    transmute(across(starts_with("freq_"),~ make_bayes_vec(度数,.), .names = "ratio_{.col}")) ->　.tbl2
  .tbl2 %>% rename_with(~ gsub("^ratio_freq_", "ratio_", .), starts_with("ratio_freq_")) -> .tbl2.1
  bind_cols(.tbl1[,1:2],.tbl2.1) -> .tbl
  return(.tbl)
}

#------
#' グラフ描画用function
#'
#' 層化MAplot（Dot plot）
#' @export
LayeredMAplot <- function(MA_group_tbl,selected_vars,layer_val,...){
  MA_group_tbl %>% select(-度数) %>%
    pivot_longer(cols = starts_with("ratio_"),  # ratio_で始まる列 (変数1〜8) をlong形式に変換
                 names_to = "variable",         # 変数名の列を"variable"として格納
                 values_to = "value") -> df_long
  gp_vari <- layer_val
  ggplot(df_long, aes(x = !!as.name(gp_vari), y = value, #color = variable,
                      shape =variable, group = variable)) +
    geom_line(aes(color = variable)) +  # 折れ線グラフ
    geom_point(aes(color = variable),size=4) + # ポイントを追加（必要なら）
    labs(x = gp_vari, y = "割合", shape = "変数",color = "変数") +  # 軸ラベルと凡例の設定
    theme_minimal() +  # 見た目をシンプルに
    scale_color_discrete() +
    #  scale_color_discrete(labels = names(df)[74:89]) + # 変数のラベルを設定
    scale_shape_manual(values = 1:length(selected_vars))
}

#----
#
#' ファセット表示
#' @export
facet_layered_MA <- function(MA_group_tbl,selected_vars,layer_val,...){
  MA_group_tbl %>% select(-度数) %>%
    pivot_longer(cols = starts_with("ratio_"),  # ratio_で始まる列 (変数1〜8) をlong形式に変換
                 names_to = "variable",         # 変数名の列を"variable"として格納
                 values_to = "value") -> df_long
  gp_vari <- layer_val
  ggplot(df_long, aes(x = !!as.name(gp_vari), y = value, color = variable, group = variable)) +
    geom_line() + geom_point() +
    facet_wrap(~ variable,ncol=3) +# scales = "free_y") + # 各変数ごとにfacetで分割
    labs(x = "Group", y = "Value") +
    theme_minimal() +
    theme(legend.position = "none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # ラベルを90度回転
}

#' sample script
#' @export
plot_layered_MA <- function(df,selected_vars,layer_val,...){
  make_grouped_MA_tbl(df,selected_vars,layer_val) %>%
  LayeredMAplot(selected_vars,layer_val)
}
