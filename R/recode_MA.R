#'
#' Recode cat of MA question MA回答のカテゴリを1/0に変換する
#' 回答あり -> 1　それ以外 0
#' On　->　1　それ以外 0
#' この回答あり、ONに該当する文字列は、positive_values で選択可能
#'
#' 文化と不平等データの例
#' idx <- c(10:21,43:47,74:93,193:204,216:223,258:303)
#' idx - 2 -> idx2  # offsetがあった
#' Bunka3 %>% recode_MA(idx=idx2,positive_values = "On") -> Bunka4
#' ENQview(Bunka4)
#'
#' @param df input df
#' @param idx MA position vector　
#' @param positive_values MAでONになっているカテゴリ "On"、"回答あり"　など
#' @export
recode_MA <- function(df,idx, positive_values="回答あり"){
  df %>%
    mutate(across(
      all_of(idx),
      ~ ifelse(.x %in% positive_values, 1, 0)
    )) %>% return()
}
