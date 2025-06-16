#'
#'　NAを”NA"に変換する
#' MCAの処理をする際には、NAを明示的に"NA"などに変換しておく必要がある。
#'
#' @param df 変換したいdf
#' @param replace 代替文字列。defaultは"NA"。replace="無回答"としてもよし
#' @export
convNA2text <- function(df,replace="NA"){
  # NA を "NA" に変換する
    df %>%
    mutate(across(
      where(~ is.character(.) || is.factor(.)), # char型またはfactor型に適用
      ~ if (is.factor(.)) {
        fct_explicit_na(., na_level = replace)   # factor列のNAを変換
      } else {
        replace(., is.na(.), replace)            # char列のNAを変換
      }
    )) %>% return()
}
