#'
#'　convert 非該当 to NA
#' for ssm2015
#'
#'@param df input data frame includede 非該当
#'
#'@export
conv_Higaito2NA <- function(df){
  df |>
  dplyr::mutate(across(
    everything(),
    ~ {
      x <- as.character(.x)
      x[x == "非該当"] <- NA
      if (is.factor(.x)) {
        factor(x)  # factor列はlevelsも整理されて戻る
      } else {
        x  # characterのまま
      }
    }
  )) %>% return()
}
