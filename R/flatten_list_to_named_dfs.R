#' @title 入れ子リストをデータフレーム群に展開
#' @description 入れ子構造の list を flatten して名前付き data.frame に展開します。
#' @details この関数を使うことで、MCA 結果などのリスト構造を Excel の複数シートに保存できます。
#'
#' @examples
#' library(openxlsx)
#' resmca_parts <- resmca[c("eig", "ind", "var", "svd")]
#' sheet_list <- flatten_list_to_named_dfs(resmca_parts)
#' write.xlsx(sheet_list, file = "resmca_with_rownames.xlsx", overwrite = TRUE)
#'
#' @export
flatten_list_to_named_dfs <- function(x, prefix = "") {
  out <- list()

  add_rownames <- function(df) {
    rn <- rownames(df)
    if (!is.null(rn)) {
      df <- data.frame(rowname = rn, df, row.names = NULL, check.names = FALSE)
    }
    return(df)
  }

  if (is.data.frame(x) || is.matrix(x) || is.table(x)) {
    df <- as.data.frame(x)
    out[[prefix]] <- add_rownames(df)

  } else if (is.atomic(x) && !is.null(names(x))) {
    # named vector → data.frame
    out[[prefix]] <- data.frame(name = names(x), value = x)

  } else if (is.atomic(x)) {
    # unnamed vector → 1列のデータフレームに
    out[[prefix]] <- data.frame(value = x)

  } else if (is.list(x)) {
    lens <- sapply(x, length)
    if (length(unique(lens)) == 1 && all(lens > 1) && !is.null(names(x))) {
      try({
        df <- as.data.frame(x)
        out[[prefix]] <- add_rownames(df)
        return(out)
      }, silent = TRUE)
    }
    for (nm in names(x)) {
      new_prefix <- if (prefix == "") nm else paste(prefix, nm, sep = "_")
      out <- c(out, flatten_list_to_named_dfs(x[[nm]], new_prefix))
    }

  } else {
    out[[prefix]] <- data.frame(note = "Unsupported type")
  }

  return(out)
}
