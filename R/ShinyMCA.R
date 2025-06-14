#' speMCA用の変数選択app
#'
#' dfを指定して、appを起動
#' Active変数を選択
#' juckカテゴリを選択
#' speMCAを実行
#' これによって、修正慣性率、変数マップ、個体マップを表示できるようになる。
#' speMCAが生成したresultは、ダウンロードすることが可能なので、これをexplorに渡す、もしくは、
#' そのあとの追加変数処理に渡すことが可能
#' 2025/06/13 変数マップなど、マップサイズをブラウザの幅にあわせて拡大/縮小
#' 2025/06/11 追加変数の選択、マップ表示、交互作用plotの機能を追加した
#' @import shiny
#' @import GDAtools
#' @import ggplot2
#' @import dplyr
#' @importFrom DT datatable
#' @export
Shiny_speMCA <- function(df) {
　showtext::showtext_auto(TRUE)
#  get_pkg_version <- function() {
#    as.character(utils::packageVersion("ENQview"))
#  }
  # get_pkg_version <- function() {
  #   desc <- read.dcf("DESCRIPTION")
  #   desc[1, "Version"]
  # }
  ui <- fluidPage(
    titlePanel("speMCA 分析アプリ"),
    tags$p(
      paste0("ENQview Version: ", as.character(utils::packageVersion("ENQview"))),
      style = "color: gray; margin-left: 15px;"
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput("variables", "Active変数を選んでください",
                    choices = names(df),
                    multiple = TRUE,
                    selectize = FALSE,
                    size = 7),
        uiOutput("junk_selector"),  # 動的UI：junkカテゴリ選択
        selectInput("supvars", "追加変数を選んでください",
                    choices = names(df),
                    multiple = TRUE,
                    selectize = FALSE,
                    size = 7),
        selectInput("inter_v1", "交互作用v1を選んでください",
                    choices = names(df),
                    multiple = FALSE),
        selectInput("inter_v2", "交互作用v1を選んでください",
                    choices = names(df),
                    multiple = FALSE),
        downloadButton("download_mca", "speMCA結果をダウンロード")  # ← ダウンロードボタン
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("選択変数", uiOutput("selected_info")),  # ←追加
          tabPanel("修正慣性率",
                   tableOutput("eig_table"),
                   plotOutput("eig_plot")
          ),
          tabPanel("変数マップ", plotOutput("var_map",height = "600px"),
                                 plotOutput("var_map_32",height = "600px"),
                                 plotOutput("var_map_13",height = "600px")),
          tabPanel("個体マップ", plotOutput("ind_map",height = "600px"),
                                 plotOutput("ind_map_32",height = "600px"),
                                 plotOutput("ind_map_13",height = "600px")
                   ),
          tabPanel("データ表示",DTOutput("data_table")),
          tabPanel("supvarsの情報", verbatimTextOutput("supvars_out")),
          tabPanel("変数マップ＋supvars", plotOutput("supvars_map")),
         tabPanel("交互作用plot", plotOutput("interaction_map"))
        )
      )
    )
  )
# ----------------------------------------------------------------------------
#         SERVER part
# ----------------------------------------------------------------------------
  server <- function(input, output, session) {
    # junkカテゴリ取得
    junk_cat <- reactive({
      req(input$variables)
      if (length(input$variables) < 2) return(NULL)
      df_sub <- df[, input$variables, drop = FALSE]
      jc <- getindexcat(df_sub)
      if (is.null(jc) || length(jc) == 0) return(NULL)
      jc
    })

    # supvars結果を計算
    supvars_result <- reactive({
      req(mca_result(), input$supvars)
      tryCatch({
        GDAtools::supvars(resmca = mca_result(), vars = (df %>% select(input$supvars)))
      }, error = function(e) {
        message("supvarsエラー: ", e$message)
        NULL
      })
    })

    ## supvarsの出力を表示（テキスト）
    output$supvars_out <- renderPrint({
      res <- supvars_result()
      if (is.null(res)) return("supvarsの結果がありません")
      print(res)
    })

    ## ggadd_supvarsを使ってマップを表示
    output$supvars_map <- renderPlot({
      req(mca_result(), input$supvars)
      tryCatch({
        base_map <- GDAtools::ggcloud_variables(mca_result(), col = "lightgrey")
        GDAtools::ggadd_supvars(p = base_map,
                               resmca = mca_result(),
                               vars = (df %>% select(input$supvars))) +#df[[input$supvars]]) +
                               theme(aspect.ratio = 1)
      }, error = function(e) {
        message("ggadd_supvars エラー: ", e$message)
        return(NULL)
      })
    },width = "auto", height = 600)

    ## 交互作用Plot
    output$interaction_map <- renderPlot({
      req(input$inter_v1, input$inter_v2)
      tryCatch({
        base_map <- GDAtools::ggcloud_variables(mca_result(), col = "lightgrey")
        GDAtools::ggadd_interaction(p = base_map,
                                resmca = mca_result(),
                                v1 = df[[input$inter_v1]],
                                v2 = df[[input$inter_v2]]) + theme(aspect.ratio = 1)
      }, error = function(e) {
        message("ggadd_interaction エラー: ", e$message)
        return(NULL)
      })
    },width = "auto", height = 600)


    # junk カテゴリ選択 for speMCA
    output$junk_selector <- renderUI({
      jc <- junk_cat()
      if (is.null(jc)) return(NULL)

      tagList(
        selectInput(
          inputId = "excluded_cats",
          label = "juck指定するカテゴリを選択してください",
          choices = jc,
          multiple = TRUE,
          selectize = FALSE,
          size = min(10, length(jc))
        ),
        actionButton("run_mca", "speMCAを実行する")
      )
    })

    # MCA 実行（specificMCA）
    mca_result <- eventReactive(input$run_mca, {
      req(input$variables)
      if (length(input$variables) < 2) return(NULL)
      df_sub <- df[, input$variables, drop = FALSE]
      jc <- junk_cat()
      selected_labels <- input$excluded_cats
      if (is.null(selected_labels) || length(selected_labels) == 0) {
        excl_indices <- NULL
      } else {
        excl_indices <- match(selected_labels, jc)
        excl_indices <- excl_indices[!is.na(excl_indices)]
      }
      tryCatch({
        speMCA(df_sub, excl = excl_indices)
      }, error = function(e) {
        message("MCAエラー: ", e$message)
        NULL
      })
    })

    ## speMCAのresult ダウンローダー
    output$download_mca <- downloadHandler(
      filename = function() {
        paste0("mca_result_", Sys.Date(), ".rds")
      },
      content = function(file) {
        result <- mca_result()
        if (is.null(result)) {
          showNotification("MCA結果がまだありません。", type = "error")
          return(NULL)
        }
        saveRDS(result, file)
      }
    )
    ## 修正寄与率の表示
    output$eig_table <- renderTable({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      data.frame(
        軸 = seq_along(res$eig$mrate),
        修正慣性率 = res$eig$mrate,
        累積慣性率 = res$eig$cum.mrate
      )
    })

    output$eig_plot <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      df_eig <- data.frame(
        dim = seq_along(res$eig$mrate),
        mrate = res$eig$mrate,
        cum_mrate = res$eig$cum.mrate
      )
      ggplot(df_eig, aes(x = dim)) +
        geom_bar(aes(y = mrate), stat = "identity", fill = "skyblue") +
        geom_line(aes(y = cum_mrate), color = "red", size = 1) +
        geom_point(aes(y = cum_mrate), color = "red", size = 2) +
        labs(x = "次元", y = "修正慣性率", title = "修正慣性率と累積慣性率") +
        theme_minimal()
    })

    ## 変数マップの表示
    output$var_map <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_variables(res) + theme(aspect.ratio=1) +
        ggtitle("変数マップ 1−2軸")
    },width="auto",height="auto")#600)

    output$var_map_32 <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_variables(res,axes = c(3,2)) + theme(aspect.ratio=1)  +
        ggtitle("変数マップ 3−2軸")
    },width="auto",height="auto")#600)

    output$var_map_13 <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_variables(res,axes = c(1,3)) + theme(aspect.ratio=1)  +
        ggtitle("変数マップ 1−3軸")
    },width="auto",height="auto")#600)


    ##　個体マップの表示
    output$ind_map <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_indiv(res) + theme(aspect.ratio=1)
    },width = "auto", height = 600)

    output$ind_map_32 <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_indiv(res, axes = c(3,2)) + theme(aspect.ratio=1)
    },width = "auto", height = 600)

    output$ind_map_13 <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_indiv(res, axes = c(1,3)) + theme(aspect.ratio=1)
    },width = "auto", height = 600)


    ## データ表の表示
    output$data_table <- renderDT({
      datatable(df, options = list(pageLength = 10))
    })

    ## Active変数、junkカテゴリのリスト
    output$selected_info <- renderUI({
      req(input$variables)  # active変数がある前提
      vars <- input$variables
      junk <- input$excluded_cats
      tagList(
        h4("Active 変数"),
        if (length(vars) > 0) {
          HTML(paste("<ul>", paste(paste0("<li>", vars, "</li>"), collapse = ""), "</ul>"))
        } else {
          em("なし")
        },
        h4("Junk カテゴリ"),
        if (!is.null(junk) && length(junk) > 0) {
          HTML(paste("<ul>", paste(paste0("<li>", junk, "</li>"), collapse = ""), "</ul>"))
        } else {
          em("なし")
        }
      )
    })
  }
# ----------------------------------------------------------------------------
#      ShinyApp実行！
# ----------------------------------------------------------------------------
  shinyApp(ui, server)
}
