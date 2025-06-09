#' speMCA用の変数選択app
#' dfを指定して、appを起動
#' Active変数を選択
#' juckカテゴリを選択
#' speMCAを実行
#' 　これによって、修正慣性率、変数マップ、個体マップを表示できるようになる。
#' 　speMCAが生成したresultは、ダウンロードすることが可能なので、これをexplorに渡す、もしくは、
#' 　そのあとの追加変数処理に渡すことが可能
#' @export

launch_app <- function(df) {
  library(shiny)
  library(FactoMineR)
  library(GDAtools)
  library(ggplot2)
#  library(plotly)
  library(DT)

  ui <- fluidPage(
    titlePanel("speMCA 分析アプリ（junkカテゴリ除外対応）"),
    sidebarLayout(
      sidebarPanel(
        selectInput("variables", "Active変数を選んでください",
                    choices = names(df),
                    multiple = TRUE,
                    selectize = FALSE,
                    size = 7),
        uiOutput("junk_selector"),  # 動的UI：junkカテゴリ選択
        downloadButton("download_mca", "speMCA結果をダウンロード")  # ← ダウンロードボタン
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("選択情報", uiOutput("selected_info")),  # ←追加
          tabPanel("修正慣性率",
                   tableOutput("eig_table"),
                   plotOutput("eig_plot")
          ),

#          tabPanel("変数マップ", plotlyOutput("var_map")),
#          tabPanel("個体マップ", plotlyOutput("ind_map")),
          tabPanel("変数マップ", plotOutput("var_map")),
          tabPanel("個体マップ", plotOutput("ind_map")),
          tabPanel("データ表示",DTOutput("data_table"))
        )
      )
    )
  )

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


    output$junk_selector <- renderUI({
      jc <- junk_cat()
      if (is.null(jc)) return(NULL)

      tagList(
        selectInput(
          inputId = "excluded_cats",
          label = "除外するカテゴリを選択してください",
          choices = jc,
          multiple = TRUE,
          selectize = FALSE,
          size = min(10, length(jc))
        ),
        actionButton("run_mca", "speMCAを実行する")
      )
    })


    # MCA 実行（除外カテゴリ反映）

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

    # output$var_map <- renderPlotly({
    #   res <- mca_result()
    #   if (is.null(res)) return(NULL)
    #
    #   p <- ggcloud_variables(res) +
    #     coord_fixed(ratio = 1) +
    #     theme(aspect.ratio = 1)  # 縦横比固定
    #
    #   ggplotly(p) %>%
    #     layout(
    #       autosize = TRUE,
    #       yaxis = list(scaleanchor = "x", scaleratio = 1),  # ← aspect比 1:1 を保証
    #       margin = list(t = 50, b = 50, l = 50, r = 50)
    #     )
    # })
    #
    # output$ind_map <- renderPlotly({
    #   res <- mca_result()
    #   if (is.null(res)) return(NULL)
    #
    #   p <- ggcloud_indiv(res) +
    #     coord_fixed(ratio = 1) +
    #     theme(aspect.ratio = 1)
    #
    #   ggplotly(p) %>%
    #     layout(
    #       autosize = TRUE,
    #       yaxis = list(scaleanchor = "x", scaleratio = 1),
    #       margin = list(t = 50, b = 50, l = 50, r = 50)
    #     )
    # })

    output$var_map <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_variables(res) + theme(aspect.ratio=1)#coord_fixed(ratio=1)
    })

    output$ind_map <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_indiv(res) + theme(aspect.ratio=1)#coord_fixed(ratio=1)
    })

    output$data_table <- renderDT({
      datatable(df, options = list(pageLength = 10))
    })

    output$selected_info <- renderUI({
      req(input$variables)  # active変数がある前提
      vars <- input$variables
      junk <- input$excluded_cats

      tagList(
        h4("選択された Active 変数"),
        if (length(vars) > 0) {
          HTML(paste("<ul>", paste(paste0("<li>", vars, "</li>"), collapse = ""), "</ul>"))
        } else {
          em("なし")
        },
        h4("除外された Junk カテゴリ"),
        if (!is.null(junk) && length(junk) > 0) {
          HTML(paste("<ul>", paste(paste0("<li>", junk, "</li>"), collapse = ""), "</ul>"))
        } else {
          em("なし")
        }
      )
    })




  }

  shinyApp(ui, server)
}
