library(shiny)
library(FactoMineR)
library(ggplot2)
library(vcd)
library(DT)

#' @export
Shiny_ca <- function(df) {
  ui <- fluidPage(
    titlePanel("対応分析アプリ（CA）"),
    sidebarLayout(
      sidebarPanel(
        selectInput("row_var", "行変数の選択", choices = names(df)),
        selectInput("col_var", "列変数の選択", choices = names(df)),
        actionButton("run_analysis", "分析を実行")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("クロス表", DTOutput("cross_table")),
          tabPanel("カイ二乗検定", verbatimTextOutput("chi_result")),
          tabPanel("モザイクプロット", plotOutput("mosaic_plot")),
          tabPanel("CAマップ",
                   plotOutput("ca_map_row"),
                   plotOutput("ca_map_col")),
          tabPanel("データ確認", DTOutput("original_data"))
        )
      )
    )
  )

  server <- function(input, output, session) {
    # クロス表をイベントで作成
    cross_tab <- eventReactive(input$run_analysis, {
      req(input$row_var, input$col_var)
      df_filtered <- df[!is.na(df[[input$row_var]]) & !is.na(df[[input$col_var]]), ]
      table(df_filtered[[input$row_var]], df_filtered[[input$col_var]])
    })

    # カイ二乗検定
    chi_result <- eventReactive(input$run_analysis, {
      chisq.test(cross_tab())
    })

    # CA実行
    ca_result <- eventReactive(input$run_analysis, {
      FactoMineR::CA(cross_tab(), graph = FALSE)
    })

    # 出力表示
    output$cross_table <- renderDT({
      req(cross_tab())
      datatable(as.data.frame.matrix(cross_tab()), options = list(pageLength = 10))
    })

    output$chi_result <- renderPrint({
      req(chi_result())
      chi_result()
    })

    output$mosaic_plot <- renderPlot({
      req(input$row_var, input$col_var)

      tbl <- table(df()[[input$row_var]], df()[[input$col_var]])

      vcd::mosaic(tbl, shade = TRUE, legend = TRUE)
    })

    # output$mosaic_plot <- renderPlot({
    #   req(cross_tab())
    #   mosaic(cross_tab(), shade = TRUE, legend = TRUE)
    # })

    output$ca_map_row <- renderPlot({
      req(ca_result())
      plot(ca_result(), invisible = "col", title = "行カテゴリのCAマップ")
    })

    output$ca_map_col <- renderPlot({
      req(ca_result())
      plot(ca_result(), invisible = "row", title = "列カテゴリのCAマップ")
    })

    output$original_data <- renderDT({
      datatable(df, options = list(pageLength = 5))
    })
  }

  shinyApp(ui, server)
}
