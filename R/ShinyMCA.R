launch_app <- function(df) {
  library(shiny)
  library(FactoMineR)
  library(GDAtools)
  library(ggplot2)
  library(DT)
  
  ui <- fluidPage(
    titlePanel("speMCA 分析アプリ"),
    sidebarLayout(
      sidebarPanel(
        selectInput("variables", "変数を選んでください",
                    choices = names(df),
                    multiple = TRUE,
                    selectize = FALSE,
                    size = 7)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("修正慣性率",
                   tableOutput("eig_table"),
                   plotOutput("eig_plot")
          ),
          tabPanel("変数マップ",
                   plotOutput("var_map")
          ),
          tabPanel("個体マップ",
                   plotOutput("ind_map")
          ),
          tabPanel("データ表示",
                   DTOutput("data_table")
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    # speMCAの結果をreactiveで保持
    mca_result <- reactive({
      req(input$variables)
      if (length(input$variables) < 2) return(NULL)
      df_sub <- df[, input$variables, drop = FALSE]
      tryCatch({
        speMCA(df_sub)
      }, error = function(e) {
        NULL
      })
    })
    
    # 修正慣性率の表
    output$eig_table <- renderTable({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      data.frame(
        軸 = seq_along(res$eig$mrate),
        修正慣性率 = res$eig$mrate,
        累積 = res$eig$cum.mrate
      )
    })
    
    # 修正慣性率のプロット
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
        labs(x = "次元", y = "修正慣性率", title = "修正慣性率と累積") +
        theme_minimal()
    })
    
    # 変数マップ（ggcloud_variable）
    output$var_map <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_variables(res)
    })
    
    # 個体マップ（ggcloud_indv）
    output$ind_map <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_indiv(res)
    })
    
    # 元データ表示
    output$data_table <- renderDT({
      datatable(df, options = list(pageLength = 10))
    })
  }
  
  shinyApp(ui, server)
}
