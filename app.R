#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# 履歴
# 2024/01/30 gitでの管理を開始

library(shiny)
library(tidyverse)
library(vcd)
library(GGally)
library(DT)
library(gtsummary)
library(gt)
library(showtext)
showtext_auto(TRUE)

#-------------------------------------------------------------------------------
# データ読み込みと前処理整形

load("./data/iwate.f.mac.rda")

#-------------------------------------------------------------------------------
# Define UI for application 
#
ui <- navbarPage("iwate 調査データ簡易集計",
               tabPanel("About",
                        h1("iwateデータを分析する"),
                        h2("アプリケーション概要"),
                        p("基本集計、調査票、関連リンク、を掲載"),
                        helpText("構成上の要望うけつけてます。"),
                        p("ver1.7 2024/10/07 CYDERデータ分析からiwateデータ分析ように修正"),
                        p("ver1.6 2024/01/30 gitでversion管理を開始、NLP2024論文をLINK"),
　　　　　　　　　　　　p("ver1.5 2023/10/23 クロス集計にgtsummary::tbl_crossを適用"),
                        p("ver1.0 2023/06/12 プロトタイプから利用可能なレベルにしてリリース"),
                        HTML("<ul>"),
                        HTML("<li>"),
                        a(href = "https://mastering-shiny.org/", target="_blank", "Mastering Shiny"),
                        HTML("</li>"),
                        HTML("<li>"),
                        a(href = "https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html",target="_blank", "gtsummaryの使い方"),
                        HTML("</li>"),
                        HTML("<li>"),
                        a(href = "https://www.danieldsjoberg.com/gtsummary/articles/shiny.html",target="_blank", "Shinyの中でgtsummaryを使う"),
                        HTML("</li>"),
                        HTML("<ul>")
               ),
               #--------------------------------
               tabPanel(
                 "基本集計",
#                 h1("分析対象df（.rda）をuploadしてください。"),
                 h1("iwateデータの基本集計を行います。"),
                 sidebarPanel(
                   selectInput(
                     "selected_data_for_plot",
                     label = h3("データセットを選択してください。"),
                     choices = c("iwate" = "iwate.f"), selected = "iwate"),
                   selectInput("select_input_data_for_hist",
                               "集計する変数",
                                choices = colnames(iwate.f),
                                selected =  colnames(iwate.f)[3]),
                   selectInput("select_input_data_for_cross",
                               "クロス集計する変数",
                               choices = NULL),
                   selectInput("select_input_data_for_layer",
                               "層化する変数",
                               choices = NULL),
                 ),
                 #----　MAIN Panel
                 mainPanel(
                   tabsetPanel(type = "tabs",
                               tabPanel("単変数集計",
                                        h2("基本集計"),
                                        plotOutput("barchart"),
                                        DT::dataTableOutput("simple_table"), # server.R も対応させること！
                               ),
                               tabPanel("2変数分析",
                                        h2("クロス集計（gtsummary::tbl_cross）"),
                                      #  verbatimTextOutput("crosstable"),
                                        gt_output(outputId = "my_gt_table"),
                                        plotOutput("crosschart",width = 600, height = 600),
                                        h3("χ2乗検定"),
                                        verbatimTextOutput("chisq_test")
                               ),
                               tabPanel("2変数分析（層化）",
                                        h2("クロス集計（gtsummary::tbl_cross）"),
                                        #  verbatimTextOutput("crosstable"),
                                        gt_output(outputId = "my_gt_table"),
                                        plotOutput("crosschart2",width = 600, height = 600),
                                        h3("χ2乗検定"),
                                        verbatimTextOutput("chisq_test")
                               ),
                               tabPanel("pairs",
                                        h2("GGally::pairs"),
                                        plotOutput("pairs",width = 600, height = 600)
                               ),
                               tabPanel("データ一覧",
                                        h2("データ一覧"),
                                        DT::dataTableOutput("table_for_plot")
                               ),
#                               tabPanel("自由記述文分析")
                   )
              　)
               ),
               #--------------------------------
               tabPanel("調査票",
#                         h3("参考資料 PDFはブラウザの設定viewerで開きます"),
# #                       tags$a(href = "http://133.167.73.14/~kazuo/ruda0010-questionnaire.pdf", "PDFを開く", target = "_blank"),
#                         tags$iframe(style="height:800px; width:100%; scrolling=yes", #400px
#                                     #src="./CYDER_ENQ3/data/CYDER2020_ENQ_20200806.pdf"
#                                     #src="./data/CYDER2020_ENQ_20200806.pdf"
#                                     src="http://133.167.73.14/~kazuo/ruda0010-questionnaire.pdf"
#                         ),
                        a(href = "http://133.167.73.14/~kazuo/ruda0010-questionnaire.pdf", "岩手調査調査票",target = "_blank"),
                        helpText("別TabでPDFが開きます")
               ),
               tabPanel("関連リンク集",
                        HTML("<ul>"),
                        HTML("<li>"),
                        a(href = "https://www.anlp.jp/nlp2024/", "NLP2024大会リンク"),
                        HTML("</li>"),
                        HTML("<li>"),
                        a(href = "https://www.anlp.jp/nlp2023/", "NLP2023大会リンク"),
                        HTML("</li>"),
                        HTML("<li>"),
                        　　　a(href = "https://419kfj.sakura.ne.jp/db/nlp2023-fujimoto-ohata/", "NLP2023 パネル発表補足リンク"),
                        HTML("</li>"),
                        HTML("<li>"),
                        　　　a(href = "https://cyder.nict.go.jp/", "`CYDER`とはなにか！"),
                        HTML("</li>"),
                        HTML("</ul>")
               ),
               navbarMenu("言語処理学会関係",
                          tabPanel(title = "NLP2024発表論文", value = 4,
                                   tags$iframe(style="height:800px; width:100%; scrolling=yes", #400px
                                               src="NLP2024_20240112_v1.2.pdf" #PDFファイルは、www フォルダに格納
                                   )
                          ),
                          tabPanel(title = "NLP2023発表論文", value = 4,
                                   tags$iframe(style="height:800px; width:100%; scrolling=yes", #400px
                                               src="https://www.anlp.jp/proceedings/annual_meeting/2023/pdf_dir/Q1-11.pdf"
                                   )
                          ),
                          tabPanel(title = "NLP2023発表パネル", value = 4,
                                   tags$iframe(style="height:800px; width:100%; scrolling=yes", #400px
                                               src="https://419kfj.sakura.ne.jp/db/wp-content/uploads/2023/04/NLP2023_03091057.pdf"
                                   )
                          ),
                          tabPanel("NLP2023発表補足スライド",value = 4,
                                   tags$iframe(style="height:800px; width:100%; scrolling=yes", #400px
                                               src="https://419kfj.sakura.ne.jp/db/wp-content/uploads/2023/03/NLP2023SupGraphv1.2.pdf"
                                   )
                          ),
                          tabPanel("ソースコード",
                                   h2("スミマセン、まだ工事中です。"),
                                   sidebarPanel(),
                                   mainPanel(
                                     img(src="job_kouji_ojigi.png", height = 500, width = 300) # www フォルダに格納
                                   )
                          )
               ),# navbarMenu close
　　　　　　　navbarMenu("日本社会学会関係",
　　　　　　　　　　　　  tabPanel(title = "2024-97回全国大会 11/9−10",  value = 4,
　　　　　　　　　　　　　　　　　 tags$iframe(style = "height:800px; width:100%; scrolling=yes", #400px
              　　　　　　　　　　 src = "https://jss-sociology.org/meeting/20231120post-15476/" 
              　　　　　　　　　　 )),
                          tabPanel(title = "2023-96回全国大会",value = 4,
                                   tags$iframe(style = "height:800px; width:100%; scrolling=yes", #400px
                                   src = "https://jss-sociology.org/meeting_archives/20221226post-13972/"
                                   )),
              
                 )# navbarMenu close
)　# ui navbarPage close
  
#-------------------------------------------------------------------------------
# Define server logic required to draw outlut tables and graphs
#
server <- function(input, output, session) {
    addResourcePath("data", "data")
    output$distPlot_shiny <- renderPlot({
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins_shiny + 1)
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    data_for_plot <- reactive({
      data <- switch(input$selected_data_for_plot,
                     "iris" = iris,
                     "titanic" = data.frame(lapply(data.frame(Titanic), 
                                                   function(i){rep(i, data.frame(Titanic)[, 5])})),
                     "Womenlf" = Womenlf,
                     "iwate.f" = iwate.f[,-c(1,2)]
      )
      updateSelectInput(session, "select_input_data_for_hist", choices = colnames(data))
      updateSelectInput(session, "select_input_data_for_cross", choices = c(" ",colnames(data)))
      updateSelectInput(session, "select_input_data_for_layer", choices = c(" ",colnames(data)))
      return(data)
    })
    # barplot by ggplot2
    output$barchart <- renderPlot({
      # barplot(table(data_for_plot()[,input$select_input_data_for_hist]),las=2,main=input$select_input_data_for_hist)
      data_for_plot() %>% count(!!!rlang::syms(input$select_input_data_for_hist)) %>% rename(V1=1) %>% mutate(rate=100*n/sum(n)) %>% 
        ggplot(aes(x=V1,y=rate)) + geom_col(aes(fill=V1))
    })
    # GGally::ggpairs
    output$pairs <- renderPlot({
      data_for_plot()[,c(input$select_input_data_for_hist,input$select_input_data_for_cross)] %>% 
        ggpairs(mapping = aes(color = !!as.name(input$select_input_data_for_hist))) +
        theme(axis.text.x = element_text(angle=45,hjust = 1)) + 
        ggtitle(input$select_input_data_for_hist) -> p
      p
    })  
    # mosaic plot
    output$crosschart <- renderPlot({
      .tbl <- table(data_for_plot()[,input$select_input_data_for_cross],
                    data_for_plot()[,input$select_input_data_for_hist]) 
      .tbl.p <- round(100 * prop.table(.tbl ,margin = 1),1)
      tab <- ifelse(.tbl.p < 1, NA, .tbl.p) 
      
      data_for_plot()[,c(input$select_input_data_for_hist,
                         input$select_input_data_for_cross)] %>% 
        structable() %>%
        mosaic(shade=TRUE,las=2,
               labeling=labeling_values
        )
    })
    # 層化mosaic　plot  
    output$crosschart2 <- renderPlot({
      .tbl <- table(data_for_plot()[,input$select_input_data_for_cross],
                    data_for_plot()[,input$select_input_data_for_hist]) 
      .tbl.p <- round(100 * prop.table(.tbl ,margin = 1),1)
      tab <- ifelse(.tbl.p < 1, NA, .tbl.p) 

      data_for_plot()[,c(input$select_input_data_for_hist,
                         input$select_input_data_for_cross,
                         input$select_input_data_for_layer)] %>% 
          structable() %>%
          mosaic(condvars = 3, # input$select_input_data_for_layer を指定
                 split_vertical = TRUE,# 分割は垂直
                 shade=TRUE,las=2,
                 labeling=labeling_values
          )
    })

    # gtsummary でクロス表表示
    output$my_gt_table <- render_gt(
        data_for_plot() %>% tbl_cross(row = input$select_input_data_for_cross,
                                      col = input$select_input_data_for_hist,
                                      percent = "row") %>% 
          add_p(test="chisq.test") %>% 
          bold_labels() %>% 
          as_gt()
      )
    output$chisq_test <- renderPrint({
      chisq.test(table(data_for_plot()[,input$select_input_data_for_cross],
                       data_for_plot()[,input$select_input_data_for_hist]))
    })
    # 度数分布表
    output$simple_table <- DT::renderDataTable({ #renderTable({#
      table(data_for_plot()[,input$select_input_data_for_hist]) -> tmp
      round(100*prop.table(tmp),1) -> tmp2
      data.frame(tmp,rate=tmp2)[,c(1,2,4)]
    })   
    
    output$table_for_plot <- DT::renderDataTable({
      data_for_plot()
    })
  }

#-------------------------------------------------------------------------------
# Run the application 
#
shinyApp(ui = ui, server = server)
