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

#-------------------------------------------------------------------------------
# データ読み込みと前処理整形

# Data load from CYDER_ENQ4/data/　← 別のディレクトリからもってくる
load("../CYDER_ENQ4/data/CYDER2020A.rda")
CYDER2020A %>% mutate(イン発生=recode_factor(イン発生,"はい（アンケート１１－２にもお答えください）"="はい"),
                      訓練演習=recode_factor(訓練演習,"定期的な訓練を行っている（アンケート１２−２にもお答えください）"="定期的に行っている",
                                         "訓練を行ったことがある（アンケート１２−２にもお答えください）"="行ったことがある",
                                         "今後、訓練を行う予定である（アンケート１２−２にもお答えください）"="今後行う予定",
                                         "その他（アンケート１２−２にもお答えください）"="その他"),
                      職業=recode_factor(職業,"市長村公務員"="市町村公務員")) -> CYDER2020A

load("../CYDER_ENQ4/data/CYDER2020B1.rda")
load("../CYDER_ENQ4/data/CYDER2020B2.rda")

load("../CYDER_ENQ4/data/CYDER2021A.rda")
CYDER2021A %>% mutate(イン発生=recode_factor(イン発生,"はい（アンケート１１−２にもお答えください）"="はい"),
                      訓練演習=recode_factor(訓練演習,"定期的な訓練を行っている（アンケート１２−２にもお答えください）"="定期的に行っている",
                                         "訓練を行ったことがある（アンケート１２−２にもお答えください）"="行ったことがある",
                                         "今後、訓練を行う予定である（アンケート１２−２にもお答えください）"="今後行う予定",
                                         "その他（アンケート１２−２にもお答えください）"="その他")) -> CYDER2021A
load("../CYDER_ENQ4/data/CYDER2021B1.rda")
load("../CYDER_ENQ4/data/CYDER2021B2.rda")

load("../CYDER_ENQ4/data/CYDER2019A.rda")
load("../CYDER_ENQ4/data/CYDER2019B1.rda")
load("../CYDER_ENQ4/data/CYDER2019B2.rda")

#-------------------------------------------------------------------------------
# Define UI for application 
#
ui <- navbarPage("CYDER受講者アンケート分析",
               tabPanel("About",
                        h1("CYDER受講者の受講後アンケートの分析ツール"),
                        h2("アプリケーション概要"),
                        p("基本集計、調査票、関連リンク、を掲載"),
                        helpText("構成上の要望うけつけてます。"),
                        p("ver1.6 2024/01/30 gitでversion管理を開始"),
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
                 h1("受講者アンケートの基本集計を行います。"),
                 sidebarPanel(
                   selectInput(
                     "selected_data_for_plot",
                     label = h3("データセットを選択してください。"),
                     choices = c(
                       "CYDER2019A" = "CYDER2019A",
                       "CYDER2019B1" = "CYDER2019B1",
                       "CYDER2019B2" = "CYDER2019B2",
                       "CYDER2020A" = "CYDER2020A",
                       "CYDER2020B1" = "CYDER2020B1",
                       "CYDER2020B2" = "CYDER2020B2",
                       "CYDER2021A" = "CYDER2021A",
                       "CYDER2021B1" = "CYDER2021B1",
                       "CYDER2021B2" = "CYDER2021B2"
                     ),
                     selected = "CYDER2020A"
                   ),
                   selectInput("select_input_data_for_hist",
                               "集計する変数",
                               choices = colnames(CYDER2020A)),
                   selectInput("select_input_data_for_cross",
                               "クロス集計する変数",
                               choices = NULL),
                 ),
                 
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
                               tabPanel("pairs",
                                        h2("GGally::pairs"),
                                        plotOutput("pairs",width = 600, height = 600)
                               ),
                               tabPanel("データ一覧",
                                        h2("データ一覧"),
                                        DT::dataTableOutput("table_for_plot")
                               ),
                               tabPanel("自由記述文分析")
                   ))
               ),
               #--------------------------------
               tabPanel("調査票",
                        h3("参考資料 PDFはブラウザの設定viewerで開きます"),
                        tags$iframe(style="height:800px; width:100%; scrolling=yes", #400px
                                    #src="./CYDER_ENQ3/data/CYDER2020_ENQ_20200806.pdf"
                                    #src="./data/CYDER2020_ENQ_20200806.pdf"
                                    #src="../CYDER_ENQ4/data/CYDER2020_ENQ_20200806.pdf"
                                    src="http://172.27.10.50/~fujimoto/CYDER2020_ENQ_20200806_A.pdf"
                        ),
                        HTML("<ul>"),
                        HTML("<li>"),
                        a(href = "http://172.27.10.50/~fujimoto/CYDER2019_A_アンケート_SCENARIO000029-1_20190528_v02.pdf", "CYDER2019A調査票"),
                        HTML("</li>"),
                        HTML("<li>"),
                        a(href = "http://172.27.10.50/~fujimoto/CYDER2019_B-1_アンケート_SCENARIO000042_20190725_03_v02.pdf", "CYDER2019B1調査票"),
                        HTML("</li>"),
                        HTML("<li>"),
                        a(href = "http://172.27.10.50/~fujimoto/CYDER2019_B-2_アンケート_SCENARIO000062_20191025_02_v02.pdf", "CYDER2019B2調査票"),
                        HTML("</li>"),
                        
                        HTML("<li>"),
                        a(href = "http://172.27.10.50/~fujimoto/CYDER2020_ENQ_20200806_A.pdf", "CYDER2020A調査票"),
                        HTML("</li>"),
                        HTML("<li>"),
                        a(href = "http://172.27.10.50/~fujimoto/CYDER2020_共通_受講後アンケート_20201008_B1.pdf", "CYDER2020B1調査票"),
                        HTML("</li>"),
                        HTML("<li>"),
                        a(href = "http://172.27.10.50/~fujimoto/CYDER2020_共通_受講後アンケート_20201008_B2.pdf", "CYDER2020B2調査票"),
                        HTML("</li>"),
                        
                        HTML("</ul>"),
                        helpText("shinyでPDFをviewできるようにします。")
               ),
               tabPanel("関連リンク集",
                        HTML("<ul>"),
                        HTML("<li>"),
                        a(href = "https://419kfj.sakura.ne.jp/db/nlp2023-fujimoto-ohata/", "NLP2023 パネル発表補足リンク"),
                        HTML("</li>"),
                        HTML("<li>"),
                        a(href = "https://cyder.nict.go.jp/", "`CYDER`とはなにか！"),
                        HTML("</li>"),
                        HTML("</ul>")
               ),
               navbarMenu("NLP2023関係",
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
                          tabPanel("発表内容補足スライド",value = 4,
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
               )# navbarMenu close
)
  
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
                     #"iwate" = iwate_utf8,
                     "SRP2018" = SRP2018,
                     # "CYDER2020A" = cyder, #CYDER2020A_all[,-c(1:9,17,19)] #%>% select(-c(2:9,17,19)),
                     "CYDER2019A" = CYDER2019A[,-c(1:2)],
                     "CYDER2019B1" = CYDER2019B1[,-c(1:2)],
                     "CYDER2019B2" = CYDER2019B2[,-c(1:2)],
                     "CYDER2020A" = CYDER2020A[,-c(1:2)],
                     "CYDER2020B1" = CYDER2020B1[,-c(1:2)],
                     "CYDER2020B2" = CYDER2020B2[,-c(1:2)],
                     "CYDER2021A" = CYDER2021A[,-c(1:2)],
                     "CYDER2021B1" = CYDER2021B1[,-c(1:2)],
                     "CYDER2021B2" = CYDER2021B2[,-c(1:2)],
                     
                     "iwate" = iwate
      )
      updateSelectInput(session, "select_input_data_for_hist", choices = colnames(data))
      updateSelectInput(session, "select_input_data_for_cross", choices = c(" ",colnames(data)))
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

      data_for_plot()[,c(input$select_input_data_for_hist,input$select_input_data_for_cross)] %>% 
        structable() %>% 
        mosaic(#main=str_c(input$select_input_data_for_cross,"-",input$select_input_data_for_hist),
          shade=TRUE,las=2,
          labeling=labeling_values
          #pop=FALSE
        )
       #labeling_cells(text=tab,clip=FALSE)(as.table(.tbl.p))
    })
    # クロス表表示
#    output$crosstable <- renderPrint({
#      .tbl <- table(data_for_plot()[,input$select_input_data_for_cross],
#                    data_for_plot()[,input$select_input_data_for_hist])
#      .tbl
#    })
    
    # gtsummary でクロス表表示
    output$my_gt_table <-
      render_gt(
        data_for_plot() %>% tbl_cross(row = input$select_input_data_for_cross,
                                      col = input$select_input_data_for_hist,
                                      percent = "row") %>% 
          add_p(test="chisq.test") %>% 
          bold_labels() %>% 
          as_gt()
      )

    output$ENQANS2018 <- DT::renderDataTable({
      EnqAns2018
    })
    
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
