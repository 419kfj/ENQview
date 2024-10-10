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
load("./data/dd3.rda")
Bunka <- .dd3 %>% as.data.frame() %>% # MA回答のOn/Offを1/0に変換
  mutate(across(c(10:21, #Q2
  　　　　　　　　43:47,#Q4
                  74:89,#Q7
                  193:204,#Q14
                  258:277,#Q36
                  278:293, #Q37
                  297:303 #Q41
                  ), 
          ~ case_when(
            . == "On"  ~ 1,
            . == "Off" ~ 0,
            TRUE ~ NA_real_
            )
          )
         )

#-------------------------------------------------------------------------------
# Define UI for application 
#
ui <- navbarPage("調査データ簡易集計",
               tabPanel("About",
                        h1("データを分析する"),
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
                 h1("データの基本集計を行います。"),
                 sidebarPanel(
                   selectInput(
                     "selected_data_for_plot",
                     label = h3("データセットを選択してください。"),
                     choices = c("iwate" = "iwate.f",
                                 "Bunka"="Bunka"), selected = "iwate"),
                   selectInput("select_input_data_for_hist",
                               "集計する変数",
                               choices = colnames("selected_data_for_plot"),
                              #  choices = colnames(iwate.f),
                                selected =  colnames(iwate.f)[3]),
                   selectInput("select_input_data_for_cross",
                               "クロス集計する変数",
                               choices = NULL),
                   selectInput("select_input_data_for_layer",
                               "層化する変数",
                               choices = NULL),
                   selectInput("variables", "MA変数の選択:", 
                               choices =  NULL,#colnames(iwate.f),#colnames("selected_data_for_plot"),
                               multiple = TRUE,
                               selectize = FALSE#,
                               #selected =  colnames(iwate.f)[3]
                               ),  # 複数選択を許可
                   #plotOutput("MAplot")
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
                               tabPanel("MA plot(Bar)",
                                        h2("MA変数集計"),
                                        plotOutput("MAplot",width = 600, height = 600)
                               ),
                               
                               tabPanel("MA plot(Dot)",
                                        h2("MA変数集計"),
                                        plotOutput("MAplot_Dot",width = 600, height = 600)
                               ),
                               
                               tabPanel("層化 MA plot",
                                        h2("層化MA変数集計"),
                                        plotOutput("MAplot_lineDot",width = 600, height = 400),
                                        plotOutput("MAplot_lineDotwarp",width = 600, height = 600)
                               ),
                               
                               tabPanel("Grid回答 mosaic表示",
                                        h2("Grid回答mosaic表示"),
                                        plotOutput("GridAnswer_mosaic",width = 600, height = 600)
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
#                tabPanel(
#                 "基本集計２",
#                 #                 h1("分析対象df（.rda）をuploadしてください。"),
#                 h1("MAデータの基本集計を行います。"),
#                 sidebarPanel(
#                   selectInput(
#                     "selected_data_for_plot2",
#                     label = h3("データセットを選択してください。"),
#                     choices = c("iwate" = "iwate.f",
#                                 "Bunka"="Bunka"), selected = "iwate"),
#                   selectInput("select_input_data_for_hist2",
#                               "集計する変数",
#                               choices = colnames("selected_data_for_plot2"),
#                               #  choices = colnames(iwate.f),
#                               selected =  colnames(iwate.f)[3]),
#                   selectInput("select_input_data_for_cross2",
#                               "クロス集計する変数",
#                               choices = NULL),
#                   selectInput("select_input_data_for_layer2",
#                               "層化する変数",
#                               choices = NULL),
#                   selectInput("variables2", "MA変数の選択:",
#                               choices =  NULL,#colnames(iwate.f),#colnames("selected_data_for_plot"),
#                               multiple = TRUE,
#                               selectize = FALSE#,
#                               #selected =  colnames(iwate.f)[3]
#                   ),  # 複数選択を許可
#                   #plotOutput("MAplot")
#                 ),
# #------
# #----　MAIN Panel
# mainPanel(
#   tabsetPanel(type = "tabs",
#               tabPanel("単変数集計",
#                        h2("基本集計"),
#                        plotOutput("barchart"),
#                        DT::dataTableOutput("simple_table"), # server.R も対応させること！
#               ),
#               tabPanel("2変数分析",
#                        h2("クロス集計（gtsummary::tbl_cross）"),
#                        #  verbatimTextOutput("crosstable"),
#                        gt_output(outputId = "my_gt_table"),
#                        plotOutput("crosschart",width = 600, height = 600),
#                        h3("χ2乗検定"),
#                        verbatimTextOutput("chisq_test")
#               ),
#               tabPanel("2変数分析（層化）",
#                        h2("クロス集計（gtsummary::tbl_cross）"),
#                        #  verbatimTextOutput("crosstable"),
#                        gt_output(outputId = "my_gt_table"),
#                        plotOutput("crosschart2",width = 600, height = 600),
#                        h3("χ2乗検定"),
#                        verbatimTextOutput("chisq_test")
#               ),
#               tabPanel("MA plot(Bar)",
#                        h2("MA変数集計（gtsummary::tbl_cross）"),
#                        plotOutput("MAplot",width = 600, height = 600)
#               ),
# 
#               tabPanel("MA plot(Dot)",
#                        h2("MA変数集計（gtsummary::tbl_cross）"),
#                        plotOutput("MAplot_Dot",width = 600, height = 600)
#               ),
#               tabPanel("pairs",
#                        h2("GGally::pairs"),
#                        plotOutput("pairs",width = 600, height = 600)
#               ),
#               tabPanel("データ一覧",
#                        h2("データ一覧"),
#                        DT::dataTableOutput("table_for_plot")
#               ),
#               #                               tabPanel("自由記述文分析")
#   )
# )
#                ),
#-----------------------------------------------------------------------------------------------
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
                     "iwate.f" = iwate.f[,-c(1,2)],
                     "Bunka" = Bunka[,-c(1,2)]
      )
      updateSelectInput(session, "select_input_data_for_hist", choices = colnames(data))
      updateSelectInput(session, "select_input_data_for_cross", choices = c(" ",colnames(data)))
      updateSelectInput(session, "select_input_data_for_layer", choices = c(" ",colnames(data)))
    #  updateSelectInput(session, "valiables", choices = colnames(data))
      updateSelectInput(session, "variables",
                        choices = colnames(data),
                        selected = colnames(data)[1:2]) 
      # updateSelectInput(session, "select_input_data_for_hist2", choices = colnames(data))
      # updateSelectInput(session, "select_input_data_for_cross2", choices = c(" ",colnames(data)))
      # updateSelectInput(session, "select_input_data_for_layer2", choices = c(" ",colnames(data)))
      # #  updateSelectInput(session, "valiables", choices = colnames(data))
      # updateSelectInput(session, "variables2",
      #                   choices = colnames(data),
      #                   selected = colnames(data)[1:2]) 
      # 
      # 
      
      
      
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
    
    # gtsummay でMA表
    output$MA_gt_table <- render_gt(
      data_for_plot() %>% tbl_cross(row = input$select_input_data_for_cross,
                                    col = input$select_input_data_for_hist,
                                    percent = "row") %>% 
        add_p(test="chisq.test") %>% 
        bold_labels() %>% 
        as_gt()
    )

    # MA plot
    output$MAplot <- renderPlot({
      selected_vars <- input$variables  # 選択された変数を取得
      if (length(selected_vars) > 0) {
        # 選択された変数を用いてプロット
        selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
        selected_data %>% dplyr::summarise(across(everything(), ~ mean(. == 1,na.rm =TRUE))) %>%
          pivot_longer(cols = everything(), names_to = "Question", values_to = "Ratio") -> ratio_df
        
        ggplot(ratio_df, aes(x = Question, y = Ratio)) +
          geom_bar(stat = "identity", fill = "skyblue") +
          scale_y_continuous(labels = scales::percent_format()) +
          labs(title = selected_vars,
               x = "質問項目",
               y = "割合（%）") +
          theme_minimal()
 #       matplot(selected_data, type = "l", lty = 1, col = 1:ncol(selected_data))
      }
    })
    
    
    # MAplot Cleverland Dot Plot
    
    output$MAplot_Dot <- renderPlot({
      selected_vars <- input$variables  # 選択された変数を取得
      if (length(selected_vars) > 0) {
        # 選択された変数を用いてプロット
        selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
        selected_data %>% dplyr::summarise(across(everything(), ~ mean(. == 1,na.rm =TRUE))) %>%
          pivot_longer(cols = everything(), names_to = "Question", values_to = "Ratio") -> ratio_df
        
        ratio_df %>% ggplot(aes(x=Ratio, y=reorder(Question,Ratio))) +
          geom_point(size=3) +
          theme_bw() +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_line(colour="grey60",linetype="dashed")) +
          labs(title = selected_vars,
               x = "割合（%）",y = "質問項目")
    　　}
    })
    
    # 層化MA折れ線グラフ
    output$MAplot_lineDot <- renderPlot({
      selected_vars <- input$variables
      if (length(selected_vars) > 0) {
        selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
        gp_vari <- input$select_input_data_for_layer #"最終学歴"
        df %>% group_by(!!!rlang::syms(gp_vari)) %>% 
          dplyr::summarise(度数=n(),across(c(74:89), ~ sum(. == 1,na.rm = TRUE)/n(),.names="ratio_{col}")) -> MA_group_tbl

        MA_group_tbl %>% select(-度数) %>% 
          pivot_longer(cols = starts_with("ratio_"),  # ratio_で始まる列 (変数1〜8) をlong形式に変換
                       names_to = "variable",         # 変数名の列を"variable"として格納
                       values_to = "value") -> df_long
        
        ggplot(df_long, aes(x = !!as.name(gp_vari), y = value, #color = variable, 
                            shape =variable, group = variable)) + 
          geom_line(aes(color = variable)) +  # 折れ線グラフ
          geom_point(aes(color = variable),size=4) + # ポイントを追加（必要なら）
          labs(x = gp_vari, y = "割合", shape = "変数",color = "変数") +  # 軸ラベルと凡例の設定
          theme_minimal() +  # 見た目をシンプルに
          scale_color_discrete() +
          #  scale_color_discrete(labels = names(df)[74:89]) + # 変数のラベルを設定
          scale_shape_manual(values = 1:16) 
        }
    })
    
    # 層化MA warp Faset
    
    output$MAplot_lineDotwarp <- renderPlot({
      selected_vars <- input$variables
      if (length(selected_vars) > 0) {
        selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
        gp_vari <- input$select_input_data_for_layer #"最終学歴"
        df %>% group_by(!!!rlang::syms(gp_vari)) %>% 
          dplyr::summarise(度数=n(),across(c(74:89), ~ sum(. == 1,na.rm = TRUE)/n(),.names="ratio_{col}")) -> MA_group_tbl
        
        MA_group_tbl %>% select(-度数) %>% 
          pivot_longer(cols = starts_with("ratio_"),  # ratio_で始まる列 (変数1〜8) をlong形式に変換
                       names_to = "variable",         # 変数名の列を"variable"として格納
                       values_to = "value") -> df_long
        
        
        ggplot(df_long, aes(x = !!as.name(gp_vari), y = value, color = variable, group = variable)) +
          geom_line() + geom_point() +
          facet_wrap(~ variable,ncol=3) +# scales = "free_y") + # 各変数ごとにfacetで分割
          labs(x = "Group", y = "Value") +
          theme_minimal() + 
          theme(legend.position = "none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # ラベルを90度回転
        
        
        # ggplot(df_long, aes(x = !!as.name(gp_vari), y = value, #color = variable, 
        #                     shape =variable, group = variable)) + 
        #   geom_line(aes(color = variable)) +  # 折れ線グラフ
        #   geom_point(aes(color = variable),size=4) + # ポイントを追加（必要なら）
        #   labs(x = gp_vari, y = "割合", shape = "変数",color = "変数") +  # 軸ラベルと凡例の設定
        #   theme_minimal() +  # 見た目をシンプルに
        #   scale_color_discrete() +
        #   #  scale_color_discrete(labels = names(df)[74:89]) + # 変数のラベルを設定
        #   scale_shape_manual(values = 1:16) 
      }
    })
    
    
    ggplot(df_long, aes(x = !!as.name(gp_vari), y = value, color = variable, group = variable)) +
      geom_line() + geom_point() +
      facet_wrap(~ variable,ncol=3) +# scales = "free_y") + # 各変数ごとにfacetで分割
      labs(x = "Group", y = "Value") +
      theme_minimal() + 
      theme(legend.position = "none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # ラベルを90度回転
    
    
    
    
    
# Grid Mosaic
# 
    
    output$GridAnswer_mosaic <- renderPlot({
      selected_vars <- input$variables 
      count_categories <- function(x) {
        table(factor(x, levels = c("++", "+", "-", "--", "DK", "無回答"), exclude = NULL))
      }
      
      # df の 1 列目から 20 列目の各列ごとにカテゴリを集計
      category_count_tbl <- data_for_plot() %>%
        dplyr::summarise(across(selected_vars, ~ count_categories(.))) 
      
      category_count_tbl %>% as.matrix() -> cat_tbl
      rownames(cat_tbl) <- c("++","+","-","--","DK","無回答")
      cat_tbl
      
      rownames(t(cat_tbl)) -> rnames
      t(cat_tbl) %>% as.tibble() %>% mutate(ID=rnames,IDn=1:20) %>% 
        mutate(Like=`++`+`+`) %>% 
        arrange(desc(Like)) %>% 
        select(IDn) %>% unlist %>% 
        setNames(NULL) -> order_vec
      
      t(cat_tbl)[order_vec,] %>% mosaic(shade = TRUE,rot_labels = c(0, 0),
                                        margins=c(left=9,top=5),just_labels=c(left="right",top="left"))
      
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
