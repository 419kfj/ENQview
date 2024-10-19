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
library(FactoMineR)
library(tidyverse)
library(vcd)
library(GGally)
library(DT)
library(gtsummary)
library(gt)
library(readxl)
library(showtext)
showtext_auto(TRUE)
#library(tidyverse)
#library(shiny)

#-------------------------------------------------------------------------------
# データ読み込みと前処理整形

load("./data/iwate.f.mac.rda")
load("./data/iwate.f.mac2.rda")
load("./data/dd3.rda") # .dd3
load("./data/Bunka2.rda") # Bunka2  変数名を記号＋日本語の構成にした 2024/10/16
Bunka3 <- Bunka2
#load(file="../../RStudio/文化と不平等202409/01.6_Recode/data/d3.rda")
load(file="./data/d3.rda")
.dd <- .d3

# #------- 2024/10/17 追加
# # 記号付きshortnameをつけて、.dd2とする
# 
# #short_vnames
# 
# load("./data/short_vnames.rda")
# conv_tables　<- read_excel("./data/Name_and_Label2.xlsx") # 変数名（Qnn）とラベルの対応表
# 
# conv_tables %>% select(Name,短縮A) %>% transmute(sname2=str_c(Name, 短縮A)) %>%
#    unlist %>% setNames(NULL) -> short_vnames2
# 
# #short_vnames2
# convNamesVec <- setNames(1:314,short_vnames2[-c(1:2)])
#  .dd %>% select(-c(1:2)) %>% rename(!!!convNamesVec) -> .dd3
#-------

#Bunka2 <- Bunka2 %>% as.data.frame() %>% # MA回答のOn/Offを1/0に変換
Bunka <- .dd3 %>% as.data.frame() %>% # MA回答のOn/Offを1/0に変換
#Bunka <- .d3 %>% as.data.frame() %>% # MA回答のOn/Offを1/0に変換
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
  
       ) %>% 
  mutate(across(c(107:139,#Q9
                  120:137,#Q10
                  138:159,#Q11
                  ),
                ~ case_when(
                  . == "好みである" ~ "A",
                  . == "好みでない" ~ "B",
                  . == "見たことがあるが、どちらでもない" ~ "C",
                  . == "タイトルは知っているが、見たことがない" ~ "D",
                  . == "知らない" ~ "E",
                )
        )
  )

# recodeしたあとの.ddは、まだ、変数名に短縮をいれてない。

Bunka3 <- Bunka3 %>% as.data.frame() %>% # MA回答のOn/Offを1/0に変換
  #Bunka2 <- Bunka2 %>% as.data.frame() %>% # MA回答のOn/Offを1/0に変換
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
                    TRUE ~ NA_real_)
                  )
                ) %>% 
  mutate(across(c(107:139,#Q9
                  120:137,#Q10
                  138:159,#Q11
                  ),
                ~ case_when(
                  . == "好みである" ~ "A",
                  . == "好みでない" ~ "B",
                  . == "見たことがあるが、どちらでもない" ~ "C",
                  . == "タイトルは知っているが、見たことがない" ~ "D",
                  . == "知らない" ~ "E",
                  )
                )
         )


Bunka2 <- .dd %>% as.data.frame() %>% # MA回答のOn/Offを1/0に変換
#Bunka2 <- Bunka2 %>% as.data.frame() %>% # MA回答のOn/Offを1/0に変換
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
  
  ) %>% 
  mutate(across(c(107:139,#Q9
                  120:137,#Q10
                  138:159,#Q11
  ),
  ~ case_when(
    . == "好みである" ~ "A",
    . == "好みでない" ~ "B",
    . == "見たことがあるが、どちらでもない" ~ "C",
    . == "タイトルは知っているが、見たことがない" ~ "D",
    . == "知らない" ~ "E",
  )
  )
  )


#-------------------------------------------------------------------------------
# Define UI for application 
#
ui <- navbarPage("調査データ簡易集計",
               tabPanel("About",
                        h1("調査データの基本集計ツール"),
                        h2("アプリケーション概要"),
                        p("基本集計、調査票、関連リンク、を掲載"),
                        helpText("機能上の要望あればメールください。"),
                        p("ver2.1 2024/10/14 変数選択を一元化。Grid集計を汎用化"),
                        p("ver2.0 2024/10/12 「文化と不平等」データを中心に基本集計機能を拡充"),
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
                        HTML("<ul>"),
　　　　　　　　　　　　h3("app.R 最終更新日時:"),
　　　　　　　　　　　　textOutput("timestamp")  # タイムスタンプの表示
               ),
               #--------------------------------
               tabPanel(
                 "基本集計",
#                 h1("分析対象df（.rda）をuploadしてください。"),
                 h1("基本集計を行います。"),
                 sidebarPanel(
                   selectInput("selected_data_for_plot",
                     　　　　　label = h3("集計対象を選択してください。"),
                               choices = c("iwate2" = "iwate.f2",
                                           "iwate" = "iwate.f",
                                           "Bunka"="Bunka",
                                           "Bunka3"="Bunka3"
                                           ), selected = "Bunka"),
                    selectInput("variables", "変数の複数選択（MAなど）単変数では最初のものだけ:", 
                                choices =  NULL,
                                multiple = TRUE,
                                selectize = FALSE,
                                size = 7
                                #selected =  colnames(iwate.f)[3]
                    ),  # 複数選択を許可
                    
                   selectInput("select_input_data_for_cross",
                               "クロス集計する変数",
                               choices = NULL),
                   selectInput("select_input_data_for_layer",
                               "層化する変数",
                               choices = NULL),
                  selectInput("select_input_data_for_hist",
                              "集計する変数",
                              choices = colnames("selected_data_for_plot"),
                              #  choices = colnames(iwate.f),
                              selected =  colnames(iwate.f)[3]),
                           #plotOutput("MAplot")
                 ),
                 #----　MAIN Panel
                 mainPanel(
                   tabsetPanel(type = "tabs",
                               tabPanel("単変数集計",
                                        h2("棒グラフと度数分布"),
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
                               tabPanel("pairs_multi",
                                        h2("GGally::pairs 多変数"),
                                        plotOutput("pairs_multi",width = 900, height = 900)
                               ),
                               
                               
                               tabPanel("2変数分析（層化）",
                                        h2("クロス集計（gtsummary::tbl_cross）"),
                                        #  verbatimTextOutput("crosstable"),
                                        gt_output(outputId = "my_gt_table"),
                                        plotOutput("crosschart2",width = 900, height = 600),
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
                               
                               tabPanel("Grid回答 General mosaic表示",
                                        h2("Grid回答mosaic表示"),
                                        plotOutput("GridAnswerG_mosaic",width = 600, height = 600),
                                        plotOutput("GridAnswerG_CA",width = 700, height = 700)
                               ),
                               
                               tabPanel("単変数check",
                                        h2("棒グラフと度数分布"),
                                        plotOutput("barchart2"),
                                        DT::dataTableOutput("simple_table2"), # 
                               ),
                               
                               
                               # tabPanel("Grid回答++/-- mosaic表示",
                               #          h2("Grid回答mosaic表示"),
                               #          plotOutput("GridAnswer_mosaic",width = 600, height = 600),
                               #          plotOutput("GridAnswer_CA",width = 700, height = 700)
                               # ),
                               # 
                               # 
                               # tabPanel("Grid回答 LK/DLK mosaic表示",
                               #          h2("Grid回答mosaic表示"),
                               #          plotOutput("GridAnswer2_mosaic",width = 600, height = 600),
                               #          plotOutput("GridAnswer2_CA",width = 700, height = 700)
                               # ),
                               
                            
                               tabPanel("選択変数のデータ一覧",
                                        h2("データ一覧"),
                                        DT::dataTableOutput("table_for_plot")
                               ),
#                               tabPanel("自由記述文分析")
                   )
              　)
               ),
#-----------------------------------------------------------------------------------------------
               tabPanel("調査票",
#                         h3("参考資料 PDFはブラウザの設定viewerで開きます"),
# #                       tags$a(href = "http://133.167.73.14/~kazuo/ruda0010-questionnaire.pdf", "PDFを開く", target = "_blank"),
#                         tags$iframe(style="height:800px; width:100%; scrolling=yes", #400px
#                                     #src="./CYDER_ENQ3/data/CYDER2020_ENQ_20200806.pdf"
#                                     #src="./data/CYDER2020_ENQ_20200806.pdf"
#                                     src="http://133.167.73.14/~kazuo/ruda0010-questionnaire.pdf"
#                         ),
                        HTML("<ul>"),
                            HTML("<li>"),
                              a(href = "http://133.167.73.14/~kazuo/BunkaQues.pdf", "「文化と不平等」調査調査票",target = "_blank"),
                            HTML("</li>"),
                            HTML("<li>"),
                              a(href = "http://133.167.73.14/~kazuo/ruda0010-questionnaire.pdf", "岩手調査調査票",target = "_blank"),
                            HTML("</li>"),
                            HTML("<li>"),
                            　a(href = "http://133.167.73.14/~kazuo/iwate%E5%A4%89%E6%95%B0%E5%AF%BE%E5%BF%9C%E8%A1%A8.pdf", "岩手調査変数対応表",target = "_blank"),
                            HTML("</li>"),
                        HTML("</ul>")
#                        helpText("別TabでPDFが開きます")
               ),

              tabPanel("使い方解説 【暫定版】",
                       a(href = "http://133.167.73.14/~kazuo//DataOverviewMan/index.html", "使い方解説：別タブで開きます",target = "_blank"),
                       
                       helpText("quarto Book で書いています")
              ),

              tabPanel("データアーカイブなど",
                       HTML("<ul>"),
                       HTML("<li>"),
                       a(href = "https://www.e-stat.go.jp/", "e-stat 政府統計の総合窓口",target = "_blank"),
                       HTML("</li>"),
                       HTML("<li>"),
                       a(href = "https://resas.go.jp/", "RESAS 地域経済分析システム",target = "_blank"),
                       HTML("</li>"),
                       
                       HTML("<li>"),
                       a(href = "https://csrda.iss.u-tokyo.ac.jp/", "SSJDA",target = "_blank"),
                       HTML("</li>"),
                       HTML("<li>"),
                       a(href = "https://jgss.daishodai.ac.jp/", "JGSS",target = "_blank"),
                       HTML("</li>"),
                       HTML("<li>"),
                       a(href = "https://issp.org/", "ISSP",target = "_blank"),
                       HTML("</li>"),
                       HTML("</ul>")
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
                     "iwate.f2" = iwate.f2[,-c(1,2)],
                     "iwate.f" = iwate.f[,-c(1,2)],
                     "Bunka" = Bunka[,-c(1,2)],
                     "Bunka3" = Bunka3[,]
                     )
      updateSelectInput(session, "select_input_data_for_hist", choices = colnames(data))
      updateSelectInput(session, "select_input_data_for_cross", choices = c(" ",colnames(data)))
      updateSelectInput(session, "select_input_data_for_layer", choices = c(" ",colnames(data)))
    #  updateSelectInput(session, "valiables", choices = colnames(data))
      updateSelectInput(session, "variables",
                        choices = colnames(data),
                        selected = colnames(data)[1:2]) 
      return(data)
    })
    # barplot by ggplot2
    output$barchart <- renderPlot({            # input$select_input_data_for_hist
      data_for_plot() %>% count(!!!rlang::syms(input$variables[1])) %>% rename(V1=1) %>% mutate(rate=100*n/sum(n)) %>% 
        ggplot(aes(x=V1,y=rate)) + geom_col(aes(fill=V1)) + ggtitle(input$variables[1])
    })
    
    output$barchart2 <- renderPlot({            
      data_for_plot() %>% count(!!!rlang::syms(input$variables[1])) %>% rename(V1=1) %>% mutate(rate=100*n/sum(n)) %>% 
        ggplot(aes(x=V1,y=rate)) + geom_col(aes(fill=V1)) + ggtitle(input$select_input_data_for_hist)
    })
    
    
    
    # GGally::ggpairs
    output$pairs <- renderPlot({
      data_for_plot()[,c(input$variables[1],input$select_input_data_for_cross)] %>% 
        ggpairs(mapping = aes(color = !!as.name(input$variables[1]))) +
        theme(axis.text.x = element_text(angle=45,hjust = 1)) + 
        ggtitle(input$variables[1]) -> p
      p
    })  
    
    output$pairs_multi<- renderPlot({
      data_for_plot()[,input$variables] %>% 
        ggpairs(mapping = aes(color = !!as.name(input$variables))) +
        theme(axis.text.x = element_text(angle=45,hjust = 1)) + 
        ggtitle(input$variables) -> p
      p
    })  
    
    
    
    # mosaic plot
    output$crosschart <- renderPlot({
      .tbl <- table(data_for_plot()[,input$select_input_data_for_cross],
                    data_for_plot()[,input$variables[1]])  #select_input_data_for_hist 
      .tbl.p <- round(100 * prop.table(.tbl ,margin = 1),1)
      tab <- ifelse(.tbl.p < 1, NA, .tbl.p) 
      
      data_for_plot()[,c(input$variables[1],     #select_input_data_for_hist,
                         input$select_input_data_for_cross)] %>% 
        structable() %>%
        mosaic(shade=TRUE,las=2,
               labeling=labeling_values
        )
    })
    # 層化mosaic　plot  
    output$crosschart2 <- renderPlot({
      .tbl <- table(data_for_plot()[,input$select_input_data_for_cross],
                    data_for_plot()[,input$variables[1]])  # select_input_data_for_hist 
      .tbl.p <- round(100 * prop.table(.tbl ,margin = 1),1)
      tab <- ifelse(.tbl.p < 1, NA, .tbl.p) 

      data_for_plot()[,c(input$variables[1], #select_input_data_for_hist,
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
                                      col = input$variables[1], #select_input_data_for_hist,
                                      percent = "row") %>% 
          add_p(test="chisq.test") %>% 
          bold_labels() %>% 
          as_gt()
      )
    
# chisq.test 
    output$chisq_test <- renderPrint({
      res.chisq <- chisq.test(table(data_for_plot()[,input$select_input_data_for_cross],
                       data_for_plot()[,input$variables[1]]))  #select_input_data_for_hist]))
#browser()
      print(res.chisq)
    })
    
# gtsummay でMA表
    output$MA_gt_table <- render_gt(
      data_for_plot() %>% tbl_cross(row = input$select_input_data_for_cross,
                                    col = input$variables[1],#select_input_data_for_hist,
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
        gp_vari <- input$select_input_data_for_layer # 層化変数
        data_for_plot() %>% group_by(!!!rlang::syms(gp_vari)) %>% 
          dplyr::summarise(度数=n(),across(selected_vars, ~ sum(. == 1,na.rm = TRUE)/n(),.names="ratio_{col}")) -> MA_group_tbl

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
          scale_shape_manual(values = 1:length(selected_vars)) 
        }
    })
    
    # 層化MA warp Faset
    
    output$MAplot_lineDotwarp <- renderPlot({
      selected_vars <- input$variables # 選択された変数群
      if (length(selected_vars) > 0) {
        selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
#        browser()
        gp_vari <- input$select_input_data_for_layer # 層化する変数
 #       df %>% group_by(!!!rlang::syms(gp_vari)) %>% 
        data_for_plot() %>% group_by(!!!rlang::syms(gp_vari)) %>% 
        dplyr::summarise(度数=n(),across(selected_vars, ~ sum(. == 1,na.rm = TRUE)/n(),.names="ratio_{col}")) -> MA_group_tbl
        
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
        
      }
    })
    

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
      t(cat_tbl) %>% as.tibble() %>% mutate(ID=rnames,IDn=1:length(rnames)) %>% 
        mutate(Like=`++`+`+`) %>% 
        arrange(desc(Like)) %>% 
        select(IDn) %>% unlist %>% 
        setNames(NULL) -> order_vec
      
      t(cat_tbl)[order_vec,] %>% 
        mosaic(shade = TRUE,rot_labels = c(0, 0),
               margins=c(left=9,top=5),just_labels=c(left="right",top="left"))
      
    })

## GridAnswer CA
    
    output$GridAnswer_CA <- renderPlot({
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

　　　res.CA <- FactoMineR::CA(t(cat_tbl))

    })
      
# Grid Mosaic 2 LK/DLK
    
    output$GridAnswer2_mosaic <- renderPlot({
      grid_ptn <- c("A","B","C","D","E","NA") # 仮のパターン
      selected_vars <- input$variables
      count_categories <- function(x) {
        table(factor(x, levels = grid_ptn,
                     exclude = NULL))
      }
      
      # df の 1 列目から 20 列目の各列ごとにカテゴリを集計
      category_count_tbl <- data_for_plot() %>%
        dplyr::summarise(across(selected_vars, ~ count_categories(.))) 
      
      category_count_tbl %>% as.matrix() -> cat_tbl
      rownames(cat_tbl) <- grid_ptn 
      cat_tbl
      
      rownames(t(cat_tbl)) -> rnames
      t(cat_tbl) %>% as.tibble() %>% mutate(ID=rnames,IDn=1:length(rnames)) %>% 
#        mutate(Like=`++`+`+`) %>% 
        arrange(desc(A)) %>% 
        select(IDn) %>% unlist %>% 
        setNames(NULL) -> order_vec
      
#      t(cat_tbl)[order_vec,] 
      t(cat_tbl) %>% mosaic(shade = TRUE,rot_labels = c(0, 0),
                                        margins=c(left=9,top=5),just_labels=c(left="right",top="left"))
      
    })
    
    
# GridAnswer2 CA
    output$GridAnswer2_CA <- renderPlot({
      grid_ptn <- c("A","B","C","D","E","NA") # 仮のパターン
      selected_vars <- input$variables 
      count_categories <- function(x) {
        table(factor(x, levels = grid_ptn,#c("++", "+", "-", "--", "DK", "無回答"), 
                     exclude = NULL))
      }
      
      # df の 1 列目から 20 列目の各列ごとにカテゴリを集計
      category_count_tbl <- data_for_plot() %>%
        dplyr::summarise(across(selected_vars, ~ count_categories(.))) 
      
      category_count_tbl %>% as.matrix() -> cat_tbl
      rownames(cat_tbl) <- grid_ptn #c("++","+","-","--","DK","無回答")
      cat_tbl
      
      res.CA <- FactoMineR::CA(t(cat_tbl))
      
    })

    
    # Grid Mosaic Genenral
    
    output$GridAnswerG_mosaic <- renderPlot({

      selected_vars <- input$variables
      vectors <- map(selected_vars, ~ {
　        data_for_plot() %>% select(selected_vars) %>% 
          count(!!sym(.x)) %>%  # 選択した列ごとにカウント
          pull(1)               # 最初の列のユニークな値を取得
      })
      
      union_all <- reduce(vectors, union)
      union_all <- ifelse(is.na(union_all), "NA", union_all)
      count_categories <- function(x) {
        table(factor(x, levels = union_all, exclude =NULL))
      }
      
      # df のselected_varsの各列ごとにカテゴリを集計
      category_count_tbl <- data_for_plot() %>%
        dplyr::summarise(across(selected_vars, ~ count_categories(.))) 
      
      category_count_tbl %>% as.matrix() -> cat_tbl
      rownames(cat_tbl) <- union_all #grid_ptn 
      cat_tbl
      # cat_tbl の名前部分に NA が含まれていれば "NA" に変換
      names(cat_tbl) <- ifelse(is.na(names(cat_tbl)), "NA", names(cat_tbl))
      
      rownames(t(cat_tbl)) -> rnames
      t(cat_tbl) %>% as.tibble() %>% mutate(ID=rnames,IDn=1:length(rnames)) %>% # ★t(cat_tbl) %>% as.tibble() でエラー
        arrange(desc(union_all[1])) %>% 
        select(IDn) %>% unlist %>% 
        setNames(NULL) -> order_vec
     
      t(cat_tbl)[order_vec,] %>%  
      #t(cat_tbl) %>% 
        mosaic(shade = TRUE,rot_labels = c(0, 0),
                            margins=c(left=12,top=5),just_labels=c(left="right",top="left"))
      
    })
    
    
    # GridAnswerG CA
    output$GridAnswerG_CA <- renderPlot({
      selected_vars <- input$variables
      #browser()
      vectors <- map(selected_vars, ~ {
        data_for_plot() %>% select(selected_vars) %>% 
          count(!!sym(.x)) %>%  # 選択した列ごとにカウント
          pull(1)               # 最初の列のユニークな値を取得
      })
      
      union_all <- reduce(vectors, union)
      
      count_categories <- function(x) {
        table(factor(x, levels = union_all, exclude = NULL))
      }
      
      # df のselected_varsの各列ごとにカテゴリを集計
      category_count_tbl <- data_for_plot() %>%
        dplyr::summarise(across(selected_vars, ~ count_categories(.))) 
      
      category_count_tbl %>% as.matrix() -> cat_tbl
      rownames(cat_tbl) <- union_all #grid_ptn 
      cat_tbl
      
      res.CA <- FactoMineR::CA(t(cat_tbl))
      
    })
    

# タイムスタンプを取得    
    output$timestamp <- renderText({
      file_info <- file.info("app.R")  # ファイル情報を取得
      timestamp <- file_info$mtime     # 最終更新日時
      paste("app.Rの最終更新:", format(timestamp, "%Y-%m-%d %H:%M:%S"))
    })

# 度数分布表
    output$simple_table <- DT::renderDataTable({ #renderTable({#
      table(data_for_plot()[,input$variables[1]]) -> tmp #select_input_data_for_hist]) -> tmp
      round(100*prop.table(tmp),1) -> tmp2
      data.frame(tmp,rate=tmp2)[,c(1,2,4)]
    })   

    output$simple_table2 <- DT::renderDataTable({ #renderTable({#
      table(data_for_plot()[,input$select_input_data_for_hist]) -> tmp
      round(100*prop.table(tmp),1) -> tmp2
      data.frame(tmp,rate=tmp2)[,c(1,2,4)]
    })   
    

    output$table_for_plot <- DT::renderDataTable({
      data_for_plot() %>% select(input$variables)
    })
  }

#-------------------------------------------------------------------------------
# Run the application 
#
shinyApp(ui = ui, server = server)
