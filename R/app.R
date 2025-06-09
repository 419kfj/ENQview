#' 調査票データ一覧ツール
#
#' 履歴
#' 2025/05/27 tableに投入するdfの参照を[,vari]から[[vari]]に変更。これでエラーがでなくなった。
#' 2024/12/22 function化、パッケージ化、一段落
#' 2024/12/21 function化作業開始
#' 2024/01/30 gitでの管理　を開始
#' @export
ENQview <- function(data.df=Bunka2,...){
#ENQview <- function(data.df=cyder2024a.all.df,...){

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

#-------------------------------------------------------------------------------
# Define UI for application
#
##ui <- navbarPage("調査データ簡易集計",
ui <- fluidPage("調査データ簡易集計",
#                tabPanel("About",
#             textOutput("timestamp")  # タイムスタンプの表示
#                ),
               #--------------------------------
               tabPanel(
                 "基本集計",
                 h1("基本集計を行います。"),
                 sidebarPanel(
                 selectInput(
                      inputId = "selected_data_for_plot",
                      label = h3("集計対象はこれです："),
                      choices = "data.df",
                      selected = "data.df" # デフォルトで最初の列を選択
#                    )
                 ),

                    selectInput("variables", "変数の複数選択（MAなど )単変数では最初のものだけ:",
                                choices =  NULL,
                                multiple = TRUE,
                                selectize = FALSE,
                                size = 7
                    ),  # 複数選択を許可

                   selectInput("select_input_data_for_cross",
                               "クロス集計する変数",
                               choices = NULL),
                   selectInput("select_input_data_for_layer",
                               "層化する変数",
                               choices = NULL),
                  selectInput("select_input_data_for_hist",
                              "集計する変数",
                              choices = NULL, #colnames("selected_data_for_plot"),
                              selected =  colnames(data.df)[3]),
                 ),
                 #---- MAIN Panel
                 mainPanel(
                   tabsetPanel(type = "tabs",
                               tabPanel("単変数集計",
                                        h2("棒グラフと度数分布"),
                                        plotOutput("barchart"),
                                        DT::dataTableOutput("simple_table"), # server.R も対応させること！
                               ),
                               tabPanel("2変数分析",
                                        h2("クロス集計（gtsummary::tbl_cross )"),
                                      #  verbatimTextOutput("crosstable"),
                                        gt_output(outputId = "my_gt_table2"),
                                        plotOutput("crosschart",width = 600, height = 600),
                                        h3("χ2乗検定"),
                                        verbatimTextOutput("chisq_test2")
                               ),
                               tabPanel("pairs",
                                        h2("GGally::pairs"),
                                        plotOutput("pairs",width = 600, height = 600)
                               ),
                               tabPanel("pairs_multi",
                                        h2("GGally::pairs 多変数"),
                                        plotOutput("pairs_multi",width = 900, height = 900)
                               ),


                               tabPanel("2変数分析（層化 )",
                                        h2("クロス集計（gtsummary::tbl_cross )"),
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
                               tabPanel("選択変数のデータ一覧",
                                        h2("データ一覧"),
                                        DT::dataTableOutput("table_for_plot")
                               ),
                   )
               )
               ),
)

#-------------------------------------------------------------------------------
# Define server logic required to draw outlut tables and graphs
#
server <- function(input, output, session) {
#    addResourcePath("data", "data")
    output$distPlot_shiny <- renderPlot({
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins_shiny + 1)
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })

    # data_for_plot <- reactive({
    #   data.df
    # })


    data_for_plot <- reactive({
      data <- switch(input$selected_data_for_plot,
                     "data.df" = data.df,
                     "iris" = iris,
                     "titanic" = data.frame(lapply(data.frame(Titanic),
                                                   function(i){rep(i, data.frame(Titanic)[, 5])})),
                     "Womenlf" = Womenlf,
                     "iwate.f2" = iwate.f2[,-c(1,2)],
                     "iwate.f" = iwate.f[,-c(1,2)],
                     "Bunka" = Bunka[,-c(1,2)],
                     "Bunka3" = Bunka3[,],
                     "UTAS2020_a" = UTAS2020_a[,-c(1,2)],
                     "issp2016" = issp2016[,-c(1,2,3,4)]
                     )
      updateSelectInput(session, "select_input_data_for_hist", choices = colnames(data))
      updateSelectInput(session, "select_input_data_for_cross", choices = c(" ",colnames(data)))
      updateSelectInput(session, "select_input_data_for_layer", choices = c(" ",colnames(data)))
    #  updateSelectInput(session, "valiables", choices = colnames(data))
      updateSelectInput(session, "variables",
                        choices = colnames(data),
                        selected = colnames(data)[3:4])
      return(data)
    })
  # barplot by ggplot2
    output$barchart <- renderPlot({            # input$select_input_data_for_hist
      data_for_plot() %>% count(!!!rlang::syms(input$variables[1])) %>% rename(V1=1) %>%
        mutate(rate=100 * .data[["n"]]/sum(.data[["n"]])) %>%
        ggplot(aes(x=V1,y=rate)) + geom_col(aes(fill=V1)) + ggtitle(input$variables[1])
    })

    output$barchart2 <- renderPlot({
      data_for_plot() %>% count(!!!rlang::syms(input$select_input_data_for_hist)) %>% rename(V1=1) %>%
        mutate(rate=100* .data[["n"]]/ sum(.data[["n"]])) %>%
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
      .tbl <- table(data_for_plot()[[input$select_input_data_for_cross]],
                    data_for_plot()[[input$variables[1]]])  #select_input_data_for_hist


            .tbl.p <- round(100 * prop.table(.tbl ,margin = 1),1)
      tab <- ifelse(.tbl.p < 1, NA, .tbl.p)

      data_for_plot()[,c(input$variables[1],     #select_input_data_for_hist,
                         input$select_input_data_for_cross)] %>%
        structable() %>%
        mosaic(shade=TRUE,las=2,
               labeling=labeling_values
        )
    })
    # 層化mosaic plot
    output$crosschart2 <- renderPlot({
      .tbl <- table(data_for_plot()[[input$select_input_data_for_cross]],
                    data_for_plot()[[input$variables[1]]])  # select_input_data_for_hist
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
        data_for_plot() %>% tbl_cross(col = input$select_input_data_for_cross,
                                      row = input$variables[1], #select_input_data_for_hist,
                                      percent = "row") %>%
          add_p(test="chisq.test") %>%
          bold_labels() %>%
          as_gt()
      )

    output$my_gt_table2 <- render_gt(
      data_for_plot() %>% tbl_cross(col = input$select_input_data_for_cross,
                                    row = input$variables[1], #select_input_data_for_hist,
                                    percent = "row") %>%
        add_p(test="chisq.test") %>%
        bold_labels() %>%
        as_gt()
    )


# chisq.test
    output$chisq_test <- renderPrint({
      res.chisq <- chisq.test(table(data_for_plot()[[input$select_input_data_for_cross]],
                       data_for_plot()[[input$variables[1]]]),correct = FALSE)  #select_input_data_for_hist]))
      print(res.chisq)
    })

    output$chisq_test2 <- renderPrint({
      res.chisq <- chisq.test(table(data_for_plot()[[input$select_input_data_for_cross]],
                                    data_for_plot()[[input$variables[1]]]),correct = FALSE)  #select_input_data_for_hist]))
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

#    MA plot
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
               y = "割合（% )") +
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

        ratio_df %>% ggplot(aes(x=Ratio, y=reorder(Question,Ratio))) + # 並べ替え
          geom_point(size=3) +
          theme_bw() +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_line(colour="grey60",linetype="dashed")) +
          labs(title = selected_vars,
               x = "割合（% )",y = "質問項目")
      }
    })

    # 層化MA折れ線グラフ
    output$MAplot_lineDot <- renderPlot({
      selected_vars <- input$variables
      if (length(selected_vars) > 0) {
        selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
        gp_vari <- input$select_input_data_for_layer # 層化変数
        data_for_plot() %>% group_by(!!!rlang::syms(gp_vari)) %>%
          dplyr::summarise(度数=n(),across(selected_vars, ~ sum(. == 1,na.rm = TRUE)/n(),.names="ratio_{col}")) -> MA_group_tbl # ここで、行を選択すればいよい
        MA_group_tbl %>% select(-度数) %>%
          pivot_longer(cols = starts_with("ratio_"),  # ratio_で始まる列 (変数1〜8) をlong形式に変換
                       names_to = "variable",         # 変数名の列を"variable"として格納
                       values_to = "value") -> df_long

        ggplot(df_long, aes(x = !!as.name(gp_vari), y = value, #color = variable,
                            shape =variable, group = variable)) +
          geom_line(aes(color = variable)) +  # 折れ線グラフ
          geom_point(aes(color = variable),size=4) + # ポイントを追加（必要なら )
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

        gp_vari <- input$select_input_data_for_layer # 層化する変数
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
      table(data_for_plot()[[input$variables[1]]]) -> tmp #select_input_data_for_hist]) -> tmp
      round(100*prop.table(tmp),1) -> tmp2
      data.frame(tmp,rate=tmp2)[,c(1,2,4)]
    })

    output$simple_table2 <- DT::renderDataTable({ #renderTable({#
      table(data_for_plot()[[input$select_input_data_for_hist]]) -> tmp
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
}
