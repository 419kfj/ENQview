#' speMCA用の変数選択app
#'
#' dfを指定して、appを起動
#' Active変数を選択
#' juckカテゴリを選択
#' speMCAを実行
#' これによって、修正慣性率、変数マップ、個体マップを表示できるようになる。
#' speMCAが生成したresultは、ダウンロードすることが可能なので、これをexplorに渡す、もしくは、
#' そのあとの追加変数処理に渡すことが可能
#'
#' 2025/06/16 η2の値、tabcontribの表、典型性検定、同質性検定を追加。
#' 2025/06/15 SHinyMCA.Rをコピー、レイアウトをGDAに則したものに変更。まずは、コピーして動作確認、
#' 2025/06/13 変数マップなど、マップサイズをブラウザの幅にあわせて拡大/縮小
#' 2025/06/11 追加変数の選択、マップ表示、交互作用plotの機能を追加した
#' @import shiny
#' @import GDAtools
#' @import ggplot2
#' @import dplyr
#' @importFrom DT datatable
#' @export
Shiny_GDA <- function(df) {
　showtext::showtext_auto(TRUE)
  draw_ellpses_map_com <- function(resmca, dim_vec, title_prefix = ""){
    # req(input$var_ellipses, input$selected_categories)
    # dim_vec <- c(1,3)
    # resmca <- mca_result()
    # var <- df[[input$var_ellipses]]
    base_map_ind <- GDAtools::ggcloud_indiv(resmca, col = "lightgrey",axes = dim_vec)
    # カテゴリ番号の取得
    var_factor <- as.factor(var)
    sel_index <- which(levels(var_factor) %in% input$selected_categories)
    # 楕円を重ねる
    GDAtools::ggadd_kellipses(base_map_ind, resmca, var = var_factor, sel = sel_index,axes=dim_vec) +
      coord_fixed(ratio=1) + ggtitle(paste("個体空間", paste(dim_vec, collapse = "-"), "軸", sep = " "))
  }

  ui <- navbarPage("GDA幾何学的データ解析",
                   tabPanel("1）speMCA実行",
                            sidebarLayout(
                              sidebarPanel(
                                tags$p(paste0("ENQview Version: ", as.character(utils::packageVersion("ENQview"))),
                                       style = "color: gray;"),
                                selectInput("variables", "Active変数を選んでください",
                                            choices = names(df), multiple = TRUE, selectize = FALSE, size = 7),
                                uiOutput("junk_selector"),
                                actionButton("run_speMCA", "speMCAを実行"),
                                downloadButton("download_mca", "speMCA結果をダウンロード")
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("選択変数", uiOutput("selected_info")),
                                  tabPanel("修正慣性率", tableOutput("eig_table"), plotOutput("eig_plot")),
                                  tabPanel("Active変数の相関比", plotOutput("eta2_map",height = "600px"),
                                           plotOutput("eta2_map_32",height = "600px"),
                                           plotOutput("eta2_map_13",height = "600px")),
                                  tabPanel("変数空間", plotOutput("var_map",height = "600px"),
                                                       plotOutput("var_map_32",height = "600px"),
                                                       plotOutput("var_map_13",height = "600px")),
                                  tabPanel("個体空間", plotOutput("ind_map",height = "600px"),
                                                       plotOutput("ind_map_32",height = "600px"),
                                                       plotOutput("ind_map_13",height = "600px")),
                                )
                              )
                            )
                   ),

                   tabPanel("2）変数空間分析",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("supvars", "追加変数を選んでください",
                                            choices = names(df), multiple = TRUE, selectize = FALSE, size = 7),
                                h5("v1とv2に同じ変数を指定すると、全体（グレー）にその変数だけを浮かび上がらせて表示します"),
                                selectInput("inter_v1", "交互作用v1を選んでください", choices = names(df), multiple = FALSE),
                                selectInput("inter_v2", "交互作用v2を選んでください", choices = names(df), multiple = FALSE)
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("軸ごとのContribution表", h4("dim1 "),
                                                                     tableOutput('tabcontrib_out_1'),
                                                                     h4("dim2"),
                                                                     tableOutput('tabcontrib_out_2'),
                                                                     h4("dim3"),
                                                                     tableOutput('tabcontrib_out_3')),
                                  tabPanel("supvarsの情報", verbatimTextOutput("supvars_out")),
                                  tabPanel("変数マップ＋supvars", plotOutput("supvars_map",height = "600px"),
                                                                  plotOutput("supvars_map_32",height = "600px"),
                                                                  plotOutput("supvars_map_13",height = "600px")),
                                  tabPanel("交互作用plot", plotOutput("interaction_map",height = "600px"),
                                                           plotOutput("interaction_map_32",height = "600px"),
                                                           plotOutput("interaction_map_13",height = "600px")),
                                )
                              )
                            )
                   ),

                   tabPanel("3）個体空間分析",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("var_ellipses", "集中楕円表示変数を選んでください",
                                            choices = names(df), multiple = FALSE,selected = NULL),
                                uiOutput("kellipses_cat_selector")
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("集中楕円",plotOutput("kellipses_map_12",height = "600px"),
                                                      plotOutput("kellipses_map_32",height = "600px"),
                                                      plotOutput("kellipses_map_13",height = "600px")
                                  ),
                                  tabPanel("典型性検定/同質性検定", verbatimTextOutput("typi_test_out"),
                                                                    verbatimTextOutput("homog_test_out")
                                 ),
                                  tabPanel("信頼楕円")
                                  )
                              　)
                            　)
                   ),

                   tabPanel("4）参考資料",
                            fluidPage(
                              h4("分析対象 データ表示"),
                              DTOutput("data_table"),
                              h4("ENQview Version"),
                              tags$p(paste0("ENQview Version: ", as.character(utils::packageVersion("ENQview"))),
                                     style = "color: gray;")
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

    # tabcontrib の出力
    ## dim1
    tabcontrib_result_1 <- reactive({
      req(mca_result())
      tryCatch({
        GDAtools::tabcontrib(resmca = mca_result(),dim = 1,best=TRUE, shortlabs = TRUE)
        }, error = function(e) {
        message("tabcontribエラー: ", e$message)
        NULL
      })
      })
    output$tabcontrib_out_1 <- renderTable({
      res <- tabcontrib_result_1()
      if (is.null(res)) return("tabcontribの結果がありません")
      print(res)
    })

    ## dim2
    tabcontrib_result_2 <- reactive({
      req(mca_result())
      tryCatch({
        GDAtools::tabcontrib(resmca = mca_result(),dim = 2,best=TRUE, shortlabs = TRUE)
      }, error = function(e) {
        message("tabcontribエラー: ", e$message)
        NULL
      })
    })
    output$tabcontrib_out_2 <- renderTable({
      res <- tabcontrib_result_2()
      if (is.null(res)) return("tabcontribの結果がありません")
      print(res)
    })

    ## dim3
    tabcontrib_result_3 <- reactive({
      req(mca_result())
      tryCatch({
        GDAtools::tabcontrib(resmca = mca_result(),dim = 3,best=TRUE, shortlabs = TRUE)
      }, error = function(e) {
        message("tabcontribエラー: ", e$message)
        NULL
      })
    })
    output$tabcontrib_out_3 <- renderTable({
      res <- tabcontrib_result_3()
      if (is.null(res)) return("tabcontribの結果がありません")
      print(res)
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

    # 典型性検定と同質性検定 #
    ## 典型性検定
    typi_test_result <- reactive({
#      req(mca_result(), input$supvars)
      tryCatch({
        GDAtools::dimtypicality(resmca = mca_result(), vars = (df %>% select(input$var_ellipses)),dim = c(1,2,3))
      }, error = function(e) {
        message("simtypicalityエラー: ", e$message)
        NULL
      })
    })
    ## typicality testの出力を表示（テキスト）
    output$typi_test_out <- renderPrint({
      res <- typi_test_result()
      if (is.null(res)) return("typi_testの結果がありません")
      print(res)
    })

    # 同質性検定
    homog_test_result <- reactive({
      req(mca_result(), input$var_ellipses)
      tryCatch({
  #      browser()##
        GDAtools::homog.test(resmca = mca_result(), var = df[[input$var_ellipses]],dim = c(1,2,3))
      }, error = function(e) {
        message("homog.testエラー: ", e$message)
        NULL
      })
    })
    ## homog.testの出力を表示（テキスト）
    output$homog_test_out <- renderPrint({
      res <- homog_test_result()
      if (is.null(res)) return("homog_testの結果がありません")
      print(res)
    })


    ## eta2 map 1−２軸
    output$eta2_map <- renderPlot({
      req(mca_result())
      tryCatch({
        #browser()##
        GDAtools::ggeta2_variables(resmca = mca_result(),axes = c(1,2)) + theme(aspect.ratio = 1)
      }, error = function(e) {
        message("ggeta2_variables エラー: ", e$message)
        return(NULL)
      })
    },width = "auto", height = 600)

　　##　3−２軸
    output$eta2_map_32 <- renderPlot({
      req(mca_result())
      tryCatch({
        #browser()##
        GDAtools::ggeta2_variables(resmca = mca_result(),axes = c(3,2)) + theme(aspect.ratio = 1)
      }, error = function(e) {
        message("ggeta2_variables エラー: ", e$message)
        return(NULL)
      })
    },width = "auto", height = 600)

    output$eta2_map_13 <- renderPlot({
      req(mca_result())
      tryCatch({
        #browser()##
        GDAtools::ggeta2_variables(resmca = mca_result(),axes = c(1,3)) + theme(aspect.ratio = 1)
      }, error = function(e) {
        message("ggeta2_variables エラー: ", e$message)
        return(NULL)
      })
    },width = "auto", height = 600)





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


    output$supvars_map_32 <- renderPlot({
      req(mca_result(), input$supvars)
      dim_vec <- c(3,2)
      tryCatch({
        base_map <- GDAtools::ggcloud_variables(mca_result(), col = "lightgrey", axes = dim_vec)
        GDAtools::ggadd_supvars(p = base_map,
                                resmca = mca_result(),
                                axes = dim_vec,
                                vars = (df %>% select(input$supvars))) +#df[[input$supvars]]) +
          theme(aspect.ratio = 1)
      }, error = function(e) {
        message("ggadd_supvars エラー: ", e$message)
        return(NULL)
      })
    },width = "auto", height = 600)


    output$supvars_map_13 <- renderPlot({
      req(mca_result(), input$supvars)
      dim_vec <- c(1,3)
      tryCatch({
        base_map <- GDAtools::ggcloud_variables(mca_result(), col = "lightgrey",axes = dim_vec)
        GDAtools::ggadd_supvars(p = base_map,
                                resmca = mca_result(),
                                axes = dim_vec,
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


    output$interaction_map_32 <- renderPlot({
      req(input$inter_v1, input$inter_v2)
      tryCatch({
        dim_vec <- c(3,2)
        base_map <- GDAtools::ggcloud_variables(mca_result(), col = "lightgrey",axes = dim_vec)
        GDAtools::ggadd_interaction(p = base_map,
                                    resmca = mca_result(),
                                    axes = dim_vec,
                                    v1 = df[[input$inter_v1]],
                                    v2 = df[[input$inter_v2]]) + theme(aspect.ratio = 1)
      }, error = function(e) {
        message("ggadd_interaction エラー: ", e$message)
        return(NULL)
      })
    },width = "auto", height = 600)

    output$interaction_map_13 <- renderPlot({
      req(input$inter_v1, input$inter_v2)
      tryCatch({
        dim_vec <- c(1,3)
        base_map <- GDAtools::ggcloud_variables(mca_result(), col = "lightgrey",axes = dim_vec)
        GDAtools::ggadd_interaction(p = base_map,
                                    resmca = mca_result(),
                                    axes = dim_vec,
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
        ggtitle("変数空間 1−2軸")
    },width="auto",height="auto")#600)

    output$var_map_32 <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_variables(res,axes = c(3,2)) + theme(aspect.ratio=1)  +
        ggtitle("変数空間 3−2軸")
    },width="auto",height="auto")#600)

    output$var_map_13 <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_variables(res,axes = c(1,3)) + theme(aspect.ratio=1)  +
        ggtitle("変数空間 1−3軸")
    },width="auto",height="auto")#600)


    ##　個体マップの表示
    output$ind_map <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_indiv(res) + theme(aspect.ratio=1) +
        ggtitle("個体空間 1−2軸")
    },width = "auto", height = 600)

    output$ind_map_32 <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_indiv(res, axes = c(3,2)) + theme(aspect.ratio=1) +
        ggtitle("個体空間 3−2軸")
    },width = "auto", height = 600)

    output$ind_map_13 <- renderPlot({
      res <- mca_result()
      if (is.null(res)) return(NULL)
      ggcloud_indiv(res, axes = c(1,3)) + theme(aspect.ratio=1) +
        ggtitle("個体空間 1−3軸")
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
      kellipse_cat <- input$category_selector
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
        },
        h4("集中楕円描画カテゴリ"),
        if (!is.null(kellipse_cat) && length(kellipse_cat) > 0) {
          HTML(paste("<ul>", paste(paste0("<li>", kellipse_cat, "</li>"), collapse = ""), "</ul>"))
        } else {
          em("なし")
        }
      )
    })

    ## 集中楕円の描画
    #
    # 選択された変数のカテゴリを取得
    # カテゴリ選択UI
    output$kellipses_cat_selector <- renderUI({
      req(input$var_ellipses)
      var <- df[[input$var_ellipses]]
      levels <- levels(as.factor(var))
      checkboxGroupInput("selected_categories",
                         "表示するカテゴリ",
                         choices = levels,
                         selected = NULL)#levels) # 未選択状態から始める
    })

    # 楕円付き個体マップの描画 1-2軸
    output$kellipses_map_12 <- renderPlot({
      req(input$var_ellipses, input$selected_categories)
      dim_vec <- c(1,2)
      resmca <- mca_result()
      var <- df[[input$var_ellipses]]
      base_map_ind <- GDAtools::ggcloud_indiv(resmca, col = "lightgrey",axes = dim_vec)
      # カテゴリ番号の取得
      var_factor <- as.factor(var)
      sel_index <- which(levels(var_factor) %in% input$selected_categories)
      # 楕円を重ねる
      GDAtools::ggadd_kellipses(base_map_ind, resmca, var = var_factor, sel = sel_index,axes=dim_vec) +
        coord_fixed(ratio=1) + ggtitle(paste("個体空間", paste(dim_vec, collapse = "-"), "軸", sep = " "))
    },width = "auto", height = 600)

    # 楕円付き個体マップの描画 3-2軸
    output$kellipses_map_32 <- renderPlot({
      req(input$var_ellipses, input$selected_categories)
      dim_vec <- c(3,2)
      resmca <- mca_result()
      var <- df[[input$var_ellipses]]
      base_map_ind <- GDAtools::ggcloud_indiv(resmca, col = "lightgrey",axes = dim_vec)
      # カテゴリ番号の取得
      var_factor <- as.factor(var)
      sel_index <- which(levels(var_factor) %in% input$selected_categories)
      # 楕円を重ねる
      GDAtools::ggadd_kellipses(base_map_ind, resmca, var = var_factor, sel = sel_index,axes=dim_vec) +
        coord_fixed(ratio=1) + ggtitle(paste("個体空間", paste(dim_vec, collapse = "-"), "軸", sep = " "))
    },width = "auto", height = 600)


    # 楕円付き個体マップの描画 1−3軸
    output$kellipses_map_13 <- renderPlot({
      req(input$var_ellipses, input$selected_categories)
      var <- df[[input$var_ellipses]]
      dim_vec <- c(1,3)
      resmca <- mca_result()
#      draw_ellpses_map_com(resmca,dim_vec)
      base_map_ind <- GDAtools::ggcloud_indiv(resmca, col = "lightgrey",axes = dim_vec)
      # カテゴリ番号の取得
      var_factor <- as.factor(var)
      sel_index <- which(levels(var_factor) %in% input$selected_categories)
      # 楕円を重ねる
      GDAtools::ggadd_kellipses(base_map_ind, resmca, var = var_factor, sel = sel_index,axes=dim_vec) +
        coord_fixed(ratio=1) + ggtitle(paste("個体空間", paste(dim_vec, collapse = "-"), "軸", sep = " "))
    },width = "auto", height = 600)

} # END of Server sectopn
# ----------------------------------------------------------------------------
#      ShinyApp実行！
# ----------------------------------------------------------------------------
  shinyApp(ui, server)
}
