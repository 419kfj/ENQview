#data_preset <- function(){

library(tidyverse)

   load("./data_0/UTAS2020_a.rda") # 2024/10/29
   load("./data_0/iwate.f.mac.rda")
   load("./data_0/iwate.f.mac2.rda")
   load("./data_0/dd3.rda") # .dd3
   load("./data_0/Bunka2.rda") # Bunka2  変数名を記号＋日本語の構成にした 2024/10/16
　 Bunka3 <- Bunka2
  #load(file="../../RStudio/文化と不平等202409/01.6_Recode/data/d3.rda")
  load(file="./data_0/d3.rda")
  .dd <- .d3

 load("./data_0/ISSP2016_jp_.d1.rda") # .d1
  issp2016_0 <- .d1 %>% select(1:75,77,95,113:131,149,167:170,188,206,241,242,277:279,297,332,
                               350:353,371,389:393)
  .vari_tbl <- read_csv(file="vari_list.csv")
  vari_conv_list <- setNames(.vari_tbl$value,.vari_tbl$Qname)
  issp2016_0 %>% rename(!!!vari_conv_list) -> issp2016


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
    mutate(across(c(10:21, #Q2 ### 先頭の２つがずれている
                    41:45,#Q4　##修正済み
                    72:90,#Q7    ##修正すみ
                    191:202,#Q14　##修正すみ
                    256:275,#Q36 ##修正すみ
                    276:291, #Q37##修正すみ
                    295:301 #Q41##修正すみ
    ),
    # mutate(across(c(10:21, #Q2
    #               43:47,#Q4
    #               74:89,#Q7
    #               193:204,#Q14
    #               258:277,#Q36
    #               278:293, #Q37
    #               297:303 #Q41
    #               ),
    ~ case_when(
      . == "On"  ~ 1,
      . == "Off" ~ 0,
      TRUE ~ NA_real_)
    )
    ) %>%
    mutate(across(c(105:117,#Q9  107:139
                    118:135,#Q10
                    136:157,#Q11
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

#}
