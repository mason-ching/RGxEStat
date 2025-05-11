#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @import tidyr
#' @import dplyr
#' @import purrr
#' @import sqldf
#' @import readxl
#' @import writexl
#' @import lme4
#' @importFrom afex mixed
#' @import broom
#' @import tibble
#' @import agricolae
#' @import cluster
#' @import grDevices
#' @import GGEBiplotGUI
#' @import forestmangr
#' @import tools

#create function for Likelihood ratio test
#a=output dataset name; example-anova1r
#b=full model name; example-fit.f1
#c=reduced model name; example-fit.f1r、
#d=effect name; example- "RPid", NOTE: call it in quotation

anova_lrt <- function (a,b,c,d){
  #level of significance
  a <-anova(b,c)
  #convert anova into data frame
  a <- data.frame(a)
  #convert row names into column
  a$name <- rownames(a)
  # drop row names
  rownames(a) <- NULL
  a <- a %>% filter(name=="b") %>%
    mutate(sov=d) %>% select(sov, Pr_Chisq = starts_with("Pr..Chisq."))
  # return the result
  return(a)
}


app_server <- function(input, output, session) {
  # application server logic


  tem1 <-reactive(
    {
      req(input$datafile)
      ext <- file_ext(input$datafile$datapath)
      tem1 <- switch(ext,
             csv  = read.csv(input$datafile$datapath, stringsAsFactors = FALSE),
             txt  = read.table(input$datafile$datapath, header = TRUE, stringsAsFactors = FALSE),
             xls  = read_excel(input$datafile$datapath),
             xlsx = read_excel(input$datafile$datapath),
             stop("Invalid file")
      )
      # data1<-input$data
      # format1<-input$format
      # if(is.null(format1)){return()}
      # else if(is.null(data1)){return()}
      # else if(format1=="3")
      #   tem1<-read_excel(data1$datapath, 1)
      # else
      #   tem1<-read.table(data1$datapath, header = TRUE)

      #Generate unique id for replication for anova
      tem1$YR <- as.factor(tem1$YR)
      tem1$LC <- as.factor(tem1$LC)
      tem1$CLT <- as.factor(tem1$CLT)
      tem1$RP <- as.factor(tem1$RP)
      tem1$RPid<-as.factor(paste(tem1$YR, tem1$LC, tem1$RP, sep ="."))
      tem1
  }
)
  #define trait name for later analysis

  tempa <- eventReactive(input$trait_submit,{

    # if(input$trait=="1")
    # {
    tem2 <- tem1() %>% rename("Trait" = input$trait) %>% select(YR, LC, RP, CLT, Trait,RPid)
    # }
    # else if(input$trait=="2")
    # {tem2 <-tem1() %>% rename("Trait" = "Trait2") %>% select(YR, LC, RP, CLT, Trait,RPid)}
    #
    # else if(input$trait=="3")
    # {tem2 <-tem1() %>% rename("Trait" = "Trait3") %>% select(YR, LC, RP, CLT, Trait,RPid)}
    #
    # else if(input$trait=="4")
    # {tem2 <-tem1() %>% rename("Trait" = "Trait4") %>% select(YR, LC, RP, CLT, Trait,RPid)}
    #
    # else if(input$trait=="5")
    # {tem2 <-tem1() %>% rename("Trait" = "Trait5") %>% select(YR, LC, RP, CLT, Trait,RPid)}

  })

  output$trait_ui <- renderUI({
    req(tem1())
    df_cols <- names(tem1())
    selectInput("trait", label = NULL, choices = df_cols[! df_cols %in% c("YR","LC","RP","CLT","RPid")])
  })

  #Compute environment -  Location by year combination
  tempa2 <- eventReactive(input$trait_submit,{
    tempa2 <- tempa() %>% mutate (ENV = paste(LC,YR, sep='-')) %>%
      #remove missing records
      na.omit()
  })


  ########################################################################
  ###               ANOSI: Analysis of Significance                    ###
  ########################################################################

  ###         ANOSI Case 1: CLT, YR, LC and RP - All Random            ###

  anosi_randall<- eventReactive(input$case1,{

    fit.f1<-lmer(Trait~ 1 + (1|YR)  + (1|LC) + (1|CLT) + (1|YR:LC) + (1|YR:CLT) +
                   (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())
    #model summary

    summary1 <- summary(fit.f1)

    #variance of random factors

    variance <- as.data.frame(summary1$varcor)

    #drop row names

    rownames(variance) <- NULL
    variance1 <- variance %>% select (-var1, -var2) %>%
      rename(sov=grp, Variance=vcov, Stddev=sdcor)

    # #Type 3  test of hypothesis
    # #Type III Wald chi-square tests
    #
    # Anova(fit.f1,  type = 3)
    #
    # #Type 1  test of hypothesis
    #
    # anova(fit.f1, type="marginal", test="F")

    #model fitness

    # anovacase1 <- plot(fit.f1,
    #                    main="Model fitness Case 1: CLT, YR, LC and RP - All Random",
    #                    xlab="Predicated Value", ylab="Residual")

    #LRT - likelihood ratio test for computing significance of random effect
    #null model for YR

    fit.f1y<-lmer(Trait~ 1 + (1|LC) + (1|CLT) + (1|YR:LC) + (1|YR:CLT) +
                    (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance

    anova1y <-anova(fit.f1,fit.f1y)

    #convert anova into data frame

    anova1y <- data.frame(anova1y)

    #convert rownames into column

    anova1y$name <- rownames(anova1y)

    #drop rownames

    rownames(anova1y) <- NULL
    anova1y <- anova1y %>% filter(name=="fit.f1") %>%
      mutate(sov="YR") %>% select(sov, Pr_Chisq = starts_with("Pr..Chisq."))

    #null model for LC

    fit.f1l<-lmer(Trait~ 1 + (1|YR)  + (1|CLT) + (1|YR:LC) + (1|YR:CLT) +
                    (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance

    anova1l <-anova(fit.f1,fit.f1l)

    #convert anova into data frame

    anova1l <- data.frame(anova1l)

    #convert row names into column

    anova1l$name <- rownames(anova1l)

    #drop row names

    rownames(anova1l) <- NULL
    anova1l <- anova1l %>% filter(name=="fit.f1") %>%
      mutate(sov="LC") %>% select(sov, Pr_Chisq = starts_with("Pr..Chisq."))

    #null model for CLT

    fit.f1c<-lmer(Trait~ 1 + (1|YR)  + (1|LC) + (1|YR:LC) + (1|YR:CLT) +
                    (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance

    anova1c <-anova(fit.f1,fit.f1c)

    #convert anova into data frame

    anova1c <- data.frame(anova1c)

    #convert row names into column

    anova1c$name <- rownames(anova1c)

    #drop row names

    rownames(anova1c) <- NULL
    anova1c <- anova1c %>% filter(name=="fit.f1") %>%
      mutate(sov="CLT") %>% select(sov, Pr_Chisq = starts_with("Pr..Chisq."))

    #null model for YR:LC
    fit.f1yl<-lmer(Trait~ 1 + (1|YR)  + (1|LC) + (1|CLT) + (1|YR:CLT) +
                     (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance

    anova1yl <-anova(fit.f1,fit.f1yl)

    #convert anova into data frame

    anova1yl <- data.frame(anova1yl)

    #convert row names into column

    anova1yl$name <- rownames(anova1yl)

    #drop row names

    rownames(anova1yl) <- NULL
    anova1yl <- anova1yl %>% filter(name=="fit.f1") %>%
      mutate(sov="YR:LC") %>% select(sov, Pr_Chisq = starts_with("Pr..Chisq."))

    #null model for YR:CLT

    fit.f1yc<-lmer(Trait~ 1 + (1|YR)  + (1|LC) + (1|CLT) + (1|YR:LC) +
                     (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance

    anova1yc <-anova(fit.f1,fit.f1yc)

    #convert anova into data frame

    anova1yc <- data.frame(anova1yc)

    #convert rownames into column

    anova1yc$name <- rownames(anova1yc)

    #drop rownames

    rownames(anova1yc) <- NULL
    anova1yc <- anova1yc %>% filter(name=="fit.f1") %>%
      mutate(sov="YR:CLT") %>% select(sov, Pr_Chisq = starts_with("Pr..Chisq."))

    #null model for LC:CLT

    fit.f1lc<-lmer(Trait~ 1 + (1|YR)  + (1|LC) + (1|CLT) + (1|YR:LC) +
                     (1|YR:CLT) +
                     (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance

    anova1lc <-anova(fit.f1,fit.f1lc)

    #convert anova into data frame

    anova1lc <- data.frame(anova1lc)

    #convert row names into column

    anova1lc$name <- rownames(anova1lc)

    #drop row names

    rownames(anova1lc) <- NULL
    anova1lc <- anova1lc %>% filter(name=="fit.f1") %>%
      mutate(sov="LC:CLT") %>% select(sov, Pr_Chisq = starts_with("Pr..Chisq."))

    #null model for YR:LC:CLT

    fit.f1ylc<-lmer(Trait~ 1 + (1|YR)  + (1|LC) + (1|CLT) + (1|YR:LC) +
                      (1|YR:CLT) +
                      (1|LC:CLT) +  (1|RPid), data=tempa())

    #level of significance

    anova1ylc <-anova(fit.f1,fit.f1ylc)

    #convert anova into data frame

    anova1ylc <- data.frame(anova1ylc)

    #convert row names into column

    anova1ylc$name <- rownames(anova1ylc)

    #drop row names

    rownames(anova1ylc) <- NULL
    anova1ylc <- anova1ylc %>% filter(name=="fit.f1") %>%
      mutate(sov="YR:LC:CLT") %>% select(sov, Pr_Chisq = starts_with("Pr..Chisq."))

    #null model for RP

    fit.f1r<-lmer(Trait~ 1 + (1|YR)  + (1|LC) + (1|CLT) + (1|YR:LC) +
                    (1|YR:CLT) +
                    (1|LC:CLT) + (1|YR:LC:CLT) , data=tempa())

    #level of significance

    anova1r <-anova(fit.f1,fit.f1r)

    #convert anova into data frame

    anova1r <- data.frame(anova1r)

    #convert row names into column

    anova1r$name <- rownames(anova1r)

    #drop row names

    rownames(anova1r) <- NULL
    anova1r <- anova1r %>% filter(name=="fit.f1") %>%
      mutate(sov="RPid") %>% select(sov, Pr_Chisq = starts_with("Pr..Chisq."))

    #Merge anova and level of significance

    anova1 <- bind_rows(anova1y, anova1l)%>% bind_rows(anova1c)%>%
      bind_rows(anova1yl)%>% bind_rows(anova1yc)%>%
      bind_rows(anova1r)%>% bind_rows(anova1lc)%>%bind_rows(anova1ylc)
    anova1 <- as.data.frame(anova1)

    #Merge final output

    anova_randall <- variance1%>% left_join(anova1 , by ="sov")
    anova_randall$Pr_Chisq[anova_randall$stddev == 0] <- NA
    anova_randall
  }
)


  BLUP_CLT_case1 <- eventReactive(input$case1,{

    # Fit model
    fit.f1 <- lmer(Trait ~ 1 + (1|YR) + (1|LC) + (1|CLT) +
                     (1|YR:LC) + (1|YR:CLT) + (1|LC:CLT) +
                     (1|YR:LC:CLT) + (1|RPid), data = tempa())

    # Extract BLUPs for CLT
    blup_raw <- ranef(fit.f1)$CLT %>%
      tibble::rownames_to_column("Genotype") %>%
      rename(BLUP_component = `(Intercept)`)

    # Add fixed effect intercept to compute full BLUP estimate
    fixef_intercept <- fixef(fit.f1)[["(Intercept)"]]

    BLUP_CLT <- blup_raw %>%
      mutate(BLUP = BLUP_component + fixef_intercept) %>%
      select(Genotype, BLUP)

  })

  observeEvent(input$case1,{
    input$case1
    isolate({
      output$anositable1 <- renderTable({
        anosi_randall()
      },  digits=4)
      output$anositable2 <-renderTable({
        BLUP_CLT_case1()
      },  digits=4)
    })
  })

  ##         ANOSI Case 2: CLT, YR and LC - Fixed; RP - Random          ##

  anosi_f2a <- eventReactive(input$case2,{

    fit.f2<-lmer(Trait~ YR*LC*CLT + (1|RPid), data=tempa())

    #model summary

    summary2 <- summary(fit.f2)

    ##variance of random factors

    variance2 <- as.data.frame(summary2$varcor)

    #drop row names

    rownames(variance2) <- NULL
    variance2 <- variance2 %>% select (-var1, -var2) %>%
      rename(sov=grp, Mean_Sq=vcov, Stddev=sdcor)

    #Type 3  test of hypothesis

    # set contrasts
    options(contrasts = c("contr.sum", "contr.poly"))

    anova.f2t3 <- as.data.frame(anova(fit.f2))

    #convert row names into column

    anova.f2t3$name <- rownames(anova.f2t3)

    #drop row names

    rownames(anova.f2t3) <- NULL

    #Type 3  test of hypothesis

    # anova.f2t2 <- anova(fit.f2, type="marginal", test="F")

    #model fitness

    # anovacase2 <- plot(fit.f2,
    #                    main="Model fitness Case 2: CLT, YR and LC - Fixed; RP - Random",
    #                    xlab="Predicated Value", ylab="Residual")

    #level of significance: "KR" is implemented corresponding to
    #the Kenward-Rogers approximation for degrees of freedom

    anova_f2t3 <- mixed(Trait~ YR*LC*CLT + (1|RPid), data=tempa(),
                        method = "KR",  # Kenward-Roger (default)
                        expand_re = TRUE)

    anova_f2t3 <- as.data.frame(anova_f2t3$anova_table)

    #convert row names into column

    anova_f2t3$name <- rownames(anova_f2t3)

    #drop row names

    rownames(anova_f2t3) <- NULL

    anova_f2t3 <- anova_f2t3 %>% select(name, Prob_F = starts_with("Pr(>F)"), Df = starts_with("num DF"))

    #final output for case 2: CLT, YR and LC - Fixed

    anova_f2 <- anova.f2t3 %>% left_join(anova_f2t3, by = "name")%>%
      rename(sov = name)%>%
      select(sov,Df,Sum_Sq=starts_with("Sum Sq"),
             Mean_Sq = starts_with("Mean Sq"),
             F_value=starts_with("F value"), Prob_F)

    anova_f2 <- anova_f2 %>% bind_rows(variance2)
    anova_f2a <- as.data.frame(anova_f2)
})

  observeEvent(input$case2,{
    input$case2
    isolate({
      output$anositable1 <- renderTable({
        anosi_f2a()
      },  digits=4)
      output$anositable2 <-renderTable({
        NULL
      },  digits=4)
    })
  })

  ##         ANOSI CASE 3: CLT - Fixed; YR, LC and RP - Random          ##

  anosi_f3 <- eventReactive(input$case3,{
    fit.f3<-lmer(Trait~ 1 + (1|YR)  + (1|LC) + CLT + (1|YR:LC) +
                   (1|YR:CLT) +
                   (1|LC:CLT) + (1|YR:LC:CLT) + (1|RPid), data=tempa())


    #Type 3 test of hypothesis

    # set contrasts
    options(contrasts = c("contr.sum", "contr.poly"))

    anova.f3t3 <- as.data.frame(anova(fit.f3))

    #convert rownames into column

    anova.f3t3$name <- rownames(anova.f3t3)

    #drop rownames

    rownames(anova.f3t3) <- NULL

    #model fitness

    # anovacase3 <- plot(fit.f3,
    #                    main="Model fitness Case 3: CLT - Fixed; YR, LC and RP - Random",
    #                    xlab="Predicated Value", ylab="Residual")

    #p value for fixed effects

    anova_f3t3 <- mixed(Trait~ 1 + (1|YR)  + (1|LC) + CLT + (1|YR:LC) +
                          (1|YR:CLT) + (1|LC:CLT) + (1|YR:LC:CLT) +
                          (1|RPid), data=tempa(),
                          method = "KR",  # Kenward-Roger (default)
                          expand_re = TRUE)

    anova_f3t3 <- as.data.frame(anova_f3t3$anova_table)

    #convert row names into column

    anova_f3t3$name <- rownames(anova_f3t3)

    #drop row names

    rownames(anova_f3t3) <- NULL

    anova_f3t3 <- anova_f3t3 %>% select(name, Prob_F = starts_with("Pr(>F)"),Df = starts_with("num DF"))

    #final output of case 3 for fixed effect - CLT

    anova_f3 <- anova.f3t3 %>% left_join(anova_f3t3, by = "name")%>%
      rename(sov = name)%>%
      select(sov,Df,Sum_Sq=starts_with("Sum Sq"),
             Mean_Sq = starts_with("Mean Sq"),
             F_value=starts_with("F value"), Prob_F)
  })


  anosi_cfix <- eventReactive(input$case3,{

    fit.f3<-lmer(Trait~ 1 + (1|YR)  + (1|LC) + CLT + (1|YR:LC) +
                   (1|YR:CLT) +
                   (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #model summary

    summary3 <- summary(fit.f3)

    ##variance of random factors

    variance3 <- as.data.frame(summary3$varcor)

    #drop row names

    rownames(variance3) <- NULL
    variance3 <- variance3 %>% select (-var1, -var2) %>%
      rename(sov=grp, Variance=vcov, Stddev=sdcor)

    #level of significance for random effects
    #null model for CLT

    fit.f3c <- lmer(Trait~ 1 + (1|YR) + (1|LC) + (1|YR:LC) + (1|YR:LC) +
                      (1|YR:CLT) + (1|LC:CLT) + (1|YR:LC:CLT) +
                      (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova3c <- anova_lrt(anova3c,fit.f3,fit.f3c,"CLT")

    #null model for LC

    fit.f3l <- lmer(Trait~ 1 + CLT + (1|YR) + (1|YR:LC) + (1|YR:CLT) +
                      (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt
    anova3l <- anova_lrt(anova3l,fit.f3,fit.f3l,"LC")

    #null model for YR

    fit.f3y <- lmer(Trait~ 1 + CLT + (1|LC) + (1|YR:LC) + (1|YR:CLT) +
                      (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova3y <- anova_lrt(anova3y,fit.f3,fit.f3y,"YR")

    #null model for YR:LC

    fit.f3yl <- lmer(Trait~ 1 + CLT + (1|LC) + (1|YR) + (1|YR:CLT) +
                       (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova3yl <- anova_lrt(anova3yl,fit.f3,fit.f3yl,"YR:LC")

    #null model for YR:CLT

    fit.f3yc <- lmer(Trait~ 1 + CLT + (1|LC) + (1|YR) + (1|YR:LC) +
                       (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova3yc <- anova_lrt(anova3yc,fit.f3,fit.f3yc,"YR:CLT")

    #null model for LC:CLT

    fit.f3lc <- lmer(Trait~ 1 + CLT + (1|LC) + (1|YR) + (1|YR:LC) +
                       (1|YR:CLT) +
                       (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova3lc <- anova_lrt(anova3lc,fit.f3,fit.f3lc,"LC:CLT")

    #null model for YR:LC:CLT

    fit.f3ylc <- lmer(Trait~ 1 + CLT + (1|LC) + (1|YR) + (1|YR:LC) +
                        (1|YR:CLT) +
                        (1|LC:CLT) + (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova3ylc <- anova_lrt(anova3ylc,fit.f3,fit.f3ylc,"YR:LC:CLT")

    #null model for RPid or replication

    fit.f3r <- lmer(Trait~ 1 + CLT + (1|LC) + (1|YR) + (1|YR:LC) +
                      (1|YR:CLT) +
                      (1|LC:CLT) + (1|YR:LC:CLT), data=tempa())

    #level of significance
    #call function anova_lrt

    anova3r <- anova_lrt(anova3r,fit.f3,fit.f3r,"RPid")

    #Merge anova and level of significance

    anova3 <- bind_rows(anova3y, anova3l)%>% bind_rows(anova3yl)%>%
      bind_rows(anova3yc)%>% bind_rows(anova3r)%>%
      bind_rows(anova3lc)%>%bind_rows(anova3ylc)
    anova3 <- as.data.frame(anova3)

    #Merge final output

    anova_cfix <- variance3%>% left_join(anova3 , by ="sov")
    anova_cfix$Pr_Chisq[anova_cfix$stddev == 0] <- NA
    anova_cfix
  })

  observeEvent(input$case3,{
    input$case3
    isolate({
      output$anositable1 <- renderTable({
        anosi_f3()
      },  digits=4)
      output$anositable2 <-renderTable({
        anosi_cfix()
      },  digits=4)
    })
  })

  ##         ANOSI Case 4: LC - Fixed; YR, CLT and RP - Random          ##
  anosi_f4 <- eventReactive(input$case4,{

    fit.f4 <- lmer(Trait~ 1 +  LC + (1|YR) + (1|CLT) + (1|YR:LC) +
                     (1|YR:CLT) +
                     (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())


    #Type 3 test of hypothesis

    # set contrasts
    options(contrasts = c("contr.sum", "contr.poly"))

    anova.f4t3 <- as.data.frame(anova(fit.f4))
    #convert row names into column

    anova.f4t3$name <- rownames(anova.f4t3)

    #drop row names

    rownames(anova.f4t3) <- NULL

    #model fitness
#
#     anovacase4 <- plot(fit.f4,
#                        main="Model fitness Case 4: LC - Fixed; YR, CLT and RP - Random",
#                        xlab="Predicated Value", ylab="Residual")

    #level of significance for fixed effects

    anova_f4t3 <- mixed(Trait~ 1 + LC + (1|YR)  + (1|CLT) + (1|YR:LC) +
                          (1|YR:CLT) +
                          (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa(),
                          method = "KR",  # Kenward-Roger (default)
                          expand_re = TRUE)

    anova_f4t3 <- as.data.frame(anova_f4t3$anova_table)

    #convert rownames into column

    anova_f4t3$name <- rownames(anova_f4t3)

    #drop rownames

    rownames(anova_f4t3) <- NULL

    anova_f4t3 <- anova_f4t3 %>% select(name, Prob_F = starts_with("Pr(>F)"), Df = starts_with("num DF"))

    #final output of case 4 for fixed effect - LC

    anova_f4 <- anova.f4t3 %>% left_join(anova_f4t3, by = "name")%>%
      rename(sov = name)%>%
      select(sov,Df,Sum_Sq=starts_with("Sum Sq"),
             Mean_Sq = starts_with("Mean Sq"),
             F_value=starts_with("F value"), Prob_F)
  })

  anosi_lfix <- eventReactive(input$case4,{

    fit.f4 <- lmer(Trait~ 1 +  LC + (1|YR) + (1|CLT) + (1|YR:LC) +
                     (1|YR:CLT) +
                     (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #model summary

    summary4 <- summary(fit.f4)

    ##variance of random factors

    variance4 <- as.data.frame(summary4$varcor)

    # drop row names

    rownames(variance4) <- NULL
    variance4 <- variance4 %>% select (-var1, -var2) %>%
      rename(sov=grp, Variance=vcov, Stddev=sdcor)


    #level of significance of random effect
    #null model for CLT

    fit.f4c <- lmer(Trait~ 1 + LC + (1|YR) +  (1|YR:LC) + (1|YR:LC) +
                      (1|YR:CLT)+
                      (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova4c <- anova_lrt(anova4c,fit.f4,fit.f4c,"CLT")

    #null model for LC

    fit.f4l <- lmer(Trait~ 1 + (1|CLT) + (1|YR) + (1|YR:LC) + (1|YR:CLT) +
                      (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova4l <- anova_lrt(anova4l,fit.f4,fit.f4l,"LC")

    #null model for YR

    fit.f4y <- lmer(Trait~ 1 + LC + (1|CLT) + (1|YR:LC) + (1|YR:CLT) +
                      (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova4y <- anova_lrt(anova4y,fit.f4,fit.f4y,"YR")

    #null model for YR:LC

    fit.f4yl <- lmer(Trait~ 1 + LC + (1|CLT) + (1|YR) + (1|YR:CLT) +
                       (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova4yl <- anova_lrt(anova4yl,fit.f4,fit.f4yl,"YR:LC")

    #null model for YR:CLT

    fit.f4yc <- lmer(Trait~ 1 + LC + (1|CLT) + (1|YR) + (1|YR:LC) +
                       (1|LC:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova4yc <- anova_lrt(anova4yc,fit.f4,fit.f4yc,"YR:CLT")

    #null model for LC:CLT

    fit.f4lc <- lmer(Trait~ 1 + LC + (1|CLT) + (1|YR) + (1|YR:LC) +
                       (1|YR:CLT) +
                       (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova4lc <- anova_lrt(anova4lc,fit.f4,fit.f4lc,"LC:CLT")

    #null model for YR:LC:CLT

    fit.f4ylc <- lmer(Trait~ 1 + LC + (1|CLT) + (1|YR) + (1|YR:LC) +
                        (1|YR:CLT) +
                        (1|LC:CLT) + (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova4ylc <- anova_lrt(anova4ylc,fit.f4,fit.f4ylc,"YR:LC:CLT")

    #null model for RPid or replication

    fit.f4r <- lmer(Trait~ 1 + LC + (1|CLT) + (1|YR) + (1|YR:LC) +
                      (1|YR:CLT) +
                      (1|LC:CLT) + (1|YR:LC:CLT), data=tempa())

    #level of significance
    #call function anova_lrt

    anova4r <- anova_lrt(anova4r,fit.f4,fit.f4r,"RPid")

    #Merge anova and level of significance

    anova4 <- bind_rows(anova4c, anova4y)%>% bind_rows(anova4yl)%>%
      bind_rows(anova4yc)%>% bind_rows(anova4r)%>%
      bind_rows(anova4lc)%>%bind_rows(anova4ylc)
    anova4 <- as.data.frame(anova4)

    #Merge final output

    anova_lfix <- variance4%>% left_join(anova4 , by ="sov")
    anova_lfix$Pr_Chisq[anova_lfix$stddev == 0] <- NA
    anova_lfix
  })

  observeEvent(input$case4,{
    input$case4
    isolate({
      output$anositable1 <- renderTable({
        anosi_f4()
      },  digits=4)
      output$anositable2 <-renderTable({
        anosi_lfix()
      },  digits=4)
    })
  })

  ##        ANOSI Case 5: CLT and LC - Fixed; YR, and RP - Random       ##

  anosi_f5 <- eventReactive(input$case5,{

    fit.f5 <- lmer(Trait~ 1 +  CLT + LC + LC:CLT + (1|YR) + (1|YR:LC) +
                     (1|YR:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #Type 3 test of hypothesis

    # set contrasts
    options(contrasts = c("contr.sum", "contr.poly"))

    anova.f5t3 <- as.data.frame(anova(fit.f5))

    #convert row names into column

    anova.f5t3$name <- rownames(anova.f5t3)

    #drop row names

    rownames(anova.f5t3) <- NULL


    #model fitness

    # anovacase5 <- plot(fit.f5,
    #                    main="Model fitness Case 5: CLT and LC - Fixed; YR and RP - Random",
    #                    xlab="Predicated Value", ylab="Residual")

    #level of significance for fixed effects

    anova_f5t3 <- mixed(Trait~ 1 +  CLT + LC + LC:CLT + (1|YR) +
                          (1|YR:LC) + (1|YR:CLT) + (1|YR:LC:CLT) +
                          (1|RPid), data=tempa(),
                          method = "KR",  # Kenward-Roger (default)
                          expand_re = TRUE)

    anova_f5t3 <- as.data.frame(anova_f5t3$anova_table)

    #convert row names into column

    anova_f5t3$name <- rownames(anova_f5t3)

    #drop row names

    rownames(anova_f5t3) <- NULL

    anova_f5t3 <- anova_f5t3 %>% select(name, Prob_F = starts_with("Pr(>F)"), Df = starts_with("num DF"))

    #final output of case 5 for fixed effect - CLT and LC

    anova_f5 <- anova.f5t3 %>% left_join(anova_f5t3, by = "name")%>%
      rename(sov = name)%>%
      select(sov,Df,Sum_Sq=starts_with("Sum Sq"),
             Mean_Sq = starts_with("Mean Sq"),
             F_value=starts_with("F value"), Prob_F)
  })

  anosi_clfix <- eventReactive(input$case5,{

    fit.f5 <- lmer(Trait~ 1 +  CLT + LC + LC:CLT + (1|YR) + (1|YR:LC) +
                     (1|YR:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #model summary

    summary5 <- summary(fit.f5)

    ##variance of random factors

    variance5 <- as.data.frame(summary5$varcor)

    #drop row names

    rownames(variance5) <- NULL
    variance5 <- variance5 %>% select (-var1, -var2) %>%
      rename(sov=grp, Variance=vcov, Stddev=sdcor)

    #level of significance for random effects
    #null model for CLT

    fit.f5c <- lmer(Trait~ 1 +  LC + LC:CLT + (1|YR) + (1|YR:LC) +
                      (1|YR:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova5c <- anova_lrt(anova5c,fit.f5,fit.f5c,"CLT")

    #null model for LC

    fit.f5l <- lmer(Trait~ 1 +  CLT + LC:CLT + (1|YR) + (1|YR:LC) +
                      (1|YR:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova5l <- anova_lrt(anova5l,fit.f5,fit.f5l,"LC")

    #null model for YR

    fit.f5y <- lmer(Trait~ 1 +  CLT + LC + LC:CLT + (1|YR:LC) +
                      (1|YR:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova5y <- anova_lrt(anova5y,fit.f5,fit.f5y,"YR")

    #null model for YR:LC

    fit.f5yl <- lmer(Trait~ 1 +  CLT + LC + LC:CLT + (1|YR) +
                       (1|YR:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova5yl <- anova_lrt(anova5yl,fit.f5,fit.f5yl,"YR:LC")

    #null model for YR:CLT

    fit.f5yc <- lmer(Trait~ 1 +  CLT + LC + LC:CLT + (1|YR) + (1|YR:LC) +
                       (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova5yc <- anova_lrt(anova5yc,fit.f5,fit.f5yc,"YR:CLT")

    #null model for LC:CLT

    fit.f5lc <- lmer(Trait~ 1 +  CLT + LC + (1|YR) + (1|YR:LC) +
                       (1|YR:CLT) + (1|YR:LC:CLT) +  (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova5lc <- anova_lrt(anova5lc,fit.f5,fit.f5lc,"LC:CLT")

    #null model for YR:LC:CLT

    fit.f5ylc <- lmer(Trait~ 1 +  CLT + LC + LC:CLT + (1|YR) + (1|YR:LC) +
                        (1|YR:CLT) + (1|RPid), data=tempa())

    #level of significance
    #call function anova_lrt

    anova5ylc <- anova_lrt(anova5ylc,fit.f5,fit.f5ylc,"YR:LC:CLT")

    #null model for RPid or replication

    fit.f5r <- lmer(Trait~ 1 +  CLT + LC + LC:CLT + (1|YR) + (1|YR:LC) +
                      (1|YR:CLT) + (1|YR:LC:CLT) , data=tempa())

    #level of significance
    #call function anova_lrt

    anova5r <- anova_lrt(anova5r,fit.f5,fit.f5r,"RPid")

    #Merge anova and level of significance

    anova5 <- bind_rows(anova5y, anova5yl)%>%
      bind_rows(anova5yc)%>% bind_rows(anova5r)%>%
      bind_rows(anova5ylc)
    anova5 <- as.data.frame(anova5)

    #Merge final output

    anova_clfix <- variance5%>% left_join(anova5 , by ="sov")
    anova_clfix$Pr_Chisq[anova_clfix$stddev == 0] <- NA
    anova_clfix
  })

  observeEvent(input$case5,{
    input$case5
    isolate({
      output$anositable1 <- renderTable({
        anosi_f5()
      },  digits=4)
      output$anositable2 <-renderTable({
        anosi_clfix()
      },  digits=4)
    })
  })

  ########################################################################
  #   Compute Mean and CV of genotype, location, year, rep, environment  #
  ########################################################################

  mean_ge <- eventReactive(input$trait_GxE,{
    mean_ge <- tempa2() %>%
      group_by (CLT, ENV) %>%
      summarize (Trait = mean(Trait,na.rm=FALSE))

    mean_ge1 <- mean_ge %>%
      spread (ENV, Trait) #transpose using library tidyr

    mean_ge2 <- as.data.frame(mean_ge1)
  }
  )

  observeEvent(input$trait_GxE,{
    input$trait_GxE
    isolate({
      output$valuetable1 <- renderTable({
        mean_ge()
      },  digits=4)
      output$valuetable2 <-renderTable({
        NULL
      },  digits=4)
    })
  })

  mean_gy <- eventReactive(input$trait_GxY,{
    mean_gy <- tempa2() %>%
      group_by (CLT, YR) %>%
      summarize (Trait = mean(Trait,na.rm=FALSE))

    mean_gy1 <- mean_gy %>%
      spread (YR, Trait) #transpose using library tidyr

    mean_gy2 <- as.data.frame(mean_gy1)
  })

  observeEvent(input$trait_GxY,{
    input$trait_GxY
    isolate({
      output$valuetable1 <- renderTable({
        mean_gy()
      },  digits=4)
      output$valuetable2 <-renderTable({
        NULL
      },  digits=4)
    })
  })

  mean_gl <- eventReactive(input$trait_GxL,{
    mean_gl <- tempa2() %>%
      group_by (CLT, LC) %>%
      summarize (Trait = mean(Trait,na.rm=FALSE))

    mean_gl1 <- mean_gl %>%
      spread (LC, Trait) #transpose using library tidyr

    mean_gl2 <- as.data.frame(mean_gl1)
  })

  observeEvent(input$trait_GxL,{
    input$trait_GxL
    isolate({
      output$valuetable1 <- renderTable({
        mean_gl()
      },  digits=4)
      output$valuetable2 <-renderTable({
        NULL
      },  digits=4)
    })
  })

  mean_ly <- eventReactive(input$trait_LxY,{
    mean_ly <- tempa2() %>%
      group_by (LC, YR) %>%
      summarize (Trait = mean(Trait,na.rm=FALSE))

    mean_ly1 <- mean_ly %>%
      spread (YR, Trait) #transpose using library tidyr

    mean_ly2 <- as.data.frame(mean_ly1)
  })

  observeEvent(input$trait_LxY,{
    input$trait_LxY
    isolate({
      output$valuetable1 <- renderTable({
        mean_ly()
      },  digits=4)
      output$valuetable2 <-renderTable({
        NULL
      },  digits=4)
    })
  })

  mean_gly <- eventReactive(input$trait_GxLxY,{
    mean_gly <- tempa2() %>%
      group_by (CLT, LC, YR) %>%
      summarize (Trait = mean(Trait,na.rm=FALSE))

    mean_gly1 <- mean_gly %>%
      spread (LC, Trait) #transpose using library tidyr

    mean_gly2 <- as.data.frame(mean_gly1)
  })

  observeEvent(input$trait_GxLxY,{
    input$trait_GxLxY
    isolate({
      output$valuetable1 <- renderTable({
        mean_gly()
      },  digits=4)
      output$valuetable2 <-renderTable({
        NULL
      },  digits=4)
    })
  })

  mean_g <- eventReactive(input$trait_G,{
    mean_g <- tempa2() %>%
      group_by (CLT ) %>%
      summarize (Trait_Mean = mean(Trait,na.rm=FALSE))

    mean_g1 <- as.data.frame(mean_g)
  })

  observeEvent(input$trait_G,{
    input$trait_G
    isolate({
      output$valuetable1 <- renderTable({
        mean_g()
      },  digits=4)
      output$valuetable2 <-renderTable({
        NULL
      },  digits=4)
    })
  })

  cv_gl <- eventReactive(input$cv,{
    cv_gl <- tempa2() %>%
      group_by (CLT, LC) %>%
      summarise (Trait_m = mean(Trait,na.rm=FALSE) ,
                 Trait_s = sd(Trait,na.rm=FALSE)) %>%
      mutate (Trait = (Trait_s/Trait_m)*100) %>%
      # CV
      select (-Trait_m, -Trait_s)

    cv_gl1 <- cv_gl %>%
      spread (LC, Trait) #transpose using library tidyr
    cv_gl2 <- as.data.frame(cv_gl1)
  })

  observeEvent(input$cv,{
    input$cv
    isolate({
      output$valuetable1 <- renderTable({
        cv_gl()
      },  digits=4)
      output$valuetable2 <-renderTable({
        NULL
      },  digits=4)
    })
  })

  ########################################################################
  #               Compute UniGene stability statistics                   #
  ########################################################################

  univariate <- eventReactive(input$stability_stats, {

    # cache data
    dst02 <- dst02()
    tempa2 <- tempa2()

    fit_model <- dst02 %>%
      group_by(CLT) %>%
      nest() %>%  # Nest data by cultivar
      mutate(
        model = map(data, ~lm(Trait ~ ENVTrait + ENV + RP, data = .x))  # Fit linear model for each cultivar
      )

    # Extract parameter estimates
    paramlm <- fit_model %>%
      mutate(tidied = map(model, tidy)) %>%  # Extract coefficients
      unnest(tidied)  # Unnest into a data frame

    # Extract model statistics (R², AIC, etc.)
    glancelm <- fit_model %>%
      mutate(glanced = map(model, glance)) %>%
      unnest(glanced)

    # Extract augmented data (fitted values, residuals, etc.)
    augmentlm <- fit_model %>%
      mutate(augmented = map(model, augment)) %>%
      unnest(augmented)

    # Perform ANOVA for each model and combine
    outmsed <- lapply(fit_model$model, anova)
    outmsed2 <- as.data.frame(do.call(rbind, outmsed))  # Convert list to data frame

    # Move row names into a column
    outmsed2$SOV <- rownames(outmsed2)
    rownames(outmsed2) <- NULL  # Drop row names

    # Remove numeric characters from SOV names
    outmsed2 <- outmsed2 %>% mutate(SOV = gsub("\\d+", "", SOV))

    # Extract unique cultivar names
    genotypes <- dst02 %>% select(CLT) %>% distinct(CLT) %>% arrange(CLT)

    # Stack genotypes 4 times to match rows of outmsed2
    genotypes1 <- genotypes %>%
      bind_rows(genotypes) %>%
      bind_rows(genotypes) %>%
      bind_rows(genotypes) %>%
      arrange(CLT)

    # Attach cultivar list to ANOVA results
    outmsed3 <- as.data.frame(outmsed2 %>% bind_cols(genotypes1))

    # Prepare Mean Squares output, filter out RP
    outmsed4 <- outmsed3 %>%
      select(CLT, SOV, MS = starts_with("Mean")) %>%  # Rename columns
      filter(SOV != "RP")

    # Spread Mean Squares into columns by source of variation
    MSDS <- outmsed4 %>%
      spread(SOV, MS) %>%
      arrange(CLT)

    # Spread degrees of freedom for F-tests
    FDS3 <- outmsed3 %>%
      filter(SOV != "RP") %>%
      select(CLT, SOV, Df) %>%
      spread(SOV, Df) %>%
      rename(
        DF_ENVTrait = ENVTrait,
        DF_Residuals = Residuals,
        DF_ENV = ENV
      )

    # Subset regression coefficients for ENVTrait term
    REGCOEFGS <- paramlm %>% filter(term == "ENVTrait") %>%
      select(-statistic, -p.value)

    # Merge MS, DF, and regression coefficients
    slope <- MSDS %>%
      inner_join(REGCOEFGS, by = "CLT") %>%
      inner_join(FDS3, by = "CLT") %>%
      rename(
        MSE = Residuals,
        LREGMS = ENVTrait,
        DEVLMS = ENV,
        BI = estimate,
        STDERR = std.error
      )

    # Calculate test statistics and p-values for slope and deviation
    slope1 <- slope %>%
      mutate(
        T_H01 = (BI - 1) / STDERR,  # Test slope = 1
        PT_H01 = 2 * pt(-abs(T_H01), DF_Residuals),
        F_DEVREG = DEVLMS / MSE,      # Test deviation from regression
        PF_HO0 = 1 - pf(F_DEVREG, DF_ENV, DF_Residuals)
      )

    # Add significance labels
    slope2 <- slope1 %>%
      mutate(
        SIG_SLOPE = ifelse(PT_H01 <= 0.001, "***",
                           ifelse(PT_H01 <= 0.01, "**",
                                  ifelse(PT_H01 <= 0.05, "*", ""))),
        SIG_DEVREG = ifelse(PF_HO0 <= 0.001, "***",
                            ifelse(PF_HO0 <= 0.01, "**",
                                   ifelse(PF_HO0 <= 0.05, "*", "")))
      )

    # Prepare final regression output table
    univariate2 <- slope2 %>%
      mutate(
        SLOPE = paste(BI, SIG_SLOPE, sep = ""),
        DEVREG = paste(DEVLMS, SIG_DEVREG, sep = "")
      ) %>%
      select(CLT, SLOPE, DEVREG)

    #  Compute stability statistics: Shukla, Wricke Ecovalence, Kang's YS  #

    # Count total number of replicates
    repno <- tempa2 %>%
      summarise(total_rep = n_distinct(RP))

    # Summarize genotype mean across locations
    dstgl <- tempa2 %>%
      group_by(CLT, LC) %>%
      summarize(Trait = mean(Trait, na.rm = TRUE))

    # Spread location means into wide format
    dstgl1 <- dstgl %>% spread(LC, Trait)

    # Convert to data frame and remove structure info row
    dstgl2 <- as.data.frame(dstgl1)

    # Set row names from first column
    rownames(dstgl2) <- dstgl2[, 1]
    shukla <- dstgl2[, -1]

    # Fit GLM to get error mean square
    tempa5 <- glm(Trait ~ LC + YR + LC:YR + RP %in% (LC:YR) + CLT + CLT:LC +
                    CLT:YR + CLT:LC:YR,
                  family = gaussian, data = tempa2)

    # Get model summary
    summary1 <- summary.glm(tempa5)

    # Extract error sum of squares and degrees of freedom
    error_ss1 <- as.data.frame(summary1$deviance) %>%
      rename(Deviance = starts_with("summary"))
    error_df1 <- as.data.frame(summary1$df.residual) %>%
      rename(Df = starts_with("summary"))

    # Compute mean square error
    mse <- as.data.frame(error_ss1 / error_df1) %>%
      rename(MS = Deviance)

    # Compute stability statistics
    univariate1a <- stability.par(shukla,
                                  rep = repno$total_rep,
                                  MSerror = mse$MS,
                                  alpha = 0.1,
                                  main = "Genotype")

    # Extract statistics
    univariate1b <- univariate1a$statistics
    univariate1b$genotype <- rownames(univariate1b)  # Add genotype column
    rownames(univariate1b) <- NULL  # Remove row names
    names(univariate1b)[2] <- "sigma"
    names(univariate1b)[3] <- "significance_sigma"
    names(univariate1b)[4] <- "ssquare"
    names(univariate1b)[5] <- "significance_s2"

    # Extract stability measures
    univariate1c <- univariate1a$stability
    univariate1c$genotype <- rownames(univariate1c)
    rownames(univariate1c) <- NULL
    names(univariate1c)[8] <- "legend"

    # Merge stability components and format final table
    univariate1d <- univariate1b %>%
      inner_join(univariate1c, by = "genotype") %>%
      select(-Yield:-Stab.rating) %>%
      select(CLT = genotype, Mean, sigma,
             significance_sigma, ssquare,
             significance_s2, Ecovalence, YSi, legend)

    # Merge regression and stability statistics
    univariate <- univariate2 %>%
      inner_join(univariate1d, by = "CLT") %>%
      mutate(
        Shukla_SIGMA = paste(sigma, significance_sigma, ""),
        Shukla_SIGMA_SQUARE = paste(ssquare, significance_s2, ""),
        YS_Kang = paste(YSi, legend, "")
      ) %>%
      select(Genotype = CLT, Mean, SLOPE, DEVREG,
             Shukla_SIGMA, Shukla_SIGMA_SQUARE,
             Ecovalence, YS_Kang)
  })

  observeEvent(input$stability_stats,{
    input$stability_stats
    isolate({
      output$valuetable1 <- renderTable({
        univariate()
      },  digits=4)
      output$valuetable2 <-renderTable({
        NULL
      },  digits=4)
    })
  })

  dst02 <- eventReactive(input$trait_submit,{
    dsterm <- tempa2() %>%
      group_by (ENV, RP, YR, LC) %>%
      summarize (ENVTrait = mean(Trait,na.rm=FALSE))
    dst02 <- tempa2() %>%
      left_join(dsterm, by=c("ENV", "RP")) %>% #Left join on multiple columns
      arrange (CLT) %>%
      rename (YR= YR.x, LC = LC.x )
  })


  ########################################################################
  ##        Compute location statistics - genotype F ratio across       ##
  ##        location and environment; location correlation              ##
  ########################################################################

  #Location values
  #F-value of genotype across location
  LocationValue <- eventReactive(input$F_ratio, {

    data_input <- tempa2()

    # Fit model by location
    fit_model <- data_input %>%
      group_by(LC) %>%
      group_split() %>%
      map(~ anova(lm(Trait ~ CLT + YR + CLT:YR + RP %in% YR, data = .x))) %>%
      set_names(unique(data_input$LC))

    # Combine ANOVA output into data frame
    anova_list <- map2(fit_model, names(fit_model), ~ {
      df <- as.data.frame(.x)
      df$SOV <- rownames(df)
      df$LC <- .y
      rownames(df) <- NULL
      df
    })

    anova_df <- bind_rows(anova_list) %>%
      mutate(SOV = gsub("\\d+", "", SOV))  # Remove numeric suffixes

    # Extract F-value for genotype
    F_values <- anova_df %>%
      filter(SOV == "CLT") %>%
      group_by(LC) %>%
      summarise(FRatioGenotype = `F value`, .groups = "drop")

    # Genotype-location mean
    glc_mean <- data_input %>%
      group_by(CLT, LC) %>%
      summarise(glcmean = mean(Trait, na.rm = TRUE), .groups = "drop")

    g_mean <- data_input %>%
      group_by(CLT) %>%
      summarise(gmean = mean(Trait, na.rm = TRUE), .groups = "drop")

    lg_mean <- glc_mean %>%
      left_join(g_mean, by = "CLT")

    # Correlation between glcmean and gmean for each LC
    correlation_df <- lg_mean %>%
      group_by(LC) %>%
      summarise(
        Corr_Value = cor(glcmean, gmean),
        Pvalue = cor.test(glcmean, gmean)$p.value,
        .groups = "drop"
      ) %>%
      mutate(
        SIG_CORR = case_when(
          Pvalue <= 0.001 ~ "***",
          Pvalue <= 0.01 ~ "**",
          Pvalue <= 0.05 ~ "*",
          TRUE ~ ""
        ),
        LocCorrelation = paste0(round(Corr_Value, 3), SIG_CORR)
      ) %>%
      select(LC, LocCorrelation)

    # Location-wise trait mean
    loc_mean <- data_input %>%
      group_by(LC) %>%
      summarise(Mean = mean(Trait, na.rm = TRUE), .groups = "drop")

    # Final merged output
    LocationValue <- loc_mean %>%
      left_join(F_values, by = "LC") %>%
      left_join(correlation_df, by = "LC") %>%
      rename(Location = LC)

    return(LocationValue)
  })


  ########################################################################
  ###              F-value of genotype across environment                ##
  ########################################################################

  EnvironmentValue <- eventReactive(input$F_ratio, {

    # Get the main input dataset
    data_input <- tempa2()

    # Fit linear model Trait ~ CLT + RP for each environment
    model_list <- data_input %>%
      group_by(ENV) %>%
      group_split() %>%
      map(~ anova(lm(Trait ~ CLT + RP, data = .x))) %>%
      set_names(unique(data_input$ENV))

    # Extract ANOVA tables and label with ENV and SOV (Source of Variation)
    anova_df_list <- map2(model_list, names(model_list), ~ {
      df <- as.data.frame(.x)
      df$SOV <- gsub("\\d+", "", rownames(df))  # Clean row names
      df$ENV <- .y                              # Assign ENV identifier
      rownames(df) <- NULL
      df
    })

    # Combine all ANOVA tables into one data frame
    anova_df <- bind_rows(anova_df_list)

    # Extract F value for genotype (CLT) effect from each environment
    F_values <- anova_df %>%
      filter(SOV == "CLT") %>%
      group_by(ENV) %>%
      summarise(FRatioGenotype = `F value`, .groups = "drop")

    # Calculate genotype mean in each environment
    gen_env_mean <- data_input %>%
      group_by(CLT, ENV) %>%
      summarise(glcmean = mean(Trait, na.rm = TRUE), .groups = "drop")

    # Calculate genotype mean across all environments
    gen_mean <- data_input %>%
      group_by(CLT) %>%
      summarise(gmean = mean(Trait, na.rm = TRUE), .groups = "drop")

    # Merge genotype-by-environment mean with overall genotype mean
    #    Then compute correlation between them for each environment
    corr_df <- gen_env_mean %>%
      left_join(gen_mean, by = "CLT") %>%
      group_by(ENV) %>%
      summarise(
        Corr_Value = cor(glcmean, gmean),
        Pvalue = cor.test(glcmean, gmean)$p.value,
        .groups = "drop"
      ) %>%
      mutate(
        SIG_CORR = case_when(
          Pvalue <= 0.001 ~ "***",
          Pvalue <= 0.01 ~ "**",
          Pvalue <= 0.05 ~ "*",
          TRUE ~ ""
        ),
        EnvCorrelation = paste0(round(Corr_Value, 3), SIG_CORR)
      ) %>%
      select(ENV, EnvCorrelation)

    # Calculate trait mean for each environment
    env_mean <- data_input %>%
      group_by(ENV) %>%
      summarise(Mean = mean(Trait, na.rm = TRUE), .groups = "drop")

    # Get distinct list of environments from external source (for order)
    env_list <- dst02() %>%
      select(ENV) %>%
      distinct() %>%
      arrange(ENV)

    # Join all parts into the final output table
    EnvironmentValue <- env_list %>%
      left_join(env_mean, by = "ENV") %>%
      left_join(F_values, by = "ENV") %>%
      left_join(corr_df, by = "ENV")

    return(EnvironmentValue)
  })

  observeEvent(input$F_ratio,{
    input$F_ratio
    isolate({
      output$valuetable1 <- renderTable({
        LocationValue()
      },  digits=4)
      output$valuetable2 <-renderTable({
        EnvironmentValue()
      },  digits=4)
    })
  })

  ########################################################################
  ###               Compute cluster analysis of location               ###
  ########################################################################

  observeEvent(input$cluster,{
    output$cluster_dia <- renderPlot({

      #location cluster analysis
      #Euclidean distance
      #Ward Hierarchical Clustering
      #trait mean over location
      mean_l <- tempa2() %>% group_by (LC ) %>% summarize (Trait = mean(Trait,na.rm=FALSE))
      mean_l1 <- as.data.frame(mean_l)

      clusterdata <- mean_l1 %>% select (Trait)
      clusterdata <- na.omit(clusterdata)
      distance <- dist(clusterdata, method = "euclidean") # distance matrix
      hcluster <- hclust(d=distance, method="ward.D")
      input$cluster
      isolate({
        locationcluster <- plot(hcluster, labels=mean_l1$LC)# display dendogram
      })
    })
  })

  dataframeInput <- reactive({
    switch(input$dataframe,
           'ANOSI Case 1'=anosi_randall(),
           'ANOSI Case 2'=anosi_f2a(),
           'ANOSI Case 3'=anosi_cfix(),
           'ANOSI Case 4'=anosi_lfix(),
           'ANOSI Case 5'= anosi_clfix(),
           'mean_trait_g×e'=mean_ge(),
           'mean_trait_g×y'=mean_gy(),
           'mean_trait_g×l×y'=mean_gly(),
           'CV of Genotype×Location'=cv_gl(),
           'univariate stability statistics'=univariate(),
           'F ratio of genotype across location'=LocationValue(),
           'F ratio of genotype across environment'=EnvironmentValue()
    )
  })

  output$download<-downloadHandler(
    filename=function(){
     if (input$dataframe != 'Download All Results(only for .txt)')
      paste(input$dataframe, input$downformat, sep=".")
     else
      paste('RGxEStat_Output.txt')
    },
    content=function(file){
      if (input$dataframe != 'Download All Results(only for .txt)')
      {sep <- switch(input$downformat, "csv"=",","txt"="    \t", "doc"="    \t")
      # Write to a file specified by the 'file' argument
      if(input$downformat=='xlsx')
        write_xlsx(dataframeInput(), file)
      else
        write.table(as.matrix(dataframeInput()), file, sep = sep,
                    row.names = FALSE, quote = FALSE)}
      else
      {
        sink(file=file, append=FALSE, split=FALSE)

        cat("#############      Print date and time     #################\n\n")
        print(Sys.time())
        cat("\n########################################################################\n")
        cat("###           Section 1: Identify level of significance of           ###\n")
        cat("###              different effects, variances and BLUP               ###\n")
        cat("########################################################################\n\n")

        # ANOSI Case 1
        cat("## ANOSI Case 1: CLT, YR, LC and RP - All Random\n")
        cat("P values are generated using LRT-Likelihood Ratio Test via model comparison and anova\n\n")
        print(anosi_randall(), row.names = FALSE)
        cat("\n## BLUP: Best linear unbiased predictor value for random genotypes\n")
        print(BLUP_CLT_case1(), row.names = FALSE)

        # ANOSI Case 2
        cat("\n## ANOSI Case 2: CLT, YR and LC - Fixed; RP - Random\n")
        cat("P values computed using Kenward-Roger method (F ratio)\n")
        print(anosi_f2a(), row.names = FALSE)

        # ANOSI Case 3
        cat("\n## ANOSI Case 3: CLT - Fixed; YR, LC and RP - Random\n")
        print(anosi_f3(), row.names = FALSE)
        print(anosi_cfix(), row.names = FALSE)

        # ANOSI Case 4
        cat("\n## ANOSI Case 4: LC - Fixed; YR, CLT and RP - Random\n")
        print(anosi_f4(), row.names = FALSE)
        print(anosi_lfix(), row.names = FALSE)

        # ANOSI Case 5
        cat("\n## ANOSI Case 5: CLT and LC - Fixed; YR, RP - Random\n")
        print(anosi_f5(), row.names = FALSE)
        print(anosi_clfix(), row.names = FALSE)

        # Section 2: Descriptive Statistics
        cat("\n########################################################################\n")
        cat("###        Section 2: Descriptive Statistics (Means and CV)          ###\n")
        cat("########################################################################\n\n")
        print("#trait mean across genotype and environment(location x year combination)")
        print(mean_ge(), row.names = FALSE)
        print("#trait mean across genotype and years")
        print(mean_gy(), row.names = FALSE)
        print("#trait mean across genotype and location")
        print(mean_gl(), row.names = FALSE)
        print("#trait mean across genotype, location and year")
        print(mean_gly(), row.names = FALSE)
        print("#trait mean across location and year")
        print(mean_ly(), row.names = FALSE)
        print("#trait mean across genotype")
        print(mean_g(), row.names = FALSE)
        print("#trait coefficient of variation (cv) across gentoype and location")
        print(cv_gl(), row.names = FALSE)

        # Section 3: UniGene Stability
        cat("\n########################################################################\n")
        cat("###              Section 3: UniGene Stability Analysis               ###\n")
        cat("########################################################################\n\n")
        print(univariate(), row.names = FALSE)

        # Section 4: Location Evaluation
        cat("\n########################################################################\n")
        cat("###                      Section 4: Location Statistics            ###\n")
        cat("########################################################################\n\n")
        "#location value"
        print("location mean, genotype F ratio across location,correlation of location with average location performace")
        print(LocationValue(), row.names = FALSE)
        print("environment value-genotype F ratio across environments")
        print(EnvironmentValue(), row.names = FALSE)
        sink()
      }
    })


  ammiInput1 <- reactive({
    switch(input$ammi_var1,
           'Trait'=0,
           'PC1'=1,
           'PC2'=2,
           'PC3'=3,
           'PC4'=4,
    )
  }
)

  ammiInput2 <- reactive({
    switch(input$ammi_var2,
           'Trait'=0,
           'PC1'=1,
           'PC2'=2,
           'PC3'=3,
           'PC4'=4,
    )
  }
)

  observeEvent(input$ammi,{


    output$ammi_graph <- renderPlot({

      attach(tem1())

      # if(input$trait=="1")
      # {
      model <- AMMI(LC, CLT,RP, get(input$trait), console=FALSE)
      #   }
      # else if(input$trait=="2")
      # {model <- AMMI(LC, CLT,RP, Fruit_Count, console=FALSE) }
      #
      # else if(input$trait=="3")
      # {model <- AMMI(LC, CLT,RP, Percentage_Cull_Fruit, console=FALSE)}
      #
      # else if(input$trait=="4")
      # {model <- AMMI(LC, CLT,RP, Percentage_Early_Fruit, console=FALSE)}
      #
      # else if(input$trait=="5")
      # {model <- AMMI(LC, CLT,RP, Fruit_Size,console=FALSE)}
      #model$ANOVA
      #Comment: see help(plot.AMMI)
      detach(tem1())
      #Comment: biplot PC1 vs. DEPENDENT VARIABLE
      input$ammi
      isolate({
        plot(model, first=ammiInput2(),second=ammiInput1(), number=input$numreplace)
      })
    }
    )
  }
  )

  observeEvent(input$triammi,{

    output$triammi_graph <- renderPlot({


      attach(tem1())
      # if(input$trait=="1")
      # {
        model <- AMMI(LC, CLT,RP, get(input$trait), console=FALSE)
      #   }
      # else if(input$trait=="2")
      # {model <- AMMI(LC, CLT,RP, Fruit_Count, console=FALSE) }
      #
      # else if(input$trait=="3")
      # {model <- AMMI(LC, CLT,RP, Percentage_Cull_Fruit, console=FALSE)}
      #
      # else if(input$trait=="4")
      # {model <- AMMI(LC, CLT,RP, Percentage_Early_Fruit, console=FALSE)}
      #
      # else if(input$trait=="5")
      # {model <- AMMI(LC, CLT,RP, Fruit_Size,console=FALSE)}
      #model$ANOVA
      #Comment: see help(plot.AMMI)
      detach(tem1())
      input$triammi

      isolate({
        plot(model, type=2, number=input$numreplace)
      })
    })
  })


  observeEvent(input$gge,{

    GGE <- mean_gl()
    #Comment: colnames( ) gives column labels
    #Comment: rownames( ) gives row labels
    rownames(GGE)=GGE[,1]
    GGE=GGE[,-1]
    #Comment: View top 6 rows of data
    head(GGE)
    #Make a function to find all the NA (blank) values and replace with either row_mean or zero
    na_check=function(dat,check)
    {
      for(i in 1:nrow(dat))
      {
        for(h in 1:ncol(dat))
        {
          if (is.na(dat[i,h])==T)
          {
            if (check=="Mean")
            {
              dat[i,h]=mean(na.omit(as.numeric(dat[i,])))
              {
                if(check=="Zero")
                {
                  dat[i,h]=0
                }
              }
            }
          }
        }
      }
      return(dat)
    }
    #Comment: Replace blank record with mean or zero using user defined function na_check
    GGE=na_check(GGE,"Mean")
    #Comment: View top 6 rows of data
    head(GGE)
    #Comment: GGE biplot analysis
    GGEBiplot(GGE)
  })

}

