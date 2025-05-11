#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyBS
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    # golem_add_external_resources(),
    # application UI logic
    fixedPage(
      # Application title
      navbarPage('RGxEStat',
                 tabPanel('Significance Analyzer',
                          sidebarLayout(
                            sidebarPanel(
                              # titlePanel('Preparations for Start                     '),
                              # sidebarLayout(
                              #   sidebarPanel(radioButtons("format", label = h6("Choose Input Data Format"),
                              #                             choices = list(".csv" = 1, ".txt" = 2, ".xls or .xlsx" = 3),
                              #                             selected = NULL),
                              #                br(),
                              #                fileInput("data", label = h6("Select Data Path")),
                              #                width = 6),
                              #   mainPanel(radioButtons("trait", label = h6("Choose Trait To Be Analyzed"),
                              #                          choices = list("Trait1" = 1, "Trait2" = 2, "Trait3" = 3,"Trait4"=4,
                              #                                         "Trait5"=5),
                              #                          selected = NULL),
                              #             actionButton("trait_submit", label = "Submit"),
                              #             width = 6)
                              # ),
                              width = 6,
                              bsCollapse(
                                id = "upload_data",
                                open = "Analysis of Significance",
                                multiple = TRUE,
                                bsCollapsePanel(
                                  title = "Dataset",
                                  fileInput("datafile", "Upload File",
                                            accept = c(".csv", ".txt", ".xls", ".xlsx")),
                                  hr(),
                                  tags$label(
                                    "Select Trait",
                                    `for`   = "trait",
                                    class   = "control-label"
                                  ),
                                  uiOutput("trait_ui"),
                                  actionButton("trait_submit", "Submit"),
                                  style = "info"
                                  ),
                                bsCollapsePanel(
                                  title = "Analysis of Significance",
                                  helpText("Case 1: CLT, YR, LC and RP - Random"),
                                  actionButton("case1", label = "Analyze"),
                                  br(), br(),
                                  helpText("Case 2: CLT, YR and LC - Fixed; RP - Random"),
                                  actionButton("case2", label = "Analyze"),
                                  br(), br(),
                                  helpText("Case 3: CLT - Fixed; YR, LC and RP - Random"),
                                  actionButton("case3", label = "Analyze"),
                                  br(), br(),
                                  helpText("Case 4: LC - Fixed; YR, CLT and RP - Random"),
                                  actionButton("case4", label = "Analyze"),
                                  br(), br(),
                                  helpText("Case 5: CLT and LC - Fixed; YR and RP - Random"),
                                  actionButton("case5", label = "Analyze"),
                                  br(), br(),
                                  style = "primary"
                                  )
                                ),
                              ),

                            mainPanel(
                              # sidebarLayout(
                              #   sidebarPanel(

                                # mainPanel(
                                  h6('Results display here:'),
                                  tableOutput('anositable1'),
                                  br(), br(), br(),
                                  tableOutput('anositable2'),
                                  # width = 6
                                  # )
                              # ),
                              width = 6),
                          ),
                 ),
                 tabPanel("Univariate Analyzer",
                          titlePanel("Univariate Stability Analyzer"),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              h4("Compute Coefficient of Variation over Genotype and Location"),
                              # helpText("CV refers to coefficient of variation"),
                              actionButton("cv", label = "Execute"),
                              hr(),
                              h4("Compute Descritptive Statistics"),
                              helpText("Trait Mean across Genotype "),
                              actionButton("trait_G", label = "Compute"),
                              helpText("Trait Mean over Genotype and Environment"),
                              actionButton("trait_GxE", label = "Compute"),
                              helpText("Trait Mean over Genotype and Year"),
                              actionButton("trait_GxY", label = "Compute"),
                              helpText("Trait Mean over Genotype and Location"),
                              actionButton("trait_GxL", label = "Compute"),
                              helpText("Trait Mean over Location and Year"),
                              actionButton("trait_LxY", label = "Compute"),
                              helpText("Trait Mean over Genotype and Location and Year"),
                              actionButton("trait_GxLxY", label = "Compute"),
                              width = 6),
                            mainPanel(
                              hr(),
                              h6('Results display here:'),
                              tableOutput('valuetable1'),
                              tableOutput('valuetable2'),
                              width = 6),
                          ),
                          hr(),hr(),
                          sidebarLayout(
                            sidebarPanel(h4("Compute Univariate Stability Statistics"),
                                         helpText("(including regression slope, deviation, Shukla's sigmma, ecovalence, YS)"),
                                         actionButton("stability_stats", label = "Execute"),width = 6),
                            mainPanel(
                              h4("Analyze Discriminative and Representative Location"),
                              helpText("(including genotype F ratio across location, environment and location, environment correlation)"),
                              actionButton("F_ratio", label = "Compute"),width = 6
                            )
                          ),
                          hr(),
                          sidebarLayout(
                            sidebarPanel(
                              h4("Cluster Analysis of Location"),
                              actionButton('cluster',label = "Analyze",value=0),
                              hr(),
                              plotOutput('cluster_dia'),
                              width = 6),
                            mainPanel(
                              hr(),
                              h4("Save and Export"),
                              selectInput("dataframe",label=h6('Select results'),
                                          choices = c('ANOSI case 1','ANOSI case 2','ANOSI case 3','ANOSI case 4',
                                                      'ANOSI case 5','mean_trait_g×e','mean_trait_g×y','mean_trait_g×l×y','CV of genotype×location',
                                                      'univariate stability statistics','F ratio of genotype across location','F ratio of genotype across environment',
                                                      'all results (only for .txt)')
                              ),
                              radioButtons("downformat", label = h6("Format"),
                                           choices = c("csv" , "txt" , "xlsx", "doc")),
                              downloadButton("download","Export"),
                              br(),
                              hr(),
                              width = 6)
                          )
                 ),

                 tabPanel("Multivariate Analyzer",
                          titlePanel("Multivariate Stability Analyzer"),
                          # helpText("(Genotype×Environment stability analysis)"),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              h4("Additive Main Effects and Multiplicative Interaction"),
                              hr(),
                              checkboxInput("numreplace", label = "number the genotypes", value = FALSE),
                              # h5("biplot variables"),
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("ammi_var1",label=h6('vertical axis'),
                                              choices = c('Trait','PC1','PC2','PC3','PC4')
                                  ),
                                  # helpText('vertical axis'),
                                  actionButton("ammi", label = "Plot",value=0),
                                  width = 6
                                ),
                                mainPanel(selectInput("ammi_var2",label=h6('horizontal axis'),
                                                      choices = c('Trait','PC1','PC2','PC3','PC4')
                                ),
                                # helpText('horizontal axis'),
                                width = 6
                                ),
                              ),
                              hr(),
                              helpText('Display AMMI triplot of PC1 vs PC2 vs PC3'),
                              actionButton("triammi", label = "Triplot",value=0),
                              br(),
                              plotOutput('triammi_graph')
                              ,width = 6),
                            mainPanel(
                              sidebarLayout(
                                mainPanel(
                                  width=0
                                ),
                                sidebarPanel(
                                  plotOutput('ammi_graph')
                                  ,width=12
                                )
                              ),
                              hr(),
                              hr(),
                              h4("Genotype Main Effects Plus Gene-Environment Interaction"),
                              # helpText("(GGE model analysis)"),
                              actionButton("gge", label = "Analyze"),
                              br(),
                              br(),
                              h6("Reminder:"),
                              h6("Please calculate trait mean over genotype and location before GGE analysis."),
                              width=6
                            )
                          )
                 )
      )
    )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "RGxEStat"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

