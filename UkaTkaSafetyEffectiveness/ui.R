library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(paste("Comparative Safety and Effectiveness of Unicompartmental vs. Total Knee Replacement: Real-World Evidence from the OHDSI Network", if(blind) "***Blinded***" else "")),
            tags$head(tags$style(type = "text/css", "
             #loadmessage {
                                 position: fixed;
                                 top: 0px;
                                 left: 0px;
                                 width: 100%;
                                 padding: 5px 0px 5px 0px;
                                 text-align: center;
                                 font-weight: bold;
                                 font-size: 100%;
                                 color: #000000;
                                 background-color: #ADD8E6;
                                 z-index: 105;
                                 }
                                 ")),
            conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                             tags$div("Procesing...",id = "loadmessage")),
            
            tabsetPanel(id = "mainTabsetPanel",
                        tabPanel("About",
                                 HTML("<br/>"),
                                 HTML("<p>These research results are from a retrospective, real-world observational study to evaluate the risk of post-operative infection,
                                      venous thromboembolism, readmission, mortality, opioid use, and revision with unicompartmental versus total knee replacement. The results 
                                      have been reported in advance of results from an ongoing clinical trial comparing the same procedures for the risk of the same outcomes.</p>"),
                                 HTML("<p>This web-based application provides an interactive platform to explore all analysis results generated as part of this study, as 
                                      a supplement to the manuscript that has been submitted to the Lancet on DATE.</p>"),
                                 HTML("<br/>"),
                                 HTML("<p>Below is the abstract of the manuscript that summarizes the findings:</p>"),
                                 HTML("<p><strong>Backgroud:</strong> </p>"),
                                 HTML("<p><strong>Methods:</strong> </p>"),
                                 HTML("<p><strong>Findings:</strong>  </p>"),
                                 HTML("<p><strong>Interpretation:</strong> </p>"),
                                 HTML("<br/>"),
                                 HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
                                 HTML("<ul>"),
                                 HTML("<li>The full study protocol is available at: <a href=\"https://github.com/OHDSI/StudyProtocolSandbox/tree/master/ukatkasafety/documents\">https://github.com/OHDSI/StudyProtocolSandbox/tree/master/ukatkasafety/documents</a></li>"),
                                 HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/OHDSI/StudyProtocolSandbox/tree/master/ukatkasafety\">https://github.com/OHDSI/StudyProtocolSandbox/tree/master/ukatkasafety</a></li>"),
                                 HTML("</ul>")
                        ),
                        tabPanel("Explore results",
                                fluidRow(
                                  column(4,
                                         selectInput("target", div("Target cohort:", actionLink("targetCohortsInfo", "", icon("info-circle"))), unique(exposureOfInterest$exposureName), selected = unique(exposureOfInterest$exposureName)[2], width = '100%'),
                                         selectInput("comparator", div("Comparator cohort:", actionLink("comparatorCohortsInfo", "", icon("info-circle"))), unique(exposureOfInterest$exposureName), selected = unique(exposureOfInterest$exposureName)[4], width = '100%'),
                                         selectInput("outcome", div("Outcomes:", actionLink("outcomesInfo", "", icon("info-circle"))), unique(outcomeOfInterest$outcomeName), width = '100%'),
                                         checkboxGroupInput("database", div("Data sources:", actionLink("dbInfo", "", icon("info-circle"))), database$databaseId, selected = database$databaseId[1], width = '100%'),
                                         checkboxGroupInput("analysis", div("Analyses:", actionLink("analysesInfo", "", icon("info-circle"))), cohortMethodAnalysis$description,  selected = cohortMethodAnalysis$description, width = '100%')
                                  ),
                                  column(8,
                                         dataTableOutput("mainTable"),
                                         conditionalPanel("output.rowIsSelected == true",
                                                          tabsetPanel(id = "detailsTabsetPanel",
                                                                      tabPanel("Power",
                                                                               uiOutput("powerTableCaption"),
                                                                               tableOutput("powerTable"),
                                                                               uiOutput("timeAtRiskTableCaption"),
                                                                               tableOutput("timeAtRiskTable")
                                                                      ),
                                                                      tabPanel("Attrition",
                                                                               plotOutput("attritionPlot", width = 600, height = 600),
                                                                               uiOutput("attritionPlotCaption"),
                                                                               downloadButton("downloadAttritionPlot", label = "Download diagram")
                                                                      ),
                                                                      tabPanel("Population characteristics",
                                                                               uiOutput("table1Caption"),
                                                                               dataTableOutput("table1Table")),
                                                                      tabPanel("Propensity scores",
                                                                               plotOutput("psDistPlot"),
                                                                               div(strong("Figure 2."),"Preference score distribution. The preference score is a transformation of the propensity score
                                                                                                                             that adjusts for differences in the sizes of the two treatment groups. A higher overlap indicates subjects in the
                                                                                                                             two groups were more similar in terms of their predicted probability of receiving one treatment over the other."),
                                                                               downloadButton("downloadPsDistPlot", label = "Download plot")),
                                                                      tabPanel("Covariate balance",
                                                                               uiOutput("hoverInfoBalanceScatter"),
                                                                               plotOutput("balancePlot",
                                                                                          hover = hoverOpts("plotHoverBalanceScatter", delay = 100, delayType = "debounce")),
                                                                               uiOutput("balancePlotCaption"),
                                                                               downloadButton("downloadBalancePlot", label = "Download plot")),
                                                                      tabPanel("Systematic error",
                                                                               plotOutput("systematicErrorPlot"),
                                                                               div(strong("Figure 4."),"Systematic error. Effect size estimates for the negative controls (true hazard ratio = 1)
                                                                                                        and positive controls (true hazard ratio > 1), before and after calibration. Estimates below the diagonal dashed
                                                                                                        lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated
                                                                                                        estimator should have the true effect size within the 95 percent confidence interval 95 percent of times."),
                                                                               downloadButton("downloadSystematicErrorPlot", label = "Download plot")),
                                                                      tabPanel("Kaplan-Meier",
                                                                               plotOutput("kaplanMeierPlot", height = 550),
                                                                               uiOutput("kaplanMeierPlotPlotCaption"),
                                                                               downloadButton("downloadKaplanMeierPlot", label = "Download plot")) #,
                                                                      # tabPanel("Subgroups",
                                                                      #          uiOutput("subgroupTableCaption"),
                                                                      #          dataTableOutput("subgroupTable")) 
                                                          )
                                         )
                                  )
                                )
                        )
            )
  )
)
