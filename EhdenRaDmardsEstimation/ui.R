library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(paste("Evidence Explorer", if(blind) "***Blinded***" else "")),
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
                                 div(p("These research results are from a retrospective, real-world, observational study to estimate the population-level effects of conventional synthetic disease-modifying antirheumatic drugs among patients with rheumatoid arthritis. This web-based application provides an interactive platform to explore all analysis results generated as part of this study, as a supplement to abstracts and a full manuscript currently in preparation for submission to scientific conferences and a peer-reviewed journal. During abstract and manuscript preparation and the subsequent review period, these results are considered under embargo and should not be disclosed without explicit permission and consent from the authors."), style="border: 1px solid black; padding: 5px;"),
                                 HTML("<br/>"),
                                 HTML("<p>Below is the abstract of the manuscript that summarizes the findings:</p>"),
                                 HTML("<p><strong>Backgroud: </strong></p>"),
                                 HTML("<p><strong>Methods: </strong></p>"),
                                 HTML("<p><strong>Findings: </strong></p>"),
                                 HTML("<p><strong>Interpretation: </strong></p>"),
                                 HTML("<br/>"),
                                 HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
                                 HTML("<ul>"),
                                 HTML("<li>The full study protocol is available at: <a href=\"https://github.com/ohdsi-studies/EhdenRaDmardsEstimation/tree/master/documents\">https://github.com/ohdsi-studies/EhdenRaDmardsEstimation/tree/master/documents</a></li>"),
                                 HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/ohdsi-studies/EhdenRaDmardsEstimation\">https://github.com/ohdsi-studies/EhdenRaDmardsEstimation</a></li>"),
                                 HTML("</ul>")
                        ),
                        tabPanel("Explore results",
                                
                                 fluidRow(
                                  column(3,
                                         selectInput("target", "Target", unique(exposureOfInterest$exposureName), selected = unique(exposureOfInterest$exposureName)[1]),
                                         selectInput("comparator", "Comparator", unique(exposureOfInterest$exposureName), selected = unique(exposureOfInterest$exposureName)[4]),
                                         selectInput("outcome", "Outcome", unique(outcomeOfInterest$outcomeName)),
                                         checkboxGroupInput("database", "Data source", database$databaseId, selected = database$databaseId),
                                         checkboxGroupInput("analysis", "Analysis", cohortMethodAnalysis$description,  selected = cohortMethodAnalysis$description)
                                  ),
                                  column(9,
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
                                                                               div(style = "display: inline-block;vertical-align:top;",
                                                                                   downloadButton("downloadAttritionPlotPng", label = "Download diagram as PNG"),
                                                                                   downloadButton("downloadAttritionPlotPdf", label = "Download diagram as PDF")
                                                                               )
                                                                      ),
                                                                      tabPanel("Population characteristics",
                                                                               uiOutput("table1Caption"),
                                                                               dataTableOutput("table1Table")),
                                                                      tabPanel("Propensity model",
                                                                               div(strong("Table 3."),"Fitted propensity model, listing all coviates with non-zero coefficients. Positive coefficients indicate predictive of the target exposure."),
                                                                               dataTableOutput("propensityModelTable")),
                                                                      tabPanel("Propensity scores",
                                                                               plotOutput("psDistPlot"),
                                                                               div(strong("Figure 2."),"Preference score distribution. The preference score is a transformation of the propensity score
                                                                                                                             that adjusts for differences in the sizes of the two treatment groups. A higher overlap indicates subjects in the
                                                                                                                             two groups were more similar in terms of their predicted probability of receiving one treatment over the other."),
                                                                               div(style = "display: inline-block;vertical-align:top;",
                                                                                   downloadButton("downloadPsDistPlotPng", label = "Download plot as PNG"),
                                                                                   downloadButton("downloadPsDistPlotPdf", label = "Download plot as PDF")
                                                                               )),
                                                                      tabPanel("Covariate balance",
                                                                               uiOutput("hoverInfoBalanceScatter"),
                                                                               plotOutput("balancePlot",
                                                                                          hover = hoverOpts("plotHoverBalanceScatter", delay = 100, delayType = "debounce")),
                                                                               uiOutput("balancePlotCaption"),
                                                                               div(style = "display: inline-block;vertical-align:top;",
                                                                                   downloadButton("downloadBalancePlotPng", label = "Download plot as PNG"),
                                                                                   downloadButton("downloadBalancePlotPdf", label = "Download plot as PDF")
                                                                               )),
                                                                      tabPanel("Systematic error",
                                                                               plotOutput("systematicErrorPlot"),
                                                                               div(strong("Figure 4."),"Systematic error. Effect size estimates for the negative controls (true hazard ratio = 1)
                                                                                                        and positive controls (true hazard ratio > 1), before and after calibration. Estimates below the diagonal dashed
                                                                                                        lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated
                                                                                                        estimator should have the true effect size within the 95 percent confidence interval 95 percent of times."),
                                                                               div(style = "display: inline-block;vertical-align:top;",
                                                                                   downloadButton("downloadSystematicErrorPlotPng", label = "Download plot as PNG"),
                                                                                   downloadButton("downloadSystematicErrorPlotPdf", label = "Download plot as PDF")
                                                                               )),
                                                                      tabPanel("Kaplan-Meier",
                                                                               plotOutput("kaplanMeierPlot", height = 550),
                                                                               uiOutput("kaplanMeierPlotPlotCaption"),
                                                                               div(style = "display: inline-block;vertical-align:top;",
                                                                                   downloadButton("downloadKaplanMeierPlotPng", label = "Download plot as PNG"),
                                                                                   downloadButton("downloadKaplanMeierPlotPdf", label = "Download plot as PDF")
                                                                               )),
                                                                      tabPanel("Subgroups",
                                                                               uiOutput("subgroupTableCaption"),
                                                                               dataTableOutput("subgroupTable")
                                                                      )
                                                                      
                                                          )
                                         )
                                  )
                                )
                        )
            )
  )
)
