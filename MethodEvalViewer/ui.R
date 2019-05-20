library(shiny)
library(DT)

shinyUI(
  fluidPage(style = 'width:1500px;',
            titlePanel("Supplementary data for 'How Confident Are We About Observational Findings in Healthcare: A Benchmark Study'"),
            tabsetPanel(
              tabPanel("About",
                       br(),
                       p("Supplementary data for:"),
                       p(HTML("Schuemie MJ, Cepeda MS, Suchard MA, Yang J, Tian Y, Schuler A, Ryan PB, Madigan D, Hripcsak G
                  How Confident Are We About Observational Findings in Healthcare: A Benchmark Study.
              Submitted")),
                       p("This web-based application provides an interactive platform to explore all perforamnce metricss generated as part of this study, as a supplement to the manuscript"),
                       h3("Article abstract"),
                       p("To do"),
                       h3("External links"),
                       HTML("<ul>
                    <li>Article: To do</li>
                    <li>Protocol: <a href = \"https://github.com/schuemie/MethodsLibraryPleEvaluation/blob/master/extras/OHDSI%20Protocol%20Method%20Evaluation.docx\">https://github.com/schuemie/MethodsLibraryPleEvaluation/blob/master/extras/OHDSI%20Protocol%20Method%20Evaluation.docx</a></li>
                    <li>Analysis source code: <a href = \"https://github.com/schuemie/MethodsLibraryPleEvaluation\">https://github.com/schuemie/MethodsLibraryPleEvaluation</a></li>
                  </ul>")
                       
              ),
              tabPanel("Overview across strata and databases",
                       fluidRow(
                         column(2,
                                selectInput("evalTypeOverview", label = div("Evaluation type:", actionLink("evalTypeInfo", "", icon = icon("info-circle"))),, choices = c("Effect estimation", "Comparative effect estimation")),
                                selectInput("calibratedOverview", label = div("Empirical calibration:", actionLink("calibrationInfo", "", icon = icon("info-circle"))), choices = c("Uncalibrated", "Calibrated"), selected = "Calibrated"),
                                selectInput("metric", label = div("Metric:", actionLink("metricInfo", "", icon = icon("info-circle"))), choices = c("AUC", "Coverage", "Mean precision (1/SE^2)", "Mean squared error (MSE)", "Type I error", "Type II error", "Non-estimable"), selected = "Mean precision (1/SE^2)"),
                                selectInput("mdrrOverview", label = div("Minimum detectable RR:", actionLink("mdrrInfo", "", icon = icon("info-circle"))), choices = c("All", "4", "2", "1.5", "1.25"), selected = "1.25")
                         ),
                         column(10,
                                uiOutput("hoverOverview"),
                                plotOutput("overviewPlot",
                                           height = "800px",
                                           hover = hoverOpts("plotHoverOverview", 
                                                             delay = 100, 
                                                             delayType = "debounce")),
                                uiOutput("overviewPlotCaption")
                         )
                       )
              ),
              tabPanel("Method performance metrics per stratum and database",
                       fluidRow(
                         column(2,
                                selectInput("evalType", label = div("Evaluation type:", actionLink("evalTypeInfo", "", icon = icon("info-circle"))), choices = c("Effect estimation", "Comparative effect estimation")),
                                selectInput("calibrated", label = div("Empirical calibration:", actionLink("calibrationInfo", "", icon = icon("info-circle"))), choices = c("Uncalibrated", "Calibrated")),
                                selectInput("mdrr", label = div("Minimum detectable RR:", actionLink("mdrrInfo", "", icon = icon("info-circle"))), choices = c("All", "4", "2", "1.5", "1.25"), selected = "1.25"),
                                selectInput("database", label = div("Database:", actionLink("databaseInfo", "", icon = icon("info-circle"))), choices = dbs),
                                selectInput("stratum", label = div("Stratum:", actionLink("stratumInfo", "", icon = icon("info-circle"))), choices = strata),
                                selectInput("trueRr", label = div("True effect size:", actionLink("trueRrInfo", "", icon = icon("info-circle"))), choices = trueRrs),
                                checkboxGroupInput("method", label =  div("Methods:", actionLink("methodsInfo", "", icon = icon("info-circle"))), choices = methods$method)
                                
                         ),
                         column(10,
                                dataTableOutput("performanceMetrics"),
                                uiOutput("tableCaption"),
                                conditionalPanel(condition = "output.details",
                                                 div(style = "display:inline-block", h4(textOutput("details"))), 
                                                 div(style = "display:inline-block", actionLink("showSettings", "Details")),
                                                 tabsetPanel(
                                                   tabPanel("Estimates", 
                                                            uiOutput("hoverInfoEstimates"),
                                                            plotOutput("estimates", 
                                                                       height = "270px",
                                                                       hover = hoverOpts("plotHoverInfoEstimates", 
                                                                                         delay = 100, 
                                                                                         delayType = "debounce")),
                                                            div(strong("Figure S.2."),"Estimates with standard errors for the negative and positive controls, stratified by true effect size. Estimates that fall above the red dashed lines have a confidence interval that includes the truth. Hover mouse over point for more information.")),
                                                   tabPanel("ROC curves", 
                                                            plotOutput("rocCurves"),
                                                            div(strong("Figure S.3."),"Receiver Operator Characteristics curves for distinguising positive controls from negative controls."))
                                                 )
                                )   
                         )
                       )
              )
            )
  )
)


