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
              tabPanel("Method performance metrics",
                       fluidRow(
                         column(2,
                                selectInput("evalType", label = span("Evaluation type", title = "Type of task to evaluate."), choices = c("Effect estimation", "Comparative effect estimation")),
                                selectInput("calibrated", label = span("Empirical calibration", title = "Should empirical calibration be applied before computing performance metrics?"), choices = c("Uncalibrated", "Calibrated")),
                                selectInput("mdrr", label = span("Minimum Detectable RR", title = "Minimum detectable relative risk used to filter the controls before computing performance metrics."), choices = c("All", "4", "2", "1.5", "1.25"), selected = "1.25"),
                                selectInput("database", label = span("Database", title = "The database on which the methods were executed"), choices = dbs),
                                selectInput("stratum", label = span("Stratum", title = "Limiting the performance metrics to a single outcome (for exposure controls) or exposure (for outcome controls)."), choices = strata),
                                selectInput("trueRr", label = span("True effect size", title = "The true effect size to be considered when computing the performance metrics."), choices = trueRrs),
                                checkboxGroupInput("method", label = span("Method", title = "Methods to include in the evaluation."), choices = methods$method)
                                
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
                                                            div(strong("Figure S.1."),"Estimates with standard errors for the negative and positive controls, stratified by true effect size. Estimates that fall above the red dashed lines have a confidence interval that includes the truth.")),
                                                   tabPanel("ROC curves", 
                                                            plotOutput("rocCurves"),
                                                            div(strong("Figure S.2."),"Receiver Operator Characteristics curves for distinguising positive controls from negative controls."))
                                                 )
                                )   
                         )
                       )
              )
            )
  )
)


