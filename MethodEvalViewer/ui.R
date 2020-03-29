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
                  <a href='https://doi.org/10.1162/99608f92.147cc28e'>How Confident Are We About Observational Findings in Healthcare: A Benchmark Study.</a>
              Harvard Data Science Review, 2(1), 2020")),
                       h3("Article abstract"),
                       p("Health care professionals increasingly rely on observational health care data, such as administrative claims and electronic health records, to estimate the causal effects of interventions. However, limited prior studies raise concerns about the real-world performance of the statistical and epidemiological methods that are used. We present the “Observational Health Data Sciences and Informatics (OHDSI) Methods Benchmark” that aims to evaluate the performance of effect estimation methods on real data. The benchmark comprises a gold standard, a set of metrics, and a set of open source software tools. The gold standard is a collection of real negative controls (drug-outcome pairs where no causal effect appears to exist) and synthetic positive controls (drug-outcome pairs that augment negative controls with simulated causal effects). We apply the benchmark using four large health care databases to evaluate methods commonly used in practice: the new-user cohort, self-controlled cohort, case-control, case-crossover, and self-controlled case series designs. The results confirm the concerns about these methods, showing that for most methods the operating characteristics deviate considerably from nominal levels. For example, in most contexts, only half of the 95% confidence intervals we calculated contain the corresponding true effect size. We previously developed an ‘empirical calibration’ procedure to restore these characteristics and we also evaluate this procedure. While no one method dominates, self-controlled methods such as the empirically calibrated self-controlled case series perform well across a wide range of scenarios."),
                       h3("Addendum"),
                       p("Since the publication of our manuscript in Harvard Data Science Review we have discovered an error in our implementation of IPTW (Inverse Probability of Treatment Weighting), which in this app is identified as CohortMethod analysis 5. We have corrected the error, and have updated the performance statistics accordingly. Note that although the observed performance has now increased somewhat, the correction has no material impact on our conclusions."),
                       h3("External links"),
                       HTML("<ul>
                    <li>Article: <a href=\"https://doi.org/10.1162/99608f92.147cc28e\">https://doi.org/10.1162/99608f92.147cc28e</a></li>
                    <li>Protocol: <a href = \"https://github.com/ohdsi-studies/MethodsLibraryPleEvaluation/blob/master/extras/OHDSI%20Protocol%20Method%20Evaluation.docx\">https://github.com/ohdsi-studies/MethodsLibraryPleEvaluation/blob/master/extras/OHDSI%20Protocol%20Method%20Evaluation.docx</a></li>
                    <li>Analysis source code: <a href = \"https://github.com/ohdsi-studies/MethodsLibraryPleEvaluation\">https://github.com/ohdsi-studies/MethodsLibraryPleEvaluation</a></li>
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
                                selectInput("evalType", label = div("Evaluation type:", actionLink("evalTypeInfo2", "", icon = icon("info-circle"))), choices = c("Effect estimation", "Comparative effect estimation")),
                                selectInput("calibrated", label = div("Empirical calibration:", actionLink("calibrationInfo2", "", icon = icon("info-circle"))), choices = c("Uncalibrated", "Calibrated")),
                                selectInput("mdrr", label = div("Minimum detectable RR:", actionLink("mdrrInfo2", "", icon = icon("info-circle"))), choices = c("All", "4", "2", "1.5", "1.25"), selected = "1.25"),
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


