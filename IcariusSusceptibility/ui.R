library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(paste("OHDSI COVID-19 Studyathon: ICARIUS - Susceptibility study", if(blind) "***Blinded***" else "")),
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
                                 div(p("The Observational Health Data Sciences and Informatics (OHDSI) international community hosted a COVID-19 virtual study-a-thon March 26-29 to inform healthcare decision-making in response to the current global pandemic. The preliminary research results on this web-based application are from a retrospective, real-world, observational study in support of this activity and will subsequently be submitted to a peer-reviewed, scientific journal. During manuscript development and the subsequent review period, these results are considered under embargo and should not be disclosed without explicit permission and consent from the authors."), style="border: 1px solid black; padding: 5px;"),
                                 HTML("<br/>"),
                                 HTML("<p>Below is the abstract of the manuscript that summarizes the findings:</p>"),
                                 HTML("<p><strong>Introduction: </strong> Angiotensin converting enzyme inhibitors (ACEs) and angiotensin receptor blockers (ARBs) could influence infection risk of coronavirus disease (COVID-19).  Observational studies to date lack pre-specification, transparency, rigorous ascertainment adjustment and international generalizability, with contradictory results. </p>"),
                                 HTML("<p><strong>Methods: </strong> Using electronic health records from Spain (SIDIAP) and the United States (Columbia University Irving Medical Center and Department of Veterans Affairs), we conducted a systematic cohort study with prevalent ACE, ARB, calcium channel blocker (CCB) and thiazide diuretic (THZ) users to determine relative risk of COVID-19 diagnosis and related hospitalization outcomes. The study minimized confounding through large-scale propensity score adjustment and negative control experiments. </p>"),
                                 HTML("<p><strong>Results: </strong> Following over 1.1 million antihypertensive users identified between November 2019 and January 2020, we observed no significant difference in relative COVID-19 diagnosis risk comparing ACE/ARB vs CCB/THZ monotherapy (hazard ratio: 0.98; 95% CI 0.84 - 1.14), nor any difference for mono/combination use (1.01; 0.90 - 1.15). ACE alone and ARB alone similarly showed no relative risk difference when compared to CCB/THZ monotherapy or mono/combination use.  Directly comparing ACE vs. ARB demonstrated no difference in monotherapy users (0.85; 0.69 - 1.05) but a modestly lower risk in mono/combination users (0.88; 0.79 - 0.99).  We observed, however, no significant difference between users for COVID-19 hospitalization or pneumonia risk across all comparisons. </p>"),
                                 HTML("<p><strong>Conclusion: </strong> There is no clinically significant increased risk of COVID-19 diagnosis or hospitalization with ACE or ARB use.  Users should not discontinue or change their treatment to avoid COVID-19. </p>"),
                                 HTML("<br/>"),
                                 HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
                                 HTML("<ul>"),
                                 HTML("<li>The full study protocol is available at: <a href=\"https://github.com/ohdsi-studies/Covid19Icarius/tree/master/Documents\">https://github.com/ohdsi-studies/Covid19Icarius/tree/master/Documents</a></li>"),
                                 HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/ohdsi-studies/Covid19Icarius/tree/master/Covid19IncidenceRasInhibitors\">https://github.com/ohdsi-studies/Covid19Icarius/tree/master/Covid19IncidenceRasInhibitors</a></li>"),
                                 HTML("</ul>")
                        ),
                        tabPanel("Explore results",

                        fluidRow(
                          column(3,
                                 selectInput("target", "Target", unique(exposureOfInterest$exposureName)),
                                 selectInput("comparator", "Comparator", unique(exposureOfInterest$exposureName), selected = unique(exposureOfInterest$exposureName)[2]),
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
