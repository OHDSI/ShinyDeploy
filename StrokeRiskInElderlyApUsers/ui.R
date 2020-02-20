library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(paste("Stroke risk among new users of typical vs. atypical antipsychotics among patients aged ≥65 years", if(blind) "***Blinded***" else "")),
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
                                 div(p("These research results are from a retrospective, real-world, observational study. This web-based application provides an interactive platform to explore all analysis results generated as part of this study, as a supplement to a full manuscript currently in development for to a peer-reviewed journal. During manuscript development and the subsequent review period, these results are considered under embargo and should not be disclosed without explicit permission and consent from the authors."), style="border: 1px solid black; padding: 5px;"),
                                 HTML("<br/>"),
                                 HTML("<p>Below is the abstract of the manuscript that summarizes the findings:</p>"),
                                 HTML("<p><strong>Backgroud: </strong>We estimated stroke risk associated with new exposure to haloperidol, or any typical antipsychotic vs. atypical antipsychotics among patients aged ≥65 years regardless of dementia status.</p>"),
                                 HTML("<p><strong>Methods: </strong>Data were obtained from the IBM MarketScan® Medicare Supplemental Database (MDCR) between 01 January 2001 and 31 December 2017. Stroke risk among new users of typical antipsychotics (T1 cohort), and among new users of haloperidol (T2 cohort), was compared to stroke risk among new users of atypical antipsychotics (C1 cohort) aged ≥65 years. Crude incidence rate (IR) and incidence proportion (IP) of stroke were estimated within each exposure cohort and gender subgroup. Three propensity score matching approaches were employed: unadjusted (crude) estimate, Sentinel PS replication (Sentinel PS strategy), and a large-scale regularized regression model involving PS estimation (adapted PS strategy).</p>"),
                                 HTML("<p><strong>Results: </strong>Overall, 43,711 patients were included in the T1 cohort, 29,780 patients in T2 cohort, and 232,353 patients in C1 cohort. Before matching, mean ages of patients in T1, T2, and C1 were, 80.9 years, 83.7 years, and 80.5 years, respectively and the majority were women. The crude IRs for stroke per 1000 person years [PY] for T1, T2, and C1 cohorts were 17.67, 23.74, and 14.17, respectively. Pre-planned analyses showed that the PS matched calibrated hazard ratio (cHR) for stroke in patients of T1 vs. C1 cohort was 1.08 (95% calibrated confidence interval [cCI]=0.75, 1.55) with the Sentinel PS strategy and 1.31 (95% cCI=1.07, 1.60) with the adapted PS strategy. The cHR for stroke in patients of T2 vs. C1 cohort was 1.69 (95% cCI 1.08, 2.75) with Sentinel PS strategy and 1.45 (95% cCI 1.17, 1.80) with the adapted PS strategy.</p>"),
                                 HTML("<p><strong>Conclusion: </strong>This study demonstrated that stroke risk in elderly new users of haloperidol or typical antipsychotics was elevated compared with stroke risk among new users of atypical antipsychotics.</p>"),
                                 HTML("<br/>"),
                                 HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
                                 HTML("<ul>"),
                                 HTML("<li>The full study protocol is available at: <a href=\"https://github.com/ohdsi-studies/STUDYNAMEHERE/tree/master/documents\">https://github.com/ohdsi-studies/STUDYNAMEHERE/tree/master/documents</a></li>"),
                                 HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/ohdsi-studies/STUDYNAMEHERE\">https://github.com/ohdsi-studies/STUDYNAMEHERE</a></li>"),
                                 HTML("</ul>")
                        ),
                        tabPanel("Explore results",
            
            fluidRow(
              column(4,
                     selectInput("target", "Target", unique(exposureOfInterest$exposureName)[1:2], selected = unique(exposureOfInterest$exposureName)[1], width = '95%'),
                     selectInput("comparator", "Comparator", unique(exposureOfInterest$exposureName[3]), selected = unique(exposureOfInterest$exposureName)[3], width = '95%'),
                     selectInput("outcome", "Outcome", unique(outcomeOfInterest$outcomeName), width = '95%'),
                     checkboxGroupInput("database", "Data source", database$databaseId, selected = database$databaseId[1], width = '95%'),
                     checkboxGroupInput("analysis", "Analysis", cohortMethodAnalysis$description,  selected = cohortMethodAnalysis$description, width = '95%')
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
                                                           dataTableOutput("table1Table")
                                                  ),
                                                  tabPanel("Propensity scores",
                                                           plotOutput("psDistPlot"),
                                                           div(strong("Figure 2."),"Preference score distribution. The preference score is a transformation of the propensity score
                                                                                                         that adjusts for differences in the sizes of the two treatment groups. A higher overlap indicates subjects in the
                                                                                                         two groups were more similar in terms of their predicted probability of receiving one treatment over the other."),
                                                           downloadButton("downloadPsDistPlot", label = "Download plot")
                                                  ),
                                                  tabPanel("Covariate balance",
                                                           uiOutput("hoverInfoBalanceScatter"),
                                                           plotOutput("balancePlot",
                                                                      hover = hoverOpts("plotHoverBalanceScatter", delay = 100, delayType = "debounce")),
                                                           uiOutput("balancePlotCaption"),
                                                           downloadButton("downloadBalancePlot", label = "Download plot")
                                                  ),
                                                  tabPanel("Systematic error",
                                                           plotOutput("systematicErrorPlot"),
                                                           div(strong("Figure 4."),"Systematic error. Effect size estimates for the negative controls (true hazard ratio = 1)
                                                                                    and positive controls (true hazard ratio > 1), before and after calibration. Estimates below the diagonal dashed
                                                                                    lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated
                                                                                    estimator should have the true effect size within the 95 percent confidence interval 95 percent of times."),
                                                           downloadButton("downloadSystematicErrorPlot", label = "Download plot")
                                                  ),
                                                  tabPanel("Kaplan-Meier",
                                                           plotOutput("kaplanMeierPlot", height = 550),
                                                           uiOutput("kaplanMeierPlotPlotCaption"),
                                                           downloadButton("downloadKaplanMeierPlot", label = "Download plot")
                                                  )
                                                  
                                      )
                     )
              )
            )
            )
            )
            )
  )