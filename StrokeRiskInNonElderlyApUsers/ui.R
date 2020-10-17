library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(paste("Stroke risk among non-elderly users of haloperidol or typical antipsychotics vs. atypical antipsychotics: A cohort study from a US health insurance claims database", if(blind) "***Blinded***" else "")),
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
                                 HTML("<p><strong>Backgroud: </strong>We estimated stroke risk associated with new exposure to any typical antipsychotic, or, haloperidol vs. atypical antipsychotics among patients aged 18-64 years without dementia and, separately, regardless of dementia diagnosis.</p>"),
                                 HTML("<p><strong>Methods: </strong>Data were obtained from IBM MarketScan® Commercial Database (CCAE, 01 January 2001 - 31 December 2017). Among those without a recent dementia diagnosis, stroke risk for new users of typical antipsychotics (T1 cohort) or haloperidol (T2 cohort) was compared with new users of atypical antipsychotics (C1 cohort). A similar comparison was conducted for new users of typical antipsychotics (T3 cohort) or haloperidol (T4 cohort) vs. new users of atypical antipsychotics (C2 cohort), regardless of recent dementia diagnosis. Crude incident stroke rates (cIRs) within each cohort were calculated. For each comparison three hazard ratios (HRs) were reported, two of which were adjusted using propensity score (PS) matching strategies to limit confounding: unadjusted (crude), Sentinel PS strategy, and large-scale regularized regression model (adapted PS strategy).</p>"),
                                 HTML("<p><strong>Results: </strong>Each cohort included ≥12,000 patients. The cIRs for stroke per 1,000 person-years were 3.10 (T1), 5.99 (T2), 3.14 (T3), 6.12 (T4), 0.85 (C1), and 0.90 (C2). Pre-planned analysis with adapted PS strategy matching yielded calibrated HRs (cHRs) for stroke: T1 vs. C1: 2.05 (calibrated confidence interval=1.13-3.89); T2 vs. C1: 2.47 (1.14-5.48), T3 vs. C2: 1.64 (0.94-2.97), T4 vs. C2: 1.98 (0.99-4.00). A post-hoc sensitivity analysis to address possible confounding introduced by the 2015 change from ICD-9 to ICD-10 yielded cHRs for T1 vs. C1: 1.59 (0.87-3.01); T2 vs. C1: 2.79 (1.24-6.42), T3 vs. C2: 1.41 (0.79-2.62), T4 vs. C2: 3.47 (1.63-7.92).</p>"),
                                 HTML("<p><strong>Conclusion: </strong>For both the pre-planned and sensitivity analysis, relative to users of atypical antipsychotics, risk of stroke among non-elderly patients without a recent dementia diagnosis was not higher among users of all typical antipsychotics, but was higher among users of haloperidol. Though it was narrowly missed for haloperidol for non-elderly patients regardless of dementia status, no statistically significant difference in stroke risk was observed among non-elderly users of all typical antipsychotics or of haloperidol.</p>"),
                                 HTML("<br/>"),
                                 HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
                                 HTML("<ul>"),
                                 HTML("<li>The full study protocol is available at: <a href=\"https://github.com/ohdsi-studies/StrokeRiskInApUsers/tree/master/documents\">https://github.com/ohdsi-studies/StrokeRiskInApUsers/tree/master/documents</a></li>"),
                                 HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/ohdsi-studies/StrokeRiskInApUsers\">https://github.com/ohdsi-studies/StrokeRiskInApUsers</a></li>"),
                                 HTML("</ul>")
                        ),
                        tabPanel("Explore results",
            
            fluidRow(
              column(4,
                     selectInput("target", "Target", unique(exposureOfInterest$exposureName[1:4]), selected = unique(exposureOfInterest$exposureName)[1], width = '95%'),
                     selectInput("comparator", "Comparator", unique(exposureOfInterest$exposureName[5:6]), selected = unique(exposureOfInterest$exposureName)[5], width = '95%'),
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