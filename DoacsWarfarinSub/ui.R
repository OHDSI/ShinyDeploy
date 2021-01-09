library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(paste("Comparative Risk Assessment of Severe Uterine Bleeding Following Exposure to Direct Oral Anticoagulants: A Network Study Across 4 US Observational Databases", if(blind) "***Blinded***" else "")),
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
                                 div(p("These research results are from a retrospective, real-world, observational study. This web-based application provides an interactive platform to explore all analysis results generated as part of this study, as a supplement to a full manuscript currently in development for submission to a peer-reviewed journal. During manuscript development and the subsequent review period, these results are considered under embargo and should not be disclosed without explicit permission and consent from the authors."), style="border: 1px solid black; padding: 5px;"),
                                 HTML("<br/>"),
                                 HTML("<p><strong>Abstract:</strong></p>"),
                                 HTML("<p>Antithrombotic therapies are associated with increased bleeding risk. Abnormal uterine bleeding has been reported in clinical trials evaluating direct oral anticoagulants (DOACs) in patients with venous thromboembolism (VTE), but data are limited in those with non-valvular atrial fibrillation (NVAF). Using real-world data from 4 US healthcare databases, we compared the occurrence of severe uterine bleeding among women newly exposed to rivaroxaban, apixaban, dabigatran, and warfarin stratified by indication. To reduce potential confounding, patients in comparative cohorts were matched on propensity scores (PS). Overall incidence of severe uterine bleeding was low in the populations exposed to DOACs, although relatively higher in the younger VTE population (NVAF: 1.9-10.1 events/1,000 person-years; VTE: 2.9-34.2 events/1,000 person-years). In the PS-matched NVAF population, a suggestive, moderately increased risk of severe uterine bleeding was observed for rivaroxaban relative to warfarin (hazard ratios [HRs] and 95% confidence intervals ranging from 0.83 [0.27-2.48] to 2.84 [1.32-6.23] across databases due to heterogeneity for this comparison), apixaban (pooled HR using meta-analysis: 1.45 [0.91-2.28]), and dabigatran (2.12 [1.01-4.43]), which were sensitive to observation period. In the PS-matched VTE population, a consistent, increased risk of severe uterine bleeding was observed for rivaroxaban relative to warfarin (2.03 [1.19-3.27]) and apixaban (2.25 [1.45-3.41]), which were insensitive to observation period. There was no strong evidence suggesting differential severe uterine bleeding risk between apixaban, dabigatran, and warfarin in either population. For women who need antithrombotic therapy, personalized management strategies with careful evaluation of benefits and risks are required.</p>"),
                                 HTML("<p><strong>Key points:</strong></p>"),
                                 HTML("<li>Using real-world data, the incidence of severe uterine bleeding was low but varied by antithrombotic therapy and occurred more often in patients with VTE versus those with NVAF.</li>"),
                                 HTML("<li>Compared with warfarin, apixaban, and dabigatran, rivaroxaban was associated with a moderately increased risk of severe uterine bleeding in NVAF patients that was sensitive to the observation period analyzed and a more consistently increased risk in VTE patients regardless of observation period analyzed.</li>"),
                                 HTML("<li>Careful evaluation of the benefits and risks of antithrombotic therapy in women requires consideration of factors including age, comorbidities, treatment duration, and indication for use.</li>"),
                                 HTML("<br/>"),
                                 HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
                                 HTML("<ul>"),
                                 HTML("<li>The study was registered ClinicalTrials.gov: <a href=\"https://clinicaltrials.gov/ct2/show/NCT04394234\">https://clinicaltrials.gov/ct2/show/NCT04394234</a></li>"),
                                 HTML("<li>The full study protocol is available at: <a href=\"https://github.com/ohdsi-studies/DoacsWarfarinSub/Protocol\">https://github.com/ohdsi-studies/DoacsWarfarinSub/Protocol</a></li>"),
                                 HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/ohdsi-studies/DoacsWarfarinSub/DoacsWarfarinSub\">https://github.com/ohdsi-studies/DoacsWarfarinSub/DoacsWarfarinSub</a></li>"),
                                 HTML("</ul>")
                        ),
                        tabPanel("Explore results",
            
            fluidRow(
              column(3,
                     selectInput("target", "Target", unique(exposureOfInterest$exposureName)),
                     selectInput("comparator", "Comparator", unique(exposureOfInterest$exposureName), selected = unique(exposureOfInterest$exposureName)[4]),
                     selectInput("outcome", "Outcome", unique(outcomeOfInterest$outcomeName)),
                     checkboxGroupInput("database", "Data source", database$databaseId, selected = database$databaseId),
                     checkboxGroupInput("analysis", "Analysis", cohortMethodAnalysis$description,  selected = cohortMethodAnalysis$description[1])
              ),
              column(9,
                     dataTableOutput("mainTable"),
                     conditionalPanel("output.rowIsSelected == true",
                                      tabsetPanel(id = "detailsTabsetPanel",
                                                  tabPanel("Power",
                                                           uiOutput("powerTableCaption"),
                                                           tableOutput("powerTable"),
                                                           conditionalPanel("output.isMetaAnalysis == false",
                                                                            uiOutput("timeAtRiskTableCaption"),
                                                                            tableOutput("timeAtRiskTable"))
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
                                                           radioButtons("charType", "", c("Pretty", "Raw"), selected = "Pretty", inline = TRUE),
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
                                                  tabPanel("Forest plot",
                                                           uiOutput("hoverInfoForestPlot"),
                                                           plotOutput("forestPlot", height = 450, 
                                                                      hover = hoverOpts("plotHoverForestPlot", delay = 100, delayType = "debounce")),
                                                           uiOutput("forestPlotCaption"),
                                                           div(style = "display: inline-block;vertical-align:top;")
                                                           ),
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
