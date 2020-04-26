library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(paste("Risk of hydroxychloroquine, alone and in combination with azithromycin: a multi-national study", if(blind) "***Blinded***" else "")),
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
                                 div(p("The Observational Health Data Sciences and Informatics (OHDSI) international community hosted a COVID-19 virtual study-a-thon over March 26-29, 2020 to collaboratively generate evidence to inform healthcare decision-making in response to the current global pandemic. The preliminary research results on this web-based application are from a retrospective, real-world, observational study in support of this activity and will subsequently be submitted to a peer-reviewed, scientific journal. During manuscript development and the subsequent review period, these results are considered under embargo and should not be disclosed without explicit permission and consent from the authors."), style="border: 1px solid black; padding: 5px;"),
                                 HTML("<br/>"),
                                 HTML("<p>Below is the abstract of the manuscript that summarizes the findings:</p>"),
                                 HTML("<p><strong>Background:</strong> Hydroxychloroquine has recently received Emergency Use Authorization by the FDA and is currently prescribed in combination with azithromycin for COVID-19 pneumonia. We studied the safety of hydroxychloroquine, alone and in combination with azithromycin.</strong></p>"),
                                 HTML("<p><strong>Methods:</strong> New user cohort studies were conducted including 16 severe adverse events (SAEs). Rheumatoid arthritis patients aged 18+ and initiating hydroxychloroquine were compared to those initiating sulfasalazine and followed up over 30 days. Self-controlled case series (SCCS) were conducted to further establish safety in wider populations. Separately, SAEs associated with hydroxychloroquine-azithromycin (compared to hydroxychloroquine-amoxicillin) were studied. Data comprised 14 sources of claims data or electronic medical records from Germany, Japan, Netherlands, Spain, UK, and USA. Propensity score stratification and calibration using negative control outcomes were used to address confounding. Cox models were fitted to estimate calibrated hazard ratios (CalHRs) according to drug use. Estimates were pooled where I2<40%.</p>"),
                                 HTML("<p><strong>Findings:</strong> Overall, 956,374 and 310,350 users of hydroxychloroquine and sulfasalazine, and 323,122 and 351,956 users of hydroxychloroquine-azithromycin and hydroxychloroquine-amoxicillin were included. No excess risk of SAEs was identified when 30-day hydroxychloroquine and sulfasalazine use were compared. SCCS confirmed these findings. However, when azithromycin was added to hydroxychloroquine, we observed an increased risk of 30-day cardiovascular mortality (CalHR 2.19 [95% CI 1.22-3.94]), chest pain/angina (CalHR 1.15 [1.05-1.26]), and heart failure (CalHR 1.22 [1.02-1.45]). Cardiovascular mortality was also increased in long term on-treatment analysis of hydroxychloroquine versus sulfasalazine (CalHR 1.65 [1.12-2.44]).</p>"),
                                 HTML("<p><strong>Interpretation:</strong> Hydroxychloroquine treatment appears safe during the 30 days post-initiation in rheumatoid arthritis, but combination use of hydroxychloroquine and azithromycin may induce heart failure and cardiovascular mortality.  Extended use of hydroxychloroquine may increase mortality risk in RA. Given that hydroxychloroquine, alone and in combination with azithromycin, is being actively considered for treatment in COVID-19, we call for caution and careful consideration about the benefit-risk trade-off of the uncertain efficacy and this identified risk when managing patients with COVID-19.</p>"),
                                 HTML("<br/>"),
                                 HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
                                 HTML("<ul>"),
                                 HTML("<li>This study was registered at ENCePP (EU PAS Register number EUPAS34497): <a href=\"http://www.encepp.eu/encepp/viewResource.htm?id=34498\">http://www.encepp.eu/encepp/viewResource.htm?id=34498</a></li>"),
                                 HTML("<li>The full study protocol is available at:  <a href=\"https://github.com/ohdsi-studies/Covid19EstimationHydroxychloroquine/tree/master/documents\">https://github.com/ohdsi-studies/Covid19EstimationHydroxychloroquine/tree/master/documents</a></li>"),
                                 HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/ohdsi-studies/Covid19EstimationHydroxychloroquine\">https://github.com/ohdsi-studies/Covid19EstimationHydroxychloroquine</a></li>"),
                                 HTML("</ul>")
                        ),
                        tabPanel("Explore results",

                        fluidRow(
                          column(3,
                                 selectInput("target", "Target", unique(exposureOfInterest$exposureName), selected = unique(exposureOfInterest$exposureName)[1], width = '100%'),
                                 selectInput("comparator", "Comparator", unique(exposureOfInterest$exposureName), selected = unique(exposureOfInterest$exposureName)[2], width = '100%'),
                                 selectInput("outcome", "Outcome", unique(outcomeOfInterest$outcomeName), width = '100%'),
                                 checkboxGroupInput("database", "Data source", database$databaseId, selected = database$databaseId, width = '100%'),
                                 checkboxGroupInput("analysis", "Analysis", cohortMethodAnalysis$description,  selected = cohortMethodAnalysis$description[1], width = '100%')
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
