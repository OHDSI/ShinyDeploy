library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(paste("Risk of hydroxychloroquine alone and in combination with azithromycin in the treatment of rheumatoid arthritis: a multinational, retrospective study", if(blind) "***Blinded***" else "")),
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
                                 HTML("<p><strong>Background:</strong> Hydroxychloroquine, a drug commonly used in the treatment of rheumatoid arthritis, has received much negative publicity for adverse events associated with its authorisation for emergency use to treat patients with COVID-19 pneumonia. We studied the safety of hydroxychloroquine, alone and in combination with azithromycin, to determine the risk associated with its use in routine care in the patients with rheumatoid arthritis.</strong></p>"),
                                 HTML("<p><strong>Methods:</strong> In this multinational, retrospective study, new user cohort studies in patients with rheumatoid arthritis aged 18 years or older and initiating hydroxychloroquine were compared with those initiating sulfasalazine and followed up over 30 days, with 16 severe adverse events studied. Self-controlled case series were done to further establish safety in wider populations, and included all users of hydroxychloroquine regardless of rheumatoid arthritis status or indication. Separately, severe adverse events associated with hydroxychloroquine plus azithromycin (compared with hydroxychloroquine plus amoxicillin) were studied. Data comprised 14 sources of claims data or electronic medical records from Germany, Japan, Netherlands, Spain, the UK, and the USA. Propensity score stratification and calibration using negative control outcomes were used to address confounding. Cox models were fitted to estimate calibrated hazard ratios (HRs) according to drug use. Estimates were pooled where the I² value was less than 40%.</p>"),
                                 HTML("<p><strong>Findings:</strong> The study included 956374 users of hydroxychloroquine, 310350 users of sulfasalazine, 323122 users of hydroxychloroquine plus azithromycin, and 351956 users of hydroxychloroquine plus amoxicillin. No excess risk of severe adverse events was identified when 30-day hydroxychloroquine and sulfasalazine use were compared. Selfcontrolled case series confirmed these findings. However, long-term use of hydroxychloroquine appeared to be associated with increased cardiovascular mortality (calibrated HR 1·65 [95% CI 1·12–2·44]). Addition of azithromycin appeared to be associated with an increased risk of 30-day cardiovascular mortality (calibrated HR 2·19 [95% CI 1·22–3·95]), chest pain or angina (1·15 [1·05–1·26]), and heart failure (1·22 [1·02–1·45]).</p>"),
                                 HTML("<p><strong>Interpretation:</strong> Hydroxychloroquine treatment appears to have no increased risk in the short term among patients with rheumatoid arthritis, but in the long term it appears to be associated with excess cardiovascular mortality. The addition of azithromycin induces heart failure and increased cardiovascular mortality even in the short term. We call for careful consideration of the benefit–risk trade-off when counselling those on hydroxychloroquine treatment.</p>"),
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
