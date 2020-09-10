library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(paste("Risk of depression, suicidal ideation, suicide and psychosis with hydroxychloroquine treatment for rheumatoid arthritis: a multi-national network cohort study", if(blind) "***Blinded***" else "")),
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
                                 HTML("<p><strong>Objectives:</strong> Recent regulatory warnings suggest that high-dose hydroxychloroquine for novel coronavirus disease 2019 (COVID-19) could cause depression or suicidal ideation. We aimed to study whether there is risk of depression, suicidal ideation, or psychosis associated with hydroxychloroquine use for rheumatoid arthritis (RA).</p>"),
                                 HTML("<p><strong>Methods:</strong> A new user cohort study using claims and electronic medical records from 10 sources and 3 countries (Germany, UK, and US) was used. Rheumatoid arthritis patients aged 18+ and initiating hydroxychloroquine were compared to those initiating sulfasalazine (active comparator) and followed up in the short (30-day) and long term (on treatment). Study outcomes were depression, suicide/suicidal ideation, and hospitalization for psychosis. Propensity score stratification and calibration using negative control outcomes were used to address confounding. Cox models were fitted to estimate database-specific calibrated hazard ratios (HR), with estimates pooled where I2<40%.</p>"),
                                 HTML("<p><strong>Results:</strong> Overall, 918,144 and 290,383 users of hydroxychloroquine and sulfasalazine, respectively, were included. No consistent risk of psychiatric events was observed with short-term hydroxychloroquine (compared to sulfasalazine) use, with meta-analytic HRs of 0.96 [95% CI 0.79-1.16] for depression, 0.94 [0.49-1.77] for suicide/suicidal ideation, and 1.03 [0.66-1.60] for psychosis. Long-term effects were similar, with meta-analytic HRs 0.94 [0.71-1.26] for depression, 0.77 [0.56-1.07] for suicide/suicidal ideation, and 0.99 [0.72-1.35] for psychosis.</p>"),
                                 HTML("<p><strong>Conclusions:</strong> Hydroxychloroquine as used to treat RA does not appear to increase the risk of depression, suicide/suicidal ideation, or psychosis compared to sulfasalazine. No effects were seen in the short (first month of treatment) or in the long term. Use at higher dose or for different indications might have other effects and needs further investigation.</p>"),
                                 HTML("<br/>"),
                                 HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
                                 HTML("<ul>"),
                                 HTML("<li>This study was registered at ENCePP (EU PAS Register number EUPAS34497): <a href=\"c\">http://www.encepp.eu/encepp/viewResource.htm?id=34498</a></li>"),
                                 HTML("<li>The full study protocol is available at:  <a href=\"https://github.com/ohdsi-studies/Covid19EstimationHydroxychloroquine2/tree/master/documents\">https://github.com/ohdsi-studies/Covid19EstimationHydroxychloroquine2/tree/master/documents</a></li>"),
                                 HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/ohdsi-studies/Covid19EstimationHydroxychloroquine2\">https://github.com/ohdsi-studies/Covid19EstimationHydroxychloroquine2</a></li>"),
                                 HTML("</ul>")
                        ),
                        tabPanel("Explore results",

                        fluidRow(
                          column(3,
                                 selectInput("target", "Target", unique(exposureOfInterest$exposureName)[1], selected = unique(exposureOfInterest$exposureName)[1], width = '100%'),
                                 selectInput("comparator", "Comparator", unique(exposureOfInterest$exposureName)[2], selected = unique(exposureOfInterest$exposureName)[2], width = '100%'),
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
