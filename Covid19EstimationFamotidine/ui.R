library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(paste("Comparative Effectiveness of Famotidine in Hospitalized COVID-19 Patients", if(blind) "***Blinded***" else "")),
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
                                 HTML("<p><strong>Background:</strong> Famotidine has been posited as a potential treatment for COVID-19. We compared the incidence of COVID-19 outcomes (i.e., death; and death or intensive services use) among hospitalized famotidine users vs. proton pump inhibitors (PPIs) users, hydroxychloroquine users or famotidine non-users separately.</strong></p>"),
                                 HTML("<p><strong>Methods:</strong> We constructed a retrospective cohort study using data from COVID-19 Premier Hospital electronic health records. Study population were COVID-19 hospitalized patients aged 18 years or older. Famotidine, PPI and hydroxychloroquine exposure groups were defined as patients dispensed any medication containing one of the three drugs on the day of admission. The famotidine non-user group was derived from the same source population with no history of exposure to any drug with famotidine as an active ingredient prior to or on the day of admission. Time-at-risk was defined based on the intention-to-treat principle starting 1 day after admission to 30 days after admission. For each study comparison group, we fit a propensity score (PS) model through large-scale regularized logistic regression. The outcome was modeled using a survival model. </p>"),
                                 HTML("<p><strong>Results:</strong> We identified 2193 users of PPI, 5950 users of the hydroxychloroquine, 1816 users of famotidine and 26,820 non-famotidine users. After PS stratification, the hazard ratios for death were as follows: famotidine vs no famotidine HR 1.03 (0.89-1.18); vs PPIs: HR 1.14 (0.94-1.39); vs hydroxychloroquine:1.03 (0.85-1.24). Similar results were observed for the risk of death or intensive services use. </p>"),
                                 HTML("<p><strong>Conclusion:</strong> We found no evidence of a reduced risk of COVID-19 outcomes among hospitalized COVID-19 patients who used famotidine compared to those who did not or compared to PPI or hydroxychloroquine users. </p>"),
                                 HTML("<br/>"),
                                 HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
                                 HTML("<ul>"),
                                 HTML("<li>The full study protocol is available at:  <a href=\"https://github.com/ohdsi-studies/Covid19EstimationFamotidine/blob/master/Protocol/Covid19EstimationFamotidineProtocol.pdf\"> github.com/ohdsi-studies/Covid9EstimationFamotidine</a></li>"),
                                 HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/ohdsi-studies/Covid19EstimationFamotidine\"> github.com/ohdsi-studies/Covid19EstimationFamotidine</a></li>"),
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
