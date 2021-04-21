library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(paste("The comparative safety of first-line disease-modifying antirheumatic drugs in rheumatoid arthritis: a multinational network cohort study", if(blind) "***Blinded***" else "")),
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
                                 div(p("These research results are from a retrospective, real-world, observational study to estimate the population-level effects of conventional synthetic disease-modifying antirheumatic drugs among patients with rheumatoid arthritis. This web-based application provides an interactive platform to explore all analysis results generated as part of this study, as a supplement to abstracts and a full manuscript currently in development for submission to scientific conferences and a peer-reviewed journal. During abstract and manuscript development and the subsequent review period, these results are considered under embargo and should not be disclosed without explicit permission and consent from the authors."), style="border: 1px solid black; padding: 5px;"),
                                 HTML("<br/>"),
                                 HTML("<p>Below is the abstract of the manuscript that summarizes the findings:</p>"),
                                 HTML("<p><strong>Objective: </strong>There is a lack of evidence on the comparative safety of conventional synthetic disease modifying antirheumatic drugs (csDMARDs). We assessed the comparative risks associated with first line csDMARDs in rheumatoid arthritis (RA).</p>"),
                                 HTML("<p><strong>Design: </strong>Multinational network cohort study with meta-analysis.</p>"),
                                 HTML("<p><strong>Setting: </strong>Routine health data from 8 databases (5 US, 1 UK, 1 Germany, and 1 Spain) mapped to a common data model.</p>"),
                								 HTML("<p><strong>Participants: </strong>New users of monotherapy csDMARD after RA diagnosis at age 18+ from 2005-2019.</p>"),
                								 HTML("<p><strong>Interventions: </strong>The four most commonly used first line csDMARDs for RA: methotrexate (MTX), hydroxychloroquine (HCQ), sulfasalazine (SSZ), and leflunomide (LEF). MTX was the reference drug for comparisons.</p>"),
                								 HTML("<p><strong>Main outcome measures: </strong>Adverse events of interest included leuko/pancytopenia, infection, myocardial infarction, stroke, and cancer. Cox regression after propensity score stratification was used to estimate hazard ratios (HRs) for the risk of each event according to drug use, with MTX as reference. Negative control outcomes were used to estimate calibrated HRs (cHRs). Findings were meta-analysed where I<sup>2</sup><40%.</p>"),
                								 HTML("<p><strong>Results: </strong>In total, 247,511 participants were included: 141,647 (57%) MTX, 73,286 (30%) HCQ, 16,521 (7%) SSZ, and 16,057 (6%) LEF. Compared to MTX, SSZ was associated with a lower risk of infection: pooled cHRs 0.74 (95% confidence interval: 0.55-1.00) for serious infections, and 0.72 (0.57-0.90) for any infection. SSZ may also confer a reduced risk of myocardial infarction (0.86, 0.62-1.18) and stroke (0.82, 0.61-1.12), but also an increased risk of leukopenia (1.41, 0.91-2.18). While not significant in meta-analyses, risk estimates implied LEF may be associated with reduced risk of leukopenia (0.68, 0.42-1.11), pancytopenia (0.51, 0.24-1.07), any infection (0.74, 0.51-1.06), and cancer (0.77, 0.53-1.11) compared to MTX. Meanwhile, HCQ appeared associated with a reduced risk of myocardial infarction (0.93, 0.83-1.04) and stroke (0.88, 0.78-0.98).</p>"),
                                 HTML("<p><strong>Conclusions: </strong>There are differential risks associated with each of the studied csDMARDs. These findings will inform personalized treatment for newly diagnosed RA patients.</p>"),
                                 HTML("<br/>"),
                                 HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
                                 HTML("<ul>"),
                                 HTML("<li>The full study protocol is available at: <a href=\"https://github.com/ohdsi-studies/EhdenRaDmardsEstimation/tree/master/documents\">https://github.com/ohdsi-studies/EhdenRaDmardsEstimation/tree/master/documents</a></li>"),
                                 HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/ohdsi-studies/EhdenRaDmardsEstimation\">https://github.com/ohdsi-studies/EhdenRaDmardsEstimation</a></li>"),
                                 HTML("</ul>")
                        ),
                        tabPanel("Explore results",
                                
                                 fluidRow(
                                  column(4,
                                         selectInput("target", "Target", unique(exposureOfInterest$exposureName)[1:3], selected = unique(exposureOfInterest$exposureName)[1], width = '100%'),
                                         selectInput("comparator", "Comparator", unique(exposureOfInterest$exposureName)[4], selected = unique(exposureOfInterest$exposureName)[4], width = '100%'),
                                         selectInput("outcome", "Outcome", unique(outcomeOfInterest$outcomeName), width = '100%'),
                                         checkboxGroupInput("database", "Data source", database$databaseId, selected = database$databaseId[15], width = '100%'),
                                         checkboxGroupInput("analysis", "Analysis", cohortMethodAnalysis$description,  selected = cohortMethodAnalysis$description[c(1, 2, 5, 6, 9)], width = '100%')
                                  ),
                                  column(8,
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
