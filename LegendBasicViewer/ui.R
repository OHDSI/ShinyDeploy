library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(
              title=div(img(src = "logo.png", height = 50, width = 50), 
                        "LEGEND Basic Viewer"),
              windowTitle = "LEGEND basic viewer"),
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
                             tags$div("Processing...",id = "loadmessage")),
            tabsetPanel(id = "mainTabsetPanel",
                        tabPanel("About",
                                 br(),
                                 p("This web-based application provides an interactive platform to explore all analysis results generated as part of the OHDSI LEGEND study."),
                                 p("These results are featured in:"),
                                 tags$ul(
                                   tags$li(HTML("Suchard MA, Schuemie MJ, Krumholz H, You SC, Chen RJ, Pratt N, Reich CG, Duke J, Madigan D, Hripcsak G, Ryan PB,
              <a href = \"https://doi.org/10.1016/S0140-6736(19)32317-7\">
                  Comprehensive comparative effectiveness and safety of first-line antihypertensive drug classes: a systematic, multinational, large-scale analysis.
              </a>Lancet, 2019")),
                                   tags$li(HTML("Hripcsak G, Suchard MA, Shea S, Chen RJ, You SC, Pratt N, Madigan D, Krumholz H, Ryan PB, Schuemie MJ, <a href = \"https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/2760777\">
                  Comparison of Cardiovascular and Safety Outcomes of Chlorthalidone vs Hydrochlorothiazide to Treat Hypertension.
              </a>JAMA Internal Medicine, 2019")),
                                   tags$li(HTML("You, SC, Krumholz HM, Suchard MA, Schuemie MJ, Hripcsak G, Chen RJ, Shea S, Duke J, Pratt N, Reich CG, Madigan D, Ryan PB, Park RW, Park SH, <a href = \"https://www.ahajournals.org/doi/10.1161/HYPERTENSIONAHA.120.16402\">
                  Comprehensive Comparative Effectiveness and Safety of First-Line β-Blocker Monotherapy in Hypertensive Patients: A Large-Scale Multicenter Observational Study.
              </a>Hypertension, 2021"))
                                 ),
                                 h3("External links"),
                                 HTML("<ul>
                    <li>Protocol: <a href = \"https://github.com/OHDSI/Legend/blob/master/Documents/OHDSI%20Legend%20Protocol%20Hypertension%20V03.docx\">https://github.com/OHDSI/Legend/blob/master/Documents/OHDSI%20Legend%20Protocol%20Hypertension%20V03.docx</a></li>
                    <li>Analysis source code: <a href = \"https://github.com/OHDSI/Legend\">https://github.com/OHDSI/Legend</a></li>
                  </ul>")
                        ),
                        tabPanel("Specific research questions",
                                 fluidRow(
                                   column(3,
                                          selectInput("indication", "Indication", indications$indicationId, selected = "Hypertension"),
                                          selectInput("exposureGroup", "Exposure group", unique(exposureGroups$exposureGroup), selected = "Drug major class"),
                                          checkboxInput("includeCombis", "Include combination exposures", FALSE),
                                          selectInput("target", "Target", unique(exposures$exposureName), selected = "Calcium channel blockers (CCB)"),
                                          selectInput("comparator", "Comparator", unique(exposures$exposureName), selected = "Beta blockers"),
                                          selectInput("outcome", "Outcome", unique(outcomes$outcomeName)),
                                          checkboxGroupInput("database", "Data source", databases$databaseId, selected = databases$databaseId),
                                          checkboxGroupInput("analysis", "Analysis", analyses$description[analyses$analysisId <= 4], selected = analyses$description[analyses$analysisId %in% c(1,3)])
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
                                                                                uiOutput("attritionPlotCaption")
                                                                       ),
                                                                       tabPanel("Population characteristics",
                                                                                uiOutput("table1Caption"),
                                                                                dataTableOutput("table1Table")),
                                                                       tabPanel("Propensity scores",
                                                                                plotOutput("psDistPlot"),
                                                                                div(strong("Figure 2."),"Preference score distribution. The preference score is a transformation of the propensity score
                                                                                                         that adjusts for differences in the sizes of the two treatment groups. A higher overlap indicates subjects in the
                                                                                                         two groups were more similar in terms of their predicted probability of receiving one treatment over the other.")),
                                                                       tabPanel("Covariate balance",
                                                                                conditionalPanel("output.isMetaAnalysis == false",
                                                                                                 uiOutput("hoverInfoBalanceScatter"),
                                                                                                 plotOutput("balancePlot",
                                                                                                            hover = hoverOpts("plotHoverBalanceScatter", delay = 100, delayType = "debounce")),
                                                                                                 uiOutput("balancePlotCaption")),
                                                                                conditionalPanel("output.isMetaAnalysis == true",
                                                                                                 plotOutput("balanceSummaryPlot"),
                                                                                                 uiOutput("balanceSummaryPlotCaption"))
                                                                       ),
                                                                       tabPanel("Systematic error",
                                                                                plotOutput("systematicErrorPlot"),
                                                                                div(strong("Figure 4."),"Systematic error. Effect size estimates for the negative controls (true hazard ratio = 1)
                                                                                    and positive controls (true hazard ratio > 1), before and after calibration. Estimates below the diagonal dashed
                                                                                    lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated
                                                                                    estimator should have the true effect size within the 95 percent confidence interval 95 percent of times."),
                                                                                conditionalPanel("output.isMetaAnalysis == true",
                                                                                                 plotOutput("systematicErrorSummaryPlot"),
                                                                                                 div(strong("Figure 8."),"Fitted null distributions per data source."))
                                                                       ),
                                                                       tabPanel("Forest plot",
                                                                                plotOutput("forestPlot"),
                                                                                uiOutput("forestPlotCaption")),
                                                                       tabPanel("Kaplan-Meier",
                                                                                plotOutput("kaplanMeierPlot", height = 550),
                                                                                uiOutput("kaplanMeierPlotPlotCaption"))
                                                           )
                                          )
                                   )

                                 )
                        )
            )
  )
)
