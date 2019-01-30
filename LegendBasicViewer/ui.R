library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(
              title=div(img(src = "logo.png", height = 50, width = 50), 
                        "LEGEND basic viewer"),
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
                                 HTML("</BR><P>This app is under development. All results are preliminary and may change without notice.</P>"),
                                 HTML("</BR><P>Do not use.</P>")
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
                                          checkboxGroupInput("analysis", "Analysis", analyses$description[analyses$analysisId <= 4], selected = analyses$description[analyses$analysisId == 1])
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
                                                                                                 div(strong("Figure 8."),"Fitter null distributions per data source."))
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
