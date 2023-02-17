library(shiny)
library(DT)

shinyUI(
  fluidPage(#style = "width:1500px;",
    titlePanel("Empirical of QBA for outcome phenotype error correction in comparative effect estimation"),
    tags$head(tags$style(type = "text/css",
      "#loadmessage {
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
      }")),
    conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                     tags$div("Procesing...",id = "loadmessage")),
    tabsetPanel(id = "mainTabsetPanel",
      tabPanel("About",
        HTML("<br/>"),
        div(p("UNDER DEVELOPMENT - DO NOT USE OR REPORT"), style="border: 1px solid black; padding: 5px;"),
        HTML("<br/>"),
        HTML("<p><strong>Abstract: </strong></p>"),
        HTML("<p><strong>Background: </strong>BACKGROUND</p>"),
  		  HTML("<p><strong>Objective: </strong>OBJECTIVE</p>"),
  		  HTML("<p><strong>Methods: </strong>METHODS</p>"),
  		  HTML("<p><strong>Results: </strong>RESULTS</p>"),
  		  HTML("<p><strong>Discussion: </strong>DISCUSSION</p>"),
        HTML("<p><strong>Key points:</strong></p>"),
        HTML("<li>POINT 1.</li>"),
        HTML("<li>POINT 2.</li>"),
        HTML("<li>POINT 3.</li>"),
        HTML("<br/>"),
        HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
        HTML("<ul>"),
        HTML("<li>The full manuscript is available at: <a href=\"https://data.ohdsi.org/\">https://data.ohdsi.org/</a></li>"),
        HTML("<li>The study was registered at XXX: <a href=\"https://data.ohdsi.org/\">https://data.ohdsi.org/</a></li>"),
        HTML("<li>The full study protocol is available at: <a href=\"https://data.ohdsi.org/\">https://data.ohdsi.org/</a></li>"),
        
  		  HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/ohdsi-studies/QbaEvaluation/\">https://github.com/ohdsi-studies/QbaEvaluation/</a></li>"),
  		  
        HTML("</ul>")),
    tabPanel("Simple QBA",
      fluidRow(
        column(3,
          selectInput("target", "Target", unique(exposureOfInterest$exposureName)[1], selected = unique(exposureOfInterest$exposureName)[1]),
          selectInput("comparator", "Comparator", unique(exposureOfInterest$exposureName)[c(2,3)], selected = unique(exposureOfInterest$exposureName)[3]),
          selectInput("outcome", "Outcome", unique(outcomeOfInterest$outcomeName), selected = unique(outcomeOfInterest$outcomeName)),
          checkboxGroupInput("database", "Data source", database$databaseId, selected = database$databaseId[1]),
          checkboxGroupInput("analysis", "Analysis", cohortMethodAnalysis$description,  selected = cohortMethodAnalysis$description[7:12])),
        column(9,
          dataTableOutput("mainTable"),
          conditionalPanel(condition = "output.rowIsSelected == false",
            tabsetPanel(id = "forestPlotTabsetPanel",
                        tabPanel("ForestPlot",
                        plotOutput("forestPlot", height = 300)),
                        tabPanel("Validation",
                                 uiOutput("validationTableCaption"),
                                 tableOutput("validationTable")))),
                     
          conditionalPanel(condition = "output.rowIsSelected == true",
            tabsetPanel(id = "detailsTabsetPanel",
                        tabPanel("Counts table",
                                 uiOutput("countsTableCaption"),
                                 tableOutput("countsTable"),
                                 tableOutput("powerTable")),
                        tabPanel("Attrition",
                                 uiOutput("attritionPlotCaption"),
                                 plotOutput("attritionPlot", width = 600, height = 600),
                                 div(style = "display: inline-block;vertical-align:top;")),
                        tabPanel("Population characteristics",
                                 uiOutput("table1Caption"),
                                 radioButtons("charType", "", c("Pretty", "Raw"), selected = "Pretty", inline = TRUE),
                                 dataTableOutput("table1Table")),
                        tabPanel("Propensity model",
                                 div(strong("Table 3."), "Fitted propensity model"),
                                 dataTableOutput("propensityModelTable")),
                        tabPanel("Propensity scores",
                                 plotOutput("psDistPlot"),
                                 div(strong("Figure 2."), "Preference score distribution"),
                                 div(style = "display: inline-block;vertical-align:top;")),
                        tabPanel("Covariate balance",
                                 uiOutput("hoverInfoBalanceScatter"),
                                 plotOutput("balancePlot",
                                             hover = hoverOpts("plotHoverBalanceScatter", delay = 100, delayType = "debounce")),
                                 uiOutput("balancePlotCaption"),
                                 div(style = "display: inline-block;vertical-align:top;"))
            )
          )
        )
      )
    ),
    tabPanel("Non-differential multidimensional QBA",
      fluidRow(
        column(3,
          selectInput("mdTarget", "Target", unique(exposureOfInterest$exposureName)[1], selected = unique(exposureOfInterest$exposureName)[1]),
          selectInput("mdComparator", "Comparator", unique(exposureOfInterest$exposureName)[c(2,3)], selected = unique(exposureOfInterest$exposureName)[3]),
          selectInput("mdOutcome", "Outcome", unique(outcomeOfInterest$outcomeName), selected = unique(outcomeOfInterest$outcomeName)),
          selectInput("mdDatabase", "Data source", database$databaseId, selected = database$databaseId[1]),
          selectInput("mdAnalysis", "Analysis", cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId %in% c(1,3,5,7,9,11)], selected = cohortMethodAnalysis$description[1]),
          sliderInput("sens", "Sensitivity", min = 0, max = 1, value = 1, step = 0.0001),
          sliderInput("spec", "Specificity", min = 0, max = 1, value = 1, step = 0.0001)),
        column(9,
          uiOutput("mdCountsTableCaption1"),
          tableOutput("mdCountsTable1a"),
          tableOutput("mdCountsTable1b"),
          uiOutput("mdCountsTableCaption2"),
          tableOutput("mdCountsTable2a"),
          tableOutput("mdCountsTable2b"),
          plotOutput("mdForestPlot", width = 600, height = 200)
        )
      )
    )#,
    # tabPanel("Simulation",
    #   fluidRow(
    #     column(3,
    #       selectInput("incidence", "Incidence proportion", unique(correctedOrs$incidence), selected = unique(correctedOrs$incidence)[1])#,
    #       #selectInput("or", "Observed odds ratio", unique(correctedOrs$or), selected = unique(correctedOrs$or)[1]),
    #       ),
    #    column(9,
    #       uiOutput("contourPlotCaption"),
    #       plotOutput("contourPlot")
    #    )
    #  )
    # )
  )
)
)
