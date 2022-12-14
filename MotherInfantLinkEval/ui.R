library(shiny)
library(ggplot2)
library(magrittr)
library(DT)

shinyUI(
  fluidPage(

    titlePanel(title = "Mother-infant linkage algorithm evaluation"),

    tabsetPanel(id = "main",
      tabPanel(title = "About",
        HTML("<br/>"),
        div(p("This research developed and evaluated an algorithm to link mothers and infants in two observational US administrative databases to facilitate research on prenatal medication exposure and infant health outcomes."), style = "border: 1px solid black; padding: 5px;"),
        HTML("<br/>"),
        HTML("<p><strong>Abstract: </strong></p>"),
        HTML("<p><strong>Background: </strong>Administrative healthcare claims databases are used in drug safety research but are limited for investigating the impacts of prenatal exposures on neonatal and pediatric outcomes without mother-infant pair identification.</p>"),
        HTML("<p><strong>Objective: </strong>We developed a mother-infant linkage algorithm and evaluated it in two, large US commercially insured populations.</p>"),
        HTML("<p><strong>Study Design: </strong>We used two US commercial health insurance claims databases during the years 2000 to 2021. Mother-infant links were constructed where persons of female sex 12-55 years of age with a pregnancy episode ending in live birth were associated with a person who was 0 years of age at database entry, who shared a common insurance plan ID, had overlapping insurance coverage time, and whose date of birth was within ±60-days of the mother’s pregnancy episode live birth date. We compared the characteristics of linked vs non-linked mothers and infants to assess similarity.</p>"),
        HTML("<p><strong>Results: </strong>The algorithm linked 3,477,960 mothers to 4,160,284 infants in the two databases. Linked mothers and linked infants comprised 73.6% of all mothers and 49.1% of all-infants, respectively. 94.9% of linked infants’ dates of birth were within ±30-days of the associated mother’s pregnancy episode end dates. Linked mothers were older, had longer pregnancy episodes, and had greater post-pregnancy observation time than mothers with live births who did not meet linkage algorithm criteria. Linked infants had less observation time and greater healthcare utilization than non-linked infants. Other characteristics were similar in linked vs non-linked mothers and infants.</p>"),
        HTML("<p><strong>Conclusion: </strong>We developed a mother-infant linkage algorithm and applied it to two US commercial healthcare claims databases that achieved a high linkage proportion and demonstrated that linked and non-linked mother and infant cohorts were similar. Transparent, reusable algorithms applied to large databases enables large-scale research on exposures during pregnancy and pediatric outcomes with relevance to drug safety. These features suggest that prenatal exposure causal risk assessment that uses this algorithm can produce valid and generalizable evidence to inform clinical, policy, and regulatory decisions.</p>"),
        HTML("<p><strong>Key findings:</strong></p>"),
        HTML("<li>This study establishes reliable mother-infant links in two US commercial healthcare databases to facilitate research on prenatal exposures and infant health outcomes</li>"),
        HTML("<li>Linked mothers with live births comprise 73.6% of all mothers with live births and linked infants comprise 49.1% of all infants</li>"),
        HTML("<li>Linked vs. non-linked mother and infant cohorts have similar demographic and clinical profiles</li>"),
        HTML("<li>Substantial linked coverage and linked vs non-linked characteristic similarity suggests that prenatal exposure causal risk assessment using the linked cohorts will produce valid and generalizable evidence</li>"),
        HTML("<li>This study created large mother-infant linked cohorts to enable research on rare exposures and outcomes available in healthcare claims databases</li>"),
        HTML("<li>Linked mother and infant coverage is similar to that reported in previous linkage studies</li>"),
        HTML("<li>Descriptive comparisons between linked vs. non-linked mother and infant cohorts increases confidence that results from research on linked cohorts also apply to mother and infant populations that do not meet linkage algorithm criteria</li>"),
        HTML("<li>This mother-infant linkage algorithm is publicly available and easily implemented in databases converted to a common data model</li>"),
        HTML("<br/>"),
        HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
        HTML("<ul>"),
        HTML("<li>The full manuscript is available at: <a href=\"https://data.ohdsi.org/\">https://data.ohdsi.org/</a></li>"),
        HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/ohdsi-studies/MotherInfantLinkEval/\">https://github.com/ohdsi-studies/MotherInfantLinkEval</a></li>"),
        HTML("</ul>")
      ),
      tabPanel(title = "Mothers: pregnancy start",
        fluidRow(
          column(width = 2,
                 selectInput(inputId = "databasePregStart",
                             label = "Database",
                             choices = databaseRef),
                 selectInput(inputId = "analysisPregStart",
                             label = "Analysis",
                             choices = analysisRef),
                 checkboxGroupInput(inputId = "domainPregStart",
                                    label = "Domain",
                                    choices = domainRef,
                                    selected = domainRef),
                 selectInput(inputId = "continousPregStart",
                             label = "Dichotomous",
                             choices = c("Y", "N"))
          ),
          column(width = 10,
                 tags$h4("Index: pregnancy start"),
                 align = "left",
                 plotOutput(outputId = "motherPregStartPlot",
                            hover = "motherPregStartPlotHover"),
                 uiOutput(outputId = "motherPregStartPlotHover"),
                 dataTableOutput("motherPregStartTable"))
        )
      ),
      tabPanel(title = "Mothers: pregnancy end",
               fluidRow(
                 column(width = 2,
                        selectInput(inputId = "databasePregEnd",
                                    label = "Database",
                                    choices = databaseRef),
                        selectInput(inputId = "analysisPregEnd",
                                    label = "Analysis",
                                    choices = analysisRef),
                        checkboxGroupInput(inputId = "domainPregEnd",
                                           label = "Domain",
                                           choices = domainRef,
                                           selected = domainRef),
                        selectInput(inputId = "continousPregEnd",
                                    label = "Dichotomous",
                                    choices = c("Y", "N"))
                 ),
                 column(width = 10,
                        tags$h4("Index: pregnancy end"),
                        align = "left",
                        plotOutput(outputId = "motherPregEndPlot",
                                   hover = "motherPregEndPlotHover"),
                        uiOutput(outputId = "motherPregEndPlotHover"),
                        dataTableOutput("motherPregEndTable"))
               )
      ),
      tabPanel(title = "Infants",
        fluidRow(
          column(width = 2,
                 selectInput(inputId = "databaseInfant",
                             label = "Database",
                             choices = databaseRef),
                 selectInput(inputId = "analysisInfant",
                             label = "Analysis",
                             choices = analysisRef),
                 checkboxGroupInput(inputId = "domainInfant",
                                    label = "Domain",
                                    choices = domainRef,
                                    selected = domainRef),
                 selectInput(inputId = "continousInfant",
                             label = "Dichotomous",
                             choices = c("Y", "N"))
          ),
          column(width = 10,
                 tags$h4("Index: birth"),
                 align = "left",
                 plotOutput(outputId = "infantPlot",
                            hover = "infantPlotHover"),
                 uiOutput("infantPlotHover"),
                 dataTableOutput("infantTable"))
        )
      )
    )
  )
)
