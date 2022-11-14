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
        HTML("<p><strong>Background: </strong>Administrative healthcare claims databases are used in drug safety research but are limited for investigating the impacts of prenatal exposures on child outcomes without mother-infant pair identification.</p>"),
        HTML("<p><strong>Objective: </strong>We developed a mother-infant linkage algorithm that builds on other linkage approaches and applied and evaluated it in two, large US commercially insured populations.</p>"),
        HTML("<p><strong>Study Design: </strong>We used two US commercial health insurance claims databases covering 2000 to 2021. Mother-infant links were constructed where persons of female sex 12-55 years of age with a pregnancy ending in live birth were associated with a person who was 0 years of age at database entry, who share a common insurance plan ID, had overlapping observation time, and whose date of birth was within ±60-days of the mother’s pregnancy episode end date. We compared the characteristics of linked vs non-linked mothers and linked vs non-linked infants to assess similarity.</p>"),
        HTML("<p><strong>Results: </strong>The algorithm linked 3,477,960 mothers to 4,160,284 infants in the two databases. Linked mothers and linked infants comprised 73.6% of all mothers and 49.1% of all-infants, respectively. 94.9% of linked infants dates of birth were within 4 weeks of the associated mother’s pregnancy episode end dates.  Linked mothers were older, had longer pregnancy episodes, and had greater post-pregnancy observation time than non-linked mothers. Linked infants had less observation time and greater healthcare utilization than non-linked infants. All other linked vs non-linked characteristics were similar in mothers and infants.</p>"),
        HTML("<p><strong>Conclusion: </strong>We applied a novel mother-infant linkage algorithm to two US commercial healthcare claims databases and achieved a high linkage proportion and demonstrated that linked and non-linked mother and infant cohorts were similar. This enables large-scale research on exposures during pregnancy and pediatric outcomes with relevance to drug safety. Linked vs non-linked population differences may be partially attributable to shared healthcare billing practices within insurance plan IDs among linked mothers and infants.</p>"),
        HTML("<p><strong>Key findings:</strong></p>"),
        HTML("<li>This study created the largest known mother-infant linked cohorts.</li>"),
        HTML("<li>Linked mothers and linked infants comprise a large proportion all mothers and all infants, respectively.</li>"),
        HTML("<li>Linked vs. non-linked mother and infant cohorts are similar indicating internal validity.</li>"),
        HTML("<li>Similarity between linked vs. non-linked mother and infant cohorts increases confidence that results from research on linked cohorts also apply mother and infant populations that do not meet linkage algorithm criteria.</li>"),
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
