library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(paste("Opioid use, postoperative complications, and implant survival after unicompartmental versus total knee replacement: a population-based network study", if(blind) "***Blinded***" else "")),
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
                                 div(p("These research results are from a retrospective, real-world observational study to evaluate the risk of opioid use, post-operative complications, and revision with unicompartmental versus total knee replacement. The results have been reported in advance of results from an ongoing clinical trial comparing the same procedures for the risk of the same outcomes. This web-based application provides an interactive platform to explore all analysis results generated as part of this study."), style="border: 1px solid black; padding: 5px;"),
                                 HTML("<br/>"),
                                 HTML("<p>Below is the abstract of the manuscript that summarizes the findings:</p>"),
                                 HTML("<p><strong>Backgroud: </strong>There is uncertainty around whether to use unicompartmental knee replacement (UKR) or total knee replacement (TKR) for individuals with osteoarthritis confined to a single compartment of the knee. We aimed to emulate the design of the Total or Partial Knee Arthroplasty Trial (TOPKAT) using routinely collected data to assess whether the efficacy results reported in the trial translate into effectiveness in routine practice, and to assess comparative safety.</p>"),
                                 HTML("<p><strong>Methods: </strong>We did a population-based network study using data from four US and one UK health-care database, part of the Observational Health Data Sciences and Informatics network. The inclusion criteria were the same as those for TOPKAT; briefly, we identified patients aged at least 40 years with osteoarthritis who had undergone UKR or TKR and who had available data for at least one year prior to surgery. Patients were excluded if they had evidence of previous knee arthroplasty, knee fracture, knee surgery (except diagnostic), rheumatoid arthritis, infammatory arthropathies, or septic arthritis. Opioid use from 91–365 days after surgery, as a proxy for persistent pain, was assessed for all participants in all databases. Postoperative complications (ie, venous thromboembolism, infection, readmission, and mortality) were assessed over the 60 days after surgery and implant survival (as measured by revision procedures) was assessed over the 5 years after surgery. Outcomes were assessed in all databases, except for readmission, which was assessed in three of the databases, and mortality, which was assessed in two of the databases. Propensity score matched Cox proportional hazards models were fitted for each outcome. Calibrated hazard ratios (cHRs) were generated for each database to account for observed differences in control outcomes, and cHRs were then combined using meta-analysis.</p>"),
                                 HTML("<p><strong>Findings: </strong>33,867 individuals who received UKR and 557,831 individuals who received TKR between Jan 1, 2005, and April 30, 2018, were eligible for matching. 32,379 with UKR and 250,377 with TKR were propensity score matched and informed the analyses. UKR was associated with a reduced risk of postoperative opioid use (cHR from meta-analysis 0.81, 95% CI 0.73–0.90) and a reduced risk of venous thromboembolism (0.62, 0.36–0.95), whereas no difference was seen for infection (0.85, 0.51–1.37) and readmission (0.79, 0.47–1.25). Evidence was insufficient to conclude whether there was a reduction in risk of mortality. UKR was also associated with an increased risk of revision (1.64, 1.40–1.94).</p>"),
                                 HTML("<p><strong>Interpretation: </strong>UKR was associated with a reduced risk of postoperative opioid use compared with TKR, which might indicate a reduced risk of persistent pain after surgery. UKR was associated with a lower risk of venous thromboembolism but an increased risk of revision compared with TKR. These findings can help to inform shared decision making for individuals eligible for knee replacement surgery.</p>"),
                                 HTML("<br/>"),
                                 HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
                                 HTML("<ul>"),
                                 HTML("<li>The full study protocol is available at: <a href=\"https://github.com/OHDSI/StudyProtocols/tree/master/UkaTkaSafetyEffectiveness/documents\">https://github.com/OHDSI/StudyProtocols/tree/master/UkaTkaSafetyEffectiveness/documents</a></li>"),
                                 HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/OHDSI/StudyProtocols/tree/master/UkaTkaSafetyEffectiveness\">https://github.com/OHDSI/StudyProtocols/tree/master/UkaTkaSafetyEffectiveness</a></li>"),
								 HTML("<li>The results were published in the journal, The Lancet Rheumatology: <a href=\"https://www.thelancet.com/journals/lanrhe/article/PIIS2665-9913(19)30075-X/fulltext\">https://www.thelancet.com/journals/lanrhe/article/PIIS2665-9913(19)30075-X/fulltext</a></li>"),
                                 HTML("</ul>")
                        ),
                        tabPanel("Explore results",
                                fluidRow(
                                  column(4,
                                         selectInput("target", div("Target cohort:", actionLink("targetCohortsInfo", "", icon("info-circle"))), unique(exposureOfInterest$exposureName), selected = unique(exposureOfInterest$exposureName)[1], width = '100%'),
                                         selectInput("comparator", div("Comparator cohort:", actionLink("comparatorCohortsInfo", "", icon("info-circle"))), unique(exposureOfInterest$exposureName), selected = unique(exposureOfInterest$exposureName)[2], width = '100%'),
                                         selectInput("outcome", div("Outcome:", actionLink("outcomesInfo", "", icon("info-circle"))), unique(outcomeOfInterest$outcomeName), width = '100%'),
                                         checkboxGroupInput("database", div("Data source:", actionLink("dbInfo", "", icon("info-circle"))), database$databaseId, selected = database$databaseId[1], width = '100%'),
                                         checkboxGroupInput("analysis", div("Analysis specification:", actionLink("analysesInfo", "", icon("info-circle"))), cohortMethodAnalysis$description,  selected = cohortMethodAnalysis$description, width = '100%')
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
                                                                               # uiOutput("timeAtRiskTableCaption"),
                                                                               # tableOutput("timeAtRiskTable")
                                                                      ),
                                                                      tabPanel("Attrition",
                                                                               plotOutput("attritionPlot", width = 600, height = 600),
                                                                               uiOutput("attritionPlotCaption"),
                                                                               downloadButton("downloadAttritionPlot", label = "Download diagram")
                                                                      ),
                                                                      tabPanel("Population characteristics",
                                                                               uiOutput("table1Caption"),
                                                                               dataTableOutput("table1Table")),
                                                                      tabPanel("Propensity scores",
                                                                               plotOutput("psDistPlot"),
                                                                               div(strong("Figure 2."),"Preference score distribution. The preference score is a transformation of the propensity score that adjusts for differences in the sizes of the two treatment groups. A higher overlap indicates subjects in the two groups were more similar in terms of their predicted probability of receiving one treatment over the other."),
                                                                               downloadButton("downloadPsDistPlot", label = "Download plot")),
                                                                      tabPanel("Covariate balance",
                                                                               uiOutput("hoverInfoBalanceScatter"),
                                                                               plotOutput("balancePlot",
                                                                                          hover = hoverOpts("plotHoverBalanceScatter", delay = 100, delayType = "debounce")),
                                                                               uiOutput("balancePlotCaption"),
                                                                               downloadButton("downloadBalancePlot", label = "Download plot")),
                                                                      tabPanel("Systematic error",
                                                                               plotOutput("systematicErrorPlot"),
                                                                               div(strong("Figure 4."),"Systematic error. Effect size estimates for the negative controls (true hazard ratio = 1) and positive controls (true hazard ratio > 1), before and after calibration. Estimates below the diagonal dashed lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated estimator should have the true effect size within the 95 percent confidence interval 95 percent of times."),
                                                                               downloadButton("downloadSystematicErrorPlot", label = "Download plot")),
                                                                      tabPanel("Kaplan-Meier",
                                                                               plotOutput("kaplanMeierPlot", height = 550),
                                                                               uiOutput("kaplanMeierPlotPlotCaption"),
                                                                               downloadButton("downloadKaplanMeierPlot", label = "Download plot")) #,
                                                                      # tabPanel("Subgroups",
                                                                      #          uiOutput("subgroupTableCaption"),
                                                                      #          dataTableOutput("subgroupTable")) 
                                                          )
                                         )
                                  )
                                )
                        )
            )
  )
)
