library(shiny)
library(DT)

shinyUI(
  fluidPage(
    style = "width:1500px;",
    titlePanel(
      title = paste("The necessity of validity diagnostics when drawing causal inferences from observational data", if(blind) "***Blinded***" else "")
    ),
    tags$head(
      tags$style(
        type = "text/css",
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
                }")
    ),
    conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      tags$div("Procesing...",id = "loadmessage")
    ),
    tabsetPanel(
      id = "mainTabsetPanel",
      tabPanel(
        title = "About",
        HTML("<br/>"),
        div(p("These research results are from a retrospective, real-world, observational study assessing the risk of non-infectious uveitis among patients newly exposed to Remicade® and alternative for Remicade®-indicated conditions. This web-based application provides an interactive platform to explore all analysis results and validity diagnostics generated as part of this study, as a supplement to the full manuscript"), style="border: 1px solid black; padding: 5px;"),
        HTML("<br/>"),
        HTML("<p><strong>Abstract:</strong></p>"),
        HTML("<p><strong>Background: </strong>Autoimmune disorders have primary manifestations such as joint pain and bowel inflammation but can also have secondary manifestations such as non-infectious uveitis (NIU). A regulatory health authority raised concerns after receiving spontaneous reports for NIU following exposure to Remicade®, a biologic therapy with multiple indications for which alternative therapies are available. In assessment of this clinical question, we applied validity diagnostics to support observational data causal inferences.</p>"),
        HTML("<p><strong>Methods: </strong>We assessed the risk of NIU among patients exposed to Remicade® compared to alternative biologics. Five databases, four study populations, and four analysis methodologies were used to estimate 80 potential treatment effects, with 20 pre-specified as primary. The study populations included inflammatory bowel conditions Crohn’s disease or ulcerative colitis (IBD), ankylosing spondylitis (AS), psoriatic conditions plaque psoriasis or psoriatic arthritis (PsO/PsA), and rheumatoid arthritis (RA). We conducted four analysis strategies intended to address limitations of causal estimation using observational data and applied four diagnostics with pre-specified quantitative rules to evaluate threats to validity from observed and unobserved confounding. We also qualitatively assessed post-propensity score matching representativeness, and bias susceptibility from outcome misclassification. We fit Cox proportional-hazards models, conditioned on propensity score-matched sets, to estimate the on-treatment risk of NIU among Remicade® initiators versus alternatives. Estimates from analyses that passed four validity tests were assessed.</p>"),
        HTML("<p><strong>Results: </strong>Of the 80 total analyses and the 20 analyses pre-specified as primary, 24% and 20% passed diagnostics, respectively. Among patients with IBD, we observed no evidence of increased risk for NIU relative to other similarly indicated biologics (pooled hazard ratio [HR] 0.75, 95% confidence interval [CI] 0.38-1.40). For patients with RA, we observed no increased risk relative to similarly indicated biologics, although results were imprecise (HR: 1.23, 95% CI 0.14-10.47).</p>"),
        HTML("<p><strong>Conclusion: </strong>We applied validity diagnostics on a heterogenous, observational setting to answer a specific research question. The results indicated that safety effect estimates from many analyses would be inappropriate to interpret as causal, given the data available and methods employed. Validity diagnostics should always be used to determine if the design and analysis are of sufficient quality to support causal inferences. The clinical implications of our findings on IBD suggests that, if an increased risk exists, it is unlikely to be greater than 40% given the 1.40 upper bound of the pooled HR confidence interval.</p>"),
        HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
        HTML("<ul>"),
        HTML("<li>The full manuscript is available at: ..."),
        HTML("<li>The full manuscript is available at: <a href=\"https://github.com/ohdsi-studies/UveitisSafetyEstimation/tree/master/Documents\">https://github.com/ohdsi-studies/UveitisSafetyEstimation/tree/master/Documents</a></li>"),
        HTML("<li>The full source code for the study is available at: <a href=\"https://github.com/ohdsi-studies/UveitisSafetyEstimation/\">https://github.com/ohdsi-studies/UveitisSafetyEstimation</a></li>"),
        HTML("</ul>")
      ),
      tabPanel(
        title = "Explore results",
        fluidRow(
          column(
            width = 3,
            selectInput("target", "Target", unique(exposureOfInterest$exposureName), selected = unique(exposureOfInterest$exposureName)[2]),
            selectInput("comparator", "Comparator", unique(exposureOfInterest$exposureName), selected = unique(exposureOfInterest$exposureName)[6]),
            selectInput("outcome", "Outcome", unique(outcomeOfInterest$outcomeName)),
            checkboxGroupInput("database", "Data source", database$databaseId, selected = database$databaseId),
            checkboxGroupInput("analysis", "Analysis", cohortMethodAnalysis$description,  selected = cohortMethodAnalysis$description[3])
          ),
          column(
            width = 9,
            dataTableOutput(
              outputId = "mainTable"
            ),
            conditionalPanel(
              condition = "output.rowIsSelected == true",
              tabsetPanel(
                id = "detailsTabsetPanel",
                tabPanel(
                  title = "Power",
                  uiOutput("powerTableCaption"),
                  tableOutput("powerTable")
                ),
                tabPanel(
                  title = "Attrition",
                  uiOutput("attritionPlotCaption"),
                  plotOutput(
                    outputId = "attritionPlot",
                    width = 600,
                    height = 600
                  )
                ),

                tabPanel(
                  title = "Target-analytic balance",
                  uiOutput("targetAnalyticBalanceCaption"),
                  dataTableOutput("targetAnalyticTable")
                ),

                tabPanel(
                  title = "Target-analytic plot",
                  # uiOutput("targetAnalyticPlotCaption"),
                  # uiOutput("hoverInfoBalanceScatter"),
                  plotOutput(outputId = "targetAnalyticPlot")
                ),

                tabPanel(
                  title = "Population characteristics",
                  uiOutput("table1Caption"),
                  radioButtons("charType", "", c("Pretty", "Raw"), selected = "Pretty", inline = TRUE),
                  dataTableOutput("table1Table")
                ),
                tabPanel(
                  title = "Propensity model",
                  div(strong("Table 3."),"Fitted propensity model, listing all coviates with non-zero coefficients. Positive coefficients indicate predictive of the target exposure."),
                  dataTableOutput("propensityModelTable")
                ),
                tabPanel(
                  title = "Propensity scores",
                  div(strong("Figure 2."), "Preference score distribution. The preference score is a transformation of the propensity score that adjusts for differences in the sizes of the two treatment groups. A higher overlap indicates subjects in the two groups were more similar in terms of their predicted probability of receiving one treatment over the other."),
                  plotOutput("psDistPlot")
                ),
                tabPanel(
                  title = "Covariate balance",
                  uiOutput("balancePlotCaption"),
                  uiOutput("hoverInfoBalanceScatter"),
                  plotOutput(
                    outputId = "balancePlot",
                    hover = hoverOpts("plotHoverBalanceScatter", delay = 100, delayType = "debounce")
                  )
                ),
                tabPanel(
                  title = "Systematic error",
                  div(strong("Figure 4."),"Systematic error. Effect size estimates for the negative controls (true hazard ratio = 1) and positive controls (true hazard ratio > 1), before and after calibration. Estimates below the diagonal dashed lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated estimator should have the true effect size within the 95 percent confidence interval 95 percent of times."),
                  plotOutput("systematicErrorPlot")
                ),
                tabPanel(
                  title = "Kaplan-Meier",
                  plotOutput("kaplanMeierPlot", height = 550),
                  uiOutput("kaplanMeierPlotPlotCaption")
                ),
                tabPanel(
                  title = "Forest plot",
                  uiOutput("hoverInfoForestPlot"),
                  plotOutput(
                    outputId = "forestPlot",
                    height = 450,
                    hover = hoverOpts("plotHoverForestPlot", delay = 100, delayType = "debounce")
                  ),
                  uiOutput("forestPlotCaption")
                )
              )

            )
          )
        )
      )
    )
  )
)
