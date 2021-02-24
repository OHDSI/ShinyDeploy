shinyWidgetsPickerOptions <- shinyWidgets::pickerOptions(
  actionsBox = TRUE,
  liveSearch = TRUE,
  liveSearchNormalize = TRUE,
  size = 'auto',
  liveSearchStyle = "contains",
  liveSearchPlaceholder = "Not selected",
  virtualScroll = 50,
  # mobile = TRUE,
  selectOnTab = TRUE,
  showTick = TRUE,
  width	= 'auto',
  windowPadding = 2,
  dropdownAlignRight = TRUE,
  dropupAuto = TRUE
)

defaultHeaderbars <- 
  tags$li(
    tags$div(
      tags$li(
        tags$div(
          tags$li(
            tags$div(
              shiny::conditionalPanel(
                condition = "output.isHeaderbarVisible == true",
                shinyWidgets::pickerInput(
                  inputId = "selectedDatabases",
                  label = "Database",
                  choices = NULL,
                  inline = TRUE,
                  multiple = TRUE,
                  options = shinyWidgetsPickerOptions
                ))
            ),
            class = "dropdown"
          ),
          tags$li(
            tags$div(
              shiny::conditionalPanel(
                condition = "output.isHeaderbarVisible == true",
                shinyWidgets::pickerInput(
                  inputId = "selectedCohorts",
                  label = "Cohorts",
                  choices = NULL,
                  inline = TRUE,
                  multiple = TRUE,
                  options = shinyWidgetsPickerOptions
                ))
            ),
            class = "dropdown"
          ),
          tags$li(
            tags$div(
              if (exists("phenotypeDescription") && nrow(phenotypeDescription) > 0) {
                shiny::conditionalPanel(
                  condition = "output.isHeaderbarVisible == true",
                  shinyWidgets::pickerInput(
                    inputId = "selectedPhenotypes",
                    label = "Phenotype",
                    choices = NULL,
                    inline = TRUE,
                    multiple = TRUE,
                    options = shinyWidgetsPickerOptions
                  ))
              }
            ),
            class = "dropdown"
          )
        ),
        class = "dropdown"
      )
    ),
    class = "dropdown"
  )

header <-
  shinydashboard::dashboardHeader(
    title = appTitle,
    tags$li(
      tags$div(
        defaultHeaderbars
      ),
      class = "dropdown"
    )
  )

#sidebarMenu
sidebarMenu <-
  shinydashboard::sidebarMenu(
    id = "tabs",
    shinydashboard::sidebarMenuOutput(outputId = "menuItems"),
    htmltools::withTags(
      div(style = "margin-left : 10px",
          h5(appVersion)
      )
    )
    # if (exists(x = "phenotypeDescription"))
      # shinydashboard::menuItem(text = "Phenotype Description", tabName = "phenotypeDescription"),
    # if (exists(x = "cohort"))
      # shinydashboard::menuItem(text = "Cohort Definition", tabName = "cohortDefinition"),
    
    
    # if (exists(x = "includedSourceConcept"))
    #   addInfo(
    #     item = shinydashboard::menuItem(text = "Included (Source) Concepts", tabName = "includedConcepts"),
    #     infoId = "includedConceptsInfo"
    #   ),
    # if (exists(x = "orphanConcept"))
    #   addInfo(
    #     item = shinydashboard::menuItem(text = "Orphan (Source) Concepts", tabName = "orphanConcepts"),
    #     infoId = "orphanConceptsInfo"
    #   ),
    # if (exists(x = "recommenderSet"))
    #   addInfo(
    #     item = shinydashboard::menuItem(text = "Concept Set Diagnostics MOVE TO COHORTS", tabName = "conceptSetDiagnostics"),
    #     infoId = "conceptSetDiagnosticsInfo"
    #   ),
    # if (exists(x = "covariateValue"))
    #   addInfo(
    #     item = shinydashboard::menuItem(text = "Compare Cohort Char.", tabName = "compareCohortCharacterization"),
    #     infoId = "compareCohortCharacterizationInfo"
    #   ),
    
  )

#Side bar code
sidebar <-
  shinydashboard::dashboardSidebar(sidebarMenu, width = NULL, collapsed = FALSE)

# Body - items in tabs --------------------------------------------------
bodyTabItems <- shinydashboard::tabItems(
  shinydashboard::tabItem(tabName = "search",
                          shinydashboard::box(
                            title = NULL,
                            width = NULL,
                            status = "primary",
                            shiny::column(3),
                            shiny::column(6,
                                          shiny::textInput(
                                            inputId = "searchText",
                                            label = "Search",
                                            placeholder = "Type here to search"
                                          )),
                            shiny::column(3,
                                          shiny::actionButton(inputId = "loadSelectedCohorts", 
                                                              label = "Select",
                                                              style = "margin-top: 25px;")),
                            DT::DTOutput(outputId = "cohortSearchTableResults"),
                            shiny::conditionalPanel(
                              condition = "output.cohortSearchResultsCountOfSelected > 0",
                              shiny::tabsetPanel(
                                id = "selectedCohortAndPhenotypeDetails",
                                shiny::tabPanel(
                                  title = "Cohort Details",
                                  tags$br(),
                                  shiny::conditionalPanel(
                                    condition = "output.cohortSearchResultsCountOfSelected == 2",
                                    shiny::radioButtons(
                                      inputId = "compareCohorts",
                                      label = "Comparision",
                                      choices = c(
                                        "No Comparision",
                                        "Difference in Logic description",
                                        "Difference in JSON expression",
                                        "Difference in SQL"
                                      ),
                                      selected = "No Comparision",
                                      inline = TRUE
                                    )
                                  ),
                                  shiny::uiOutput(outputId = "dynamicUIGenerationCohortDetailsOne"),
                                  shiny::uiOutput(outputId = "dynamicUIGenerationCohortDetailsTwo"),
                                  column(
                                    12,
                                    conditionalPanel(
                                      "output.cohortSearchResultsCountOfSelected == 2&input.compareCohorts=='Difference in Logic description'",
                                      diffr::diffrOutput(outputId = "logicDifferenceBetweenCohorts", width = "100%")
                                    )
                                  ),
                                  column(
                                    12,
                                    conditionalPanel(
                                      "output.cohortSearchResultsCountOfSelected == 2&input.compareCohorts=='Difference in JSON expression'",
                                      diffr::diffrOutput(outputId = "jsonDifferenceBetweenCohorts", width = "100%", height = "50000px")
                                    )
                                  ),
                                  column(
                                    12,
                                    conditionalPanel(
                                      "output.cohortSearchResultsCountOfSelected == 2&input.compareCohorts=='Difference in SQL'",
                                      diffr::diffrOutput(outputId = "sqlDifferenceBetweenCohorts", width = "100%", height = "50000px")
                                    )
                                  )
                                )
                              )
                            )
                          )),
  shinydashboard::tabItem(tabName = "about",
                          if (exists(x = "aboutText"))
                            HTML(aboutText)),
  # shinydashboard::tabItem(
  #   tabName = "phenotypeDescription",
  #   shinydashboard::box(
  #     title = "Phenotype Description",
  #     width = NULL,
  #     status = "primary",
  #     DT::DTOutput(outputId = "phenoTypeDescriptionTable"),
  #     
  #     shiny::conditionalPanel(
  #       condition = "output.phenotypeRowIsSelected == true",
  #       shiny::actionButton("selectPhenotypeButton", label = "Select this phenotype", style = "margin-top: 5px; margin-bottom: 5px;"),
  #       
  #     )
  #   )
  # ),
  # shinydashboard::tabItem(
  #   tabName = "cohortDefinition",
  #   shinydashboard::box(
  #     title = "Cohort Definition",
  #     width = NULL,
  #     status = "primary",
  #     DT::DTOutput(outputId = "cohortDefinitionTable"),
  # 
  #     
  #   )
  # ),
  shinydashboard::tabItem(
    tabName = "cohortCounts",
    shiny::radioButtons(
      inputId = "pivotCohortCount",
      label = "Pivot data over data sources with value from",
      selected = "Subjects",
      inline = TRUE,
      choices = c(
        "None",
        "Subjects",
        "Entries"
      )),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      DT::DTOutput("cohortCountsTable")
    )
  ),
  shinydashboard::tabItem(
    tabName = "incidenceRate",
    #cohortReference("incidenceRateSelectedCohort"),
    shinydashboard::box(
      title = NULL,
      width = NULL,
      status = "primary",
      collapsible = FALSE,
      shiny::column(width =  3,
                     shiny::checkboxGroupInput(
                       inputId = "irStratification",
                       label = "Stratify by",
                       choices = c("Age", "Gender", "Calendar Year"),
                       selected = c("Age", "Gender", "Calendar Year"),
                       inline = TRUE
                     )
                   ),
      shiny::column(width = 3,
                    shinyWidgets::pickerInput(
                      inputId = "incidenceRateAgeFilter",
                      label = "Select Age Range",
                      choices = NULL,
                      inline = FALSE,
                      multiple = TRUE,
                      width = 'auto',
                      options = shinyWidgetsPickerOptions
                    )
      ),
      shiny::column(width = 3,
                    shinyWidgets::pickerInput(
                      inputId = "incidenceRateGenderFilter",
                      label = "select Gender",
                      choices = NULL,
                      inline = FALSE,
                      multiple = TRUE,
                      width = 'auto',
                      options = shinyWidgetsPickerOptions
                    )),
      shiny::column(width = 3,
                    shinyWidgets::pickerInput(
                      inputId = "incidenceRateCalendarFilter",
                      label = "Select Calendar Year",
                      choices = NULL,
                      inline = FALSE,
                      multiple = TRUE,
                      width = 'auto',
                      options = shinyWidgetsPickerOptions
                    ))
    ),
    shinydashboard::box(
      title = "Plot",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      shiny::column(width = 12, 
                    shiny::checkboxInput(inputId = "irYscaleFixed", 
                                         label = "Use same y-scale across databases",
                                         value = FALSE)),
      ggiraph::ggiraphOutput(
        outputId = "incidenceRatePlot",
        width = "100%",
        height = "100%"
      )
    ),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      DT::DTOutput("incidenceRateTable")
    )
  ),
  shinydashboard::tabItem(
    tabName = "timeDistribution",
    shinydashboard::box(
      title = "Plot",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      ggiraph::ggiraphOutput(
        outputId = "timeDistributionPlot",
        width = "100%",
        height = "100%"
      )
    ),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      DT::DTOutput("timeDistributionTable")
    )
  ),
  # shinydashboard::tabItem(
  #   tabName = "includedConcepts",
  #   shiny::radioButtons(
  #     inputId = "includedType",
  #     label = "",
  #     choices = c("Source Concepts", "Standard Concepts"),
  #     selected = "Source Concepts",
  #     inline = TRUE
  #   ),
  #   DT::DTOutput("includedConceptsTable")
  # ),
  # shinydashboard::tabItem(tabName = "orphanConcepts",
  #                         DT::DTOutput("orphanConceptsTable")),
  # shinydashboard::tabItem(
  #   tabName = "conceptSetDiagnostics",
  #   shiny::radioButtons(
  #     inputId = "conceptSetDiagnosticsType",
  #     label = "",
  #     choices = c("Standard Concepts", "Source Concepts"),
  #     selected = "Standard Concepts",
  #     inline = TRUE
  #   ),
  #   DT::DTOutput("conceptSetDiagnosticsTable")
  # ),
  shinydashboard::tabItem(
    tabName = "inclusionRuleStats",
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      DT::DTOutput("inclusionRuleTable")
    )
    #cohortReference("inclusionRuleStatSelectedCohort"),
  ),
  shinydashboard::tabItem(
    tabName = "indexEventBreakdown",
    shiny::radioButtons(
      inputId = "pivotIndexEventBreakDown",
      label = "Pivot data over data sources with value from",
      selected = "Percent entries",
      inline = TRUE,
      choices = c(
        "None",
        "Concept count",
        "Subject count",
        "Percent entries",
        "Percent persons"
      )),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      DT::DTOutput("indexEventBreakDownTable")
    )
    #cohortReference("indexEventBreakdownSelectedCohort"),
  ),
  shinydashboard::tabItem(
    tabName = "visitContext",
    shiny::radioButtons(
      inputId = "pivotVisitContext",
      label = "Show only",
      selected = "Percent",
      inline = TRUE,
      choices = c(
        "All",
        "Percent",
        "Subjects"
      )),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      DT::DTOutput("visitContextTable")
    )
    #cohortReference("visitContextSelectedCohort"),
  ),
  shinydashboard::tabItem(
    tabName = "cohortCharacterization",
    shinydashboard::box(
      title = "Data (Pretty)",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      shiny::column(width = 4,
                    shinyWidgets::pickerInput(
                      inputId = "characterizationTablePrettyDtDropDownDatabase",
                      label = "Database (not working)",
                      choices = NULL,
                      multiple = FALSE,
                      inline = FALSE,
                      options = shinyWidgetsPickerOptions
                    )),
      shiny::column(width = 4,
                    shinyWidgets::pickerInput(
                      inputId = "characterizationTablePrettyDtDropDownCohort",
                      label = "Cohort (not working)",
                      choices = NULL,
                      multiple = FALSE,
                      inline = FALSE,
                      options = shinyWidgetsPickerOptions
                    )),
      DT::DTOutput("characterizationTablePrettyDt")
    ),
    #cohortReference("characterizationSelectedCohort"),
    shinydashboard::box(
      title = "Data (Raw)",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      shinydashboard::box(
        title = NULL,
        width = NULL,
        status = "primary",
        collapsible = FALSE,
        shiny::column(width = 6,
                      shinyWidgets::pickerInput(
                        inputId = "characterizationAnalysisNameFilter",
                        label = "Analysis Choices",
                        choices = NULL,
                        multiple = TRUE,
                        inline = FALSE,
                        options = shinyWidgetsPickerOptions
                      )),
        shiny::column(width = 6,
                      shinyWidgets::pickerInput(
                        inputId = "characterizationDomainFilter",
                        label = "Domain Choices",
                        choices = NULL,
                        multiple = TRUE,
                        inline = FALSE,
                        options = shinyWidgetsPickerOptions
                      ))
      ),
      DT::DTOutput("characterizationTableRaw")
    )
  ),
  shinydashboard::tabItem(
    tabName = "temporalCharacterization",
    shinydashboard::box(
      title = NULL,
      width = NULL,
      status = "primary",
      collapsible = FALSE,
      shiny::column(width = 6,
                    shinyWidgets::pickerInput(
                      inputId = "temporalCharacterizationAnalysisNameFilter",
                      label = "Analysis Choices",
                      choices = NULL,
                      multiple = TRUE,
                      inline = FALSE,
                      options = shinyWidgetsPickerOptions
                    )),
      shiny::column(width = 6,
                    shinyWidgets::pickerInput(
                      inputId = "temporalCharacterizationDomainFilter",
                      label = "Domain Choices",
                      choices = NULL,
                      multiple = TRUE,
                      inline = FALSE,
                      options = shinyWidgetsPickerOptions
                    ))
    ),
    #cohortReference("temporalCharacterizationSelectedCohort"),
    shinydashboard::box(
      title = "Temporal Characterization Plot",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      shiny::textOutput(outputId = "temporalCharacterizationPlotText"),
      shiny::column(width = 6,
                    shinyWidgets::pickerInput(
                      inputId = "temporalCharacterizationPlotCohorts",
                      label = "Cohorts for plotting",
                      choices = NULL,
                      multiple = TRUE,
                      inline = FALSE,
                      options = shinyWidgetsPickerOptions
                    ))
      # shiny::htmlOutput(outputId = "hoverInfoIr"),
      # ggiraph::ggiraphOutput(
      #   outputId = "temporalCharacterizationPlot",
      #   width = "100%",
      #   height = "100%"
      # )
    ),
    shinydashboard::box(
      title = "Temporal Characterization Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      DT::DTOutput("temporalCharacterizationTable")
    ),
    shinydashboard::box(
      title = "Raw data",
      width = NULL,
      status = "primary",
      collapsible = TRUE, 
      collapsed = TRUE,
      DT::DTOutput("temporalCharacterizationTableRaw")
    )
    
    
    
    
    
    
    
    
    
    
    
    # shiny::radioButtons(
    #   inputId = "tempCharType",
    #   label = "",
    #   choices = c("Table", "Plot"),
    #   selected = "Table",
    #   inline = TRUE
    # ),
    # shiny::conditionalPanel(
    #   condition = "input.tempCharType=='Table'",
    #   DT::DTOutput("temporalCharacterizationTable")
    # ),
    # shiny::conditionalPanel(
    #   condition = "input.tempCharType=='Plot'",
    #   
    #   tags$table(style = "width:100%",
    #              tags$tr(
    #                tags$td(
    #                  shinyWidgets::pickerInput(
    #                    inputId = "timeIdChoicesFilter",
    #                    label = "Filter By Temporal Choices",
    #                    choices = c("All", temporalCovariateChoices$choices),
    #                    multiple = FALSE,
    #                    choicesOpt = list(style = rep_len("color: black;", 999)),
    #                    options = shinyWidgets::pickerOptions(
    #                      actionsBox = TRUE,
    #                      liveSearch = TRUE,
    #                      size = 10,
    #                      liveSearchStyle = "contains",
    #                      liveSearchPlaceholder = "Type here to search",
    #                      virtualScroll = 50
    #                    )
    #                  )
    #                ),
    #                tags$td(
    #                  shinyWidgets::pickerInput(
    #                    inputId = "temporalDomainId",
    #                    label = "Filter By Covariate Domain",
    #                    choices = c(
    #                      "all",
    #                      "condition",
    #                      "device",
    #                      "drug",
    #                      "measurement",
    #                      "observation",
    #                      "procedure",
    #                      "other"
    #                    ),
    #                    multiple = FALSE,
    #                    choicesOpt = list(style = rep_len("color: black;", 999)),
    #                    options = shinyWidgets::pickerOptions(
    #                      actionsBox = TRUE,
    #                      liveSearch = TRUE,
    #                      size = 10,
    #                      liveSearchStyle = 'contains',
    #                      liveSearchPlaceholder = "Type here to search",
    #                      virtualScroll = 50
    #                    )
    #                  )
    #                )
    #              )),
    #   shinydashboard::box(
    #     title = "Compare Temporal Characterization",
    #     width = NULL,
    #     status = "primary",
    #     fluidPage(fluidRow(
    #       column(
    #         3,
    #         DT::DTOutput("temporalCharacterizationCovariateTable")
    #       ),
    #       column(
    #         9,
    #         ggiraph::ggiraphOutput(
    #           "compareTemporalCharacterizationPlot",
    #           width = "100%",
    #           height = "100%"
    #         )
    #       )
    #     ))
    #   ),
    #   shiny::conditionalPanel(
    #     condition = "input.compareTemporalCharacterizationPlot_selected.length>0",
    #     shinydashboard::box(
    #       title = "Selected covariates",
    #       width = NULL,
    #       status = "primary",
    #       fluidPage(fluidRow(
    #         column(
    #           6,
    #           DT::DTOutput("temporalCharacterizationCovariateLassoTable")
    #         ),
    #         column(
    #           6,
    #           ggiraph::ggiraphOutput(
    #             "compareTemporalCharacterizationLassoPlot",
    #             width = "100%",
    #             height = "100%"
    #           )
    #         )
    #       ))
    #     )
    #   )
    # )
  ),
  shinydashboard::tabItem(
    tabName = "cohortOverlap",
    #cohortReference("cohortOverlapSelectedCohort"),
    shinydashboard::box(
      title = "Plot",
      width = NULL,
      status = "primary",
      shiny::radioButtons(
        inputId = "overlapPlotType",
        label = "",
        choices = c("Percentages", "Counts"),
        selected = "Percentages",
        inline = TRUE
      ),
      collapsible = TRUE,
      collapsed = FALSE,
      ggiraph::ggiraphOutput(
        outputId = "cohortOverlapPlot",
        width = "100%",
        height = "100%"
      )
    ),
    shinydashboard::box(
      title = "Data",
      width = NULL,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      DT::DTOutput("cohortOverlapData")
    )
  ),
  # shinydashboard::tabItem(
  #   tabName = "compareCohortCharacterization",
  #   #cohortReference("cohortCharCompareSelectedCohort"),
  #   shiny::radioButtons(
  #     inputId = "charCompareType",
  #     label = "",
  #     choices = c("Pretty table", "Raw table", "Plot"),
  #     selected = "Pretty table",
  #     inline = TRUE
  #   ),
  #   shiny::conditionalPanel(condition = "input.charCompareType=='Pretty table' | input.charCompareType=='Raw table'",
  #                           DT::DTOutput("charCompareTable")),
  #   shiny::conditionalPanel(
  #     condition = "input.charCompareType=='Plot'",
  #     shinydashboard::box(
  #       title = "Compare Cohort Characterization",
  #       width = NULL,
  #       status = "primary",
  #       shiny::htmlOutput("compareCohortCharacterizationSelectedCohort"),
  #       shinyWidgets::pickerInput(
  #         inputId = "domainId",
  #         label = "Filter By Domain",
  #         choices = c(
  #           "all",
  #           "condition",
  #           "device",
  #           "drug",
  #           "measurement",
  #           "observation",
  #           "procedure",
  #           "other"
  #         ),
  #         multiple = FALSE,
  #         choicesOpt = list(style = rep_len("color: black;", 999)),
  #         options = shinyWidgets::pickerOptions(
  #           actionsBox = TRUE,
  #           liveSearch = TRUE,
  #           size = 10,
  #           liveSearchStyle = 'contains',
  #           liveSearchPlaceholder = "Type here to search",
  #           virtualScroll = 50
  #         )
  #         
  #       ),
  #       ggiraph::ggiraphOutput(
  #         outputId = "charComparePlot",
  #         width = "100%",
  #         height = "100%"
  #       )
  #     )
  #   )
  # ),
  shinydashboard::tabItem(tabName = "databaseInformation",
                          DT::DTOutput("databaseInformationTable"))
)


#body
body <- shinydashboard::dashboardBody(bodyTabItems, 
                                      tags$script(HTML("$('body').addClass('fixed');")) # fixed header bar.
                                      )


#main
shinydashboard::dashboardPage(
  tags$head(tags$style(HTML(
    "
      th, td {
        padding-right: 10px;
      }
      
      #sidebarItemExpanded h5 {
        position: absolute;
        bottom: 0;
        width: 100%;
      }
    "
  ))),
  header = header,
  sidebar = sidebar,
  body = body
)
