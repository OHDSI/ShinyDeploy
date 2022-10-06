# packages
library(shiny)
library(ggplot2)
library(scales)
library(ggthemes)
library(tidyr)
library(dplyr, warn.conflicts = FALSE)

# Override default DatabaseConnector query function
renderTranslateQuerySql <-
  function(connection,
           sql,
           dbms,
           ...,
           snakeCaseToCamelCase = FALSE) {
    if (is(connection, "Pool")) {
      sql <- SqlRender::render(sql, ...)
      sql <- SqlRender::translate(sql, targetDialect = dbms)

      tryCatch(
      {
        data <- DatabaseConnector::dbGetQuery(connection, sql)
      },
        error = function(err) {
          writeLines(sql)
          if (dbms %in% c("postgresql", "redshift")) {
            DatabaseConnector::dbExecute(connection, "ABORT;")
          }
          stop(err)
        }
      )
      if (snakeCaseToCamelCase) {
        colnames(data) <- SqlRender::snakeCaseToCamelCase(colnames(data))
      }
      return(data)
    } else {
      return(
        DatabaseConnector::renderTranslateQuerySql(
          connection = connection,
          sql = sql,
          ...,
          snakeCaseToCamelCase = snakeCaseToCamelCase
        )
      )
    }
  }


# params
# resultsSchema <- "reward_truven_ccae_v1676"
resultsSchema <- "comparator_selector"
similarityTable <- "oscsp_similarity_for_shiny"
covDataTable <- "oscsp_covdata_for_shiny"

# details of results database
# change if testing on redshift
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  user = Sys.getenv("phenotypeLibrarydbUser"),
  password = Sys.getenv("phenotypeLibrarydbPw"),
  server = paste0(Sys.getenv("phenotypeLibraryServer"), "/", Sys.getenv("phenotypeLibrarydb")),
  port = 5432
)

conn <-
  pool::dbPool(
    drv = DatabaseConnector::DatabaseConnectorDriver(),
    dbms = connectionDetails$dbms,
    server = connectionDetails$server(),
    port = connectionDetails$port(),
    user = connectionDetails$user(),
    password = connectionDetails$password(),
    connectionString = connectionDetails$connectionString()
  )

# Handle closing connection cleanly
shiny::onStop(function() {
  if (DBI::dbIsValid(conn)) {
    writeLines("Closing database pool")
    pool::poolClose(conn)
  }
})

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # initial query to get list of cohort definitions
  getCohortDefinitions <- reactive({
    renderTranslateQuerySql(
      connection = conn,
      sql = "select distinct
               cohort_definition_id,
               cohort_short_name,
               is_atc,
               cohort_n
             from @schema.@table
             where cohort_definition_id is not null and
                   is_atc in (0, 1)
             order by cohort_short_name",
      dbms = connectionDetails$dbms,
      schema = resultsSchema,
      table = covDataTable,
      snakeCaseToCamelCase = TRUE
    )
  }) %>% bindCache("cohortDefs")

  observe({
    cohortDefinitions <- getCohortDefinitions()

    exposureSelection <- cohortDefinitions$cohortDefinitionId
    names(exposureSelection) <- cohortDefinitions$cohortShortName
    updateSelectizeInput(
      session,
      "selectedExposure",
      choices = exposureSelection,
      selected = 8826,
      server = TRUE)

    updateSelectizeInput(
      session,
      "selectedComparator",
      choices = exposureSelection,
      selected = 5736,
      server = TRUE)
  })


  #### ---- function to get cosine similarity data (for table and plot) ---- ####
  getSimilarity <- shiny::reactive({
    # identify target cohort
    targetCohortId <- input$selectedExposure

    # identify selected comparator types
    if (length(input$selectedComparatorTypes) == 2L) { atcSelection <- c(0, 1) }
    else if (input$selectedComparatorTypes == "RxNorm Ingredients") { atcSelection <- c(0) }
    else if (input$selectedComparatorTypes == "ATC Classes") { atcSelection <- c(1) }

    # send query to get results data 
    resultsData <- renderTranslateQuerySql(
      connection = conn,
      sql = "select
               is_atc_2,
               cohort_short_name_2,
               cosine_sim_all,
               cosine_sim_demo,
               cosine_sim_pres,
               cosine_sim_mhist,
               cosine_sim_pmeds,
               cosine_sim_visit,
               atc_4_related,
               atc_3_related
             from @schema.@table
             where cohort_definition_id_1 = @targetCohortId and 
                   is_atc_2 in (@atc)
             order by cosine_sim_all desc",
      dbms = connectionDetails$dbms,
      schema = resultsSchema,
      table = similarityTable,
      snakeCaseToCamelCase = TRUE,
      targetCohortId = targetCohortId,
      atc = atcSelection)

    resultsData

  })

  #### ---- cosine similarity reactable ---- ####
  output$cosineSimilarityTbl <- reactable::renderReactable({
    reactable::reactable(
      data = getSimilarity(),
      columns = list(
        "isAtc2" = reactable::colDef(name = "Type", cell = function(value) { ifelse(value == 1, "ATC Class", "RxNorm Ingredient") }, align = "right", vAlign = "center", headerVAlign = "bottom", minWidth = 125),
        "cosineSimAll" = reactable::colDef(name = "Avg.", cell = function(value) { sprintf("%.3f", value) }, align = "center", vAlign = "center", headerVAlign = "bottom", minWidth = 125),
        "cosineSimDemo" = reactable::colDef(name = "Demographics", cell = function(value) { sprintf("%.3f", value) }, align = "center", vAlign = "center", headerVAlign = "bottom", minWidth = 125),
        "cosineSimPres" = reactable::colDef(name = "Presentation", cell = function(value) { sprintf("%.3f", value) }, align = "center", vAlign = "center", headerVAlign = "bottom", minWidth = 125),
        "cosineSimMhist" = reactable::colDef(name = "Medical History", cell = function(value) { sprintf("%.3f", value) }, align = "center", vAlign = "center", headerVAlign = "bottom", minWidth = 125),
        "cosineSimPmeds" = reactable::colDef(name = "Prior Medications", cell = function(value) { sprintf("%.3f", value) }, align = "center", vAlign = "center", headerVAlign = "bottom", minWidth = 125),
        "cosineSimVisit" = reactable::colDef(name = "Visit Context", cell = function(value) { sprintf("%.3f", value) }, align = "center", vAlign = "center", headerVAlign = "bottom", minWidth = 125),
        "cohortShortName2" = reactable::colDef(name = "Name", cell = function(value) { ifelse(substr(value, 1, 6) == "RxNorm", gsub("RxNorm - ", "", value), gsub("ATC - ", "", value)) }, align = "left", vAlign = "center", headerVAlign = "bottom", minWidth = 125),
        "atc3Related" = reactable::colDef(name = "At Level 3", cell = function(value) ifelse(is.na(value) | value == 0, "No", "Yes"), align = "center", vAlign = "center", headerVAlign = "bottom", filterable = TRUE),
        "atc4Related" = reactable::colDef(name = "At Level 4", cell = function(value) ifelse(is.na(value) | value == 0, "No", "Yes"), align = "center", vAlign = "center", headerVAlign = "bottom", filterable = TRUE)),
      searchable = TRUE,
      columnGroups = list(
        reactable::colGroup(
          "Comparator",
          c("cohortShortName2", "isAtc2")),
        reactable::colGroup(
          "Covariate Domain",
          c("cosineSimAll", "cosineSimDemo", "cosineSimPres", "cosineSimMhist", "cosineSimPmeds", "cosineSimVisit")),
        reactable::colGroup(
          "ATC Relationship to Target",
          c("atc3Related", "atc4Related"))),
      fullWidth = TRUE,
      bordered = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 10, 20, 50, 100, 1000),
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      theme = reactable::reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#eab676",
        cellPadding = "8px 12px",
        searchInputStyle = list(width = "100%")),
      showSortIcon = TRUE) })

  #### ---- step-function plot of cosine similarity by rank ---- ####
  output$stepPlot <- renderPlot({

    getSimilarity() %>%
      pivot_longer(
        c(cosineSimAll, cosineSimDemo, cosineSimPres, cosineSimMhist, cosineSimPmeds, cosineSimVisit),
        values_to = "cosineSimilarity",
        names_to = "var") %>%
      mutate(simType = NA,
             simType = ifelse(var == "cosineSimAll", "Avg.", simType),
             simType = ifelse(var == "cosineSimDemo", "Demographics", simType),
             simType = ifelse(var == "cosineSimPres", "Presentation", simType),
             simType = ifelse(var == "cosineSimMhist", "Medical History", simType),
             simType = ifelse(var == "cosineSimPmeds", "Prior Medications", simType),
             simType = ifelse(var == "cosineSimVisit", "Visit Context", simType),
             simType = factor(simType, levels = c("Avg.", "Demographics", "Presentation", "Medical History", "Prior Medications", "Visit Context"))) %>%
      group_by(simType) %>%
      mutate(simRank = row_number(desc(cosineSimilarity))) %>%
      ungroup() %>%
      ggplot(aes(x = simRank, y = cosineSimilarity, color = simType)) +
      geom_step() +
      scale_x_continuous(labels = comma) +
      scale_color_manual(
        values = c("Avg." = "#4E79A7",
                   "Demographics" = "#F28E2B",
                   "Presentation" = "#E15759",
                   "Medical History" = "#76B7B2",
                   "Prior Medications" = "#59A14F",
                   "Visit Context" = "#EDC948")) +
      labs(y = "Cosine Similarity",
           x = "Comparator Rank",
           color = "Covariate Domain") +
      theme_minimal(base_size = 9) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(ncol = 2))

  })

  ##### ---- function to get covariate data for a given comparison ---- ####
  getCovData <- shiny::reactive({
    covData <- renderTranslateQuerySql(
      connection = conn,
      sql = "with means as (
              	select
              		@cohortDefinitionId1 as cohort_definition_id_1,
              		@cohortDefinitionId2 as cohort_definition_id_2,
              		case 
              			when c1.covariate_type is null then c2.covariate_type
              			when c2.covariate_type is null then c1.covariate_type
              			else c1.covariate_type
              		end as covariate_type,
              		case 
              			when c1.covariate_id is null then c2.covariate_id
              			when c2.covariate_id is null then c1.covariate_id
              			else c1.covariate_id
              		end as covariate_id,
              		case 
              			when c1.covariate_short_name is null then c2.covariate_short_name
              			when c2.covariate_short_name is null then c1.covariate_short_name
              			else c1.covariate_short_name
              		end as covariate_short_name,
              		case 
              			when c1.covariate_mean is null then 0.0
              			else c1.covariate_mean
              		end as mean_1,
              		case 
              			when c2.covariate_mean is null then 0.0
              			else c2.covariate_mean
              		end as mean_2
              	from (select * from @schema.@table where cohort_definition_id = @cohortDefinitionId1) as c1
              	full join (select * from @schema.@table where cohort_definition_id = @cohortDefinitionId2) as c2
              		on c1.covariate_id = c2.covariate_id)
              		
              select 
              	m.*,
              	case 
              		when m.mean_1 = m.mean_2 then 0.0
              		when m.mean_1 = 0.0 and m.mean_2 = 1.0 then null 
              		when m.mean_1 = 1.0 and m.mean_2 = 0.0 then null 
              		else (mean_1 - mean_2) / (sqrt((mean_1 * (1 - mean_1) + mean_2 * (1 - mean_2)) / 2))
              	end as std_diff,
              	c1.cohort_n as n_1,
              	c2.cohort_n as n_2
              from means as m
              join (select distinct cohort_definition_id, cohort_n from @schema.@table) as c1
              	on m.cohort_definition_id_1 = c1.cohort_definition_id 
              join (select distinct cohort_definition_id, cohort_n from @schema.@table) as c2
              	on m.cohort_definition_id_2 = c2.cohort_definition_id ;",
      dbms = connectionDetails$dbms,
      snakeCaseToCamelCase = TRUE,
      schema = resultsSchema,
      table = covDataTable,
      cohortDefinitionId1 = input$selectedExposure,
      cohortDefinitionId2 = input$selectedComparator)

    # return data
    covData

  })

  #### ---- scatterplot of covariate prevalence ---- ####
  output$scatterPlot <- renderPlot({

    getCovData() %>%
      mutate(type = NA,
             type = ifelse(covariateType == "demographic", "Demographics", type),
             type = ifelse(covariateType == "presentation", "Presentation", type),
             type = ifelse(covariateType == "medical history", "Medical History", type),
             type = ifelse(covariateType == "prior meds", "Prior Medications", type),
             type = ifelse(covariateType == "visit context", "Visit Context", type),
             type = factor(type, levels = c("Demographics", "Presentation", "Medical History", "Prior Medications", "Visit Context"))) %>%
      ggplot(aes(x = mean1, y = mean2, color = type)) +
      geom_abline(linetype = 2) +
      geom_point(alpha = 0.7) +
      scale_color_manual(
        values = c("Demographics" = "#F28E2B",
                   "Presentation" = "#E15759",
                   "Medical History" = "#76B7B2",
                   "Prior Medications" = "#59A14F",
                   "Visit Context" = "#EDC948")) +
      guides(color = guide_legend(override.aes = list(alpha = 1))) +
      theme_minimal(base_size = 9) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(ncol = 2)) +
      labs(x = "Prevalence in Target Cohort",
           y = "Prevalence in Comparator Cohort",
           color = "Covariate Domain")

  })

  #### ---- plot of std. diffs. ---- ####
  output$smdPlot <- renderPlot({

    getCovData() %>%
      mutate(type = NA,
             type = ifelse(covariateType == "demographic", "Demographics", type),
             type = ifelse(covariateType == "presentation", "Presentation", type),
             type = ifelse(covariateType == "medical history", "Medical History", type),
             type = ifelse(covariateType == "prior meds", "Prior Medications", type),
             type = ifelse(covariateType == "visit context", "Visit Context", type),
             type = factor(type, levels = c("Demographics", "Presentation", "Medical History", "Prior Medications", "Visit Context"))) %>%
      ggplot(aes(x = stdDiff, y = type, color = type)) +
      geom_vline(aes(xintercept = 0)) +
      geom_vline(aes(xintercept = 0.1), linetype = 2) +
      geom_vline(aes(xintercept = -0.1), linetype = 2) +
      geom_point(position = position_jitter(), alpha = 0.7) +
      scale_color_manual(
        values = c("Demographics" = "#F28E2B",
                   "Presentation" = "#E15759",
                   "Medical History" = "#76B7B2",
                   "Prior Medications" = "#59A14F",
                   "Visit Context" = "#EDC948")) +
      theme_minimal(base_size = 9) +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank()) +
      guides(color = guide_legend(ncol = 2)) +
      labs(x = "Standardized Difference",
           y = "",
           color = "Covariate Domain")

  })

  #### ---- "table 1": demographics (default to unsorted) ---- ####
  output$covTableDemo <- reactable::renderReactable({
    cohortDefinitions <- getCohortDefinitions()
    # get data
    covData <- getCovData()

    # create column names with cohort sample sizes
    colNameTarget <- paste0(
      cohortDefinitions$cohortShortName[cohortDefinitions$cohortDefinitionId == input$selectedExposure],
      " (n = ",
      prettyNum(first(covData$n1), big.mark = ","),
      ")")

    colNameComparator <- paste0(
      cohortDefinitions$cohortShortName[cohortDefinitions$cohortDefinitionId == input$selectedComparator],
      " (n = ",
      prettyNum(first(covData$n2), big.mark = ","),
      ")")

    # subset data and select relevant columns
    tableData <- covData %>%
      filter(covariateType == "demographic") %>%
      arrange(desc(covariateShortName)) %>%
      select(covariateShortName, mean1, mean2, stdDiff)

    # table code
    reactable::reactable(
      data = tableData,
      columns = list(
        "covariateShortName" = reactable::colDef(name = "Covariate", align = "right", vAlign = "bottom"),
        "mean1" = reactable::colDef(name = colNameTarget, cell = function(value) { percent(value, accuracy = 0.1) }, align = "center", vAlign = "bottom"),
        "mean2" = reactable::colDef(name = colNameComparator, cell = function(value) { percent(value, accuracy = 0.1) }, align = "center", vAlign = "bottom"),
        "stdDiff" = reactable::colDef(name = "Std. Diff.", cell = function(value) { sprintf("%.2f", value) }, align = "center", vAlign = "bottom")),
      bordered = TRUE,
      searchable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 10, 20, 50, 100, 1000),
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      theme = reactable::reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#eab676",
        cellPadding = "8px 12px",
        searchInputStyle = list(width = "100%")),
      showSortIcon = TRUE)
  })

  #### ---- "table 1": presentation (default to sort by abs. std. diff.) ---- ####
  output$covTablePres <- reactable::renderReactable({
    cohortDefinitions <- getCohortDefinitions()
    # get data
    covData <- getCovData()

    # create column names with cohort sample sizes
    colNameTarget <- paste0(
      cohortDefinitions$cohortShortName[cohortDefinitions$cohortDefinitionId == input$selectedExposure],
      " (n = ",
      prettyNum(first(covData$n1), big.mark = ","),
      ")")

    colNameComparator <- paste0(
      cohortDefinitions$cohortShortName[cohortDefinitions$cohortDefinitionId == input$selectedComparator],
      " (n = ",
      prettyNum(first(covData$n2), big.mark = ","),
      ")")

    # subset data and select relevant columns
    tableData <- covData %>%
      filter(covariateType == "presentation") %>%
      arrange(desc(abs(stdDiff))) %>%
      select(covariateShortName, mean1, mean2, stdDiff)

    # table code
    reactable::reactable(
      data = tableData,
      columns = list(
        "covariateShortName" = reactable::colDef(name = "Covariate", align = "right", vAlign = "bottom"),
        "mean1" = reactable::colDef(name = colNameTarget, cell = function(value) { percent(value, accuracy = 0.1) }, align = "center", vAlign = "bottom"),
        "mean2" = reactable::colDef(name = colNameComparator, cell = function(value) { percent(value, accuracy = 0.1) }, align = "center", vAlign = "bottom"),
        "stdDiff" = reactable::colDef(name = "Std. Diff.", cell = function(value) { sprintf("%.2f", value) }, align = "center", vAlign = "bottom")),
      bordered = TRUE,
      searchable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 10, 20, 50, 100, 1000),
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      theme = reactable::reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#eab676",
        cellPadding = "8px 12px",
        searchInputStyle = list(width = "100%")),
      showSortIcon = TRUE)

  })

  #### ---- "table 1": medical history (default to sort by abs. std. diff.) ---- ####
  output$covTableMhist <- reactable::renderReactable({
    cohortDefinitions <- getCohortDefinitions()
    # get data
    covData <- getCovData()

    # create column names with cohort sample sizes
    colNameTarget <- paste0(
      cohortDefinitions$cohortShortName[cohortDefinitions$cohortDefinitionId == input$selectedExposure],
      " (n = ",
      prettyNum(first(covData$n1), big.mark = ","),
      ")")

    colNameComparator <- paste0(
      cohortDefinitions$cohortShortName[cohortDefinitions$cohortDefinitionId == input$selectedComparator],
      " (n = ",
      prettyNum(first(covData$n2), big.mark = ","),
      ")")

    # subset data and select relevant columns
    tableData <- covData %>%
      filter(covariateType == "medical history") %>%
      arrange(desc(abs(stdDiff))) %>%
      select(covariateShortName, mean1, mean2, stdDiff)

    # table code
    reactable::reactable(
      data = tableData,
      columns = list(
        "covariateShortName" = reactable::colDef(name = "Covariate", align = "right", vAlign = "bottom"),
        "mean1" = reactable::colDef(name = colNameTarget, cell = function(value) { percent(value, accuracy = 0.1) }, align = "center", vAlign = "bottom"),
        "mean2" = reactable::colDef(name = colNameComparator, cell = function(value) { percent(value, accuracy = 0.1) }, align = "center", vAlign = "bottom"),
        "stdDiff" = reactable::colDef(name = "Std. Diff.", cell = function(value) { sprintf("%.2f", value) }, align = "center", vAlign = "bottom")),
      bordered = TRUE,
      searchable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 10, 20, 50, 100, 1000),
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      theme = reactable::reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#eab676",
        cellPadding = "8px 12px",
        searchInputStyle = list(width = "100%")),
      showSortIcon = TRUE)

  })

  #### ---- "table 1": prior meds (default to sort by abs. std. diff.) ---- ####
  output$covTablePmeds <- reactable::renderReactable({
    cohortDefinitions <- getCohortDefinitions()
    # get data
    covData <- getCovData()

    # create column names with cohort sample sizes
    colNameTarget <- paste0(
      cohortDefinitions$cohortShortName[cohortDefinitions$cohortDefinitionId == input$selectedExposure],
      " (n = ",
      prettyNum(first(covData$n1), big.mark = ","),
      ")")

    colNameComparator <- paste0(
      cohortDefinitions$cohortShortName[cohortDefinitions$cohortDefinitionId == input$selectedComparator],
      " (n = ",
      prettyNum(first(covData$n2), big.mark = ","),
      ")")

    # subset data and select relevant columns
    tableData <- covData %>%
      filter(covariateType == "prior meds") %>%
      arrange(desc(abs(stdDiff))) %>%
      select(covariateShortName, mean1, mean2, stdDiff)

    # table code
    reactable::reactable(
      data = tableData,
      columns = list(
        "covariateShortName" = reactable::colDef(name = "Covariate", align = "right", vAlign = "bottom"),
        "mean1" = reactable::colDef(name = colNameTarget, cell = function(value) { percent(value, accuracy = 0.1) }, align = "center", vAlign = "bottom"),
        "mean2" = reactable::colDef(name = colNameComparator, cell = function(value) { percent(value, accuracy = 0.1) }, align = "center", vAlign = "bottom"),
        "stdDiff" = reactable::colDef(name = "Std. Diff.", cell = function(value) { sprintf("%.2f", value) }, align = "center", vAlign = "bottom")),
      bordered = TRUE,
      searchable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 10, 20, 50, 100, 1000),
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      theme = reactable::reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#eab676",
        cellPadding = "8px 12px",
        searchInputStyle = list(width = "100%")),
      showSortIcon = TRUE)

  })

  #### ---- "table 1": visit context (default to unsorted) ---- ####
  output$covTableVisit <- reactable::renderReactable({
    cohortDefinitions <- getCohortDefinitions()
    # get data
    covData <- getCovData()

    # create column names with cohort sample sizes
    colNameTarget <- paste0(
      cohortDefinitions$cohortShortName[cohortDefinitions$cohortDefinitionId == input$selectedExposure],
      " (n = ",
      prettyNum(first(covData$n1), big.mark = ","),
      ")")

    colNameComparator <- paste0(
      cohortDefinitions$cohortShortName[cohortDefinitions$cohortDefinitionId == input$selectedComparator],
      " (n = ",
      prettyNum(first(covData$n2), big.mark = ","),
      ")")

    # subset data and select relevant columns
    tableData <- covData %>%
      filter(covariateType == "visit context") %>%
      arrange(covariateShortName) %>%
      select(covariateShortName, mean1, mean2, stdDiff)

    # table code
    reactable::reactable(
      data = tableData,
      columns = list(
        "covariateShortName" = reactable::colDef(name = "Covariate", align = "right", vAlign = "bottom"),
        "mean1" = reactable::colDef(name = colNameTarget, cell = function(value) { percent(value, accuracy = 0.1) }, align = "center", vAlign = "bottom"),
        "mean2" = reactable::colDef(name = colNameComparator, cell = function(value) { percent(value, accuracy = 0.1) }, align = "center", vAlign = "bottom"),
        "stdDiff" = reactable::colDef(name = "Std. Diff.", cell = function(value) { sprintf("%.2f", value) }, align = "center", vAlign = "bottom")),
      bordered = TRUE,
      searchable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 10, 20, 50, 100, 1000),
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      theme = reactable::reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#eab676",
        cellPadding = "8px 12px",
        searchInputStyle = list(width = "100%")),
      showSortIcon = TRUE)

  })
})
