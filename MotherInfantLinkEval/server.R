library(shiny)
library(DT)

mainColumns <- c("domainId",
                 "covariateName",
                 "mean1",
                 #"sd1",
                 "mean2",
                 #"sd2",
                 "stdDiff")

colNames <- c("Domain",
              "Covariate",
              "Linked prop.",
              #"SD linked",
              "Not linked prop.",
              #"SD all",
              "SMD")

shinyServer(
  function(input, output) {

    roundDf <- function(df, digits = 5) {
      nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
      df[, nums] <- round(df[, nums], digits = digits)
      return(df)
    }


    # mother preg start plot ===================================================
    motherPregStartPlotData <- reactive({
      motherPregStartPlotData <- prepareData(compareData = motherPregStart,
                                             databaseId = input$databasePregStart,
                                             analysisVariant = input$analysisPregStart,
                                             domainIds = input$domainPregStart,
                                             isBinary = input$continousPregStart)
           return(motherPregStartPlotData)
    })
    motherPregStartCounts <- reactive({
      xCount <- counts$records[counts$cohortDefinitionId == 1 &
                                 counts$databaseId == input$databasePregStart &
                                 counts$analysisVariant == input$analysisPregStart]

      yCount <- counts$records[counts$cohortDefinitionId == 7 &
                                 counts$databaseId == input$databasePregStart &
                                 counts$analysisVariant == input$analysisPregStart]
      return(c(xCount, yCount))
    })


    motherPregStartPlot <- reactive({
      data <- motherPregStartPlotData()
      plot <- plotScatter(data,
                          xCount = motherPregStartCounts()[1],
                          yCount = motherPregStartCounts()[2])
      return(plot)
    })
    output$motherPregStartPlot <- renderPlot({
      return(motherPregStartPlot())
    }, res = 100)

    output$motherPregStartPlotHover <- renderUI({
      motherPregStartPlotData <- prepareData(compareData = motherPregStart,
                                             databaseId = input$databasePregStart,
                                             analysisVariant = input$analysisPregStart,
                                             domainIds = input$domainPregStart,
                                             isBinary = input$continousPregStart)
      hover <- input$motherPregStartPlotHover
      point <- nearPoints(motherPregStartPlotData,
                          hover,
                          threshold = 5,
                          maxpoints = 1,
                          addDist = TRUE)
      if (nrow(point) == 0) {
        return(NULL)
      }
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Covariate name: </b>", point$covariateName, "<br/>",
                      "<b> Linked prop.: </b>", round(point$mean1, 3), "<br/>",
                      "<b> All prop.: </b>", round(point$mean2, 3), "<br/>",
                      "<b> SMD: </b>", round(point$stdDiff, 3))))
      )
    })


    # mother preg start table ==================================================
    motherPregStartTableData <- reactive({
      motherPregStartTableData <- prepareData(compareData = motherPregStart,
                                              databaseId = input$databasePregStart,
                                              analysisVariant = input$analysisPregStart,
                                              domainIds = input$domainPregStart,
                                              isBinary = input$continousPregStart)
      return(motherPregStartTableData)
    })
    motherPregStartTable <- reactive({
      data <- motherPregStartTableData()
      table <- data[, c(mainColumns)]
      table <- roundDf(table)
      names(table) <- colNames
      return(table)
    })
    output$motherPregStartTable <- renderDataTable({
      return(motherPregStartTable())
    }, rownames = FALSE,
       options = list(lengthMenu = list(c(5, 10, 15, 20), c('5', '10', '15', '20')),
                      pageLength = 5,
                      filter = 'none'))

    # mother preg end plot =====================================================
    motherPregEndPlotData <- reactive({
      motherPregEndData <- prepareData(compareData = motherPregEnd,
                                       databaseId = input$databasePregEnd,
                                       analysisVariant = input$analysisPregEnd,
                                       domainIds = input$domainPregEnd,
                                       isBinary = input$continousPregEnd)
      return(motherPregEndData)
    })

    motherPregEndCounts <- reactive({
      xCount <- counts$records[counts$cohortDefinitionId == 2 &
                                 counts$databaseId == input$databasePregEnd &
                                 counts$analysisVariant == input$analysisPregEnd]

      yCount <- counts$records[counts$cohortDefinitionId == 8 &
                                 counts$databaseId == input$databasePregEnd &
                                 counts$analysisVariant == input$analysisPregEnd]
      return(c(xCount, yCount))
    })


    motherPregEndPlot <- reactive({
      data <- motherPregEndPlotData()
      plot <- plotScatter(data,
                          xCount = motherPregEndCounts()[1],
                          yCount = motherPregEndCounts()[2])
      return(plot)
    })
    output$motherPregEndPlot <- renderPlot({
      return(motherPregEndPlot())
    }, res = 100)

    output$motherPregEndPlotHover <- renderUI({
      motherPregEndPlotData <- prepareData(compareData = motherPregEnd,
                                           databaseId = input$databasePregEnd,
                                           analysisVariant = input$analysisPregEnd,
                                           domainIds = input$domainPregEnd,
                                           isBinary = input$continousPregEnd)
      hover <- input$motherPregEndPlotHover
      point <- nearPoints(motherPregEndPlotData,
                          hover,
                          threshold = 5,
                          maxpoints = 1,
                          addDist = TRUE)
      if (nrow(point) == 0) {
        return(NULL)
      }
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Covariate name: </b>", point$covariateName, "<br/>",
                      "<b> Linked prop.: </b>", round(point$mean1, 3), "<br/>",
                      "<b> All prop.: </b>", round(point$mean2, 3), "<br/>",
                      "<b> SMD: </b>", round(point$stdDiff, 3))))
      )
    })

    # mother preg end table ==================================================
    motherPregEndTableData <- reactive({
      motherPregEndTableData <- prepareData(compareData = motherPregEnd,
                                            databaseId = input$databasePregEnd,
                                            analysisVariant = input$analysisPregEnd,
                                            domainIds = input$domainPregEnd,
                                            isBinary = input$continousPregEnd)
    })
    motherPregEndTable <- reactive({
      data <- motherPregEndTableData()
      table <- data[, c(mainColumns)]
      table <- roundDf(table)
      names(table) <- colNames
      return(table)
    })
    output$motherPregEndTable <- renderDataTable({
      return(motherPregEndTable())
    }, rownames = FALSE,
       options = list(lengthMenu = list(c(5, 10, 15, 20), c('5', '10', '15', '20')),
                      pageLength = 5,
                      filter = 'none'))


    # infants plot =============================================================
    infantPlotData <- reactive({
      infantPlotData <- prepareData(compareData = infant,
                                    databaseId = input$databaseInfant,
                                    analysisVariant = input$analysisInfant,
                                    domainIds = input$domainInfant,
                                    isBinary = input$continousInfant)
      return(infantPlotData)
    })

    infantCounts <- reactive({
      xCount <- counts$records[counts$cohortDefinitionId == 3 &
                                 counts$databaseId == input$databaseInfant &
                                 counts$analysisVariant == input$analysisInfant]

      yCount <- counts$records[counts$cohortDefinitionId == 9 &
                                 counts$databaseId == input$databaseInfant &
                                 counts$analysisVariant == input$analysisInfant]
      return(c(xCount, yCount))
    })



    infantPlot <- reactive({
      data <- infantPlotData()
      plot <- plotScatter(data,
                          xCount = infantCounts()[1],
                          yCount = infantCounts()[2])
      return(plot)
    })
    output$infantPlot <- renderPlot({
      return(infantPlot())
    }, res = 100)

    output$infantPlotHover <- renderUI({
      infantPlotData <- prepareData(compareData = infant,
                                    databaseId = input$databaseInfant,
                                    analysisVariant = input$analysisInfant,
                                    domainIds = input$domainInfant,
                                    isBinary = input$continousInfant)
      hover <- input$infantPlotHover
      point <- nearPoints(infantPlotData,
                          hover,
                          threshold = 5,
                          maxpoints = 1,
                          addDist = TRUE)
      if (nrow(point) == 0) {
        return(NULL)
      }
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Covariate name: </b>", point$covariateName, "<br/>",
                      "<b> Linked prop.: </b>", round(point$mean1, 3), "<br/>",
                      "<b> All prop.: </b>", round(point$mean2, 3), "<br/>",
                      "<b> SMD: </b>", round(point$stdDiff, 3))))
      )
    })

    # infants table ============================================================
    infantTableData <- reactive({
      infantTableData <- prepareData(compareData = infant,
                                     databaseId = input$databaseInfant,
                                     analysisVariant = input$analysisInfant,
                                     domainIds = input$domainInfant,
                                     isBinary = input$continousInfant)
    })
    infantTable <- reactive({
      data <- infantTableData()
      table <- data[, c(mainColumns)]
      table <- roundDf(table)
      names(table) <- colNames
      return(table)
    })
    output$infantTable <- renderDataTable({
      return(infantTable())
    }, rownames = FALSE,
       options = list(lengthMenu = list(c(5, 10, 15, 20), c('5', '10', '15', '20')),
                      pageLength = 5,
                      filter = 'none'))
})

