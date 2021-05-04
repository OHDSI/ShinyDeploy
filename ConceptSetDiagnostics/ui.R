

shinydashboard::dashboardPage(
  sidebar = shinydashboard::dashboardSidebar(disable = TRUE),
  header = shinydashboard::dashboardHeader(title = "Concept Set Diagnostric"),
  body = shinydashboard::dashboardBody(
    shinydashboard::box(
      title = "Concept Set Diagnostic",
      width = NULL,
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      shiny::h5("Enter Keyword(s) :"),
      column(4, shiny::uiOutput(outputId = "col")),
      shinyjs::useShinyjs(),
      # tags$style("#col input {background-color:red !important}"),
      column(
        2,
        shiny::actionButton(
          inputId = "addKeyword",
          icon = icon("plus"),
          label = ""
        ),
        shiny::actionButton(
          inputId = "removeKeyword",
          icon = icon("minus"),
          label = ""
        )
      ),
      column(
        3,
        shiny::selectInput(
          inputId = "vocabularyId",
          label = "Filter by Vocabulary",
          choices =  c('SNOMED', 'HCPCS', 'ICD10CM', 'ICD10', 'ICD9CM', 'ICD9', 'Read'),
          selected = c('SNOMED', 'HCPCS', 'ICD10CM', 'ICD10', 'ICD9CM', 'ICD9', 'Read'),
          multiple = TRUE
        )
      ),
      column(
        3,
        shiny::selectInput(
          inputId = "domainId",
          label = "Filter by Domain",
          choices =  c('Condition', 'Observation'),
          selected = c('Condition', 'Observation'),
          multiple = TRUE
        )
      ),
      column(12,
             shiny::actionButton(inputId = "search", label = "Search"))
    ),
    shiny::conditionalPanel(
      condition = "!(output.isSearchResultFound)",
      shinydashboard::box(
        title = "Concept Set Result",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        shiny::tabsetPanel(
          id = "cohortDetails",
          type = "tab",
          shiny::tabPanel(
            title = "Search Result",
            value = "searchResult",
            shiny::conditionalPanel(
              condition = "output.numberOfRowSelectedInSearchResult >= 1",
              shiny::actionButton(
                inputId = "deleteSearchResult",
                label = "Delete",
                style = "background-color:#c45;color:#fff"
              )
            ),
            DT::DTOutput(outputId = "searchResultConceptIds")
          ),
          shiny::tabPanel(
            title = "Concept Set Expression",
            value = "conceptSetExpression",
            shiny::conditionalPanel(
              condition = "output.numberOfRowSelectedInConceptSetExpression >= 1",
              shiny::actionButton(
                inputId = "deleteConceptSetExpression",
                label = "Delete",
                style = "background-color:#c45;color:#fff"
              )
            ),
            DT::DTOutput(outputId = "conceptSetExpression"),
            tags$script(
              HTML(
                '
            $(document).on("click", ".selectConceptSetExpressionRow", function () {
                       var conceptSetExpressionRowCheckboxes = document.getElementsByName("selectConceptSetExpressionRow");
                       var conceptSetExpressionRowCheckboxesChecked = [];
                       for (var i=0; i<conceptSetExpressionRowCheckboxes.length; i++) {
                       if (conceptSetExpressionRowCheckboxes[i].checked) {
                       conceptSetExpressionRowCheckboxesChecked.push(conceptSetExpressionRowCheckboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("conceptSetExpression_checkboxes_checked",conceptSetExpressionRowCheckboxesChecked);});

            $(document).on("click", ".selectDescendants", function () {
                       var descendantsCheckboxes = document.getElementsByName("selectDescendants");
                       var descendantsCheckboxesChecked = [];
                       for (var i=0; i<descendantsCheckboxes.length; i++) {
                       if (descendantsCheckboxes[i].checked) {
                       descendantsCheckboxesChecked.push(descendantsCheckboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("descendants_checkboxes_checked",descendantsCheckboxesChecked);});

            $(document).on("click", ".selectMapped", function () {
                       var mappedCheckboxes = document.getElementsByName("selectMapped");
                       var mappedCheckboxesChecked = [];
                       for (var i=0; i<mappedCheckboxes.length; i++) {
                       if (mappedCheckboxes[i].checked) {
                       mappedCheckboxesChecked.push(mappedCheckboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("mapped_checkboxes_checked",mappedCheckboxesChecked);});

            $(document).on("click", ".selectExcluded", function () {
                       var excludedCheckboxes = document.getElementsByName("selectExcluded");
                       var excludedCheckboxesChecked = [];
                       for (var i=0; i<excludedCheckboxes.length; i++) {
                       if (excludedCheckboxes[i].checked) {
                       excludedCheckboxesChecked.push(excludedCheckboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("excluded_checkboxes_checked",excludedCheckboxesChecked);});
            '
              )
            ),
            tags$style(
              HTML(
                '.selectDescendants,.selectMapped,.selectExcluded {
              height : 30px;
              width : 30px;
            }'
              )
            )
          ),
          shiny::tabPanel(
            title = "Resolved",
            value = "resolved",
            shiny::tabsetPanel(
              id = "resolvedConceptsetExpressionTab",
              type = "tab",
              shiny::tabPanel(
                value = "resolvedConceptsetExpressionTabPanel",
                title = "Resolved",
                DT::DTOutput(outputId = "resolvedConceptSetExpression"),
              ),
              shiny::tabPanel(
                value = "mappedConceptsetExpressionTabPanel",
                title = "Mapped",
                DT::DTOutput(outputId = "mappedConceptSetExpression")
              )
            )
            
          ),
          shiny::tabPanel(
            title = "Recommended",
            value = "recommended",
            shiny::tabsetPanel(
              id = "recommendedConceptsetExpressionTab",
              type = "tab",
              shiny::tabPanel(
                value = "recommendedStandardConceptSetExpressionTabPanel",
                title = "Standard",
                DT::DTOutput(outputId = "recommendedStandardConceptSetExpression"),
              ),
              shiny::tabPanel(
                value = "recommendedSourceConceptSetExpressionTabPanel",
                title = "Source",
                DT::DTOutput(outputId = "recommendedSourceConceptSetExpression")
              )
            )
            
          ),
          shiny::tabPanel(
            title = "JSON",
            value = "json",
            copyToClipboardButton(toCopyId = "conceptSetExpressionJSON",
                                  style = "margin-top: 5px; margin-bottom: 5px;"),
            shiny::verbatimTextOutput(outputId = "conceptSetExpressionJSON"),
            tags$head(tags$style(
              "#conceptSetExpressionJSON { max-height:700px};"
            ))
          )
        )
      )
    ),
    shiny::conditionalPanel(
      condition = "output.numberOfRowSelectedInSearchResult >= 1",
      shinydashboard::box(
        title = "Concept ID Details",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        column(
          4,
          shinydashboard::box(
            title = "Concept Synonym",
            width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::DTOutput(outputId = "conceptSynonym")
          )
        ),
        column(
          8,
          shinydashboard::box(
            title = "Relationship",
            width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::DTOutput(outputId = "conceptRelationship")
          )
        )
      )
    )
  )
)
