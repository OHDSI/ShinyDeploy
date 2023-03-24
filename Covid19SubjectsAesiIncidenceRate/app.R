library(shiny)
ui <- fluidPage(
  HTML("<p>This page has moved to <a href=\"http://shiny.ohdsi.org:1010/Covid19SubjectsAesiIncidenceRate/\">http://shiny.ohdsi.org:1010/Covid19SubjectsAesiIncidenceRate/</a>. You will be redirected.<p>"),
  HTML("<script>
        var timer = setTimeout(function() {
            window.location='http://shiny.ohdsi.org:1010/Covid19SubjectsAesiIncidenceRate/'
        }, 3000);
    </script>")
)
server <- function(input, output, session) {
}
shinyApp(ui, server)
