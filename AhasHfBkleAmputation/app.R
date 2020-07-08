library(shiny)
ui <- fluidPage(
  HTML("<p>This page has moved to <a href=\"http://shiny.ohdsi.org:2020/AhasHfBkleAmputation/\">http://shiny.ohdsi.org:2020/AhasHfBkleAmputation/</a>. You will be redirected.<p>"),
  HTML("<script>
        var timer = setTimeout(function() {
            window.location='http://shiny.ohdsi.org:2020/AhasHfBkleAmputation/'
        }, 3000);
    </script>")
)
server <- function(input, output, session) {
}
shinyApp(ui, server)