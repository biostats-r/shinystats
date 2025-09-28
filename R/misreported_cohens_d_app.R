#' Misreported Cohen's D app

#' @description
#' A shiny app that helps visualise what happens when Cohen's D is misreported.
#'
#' @import shiny
#' @import bslib
#' @importFrom graphics boxplot par
#' @importFrom stats arima.sim anova acf pacf
#' @examples
#' \dontrun{
#' misreported_cohens_d_app()
#' }
#' @export

misreported_cohens_d_app <- function() {
  ui <- page_sidebar(
    title = "Misreporting Cohen's D",
    sidebar = sidebar(
      p("Cohens's D is the difference in means divided by the standard deviation. Sometimes it is miscalculated, dividing the differnce by standard error instead."),
      p("As the standard error is smaller than the standard deviation, expecially so when the sample size is large, extreme Cohens's D values can be reported."),
      shinyWidgets::sliderTextInput("cohens_d", "Reported Cohen's D", choices = c(-50, -30, -10, -5,  -1, 1,  5, 10, 30, 50), selected = 30, grid = TRUE),
      checkboxInput("error", "Cohen's D is miscalculated", value = FALSE),
      shinyWidgets::sliderTextInput("samplesize", "Sample Size (per group)", choices = c(10, 20, 40, 80), selected = 20, grid = TRUE),
    ),
    card(
      card_header(textOutput("plot_header")),
      plotOutput("plot")
    ),
    layout_columns(
      value_box("Cohen's D", textOutput("cohens_d"))
    )
  )
  server <- function(input, output, session) {
    mean1 <- 10
    delta <- 10

    mean2 <- reactive(mean1 + delta * sign(input$cohens_d))
    sd1 <- reactive(
      if (!input$error) {
        delta / abs(input$cohens_d)

      } else {
        (delta / abs(input$cohens_d)) * sqrt(input$samplesize)
      }
    )

    output$plot <- renderPlot({
      par(par_list)
      dist1 <- rnorm(1000, mean1, sd1())
      dist2 <- rnorm(1000, mean2(), sd1())
      boxplot(list(control = dist1, treatment = dist2),
              col = c("grey", palette[2]),
              ylab = "Response")
    })

    output$plot_header <- renderText({
      if (!input$error) {
        "Boxplot of what the data might look like if the reported Cohen's D is correct ."
      } else {
        "Boxplot of what the data might look like if the reported Cohen's D had been miscalculated."
      }
    })

    output$cohens_d <- renderText({
      if(!input$error){
        input$cohens_d
      } else {
        round(delta / sd1(), 2) * sign(input$cohens_d)
      }
    })
  }

  shinyApp(ui, server)

}
