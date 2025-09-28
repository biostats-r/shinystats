#' Standard error app
#' @description
#' A shiny app to visualise how the population standard deviation and the number
#' samples affects the uncertainty of the mean.
#'
#' @import shiny
#' @import bslib
#' @importFrom graphics par hist lines abline
#' @importFrom stats rnorm dnorm
#' @examples
#' \dontrun{
#' standard_error_app()
#' }

#' @export

standard_error_app <- function(){
ui <- page_sidebar(
  sidebar = sidebar(
    h4("Define population"),
    sliderInput(
      inputId = "mean", label = "Mean",
      min = -5, max = 5, value = 2
    ),
    sliderInput(
      inputId = "sd", label = "Standard deviation",
      min = 1, max = 5, value = 2
    ),

    h4("Define sample"),
    sliderInput(
      inputId = "n", label = "Number of Observation",
      min = 5, max = 50, step = 5, value = 10
    ),
  ),
  navset_underline(
    id = "nav",
    nav_panel(
      title = "Population & Samples",
      card(plotOutput("pop_hist", height = "250px")),
      card(plotOutput("sample_hist", height = "250px")),
    ),
    nav_panel(
      title = "Sample Means",
      card(
        p("Mean of sample means = population mean"),
        p("Standard deviation of sample means = standard error = population standard error / sqrt(sample size)"),
        plotOutput("means_hist")
      )
    )
  )
)

server <- function(input, output) {
  population <- reactive({
    n <- 100000
    rnorm(n = n, mean = input$mean, sd = input$sd)
  })


  dat <- reactive({
    invalidateLater(2000)
    sample(population(), size = input$n)
  })

  means <- reactive({
    vapply(1:100, \(i){
      mean(sample(population(), size = input$n))
    },
    FUN.VALUE = 0
    )
  })


  output$pop_hist <- renderPlot({
    par(par_list)

    xlim <- range(population())
    hist(population(),
      xlim = xlim, main = "Population",
      breaks = seq(floor(xlim[1]), ceiling(xlim[2]), length = 20),
      xlab = "Value", include.lowest = TRUE
    )
    abline(v = mean(population()), col = palette[1], lwd = 2)
  })



  output$sample_hist <- renderPlot({
    par(par_list)

    xlim <- range(population())
    hist(dat(),
      xlim = xlim, main = "Sample",
      breaks = seq(floor(xlim[1]), ceiling(xlim[2]), length = 20),
      xlab = "Value"
    )
    abline(v = mean(population()), col = palette[1], lwd = 3)
    abline(v = mean(dat()), col = palette[2], lwd = 3)
  })

  output$means_hist <- renderPlot({
    par(par_list)

    xlim <- input$mean + c(-input$sd / sqrt(5), input$sd / sqrt(5)) * 4 # 4 se
    hist(means(),
      xlim = xlim, main = "Mean", freq = FALSE,
      breaks = seq(floor(xlim[1]), ceiling(xlim[2]), length = 20),
      xlab = "Value"
    )
    abline(v = mean(population()), col = palette[1], lwd = 2)
    lines(
      x = seq(xlim[1], xlim[2], length = 100),
      y = dnorm(seq(xlim[1], xlim[2], length = 100),
        mean = mean(population()),
        sd = input$sd / sqrt(input$n)
      ),
      col = palette[2],
      lwd = 2
    )
  })
}

shinyApp(ui, server)
}
