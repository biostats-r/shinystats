#' Auto-regressive app
#' @import shiny
#' @import bslib
#' @importFrom graphics par
#' @importFrom stats arima.sim ts.union as.ts acf
#' @export
#'
acf_app <- function() {
  ui <- page_sidebar(
    title = "Measuring Autocorrelation",
    sidebar = sidebar(
      h4(withMathJax("$$y_t = \\rho_1 y_{t-1} + \\epsilon_t$$")),
      sliderInput("ar", "Autoregression coefficient", value = 0.5, min = -0.9, max = 0.9, step = 0.1),
      sliderInput("lag", label = "Lag", value = 0, min = 0, max = 10, step = 1)
    ),

    # Sample autocorrelation
    h4("Sample Autocorrelation"),
    h4(withMathJax("$$\\hat\\rho_j = \\frac{\\sum_{t=j+1}^T(y_t-\\bar{y})(y_{t-j}-\\bar{y})}{\\sum_{t=1}^T(y_t-\\bar{y})^2}$$")),

    layout_column_wrap(
      # Show a plot of the generated ts with lag
      card(plotOutput("lagPlot")),
      card(plotOutput("lagCorPlot"))
    ),
    value_box(title = textOutput("cor_label"), value = textOutput("acf_lag"))






  )

  server <- function(input, output, session) {
    n <- 100
    y <- reactive({
      if (input$ar == 0) {
        y <- as.ts(rnorm(n = n))
      } else {
        y <- arima.sim(list(ar = input$ar), n = n)
      }
      y
    })

    output$lagPlot <- renderPlot({
      par(mar = c(2.2, 2.2, 1, 1), cex = 1.5, tcl = -0.1, mgp = c(1.2, 0.2, 0))
      plot(y(), type = "l", xlab = "Time", ylab = "Value")
      lines(stats::lag(y(), k = input$lag), type = "l", col = "#832424", lty = 2)

    })

    output$lagCorPlot <- renderPlot({
      dat <- ts.union(y(), stats::lag(y(), k = input$lag)) |>
        as.data.frame()
      colnames(dat) <- c("y", "y_lag")
      par(mar = c(2.2, 2.2, 1, 1), cex = 1.5, tcl = -0.1, mgp = c(1.2, 0.2, 0))
      plot(dat, type = "p", xlab = "Original Series", ylab = "Lagged Series", asp = 1)
      abline(lm(y_lag ~ y, data = dat), col = "skyblue", lty = 2)

    })
    output$cor_label <- renderText(paste("Sample Autocorrelation at lag =", input$lag))
    output$acf_lag <- renderText(acf(y(), plot = FALSE)$acf[input$lag + 1, , ])


  }

  shinyApp(ui, server)
}
