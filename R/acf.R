#' Autocorrelation function (ACF) app
#' @description
#' A shiny app to help understand understand the autocorrelation function.
#'
#' The first plot shows a simulated time series from a first order
#' autoregressive process with the autoregressive coefficient set by the slider.
#' When the lag slider is set above zero, a lagged version of the
#' time series appears.
#'
#' The second plot shows the correlation between the time series and the
#' lagged version.
#' When the lag is zero, the time series is perfectly correlated with itself.
#'
#' The third plot shows the sample autocorrelation function (ACF) for the
#' time series with the current lag highlighted.
#'
#' @examples
#' \dontrun{
#' acf_app()
#' }
#'
#' @import shiny
#' @import bslib
#' @importFrom graphics par
#' @importFrom stats arima.sim ts.union as.ts acf
#' @export
#'
acf_app <- function() {
  ui <- page_sidebar(
    title = "Measuring Autocorrelation: sample autocorrelation",
    sidebar = sidebar(
      p("First order autoregressive process"),
      p(withMathJax("$$y_t = \\rho_1 y_{t-1} + \\epsilon_t$$")),
      sliderInput(
        "ar",
        "Autoregression coefficient",
        value = 0.5,
        min = -0.9,
        max = 0.9,
        step = 0.1
      ),
      sliderInput("lag", label = "Lag", value = 0, min = 0, max = 10, step = 1)
    ),

    # Sample autocorrelation
    p(withMathJax(
      "$$\\hat\\rho_j = \\frac{\\sum_{t=j+1}^T(y_t-\\bar{y})(y_{t-j}-\\bar{y})}{\\sum_{t=1}^T(y_t-\\bar{y})^2}$$"
    )),
    layout_column_wrap(
      # Show a plot of the generated ts with lag
      card(plotOutput("lagPlot")),
      card(plotOutput("lagCorPlot"))
    ),
    layout_column_wrap(
      value_box(title = textOutput("cor_label"), value = textOutput("acf_lag")),
      card(plotOutput("acfPlot"))
    )
  )

  server <- function(input, output, session) {
    # graphical parameters
    par_list <- list(
      mar = c(2.2, 2.2, 0.4, 1),
      cex = 1.5,
      tcl = -0.1,
      mgp = c(1.2, 0.2, 0)
    )

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
      par()
      plot(y(), type = "l", xlab = "Time", ylab = "Value")
      lines(
        stats::lag(y(), k = input$lag),
        type = "l",
        col = "#832424",
        lty = 2
      )
    })

    output$lagCorPlot <- renderPlot({
      dat <- ts.union(y(), stats::lag(y(), k = input$lag)) |>
        as.data.frame()
      colnames(dat) <- c("y", "y_lag")
      par(par_list)
      plot(
        dat,
        type = "p",
        xlab = "Original Series",
        ylab = "Lagged Series",
        asp = 1
      )
      abline(lm(y_lag ~ y, data = dat), col = "skyblue", lty = 2)
    })
    output$cor_label <- renderText({
      paste("Sample Autocorrelation at lag =", input$lag)
    })

    acf_fit <- reactive({
      acf(y(), plot = FALSE)
    })
    output$acf_lag <- renderText(
      acf_fit()$acf[input$lag + 1, , ] |>
        round(3)
    )
    output$acfPlot <- renderPlot({
      par(par_list)
      plot(acf_fit(), main = "")
      segments(
        x0 = input$lag,
        y0 = 0,
        y1 = acf_fit()$acf[input$lag + 1, , ],
        col = "#832424",
        lwd = 4
      )
    })
  }
  shinyApp(ui, server)
}
