#' Auto-regressive app
#' @details
#' This shiny app allows the user to explore the effect of autocorrelation on
#' regression models if the autocorrelation is ignored.
#' The app simulates and plots a first order autoregressive process,
#' a time series where the next value depends only the previous value.
#' The strength of the
#' autoregressive coefficient can be changed with a slider to values strictly
#' between -1 and 1. Positive values generate positive autocorrelation
#' (neighbouring values are more similar than expected);
#' negative values generate negative autocorrelation
#' (neighbouring values are less similar than expected).
#'
#' A linear regression model can be added to the plot. The model assumes that
#' there is no autocorrelation. If the autocorrelation is high, the slope is
#' often quite different from zero.
#'
#' In the second tab, the results of 100 linear models fitted to autocorrelated
#' time series and 100 models fitted to time series with independent
#' observations are shown.
#' A histogram shows the distribution of p-values for the models fitted to
#' autocorrelated time series.
#' If there was not autocorrelation, the histogram is expected to be flat.
#'
#' @examples
#' \dontrun{
#' ar_app()
#' }
#'
#' @import shiny
#' @import bslib
#' @importFrom graphics par
#' @importFrom stats arima.sim anova
#' @export
#'
ar_app <- function() {
  ui <- page_sidebar(
    title = "First-order autoregressive process",
    sidebar = sidebar(
      p(withMathJax("$$y_t = \\rho_1 y_{t-1} + \\epsilon_t$$")),
      sliderInput(
        "ar",
        "Autoregression coefficient",
        value = 0.5,
        min = -0.9,
        max = 0.9,
        step = 0.1
      ),
      checkboxInput("show_model", label = "Show OSL model fit", value = FALSE)
    ),
    # Show a plot of the generated distribution

    navset_tab(
      nav_panel(
        title = "Autoregressive process",
        card(
          plotOutput("arPlot")
        )
      ),
      nav_panel(
        title = "Effect on models",
        layout_column_wrap(
          width = 1 / 2,
          card(
            card_header(
              HTML(
                "100 models fitted to AR or <span style = 'color: #832424'>i.d.d.</span> series"
              )
            ),
            plotOutput("slopes")
          ),
          card(
            card_header("Distribution of p-values"),
            plotOutput("hist_p")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    n <- 100
    x <- 1:n
    y <- reactive({
      if (input$ar == 0) {
        y <- rnorm(n = n)
      } else {
        y <- arima.sim(list(ar = input$ar), n = n)
      }
      y
    })

    output$arPlot <- renderPlot({
      par(par_list)
      plot(x, y(), type = "l", xlab = "Time", ylab = "Value")
      if (input$show_model) {
        abline(lm(y() ~ x), col = "#832424")
      }
    })

    idd <- reactive({
      sapply(1:100, function(i) {
        y <- rnorm(n = 100)
        mod <- lm(y ~ x)
        coef(mod)
      })
    })

    models <- reactive({
      sapply(1:100, function(i) {
        if (input$ar == 0) {
          y <- rnorm(n = 100)
        } else {
          y <- arima.sim(list(ar = input$ar), n = n)
        }
        mod <- lm(y ~ x)
        c(coef(mod), p = anova(mod)$`Pr(>F)`[1])
      })
    })
    output$hist_p <- renderPlot({
      par(par_list)
      hist(
        models()["p", ],
        breaks = seq(0, 1, 1 / 20),
        main = "",
        xlab = "p value"
      )
      abline(h = n / 20, col = "#832424", lty = 2)
    })
    output$slopes <- renderPlot({
      par(par_list)
      plot(
        1,
        1,
        type = "n",
        xlim = c(1, 100),
        ylim = range(c(idd()[1, ], models()[1, ])),
        xlab = "Time",
        ylab = "Value"
      )
      if (input$ar > 0) {
        apply(models(), 2, function(z) abline(a = z[1], b = z[2]))
        apply(idd(), 2, function(z) abline(a = z[1], b = z[2], col = "#832424"))
      } else {
        apply(idd(), 2, function(z) abline(a = z[1], b = z[2], col = "#832424"))
        apply(models(), 2, function(z) abline(a = z[1], b = z[2]))
      }
    })

    output$hist_slope <- renderPlot({
      par(par_list)
      hist(models()["slope.x", ], main = "", xlab = "Slope")
    })
  }

  shinyApp(ui, server)
}
