#' Auto-regressive app
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
      sliderInput("ar", "Autoregression coefficient", value = 0.5, min = -0.9, max = 0.9, step = 0.1),
      checkboxInput("show_model", label = "Show OSL model fit", value = FALSE)
    ),
    # Show a plot of the generated distribution
    h3(withMathJax("$$x_t = \\rho_1 x_{t-1} + \\epsilon_t$$")),
    navset_tab(

      nav_panel(title = "Autoregressive process",
        card(
          plotOutput("arPlot")
        )
      ),
      nav_panel(
        title = "Effect on models",
        card(
          "100 fitted models",
          plotOutput("slopes")
        ),
        card(
          "Distribution of p-values",
          plotOutput("hist_p")
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
      par(mar = c(2.2, 2.2, 1, 1), cex = 1.5, tcl = -0.1, mgp = c(1.2, 0.2, 0))
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
      }
      )
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
      }
        )
    })
    output$hist_p <- renderPlot({
      par(mar = c(2.2, 2.2, 1, 1), cex = 1.5, tcl = -0.1, mgp = c(1.2, 0.2, 0))
      hist(models()["p", ], breaks = seq(0, 1, 1/20), main = "", xlab = "p value")
      abline(h = n/20,  col = "#832424", lty = 2)
    })
    output$slopes <- renderPlot({
      par(mar = c(2.2, 2.2, 1, 1), cex = 1.5, tcl = -0.1, mgp = c(1.2, 0.2, 0))
      plot(1, 1, type = "n", xlim = c(1, 100), ylim = range(c(idd()[1, ], models()[1, ])), xlab = "Time", ylab = "Value")
      if(input$ar > 0) {
        apply(models(), 2, function(z) abline(a = z[1], b = z[2]))
        apply(idd(), 2, function(z) abline(a = z[1], b = z[2], col = "#832424"))
      } else {
        apply(idd(), 2, function(z) abline(a = z[1], b = z[2], col = "#832424"))
        apply(models(), 2, function(z) abline(a = z[1], b = z[2]))
      }

    })

    output$hist_slope <- renderPlot({
      par(mar = c(2.2, 2.2, 1, 1), cex = 1.5, tcl = -0.1, mgp = c(1.2, 0.2, 0))
      hist(models()["slope.x", ], main = "", xlab = "Slope")
    })

  }

  shinyApp(ui, server)
}
