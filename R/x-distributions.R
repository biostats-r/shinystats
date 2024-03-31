#' x-distributions
#' @import shiny
#' @import bslib
#' @importFrom graphics hist barplot par abline layout
#' @importFrom stats lm resid dnorm sd fitted rnorm
#' @export

x_distribution_app <- function() {
  ui <- page_sidebar(
    # Application title
    title = "Which distribution is important?",
    sidebar = sidebar(
      radioButtons("dist", "Predictor distributor", choices = c("Normal", "Skewed", "Bimodal"), selected = "Normal"),
      checkboxInput("show_residuals", label = "Show residuals", value = FALSE),
      radioButtons("residual_plot", "Residual plot", choices = c("None", "Histogram", "QQplot"), selected = "None")
    ),
    # Show a plot of the generated distribution
    card(
      plotOutput("distPlot")
    )
  )

  horizHist <- function(
      Data,
      breaks = "Sturges",
      freq = TRUE,
      plot = TRUE,
      col = par("bg"),
      border = par("fg"),
      las = 1,
      xlab = if (freq) "Frequency" else "Density",
      main = paste("Histogram of", deparse(substitute(Data))),
      ylim = range(HBreaks),
      labelat = pretty(ylim),
      labels = labelat,
      ...) {
    a <- hist(Data, plot = FALSE, breaks = breaks)
    HBreaks <- a$breaks
    hpos <- function(Pos) (Pos - HBreaks[1]) * (length(HBreaks) - 1) / diff(range(HBreaks))
    if (plot) {
      barplot(if (freq) a$counts else a$density,
        space = 0, horiz = TRUE, ylim = hpos(ylim), col = col, border = border,
        xlab = xlab, main = main, ...
      )
    }
  } # End of function

  server <- function(input, output, session) {
    x <- reactive(switch(input$dist,
      Normal = rnorm(50),
      Skewed = exp(rnorm(50)),
      Bimodal = c(rnorm(25, mean = 1, sd = 0.4), rnorm(25, mean = 5, sd = 0.4))
    ))
    y <- reactive(rnorm(length(x()), mean = x()))
    mod <- reactive(lm(y() ~ x()))

    output$distPlot <- renderPlot({
      layout(
        mat = matrix(c(1, 2, 4, 0, 3, 0),
          nrow = 3,
          ncol = 2
        ),
        heights = c(1, 3, 3), # Heights of the two rows
        widths = c(3, 1)
      ) # Widths of the two columns
      par(mar = c(1.2, 2, 2, 1), cex = 1.3, tcl = -0.1, mgp = c(1.5, 0.2, 0))
      par(mar = c(0, 3, 0, 0))
      hist(x(), axes = FALSE, main = "", ylab = "", xlab = "", breaks = 10)
      par(mar = c(3, 3, 0, 0))
      plot(x(), y(), xlab = "Predictor", ylab = "Response")
      abline(coef = coef(mod()))
      if (input$show_residuals) {
        segments(x0 = x(), y0 = y(), x1 = x(), y1 = fitted(mod()))
      }
      par(mar = c(3, 0.2, 0, 0))
      horizHist(y(), axes = FALSE, main = "", ylab = "", xlab = "", col = "lightgrey", breaks = 10)

      if (input$residual_plot == "Histogram") {
        par(mar = c(3, 3, 1, 0))
        hist(resid(mod()), freq = FALSE)
        x_norm <- seq(-10, 10, length.out = 100)
        lines(x_norm, dnorm(x_norm, mean = 0, sd = sd(resid(mod()))), col = "#832424", xlab = "Residuals", main = "Histogram of residuals")
      } else if (input$residual_plot == "QQplot") {
        par(mar = c(3, 3, 1, 0))
        plot(mod(), which = 2, id.n = 0)
      }
    })
  }

  shinyApp(ui, server)
}
