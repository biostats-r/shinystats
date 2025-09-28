#' F distribution
#' @description
#' A shiny app to visualise how f-distribution changes with the number of
#' numerator and denominator degrees of freedom.
#'
#' @import shiny
#' @import bslib
#' @importFrom graphics par polygon lines text
#' @importFrom stats qf df
#' @examples
#' \dontrun{
#' f_dist_app()
#' }

#' @export

f_dist_app <- function() {
  ui <- page_sidebar(
    # Application title
    title = h1("F distribution"),

    # Sidebar with a slider input for number of bins
    sidebar = sidebar(
      sliderInput(
        "numerator",
        "Regression degrees of freedom:",
        min = 1,
        max = 10,
        round = TRUE,
        value = 1
      ),
      sliderInput(
        "denominator",
        "Residual degrees of freedom:",
        min = 1,
        max = 10,
        round = TRUE,
        value = 5
      ),
      radioButtons(
        "alpha",
        "\u03b1:",
        c("p = 0.05" = "0.05", "p = 0.01" = "0.01")
      )
    ),
    withMathJax(h3(
      "$$F = \\frac{SS_{regression} / df_{regression}}{SS_{residual} / df_{residual}}$$"
    )),
    # Show a plot of the generated distribution
    card(
      plotOutput("distPlot")
    )
  )

  # Define server logic required to draw a histogram
  f_test_server <- function(input, output) {
    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      axis_max <- 500
      xmax <- min(
        axis_max,
        qf(
          p = 0.995,
          df1 = input$numerator,
          df2 = input$denominator
        )
      )
      x <- seq(0, ceiling(xmax), length.out = 200)
      y <- df(x, df1 = input$numerator, df2 = input$denominator)
      x <- x[is.finite(y)]
      y <- y[is.finite(y)]

      xthresh <- qf(
        p = 1 - as.numeric(input$alpha),
        df1 = input$numerator,
        df2 = input$denominator
      )
      if (xthresh > axis_max) {
        xthresh <- NA_real_
        x2 <- numeric(0)
      } else {
        x2 <- seq(xthresh, ceiling(xmax), length.out = 100)
      }
      y2 <- df(x2, df1 = input$numerator, df2 = input$denominator)
      # df2 <- data.frame(x = x2, y = y2)

      par(cex = 1.5, mar = c(3, 3, 1, 1), tcl = -0.1, mgp = c(2, 0.2, 0))
      plot(
        x,
        y,
        type = "n",
        xlab = expression(italic(F) ~ value),
        ylab = "Density"
      )
      polygon(c(x[1], x, x[length(x)]), c(0, y, 0), col = "grey80", border = NA)
      polygon(
        c(x2[1], x2, x2[length(x2)]),
        c(0, y2, 0),
        col = "#832424",
        border = NA
      )
      lines(x, y)
      text(
        xthresh,
        y2[1] + 0.05 * (max(y) - y2[1]),
        labels = bquote(
          italic(F)[
            .(input$numerator) * "," ~ .(input$denominator) * ";" ~
              .(input$alpha)
          ] ==
            .(round(xthresh, 2))
        ),
        adj = 0
      )
    })
  }
  # Run the application
  shinyApp(ui = ui, server = f_test_server)
}
