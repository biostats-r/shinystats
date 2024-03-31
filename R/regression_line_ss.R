#' Regression line ss app
#' @import shiny
#' @import bslib
#' @importFrom graphics abline par rect segments
#' @importFrom stats lm rnorm
#' @export

regression_line_ss_app <- function() {
  ui <- page_sidebar(
    # Application title
    title = "Minimise the sum of squared residuals",
    sidebar = sidebar(
      radioButtons("residuals", "Show residuals",
        choices = c("None", "Residuals", "Squared-residuals"), selected = "None"
      ),
      checkboxInput("best", "Show best model"),
      textOutput("slope"),
      textOutput("intercept")
    ),
    # Show a plot of the generated distribution
    card(
      plotOutput("plot", click = "plot_click")
    ),
    value_box(
      title = "Sum of Squares",
      value = textOutput("SumSq")
    )
  )

  server <- function(input, output, session) {
    # make some data
    set.seed(Sys.Date())
    data <- data.frame(x = 1:10, y = rnorm(10, 1:10))
    xlab <- "Predictor"
    ylab <- "Response"

    v <- reactiveValues(
      click1 = NULL, # Represents the first mouse click, if any
      intercept = NULL, # After two clicks, this stores the intercept
      slope = NULL, # after two clicks, this stores the slope,
      pred = NULL,
      resid = NULL
    )

    # Handle clicks on the plot
    observeEvent(input$plot_click, {
      if (is.null(v$click1)) {
        # We don't have a first click, so this is the first click
        v$click1 <- input$plot_click
      } else {
        # We already had a first click, so this is the second click.
        # Make slope and intercept from the previous click and this one.
        v$slope <- (input$plot_click$y - v$click1$y) / (input$plot_click$x - v$click1$x)
        v$intercept <- (input$plot_click$y + v$click1$y) / 2 - v$slope * (input$plot_click$x + v$click1$x) / 2

        # predictions & residuals
        v$pred <- v$intercept + v$slope * data$x
        v$resid <- v$pred - data$y
        # And clear the first click so the next click starts a new line.
        v$click1 <- NULL
      }
    })


    output$plot <- renderPlot({
      par(cex = 1.5, mar = c(3, 3, 1, 1), tcl = -0.1, mgp = c(2, 0.2, 0))
      plot(data, pch = 16, xlab = xlab, ylab = ylab)
      if (input$best) {
        mod <- lm(y ~ x, data = data)
        abline(mod, col = "navy", lty = "dashed")
      }
      if (!is.null(v$intercept)) {
        abline(a = v$intercept, b = v$slope)
        if (input$residuals == "Residuals") {
          segments(
            x0 = data$x,
            x1 = data$x,
            y0 = data$y,
            y1 = v$pred
          )
        } else if (input$residuals == "Squared-residuals") {
          w <- par("pin")[1] / diff(par("usr")[1:2])
          h <- par("pin")[2] / diff(par("usr")[3:4])
          asp <- w / h
          rect(
            xleft = ifelse(v$resid < 0, data$x, data$x + v$resid / asp),
            ybottom = ifelse(v$resid < 0, v$pred, data$y),
            xright = ifelse(v$resid < 0, data$x + v$resid / asp, data$x),
            ytop = ifelse(v$resid < 0, data$y, v$pred),
            col = "#83242455", border = "#832424"
          )
        }
      }
    })
    output$SumSq <- renderText({
      if (is.null(v$click1) && is.null(v$intercept)) { # initial state
        "Click on the plot to start a line"
      } else if (!is.null(v$click1)) { # after one click
        "Click again to finsh a line"
      } else if (input$residuals == "None") {
        "Use radio buttons to display residuals"
      } else {
        paste0("Sum of squares = ", signif(sum(v$resid^2), 3))
      }
    })
    output$slope <- renderText({
      if (!is.null(v$slope)) {
        paste0("Slope = ", signif(v$slope, 3))
      } else {
        ""
      }
    })
    output$intercept <- renderText({
      if (!is.null(v$intercept)) {
        paste0("Intercept = ", signif(v$intercept, 3))
      } else {
        ""
      }
    })
  }

  shinyApp(ui, server)
}
