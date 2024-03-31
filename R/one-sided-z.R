#' One sided z-test app
#' @import shiny
#' @import bslib
#' @importFrom graphics par polygon mtext abline
#' @importFrom stats dnorm qnorm
#' @export

one_sided_z_app <- function() {
  ui <- page_sidebar(
    title = "One sided Z-test",
    sidebar = sidebar(
      sliderInput("delta", "True mean", value = 1, min = 0.1, max = 2),
      sliderInput("sd", "Standard deviation", value = 1, min = 0.5, max = 2),
      sliderInput("n", "Sample size, n", value = 10, min = 10, max = 100),
      radioButtons("alpha", "Significance level, alpha", choices = c(0.05, 0.01, 0.001), selected = 0.05)
    ),
    # Show a plot of the generated distribution
    card(
      plotOutput("distPlot")
    )
  )

  server <- function(input, output, session) {
    output$distPlot <- renderPlot({
      n <- input$n
      alpha <- as.numeric(input$alpha)
      delta <- input$delta
      sd <- input$sd
      mx <- 2.5
      mn <- -1.5
      crit <-
        qnorm(
          alpha,
          mean = 0,
          sd = sd / sqrt(n),
          lower.tail = FALSE
        )
      H0 <- data.frame(x = seq(mn, mx, length = 201))
      H0$y <- dnorm(x = H0$x, mean = 0, sd = sd / sqrt(n))
      H0$what <- ifelse(H0$x < crit, yes = "True negative", no = "False positive")
      H0$hypothesis <- "H[0]"

      H1 <- data.frame(x = seq(mn, mx, length = 201))
      H1$y <- dnorm(x = H1$x, mean = delta, sd = sd / sqrt(n))
      H1$what <- ifelse(H1$x < crit, yes = "True negative", no = "False positive")
      H1$hypothesis <- "H[1]"

      par(oma = c(2, 2, 0, 0), mar = c(1.2, 2, 2, 1), mfrow = c(2, 1), cex = 1.5, tcl = -0.1, mgp = c(3, 0.2, 0))
      plot(H0$x, H0$y, type = "n", main = expression(H[0] ~ is ~ true), xlab = "", ylab = "")
      polygon(H0$x, H0$y)
      polygon(c(H0$x[H0$x > crit][1], H0$x[H0$x > crit]), c(0, H0$y[H0$x > crit]), col = "#832424")
      abline(v = crit)

      plot(H1$x, H1$y, type = "n", main = expression(H[1] ~ is ~ true), xlab = "", ylab = "")
      polygon(H1$x, H1$y)
      polygon(c(H1$x[H1$x > crit][1], H1$x[H1$x > crit]), c(0, H1$y[H1$x > crit]), col = "#832424")
      abline(v = crit)
      mtext(text = "x", side = 1, line = 0, outer = TRUE, 1.5)
      mtext(text = "Density", side = 2, line = 0, outer = TRUE, cex = 1.5)
    })
  }

  shinyApp(ui, server)
}
