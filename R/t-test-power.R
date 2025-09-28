#' T-test power app
#' @description
#' A shiny app to visualise how the effect size and number of observations affect
#' the power of a t-test.
#'
#' The first panel show the expected distribution of two a control and treatment group
#' with a specified difference in means and standard deviation. The uncertainty of the means
#' can also be shown.
#'
#' The second panel shows, in the upper pane, the distribution of t-values under the null hypothesis.
#' The red portion of the curve show values that exceed the 95% significance thresholds.
#' These represent false positives (type 1 errors).
#' The lower pane shows the distribution of t-values under the alternative hypothesis.
#' The red portion of the curve show values that exceed the 95% significance thresholds.
#' These are true positives. The area under the curve in the red region is the power of the test.
#'
#' @import shiny
#' @import bslib
#' @importFrom graphics par polygon mtext abline
#' @importFrom stats dnorm qnorm
#' @importFrom stats dt pt qt
#' @importFrom utils tail
#' @examples
#' \dontrun{
#' t_test_power_app()
#' }

#' @export

t_test_power_app <- function() {
  ui <- page_sidebar(
    title = "t-test power analysis",
    sidebar = sidebar(
      sliderInput("delta", "True mean difference", value = 1, min = 0.1, max = 2),
      sliderInput("sd", "Standard deviation", value = 1, min = 0.5, max = 2),
      sliderInput("n", "Sample size, n", value = 10, min = 10, max = 100),
      radioButtons("sd_se", "Show",
                   choices = c("Raw data distribution", "Uncertainty of mean"),
                   selected = "Raw data distribution"),
      radioButtons(inputId = "alternative", label = "Alternative",
                   choices = c("Two-sided", "One-sided - greater than", "One-sided - less than"),
                   selected = "Two-sided"),
      radioButtons(inputId = "alpha", label = "Significance level, alpha",
                   choices = c(0.05, 0.01, 0.001), selected = 0.05)
    ),
    bslib::navset_underline(
      nav_panel(
        title = "Data distribution",
        card(
          plotOutput("dataPlot")
        ),
        value_box(
          title = "Cohen's d",
          value = textOutput("cohens")
        )
      ),
      nav_panel(
        title = "Distribution of t-values",
        card(
          # Show a plot of the generated distribution
          plotOutput("statPlot")
        ),
        layout_columns(
          card(
            card_header("t-statistic"),
            p(withMathJax("$$t = \\frac{Difference~in~means}{Uncertainty~of~the~means}$$"))
          ),
          value_box(
            title = "Risk of a false positive if H0 is true",
            value = textOutput("false_positive")
          ),
          value_box(
            title = "Probability null hypothesis is correctly rejected if H1 is true",
            value = textOutput("rejected")
          )
        )
      )
    ),
  )

  server <- function(input, output, session) {
    # top plot. Distributions and se about mean
    # second plot. null hypothesis is true t-dist
    # third plot. null hypothesis is false non central t-dist

    output$dataPlot <- renderPlot({
      par(par_list)
      x <- seq(-10, 10, length = 400)
      if (input$sd_se == "Raw data distribution") {
        control <- dnorm(x, mean = 0, sd = input$sd)
        treat <- dnorm(x, mean = input$delta, sd = input$sd)
      } else {
        control <- dnorm(x, mean = 0, sd = input$sd / sqrt(input$n))
        treat <- dnorm(x, mean = input$delta, sd = input$sd / sqrt(input$n))
      }

      plot(x, control, type = "l", xlab = "Value", ylab = "Density", xlim = c(-5, 10))
      polygon(x, control, col = set_alpha("#aaaaaa", 0.5))
      polygon(x, treat, col = set_alpha(palette[2], 0.5))
    })

    alpha <- reactive({
      alpha <- as.numeric(input$alpha)
      alpha <- switch(input$alternative,
        "Two-sided" = c(alpha / 2, 1 - alpha / 2),
        "One-sided - greater than" = 1 - alpha,
        "One-sided - less than" = alpha
      )
      alpha
    })

    crit <- reactive({
      crit <- qt(
        p = alpha(),
        df = input$n * 2 - 1,
        lower.tail = TRUE
      )
      crit
    })


    output$statPlot <- renderPlot({
      par(par_list)
      n <- input$n


      delta <- input$delta
      sd <- input$sd
      mx <- 10
      mn <- -8

      h0 <- data.frame(x = sort(c(crit(), seq(mn, mx, length = 201))))
      h0$y <- dt(x = h0$x, df = input$n * 2 - 1)

      h1 <- data.frame(x = c(h0$x, 100))
      h1$y <- dt(x = h1$x, df = input$n * 2 - 1, ncp = delta / (sd / sqrt(input$n / 2)))

      par(oma = c(2, 2, 0, 0), mar = c(1.2, 2, 2, 1), mfrow = c(2, 1), cex = 1.5, tcl = -0.1, mgp = c(3, 0.2, 0))

      # H0 panel
      plot(h0$x, h0$y, type = "n", main = expression(H[0] ~ is ~ true), xlab = "t statistic", ylab = "", xlim = c(mn + 4, mx - 2))
      polygon(h0$x, h0$y, col = "grey80")

      switch(input$alternative,
        "Two-sided" = {
          # below lower limit
          polygon(c(h0$x[h0$x <= crit()[1]], tail(h0$x[h0$x <= crit()[1]], 1)),
            c(h0$y[h0$x <= crit()[1]], 0),
            col = "#832424"
          )
          # above upper limit
          polygon(c(h0$x[h0$x >= crit()[2]][1], h0$x[h0$x >= crit()[2]]),
            c(0, h0$y[h0$x >= crit()[2]]),
            col = "#832424"
          )
        },
        "One-sided - greater than" = {
          # above upper limit
          polygon(c(h0$x[h0$x >= crit()][1], h0$x[h0$x >= crit()]),
            c(0, h0$y[h0$x >= crit()]),
            col = "#832424"
          )
        },
        "One-sided - less than" = {
          # below lower limit
          polygon(c(h0$x[h0$x <= crit()], tail(h0$x[h0$x <= crit()], 1)),
            c(h0$y[h0$x <= crit()], 0),
            col = "#832424"
          )
        }
      )
      abline(v = crit())


      # H1 panel
      plot(h1$x, h1$y, type = "n", main = expression(H[1] ~ is ~ true), xlab = "", ylab = "", xlim = c(mn + 4, mx - 2))
      polygon(h1$x, h1$y, col = "grey80")

      switch(input$alternative,
        "Two-sided" = {
          # below lower limit
          polygon(c(h1$x[h1$x <= crit()[1]], tail(h1$x[h1$x <= crit()[1]], 1)),
            c(h1$y[h1$x <= crit()[1]], 0),
            col = "#832424"
          )
          # above upper limit
          polygon(c(h1$x[h1$x >= crit()[2]][1], h1$x[h1$x >= crit()[2]]),
            c(0, h1$y[h1$x >= crit()[2]]),
            col = "#832424"
          )
        },
        "One-sided - greater than" = {
          # above upper limit
          polygon(c(h1$x[h1$x >= crit()][1], h1$x[h1$x >= crit()]),
            c(0, h1$y[h1$x >= crit()]),
            col = "#832424"
          )
        },
        "One-sided - less than" = {
          # below lower limit
          polygon(c(h1$x[h1$x <= crit()], tail(h1$x[h1$x <= crit()], 1)),
            c(h1$y[h1$x <= crit()], 0),
            col = "#832424"
          )
        }
      )


      abline(v = crit())
      mtext(text = "x", side = 1, line = 0, outer = TRUE, 1.5)
      mtext(text = "Density", side = 2, line = 0, outer = TRUE, cex = 1.5)
    })



    output$cohens <- renderText({
      round(input$delta / input$sd, 2)
    })

    output$false_positive <- renderText({
      input$alpha
    })

    output$rejected <- renderText({
      switch(input$alternative,
        "Two-sided" = round(1 - pt(qt(alpha()[2], df = input$n * 2 - 1), df = input$n * 2 - 1, ncp = input$delta / (input$sd / sqrt(input$n / 2))), 2),
        "One-sided - greater than" = {
          round(1 - pt(qt(alpha(), df = input$n * 2 - 1), df = input$n * 2 - 1, ncp = input$delta / (input$sd / sqrt(input$n / 2))), 2)
        },
        "One-sided - less than" = {
          0
        }
      )
    })
  }
  shinyApp(ui, server)
}
