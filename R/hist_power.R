#' t-test simulation app
#' @description
#' A shiny app to visualise how
#'
#' @import shiny
#' @import bslib
#' @importFrom graphics par stripchart polygon plot abline
#' @importFrom stats rnorm dt qt t.test
#' @examples
#' \dontrun{
#' t_test_sim_app()
#' }

#' @export

t_test_sim_app <- function(){
ui <- page_sidebar(
  title = "Simulation of t-tests",
  sidebar = sidebar(
    h4("Describe populations"),

    sliderInput(
      inputId = "delta", label = "Difference in means",
      min = 0, max = 2, step = 0.1, value = 1
    ),
    sliderInput(
      inputId = "sd", label = "Standard deviation",
      min = 1, max = 5, value = 2
    ),
    h4("Describe samples"),
    sliderInput(
      inputId = "n", label = "Number of Observation in each group",
      min = 5, max = 50, step = 5, value = 10
    ),
    value_box(
      title = "Cohen's d",
      value = textOutput("cohens")
    ),
  ),
  navset_underline(
    nav_panel(
      title = "Samples",
      card(plotOutput("stripplot")),
      value_box(
        title = "Estimated effect of treatment (95% conf interval)",
        value = textOutput("effect")
      )
    ),
    nav_panel(
      title = "Estimates and uncertainty",
      card(plotOutput("estimate_ci", height = "500px"))
    ),
    nav_panel(
      title = "Difference in means",
      card(plotOutput("delta_hist"))
    )
  )
)

server <- function(input, output) {
  population <- reactive({
    n <- 100000
    list(
      control = rnorm(n = n, mean = 0, sd = input$sd),
      treatment = rnorm(n = n, mean = input$delta, sd = input$sd)
    )
  })


  dat <- reactive({
    invalidateLater(2000)
    list(
      control = sample(population()$control, size = input$n),
      treatment = sample(population()$treatment, size = input$n)
    )
  })

  output$cohens <- renderText({
    cohen_d <- (input$delta) / input$sd
    size <- cut(cohen_d,
      breaks = c(0, 0.2, 0.8, 1.1, Inf),
      labels = c("small", "medium", "large", "very large"),
      right = TRUE
    )

    paste0(round(cohen_d, 2), " - ", size)
  })

  output$stripplot <- renderPlot({
    par(par_list)

    ylim <- range(unlist(population()))
    stripchart(dat(),
      ylim = ylim, vertical = TRUE,
      method = "jitter", jitter = 0.15, pch = 1,
      ylab = "Value"
    )
    means <- vapply(dat(), FUN = mean, FUN.VALUE = 0)
    ses <- vapply(dat(), FUN = sd, FUN.VALUE = 0) / sqrt(input$n)
    points(x = 1:2, y = means, pch = 16, col = palette[1], cex = 2.5)
    segments(
      x0 = 1:2, y0 = means - 1.96 * ses, y1 = means + 1.96 * ses,
      lwd = 3, col = palette[1]
    )
  })

  t_res <- reactive({
    t.test(dat()$treatment, dat()$control)
  })

  output$effect <- renderText({
    est <- t_res()$estimate[1] - t_res()$estimate[2]
    ci <- t_res()$conf.int |>
      round(2) |>
      paste(collapse = ", ")
    paste0(round(est, 2), " (", ci, ")")
  })

  est_ci <- reactive({
    vapply(1:100,
           FUN = \(i){
             tst <- t.test(
               x = sample(population()$treatment, size = input$n),
               y = sample(population()$control, size = input$n))
             c(estimate = tst$estimate[1] - tst$estimate[2],
               ci_low = tst$conf.int[1],
               ci_high = tst$conf.int[2])

           },
           FUN.VALUE = c(0, 0, 0)
    )


  })

  output$estimate_ci <- renderPlot({
    par(par_list)
    col <- ifelse(est_ci()[2, ] > 0, palette[1], "grey60")

    plot(est_ci()[1, ], y = 1:100,
         xlim = c(min(input$delta - 3, min(est_ci()[2, ])), max(input$delta + 3, max(est_ci()[3, ]))),
         col = col, pch = 20,
         xlab = "Estimate and 95% confidence interval", ylab = "Trial number")
    segments(x0 = est_ci()[2, ], x1 = est_ci()[3, ], y = 1:100, y1 = 1:100, col = col)
    abline(v = input$delta)
    abline(v = 0, lty = 2)
    })

  delta <- reactive({
    vapply(1:100,
      FUN = \(i){
        mean(sample(population()$treatment, size = input$n)) -
          mean(sample(population()$control, size = input$n))
      },
      FUN.VALUE = 0
    )
  })

  output$delta_hist <- renderPlot({
    par(par_list)

    h <- hist(delta(),
      plot = FALSE
    )

    plot(h,
      freq = FALSE,
      main = "Difference in means",
      xlab = "Treatment - Control",
      xlim = range(c(0, h$breaks))
    )
    # plot t distribution
    lim <- qt(c(0.001, 0.999), df = input$n * 2 - 2)
    thresh95 <- qt(c(0.025, 0.975), df = input$n * 2 - 2)
    x <- c(
      seq(lim[1], thresh95[1], length = 50),
      seq(thresh95[1], thresh95[2], length = 50),
      seq(thresh95[2], lim[2], length = 50)
    )
    y <- dt(x, df = input$n * 2 - 2)
    polygon(
      x = c(x, max(x), min(x)),
      y = c(y, 0, 0),
      col = palette[2],
      lwd = 2,
    )

    polygon(
      x = c(x[x >= thresh95[2]], max(x), thresh95[2]),
      y = c(y[x >= thresh95[2]], 0, 0),
      col = palette[1],
      lwd = 2,
    )
    polygon(
      x = c(min(x), x[x <= thresh95[1]], thresh95[1]),
      y = c(0, y[x <= thresh95[1]], 0),
      col = palette[3],
      lwd = 2,
    )

    plot(h,
      freq = FALSE, add = TRUE, col = set_alpha("#aaaaaa", 0.5),
      main = "Difference in means",
      xlab = "Treatment - Control"
    )
    abline(v = input$delta, col = palette[1], lwd = 3)
  })
}

shinyApp(ui, server)
}
