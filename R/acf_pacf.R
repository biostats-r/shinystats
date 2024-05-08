#' Acf app
#' @import shiny
#' @import bslib
#' @importFrom graphics par
#' @importFrom stats arima.sim anova acf pacf
#' @export
#'
acf_pacf_app <- function() {
  ui <- page_sidebar(
    title = "Autoregressive process",

    sidebar = sidebar(
      h5(withMathJax("$$y_t = \\sum_{i = 1}^{p}\\rho_{i} y_{t-i} + \\epsilon_t$$")),
      h5(withMathJax("$$y_t = \\rho_{1} y_{t-1} + \\rho_{2} y_{t-2} + \\cdots +\\epsilon_t$$")),

      radioButtons("order", "Autoregressive order", choices = 1:2, selected = 1),
      uiOutput("ar_coef_sliders"),
    ),
    # Show a plot of the generated distribution

    card(
      plotOutput("arPlot")
    ),
    card(
      card_header("ACF"),
      plotOutput("acfPlot")
    ),
    card(
      card_header("pACF"),
      plotOutput("pacfPlot")
    ),



  )

  server <- function(input, output, session) {
    ar_names <- reactive(paste0("ar", seq_len(input$order)))
    # make ui sliders
    output$ar_coef_sliders <- renderUI({
      # freeze reactives
      ar_names() |> map(\(n)freezeReactiveValue(input, n))


      ar1_start <- 0.5
      if(length(ar_names()) == 1) {
        list(
          sliderInput(inputId = ar_names()[1], label = "Autoregression coefficient 1", value = ar1_start, min = -0.99, max = 0.99, step = 0.01)
          )

      } else {
        list(
          sliderInput(inputId = ar_names()[1], label = "Autoregression coefficient 1", value = ar1_start, min = -1.99, max = 1.99, step = 0.01),
          sliderInput(inputId = ar_names()[2], label = "Autoregression coefficient 2", value = ar1_start, min = -0.99, max = 1 - abs(ar1_start) - 0.01, step = 0.01)
        )

      }

    })

  # update slider so only stationary coef possible
    observeEvent(input$ar1, {
      if(length(ar_names()) > 1){
        updateSliderInput(inputId = "ar2",
                          max = 1 - abs(input$ar1) - 0.01)
      }
    })

    # data
    n <- 100
    x <- 1:n
    ar_coefs <- reactive(vapply(ar_names(), \(x) input[[x]], FUN.VALUE = 1))

    y <- reactive({
      if (all(ar_coefs() == 0)) {
        y <- rnorm(n = n)
      } else {
        y <- arima.sim(list(ar = c(ar_coefs())), n = n)
      }
      y
    })

 # plots
    output$arPlot <- renderPlot({
      par(mar = c(2.2, 2.2, 0.5, 1), cex = 1.5, tcl = -0.1, mgp = c(1.2, 0.2, 0))
      plot(x, y(), type = "l", xlab = "Time", ylab = "Value")
    })

    output$acfPlot <- renderPlot({
      par(mar = c(2.2, 2.2, 0.5, 1), cex = 1.5, tcl = -0.1, mgp = c(1.2, 0.2, 0))
      acf(y())
    })

    output$pacfPlot <- renderPlot({
      par(mar = c(2.2, 2.2, 0.5, 1), cex = 1.5, tcl = -0.1, mgp = c(1.2, 0.2, 0))
      pacf(y())
    })

  }

  shinyApp(ui, server)
}
