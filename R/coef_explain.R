#' Coef-explain app
#' @import shiny
#' @import bslib
#' @importFrom graphics stripchart points
#' @importFrom stats lm coef predict as.formula rnorm
#' @export

coef_explain_app <- function() {
  ui <- page_sidebar(
    sidebar = sidebar(
      checkboxGroupInput("pred", "Predictor", c("Food", "Temperature", "Interaction"))
    ),
    card(
      card_header("Model formula"),
      textOutput("formula")
    ),
    card(
      card_header("Coefficients"),
      tableOutput("table")
    ),
    card(
      plotOutput("plot")
    )
  )



  server <- function(input, output, session) {
    data <- reactive({
      b0 <- 5
      b1 <- 3
      b2 <- 2
      b3 <- 2
      food <- rep(c("A", "B"), each = 24) |> factor()
      temperature <- rep(c("low", "high"), times = 24) |> factor(levels = c("low", "high"))
      response <- b0 + b1 * (food == "B") + b2 * (temperature == "high") + b3 * (food == "B" & temperature == "high") + rnorm(48)
      data.frame(food = food, temperature = temperature, response = response)
    })
    form <- "response ~"
    form2 <- reactive({
      if ("Interaction" %in% input$pred) {
        paste(form, "food * temperature")
      } else if ("Food" %in% input$pred & "Temperature" %in% input$pred) {
        paste(form, "food + temperature")
      } else if ("Food" %in% input$pred) {
        paste(form, "food")
      } else if ("Temperature" %in% input$pred) {
        paste(form, "temperature")
      } else {
        paste(form, "1")
      }
    })

    model <- reactive({
      lm(form2(), data = data())
    })
    table <- reactive({
      tab <- data.frame(
        Beta = paste0("\u03B2", seq_along(coef(model()))),
        Coefficents = names(coef(model())),
        Estimates = coef(model())
      )
      tab
    })

    output$formula <- renderText(form2())
    output$table <- renderTable(table())
    output$plot <- renderPlot({
      set.seed(1)
      f <- as.formula(form2())
      fc <- as.character(f)
      if (fc[3] == "1") {
        stripchart(data()$response, method = "jitter", jitter = 0.1, vertical = TRUE, pch = 1)
      } else {
        stripchart(f, data = data(), method = "jitter", jitter = 0.1, vertical = TRUE, pch = 1)
      }

      cols <- c("food", "temperature")[c(grepl("food", fc[3]), grepl("temperature", fc[3]))]
      if (length(cols) == 0) {
        cols <- "food"
      }
      pred <- predict(model(), newdata = unique(data()[, cols, drop = FALSE]))
      points(seq_along(pred), pred, col = "#832424", pch = 16, cex = 4)
    })
  }

  shinyApp(ui, server)
}
