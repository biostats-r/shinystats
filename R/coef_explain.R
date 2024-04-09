#' Coef-explain app
#' @import shiny
#' @import bslib
#' @importFrom graphics stripchart points arrows
#' @importFrom stats lm coef predict as.formula rnorm
#' @export

coef_explain_app <- function() {
  ui <- page_sidebar(
    sidebar = sidebar(
      checkboxGroupInput("pred", "Predictor", choiceValues = c("Food", "Temperature", "Interaction"),
                         choiceNames = c("Food (A vs. B)", "Temperature (Low vs. High)", "Interaction"))
    ),
    card(
      card_header("Model formula"),
      textOutput("formula")
    ),
    card(
      card_header("Coefficients"),
      tableOutput("coef_table")
    ),
    card(
      plotOutput("plot")
    ),
    tags$head(tags$style("#coef_table td{
                     position:relative;
                     };

                     ")),


  )



  server <- function(input, output, session) {
    data <- reactive({
      b0 <- 5
      b1 <- 3
      b2 <- 2
      b3 <- 2
      food <- rep(c("A", "B"), times = 24) |> factor()
      temperature <- rep(c("Low", "High"), each = 24) |> factor(levels = c("Low", "High"))
      response <- b0 + b1 * (food == "B") + b2 * (temperature == "High") + b3 * (food == "B" & temperature == "High") + rnorm(48)
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

    observe({
      if ("Interaction" %in% input$pred){
        updateCheckboxGroupInput(session, "pred",
                                 selected = c("Food", "Temperature", "Interaction")
        )
     }
    })

    model <- reactive({
      lm(form2(), data = data())
    })

    coefs <- reactive({
      coef(model())
    })

    coef_colours <- reactive({
      c(`(Intercept)` = "skyblue", foodB = "red", temperatureHigh = "green", `foodB:temperatureHigh` = "orange")[names(coefs())]
    })

    coef_table <- reactive({
      c1 <- '<div style="width: 100%; height: 100%; z-index: 0; background-color: '
      c2 <- '; position:absolute; top: 0; left: 0; padding:5px;">\n<span>'
      c3 <- '</span></div>'

      tab <- data.frame(
        Beta = paste0(c1, coef_colours(), c2, "\u03B2", seq_along(coefs()), c3),
        Coefficent = names(coefs()),
        Estimate = coefs()
      )
      tab
    })


    output$formula <- renderText(form2())
    output$coef_table <- renderTable(coef_table(), sanitize.text.function = function(x) x)
    output$plot <- renderPlot({
      set.seed(1)
      f <- as.formula(form2())
      fc <- as.character(f)
      ylim <- c(0, max(data()$response))
      if (fc[3] == "1") {
        stripchart(data()$response, method = "jitter", jitter = 0.1, vertical = TRUE, pch = 1, ylim = ylim)
      } else {
        stripchart(f, data = data(), method = "jitter", jitter = 0.1, vertical = TRUE, pch = 1, ylim = ylim)
      }

      cols <- c("food", "temperature")[c(grepl("food", fc[3]), grepl("temperature", fc[3]))]
      if (length(cols) == 0) {
        cols <- "food"
      }
      pred <- predict(model(), newdata = unique(data()[, cols, drop = FALSE]))
      points(seq_along(pred), pred, col = "#832424", pch = 16, cex = 4)
#browser()
      # add arrows for betas
      xs <- seq_along(pred) - 0.2
      # b0
      arrows(xs, rep(0, length(pred)), xs, rep(coefs()[1], length(pred)), col = coef_colours()[1], lwd = 2, length = 0.1)

      # food main effect
      pos <- 2 - 0.2 # x position
      if ("foodB" %in% names(coefs()) ){
        arrows(pos, coefs()[1], pos, coefs()[1] + coefs()["foodB"], col = coef_colours()["foodB"], lwd = 2, length = 0.1)

        pos <- pos + 1 # move to next position

      }

      if ("temperatureHigh" %in% names(coefs()) ){
        arrows(pos, coefs()[1], pos, coefs()[1] + coefs()["temperatureHigh"], col = coef_colours()["temperatureHigh"], lwd = 2, length = 0.1)
        pos <- pos + 1

      }

      if (all(c("foodB", "temperatureHigh") %in% names(coefs()))){
        arrows(pos, coefs()[1], pos, coefs()[1] + coefs()["foodB"], col = coef_colours()["foodB"], lwd = 2, length = 0.1)

        arrows(pos, coefs()[1] + coefs()["foodB"], pos, coefs()[1] + coefs()["foodB"] + coefs()["temperatureHigh"], col = coef_colours()["temperatureHigh"], lwd = 2, length = 0.1)

        if ("foodB:temperatureHigh" %in% names(coefs())) { # interaction
          arrows(pos, coefs()[1] + coefs()["foodB"] + coefs()["temperatureHigh"], pos, coefs()[1] + coefs()["foodB"] + coefs()["temperatureHigh"] + coefs()["temperatureHigh"], col = coef_colours()["foodB:temperatureHigh"], lwd = 2, length = 0.1)
        }
      }

    })
  }

  shinyApp(ui, server)
}
