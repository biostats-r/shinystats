#' Penguin Coef-explain app
#' @import shiny
#' @import bslib
#' @importFrom graphics stripchart points arrows
#' @importFrom stats lm coef predict as.formula rnorm
#' @export

penguin_coef_explain_app <- function() {
  ui <- page_sidebar(
    sidebar = sidebar(
      radioButtons("response", "Response",
                         choiceNames = c("body_mass_g", "bill length_mm", "bill depth mm", "flipper length mm"),
                         choiceValues = c(
                           "body_mass",
                           "bill_len",
                           "bill_dep",
                           "flipper_len"
                         ),
                   selected = "body_mass"
      ),
      checkboxGroupInput("pred", "Predictor",
                         choiceValues = c("species", "sex", "interaction"),
                         choiceNames = c(
                           "species",
                           "sex",
                           "interaction"
                         )
      )


    ),
    card(
      textOutput("formula"),
      fill = FALSE
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
    data <-  penguins |> na.omit()
    data <- data[order(data$species), ]
    data <- data[order(data$sex), ]



    form2 <- reactive({
      form <- paste(input$response,"~")

      if ("interaction" %in% input$pred) {
        paste(form, "species * sex")
      } else if ("species" %in% input$pred & "sex" %in% input$pred) {
        paste(form, "species + sex")
      } else if ("species" %in% input$pred) {
        paste(form, "species")
      } else if ("sex" %in% input$pred) {
        paste(form, "sex")
      } else {
        paste(form, "1")
      }
    })

    observe({
      if ("interaction" %in% input$pred) {
        updateCheckboxGroupInput(session, "pred",
                                 selected = c("species", "sex", "interaction")
        )
      }
    })

    model <- reactive({
      lm(form2(), data = data)
    })

    coefs <- reactive({
      coef(model())
    })

    coef_colours <- reactive({
      c(
        `(Intercept)` = "skyblue",
        speciesChinstrap = "pink",
        speciesGentoo = "red",


        sexmale = "green",
        `speciesChinstrap:sexmale` = "brown",
        `speciesGentoo:sexmale` = "orange"

      )[names(coefs())]
    })

    coef_table <- reactive({
      c1 <- '<div style="width: 100%; height: 100%; z-index: 0; background-color: '
      c2 <- '; position:absolute; top: 0; left: 0; padding:5px;">\n<span>'
      c3 <- "</span></div>"

      tab <- data.frame(
        Beta = paste0(c1, coef_colours(), c2, "\u03B2", seq_along(coefs()), c3),
        Coefficent = names(coefs()),
        Estimate = coefs()
      )
      tab
    })


    output$formula <- renderText(
      paste0("lm(", form2(), ", data = penguins)")
      )
    output$coef_table <- renderTable(
      coef_table(),
      sanitize.text.function = function(x) x
    )
    output$plot <- renderPlot({
      par(par_list)
      set.seed(1)
      f <- as.formula(form2())
      fc <- as.character(f)
      ylim <- c(0, max(data[, input$response]))
      if (fc[3] == "1") {
        stripchart(data[,input$response],
                   method = "jitter", jitter = 0.1,
                   vertical = TRUE, pch = 1, ylim = ylim,
                   ylab = fc[2]
        )
      } else {
        spp <- levels(data$species)
        sex <- levels(data$sex)
        group_names <-
           if ("species" %in% input$pred & "sex" %in% input$pred) {
          paste("\n", rep(spp, times = length(sex)), rep(sex, each = length(spp)), sep = "\n")
          } else if ("species" %in% input$pred) {
            spp
          } else if ("sex" %in% input$pred) {
            sex
          } else {
            1
          }

        stripchart(f,
                   data = data, method = "jitter", jitter = 0.1,
                   vertical = TRUE, pch = 1, ylim = ylim,
                   group.names = group_names
        )
      }

      cols <- c("species", "sex")[c(grepl("species", fc[3]), grepl("sex", fc[3]))]
      if (length(cols) == 0) {
        cols <- "species"
      }
      pred <- predict(model(), newdata = unique(data[, cols, drop = FALSE]))
      points(seq_along(pred), pred, col = "#832424", pch = 16, cex = 4)

      # add arrows for betas
      lwd <- 3
      xs <- seq_along(pred) - 0.2
      # b0
      arrows(xs, rep(0, length(pred)), xs, rep(coefs()[1], length(pred)),
             col = coef_colours()[1], lwd = lwd, length = 0.1
      )

      # species main effect
      pos <- 2 - 0.2 # x position
      if ("speciesChinstrap" %in% names(coefs())) {
        arrows(pos, coefs()[1], pos, coefs()[1] + coefs()["speciesChinstrap"],
               col = coef_colours()["speciesChinstrap"], lwd = lwd, length = 0.1
        )

        pos <- pos + 1 # move to next position
        arrows(pos, coefs()[1], pos, coefs()[1] + coefs()["speciesGentoo"],
               col = coef_colours()["speciesGentoo"], lwd = lwd, length = 0.1
        )

        pos <- pos + 1 # move to next position
      }

      if ("sexmale" %in% names(coefs())) {
        arrows(pos, coefs()[1], pos, coefs()[1] + coefs()["sexmale"],
               col = coef_colours()["sexmale"], lwd = lwd, length = 0.1
        )
        pos <- pos + 1
      }

      if (all(c("speciesChinstrap", "sexmale") %in% names(coefs()))) {
        #chinstraps
        arrows(pos, coefs()[1], pos, coefs()[1] + coefs()["speciesChinstrap"],
               col = coef_colours()["speciesChinstrap"], lwd = lwd, length = 0.1
        )

        arrows(pos,
               coefs()[1] + coefs()["speciesChinstrap"],
               pos,
               coefs()[1] + coefs()["speciesChinstrap"] + coefs()["sexmale"],
               col = coef_colours()["sexmale"],
               lwd = lwd, length = 0.1
        )

        if ("speciesChinstrap:sexmale" %in% names(coefs())) { # interaction
          arrows(pos,
                 coefs()[1] + coefs()["speciesChinstrap"] + coefs()["sexmale"],
                 pos,
                 coefs()[1] + coefs()["speciesChinstrap"] +
                   coefs()["sexmale"] +
                   coefs()["speciesChinstrap:sexmale"],
                 col = coef_colours()["speciesChinstrap:sexmale"],
                 lwd = lwd, length = 0.1
          )
        }

        # gentoo
        pos <- pos + 1
        arrows(pos, coefs()[1], pos, coefs()[1] + coefs()["speciesGentoo"],
               col = coef_colours()["speciesGentoo"], lwd = lwd, length = 0.1
        )

        arrows(pos,
               coefs()[1] + coefs()["speciesGentoo"],
               pos,
               coefs()[1] + coefs()["speciesGentoo"] + coefs()["sexmale"],
               col = coef_colours()["sexmale"],
               lwd = lwd, length = 0.1
        )

        if ("speciesGentoo:sexmale" %in% names(coefs())) { # interaction
          arrows(pos,
                 coefs()[1] + coefs()["speciesGentoo"] + coefs()["sexmale"],
                 pos,
                 coefs()[1] + coefs()["speciesGentoo"] +
                   coefs()["sexmale"] +
                   coefs()["speciesGentoo:sexmale"],
                 col = coef_colours()["speciesGentoo:sexmale"],
                 lwd = lwd, length = 0.1
          )
        }
      }
    })
  }

  shinyApp(ui, server)
}
