#' Proportion app
#' @import shiny
#' @import bslib
#' @importFrom graphics barplot
#' @importFrom stats dbinom
#' @export

prop_app <- function() {
  ui <- page_sidebar(

    # Application title
    title = h1("Distribution for proportion data"),

    # Sidebar with a slider input for number of bins
    sidebar = sidebar(
      accordion(
        accordion_panel(
          title = "Binomial Distribution",
          p("The binomial distribution is used when we have success/failure. You can use it for"),
          tags$ol(
            tags$li("Presence/absence data"),
            tags$li("Success/failure data (8 out of 10 seeds germinated)"),
            tags$li("Proportion data (80% of seeds germinates)")
          )

        ),
        sliderInput("ntrials",
                    "Number of trials",
                    min = 1,
                    max = 10,
                    round = TRUE,
                    value = 1,
                    step = 1
        ),
        sliderInput("prob",
                    "Probability of Success",
                    min = 0,
                    max = 1,
                    value = 0.5
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        plotOutput("distPlot")
      )
    )
  )




  # Define server logic required to draw a histogram
  server <- function(input, output) {

    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      axis_max <- input$ntrials
      x <- 0:axis_max
      y <- dbinom(x, size = input$ntrials, prob = input$prob)


      par(cex = 1.5, mar = c(3, 3, 1, 1), tcl = -0.1, mgp = c(2, 0.2, 0))
      plot(x, y,
        type = "n", ylim = c(0, max(max(y), y[x==0] + (input$zero))),
        xlab = "Number of successes",
        ylab = "Density"
      )
      segments(x, 0, x, y, lwd = 10, lend = 1)
    })
  }
  # Run the application
  shinyApp(ui = ui, server = server)
}
