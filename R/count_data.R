#' Count app
#' @description
#' A shiny app to explore distributions for count data,
#' with options for the Poisson and Negative binomial distributions,
#' with or without zero inflation.
#'
#' @import shiny
#' @import bslib
#' @importFrom graphics barplot
#' @importFrom stats dpois dnbinom
#' @examples
#' \dontrun{
#' count_app()
#' }
#'
#' @export

count_app <- function() {
  ui <- page_sidebar(

    # Application title
    title = h1("Distributions for count data"),

    # Sidebar with a slider input for number of bins
    sidebar = sidebar(
      accordion(
        accordion_panel(
          title = "Distribution",
          p("Two distributions are commonly used for count data."),
          radioButtons("dist", "Distribution",
            choices = c("Poisson", "Negative Binomial")
          ),
          sliderInput("mean",
            "Mean",
            min = 0,
            max = 10,
            round = FALSE,
            value = 1.5,
            step = 0.5
          ),
          uiOutput("negbin"),
        ),
        accordion_panel(
          title = "Zero Inflation",
          p("A dataset with more zeros than expected from a Poisson/negative
            binomial distribution is zero inflated."),
          sliderInput("zero", "Proportion excess zeros",
            min = 0, max = 1,
            value = 0
          ),
          p("In the plot, excess zeros are shown in red.")
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
    output$negbin <- renderUI({
      if (input$dist == "Negative Binomial") {
        freezeReactiveValue(input, "variance")
        list(
          sliderInput("variance", "Variance",
            min = input$mean,
            max = 20, value = 10
          ),
          p("With the negative binomial distribution, the variance can change
            independently of the mean, giving the distribution more
            flexibility.")
        )
      } else {
        p("With the Poisson distribution, the mean is equal to the variance.")
      }
    })

    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      axis_max <- 25
      x <- 0:axis_max
      if (input$dist == "Poisson") {
        y <- dpois(x, lambda = input$mean)
      } else {
        mu <- input$mean
        v <- input$variance + 0.0001


        size <- mu^2 / (v - mu)

        prob <- mu / v
        y <- dnbinom(x, prob = prob, size = size)
      }

      # zero inflation
      y <- y * (1 - input$zero)

      par(par_list)
      plot(x, y,
        type = "n", ylim = c(0, max(max(y), y[x == 0] + (input$zero))),
        xlab = "Count",
        ylab = "Density"
      )
      segments(x, 0, x, y, lwd = 10, lend = 1)
      segments(0, y[x == 0], 0, y[x == 0] + (input$zero),
        col = set_alpha(palette[1], 0.7), lwd = 10, lend = 1
      )
    })
  }
  # Run the application
  shinyApp(ui = ui, server = server)
}
