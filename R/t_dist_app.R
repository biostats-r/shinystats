#' t distribution
#' @description
#' A shiny app to visualise how t-distribution changes with the number of
#' degrees of freedom.
#'
#' @import shiny
#' @import bslib
#' @importFrom graphics par polygon lines text
#' @importFrom stats dt qt
#' @examples
#' \dontrun{
#' f_dist_app()
#' }

#' @export

t_dist_app <- function(){
  # Define UI for application
  ui <- page_sidebar(
      # Application title
      title = "t distribution",

      # Sidebar with a slider input for number of bins
      sidebar = sidebar(
            sliderInput("df",
                          "Number of degrees of freedom:",
                          min = 1,
                          max = 50,
                          round = TRUE,
                          value = 5),
              radioButtons("alpha",
                          "\u03b1:",
                          c("p = 0.05" = "0.05", "p = 0.01" = "0.01")
              ),
              radioButtons("type",
                           "Alternative:",
                           c("two-sided", "less than", "greater than")
              ),
          ),

          # Show a plot of the generated distribution
          card(
             plotOutput("distPlot")
          )

  )
  # Run the application
  shinyApp(ui = ui, server = t_test_server)
}
# Define server logic required to draw a histogram
t_test_server <- function(input, output) {

    output$distPlot <- renderPlot({
        # calculate t-distribution with input$df degrees of freedom
        xmax <- min(qt(p = 0.999, df = input$df), 100) # limit distribution
        x    <- seq(-ceiling(xmax), ceiling(xmax), length.out = 600)
        y <- dt(x, df =  input$df)

        plot(x, y, type = "n")
        polygon(c(x[1], x, x[200]), c(0, y, 0), col = "grey90")

        # critical regions
        if (input$type != "two-sided") {
          xthresh <- qt(p = 1 - as.numeric(input$alpha), df = input$df)
          x2 <- seq(xthresh, ceiling(xmax), length.out = 100)
          if (input$type == "less than") {
            xthresh <- -xthresh
            x2 <- -x2
          }

          y2 <- dt(x2, df = input$df)

          polygon(c(x2[1], x2, x2[100]), c(0, y2, 0), col = palette[1], border = palette[1])
          text(x = xthresh,
               y = y2[1] + 0.05 * (max(y)- y2[1]),
               labels = bquote(t[.(input$df)*';'~.(input$alpha)]==.(round(xthresh, 2))),
               pos = ifelse(input$type == "less than", 2, 4)
        )

         } else {
           xthresh <- qt(p = 1 - as.numeric(input$alpha)/2, df = input$df)
           x2 <- seq(xthresh, ceiling(xmax), length.out = 100)

           y2 <- dt(x2, df = input$df)

           polygon(c(x2[1], x2, x2[100]), c(0, y2, 0), col = palette[1], border = palette[1])
           polygon(-c(x2[1], x2, x2[100]), c(0, y2, 0), col = palette[1], border = palette[1])


           text(x = -xthresh,
                y = y2[1] + 0.05 * (max(y)- y2[1]),
                labels = bquote(t[.(input$df)*';'~.(input$alpha)]==.(round(-xthresh, 2))),
                pos = 2
           )
           text(x = xthresh,
                y = y2[1] + 0.05 * (max(y)- y2[1]),
                labels = bquote(t[.(input$df)*';'~.(input$alpha)]==.(round(xthresh, 2))),
                pos = 4
           )
        #   base +
        #     annotate(geom = "text",
        #              x = xthresh,
        #              y = y2[1] + 0.05 * (max(y)- y2[1]),
        #              label = glue::glue("t[{input$df}*';'~{input$alpha}]=={round(xthresh, 2)}"),
        #              hjust = 0, vjust = 0, parse = TRUE, size = 5) +
        #     annotate(geom = "text",
        #              x = - xthresh,
        #              y = y2[1] + 0.05 * (max(y)- y2[1]),
        #              label = glue::glue("t[{input$df}*';'~{input$alpha}]=={round(-xthresh, 2)}"),
        #              hjust = 1, vjust = 0, parse = TRUE, size = 5)
        #
         }
    })

}


