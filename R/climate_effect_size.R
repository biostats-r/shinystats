#' Visualising effect sizes through the weather
#' @description A shiny app to help visualise effect sizes.
#' The app lets you choose which months to compare in a city, and shows a boxplot
#' of the mean monthly temperatures, the effect size (Cohen's D),
#' and the required sample size to detect an effect this large with
#' the required power at the requested significance level.
#' @importFrom graphics boxplot par
#' @importFrom stats power.t.test
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' climate_effect_app()
#' }
#'
#' @export
#'

# TODO
# acquire climate data for selected cities
# compile and clean - want selected years - cols = city, month, temperature
# city list & text from file - populate dynamically


climate_effect_app <- function() {
  ui <- page_sidebar(
    title = "Visualising effect sizes through the weather",
    sidebar = sidebar(
      uiOutput("cityUI"),
      p(textOutput("city_weather")),
      shinyWidgets::sliderTextInput(
        inputId = "month1",
        label = "Month 1",
        choices = month.abb,
        selected = "Jan"
      ),
      shinyWidgets::sliderTextInput(
        inputId = "month2",
        label = "Month 2",
        choices = month.abb,
        selected = "Feb"
      ),
      sliderInput(inputId = "power", label = "Required power",
                  min = 0, max = 1, step = 0.1, value = 0.8),
      radioButtons("alpha", "Significance level, alpha",
                   choices = c(0.05, 0.01, 0.001), selected = 0.05)
    ),
    card(
      card_header("Boxplots of mean monthly temperatures"),
      card_body(plotOutput("boxplot"))
    ),
    layout_columns(
      tooltip(
        value_box(
          title = "Cohen's D",
          value = textOutput("cohens")
        ),
        "If the first month is cooler on average, Cohen's D will be negative, and vice versa."
      ),
      tooltip(
        value_box(
          title = "Required sample size (for each group)",
          value = textOutput("sample_size")
        ),
        "Required sample size (per group) to have 80% power at the selected significance level to reject null hypothesis that two months with this Cohen's d have the same climate."
      ),
    )
  )

  server <- function(input, output) {
    # load climate data (will be preprocessed)
    weather_file <- system.file("extdata/city_climate.csv", package = "shinystats")
    dat <- read.csv(weather_file)
    dat$month <- month.abb[as.integer(substr(dat$date, 0, 2))]

    # load city data
    cities_file <- system.file("extdata/cities.csv", package = "shinystats")
    cities <- read.csv(cities_file)

    output$cityUI <- renderUI({
      freezeReactiveValue(input, "city")
      radioButtons(
        inputId = "city",
        label = "City",
        selected = cities$city[1],
        choices = cities$city
      )
    })

    output$city_weather <- renderText({
      cities[cities$city == input$city, "climate"]
    })

    selected_data <- reactive({
      d <- dat[dat$month %in% c(input$month1, input$month2), ]
      d <- d[d$city == input$city, ]
      if(input$month1 != input$month2) { # make factor if months differ
        d$month <- factor(d$month, levels = c(input$month1, input$month2))
      }
      d
    })

    output$boxplot <- renderPlot({
      par(par_list)
      if(input$month1 == input$month2) {
        double_month <- list(
          temperature = selected_data()$temperature,
          temperature = selected_data()$temperature
        )
        names(double_month) <- paste(input$month1, c("", "again"))
        boxplot(double_month, ylab = "Mean temperature", main = "Same month selected")
      }
      else {
        boxplot(temperature ~ month, selected_data(), ylab = "Mean temperature")
      }

    })

    cohens <- reactive({
      if(input$month1 == input$month2) {
        list(Cohens_d = 0)
      }
      else{
        effectsize::cohens_d(temperature ~ month, data = selected_data())
      }
    })

    output$cohens <- renderText({
      round(cohens()$Cohens_d, 2)
    })

    output$sample_size <- renderText({
      if(input$month1 == input$month2) {
        return("Infinite sample size")
      }
      ceiling(
        power.t.test(
          #
          delta = 1,
          sd = 1 / abs(cohens()$Cohens_d),
          power = input$power,
          sig.level = as.numeric(input$alpha)
        )$n
      )
    })
  }


  shinyApp(ui, server)
}
