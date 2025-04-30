library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

violence_data <- read_csv("Violence_clean_missing.csv", show_col_types = FALSE)
# ========== UI ==========
ui2 <- fluidPage(
  titlePanel("Victim Demographics by Crime Type"),
  sidebarLayout(
    sidebarPanel(
      selectInput("crime_type", "Select Crime Type:",
                  choices = unique(violence_data$PRIMARY_TYPE),
                  selected = "BATTERY"),
      checkboxGroupInput("sex", "Select Gender:",
                         choices = unique(violence_data$SEX),
                         selected = c("M", "F")),
      checkboxGroupInput("race", "Select Race:",
                         choices = unique(violence_data$RACE),
                         selected = c("BLK", "WHI", "HIS", "API"))
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


# ========== SERVER ==========
server2 <- function(input, output) {
  output$distPlot <- renderPlot({
    filtered <- violence_data |>
      filter(PRIMARY_TYPE == input$crime_type,
             SEX %in% input$sex,
             RACE %in% input$race)
    
    ggplot(filtered, aes(x = SEX, fill = RACE)) +
      geom_bar(position = "stack") +
      labs(
        title = paste("Victim Gender-Race Distribution for", input$crime_type),
        x = "Gender", y = "Number of Victims", fill = "Race"
      ) +
      theme_minimal()
  })
}

# ==== Run App ====
shinyApp(ui2, server2)