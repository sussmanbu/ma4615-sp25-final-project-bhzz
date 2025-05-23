library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

violence_data <- read_csv("Violence_clean_missing.csv", show_col_types = FALSE)

ui <- fluidPage(
  titlePanel("Race Representation in Chicago: Crime Victims vs Population"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = c(2021, 2022, 2023), selected = 2021)
    ),
    mainPanel(
      plotOutput("race_plot"),
      br(),
      textOutput("note")
    )
  )
)

# ==== Server ====
server <- function(input, output) {
  
  output$race_plot <- renderPlot({
    # ==== 1. Load census data ====
    census_raw <- readRDS(paste0("chicago_census_", input$year, ".rds"))
    
    census_df <- census_raw |>
      filter(NAME == "Chicago city, Illinois") |>
      select(variable, estimate) |>
      pivot_wider(names_from = variable, values_from = estimate) |>
      mutate(
        WHI = white / total * 100,
        BLK = black / total * 100,
        HIS = hispanic / total * 100,
        API = asian / total * 100,
        I   = native / total * 100
      ) |>
      select(WHI, BLK, HIS, API, I) |>
      pivot_longer(cols = everything(), names_to = "RACE", values_to = "Proportion") |>
      mutate(Source = "Census")
    
    # ==== 2. Create crime victim data ====
    victim_df <- violence_data |>
      filter(Year == input$year) |>
      filter(!(RACE %in% c("UNKNOWN", "S"))) |>
      mutate(RACE = ifelse(RACE %in% c("WBH", "WWH"), "HIS", RACE)) |>
      group_by(RACE) |>
      summarise(Count = n(), .groups = "drop") |>
      mutate(Proportion = Count / sum(Count) * 100,
             Source = "Crime Victims") |>
      select(RACE, Proportion, Source)
    
    
    # ==== 3. Combine and plot ====
    combined <- bind_rows(census_df, victim_df)
    
    ggplot(combined, aes(x = RACE, y = Proportion, fill = Source)) +
      geom_col(position = "dodge") +
      labs(title = paste("Race Representation in Chicago (", input$year, ")", sep = ""),
           x = "Race", y = "Proportion (%)") +
      theme_minimal()
  })
  
  # Add a note
  output$note <- renderText({
    paste0("Note: Population data for ", input$year,
           " comes from the U.S. Census ACS 1-Year Estimates. ",
           "Crime data reflects victim demographics recorded by CPD in the same year.")
  })
}


# ==== Run App ====
shinyApp(ui = ui, server = server)