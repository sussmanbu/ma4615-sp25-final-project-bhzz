Dashboard#1: Race Representation in Chicago: Crime Victims vs Population by year 2021, 2022, and 2023

```{shinylive-r setup}
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

violence_data <- read_csv("https://sussmanbu.github.io/ma4615-sp25-final-project-bhzz/scripts/Violence_clean_missing.csv", show_col_types = FALSE)

```

```{shinylive-r ui}
# ==== UI ====
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


```


```{shinylive-r server}
# ==== Server ====
server <- function(input, output) {
  
  output$race_plot <- renderPlot({
    # ==== 1. Load census data ====
    census_raw <- readRDS(paste0("https://sussmanbu.github.io/ma4615-sp25-final-project-bhzz/scripts/chicago_census_", input$year, ".rds"))
    
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

```

```{shinylive-r run}
# ==== Run App ====
shinyApp(ui = ui, server = server)

````

Dashboard#2: 

```{shinylive-r ui2}

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

```

```{shinylive-r server2}

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

```


```{shinylive-r run2}
# ==== Run App ====
shinyApp(ui2, server2)

```


