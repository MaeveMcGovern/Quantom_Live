# Packages ----
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)

# Loading data ----
NVE_14 <- read_csv("Data/QUANTOM_1000X_sn6221_Table15min.dat", skip =1) %>% filter(!row_number() %in% c(1L, 2L)) %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  mutate(RECORD = as.numeric(RECORD),
         OptodeSatOut= as.numeric(OptodeSatOut),
         OptodeConOut = as.numeric(OptodeConOut),
         OptodeTempOut = as.numeric(OptodeTempOut),
         BarometerPressOut = as.numeric(BarometerPressOut),
         PAR_Den_Out_Avg = as.numeric(PAR_Den_Out_Avg),
         CdomRawVoltOut = as.numeric(CdomRawVoltOut),
         CdomVoltOut = as.numeric(CdomVoltOut),
         CdomCalcConOut = as.numeric(CdomCalcConOut)) %>%
  select(-c(RECORD, BarometerPressOut, Batt_volt_Min)) %>%
  pivot_longer(cols= -c(TIMESTAMP), names_to = "Variable", values_to = "Measurement")

# shiny app where user can chose the variable and time period
# ui.R ----

# You have to Adequate your data: You have to create a dete variable
# in order to make the `dateRangeInput` work. You can do that using
# `year`, `month` and `day` variables as follow.

ui <- navbarPage(
  title = "Quantom live",
  tabPanel(
    title = "NVE_14 sensor",
    sidebarPanel(
      h4("Water chemistry"),
      # duplicates
      selectInput(
        "Variable",
        label = "Select Variable",
        choices = unique(NVE_14$Variable),
        selected = 'CdomRawVoltOut' # It is a good idea to select a value
        # visible when you launch the app
      ),
      dateRangeInput(
        "dates",
        label = "Time_period",
        start = min(NVE_14$TIMESTAMP),
        end = max(NVE_14$TIMESTAMP)
      ),
      tags$img(src = "niva_logo.jpeg"),
    ),

    mainPanel(
      plotOutput("plot")
    )

  )

)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    NVE_14 %>%
      # Use your inputs to filter the data
      filter(TIMESTAMP >= input$dates[1], TIMESTAMP <= input$dates[2], Variable == input$Variable) %>%
      ggplot(aes(x = TIMESTAMP, y = Measurement)) +
      geom_point(size=0.6) + theme_classic() + xlab(NULL) +
      theme(axis.text = element_text(size =12))

  })

}


# Run the app ----
shinyApp(ui = ui, server = server)
