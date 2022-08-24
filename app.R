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
         CdomCalcConOut = as.numeric(CdomCalcConOut),
         BarometerPressOut = as.numeric(BarometerPressOut),
         Batt_volt_Min = as.numeric(Batt_volt_Min)) %>%
  select(-c(RECORD, CdomRawVoltOut, CdomVoltOut)) %>%
  pivot_longer(cols= -c(TIMESTAMP), names_to = "Variable", values_to = "Measurement")

NVE_14$Variable <- factor(NVE_14$Variable,
                                  levels = c("PTemp", "OptodeSatOut", "OptodeConOut", "OptodeTempOut",
                                             "PAR_Den_Out_Avg", "CdomCalcConOut", "pCO2_out", "BarometerPressOut", "Batt_volt_Min"),
                                  labels = c(expression("Air temperature"),
                                             expression("Oxygen saturation"),
                                             expression("Oxygen concentration"),
                                             expression("Water temperature"),
                                             expression("PAR Density"),
                                             expression("CDOM"),
                                             expression("pCO2"),
                                             expression("Air pressure"),
                                             expression("Battery voltage")))

# shiny app where user can chose the variable and time period

# ui.R ----
ui <- fluidPage(
  titlePanel("Quantom Live"),
  sidebarLayout(
    sidebarPanel(
      h4("Station 14 River Sensor"),
      # duplicates
      selectInput(
        inputId = "Variable",
        label = "Select variable",
        choices = unique(NVE_14$Variable),
        selected = 'Water temperature' # It is a good idea to select a value
        # visible when you launch the app
      ),
      dateRangeInput(
        "dates",
        label = "Time period",
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

# server.R ----
server <- function(input, output, session) {
  output$plot <- renderPlot({

  y_label <- reactive({
      req(input$Variable)
      if(input$Variable == "Air temperature"){
        y_label <- expression("Air temperature ("~ "C)")
      } else if(input$Variable == "Oxygen saturation"){
        y_label <- expression("Oxygen saturation"~"(%)")
      } else if(input$Variable == "Oxygen concentration"){
        y_label <- expression("Oxygen concentration"~"(µM)")
      } else if(input$Variable == "Water temperature"){
        y_label <-  expression("Water temperature"~("C"))
      } else if(input$Variable == "PAR Density"){
        y_label <- expression("PAR Density"~"(µmol"~s^-1~m2^-1~")")
      } else if(input$Variable == "CDOM"){
        y_label <- bquote('CDOM'~"(ppb QSU)")
      } else if(input$Variable == "pCO2"){
        y_label <- bquote('pCO2'~"(ppm)")
      } else if(input$Variable == "Air pressure"){
        y_label <- expression("Barometeric pressure"~"(mbar)")
      } else if(input$Variable == "Battery voltage"){
        y_label <- expression("Battery voltage"~"(V)")
      }})


    NVE_14 %>%
      filter(TIMESTAMP >= input$dates[1], TIMESTAMP <= input$dates[2], Variable == input$Variable) %>%  # Use inputs to filter the data
      ggplot(aes(x = TIMESTAMP, y = Measurement)) +
      geom_point(size=0.6) + theme_classic() + xlab(NULL) +
      theme(text = element_text(size = 16)) +
      labs(x = "Date", y = y_label())

  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
