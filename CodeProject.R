library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)

# Load the data
emissions <- read_excel("C:/Users/mcnei/OneDrive/Documents/Carbon Project/Carbon Emissions(1970-2015).xlsx")
temperature <- read_excel("C:/Users/mcnei/OneDrive/Documents/Carbon Project/Yearly Temp.xlsx")
df1 <- emissions
df2 <- temperature

df1 <- subset(df1, Record == "PBA_GgCO2")

# Create an example data frame with columns 1970 to 2015
set.seed(123) # For reproducibility
years <- 1970:2015
data <- as.data.frame(matrix(sample(1:100, length(years) * 190, replace = TRUE), nrow = 190, ncol = length(years)))
colnames(data) <- as.character(years)


# Sum the columns 1970 to 2015
sums <- colSums(df1[, as.character(1970:2015)])

# Add the sums as a new row at the end of the data frame
df1[nrow(df1) + 1, ] <- sums

# Optionally, you can rename this row to identify it
rownames(data)[nrow(data)] <- "Sum"


# Merge the data on Year
df <- merge(df1, df2, by = "Year")

# Convert necessary columns to numeric
df$Ca.Total <- as.numeric(df$Ca.Total)
df$Average_Global_Temp <- as.numeric(df$Average_Global_Temp)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Carbon Emissions and Temperature Analysis"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(title = "Ca Total by Year", status = "primary", solidHeader = TRUE, plotOutput("ca_total_by_year")),
      box(title = "Average Global Temperature by Year", status = "primary", solidHeader = TRUE, plotOutput("avg_temp_by_year")),
      box(title = "Correlation Plot", status = "primary", solidHeader = TRUE, plotOutput("correlation_plot"))
    ),
    fluidRow(
      box(title = "Correlation Coefficient", status = "primary", solidHeader = TRUE, verbatimTextOutput("correlation_coefficient"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$ca_total_by_year <- renderPlot({
    ggplot(df, aes(x = Year, y = Ca.Total)) +
      geom_bar(stat = "identity", position = "dodge", fill = "#ffd700") +
      labs(title = "Ca Total by Year", x = "Year", y = "Ca Total") +
      scale_y_continuous(breaks = seq(0, max(df$Ca.Total, na.rm = TRUE), by = 0.5))
  })
  
  output$avg_temp_by_year <- renderPlot({
    ggplot(df, aes(x = Year, y = Average_Global_Temp)) +
      geom_line(color = "#ff0800") +
      labs(title = "Average Global Temperature by Year", x = "Year", y = "Average Global Temperature") +
      scale_y_continuous(breaks = seq(min(df$Average_Global_Temp, na.rm = TRUE), max(df$Average_Global_Temp, na.rm = TRUE), by = 0.5))
  })
  
  output$correlation_plot <- renderPlot({
    ggplot(df, aes(x = Ca.Total, y = Average_Global_Temp)) +
      geom_point(color = "#107c10") +
      geom_smooth(method = "lm", se = FALSE, color = "#65350f") +
      labs(title = "Correlation between Ca Total and Average Global Temperature", x = "Ca Total", y = "Average Global Temperature")
  })
  
  output$correlation_coefficient <- renderPrint({
    correlation <- cor(df$Ca.Total, df$Average_Global_Temp, use = "complete.obs")
    paste("Correlation Coefficient: ", round(correlation, 2))
  })
}

# Run the application
shinyApp(ui = ui, server = server)