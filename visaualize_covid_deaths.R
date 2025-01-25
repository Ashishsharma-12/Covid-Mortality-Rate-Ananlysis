#-----Section 01: Import Libraries-------------------------------------------

# Load necessary libraries
library(plotly)
library(dplyr)
library(tidyr)

#-----Section 02: Data loading and corrections-------------------------------------------

# set working directory
setwd(dirname(file.choose()))
getwd()

# read in data from csv file
covid_data <- read.csv("COVID-19_deaths.csv", stringsAsFactors = FALSE)
dim(covid_data)
str(covid_data)
head(covid_data)


# Gather the data to create a long format for better visualization
covid_long <- covid_data %>%
  gather(key = "Month", value = "Deaths", c(3:16)) %>%
  mutate(Month = gsub("_", " ", Month)) # Clean month names

dim(covid_long)

# Create a custom order for the months
month_order <- c("March 2020", "April 2020", "May 2020", "June 2020", "July 2020", 
                 "August 2020", "September 2020", "October 2020", "November 2020", 
                 "December 2020", "January 2021", "February 2021", "March 2021", 
                 "April 2021")

# Convert the 'Month' column to a factor with the custom order
covid_long$Month <- factor(covid_long$Month, levels = month_order)

# Plot using Plotly
fig <- covid_long %>%
  plot_ly(
    x = ~Month,
    y = ~Deaths,
    color = ~LA_name,  # Color by Local Authority name
    type = 'scatter',
    mode = 'lines+markers',
    text = ~LA_name,  # Hover text
    hoverinfo = 'text+y'
  ) %>%
  layout(
    title = 'COVID-19 Deaths Trend by Local Authority',
    xaxis = list(title = 'Month'),
    yaxis = list(title = 'Number of Deaths'),
    showlegend = TRUE
  )

fig <- fig %>% layout(
  colorway = RColorBrewer::brewer.pal(12, "Paired")
)

fig

# Save the interactive plot as an HTML file
htmlwidgets::saveWidget(fig, "D:/University of East London/Data Science/Modules/DS7006 2425 (T1) -  Quantitative Data Analysis (QDA) (OC)/general resources/covid_deaths_trend.html")
