library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

# Wrangle the data :)
df <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
# get rid of continent data. Only want country
mdf <- subset(subset(df, iso_code != ""), iso_code != "OWID_WRL")

# Calculate Summary Values

# Value 1
# average co2_growth_abs for all countries in 2018
df_growth_abs <- mdf %>% 
  filter(year == 2018) %>% 
  select(co2_growth_abs) %>%
  na.omit()
avg_growth <- mean(df_growth_abs$co2_growth_abs, na.rm = T)

# Value 2
# highest co2_per_capita for all countries over time
df_capita <- mdf %>% 
  filter(co2_per_capita == max(co2_per_capita, na.rm = T)) %>% 
  select(year, country, co2_per_capita)
per_capita_country <- df_capita$country
per_capita_co2 <- prettyNum(df_capita$co2_per_capita, big.mark = ",", scientific = FALSE)
per_capita_year <- df_capita$year

# Value 3
# total gas_co2 for all countries in 2018
df_gas <- mdf %>% 
  filter(year == 2018) %>% 
  select(gas_co2) %>% 
  summarise(total = sum(gas_co2, na.rm = T))
total_gas <- prettyNum(df_gas$total, big.mark = ",", scientific = FALSE)

# Value 4
# average of coal_co2 for all countries in 2018
df_avg_coal <- mdf %>% 
  filter(year == 2018) %>% 
  select(coal_co2) %>% 
  summarise(avg = mean(coal_co2, na.rm = T)) %>% 
  pull(avg)
avg_coal <- prettyNum(df_avg_coal, big.mark = ",", scientific = FALSE)

# Value 5
# country with highest cement_co2 (of all time)
max_cement_country <- mdf %>% 
  filter(cement_co2 == max(cement_co2, na.rm = T)) %>% 
  pull(country)

server <- function(input, output) {
  output$greeting <- renderText({
    my_greeting <- "Hello, "
    user_name <- input$name
    if (user_name == "") {
      user_name <- "User"
    }
    message_str <- paste0(my_greeting, user_name, "!")
    message_str 
  })
  
  output$summary_one <- renderText({
    return(paste("1.", "The average for CO2 growth across all countries in 2018 was",
                 prettyNum(avg_growth, big.mark = ",", scientific = FALSE), "million tonnes.", sep = " "))
  })
  output$summary_two <- renderText({
    # return(paste("2.", prettyNum(per_capita, big.mark = ",", scientific = FALSE), sep = " "))
    return(paste("2.", "The country with the highest average per capita CO2 emissions was", 
                 per_capita_country, "with a value of", 
                 per_capita_co2, "million tonnes in", per_capita_year,".", sep = " "))
  })
  output$summary_three <- renderText({
    return(paste("3.", "Total CO2 emissions from gas production across all countries in 2018 was",
                 total_gas, "million tonnes.", sep = " "))
  })
  output$summary_four <- renderText({
    return(paste("4.", "Total CO2 emissions from coal production was", 
                 avg_coal, "million tonnes.", sep = " "))
  })
  output$summary_five <- renderText({
    return(paste("5. ", "The country with the highest CO2 emissions from cement production is ", 
                 max_cement_country, ".", sep = ""))
  })
  
  output$scatter <- renderPlotly({
    mdf2 <- mdf %>% 
      group_by(country) %>% 
      select(year, cement_co2, flaring_co2, oil_co2, coal_co2, gas_co2, other_industry_co2) %>% 
      rename(cement = cement_co2, 
             flare = flaring_co2, 
             oil = oil_co2, 
             coal = coal_co2, 
             gas = gas_co2, 
             other = other_industry_co2) %>% 
      filter(country == input$country_one)
    
    my_plot <- ggplot(mdf2, aes_string(x = mdf2$year, y = input$y_var)) + 
      geom_point() +
      xlab("Year") +
      ylab(paste("CO2 emission from", input$y_var, "(million tonnes)", sep = " ")) +
      ggtitle(paste("Growth of CO2 Emission by year from",
                    input$y_var, "in", input$country_one, sep = " "))
    
    ggplotly(my_plot) 
  })
}