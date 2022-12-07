# Set Up ------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

df <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
# get rid of continent data. Only want country
mdf <- subset(subset(df, iso_code != ""), iso_code != "OWID_WRL")

# Value Calculation ----------------------------------------------------------

# 1
# avg co2_growth_abs across all countries in 2018
df_growth <- mdf %>% 
  filter(year == 2018) %>% 
  select(co2_growth_abs) %>%
  na.omit()
avg_growth <- mean(df_growth$co2_growth_abs, na.rm = T)

# 2
# highest co2_per_capita across all countries over time
# output: year, country w/ highest co2_per_capita, and the exact value
df_capita <- mdf %>% 
  filter(co2_per_capita == max(co2_per_capita, na.rm = T)) %>% 
  select(year, country, co2_per_capita)
capita_summary <- paste("The country with the highest average per capita CO2 emissions was", 
                        df_capita$country, "with a value of", 
                        df_capita$co2_per_capita, "million tonnes in", df_capita$year,".", sep = " ")

# 3
# total gas_co2 in 2018 across all countries
df_gas <- mdf %>% 
  filter(year == 2018) %>% 
  select(gas_co2) %>% 
  summarise(total = sum(gas_co2, na.rm = T))
total_gas <- df_gas$total

# 4
# avg of coal_co2 in 2018 across all countries
avg_coal <- mdf %>% 
  filter(year == 2018) %>% 
  select(coal_co2) %>% 
  summarise(avg = mean(coal_co2, na.rm = T)) %>% 
  pull(avg)

# 5
# Country with the highest cement_co2 of all times
cement_ctr <- mdf %>% 
  filter(cement_co2 == max(cement_co2, na.rm = T)) %>% 
  pull(country)

server <- function(input, output) {
  output$message <- renderText({
    my_greeting <- "Hello, "
    user_name <- input$name
    if (user_name == "") {
      user_name <- "User"
    }
    message_str <- paste0(my_greeting, user_name, "!")
    message_str 
  })
  
  output$summary1 <- renderText({
    return(paste("1.", "The average for CO2 growth across all countries in 2018 was",
                 prettyNum(avg_growth, big.mark = ",", scientific = FALSE), "million tonnes.", sep = " "))
  })
  output$summary2 <- renderText({
    return(paste("2.", prettyNum(capita_summary, big.mark = ",", scientific = FALSE), sep = " "))
  })
  output$summary3 <- renderText({
    return(paste("3.", "Total CO2 emissions from gas production across all countries in 2018 was",
                 prettyNum(total_gas, big.mark = ",", scientific = FALSE), "million tonnes.", sep = " "))
  })
  output$summary4 <- renderText({
    return(paste("4.", "Total CO2 emissions from coal production was", 
                 prettyNum(avg_coal, big.mark = ",", scientific = FALSE), "million tonnes.", sep = " "))
  })
  output$summary5 <- renderText({
    return(paste("5.", "The country with the highest CO2 emissions from cement production is ", 
                 cement_ctr, ".", sep = ""))
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
      filter(country == input$country1)
    
    my_plot <- ggplot(mdf2, aes_string(x = mdf2$year, y = input$y_var)) + 
      geom_point() +
      xlab("Year") +
      ylab(paste("CO2 emission from", input$y_var, "(million tonnes)", sep = " ")) +
      ggtitle(paste("Growth of CO2 Emission by year from",
                    input$y_var, "in", input$country1, sep = " "))
    
    ggplotly(my_plot) 
  })
}