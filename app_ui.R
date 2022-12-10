library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

# Wrangle the data :)
df <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
# get rid of continent data. Only want country
mdf <- subset(subset(df, iso_code != ""), iso_code != "OWID_WRL")

# Cleaning data
cdf1 <- mdf %>% 
  select(country, year, cement_co2, flaring_co2, oil_co2, 
         coal_co2, gas_co2, other_industry_co2) %>% 
  rename(cement = cement_co2, 
         flare = flaring_co2, 
         oil = oil_co2, 
         coal = coal_co2, 
         gas = gas_co2, 
         other = other_industry_co2)
cdf2 <- cdf1 %>% 
  select(cement, flare, oil, 
         coal, gas, other)
country_one <- unique(cdf1$country)

# UI Layout
page_one <- tabPanel(
  "Introduction",
  titlePanel(textOutput("greeting")),
  name_input <- textInput(
    inputId = "name",
    label = "Enter your name:"
  ),
  p("Climate change, which should be considered a scientific matter has turned 
    political in many areas of the world. It can be hard to cope or even 
    conceptualize the phenomenon of climate change, but it needs to be taken
    very seriously by the entire world. This is partially due to the concept of
    climate change vulnerability, which finds that some social groups experience 
    greater loss of resources and more significant impacts to livelihoods and 
    cultural identity than others as a result of climate change. In other words,
    climate change does not affect all of us equally. Even if you may be causing
    more climate change than the next individual, you may never realize your
    impact because someone else could be living through the consequences of your
    actions. It is clear that climate change is real and is not disappearing
    anytime soon, we need to start taking action to help the environment,
    with science as our guide."),
  p(),
  h3("Variables to Explore:"),
  p("I have chosen to analyze the annual production-based emissions of carbon
  dioxide (CO₂) from different materials such as cement, flare, coal, oil, gas,
  and other for each country."),
  p(),
  h3("Measuring CO2:"),
  p("The annual production-based emissions of carbon dioxide (CO₂) from cement, 
  flare, coal, oil, gas, and other for each country will be measured in million
  tonnes."),
  p(),
  h3("5 Summary Statistics:"),
  textOutput("summary_one"),
  textOutput("summary_two"),
  textOutput("summary_three"),
  textOutput("summary_four"),
  textOutput("summary_five")
)

page_two <- tabPanel(
  "Data Visualization",
  y_input <- selectInput(
    inputId = "y_var",
    choices = colnames(cdf2),            
    label = "Choose a resource"
  ),
  year_input <- selectInput(
    inputId = "country_one",
    choices = country_one,            
    label = "Choose a country"
  ),
  plotlyOutput("scatter"),
  p(em("Figure 1."), "This visualization demonstrates the overall trend of CO2 
    emissions by year per product per country. This visualization also allows
    us to compare different resources and countries with another over time to
    be able to get a bigger picture on how the world's CO2 emissions by year can
    be broken down. It will take both an analysis of the larger picture as well
    as smaller details to be able to positively impact climate change.")
)

ui <- navbarPage(
  "CO2 Emission Investigation", # application title
  page_one,
  page_two
)