---
title: "HW2"
author: "Xinyi Wang"
date: "2023-02-25"
output: html_document
  pdf_document:
    latex_engine: pdflatex
  always_allow_html: true
---


```{r,message = FALSE}
library(feasts)
library(tsibble)
library(scales)
library("knitr")
library(maps)
library(rhandsontable)
library(rnaturalearth)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(ggplot2)
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(MAP)
library(usmap)
theme_set(theme_bw())
```

# Data cleaning
```{r eruptions, echo=FALSE}
honey_latlong = read.csv("US_honey_dataset_updated.csv") %>% select(-1) 
year = pull(honey_latlong, year) %>% unique()
state = pull(honey_latlong, state) %>% unique()
head(honey_latlong)
```



# Shiny App      
```{r}
#to determine limits of legend
min(honey_latlong$production)
max(honey_latlong$production)

#create functions
plotProduction <- function(df) {
  df %>% 
  ggplot(aes(x = year, y = production)) + 
  geom_point() +
  geom_smooth() +
    labs(title = "different states with change of production by years", y = "Production") +
    guides(color = FALSE)+
    scale_x_continuous(limits = c(min(year), max(year)), breaks = seq(min(year), max(year), 1)) +
    theme(
    axis.text.x = element_text(angle = 90))
}

usmap <- function(df) {
  plot_usmap(data = df, values = "production",labels = TRUE) +
    scale_fill_continuous(low = "white",high = "purple",name = "Production", 
                          label = scales::comma) +
    theme(legend.position = "bottom",
          legend.key.width = unit(2.5, "cm")) +
    scale_color_gradient2(limits = c(10000, 40000000)) +
    labs(title = "usmap of different states' production in one year", color = "Production")
}


 
 ### definition of app
 ui <- fluidPage(
  titlePanel("Honey Production in the U.S.A."),
  fluidRow(
    column(6, sliderInput("year", "Year of Honey production:",
                          min = min(year), max = max(year),value = min(year), step = 1,sep = "")),
    column(6, selectInput("state", "State:", state, selected = "Alabama"))
  ),
  fluidRow(
    column(4, plotOutput("usmap", width = "100%", height = "400px")),
    column(4, plotOutput("production", width = "100%", height = "400px"))
  ),
  fluidRow(
  column(12, HTML("<p>The usmap at the left side displays comparison of honey production between different states at a certain year (from 1995 to 2021), darker the color, greater the production. </p>
                  <p>The second scatterplot at the right side shows the trend of change of honey production of a certain us state from 1995 to 2021. </p>
                  <p><mark>The usmap focuses on geographic comparison in different states by controlling year, whereas scatterplot focuses on quantitative comparison in different years by controlling state. <mark></p>"))
  )
  )


 
server <- function(input, output) {
  state_selected <- reactive({
    honey_latlong %>%
      filter(state == input$state)
  })
  
  output$production <- renderPlot({
    plotProduction(state_selected())
  })
  
  Production_year <- reactive({
  honey_latlong %>%
    filter(year == input$year)
})

  
  output$usmap <- renderPlot({
    usmap(Production_year())
  })

}


 
app = shinyApp(ui, server)
```

```{r}
library(feasts)
library(tsibble)
honey_tsi = 
  honey_latlong %>% 
  as_tsibble(index = year, key = state) %>% 
  select(year, state, production)
honey_tsi %>% 
  ggplot()+
  geom_line(aes(x = year, y = production)) +
  labs(title = "Honey production time series plot")
```


# Write-ups

What are some interesting facts that you learned through the visualization. Provide at least one unexpected finding.

1. How did you create the interface? Were there any data preparation steps? What guided the style customization and interface layout that you used.

2. What is the reactive graph structure of your application?
**Write-up is in pdf uploaded on the Piazza**

**The reactive graph structure is demonstrated below:**

```{r echo = FALSE, out.width = "500px"}
knitr::include_graphics("/Users/wangxinyi/Desktop/junior2/STAT 436/Homework/WechatIMG1882.png")

```

**The recordings of shiny app can be watched by clicking the [Click here to watch the video] below:**

[Click here to watch the video](https://www.youtube.com/watch?v=ixRFtPMzBqY)








