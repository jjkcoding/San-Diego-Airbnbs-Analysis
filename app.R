
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinydashboardPlus)
library(shinydashboard)

# setwd("C:/Users/joshu/Desktop/RPractice/san diego analysis/")
groups = 6
sd_dat = read.csv("san_diego_listings.csv", header = T)
sd_filt = sd_dat[!is.na(sd_dat$reviews_per_month),]
sd_filt = sd_filt[sd_filt$last_review >= "2022-01-01",]
sd_filt = sd_filt[sd_filt$price <= 2000,]

# Making Tier List By Average Price of Neighbourhood
neigh_price = sd_filt %>%
  group_by(neighbourhood) %>% 
  summarize(avg_price = mean(price))
neigh_price = neigh_price[order(neigh_price$avg_price, decreasing = T),]

tier_names = paste0("Tier ", seq(1, groups))
perGroup = nrow(neigh_price) / groups
tier_neigh = split(neigh_price$neighbourhood, rep(1:groups, each = perGroup, length.out = nrow(neigh_price))) # Split Neighbourhood names to tiers
tier_price = split(neigh_price$avg_price, rep(1:groups, each = nrow(neigh_price) / groups, length.out = nrow(neigh_price))) # Split Neighbourhood prices to tiers

# Getting Tier Name With Average Price Intervals
tier_print = c()
for(i in 1:groups){
  cur_vals = as.vector(unlist(tier_price[i]))
  cur_min = round(min(cur_vals), 2)
  cur_max = round(max(cur_vals), 2)
  tier_print = c(tier_print, paste(tier_names[i], " (Averages: $", cur_min, " - $", cur_max, ")", sep = ""))
}


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "SD Airbnb"),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Visit Inside Airbnb For Data", icon = icon("send", lib = "glyphicon"),
               href = "http://insideairbnb.com/"),

      selectInput(inputId = "tier", label = "Neighbourhood Tier:",
                  choices = tier_print,
                  selected = tier_print[1]),
      sliderInput(inputId = "priceNight",
                  label = "Listing Price Per Night Filter:",
                  min = 0,
                  max = max(sd_filt$price),
                  value = c(0, max(sd_filt$price)))
        
  

  )),
  dashboardBody(
    fluidRow(
      valueBoxOutput("price_median"),
      valueBoxOutput("reviews_mean"),
      valueBoxOutput("listing_mean")
    ),
    box(title = "Price Distribution By Listing", 
        status = "primary",
        solidHeader = T,
        plotOutput(outputId = "pricesHist")),
    box(title = "Boxplots By Room Type", 
        status = "primary",
        solidHeader = T,
        plotOutput(outputId = "boxPlot")),
    box(title = "Average Prices By Neighbourhood", 
        status = "primary",
        solidHeader = T,
        plotOutput(outputId = "neighPrice")),
    box(title = "Number Of Listings By Neighbourhood", 
        status = "primary",
        solidHeader = T,
        plotOutput(outputId = "neighListing"))
  )
)

server <- function(input, output) {
  
  dataDf = reactive({
    cur_neigh = as.vector(unlist(tier_neigh[which(tier_print == input$tier)]))
    cur_df = sd_filt %>%
      filter(neighbourhood %in% cur_neigh & price >= input$priceNight[1] & price <= input$priceNight[2])
    cur_df
  })

  
  output$pricesHist <- renderPlot({
    ggplot(data = dataDf(), aes(x = price)) +
      geom_histogram(binwidth = 40, fill = "orange") +
      labs(x = "", y = "") +
      scale_x_continuous(labels=scales::dollar_format()) +
      theme_bw() +
      theme(text = element_text(size = 15))
    
  })
  
  output$boxPlot <- renderPlot({
    ggplot(data = dataDf(), aes(x = room_type, y = price)) +
      geom_boxplot(color = "black", fill = "orange") +
      labs(x = "", y = "") + 
      scale_y_continuous(labels=scales::dollar_format()) +
      theme_bw() + 
      theme(legend.position = "none",
            text = element_text(size = 15))

  })
  
  output$neighPrice <- renderPlot({
    cur_neigh_price = dataDf() %>%
      group_by(neighbourhood) %>% 
      summarize(avg_price = mean(price))
    cur_neigh_price = cur_neigh_price[order(cur_neigh_price$avg_price, decreasing = T),]
    cur_neigh_count = dataDf() %>%
      group_by(neighbourhood) %>%
      count()

    top_neigh_prices = merge(cur_neigh_price, cur_neigh_count, by = "neighbourhood")
    
    ggplot(data = top_neigh_prices, aes(x = fct_reorder(neighbourhood, avg_price), y = avg_price, fill = n)) +
      geom_bar(stat = "identity", aes(fill = n)) + 
      labs(x = "", y = "")+
      scale_y_continuous(labels=scales::dollar_format()) +
      coord_flip() + 
      theme_bw() +
      guides(fill = guide_colourbar(title="Number Of Listings")) +
      scale_fill_gradient(low = "blue", high = "red", na.value = NA) +
      theme(text = element_text(size = 15))
    
  })
  
  output$neighListing <- renderPlot({
    cur_neigh_count = dataDf() %>%
      group_by(neighbourhood) %>%
      count()
    cur_neigh_count = cur_neigh_count[order(cur_neigh_count$n, decreasing = T),]
    ggplot(data = cur_neigh_count, aes(x = fct_reorder(neighbourhood, n), y = n)) +
      geom_point(size = 3) +
      geom_segment(aes(x = neighbourhood,
                       xend = neighbourhood,
                       y = 0,
                       yend = n)) +
      labs(x = "", y = "") +
      coord_flip() + 
      theme_bw() +
      theme(text = element_text(size = 15))
  })
  
  output$price_median <- renderValueBox(
    valueBox(
      value = paste("$", round(median(dataDf()$price),2), sep = ""),
      subtitle = "Median Listing Price Per Night",
      color = "green",
      icon = icon("bar-chart")
    )
  )
  
  output$reviews_mean <- renderValueBox(
    valueBox(
      value = round(mean(dataDf()$number_of_reviews),2),
      subtitle = "Average Listing Reviews",
      color = "blue",
      icon = icon("bar-chart")
    )
  )
  
  output$listing_mean <- renderValueBox({
    host_count = dataDf() %>%
      group_by(host_id) %>%
      count()
    
    valueBox(
      value = round(mean(host_count$n),2),
      subtitle = "Average Number Of Listings",
      color = "yellow",
      icon = icon("bar-chart")
    )
  })
  
}

shinyApp(ui = ui, server = server)
