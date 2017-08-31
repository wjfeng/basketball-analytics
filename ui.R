library(shiny)
library(ggplot2)
library(grid)
library(jpeg)
library(RCurl)
library(hexbin)
library(data.table)

source("passes.R")
source("fieldgoals.R")

ui <- fluidPage(
  titlePanel("Interactive NBA Pass and Shot Maps"),
  sidebarLayout(position = "right",
                sidebarPanel(
  selectInput(inputId = "player_name",
              label = "Player",
              choices = c("Willie Cauley-Stein","Kosta Koufos","Garrett Temple","George Hill","Zach Randolph","Vince Carter")
              ),
  selectInput(inputId = "map_type",
              label = "Type",
              choices = c("Pass","Shot")
              )
  ,h3(textOutput("summary_stats_header"))
  ,tableOutput("summary_stats")
  ),
  mainPanel(
  plotOutput("court")
  )
  )
)