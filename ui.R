#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Template for Shiny-Spoonacular-AMPL-Neos", windowTitle = "DietSpoonApp"),
  
  sidebarLayout(
    sidebarPanel(
      "Put a + sign between words. Here on the right hand side you will see the API request:",
       textInput("mashString",
                label = "Paste your MASHKEY here below",
                value = "",
                width = '100%'),
       textInput("cuisineString",
                   label = "cuisine=",
                   value = "",
                   width = '100%'),
       textInput("dietString",
                   label = "diet=",
                   value = "",
                   width = '100%'),
       textInput("queryString",
                   label = "query=",
                   value = "burger+chips",
                   width = '100%'),
       actionButton("seek", "1. Search Spoonacular", icon = icon("search"), width='100%'),
       actionButton("ampl", "2. Write AMPL data", icon = icon("pencil"), width='100%'),
       actionButton("neossend", "3. Send to Neos", icon = icon("send"), width='100%'),
       actionButton("neosget", "4. Try to get result from Neos", icon = icon("refresh"), width='100%'),
       width = 3
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       textOutput("urlString"),
       dataTableOutput("recipeString"),
       textOutput("writeString"),
       textOutput("optimizeString"),
       textOutput("neosString")
    )
  )
))
