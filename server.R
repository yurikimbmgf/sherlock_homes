##############
##          ##
##  Server  ##
##          ##
##############

library(tidyverse)
library(here)
library(rvest)
library(DT)
library(gargle)
library(googlesheets4)
library(httr)




# Prep --------------------------------------------------------------------
# Only run this once then comment out. Needed to save the login info for Google Sheets
# From here: https://gargle.r-lib.org/articles/non-interactive-auth.html

# # designate project-specific cache
# options(gargle_oauth_cache = ".secrets")
# 
# # check the value of the option, if you like
# gargle::gargle_oauth_cache()
# 
# # trigger auth on purpose --> store a token in the specified cache
# gs4_auth()
# 
# # see your token file in the cache, if you like
# list.files(".secrets/")


# Getting Google Sheets to work -------------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "yurickim@gmail.com",
  gargle_quiet = FALSE
)

# Loading stuff -----------------------------------------------------------
# Loading functions
source("00_function.R") 


# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  # Run mls_scrape() and then add the ratings.
  new_full_entry_df <- eventReactive(input$submit_url_actionbutton, {
    mls_scrape(input$mls_number_textinput) %>% 
      add_row(mls = input$mls_number_textinput, attribute = "Location Rating", value = as.character(input$location_rating)) %>%
      add_row(mls = input$mls_number_textinput, attribute = "Price Rating", value = as.character(input$price_rating)) %>%
      add_row(mls = input$mls_number_textinput, attribute = "Place Rating", value = as.character(input$place_rating))
  })
  
  # Runs the essential fucntions
  new_essential_entry_df <- eventReactive(input$submit_url_actionbutton, {
    essentials_function(new_full_entry_df())
  })
  
  # Wrapper to make the new_full_entry_df a datatable
  output$new_full_dt <- renderDataTable({
    new_full_entry_df()
  })
  
  # Saving the data to google drive
  observeEvent(
    input$save_to_drive,{
      googlesheets4::sheet_append(new_full_entry_df(), ss = "https://docs.google.com/spreadsheets/d/1-3zc3pxaxipcvpydSBzPiZ_iZDismmZRMKaCuAPdYIs/edit#gid=1751745956", sheet = "entry_full_seed")
      googlesheets4::sheet_append(new_essential_entry_df(), ss = "https://docs.google.com/spreadsheets/d/1wxAF8AkZLcNl1uD05ByOmZ9ffMxTGeVHgljUtOkzgsM/edit#gid=60631602", sheet = "entry_full_essential")
    })
  
  # I don't really know how this works.
  # Pushing the button to reload Google Sheet in the view mode.
  rv <- reactiveValues()
  observeEvent(input$googlesheet_reload, {
    rv$essential_googlesheet <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1wxAF8AkZLcNl1uD05ByOmZ9ffMxTGeVHgljUtOkzgsM/edit?usp=sharing")
  })
  output$all_essential_no_action_req <- renderDataTable({
    rv$essential_googlesheet
  })
  
  
  
  
} # Server