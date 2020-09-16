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
  gargle_oob_default = TRUE, # from https://github.com/jennybc/googlesheets/issues/343#issuecomment-370202906
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "ivelasq@gmail.com",
  gargle_quiet = FALSE
)

options(gargle_quiet = FALSE)

gs4_auth(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

# Loading stuff -----------------------------------------------------------
# Loading functions
source("00_function.R") 

# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  new_full_entry_df <- reactive({
      mls_scrape(input$mls_number_textinput) %>% 
      add_row(mls = input$mls_number_textinput, attribute = "Location Rating", value = as.character(input$location_rating)) %>%
      add_row(mls = input$mls_number_textinput, attribute = "Price Rating", value = as.character(input$price_rating)) %>%
      add_row(mls = input$mls_number_textinput, attribute = "Place Rating", value = as.character(input$place_rating))
  })
  
  new_essential_entry_df <- reactive({
    new_full_entry_df() %>% 
      essentials_function
  })
  
  essential_googlesheet <- reactive({
    googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1wxAF8AkZLcNl1uD05ByOmZ9ffMxTGeVHgljUtOkzgsM/edit?usp=sharing")
  })
  
  # Run mls_scrape() and then add the ratings.
  observeEvent(input$submit_url_actionbutton, {
    
    output$new_full_dt <- DT::renderDataTable({
      new_full_entry_df()
      
  })
  })
  
  # Saving the data to google drive
  observeEvent(input$save_to_drive, {
    
      googlesheets4::sheet_append(new_full_entry_df(), 
                                  ss = "https://docs.google.com/spreadsheets/d/1-3zc3pxaxipcvpydSBzPiZ_iZDismmZRMKaCuAPdYIs/edit#gid=1751745956", 
                                  sheet = "entry_full_seed")
      googlesheets4::sheet_append(new_essential_entry_df(), ss = "https://docs.google.com/spreadsheets/d/1wxAF8AkZLcNl1uD05ByOmZ9ffMxTGeVHgljUtOkzgsM/edit#gid=60631602", sheet = "entry_full_essential")
      
    })
  
  observeEvent(input$googlesheet_reload, {

  output$all_essential_no_action_req <- DT::renderDataTable({
    essential_googlesheet()
    })
  
  })
  
} # Server