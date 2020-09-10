library(shiny)


ui <- navbarPage("Sherlock Homes",
                 
                 # Add Entry Tab
                 tabPanel("Add Entry",
                          # Input the MLS number
                          fluidRow(
                            column(2, div()),
                            column(8,
                                   textInput("mls_number_textinput", "Enter MLS Number", ""),
                                   p(),
                                   h3("Ratings")
                            )
                          ),
                          # Add ratings dropdowns
                          fluidRow(
                            column(2, div()),
                            column(2, selectInput("location_rating", "Location Rating", c("Positive", "Neutral", "Negative"))),
                            column(1, div()),
                            column(2, selectInput("price_rating", "Price Rating", c("Positive", "Neutral", "Negative"))),
                            column(1, div()),
                            column(2, selectInput("place_rating", "Place Rating", c("Positive", "Neutral", "Negative")))
                          ),
                          # Button to load the MLS Data + Ratings
                          fluidRow(
                            column(4, div()),
                            column(4, actionButton("submit_url_actionbutton", "Preview Entry", width = "100%"), p())
                          ),
                          # Button to save to Google Drive
                          fluidRow(
                            column(4, div()),
                            column(4, actionButton("save_to_drive", "Save Entry", width = "100%"))
                          ),
                          # Adding spacing
                          fluidRow(
                            column(10, div(style = "height:150px;"))
                          ),
                          # When you press the the submit_url_actionbutton button, it loads this output
                          fluidRow(
                            column(2, div()),
                            column(8, 
                                   h3("Your Entry"),
                                   dataTableOutput("new_full_dt"))
                          )
                 ),
                 # View Entries Tab
                 tabPanel("View Entries",
                          fluidRow(
                            column(12,
                                   h3("All Entries"),
                                   # Button so you can re-load the Google sheet to get the latest data.
                                   actionButton("googlesheet_reload", "Load / Reload Table"),
                                   div(dataTableOutput("all_essential_no_action_req"))
                                   
                            )
                          )
                 )
)
