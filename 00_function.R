################
##            ##
##  Functions ##
##            ##
################

# Functions for the app

# Key Variables -----------------------------------------------------------
# Hard coding the mortgage rate
mortage_int_rate <- .03014

# Hard coding the "essential" variables for the condensed view of the data.
ordered_essentials <- data.frame(
  stringsAsFactors = FALSE,
  id = c(1L,2L,3L,4L,5L,6L,
         7L,8L,9L,10L,11L,12L,13L,
         14L,15L,16L,17L),
  attribute = c("List Price",
                "Monthly Payment Total","Bedrooms",
                "Total Baths","Finished SqFt","Address",
                "Community",
                "Style","Parking Type",
                "Monthly Taxes",
                "Monthly Principal and Interest",
                "Monthly Insurance","Monthly HOA","MLS", 
                "Location Rating",
                "Price Rating",
                "Place Rating")
)

# Scraping ----------------------------------------------------------------
# Function that scrapes the findourpad.com site, pulls the data and puts it into a table.
# Also does calculations for monthly cost estimates.
mls_scrape <-  function(mls_code) {
  # Read the page
  df <- read_html(paste0("https://www.findourpad.com/property/", mls_code, "/"))
  
  
  scraped_data <- tibble(
    # Gragging address
    attribute = "Address", value = df %>% html_nodes("h1") %>% html_text()) %>% 
    # Grabbing the first table
    bind_rows(
      df %>% 
        html_nodes(".property-details-section") %>% 
        .[2] %>% 
        html_nodes(".prop-descrip") %>%
        html_text(trim = TRUE) %>% 
        str_replace_all("\n", ":") %>% 
        as_tibble() %>% 
        separate(value, c("attribute", "value"), ":") %>% 
        mutate_at(vars(value), str_replace, "^[:blank:]+", "")
    ) %>% 
    # Grabbing second table
    bind_rows(
      df %>% 
        html_nodes(".property-details-section") %>% 
        html_nodes(".prop-descrip") %>%
        html_text(trim = TRUE) %>% 
        str_replace_all("\n", ":") %>% 
        as_tibble() %>% 
        separate(value, c("attribute", "value"), ":") %>% 
        mutate_at(vars(value), str_replace, "^[:blank:]+", "")
    ) %>% 
    distinct()
  
  # Pulling out just the MLS number
  mls_only <- scraped_data %>% 
    filter(attribute == "MLS#") %>% 
    select(value) %>% 
    as.character()
  
  # Gragging a bunch of the numbers needed for the cacluations
  list_price_only <- scraped_data %>% 
    filter(attribute == "List Price") %>% 
    mutate(value = parse_number(value)) %>% 
    select(value) %>% 
    as.numeric() 
  
  monthly_taxes_only <- scraped_data %>% 
    filter(attribute == "Annual Taxes") %>% 
    mutate(value = parse_number(value)/12) %>% 
    select(value) %>% 
    as.numeric()
  
  # Some calculations here.
  monthly_principal_and_interest_only <- (list_price_only*.8)*(mortage_int_rate/12)*(1+(mortage_int_rate/12))^(30*12)/((1+(mortage_int_rate/12))^(30*12)-1)
  
  monthly_insurance_only <- scraped_data %>% 
    filter(attribute == "List Price") %>% 
    mutate(value = parse_number(value)/100000) %>% 
    select(value) %>% 
    as.numeric()*35
  
  # Not all pages have HOA data so this is an if/else to avoid errors.
  monthly_hoa_only <- if(scraped_data %>% filter(attribute == "HOA Dues") %>% nrow() == 0) {
    0
  } else {
    scraped_data %>% 
      filter(attribute == "HOA Dues") %>% 
      mutate(value = case_when(is.na(value) ~ 0, TRUE ~ parse_number(value)))%>% 
      select(value) %>% 
      as.numeric() 
  }
  
  total_monthly_payment_only <- round(monthly_taxes_only + monthly_principal_and_interest_only + monthly_insurance_only + monthly_hoa_only)
  
  # Bath data is split by the type of bath on the page. Making it a single number.
  total_baths_only <- scraped_data %>% 
    filter(str_detect(attribute, "Baths")) %>% 
    mutate(bath_prep = case_when(attribute == "Full Baths" ~ 1,
                                 attribute == "Half Baths" ~ .5,
                                 attribute == "Three Quarter Baths" ~ .75)) %>% 
    mutate(bath_percentage = as.numeric(value) * bath_prep) %>% 
    summarise(total_baths = sum(bath_percentage, na.rm = T)) %>% 
    as.numeric()
  
  
  # Bringing it all together.
  final_df <- scraped_data %>% 
    add_row(attribute = "Monthly Taxes", value = as.character(monthly_taxes_only))%>% 
    add_row(attribute = "Monthly Principal and Interest", value = as.character(monthly_principal_and_interest_only)) %>% 
    add_row(attribute = "Monthly Insurance", value = as.character(monthly_insurance_only)) %>% 
    add_row(attribute = "Monthly HOA", value = as.character(monthly_hoa_only)) %>% 
    add_row(attribute = "Monthly Payment Total", value = as.character(total_monthly_payment_only)) %>% 
    add_row(attribute = "Total Baths", value = as.character(total_baths_only))   %>%
    add_row(attribute = "MLS", value = mls_only) %>% 
    mutate(mls = mls_only) %>%
    select(mls, attribute, value)
}


# This function takes the completed DF from mls_scrape() and just pulls out the key data.
# Also rounds the numbers.
essentials_function <- function(df) {
  df %>% 
    left_join(ordered_essentials, by = "attribute") %>% 
    filter(!is.na(id)) %>% 
    arrange(id) %>% 
    select(-mls, -id) %>% 
    pivot_wider(names_from = attribute, values_from = value) %>% 
    mutate_at(vars(`List Price`,
                   `Monthly Taxes`,
                   `Finished SqFt`,
                   `Bedrooms`,
                   `Total Baths`,
                   `Monthly Taxes`,
                   `Monthly Principal and Interest`,
                   `Monthly Insurance`,
                   `Monthly HOA`,
                   `Monthly Payment Total`), parse_number) %>% 
    mutate_at(vars(`Monthly Taxes`,
                   `Monthly Principal and Interest`,
                   `Monthly Insurance`,
                   `Monthly HOA`,
                   `Monthly Payment Total`), round) 
}

# Testing grabbing the Google Sheets data.
# googlesheets4::sheet_write(test, ss = "https://docs.google.com/spreadsheets/d/1-3zc3pxaxipcvpydSBzPiZ_iZDismmZRMKaCuAPdYIs/edit#gid=1751745956", sheet = "entry_full_seed")
# googlesheets4::sheet_write(test_essentials, ss = "https://docs.google.com/spreadsheets/d/1wxAF8AkZLcNl1uD05ByOmZ9ffMxTGeVHgljUtOkzgsM/edit#gid=60631602", sheet = "entry_full_essential")
