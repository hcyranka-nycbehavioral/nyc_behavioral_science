remove(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

library(tidyverse);library(RSocrata);library(lubridate)

dict <- readxl::read_excel("Housing Database - Data Dictionary.xlsx",sheet = 2,skip = 1) %>%
    janitor::clean_names()

x <- vroom::vroom("HousingDB_post2010.csv") %>%
    janitor::clean_names()


# -------------------------------------------------------------------------
completed_construction <- x %>%
    filter(job_status == "5. Completed Construction")

# -------------------------------------------------------------------------
permits_by_date <- x %>%
    dplyr::select(job_number,job_type,job_status,date_permit,date_lst_upd,date_complt,boro,ownership,latitude, longitude) %>%
    mutate(date_permit = mdy(str_replace_all(date_permit," 0:00:00", "")),
           date_complt = mdy(str_replace_all(date_complt," 0:00:00", ""))) 

new_buildings <- permits_by_date %>%
    filter(job_type == "New Building") %>%
    arrange(date_permit)

# -------------------------------------------------------------------------
permits_by_month <- new_buildings %>%
    mutate(permit_month = floor_date(date_permit, "month")) %>%
    group_by(permit_month, boro) %>%
    summarize(
        total_permits = n()
    ) %>%
    arrange(boro, permit_month) %>%
    mutate(boro_name = case_when(
        boro == 1 ~ "Manhattan",
        boro == 2 ~ "Bronx",
        boro == 3 ~ "Brooklyn",
        boro == 4 ~ "Queens",
        boro == 5 ~ "Staten Island"
    )) %>%
    filter(permit_month >= ymd("2009-07-01"),
           permit_month < ymd("2022-07-01")) %>%
    ggplot(aes(x = permit_month
               ,y = total_permits
               ,color = boro_name)) + 
    geom_line(show.legend = FALSE) + 
    labs(x = "Time", y = "Total permits issued",
         title = "New residential building permits issued by month for the 5 boroughts",
         subtitle = "July 2009 to July 2022") + 
    facet_wrap(~boro_name, scales = "free_x") + 
    theme_bw()

permits_by_month


# -------------------------------------------------------------------------
library(zipcodeR)

ny_zips <- as_tibble(zip_code_db) %>%
    filter(state == "NY",
           zipcode_type == "Standard",
           county %in% c("New York County",
                         "Kings County",
                         "Queens County",
                         "Richmond County",
                         "Bronx County"))






