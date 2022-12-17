remove(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

library(tidyverse);library(lubridate)


# Rename columns ----------------------------------------------------------
cols_list <- c("state_code_geo_a","fips_county_code_geo_a","state_code_geo_b","fips_county_code_geo_b",
               "state_name_geo_a", "county_name_geo_a", "state_name_geo_b","county_name_geo_b",
               "b_to_a_flow","b_to_a_flow_moe","a_to_b_counterflow","a_to_b_counterflow_moe",
               "net_migration_b_to_a","net_migration_b_to_a_moe","gross_migration_a_and_b",
               "gross_migration_a_and_b_moe")

x <- readxl::read_excel("county-to-county-2015-2019-ins-outs-nets-gross.xlsx", 
                        sheet = "New York",col_names = FALSE,
                        skip = 3)
colnames(x) <- cols_list

# -------------------------------------------------------------------------
nyc_counties <- c("Kings County","Richmond County","New York County","Queens County",
                  "Bronx County")

nyc <- x %>%
    filter(county_name_geo_a %in% nyc_counties)

##These are the top states and the top states for movements out of each county
##Unsurprisingly, top states are internal (probably accounting for internal relocation)
top_states <- nyc %>%
    group_by(state_name_geo_b) %>%
    summarize(total_state = sum(a_to_b_counterflow, na.rm = TRUE)) %>%
    arrange(desc(total_state)) %>%
    mutate(pct = total_state/sum(total_state))

top_states_by_county <- nyc %>%
    group_by(county_name_geo_a,
             state_name_geo_b) %>%
    summarize(total_state = sum(a_to_b_counterflow, na.rm = TRUE)) %>%
    arrange(county_name_geo_a,desc(total_state)) %>%
    mutate(pct = total_state/sum(total_state)) %>%
    top_n(10)



###
internal_and_neighbors <- c("New York","New Jersey","Connecticut",
                            "Pennsylvania","Vermont","Massachusetts",
                            "Rhode Island")

top_states_excludes_neighbors <- nyc %>%
    group_by(state_name_geo_b) %>%
    summarize(total_state = sum(a_to_b_counterflow, na.rm = TRUE)) %>%
    filter(!state_name_geo_b %in% internal_and_neighbors) %>%
    arrange(desc(total_state)) %>%
    mutate(pct = total_state/sum(total_state))

top_states_excludes_neighbors_by_county <- nyc %>%
    group_by(county_name_geo_a,
             state_name_geo_b) %>%
    summarize(total_state = sum(a_to_b_counterflow, na.rm = TRUE)) %>%
    filter(!state_name_geo_b %in% internal_and_neighbors) %>%
    arrange(county_name_geo_a,desc(total_state)) %>%
    mutate(pct = total_state/sum(total_state)) %>%
    top_n(10)

# What about net migration? -----------------------------------------------
top_states_net_migration <- nyc %>%
    group_by(state_name_geo_b) %>%
    summarize(total_state = sum(net_migration_b_to_a, na.rm = TRUE)) %>%
    arrange(total_state) %>%
    mutate(pct = total_state/sum(total_state))

top_states_net_migration_no_neighbors <- nyc %>%
    group_by(state_name_geo_b) %>%
    summarize(total_state = sum(net_migration_b_to_a, na.rm = TRUE)) %>%
    filter(!state_name_geo_b %in% internal_and_neighbors)  %>%
    arrange(total_state) %>%
    mutate(pct = total_state/sum(total_state))

top_states_net_migration_by_county_no_neighbors <- nyc %>%
    group_by(county_name_geo_a,
             state_name_geo_b) %>%
    summarize(total_state = sum(net_migration_b_to_a, na.rm = TRUE)) %>%
    filter(!state_name_geo_b %in% internal_and_neighbors)  %>%
    arrange(county_name_geo_a,total_state) %>%
    mutate(pct = total_state/sum(total_state)) %>%
    top_n(10)

# What are the top counties in Florida -----------------------------------
top_florida_counties <- nyc %>%
    filter(state_name_geo_b == "Florida") %>%
    mutate(county_fips_full = str_replace(paste0(state_code_geo_b,fips_county_code_geo_b),"^0","")) %>%
    group_by(county_fips_full,county_name_geo_b) %>%
    summarize(total_flow = sum(a_to_b_counterflow, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pct_of_total_flow = total_flow/sum(total_flow)) %>%
    arrange(desc(total_flow))

top_media_markets_in_florida <- nyc %>%
    filter(state_name_geo_b == "Florida") %>%
    mutate(county_fips_full = str_replace(paste0(state_code_geo_b,fips_county_code_geo_b),"^0","")) %>%
    inner_join(read_csv("county_to_dma_2018.csv"), by = c("county_fips_full" = "county_fips")) %>%
    group_by(media_market) %>%
    summarize(total_flow = sum(a_to_b_counterflow, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pct_of_total_flow = total_flow/sum(total_flow)) %>%
    arrange(desc(total_flow))


# Create a map of Florida -------------------------------------------------
library(urbnmapr)

urbnmapr::counties %>%
    filter(state_abbv == "FL") %>%
    left_join(top_florida_counties, by = c("county_fips" = "county_fips_full")) %>%
    ggplot(aes(x = long, y = lat, fill = total_flow, group = group))+ 
    geom_polygon(color = "black", size = 0.1) + 
    scale_fill_viridis_c(na.value = "gray70") + 
    theme_void() + 
    labs(fill = "Total flow from NYC 2015-2019") + 
    theme(legend.position = "bottom") + 
    guides(fill = guide_colorbar(title.position = "top",barwidth = 20))

