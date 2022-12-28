remove(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

library(tidyverse);library(RSocrata);library(lubridate)

# Downloading data ----------------------------------------------
x <- read.socrata("https://data.cityofnewyork.us/resource/f3yh-rnye.json") %>%
    as_tibble() %>%
    janitor::clean_names()%>%
    mutate_at(c("expenditures_fy21_000_000","modified_budget_for_fy21"),
              .funs = ~as.numeric(.))


# Top agencies by expenditures --------------------------------------------
top_agencies <- x %>%
   group_by(agency_name) %>%
   summarize(total_expenditures = sum(expenditures_fy21_000_000, na.rm = TRUE)) %>%
   arrange(desc(total_expenditures)) %>%
   mutate(pct_of_total = total_expenditures/sum(total_expenditures),
          cumulative_pct = cumsum(pct_of_total),
          rank = row_number()) 

##Simple chart
top_agencies_chart <- top_agencies %>%
    filter(cumulative_pct <=0.99) %>%
    ggplot(aes(x = reorder(str_to_title(agency_name),total_expenditures)
               ,y = total_expenditures)) + 
    geom_col() + 
    coord_flip() + 
    labs(y = "Total expenditures in FY 2021 (in U$1,000)",
         x = "Agency name\n",
         title = "Total expenditures by agency in FY 2021",
         subtitle = "Agencies representing 99% of total expenditures",
         caption = "There were other 16 NYC agencies that represent 1% of total expenditures") + 
    scale_y_continuous(labels = scales::comma)
top_agencies_chart

##Top 15 makes 90% of total. Grabbing these to make visualizatione easier
top_99_agencies <- top_agencies %>% filter(cumulative_pct <=0.99) %>%
    pull(agency_name)

# -------------------------------------------------------------------------
#NYC HHC has salary missing
pct_otps_ps <- x %>%
    filter(agency_name %in% top_99_agencies) %>%
    mutate(agency_name = str_to_title(agency_name)) %>%
    group_by(agency_name, ps_or_otps) %>%
    summarize(
        total_expenditures = sum(expenditures_fy21_000_000, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    spread(ps_or_otps,total_expenditures, fill = NA) %>%
    rename(other_than_personal_services = 2,
           personal_services = 3) %>%
    mutate(pct_personal_services = personal_services/(personal_services + other_than_personal_services),
           pct_other_than_personal_services = other_than_personal_services/(personal_services + other_than_personal_services)) %>%
    drop_na()

reorder_rank <- top_agencies %>%
    mutate(agency_name = str_to_title(agency_name)) %>%
    dplyr::select(agency_name, rank)

pct_devoted_to_each <- pct_otps_ps %>%
    select(1,4,5) %>%
    gather(expenditure_type, pct,-agency_name) %>%
    mutate(expenditure_type = ifelse(expenditure_type == "pct_personal_services",
                                     "Personal Services", "Other than Personal Services")) %>%
    inner_join(reorder_rank) %>%
    ggplot(aes(x = reorder(agency_name,length(top_99_agencies)-rank)
               , y = pct
               , fill = expenditure_type)) + 
    geom_col(color = "black") + 
    coord_flip() + 
    labs(x = "Agency\n", y = "% of Total Expenditures",
         fill = "Expenditure type",
         title = "Proportion of expenditures allocated between PS and OTPS for selected NYC agencies",
         subtitle = "Agencies that comprise 99% of city expenditures inn FY 2021\nSorted by Total Expenditures",
         caption = "NYC Health + Hospitals dropped due to missingness") + 
    scale_y_continuous(labels = scales::percent) + 
    theme(
        legend.position = "bottom",
    ) + 
    scale_fill_manual(values = c("orange", "lightblue")) + 
    guides(fill = guide_legend(title.position = "top",
                               title.hjust = 0.5,
                               nrow = 2)) + 
    geom_hline(yintercept = 0.75,linetype = 2) + 
    geom_hline(yintercept = 0.5, linetype = 2) + 
    geom_hline(yintercept = 0.25, linetype = 2)
pct_devoted_to_each    
 


