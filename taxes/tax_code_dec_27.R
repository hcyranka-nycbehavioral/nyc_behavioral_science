remove(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

library(tidyverse);library(RSocrata);library(lubridate)

# Downloading inmate custody ----------------------------------------------
x <- read.socrata("https://data.cityofnewyork.us/resource/hdnu-nbrh.json") %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    arrange(year)

##All of this to clean the data
y <- x %>%
    mutate_at(c(2:14), .funs = ~str_replace_all(.,"\\(","-\\(")) %>%
    mutate_at(c(2:14), .funs = ~str_replace_all(.,"\\(","-")) %>%
    mutate_at(c(2:14), .funs = ~str_replace_all(.,"--","-")) %>%
    mutate_at(c(2:14), .funs = ~str_replace_all(.,"- 0","0")) %>%
    mutate_at(c(2:14), .funs = ~str_replace_all(.,"\\)","")) %>%
    mutate_all(.funs = ~as.numeric(str_replace_all(.,",","")))

# remove transfers --------------------------------------------------------
no_transf <- y %>% dplyr::select(-less_transfers_to_debt_service_funds_and_adjustments,
                                 -total_taxes,
                                 -personal_income_total)

no_transf <- no_transf %>%
    gather(tax_source, total_value,-year)


# Checking overall trajectory --------------------------------------------
overall_trajectory_lines_chart <- no_transf %>%
    mutate(tax_source = str_to_title(str_replace_all(tax_source, "_"," "))) %>%
    ggplot(aes(x = year, y  = total_value, color = tax_source)) + 
    geom_line(size = 1.5) + 
    scale_y_continuous(labels = scales::comma) + 
    scale_color_brewer(palette = "Spectral") + 
    labs(x= "Year", y = "Nominal Dollar Value",
         fill = "Tax source",
         title = "Total tax revenue by source (nominal values)") + 
    theme_minimal() + 
    theme(
        legend.position = "bottom"
    ) + 
    guides(color = guide_legend(title = "Tax source", title.position = "top",
                                title.hjust = 0.5))


# Check average yearly growth by source  --------------------------------------------------
added_indexed_value <- no_transf %>%
    mutate(tax_source = str_to_title(str_replace_all(tax_source, "_"," "))) %>%
    arrange(tax_source, year) %>%
    mutate(change_from_previous_year = total_value/lag(total_value,1) - 1) %>%
    filter(tax_source != "Financial Corporation")

added_indexed_value %>%
    group_by(tax_source) %>%
    summarize(
        average_change = mean(change_from_previous_year, na.rm = TRUE),
        median_change = median(change_from_previous_year, na.rm = TRUE)
    ) %>%
    arrange(desc(average_change))



# Checking the proportion of totals by source -----------------------------
area_chart_for_proportion_1 <- no_transf %>%
    mutate(tax_source = str_to_title(str_replace_all(tax_source, "_"," "))) %>%
    ggplot(aes(x = year, y  = total_value, fill = tax_source)) + 
    geom_area() + 
    scale_y_continuous(labels = scales::comma) + 
    scale_fill_brewer(palette = "Spectral") + 
    theme_minimal() + 
    labs(x= "Year", y = "Nominal Dollar Value",
         fill = "Tax source",
         title = "Total tax revenue by source") + 
    theme(
        legend.position = "bottom"
    )
area_chart_for_proportion_1

added_proportions <- no_transf %>%
    mutate(tax_source = str_to_title(str_replace_all(tax_source, "_"," "))) %>%
    group_by(year) %>%
    mutate(pct_of_total = total_value/sum(total_value))  %>%
    arrange(year, desc(pct_of_total))

added_proportions %>%
    filter(year %in% c(1980, 2020)) %>%
    dplyr::select(1,2,4) %>%
    spread(year, pct_of_total) %>%
    mutate(total_change = `2020` - `1980`) %>%
    arrange(desc(total_change)) %>%
    mutate_at(c(2:4), .funs = ~scales::percent(., accuracy =0.1))

proportion_chart <- added_proportions %>%
    ungroup() %>%
    ggplot(aes(x = year, y = pct_of_total, fill = tax_source)) + 
    geom_area() + 
    scale_y_continuous(labels = scales::comma) + 
    scale_fill_brewer(palette = "Spectral") + 
    theme_minimal() + 
    labs(x= "Year", y = "% of total",
         fill = "Tax source",
         title = "% of total tax revenue by source") + 
    theme(
        legend.position = "bottom"
    ) + 
    guides(fill = guide_legend(title.position = "top", 
                               title.hjust = 0.5))


# Index on first ----------------------------------------------------------
index_on_first <- no_transf %>%
    mutate(tax_source = str_to_title(str_replace_all(tax_source, "_"," "))) %>%
    group_by(tax_source) %>%
    mutate(index_on_first_chage = total_value/first(total_value))  

index_on_first %>%
    filter(tax_source %in% c("Property", "Personal Income General Fund Revenue")) %>%
    ggplot(aes(x = year, y  = index_on_first_chage, color = tax_source)) + 
    geom_line(size = 1.5) + 
    scale_y_continuous(labels = scales::comma) + 
    scale_color_manual(values = c("orange", "green")) + 
    labs(x= "Year", y = "Indexed total (1980 = 1)",
         fill = "Tax source",
         title = "Indexed growth trajectory of Personal Income and Property Taxes",
         subtitle = "Index = 1 in 1980 (first year on dataset)") + 
    theme_minimal() + 
    facet_wrap(~tax_source) + 
    theme(
        legend.position = "bottom"
    ) + 
    guides(color = guide_legend(title = "Tax source", title.position = "top",
                                title.hjust = 0.5))


index_on_first  %>%
    filter(year %in% c(1980, 2020)) %>%
    dplyr::select(1,2,4) %>%
    spread(year, index_on_first_chage)

# -------------------------------------------------------------------------
for_log_change <- index_on_first %>%
    filter(tax_source != "Financial Corporation") %>%
    ungroup() %>%
    group_by(tax_source) %>%
    mutate(lin_trend = row_number())
    
##Using log-lin model gives a similar perspective
lm(log(total_value) ~ lin_trend, data = filter(for_log_change, tax_source == 'Property'))
lm(log(total_value) ~ lin_trend, data = filter(for_log_change, tax_source == 'Personal Income General Fund Revenue'))

# -------------------------------------------------------------------------
bring_deflator <- readxl::read_excel("cpi_u_minneapolis_fed.xlsx")

index_at_1980 <- bring_deflator %>% filter(year == 1980) %>%
    pull(average_cpi_u)

bring_deflator <- bring_deflator %>%
    mutate(cpi_u_1980 = index_at_1980) %>%
    mutate(adjustment = average_cpi_u/cpi_u_1980,
           year = 1913:2020)  

# Real growth -------------------------------------------------------------
real_growth_dataset <- index_on_first %>%
    dplyr::select(1:3) %>%
    inner_join(select(bring_deflator, 1,5)) %>%
    mutate(adjusted_dollars = total_value/adjustment)

real_growth_dataset %>%
    mutate(index_first = adjusted_dollars/first(adjusted_dollars)) %>%
    filter(year == 2020)
    
