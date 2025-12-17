##Step 1:Load needed packages
library(dplyr)
library(ggplot2)
library(janitor)
library(kableExtra)
library(knitr)
library(quarto)
library(readxl)
library(rvest)
library(tidyr)
library(tidyverse)

###Ella's Code
##Step 2: Load and clean up the needed data
#Price Index is based on percent change
ConsumerPrice <- read_xlsx(
  path = "C:/Users/ellan/OneDrive/Documents/ConsumerPriceData.xlsx") %>%
  rename("Consumer Price" = Annual)

ProducerExtractionPrice <- read_xlsx(
  path = "C:/Users/ellan/OneDrive/Documents/OilExtractionCost.xlsx") %>%
  rename("Average Extraction Cost" = Annual)

ProducerTransportPrice <- read_xlsx(
  path = "C:/Users/ellan/OneDrive/Documents/ProducerTransportPrice.xlsx") %>%
  rename("Average Transport Cost" = Annual)

ProducerMachinePrice <- read_xlsx(
  path = "C:/Users/ellan/OneDrive/Documents/ProducerMachinePrice.xlsx") %>%
  rename("Average Machinery Cost" = Annual)

ProducerWellPrice <- read_xlsx(
  path = "C:/Users/ellan/OneDrive/Documents/ProducerWellPrice.xlsx") %>%
  rename("Average Oil Well Cost" = Annual)


##Step 3: Create new data table to show yearly averages for each producer cost
ProducerPriceIndex <- bind_cols( #combine producer data tables
                           ProducerExtractionPrice, 
                           ProducerMachinePrice,
                           ProducerTransportPrice,
                           ProducerWellPrice) %>%
  dplyr::select( #keep only the yearly average columns
                1, 
                "Average Oil Well Cost", 
                "Average Machinery Cost", 
                "Average Extraction Cost", 
                "Average Transport Cost") %>%
  rename( #make the Year column easier to read
    Year = "Year...1") %>%
  pivot_longer(
    cols = c("Average Oil Well Cost", 
             "Average Machinery Cost", 
             "Average Extraction Cost", 
             "Average Transport Cost"),
    names_to = "Variable",
    values_to = "Change")

ConsumerVSExtraction <- bind_cols( #combine producer data tables
  ProducerExtractionPrice, 
  ConsumerPrice) %>%
  dplyr::select( #keep only the yearly average columns
    1, 
    "Consumer Price", 
    "Average Extraction Cost") %>%
  rename( #make the Year column easier to read
    Year = "Year...1") %>%
  pivot_longer(
    cols = c("Consumer Price", 
             "Average Extraction Cost"),
    names_to = "Variable",
    values_to = "Change")

##Step 4: Make graphics for each table, and one graph contrasting each variable
#Producer costs over the years
ggplot(ProducerPriceIndex, aes(Year, Change, colour = Variable)) + 
         geom_line(linewidth = 1.05) + 
  scale_color_manual(values=c('#4169E1', '#A020F0', '#FA8072', '#4EEE94')) +
  theme_gray() +
  labs(dictionary = c(
    Variable = "Cost Type",
    Change = "Percent Change"),
    title = "Percent Change In Production Cost of Oil Over Time",
    subtitle = "2005-2024") + 
  facet_wrap(. ~ Variable)

#Consumer costs over the year
ggplot(ConsumerPrice, aes(Year, `Consumer Price`)) + 
  geom_line(linewidth = 1, color = '#EE2C2C') +
  theme_gray() +
  labs( dictionary = c(
    `Consumer Price` = "Percent Change"),
    title = "Percent Change in Consumer Price of Oil Over Time",
    subtitle = "2005-2024")

ggplot(ConsumerVSExtraction, aes(Year, Change, colour = Variable)) + 
  geom_line(linewidth = 1) + 
  scale_color_manual(values=c('#4169E1', '#EE2C2C')) +
  theme_gray() +
  labs(dictionary = c(
    Variable = "Cost Type",
    Change = "Percent Change"),
    title = "Percent Change In Extraction Cost Vs. Consumer Price",
    subtitle = "2005-2024")





###Connor's Code
## Helper function:
## Reads a table, extracts annual values, and converts to percent change
load_table <- function(path, value_name) {
  read_xlsx(path) %>%
    clean_names() %>%
    select(year, annual) %>%
    rename(!!value_name := annual) %>%
    mutate(
      !!paste0(value_name, "_pct_change") :=
        (!!sym(value_name) / first(!!sym(value_name)) - 1) * 100
    )
}



## Step 2: Load all tables using the function
consumer_df  <- load_table("C:/Users/AINE/Documents/Re_Github_Repo/ConsumerPriceData.xlsx",
                           "consumer_price")

extract_df   <- load_table("C:/Users/AINE/Documents/Re_Github_Repo/OilExtractionCost.xlsx",
                           "extraction_cost")

transport_df <- load_table("C:/Users/AINE/Documents/Re_Github_Repo/ProducerTransportPrice.xlsx",
                           "transport_cost")

machine_df   <- load_table("C:/Users/AINE/Documents/Re_Github_Repo/ProducerMachinePrice.xlsx",
                           "machine_cost")

well_df      <- load_table("C:/Users/AINE/Documents/Re_Github_Repo/ProducerWellPrice.xlsx",
                           "well_cost")


## Step 3: Join all into a single dataset
combined <- consumer_df %>%
  left_join(extract_df,   by = "year") %>%
  left_join(transport_df, by = "year") %>%
  left_join(machine_df,   by = "year") %>%
  left_join(well_df,      by = "year")


## Step 4: Compute SUM of all production-related % changes
combined <- combined %>%
  mutate(
    production_pct_sum =
      extraction_cost_pct_change +
      transport_cost_pct_change +
      machine_cost_pct_change +
      well_cost_pct_change
  )


## Step 5: Reshape for plotting
plot_df <- combined %>%
  select(year,
         consumer_pct = consumer_price_pct_change,
         production_pct_sum) %>%
  pivot_longer(
    cols = -year,
    names_to = "type",
    values_to = "pct"
  )


## Step 6: Graph comparing consumer % change vs production-sum % change
ggplot(plot_df, aes(x = year, y = pct, color = type)) +
  geom_line(size = 1.2) +
  labs(
    title = "Annual Percentage Change Comparison:\nConsumer Gas Price vs Sum of Production Costs",
    x = "Year",
    y = "Percent Change (%)",
    color = "Category"
  ) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Consumer Gas Price % Change",
                                "Total Production Cost % Change")) +
  theme_minimal(base_size = 14)

## Step 7: Create a comparison table comparing each individual dataset's percentage change
comparison_table <- combined %>%
  select(
    year,
    Consumer = consumer_price_pct_change,
    Extraction = extraction_cost_pct_change,
    Transport = transport_cost_pct_change,
    Machinery = machine_cost_pct_change,
    Well = well_cost_pct_change,
    Production_Sum = production_pct_sum
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "Category",
    values_to = "Percent_Change"
  )

## Step 8: Graph comparing all datasets
ggplot(comparison_table,
       aes(x = year, y = Percent_Change, color = Category)) +
  geom_line(size = 1.2) +
  labs(
    title = "Annual Percentage Change Comparison Across All Cost Categories",
    x = "Year",
    y = "Percent Change (%)",
    color = "Category"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Dark2")
