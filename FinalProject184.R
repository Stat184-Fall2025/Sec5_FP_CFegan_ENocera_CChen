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


##Step 2: Load and clean up the needed data
#Consumer Index is based on dollars-per-gallon
ConsumerPrice <- read_xlsx(path = "C:/Users/ellan/OneDrive/Documents/ConsumerPriceData.xlsx") %>%
  rename( #rename "annual" to make it more recognizable when data tables are combined
    "Average Consumer Price" = Annual)

#Producer Index is based on cents-per-gallon, so change that to match consumer index and make more sense to audience
ProducerExtractionPrice <- read_xlsx(path = "C:/Users/ellan/OneDrive/Documents/OilExtractionCost.xlsx") %>%
  rename("Average Extraction Cost" = Annual) %>%
  mutate_if(is.numeric, ~ . / 100) %>%
  mutate(Year = Year * 100)

ProducerTransportPrice <- read_xlsx(path = "C:/Users/ellan/OneDrive/Documents/ProducerTransportPrice.xlsx") %>%
  rename("Average Transport Cost" = Annual)%>%
  mutate_if(is.numeric, ~ . / 100) %>%
  mutate(Year = Year * 100)

ProducerMachinePrice <- read_xlsx(path = "C:/Users/ellan/OneDrive/Documents/ProducerMachinePrice.xlsx") %>%
  rename("Average Machinery Cost" = Annual)%>%
  mutate_if(is.numeric, ~ . / 100) %>%
  mutate(Year = Year * 100)

ProducerWellPrice <- read_xlsx(path = "C:/Users/ellan/OneDrive/Documents/ProducerWellPrice.xlsx") %>%
  rename("Average Oil Well Cost" = Annual)%>%
  mutate_if(is.numeric, ~ . / 100) %>%
  mutate(Year = Year * 100)


##Step 3: Create new data table to show yearly averages for each table
OilPriceIndex <- bind_cols( #combine all of the data tables
                           ConsumerPrice, 
                           ProducerExtractionPrice, 
                           ProducerMachinePrice,
                           ProducerTransportPrice,
                           ProducerWellPrice) %>%
  dplyr::select( #keep only the yearly average columns
                1, 
                "Average Consumer Price", 
                "Average Oil Well Cost", 
                "Average Machinery Cost", 
                "Average Extraction Cost", 
                "Average Transport Cost") %>%
  rename( #make the Year column easier to read
    Year = "Year...1") %>%
  pivot_longer(
    cols = c("Average Consumer Price", 
             "Average Oil Well Cost", 
             "Average Machinery Cost", 
             "Average Extraction Cost", 
             "Average Transport Cost"),
    names_to = "Cost Type",
    values_to = "Cost")

##Step 4: Make graphics for each table, and one graph contrasting each variable
#Consumer price over the years
ggplot(OilPriceIndex, aes(Year, Cost, colour = "Cost Type")) + 
         geom_line(linewidth = 1) + scale_color_manual(values=c('#EE2C2C', '#43CD80', '#4169E1', '#9370DB', 'lightpink1'))

ConsumerPriceGraph <- ConsumerPrice %>%
  dplyr::select(!"Average Consumer Price") %>%
  pivot_longer(
    cols = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    names_to = "Month",
    values_to = "Price") %>%
  group_by(Year)

ggplot(ConsumerPrice, aes(x=Year, y="Average Consumer Price")) + geom_line(aes(group="Price"),linewidth=1, colour = '#EE2C2C')
