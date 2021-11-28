library(tidyverse)
library(lubridate)
library(viridis)

#pull in public data from BLS website
raw_data <- 
  #Total nonfarm"
  read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.00a.TotalNonfarm.Employment', col_type = 'c') %>% 
  #"Total private",
  full_join(read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.05a.TotalPrivate.Employment', col_type = 'c')) %>%
  #"Mining and logging",
    full_join( read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.10a.MiningAndLogging.Employment', col_type = 'c')) %>%
  #"Construction",
    full_join( read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.20a.Construction.Employment', col_type = 'c')) %>%
  #"Manufacturing",
  full_join( read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.30a.Manufacturing.Employment', col_type = 'c') )%>%
  #Durable Goods",
  full_join(  read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.31a.ManufacturingDurableGoods.Employment', col_type = 'c')) %>%
  #"Nondurable Goods",
  full_join( read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.32a.ManufacturingNondurableGoods.Employment', col_type = 'c')) %>%
  #"Trade, transportation, and utilities",
  full_join(read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.40a.TradeTransportationAndUtilities.Employment', col_type = 'c')) %>%
  #Wholesale trade",
  full_join(read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.41a.WholesaleTrade.Employment', col_type = 'c' ))%>%
  #"Retail trade",  
  full_join(read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.42a.RetailTrade.Employment', col_type = 'c' ))%>%
  #Transportation and warehousing and utitlies",
  full_join(read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.43a.TransportationAndWarehousingAndUtilities.Employment', col_type = 'c')) %>%
  #Information",
  full_join(read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.50a.Information.Employment', col_type = 'c')) %>%
  #Financial activities",
  full_join(read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.55a.FinancialActivities.Employment', col_type = 'c')) %>%
  #Professional and business services",
  full_join(read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.60a.ProfessionalBusinessServices.Employment', col_type = 'c')) %>%
  #"Education and health services",
  full_join(read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.65a.EducationAndHealthCare.Employment', col_type = 'c')) %>%
  #"Leisure and hospitality",
  full_join(read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.70a.LeisureAndHospitality.Employment', col_type = 'c')) %>% 
  #Other services",
  full_join(read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.80a.OtherServices.Employment', col_type = 'c')) %>%
  #Government"
  full_join(read_delim('https://download.bls.gov/pub/time.series/ce/ce.data.90a.Government.Employment', col_type = 'c'))
  
#sector category codes
code_mapping <- 
  tribble(~id, ~series_title,
          "CES0000000001",	"Total nonfarm___________________________________",
          "CES0500000001",	"Total private_________________________________",
          "CES0600000001",	"Goods-producing___________________________",
          "CES1000000001",	"Mining and logging___________________",
          "CES2000000001",	"Construction_______________________",
          "CES3000000001",	"Manufacturing______________________",
          "CES3100000001",	"Durable Goods",
          "CES3200000001",	"Nondurable Goods",
           "CES0700000001", "Service-providing__________________________",
          "CES0800000001",	"Private service-providing________________", 
          "CES4000000001",	"Trade, transportation, and utilities_____",
          "CES4100000001",	"Wholesale trade ",
          "CES4200000001",	"Retail trade  ",
          "CES4300000001",	"Transportation and warehousing",
          "CES4400000001",	"Utilities",
          "CES5000000001",	"Information_______________________",
          "CES5500000001",	"Financial activities_________________",
          "CES6000000001",	"Professional and business services___",
          "CES6500000001",	"Education and health services_______",
          "CES7000000001", "Leisure and hospitality______________",
          "CES8000000001",	"Other services_____________________",
          "CES9000000001",	"Government _________________________________")

category_list <- 
  unique(code_mapping$id)

cleaned_data_since_2018 <- 
#clean data
  raw_data %>% 
  #reformat and clean data
  rename(id = `series_id        `, 
         year = `year`,
         month = `period`, 
         value = `       value`) %>% 
  select(id, year, month, value) %>% 
  mutate(id = str_trim(id),
         value = str_sub(value, start = 8, end = -1), 
         value = as.double(value), 
         month = str_sub(month, 2, -1), 
         date = str_c(year, "-", month), 
         date = ym(date)) %>% 
  select(date, id, value) %>%
  filter(id %in% category_list) %>% 
  #merging mapping
    left_join(code_mapping) %>% 
  # select(date, value, series_title) %>%
  #calculating change per month
  group_by(id) %>% 
  mutate(monthly_chg = round(value - lag(value), 1)) %>% 
  ungroup() %>% 
  filter(date >="2018-01-01")
    
#generate dataset since 2021
cleaned_data_since_2021 <-
  cleaned_data_since_2018 %>% 
  filter(date >= "2021-01-01")

factor_list <- 
  unique(code_mapping$series_title)

  cleaned_data_since_2021 %>% 
  mutate(series_title = fct_rev(factor(series_title, levels = factor_list)))  %>% 
  ggplot(aes(x = date, series_title)) +
  geom_tile(aes(fill = monthly_chg)) +
  geom_text(aes(label =  monthly_chg), color = "white") +
  scale_x_date(date_breaks = "1 month", date_label = "%B", 
               expand = c(0, 0)) +
  scale_fill_viridis( option="E")+
  labs(x = "2021", y = "", title = "Change In U.S. Nonfarm Payrolls",
       fill = "Thousands")+
  theme_bw()+
  theme(axis.text = element_text(face = "bold", size = 10),
        # legend.margin = margin(0, 1, 0, -0.5, unit = "cm"),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border  =element_blank())

ggsave("latest_nonfarm_heatmap.png", w = 11, h = 7)

