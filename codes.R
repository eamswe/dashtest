library('tidyverse')
library('janitor')
library('zoo')
library('openxlsx')
library('httr')
library('ISOweek')
fnrollsuml <- function (x) {
  if (length(x) < 14) {
    rep(NA,length(x)) 
  } else {
    rollsum(x,14,align="right",na.pad=TRUE)
  }
}
mobility <- read.csv(url("https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_countries.csv"))
mobility <- clean_names(mobility)

mobility_IE <- subset(mobility, country=="Ireland" & region=="Total",
                      select=c(date, retail_and_recreation, grocery_and_pharmacy, transit_stations, workplaces, residential))


mobility_IE <- mobility_IE %>% mutate(retail_recreation = zoo::rollmeanr(retail_and_recreation, k = 7, fill = NA),
                                      grocery_pharmacy = zoo::rollmeanr(grocery_and_pharmacy, k = 7, fill = NA),
                                      public_transport = zoo::rollmeanr(transit_stations, k = 7, fill = NA),
                                      workplaces = zoo::rollmeanr(workplaces, k = 7, fill = NA),
                                      residential = zoo::rollmeanr(residential, k = 7, fill = NA)) 
mob_IE <- subset(mobility_IE, 
                 select=c(date, retail_recreation, grocery_pharmacy, public_transport, workplaces, residential))
mob_IE$date <- as.Date(mob_IE$date , format = "%Y-%m-%d")

write.csv(mob_IE[,c("date","retail_recreation","grocery_pharmacy", "public_transport", "workplaces", "residential" )], file="mobility.csv",row.names=TRUE)

##### ***EU 14 day incicdence****

ECDCcovid <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
ECDCcovid <- clean_names(ECDCcovid)
ECDCcovid$date_rep <- as.Date(ECDCcovid$date_rep , format = "%d/%m/%y")
#Only EU MSs
COVID_EU_pop <- subset(ECDCcovid, geo_id=="IE" | geo_id=="BE" | geo_id=="BG" | geo_id=="CZ" | geo_id=="DK" | geo_id=="DE" | geo_id=="EE" | geo_id=="EL" | geo_id=="ES" | geo_id=="FR" | geo_id=="HR" | geo_id=="IT" | geo_id=="CY" | geo_id=="LV" | geo_id=="LT" | geo_id=="LU" | geo_id=="HU" | geo_id=="MT" | geo_id=="NL" | geo_id=="AT" | geo_id=="PL" | geo_id=="PT" | geo_id=="RO" | geo_id=="SI" | geo_id=="SK" | geo_id=="FI" | geo_id=="SE" | geo_id=="UK",
                       select=c(date_rep, countries_and_territories, pop_data2019))
#We only use one date because all we want from this data is the population
COVID_EU_pop <- subset(COVID_EU_pop, date_rep == "2020-12-14")

#Rename to match JHU names
COVID_EU_pop <- COVID_EU_pop %>% 
  rename(
    country = countries_and_territories,
  )
COVID_EU_pop <- COVID_EU_pop %>% 
  mutate(country = ifelse(as.character(country) == "United_Kingdom", "United.Kingdom", as.character(country)))
#Download JHU data from OWID
JHU_covid <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_cases.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
JHU_covid <- JHU_covid %>% gather(country, cases, -c(date))
JHU_covid <- merge(COVID_EU_pop, JHU_covid, by ="country")
JHU_covid$date_rep <- NULL
#calculate 14 incidence
JHU_covid <- JHU_covid %>%
  group_by(country) %>%
  mutate(pop100k = pop_data2019/100000)
JHU_covid$date <- as.Date(JHU_covid$date , format = "%Y-%m-%d")
JHU_covid <- arrange(JHU_covid, date)
JHU_covid <- JHU_covid %>% 
  group_by(country)%>%
  mutate(cumcases=fnrollsuml(cases))
JHU_covid <- JHU_covid %>% 
  mutate(incidence_14day_per_100k = cumcases/pop100k)
write.csv(JHU_covid, file="eu_tidy_cases.csv",row.names=TRUE)

spread_JHU <- spread(JHU_covid, country, incidence_14day_per_100k)
#write CSV with the output data for our use
write.csv(spread_JHU, file="eu_cases.csv",row.names=TRUE)
JHU_covid_ie <- subset(JHU_covid, country=="Ireland",
                    select=c(date, incidence_14day_per_100k))
write.csv(JHU_covid_ie, file="ie_cases.csv",row.names=TRUE)


#####ECDC
ECDCVAX <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
ECDCVAX <- clean_names(ECDCVAX)
ECDCVAX$week <- paste(ECDCVAX$year_week_iso,"7", sep="-")
ECDCVAX$week <- ISOweek2date(ECDCVAX$week)
#Only whole countries- no regions 
ECDC_vax <- ECDCVAX %>% 
  group_by(reporting_country) %>%
  filter(target_group=="ALL") %>% 
  subset(reporting_country == region)

ECDC_dose1 <- ECDC_vax %>% 
  group_by(reporting_country) %>% 
  subset(vaccine =  "COM" | "MOD" | "AZ") %>% 
  summarise(dose1 = sum(first_dose))

ECDC_dose2 <- ECDC_vax %>% 
  group_by(reporting_country) %>% 
  subset(vaccine ="COM" | "MOD" | "AZ") %>% 
  summarise(dose2 = sum(second_dose))
ECDC_jans <- ECDC_vax %>% 
  group_by(reporting_country) %>% 
  subset(vaccine =="JANSS") %>% 
  summarise(janss = sum(first_dose))
#adding the janssen with the two dose jabs to create a fully vaxxed number
ECDC_full <- merge(ECDC_jans, ECDC_dose2,by="reporting_country")
ECDC_full <- ECDC_full %>% 
  mutate(fully_vaccinated = janss + dose2)
vars <- c("reporting_country", "fully_vaccinated")
ECDC_fully <- ECDC_full[vars]
doses<- merge(ECDC_dose1, ECDC_fully, by="reporting_country")
#extracting population data
ECDC_pop <- ECDC_vax %>% 
  group_by(reporting_country) %>%
  summarise(population = mean(population)) 
doses<- merge(doses, ECDC_pop, by="reporting_country")
write.csv(doses, file="eu_doses.csv",row.names=TRUE)

#### Stringency

ox <- read.csv(url("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/timeseries/stringency_index.csv"))
#subset: only interested in some countries
ox_comp<- subset(ox, country_code=="IRL" | country_code=="GBR" | country_code=="ESP" | country_code=="ITA" | country_code=="SWE" | country_code=="USA" | country_code=="FRA" | country_code=="DEU", select = -c(country_name, X))
ox_comp1 <- data.frame(t(ox_comp[-1]))
colnames(ox_comp1) <- ox_comp[, 1]
ox_comp1$date <- rownames(ox_comp1)

ox_comp1$date <-as.Date(ox_comp1$date, format = "X%d%b%Y")
write.csv(doses, file="stringency.csv",row.names=TRUE)

### deaths

JHU_deaths <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
JHU_deaths <- JHU_deaths %>% gather(country, deaths, -c(date))
JHU_deaths <- merge(COVID_EU_pop, JHU_deaths, by ="country")
JHU_deaths$date_rep <- NULL

JHU_deaths <- JHU_deaths %>%
  group_by(country) %>%
  mutate(pop100k = pop_data2019/100000)
JHU_deaths$date <- as.Date(JHU_deaths$date , format = "%Y-%m-%d")
JHU_deaths <- arrange(JHU_deaths, date)
JHU_deaths <- JHU_deaths %>% 
  group_by(country)%>%
  mutate(cumdeaths=fnrollsuml(deaths))
JHU_deaths <- JHU_deaths %>% 
  mutate(deaths_14day_per_100k = cumdeaths/pop100k)
spread_deaths <- subset(JHU_deaths,
                        select=c(country, date, deaths_14day_per_100k))
spread_deaths <- spread(spread_deaths, country, deaths_14day_per_100k)
write.csv(spread_deaths, file="deaths.csv", row.names=TRUE)

JHU_deaths <- subset(JHU_deaths, country=="Ireland",
                     select=c(date, deaths_14day_per_100k))
write.csv(JHU_deaths, file="deaths_ie.csv", row.names=TRUE)




indeed <- read.csv(url("https://raw.githubusercontent.com/hiring-lab/data/master/IE/aggregate_job_postings_IE.csv"))
indeed <-  spread(indeed, variable, pct_chng_feb_1)
indeed$date <- as.Date(indeed$date , format = "%Y-%m-%d")
indeed <- clean_names(indeed)
write.csv(indeed, file="indeed_IE.csv",row.names=TRUE)


url_cbi <- "https://www.centralbank.ie/docs/default-source/statistics/data-and-analysis/credit-and-banking-statistics/credit-and-debit-card-statistics/credit-and-debit-card-statistics-data/table-a-13-2-daily-card-payments-data.xlsx"
cbi <- read.xlsx(url_cbi, startRow = 5)
cbi$X1 <- convertToDate(cbi$X1, origin = "1900-01-01")
cbi <- clean_names(cbi)
cbi <- cbi %>% 
  rename(
    date = x1,
  )
cbi_total <- subset(cbi, select = c (date, spending_on_all_cards))  
cbi_total <- cbi_total %>% mutate(spend_ma = zoo::rollmean(spending_on_all_cards, k = 7, fill = NA, align = c("right")))
cbi_total <- cbi_total %>% mutate(cbi_index = (spend_ma/spend_ma[7])*100)
cbi_total <-  cbi_total %>%
  filter(date > "2020-03-06") 
write.csv(cbi_total, file = "cbi_total_spending.csv", row.names =TRUE)

cbi_online <- cbi %>% 
  rename(
    online = o_w_online_spending_card_not_present,
    offline = o_w_in_store_spending_card_present,
    atm = o_w_atm_withdrawals
  )
cbi_online$online <- as.numeric(cbi_online$online)
cbi_online$offline <- as.numeric(cbi_online$offline)
cbi_online$atm <- as.numeric(cbi_online$atm)
cbi_online <- cbi_online %>% mutate(spend_online = zoo::rollmean(online, k = 7, fill = NA, align = c("right"))) %>%
  mutate(spend_pos = zoo::rollmean(offline, k = 7, fill = NA, align = c("right"))) %>%
  mutate(spend_atm = zoo::rollmean(atm, k = 7, fill = NA, align = c("right"))) 



cbi_online <-  cbi_online %>%
  filter(date > "2020-10-01") 
cbi_online <- cbi_online %>%
  group_by(date) %>%
  mutate(total_cat = spend_online + spend_pos + spend_atm)
cbi_online <- cbi_online %>%
  mutate(Online_proportion = (spend_online/total_cat)*100) %>%
  mutate(ATM_proportion = (spend_atm/total_cat)*100) %>%
  mutate(POS_proportion = (spend_pos/total_cat)*100)
cbi_online <-  cbi_online %>%
  filter(date > "2020-10-06")
cbi_online <-  subset(cbi_online, select = c(date, Online_proportion, ATM_proportion, POS_proportion))  
cbi_online$date <- as.Date(cbi_online$date, format="%Y-%m-%d")
write.csv(cbi_online, file = "cbi_online.csv", row.names =TRUE)

cbi_cats <-  cbi %>%
  filter(date > "2020-10-01")

cbi_cats$groceries_perishables <- as.numeric(cbi_cats$groceries_perishables)
cbi_cats$other_retail <- as.numeric(cbi_cats$other_retail)
cbi_cats$transport <- as.numeric(cbi_cats$transport)
cbi_cats$accommodation <- as.numeric(cbi_cats$accommodation)
cbi_cats$restaurants_dining <- as.numeric(cbi_cats$restaurants_dining)


cbi_cats <- cbi_cats %>% mutate(other_retail_ma = zoo::rollmeanr(other_retail, k = 7, fill = NA),
                                transport_ma = zoo::rollmeanr(transport, k = 7, fill = NA),
                                accommodation_ma = zoo::rollmeanr(accommodation, k = 7, fill = NA),
                                restaurants_dining_ma = zoo::rollmeanr(restaurants_dining, k = 7, fill = NA),
                                groceries_perishables_ma = zoo::rollmeanr(groceries_perishables, k = 7, fill = NA))

cbi_cats <-  cbi_cats %>%
  filter(date > "2020-10-07")

cbi_cats <- cbi_cats %>% 
  mutate(Other_retail = (other_retail_ma/other_retail_ma[1])*100) %>% 
  mutate(Accommodation = (accommodation_ma/accommodation_ma[1])*100)%>%
  mutate(Transport = (transport_ma/transport_ma[1])*100) %>%
  mutate(Restaurants_dining = (restaurants_dining_ma/restaurants_dining_ma[1])*100) %>% 
  mutate(Groceries_perishables = (groceries_perishables_ma/groceries_perishables_ma[1])*100)

cbi_cats <- subset(cbi_cats, select = c(date, Other_retail, Accommodation, Transport, Restaurants_dining, Groceries_perishables))  
write.csv(cbi_cats, file = "cbi_cats.csv", row.names =TRUE)

fnrollsuml <- function (x) {
  if (length(x) < 7) {
    rep(NA,length(x)) 
  } else {
    rollsum(x,7,align="right",na.pad=TRUE)
  }
}


JHU_covid <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_cases.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
JHU_covid <- JHU_covid %>% gather(country, cases, -c(date))

JHU_covid$date <- as.Date(JHU_covid$date , format = "%Y-%m-%d")
JHU_covid <- arrange(JHU_covid, date)
JHU_covid <- JHU_covid %>% 
  group_by(country)%>%
  mutate(cumcases=fnrollsuml(cases))
JHU_covid <- JHU_covid %>% 
  mutate(incidence_14day_per_100k = cumcases/7)

JHU_covid1 <- subset(JHU_covid, country=="Ireland",
                     select=c(date, cases))
JHU_covid1[is.na(JHU_covid1)] <- 0


JHU_covid <- subset(JHU_covid, country=="Ireland",
                    select=c(date, incidence_14day_per_100k))


JHU_IE <-JHU_covid


JHU_IE <- JHU_IE %>% 
  rename(
    "7 day average cases" = incidence_14day_per_100k
  )
write.csv(JHU_ie, file = "ie_7d_cases.csv", row.names =TRUE)

JHU_deaths<- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths_per_million.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
JHU_deaths <- JHU_deaths %>% gather(country, deaths_per_mill, -c(date))
JHU_deaths <- merge(COVID_EU_pop, JHU_deaths, by ="country")
JHU_deaths$date_rep <- NULL


JHU_deaths$date <- as.Date(JHU_deaths$date , format = "%Y-%m-%d")
JHU_deaths <- arrange(JHU_deaths, date)
JHU_deaths<- JHU_deaths %>% 
  group_by(country)%>%
  mutate(day_14_deaths_per_mill=fnrollsuml(deaths_per_mill))

spread_deaths <- subset(JHU_deaths,
                        select=c(country, date, day_14_deaths_per_mill))
spread_deaths <- spread(spread_deaths, country, day_14_deaths_per_mill)
write.csv(spread_deaths, file = "deaths_per_mill.csv", row.names =TRUE)

COVID_EU_pop <- subset(ECDCcovid, geo_id=="IE" | geo_id=="BE" | geo_id=="CZ" | geo_id=="DK" | geo_id=="DE" | geo_id=="EL" | geo_id=="ES" | geo_id=="FR" | geo_id=="HR" | geo_id=="IT" | geo_id=="NL" | geo_id=="AT" | geo_id=="PT"| geo_id=="FI" | geo_id=="SE" | geo_id=="UK",
                       select=c(date_rep, countries_and_territories, pop_data2019))
COVID_EU_pop <- subset(COVID_EU_pop, date_rep == "2020-12-14")

COVID_EU_pop <- COVID_EU_pop %>% 
  rename(
    country = countries_and_territories,
  )
ECDCcovidhospital <- read.csv("https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
ECDCcovidhospital <- merge(COVID_EU_pop, ECDCcovidhospital, by ="country")
ECDC_hospital_occupancy <- subset(ECDCcovidhospital, indicator == "Daily hospital occupancy")
ECDC_hospital_occupancy  <- ECDC_hospital_occupancy  %>% 
  mutate(pop100k=pop_data2019/100000)
ECDC_hospital_occupancy  <- ECDC_hospital_occupancy  %>% 
  mutate(occupancy_per_100k=value/pop100k)
ECDC_hospital_occupancy<- subset(ECDC_hospital_occupancy, select=c(country, date, occupancy_per_100k))
spread_hosp <- spread(ECDC_hospital_occupancy, country, occupancy_per_100k)
spread_hosp$date <- as.Date(spread_hosp$date)
write.csv(spread_hosp, file="hosp.csv")


ECDC_ICU_occupancy <- subset(ECDCcovidhospital, indicator == "Daily ICU occupancy")
ECDC_ICU_occupancy  <- ECDC_ICU_occupancy  %>% 
  mutate(pop100k=pop_data2019/100000)
ECDC_ICU_occupancy  <- ECDC_ICU_occupancy  %>% 
  mutate(ICUoccupancy_per_100k=value/pop100k)
ECDC_ICU_occupancy<- subset(ECDC_ICU_occupancy, select=c(country, date, ICUoccupancy_per_100k))
spread_ICU <- spread(ECDC_ICU_occupancy, country, ICUoccupancy_per_100k)
spread_ICU$date <- as.Date(spread_ICU$date)
write.csv(spread_hosp, file="ICU_occupancy.csv")


testing <- read.csv(url("https://github.com/owid/covid-19-data/raw/master/public/data/testing/covid-testing-all-observations.csv"))
testing <- clean_names(testing)
testing <- subset(testing, iso_code=="AUT" | iso_code=="BEL" | iso_code=="CZE" | iso_code=="DNK" | iso_code=="EST" | iso_code=="FIN" | iso_code=="FRA" | iso_code=="DEU" | iso_code=="IRL" | iso_code=="ITA"| iso_code=="NLD" | iso_code=="PRT" | iso_code=="ESP" | iso_code=="SWE" | iso_code=="GBR")
testing <-  subset(testing, entity!="France - people tested")
testing <-  subset(testing, entity!="Italy - people tested")
testing <-  subset(testing, entity!="Poland - people tested")
testing$date <- as.Date(testing$date , format = "%Y-%m-%d")
testing <- testing %>% group_by(iso_code) %>%
  mutate(change_in_cumulative_total_per_thousand_ma = zoo::rollmeanr(daily_change_in_cumulative_total_per_thousand, 7, fill = NA))
testing <- testing %>% group_by(iso_code) %>%
  mutate(positivity_rate_ma = zoo::rollmeanr(short_term_positive_rate, 7, fill = NA))
testing_tests_per_thousand <- subset(testing, select=c(date, iso_code, x7_day_smoothed_daily_change_per_thousand))
testing_tests_per_thousand <- spread(testing_tests_per_thousand, iso_code, x7_day_smoothed_daily_change_per_thousand)
write.csv(testing_tests_per_thousand, file="tests_per_k.csv")
testing_positive_rate <- subset(testing, select=c(date, iso_code, positivity_rate_ma))
testing_positive_rate <- spread(testing_positive_rate, iso_code, positivity_rate_ma)
write.csv(testing_positive_rate, file="tests_positivity.csv")


ECDCVAX <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
ECDCVAX <- clean_names(ECDCVAX)
ECDCVAX$week <- paste(ECDCVAX$year_week_iso,"1", sep="-")
ECDCVAX$week <- ISOweek2date(ECDCVAX$week)
ECDC_vax <- ECDCVAX %>% 
  group_by(reporting_country) %>%
  filter(target_group=="ALL") %>% 
  subset(reporting_country == region)

ECDC_dose1 <- ECDC_vax %>% 
  group_by(reporting_country) %>% 
  subset(vaccine =  "COM" | "MOD" | "AZ") %>% 
  summarise(dose1 = sum(first_dose))

ECDC_dose2 <- ECDC_vax %>% 
  group_by(reporting_country) %>% 
  subset(vaccine ="COM" | "MOD" | "AZ") %>% 
  summarise(dose2 = sum(second_dose))
ECDC_jans <- ECDC_vax %>% 
  group_by(reporting_country) %>% 
  subset(vaccine =="JANSS") %>% 
  summarise(janss = sum(first_dose))

ECDC_full <- merge(ECDC_jans, ECDC_dose2,by="reporting_country")
ECDC_full <- ECDC_full %>% 
  mutate(fully_vaccinated = janss + dose2)
vars <- c("reporting_country", "fully_vaccinated")
ECDC_fully <- ECDC_full[vars]
doses<- merge(ECDC_dose1, ECDC_fully, by="reporting_country")

ECDC_pop <- ECDC_vax %>% 
  group_by(reporting_country) %>%
  summarise(population = mean(population))
doses<- merge(doses, ECDC_pop, by="reporting_country")
doses <- doses %>% 
  group_by(reporting_country) %>% 
  mutate(partially_vaccinated = dose1/population*100) %>% 
  mutate(partially_vaccinated = round(partially_vaccinated, 2)) %>% 
  mutate(fully_vaccinated = fully_vaccinated/population*100) %>% 
  mutate(fully_vaccinated = round(fully_vaccinated, 2)) 
vars <- c("reporting_country", "fully_vaccinated", "partially_vaccinated")
ECDC_doses <- doses[vars]
ECDC_doses <- ECDC_doses %>% mutate( ToHighlight = ifelse( reporting_country == "IE", "yes", "no" ) )
ECDC_doses <- ECDC_doses %>% 
  ungroup() %>% 
  mutate(position = rank(fully_vaccinated))
ECDC_doses_l <- ECDC_doses %>% gather(statistic, value, -c(reporting_country, ToHighlight, position))
write.csv(ECDC_doses_l, file="ECDC_vax_1")

ECDC_IE <- ECDCVAX %>% 
  group_by(target_group) %>% 
  subset(reporting_country ==  "IE") %>% 
  subset(vaccine =  "COM" | "MOD" | "AZ") %>%
  summarise(partially_vaccinated = sum(first_dose)) 

ECDC_IE <- filter(ECDC_IE, target_group == "Age15_17" | target_group == "Age18_24" | target_group == "Age25_49" |target_group == "Age50_59"|target_group == "Age60_69" |target_group == "Age70_79" |target_group == "Age80+")

ECDC_IE_2 <- ECDCVAX %>% 
  group_by(target_group) %>% 
  subset(reporting_country ==  "IE") %>% 
  subset(vaccine =  "COM" | "MOD" | "AZ") %>%
  summarise(dose2 = sum(second_dose)) 

ECDC_IE_2 <- filter(ECDC_IE_2, target_group == "Age15_17" | target_group == "Age18_24" | target_group == "Age25_49" |target_group == "Age50_59"|target_group == "Age60_69" |target_group == "Age70_79" |target_group == "Age80+")

ECDC_IE_j <- ECDCVAX %>% 
  group_by(target_group) %>% 
  subset(reporting_country ==  "IE") %>% 
  subset(vaccine ==  "JANSS") %>%
  summarise(dose = sum(first_dose)) 

ECDC_IE_j <- filter(ECDC_IE_j, target_group == "Age15_17" | target_group == "Age18_24" | target_group == "Age25_49" |target_group == "Age50_59"|target_group == "Age60_69" |target_group == "Age70_79" |target_group == "Age80+")

ECDC_IE_full <-  inner_join(ECDC_IE_j, ECDC_IE_2, by = "target_group")
ECDC_IE_full <- mutate(ECDC_IE_full, fully_vaccinated= dose +dose2)
ECDC_IE_full <- subset(ECDC_IE_full, select=c(target_group, fully_vaccinated))

ECDC_pop_IE <- ECDCVAX %>% 
  group_by(target_group) %>%
  subset(reporting_country=="IE") %>% 
  summarise(denominator = mean(denominator)) 

ECDC_IE_p <- inner_join(ECDC_pop_IE, ECDC_IE, by="target_group")
ECDC_IE_p <- inner_join(ECDC_IE_p, ECDC_IE_full, by="target_group")
age_props <- ECDC_IE_p %>% 
  mutate(fully_vaccinated = fully_vaccinated/denominator*100) %>% 
  mutate(partially_vaccinated = partially_vaccinated/denominator*100) %>% 
  mutate(fully_vaccinated = round(fully_vaccinated, 2)) %>% 
  mutate(partially_vaccinated = round(partially_vaccinated, 2))
age_props_l <- age_props %>% gather(group, proportion, -c(target_group, denominator))
write.csv(age_props_l, file="ECDC_vax_2")

ECDC_cumulative <- ECDCVAX %>% 
  subset(reporting_country ==  "IE") %>% 
  subset(target_group == "ALL") %>% 
  mutate(vaccines_administered = first_dose + second_dose) %>% 
  group_by(vaccine) %>% 
  mutate(total = cumsum(vaccines_administered)) 
ECDC_cumulative$vaccine <- gsub('COM', 'Pfizer', ECDC_cumulative$vaccine)
ECDC_cumulative$vaccine <- gsub('JANSS', 'J&J', ECDC_cumulative$vaccine)
ECDC_cumulative$vaccine <- gsub('MOD', 'Moderna', ECDC_cumulative$vaccine)
ECDC_cumulative$vaccine <- gsub('UNK', 'Unknown', ECDC_cumulative$vaccine)
write.csv(ECDC_cumulative, file="ECDC_vax_3")



