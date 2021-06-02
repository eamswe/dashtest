library('tidyverse')
library('janitor')
library('zoo')
library('openxlsx')
library('httr')
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
