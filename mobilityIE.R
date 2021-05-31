library('tidyverse')
library('janitor')
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
