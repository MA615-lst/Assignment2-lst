library(tidyverse)
library(tidyr)
library(dplyr)
hapi1 <- read.csv("https://raw.githubusercontent.com/MA615-lst/Assignment2-lst/main/hapiscore_whr.csv")
hdi1 <- read.csv("https://raw.githubusercontent.com/MA615-lst/Assignment2-lst/main/hdi_human_development_index.csv")

# pivot_longer(hapi1, year = )
#summary(hapi[,2:5]), check wierd colums
hapi_1 <- hapi1[,-2]

#match "year"
hapi_s <- hapi_1[,-15] 
hdi_s <- hdi1[,-(2:17)]

#convert datasets separately
hdi_ss <- gather(hdi_s,key="year",value="hdi",2:14)
hapi_ss <- gather(hapi_s,key = "year",value = "hapi",2:14)

#match "country"
hdi_sss <- hdi_ss %>%
   semi_join(hapi_ss,by="country")
hapi_sss <- hapi_ss %>%
   semi_join(hdi_ss,by="country")

#check two datasets
a <- group_by(hdi_sss,country)
summarise(a,
          count=n()   )
b <- group_by(hapi_sss,country)
summarise(b,count=n())
##or
hdi_sss %>%
   count(year)
hapi_sss %>%
   count(year)

#construct a tidy tibble
hdi_hapi <- cbind(hdi_sss[,1:3],hapi_sss[,3])
hdi_hapi <- rename(hdi_hapi,"hapi"="hapi_sss[, 3]")
hdi_hapi <- data.frame(country=hdi_hapi$country,year=hdi_hapi$year,hdi=hdi_hapi$hdi,hapi=hdi_hapi$hapi)
hdi_hapi$year <-sub("X","",hdi_hapi$year,fixed = TRUE) 
