## hdi~hapi, show by year
visualization_hdi_hapi_byyear <- function(data,year1,year2){
  #not run:
  # data <- hdi_hapi
  # year1="2005"
  # year2="2017"
  data_1 <- filter(data,year==year1|year==year2)
  ggplot(data_1)+
    geom_point(aes(x=hdi,y=hapi, color = year)) +
    #facet_wrap(~country,nrow=3)
    labs(x="hdi",y="hapi")+
    geom_smooth(aes(x=hdi,y=hapi, color = year), se = FALSE)
  
}
#not run: visualization_hdi_hapi_byyear(hdi_hapi,year1 = "2005",year2 = "2017")

#hdi~hapi, show by country
visualization_hdi_hapi_bycountry <- function(data,country1,country2){
  data_1 <- filter(data,country==country1|country==country2)
  ggplot(data_1)+
    geom_point(aes(x=hdi,y=hapi, color = country)) +
    #facet_wrap(~country,nrow=3)
    labs(x="hdi",y="hapi")+
    geom_smooth(aes(x=hdi,y=hapi, color = country), se = FALSE)
  
}
#not run: visualization_hdi_hapi_bycountry(hdi_hapi,country1 ="China",country2 = "United States")


## year~hdi or year~hapi, show by country
visualization_hdiorhapi_bycountry <- function(data,country1,country2,outcome){
  # data=hdi_hapi
  # country1 = "China"
  # country2 = "United States"
  # outcome="hdi"
if(outcome=="hdi"){
  data_2 <- filter(data,country==country1|country==country2)
  data_2$year <- as.numeric(data_2$year)
  ggplot(data_2)+
    geom_point(aes(x=year,y=hdi,color=country)) +
    geom_line(aes(x=year,y=hdi,color=country))+
    labs(x="year",y="hdi")
}
  if(outcome=="hapi"){
    data_2 <- filter(data,country==country1|country==country2)
    data_2$year <- as.numeric(data_2$year)
  ggplot(data_2)+
    geom_point(aes(x=year,y=hapi,color=country)) +
    geom_line(aes(x=year,y=hapi,color=country))+
    labs(x="year",y="hapi")
  }
 # else{print("input is not working")}
}

visualization_hdiorhapi_bycountry(hdi_hapi,country1 = "China",country2 = "United States",outcome="hdi")
visualization_hdiorhapi_bycountry(hdi_hapi,country1 = "China",country2 = "United States",outcome="hapi")

## contrast worldwide hdi or hapi between 2 different years
visualization_hdiorhapi_contrast <- function(data,year1,year2,outcome){
  # data=hdi_hapi
  # year1 = "2005"
  # year2 = "2017"
  # outcome="hapi"
  # country1="China"
  # country2="United States"
  if(outcome=="hdi"){
    data_2 <- filter(data,year==year1|year==year2)
    data_2 <- data_2[,-4]
    a <- data_2 %>% group_by(year)
    a <- summarise(a,countrymean=mean(hdi,na.rm=TRUE))
    countrymean_byyear_n <- dim(data_2)[1]
    countrymean_byyear <- vector()
    for (i in 1:countrymean_byyear_n){
      if(i<=161){countrymean_byyear[i] <- a[1,2]}
      else{countrymean_byyear[i] <- a[2,2]} 
    }
    ggplot(data_2)+
      geom_bar(aes(year,countrymean_byyear,fill=year),stat="identity")+
      labs(x="year",y="hdi")
  }
  if(outcome=="hapi"){
    data_2 <- filter(data,year==year1|year==year2)
    data_2 <- data_2[,-3]
    a <- data_2 %>% group_by(year)
    a <- summarise(a,countrymean=mean(hapi,na.rm=TRUE))
    countrymean_byyear_n <- dim(data_2)[1]
    countrymean_byyear <- vector()
    for (i in 1:countrymean_byyear_n){
      if(i<=161){countrymean_byyear[i] <- a[1,2]}
      else{countrymean_byyear[i] <- a[2,2]} 
    }
    ggplot(data_2)+
      geom_bar(aes(year,countrymean_byyear,fill=year),stat="identity")+
      labs(x="year",y="hapi")
  }
  # else{print("input is not working")}
}
visualization_hdiorhapi_contrast(hdi_hapi,year1 = "2005",year2 = "2017",outcome="hdi")
visualization_hdiorhapi_contrast(hdi_hapi,year1 = "2005",year2 = "2017",outcome="hapi")

#contrast worldwide hdi or hapi between all years
visualization_hdiorhapi_contrast_total <- function(data,outcome){
  # data <- hdi_hapi
  # outcome <- "hdi"
  if(outcome=="hdi"){
    data_2 <- data
    data_2 <- data_2[,-4]
    a <- data_2 %>% group_by(year)
    a <- summarise(a,countrymean_byyear=mean(hdi,na.rm=TRUE))
    data_2 <- left_join(data_2,a,by="year")
    ggplot(data_2)+
      geom_bar(aes(year,countrymean_byyear,fill=year),stat="identity")+
      labs(x="year",y="hdi")
  }
  if(outcome=="hapi"){
    data_2 <- data
    data_2 <- data_2[,-3]
    a <- data_2 %>% group_by(year)
    a <- summarise(a,countrymean_byyear=mean(hapi,na.rm=TRUE))
    data_2 <- left_join(data_2,a,by="year")
    ggplot(data_2)+
      geom_bar(aes(year,countrymean_byyear,fill=year),stat="identity")+
      labs(x="year",y="hapi")
  }
  # else{print("input is not working")}
}
visualization_hdiorhapi_contrast_total(hdi_hapi,outcome="hdi")
visualization_hdiorhapi_contrast_total(hdi_hapi,outcome="hapi")

