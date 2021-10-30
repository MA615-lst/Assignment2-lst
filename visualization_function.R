
# Shuting did a really fantastic job on this visualization function homework.
# To be honest, I could not find any problems at first cause my experience with R only started on September.
# I was thrilled to watch and learn from her code and my understanding of function coding improved as I read
# her code.
# After three days of consideration, I admitted that her code is not within my ability to improve or change. 
# However, from my humble opinion, simplicity is a very important part of coding, and your code produced some
# unused graphs. Here I present one of my suggestions of the function, it is very simple compare to the original coding,
# but I think this is what I can do for now. 



#If I were to do the hdi~hapi by country graph, I'd do it like this.
visual_hdi_hapi_bycountry <- function(data,country1,country2){
  ggplot(data_1)+
    geom_point(aes(x=hdi,y=hapi, color = country)) +
    labs(x="hdi",y="hapi")+
    geom_smooth(aes(x=hdi,y=hapi, color = country), method = "lm")
}
visualization_hdi_hapi_bycountry(hdi_hapi,country1 ="China",country2 = "United States")





#Below is the orginal code by Shuting.
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


