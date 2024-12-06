
    write.csv(wealth2018,"Data/wealth2018.csv")
    wealth_data$nN <- (wealth_data$NforestES+wealth_data$Npa)/wealth_data$TotalWealth
    wealth_data$mN <- (wealth_data$NforestT)/wealth_data$TotalWealth
    write.csv(wealth_data,"Data/wealth_data.csv")

    nc <- read.csv("Data/natural_capital.csv")
    glimpse(nc)
    glimpse(wealth2018)

    nc <- nc %>% left_join(wealth2018,"countrycode")
    glimpse(nc)
    library(ggrepel)
    NK_plot <- ggplot(nc)+
    geom_point(aes(x=mNK,NforestT)) + 
    geom_text_repel(aes(x=mNK,y=NforestT,label=countrycode)) + 
    geom_abline(slope=1)

    NK_gamma <- ggplot(nc)+
    geom_point(aes(x=mNK_gama,NatRents/100)) + 
    geom_text_repel(aes(x=mNK_gama,NatRents/100,label=countrycode)) + 
    geom_abline(slope=1)

    ggarrange(NK_plot,NK_gamma)
