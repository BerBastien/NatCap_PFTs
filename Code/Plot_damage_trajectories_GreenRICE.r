#setup#

    x <- c('arrow','raster',"hacksaw", 'ggpubr','dplyr','ncdf4','ggplot2','countrycode','tidyverse','RColorBrewer','colorspace','spData','sf','lfe','marginaleffects','rgdal',"rnaturalearth")
    lapply(x, require, character.only = TRUE)
    #install.packages('marginaleffects')
    setwd('C:\\Users\\basti\\Documents\\GitHub\\NatCap_PFTs')
    
    GR_Results <- read_parquet("Data/results.parquet")
    glimpse(GR_Results)    

   
    YGross2020 <- GR_Results[which(GR_Results$t==3),]
    GR_Results <- merge(GR_Results,YGross2020[which(names(YGross2020) %in% c("n","YGROSS_baseline","pop"))],by="n",All=FALSE)
    GR_Results$YGROSS2020 <- GR_Results$YGROSS_baseline.y
    GR_Results$YGROSS_baseline <- GR_Results$YGROSS_baseline.x
    GR_Results$pop2020 <- GR_Results$pop.y
    GR_Results$pop <- GR_Results$pop.x
    GR_Results <- GR_Results[,which(names(GR_Results) %notin% c("YGROSS_baseline.x","YGROSS_baseline.y","pop.x","pop.y"))]
    glimpse(GR_Results)    

    GR_Results$year <- 2005 + GR_Results$t * (5)

    
    GDP2100 <- ggplot(GR_Results[which(GR_Results$t==18),])+
        geom_point(aes(x=log(YGROSS2020/pop2020),y=(YGROSS_b_0.5-YGROSS_baseline)*100/YGROSS2020,color=r5))+theme_bw() +
        geom_smooth(data=GR_Results[which(GR_Results$t==18),],aes(x=log(YGROSS2020/pop2020),y=(YGROSS_b_0.5-YGROSS_baseline)*100/YGROSS2020))+ylab("GDP change (%)")+
        geom_hline(aes(yintercept=0),linetype="dashed")
    GDP2100        

    ES2100 <- ggplot(all_sim[which(all_sim$exp=="RCP6" & all_sim$year==2100),])+
       geom_point(aes(x=log(GDP),y=es_change*100,color=Continent))+theme_bw() + geom_smooth(data=all_sim[which(all_sim$exp=="RCP6" & all_sim$year==2100),],
     aes(x=log(GDP),y=es_change*100))+ylab("Ecosystem benefits change (%)")+
     geom_hline(aes(yintercept=0),linetype="dashed")
    
     ggarrange(GDP2100,ES2100,common.legend=TRUE,legend="bottom")



    GR_Results$country <-(GR_Results$n)
    GR_Results$Continent <-(GR_Results$r5)
    GR_Results$es_change <-(GR_Results$nat_omega_b_0.5_nN)-1
    GR_Results$gdp_change <- (GR_Results$YGROSS_b_0.5/GR_Results$pop-GR_Results$YGROSS_baseline/GR_Results$pop)/(GR_Results$YGROSS2020/GR_Results$pop2020)
    GR_Results$gdp_change <- (GR_Results$YGROSS_b_0.5/GR_Results$pop-GR_Results$YGROSS_baseline/GR_Results$pop)
    GR_Results$gdp_change <- (GR_Results$YGROSS_b_0.5-GR_Results$YGROSS_baseline)/GR_Results$YGROSS_baseline
    GR_Results$gdp_change <- (GR_Results$YGROSS_b_0.5-GR_Results$YGROSS_baseline)

    plot_by_year <- ggplot(GR_Results)+
    geom_line(aes(x=year,y=(YGROSS_b_0.5-YGROSS_baseline)/(YGROSS_baseline), group=n, color=r5)) + theme_bw()+
    xlab("Year") + ylab("GDP change w.r.t. baseline (%)")

    plot_by_GDP <- ggplot(GR_Results[which(GR_Results$year==2100),])+
    geom_point(aes(x=YGROSS_baseline/pop,y=100*(YGROSS_b_0.5-YGROSS_baseline)/(YGROSS_baseline), group=n, color=r5)) + theme_bw()+
    xlab("GDP per capita in 2100") + ylab("GDP change w.r.t. baseline (%)")

    ggarrange(plot_by_year,plot_by_GDP,common.legend=TRUE,legend="bottom")


    plot_by_year <- ggplot(GR_Results)+
    geom_line(aes(x=year,y=(YGROSS_b_0.5-YGROSS_baseline)/(YGROSS2020), group=n, color=r5)) + theme_bw()+
    xlab("Year") + ylab("GDP change w.r.t. GDP in 2020 (%)")

    plot_by_GDP <- ggplot(GR_Results[which(GR_Results$year==2100),])+
    geom_point(aes(x=YGROSS_baseline/pop,y=100*(YGROSS_b_0.5-YGROSS_baseline)/(YGROSS2020), group=n, color=r5)) + theme_bw()+
    xlab("GDP per capita in 2100") + ylab("GDP change w.r.t. GDP in 2020 (%)")

    ggarrange(plot_by_year,plot_by_GDP,common.legend=TRUE,legend="bottom")

    plot_by_year <- ggplot(GR_Results)+
    geom_line(aes(x=year,y=(YGROSS_b_0.5-YGROSS_baseline), group=n, color=r5)) + theme_bw()+
    xlab("Year") + ylab("GDP damages (2005 trill USD)")+
  scale_y_continuous(trans = scales::pseudo_log_trans())

    plot_by_GDP <- ggplot(GR_Results[which(GR_Results$year==2100),])+
    geom_point(aes(x=YGROSS_b_0.5/pop,y=(YGROSS_b_0.5-YGROSS_baseline), group=n, color=r5)) + theme_bw()+
    xlab("GDP per capita in 2100") + ylab("GDP damages (2005 trill USD)")+
  #scale_y_continuous(trans = scales::pseudo_log_trans())+
  geom_smooth(data=GR_Results[which(GR_Results$year==2100),],aes(x=YGROSS_baseline/pop,y=(YGROSS_b_0.5-YGROSS_baseline)))

    ggarrange(plot_by_year,plot_by_GDP,common.legend=TRUE,legend="bottom")


    plot_by_GDP <- ggplot(GR_Results[which(GR_Results$year==2100),])+
    geom_point(aes(x=YGROSS_b_0.5/pop,y=(YGROSS_b_0.5-YGROSS_baseline), group=n, color=r5)) + theme_bw()+
    xlab("GDP per capita in 2100") + ylab("GDP damages (2005 trill USD)")+
  #scale_y_continuous(trans = scales::pseudo_log_trans())+
  geom_smooth(data=GR_Results[which(GR_Results$year==2100),],aes(x=YGROSS_b_0.5/pop,y=(YGROSS_b_0.5-YGROSS_baseline))) 

  plot_by_GDP_2 <- ggplot(GR_Results[which(GR_Results$year==2100),])+
    geom_point(aes(x=YGROSS_b_0.5/pop,y=(YGROSS_b_0.5-YGROSS_baseline)/YGROSS_baseline, group=n, color=r5)) + theme_bw()+
    xlab("GDP per capita in 2100") + ylab("GDP damages (2005 trill USD)")+
  #scale_y_continuous(trans = scales::pseudo_log_trans())+
  geom_smooth(data=GR_Results[which(GR_Results$year==2100),],aes(x=YGROSS_b_0.5/pop,y=(YGROSS_b_0.5-YGROSS_baseline)/YGROSS_baseline))

    ggarrange(plot_by_GDP,plot_by_GDP_2,common.legend=TRUE,legend="bottom")

     ggplot(GR_Results[which(GR_Results$year==2100),])+
    geom_point(aes(x=pop,y=(YGROSS_b_0.5-YGROSS_baseline), group=n, color=r5)) + theme_bw()+
    xlab("Pop") + ylab("GDP damages (2005 trill USD)")+
  #scale_y_continuous(trans = scales::pseudo_log_trans())+
  geom_smooth(data=GR_Results[which(GR_Results$year==2100),],aes(x=pop,y=(YGROSS_b_0.5-YGROSS_baseline)))
    
    GR_Results <- GR_Results[which(GR_Results$year <2101),]
    
    plot_trajectory <- ggplot(GR_Results,
     aes(x=es_change*100,y=gdp_change,color=r5,group=n))+
     #aes(x=es_change*100,y=gdp_change*100,group=countrycode))+
     geom_hline(aes(yintercept=0),linetype="dashed")+
     geom_vline(aes(xintercept=0),linetype="dashed")+
       geom_line(alpha=0.5)+
       geom_point(data = GR_Results[which(GR_Results$year %in% c(2050,2080,2100) ),], 
        aes(x=es_change*100,y=gdp_change,color=r5,shape=factor(year)),alpha=0.3)+
        #ymin=gdp_change-gdp_change_sd,ymax=gdp_change+gdp_change_sd))+
       theme_bw()+
        #geom_errorbar(data=all_sim[which(all_sim$exp=="RCP6" & all_sim$year %in% c(2100) ),],
        #aes(ymin=(gdp_change_minus_se)*100, ymax=(gdp_change_plus_se)*100),alpha=0.3)+
        #geom_errorbarh(data=all_sim[which(all_sim$exp=="RCP6" & all_sim$year %in% c(2100) ),],
        #aes(xmin=(es_change_minus_se)*100, xmax=(es_change_plus_se)*100),alpha=0.3)+
        theme(legend.position="bottom",legend.box="vertical")+
       labs(color="Region",shape="Year")+
       xlab("Ecosystem services change (%)") +
       ylab("GDP loss (trill 2005 USD)") + ggtitle("")  +
  scale_y_continuous(trans = scales::pseudo_log_trans())
       
    #    geom_text(data = all_sim[which(all_sim$exp=="RCP6" & all_sim$year %in% c(2100) ),], 
    #     aes(x=es_change*100,y=gdp_change*100,color=Continent,label=countrycode))
       
        #coord_cartesian(xlim=c(-100, 60),ylim=c(-15,6))
       plot_trajectory

         leg_trj <-  get_legend(plot_trajectory)

    maxyear <- aggregate(year~country, data=GR_Results,FUN="max")
    maxyear$countrymax <- paste0(maxyear$country,maxyear$year)
    GR_Results$countrymax <- paste0(all_sim$country,all_sim$year)

    all_simmax <- GR_Results[GR_Results$countrymax %in% maxyear$countrymax,]
    
plot_count_es <- ggplot(GR_Results[which(GR_Results$year==2100),], # & abs(all_sim_mean$es_change)>0.01
     aes(x=es_change*100,fill=Continent))+
     geom_histogram()+theme_minimal()+ xlab("")+ylab("count in 2100") #+xlim(-12,5)#+ xlim(-100,60)

     ordered_GR <- GR_Results[which(GR_Results$year==2100),]
ordered_GR <- ordered_GR[order(ordered_GR$es_change),]

plot_count_es <- ggplot(ordered_GR, #& abs(all_sim_mean$gdp_change)>0.01
    aes(x=(es_change),y=cumsum(100*pop/totpop)))+
    #geom_bar(stat="identity")+
    geom_line(color="gray")+
    geom_point(aes(color=r5),shape=15)+
    theme_bw()+ ylab("Cumulative \npopulation in 2100 (%)")+xlab('')
plot_count_es

plot_count_gdp <- ggplot(GR_Results[which(GR_Results$year==2100),], #& abs(all_sim_mean$gdp_change)>0.01
     aes(x=gdp_change*100,fill=Continent))+xlab('')+#xlim(-15,6)+
     ylab("count in 2100")+
     geom_histogram()+theme_minimal()+coord_flip() 

glimpse(GR_Results[which(GR_Results$year==2100),])

totpop <- sum(GR_Results$pop[which(GR_Results$year==2100)])

plot_count_gdp <- ggplot(GR_Results[which(GR_Results$year==2100),], #& abs(all_sim_mean$gdp_change)>0.01
    aes(x=(gdp_change),y=(100*pop/totpop)))+
    #geom_bar(stat="identity")+
    geom_line(color="gray")+
    geom_point(aes(color=r5),shape=15)+
    theme_bw() + coord_flip() + ylab("Population in 2100 (%)")+
  scale_x_continuous(trans = scales::pseudo_log_trans())+theme_minimal()+xlab('')
plot_count_gdp

ordered_GR <- GR_Results[which(GR_Results$year==2100),]
ordered_GR <- ordered_GR[order(-ordered_GR$gdp_change),]
plot_count_gdp_cum <- ggplot(ordered_GR, #& abs(all_sim_mean$gdp_change)>0.01
    aes(x=(gdp_change),y=cumsum(100*pop/totpop)))+
    #geom_bar(stat="identity")+
    geom_line(color="gray")+
    geom_point(aes(color=r5),shape=15)+
    theme_bw() + coord_flip() + ylab("Cumulative\npopulation in 2100 (%)")+
  scale_x_continuous(trans = scales::pseudo_log_trans())+theme_minimal()+xlab('')

#install.packages("ggridges")
#library("ggridges")



    emptyplot <- ggplot()+theme_void()
        
    traj_plot <- ggarrange(ggarrange(plot_trajectory, plot_count_gdp,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="bottom"),
        ncol=1,nrow=2,common.legend=TRUE,heights=c(3,1),legend="bottom",align="hv")
    
    annotate_figure(traj_plot, top = text_grob("Ecosystem Benefits change under RCP6.0", 
                face = "bold", size = 14))
    #ggsave("Figures/Trajectory_completeHist.png",dpi=300)

     ggarrange(ggarrange(plot_trajectory, plot_count_gdp_cum,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="none"),as_ggplot(leg_trj),
        ncol=1,nrow=3,common.legend=TRUE,heights=c(4,1,1),legend="none",align="hv")

        ggsave("Figures/Trajectory.png",dpi=300)



    
    
    ggplot(GR_Results)+
    geom_line(aes(x=t,y=(value-baseline),color=n,linetype=b))

    
    ggplot(GR_Results[which(GR_Results$t==18),])+
    geom_point(aes(x=value,y=(value-baseline)/baseline,color=r5,shape=b))


    dir1 <- "C:/Users/basti/Box/VegetationData/"
    isos <- world$iso_a2

    library(RColorBrewer)
    my.palette <- brewer.pal(n = 10, name = "BrBG")
    `%notin%` <- Negate(`%in%`)

    ## Function
        sqest <- function(data, model, namevar, exp) {
            dataset <- data
            Sigma <- vcov(model)
            coefT <- namevar
            start1 <- which(names(coef(model))==coefT)
            end1 <- which(names(coef(model))==paste("I(",coefT,"^2)",sep=""))
            
            sigma = Sigma[c(1:end1),c(1:end1)]
            beta.hat <- coef(model)[c(1:end1)]
            x <- seq(from=min(dataset[,which(names(dataset)==namevar)],na.rm=TRUE),to=max(dataset[,which(names(dataset)==namevar)],na.rm=TRUE), length=100)
            xmat <- cbind(x, x^2)
            gestimated <- colSums(beta.hat*t(xmat)) 
            ci12 <- gestimated + 1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
            ci22 <- gestimated -  1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))


            return(data.frame(gestimated=gestimated,ci1=ci12,ci2=ci22,exp=exp,temp=x))
        }
    ## Function
#setup
