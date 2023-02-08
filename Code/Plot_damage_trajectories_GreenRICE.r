#setup#

    x <- c('arrow','raster',"hacksaw", 'ggpubr','dplyr','ncdf4','ggplot2','countrycode','tidyverse','RColorBrewer','colorspace','spData','sf','lfe','marginaleffects','rgdal',"rnaturalearth")
    lapply(x, require, character.only = TRUE)
    
    #install.packages('marginaleffects')
    setwd('C:\\Users\\basti\\Documents\\GitHub\\NatCap_PFTs')
    
    GR_Results <- read_parquet("Results/results.parquet")
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

    GR_Results$GDP_change_perc <- (GR_Results$YGROSS_damages-GR_Results$YGROSS_baseline)*100/GR_Results$YGROSS_baseline
    GR_Results$GDP_change_perc_ub <- (GR_Results$YGROSS_ub_mkt_damages-GR_Results$YGROSS_baseline)*100/GR_Results$YGROSS_baseline
    GR_Results$GDP_change_perc_lb <- (GR_Results$YGROSS_lb_mkt_damages-GR_Results$YGROSS_baseline)*100/GR_Results$YGROSS_baseline
    
    aggregate(GDP_change_perc~r5,data=GR_Results[which(GR_Results$t==18),],FUN="mean")

    
    GR_Results$GDP_change <- (GR_Results$YGROSS_damages-GR_Results$YGROSS_baseline)
    
    ggplot(GR_Results)+
    geom_line(aes(x=year,y=YGROSS_damages,color=r5,group=n))+
    geom_line(aes(x=year,y=YGROSS_baseline,color=r5,group=n))+
    scale_y_continuous(trans="log")

    ggplot(GR_Results[])+
    geom_line(aes(x=year,y=GDP_change,color=r5,group=n))+
    xlim(c(2020,2100))


    GR_Results$dr <- 1/(1+0.03)^(GR_Results$year  - 2020)
    GR_Results$dr[which(GR_Results$year==2020)]=1
    GR_Results$disc_damage <- GR_Results$dr * GR_Results$GDP_change

    isos <- levels(factor(GR_Results$n))
    for(i in 1:length(isos)){
      GR_d <- data.frame(approx(GR_Results$year[which(GR_Results$n==isos[i])],GR_Results$disc_damage[which(GR_Results$n==isos[i])],n=57*5+1),n=isos[i])
      n_npv<-data.frame(n=isos[i],NPV=sum(GR_d$y[which(GR_d$x>2019)]))
      if(i==1){
        n_NPV <- n_npv
      }else{
        n_NPV <- rbind(n_NPV,n_npv)
      }

    }
    
    
 
    GR_Results<-merge(GR_Results,n_NPV,by="n",all=TRUE)
    glimpse(GR_Results)

    100*sum(GR_Results$NPV[which(GR_Results$t==18)])/sum(GR_Results$YGROSS2020[which(GR_Results$t==18)])

    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="MAF")] )/sum(GR_Results$YGROSS2020[which(GR_Results$t==18  & GR_Results$r5=="MAF")])
    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="OECD")] )/sum(GR_Results$YGROSS2020[which(GR_Results$t==18  & GR_Results$r5=="OECD")])
    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="LAM")] )/sum(GR_Results$YGROSS2020[which(GR_Results$t==18  & GR_Results$r5=="LAM")])
    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="REF")] )/sum(GR_Results$YGROSS2020[which(GR_Results$t==18  & GR_Results$r5=="REF")])
    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="ASIA")] )/sum(GR_Results$YGROSS2020[which(GR_Results$t==18  & GR_Results$r5=="ASIA")])


    GR_Results$NPV_perc <- 100*GR_Results$NPV/GR_Results$YGROSS2020
    aggregate(NPV_perc~r5,data=GR_Results[which(GR_Results$t==18),],FUN="mean")    



    mean(GR_Results$NPV_perc[which(GR_Results$t==18)])
    mean(GR_Results$NPV_perc[which(GR_Results$t==18)])
    mean(GR_Results$GDP_change_perc[which(GR_Results$t==18)])
    mean(GR_Results$ES_change[which(GR_Results$t==18)])
    median(GR_Results$ES_change[which(GR_Results$t==18)])
    weighted.mean(x=GR_Results$ES_change[which(GR_Results$t==18)],w=GR_Results$pop[which(GR_Results$t==18)])
    weighted.mean(x=GR_Results$NPV_perc[which(GR_Results$t==18)],w=GR_Results$pop[which(GR_Results$t==18)])

    library('plyr')
    ddply(GR_Results[which(GR_Results$t==18),], .(r5),   # so by asset class invoke following function
      function(x) data.frame(npvr5=weighted.mean(x$NPV_perc, x$pop)))

    GR_Results$ES_change[which(GR_Results$t==18)]>0


    ggplot(GR_Results[which(GR_Results$t<=18),])+
    geom_point(aes(x=year,y=dr,color=r5))+theme_bw()


    
library(ggallin)
GDPNPV <- ggplot(GR_Results[which(GR_Results$t==18),])+
        geom_point(aes(x=log(YGROSS2020/pop2020),y=NPV_perc,color=r5))+theme_bw() +
        geom_smooth(data=GR_Results[which(GR_Results$t==18),],aes(x=log(YGROSS2020/pop2020),y=NPV_perc,formula="y~x",method="lm"))+
        ylab("NPV Damages (% GDP in 2020)")+
        geom_hline(aes(yintercept=0),linetype="dashed")+
        scale_y_continuous(trans=pseudolog10_trans)
    GDPNPV       


    GR_Results$ES_change <- (GR_Results$nat_omega_damages_nN-1)*100
    GR_Results$ES_change_ub <- (GR_Results$nat_omega_ub_nonmkt_damages_nN-1)*100
    GR_Results$ES_change_lb <- (GR_Results$nat_omega_lb_nonmkt_damages_nN-1)*100
    GR_Results$ES_change <- (GR_Results$nat_omega_damages_nN-1)*100
    aggregate(GDP_change~r5,data=GR_Results[which(GR_Results$t==18),],FUN="mean")
    aggregate(GDP_change_perc~r5,data=GR_Results[which(GR_Results$t==18),],FUN="mean")
    aggregate(ES_change~r5,data=GR_Results[which(GR_Results$t==18),],FUN="mean")

    
    GDP2100 <- ggplot(GR_Results[which(GR_Results$t==18),])+
        geom_point(aes(x=log(YGROSS2020/pop2020),y=(YGROSS_damages-YGROSS_baseline)*100/YGROSS_baseline,color=r5))+theme_bw() +
        geom_smooth(data=GR_Results[which(GR_Results$t==18),],aes(x=log(YGROSS2020/pop2020),y=(YGROSS_damages-YGROSS_baseline)*100/YGROSS_baseline),formula="y~x",method="lm")+
        ylab("GDP change in 2100 (%)")+
        geom_hline(aes(yintercept=0),linetype="dashed")+
     xlab("Log GDP per capita in 2020")+ 
     guides(color=guide_legend(title="Region"))
    GDP2100        

    ES2100 <- ggplot(GR_Results[which(GR_Results$t==18),])+
       geom_point(aes(x=log(YGROSS2020/pop2020),y=ES_change,color=r5))+theme_bw() + 
       geom_smooth(data=GR_Results[which(GR_Results$t==18),],
     aes(x=log(YGROSS2020/pop2020),y=ES_change),formula="y~x",method="lm")+
     ylab("Ecosystem services change in 2100 (%)")+
     geom_hline(aes(yintercept=0),linetype="dashed")+
     xlab("Log GDP per capita in 2020")+ 
     guides(color=guide_legend(title="Region"))

    ES2100    
     
     ggarrange(GDP2100,ES2100,common.legend=TRUE,legend="bottom")
     ggsave("Figures/Change_GDP_ES_in2100.png",dpi=600)



    GR_Results$country <-(GR_Results$n)
    GR_Results$Continent <-(GR_Results$r5)
    #GR_Results$es_change <-(GR_Results$nat_omega_damages_nN)-1
    # GR_Results$gdp_change <- (GR_Results$YGROSS_b_0.5/GR_Results$pop-GR_Results$YGROSS_baseline/GR_Results$pop)/(GR_Results$YGROSS2020/GR_Results$pop2020)
    # GR_Results$gdp_change <- (GR_Results$YGROSS_b_0.5/GR_Results$pop-GR_Results$YGROSS_baseline/GR_Results$pop)
    # GR_Results$gdp_change <- (GR_Results$YGROSS_b_0.5-GR_Results$YGROSS_baseline)/GR_Results$YGROSS_baseline
    # GR_Results$gdp_change <- (GR_Results$YGROSS_b_0.5-GR_Results$YGROSS_baseline)

    # plot_by_year <- ggplot(GR_Results)+
    # geom_line(aes(x=year,y=(YGROSS_b_0.5-YGROSS_baseline)/(YGROSS_baseline), group=n, color=r5)) + theme_bw()+
    # xlab("Year") + ylab("GDP change w.r.t. baseline (%)")

    # plot_by_GDP <- ggplot(GR_Results[which(GR_Results$year==2100),])+
    # geom_point(aes(x=YGROSS_baseline/pop,y=100*(YGROSS_b_0.5-YGROSS_baseline)/(YGROSS_baseline), group=n, color=r5)) + theme_bw()+
    # xlab("GDP per capita in 2100") + ylab("GDP change w.r.t. baseline (%)")

    # ggarrange(plot_by_year,plot_by_GDP,common.legend=TRUE,legend="bottom")


  #   plot_by_year <- ggplot(GR_Results)+
  #   geom_line(aes(x=year,y=(YGROSS_b_0.5-YGROSS_baseline)/(YGROSS2020), group=n, color=r5)) + theme_bw()+
  #   xlab("Year") + ylab("GDP change w.r.t. GDP in 2020 (%)")

  #   plot_by_GDP <- ggplot(GR_Results[which(GR_Results$year==2100),])+
  #   geom_point(aes(x=YGROSS_baseline/pop,y=100*(YGROSS_b_0.5-YGROSS_baseline)/(YGROSS2020), group=n, color=r5)) + theme_bw()+
  #   xlab("GDP per capita in 2100") + ylab("GDP change w.r.t. GDP in 2020 (%)")

  #   ggarrange(plot_by_year,plot_by_GDP,common.legend=TRUE,legend="bottom")

  #   plot_by_year <- ggplot(GR_Results)+
  #   geom_line(aes(x=year,y=(YGROSS_b_0.5-YGROSS_baseline), group=n, color=r5)) + theme_bw()+
  #   xlab("Year") + ylab("GDP damages (2005 trill USD)")+
  # scale_y_continuous(trans = scales::pseudo_log_trans())

  #   plot_by_GDP <- ggplot(GR_Results[which(GR_Results$year==2100),])+
  #   geom_point(aes(x=YGROSS_b_0.5/pop,y=(YGROSS_b_0.5-YGROSS_baseline), group=n, color=r5)) + theme_bw()+
  #   xlab("GDP per capita in 2100") + ylab("GDP damages (2005 trill USD)")+
  # #scale_y_continuous(trans = scales::pseudo_log_trans())+
  # geom_smooth(data=GR_Results[which(GR_Results$year==2100),],aes(x=YGROSS_baseline/pop,y=(YGROSS_b_0.5-YGROSS_baseline)))

  #   ggarrange(plot_by_year,plot_by_GDP,common.legend=TRUE,legend="bottom")


  #   plot_by_GDP <- ggplot(GR_Results[which(GR_Results$year==2100),])+
  #   geom_point(aes(x=YGROSS_b_0.5/pop,y=(YGROSS_b_0.5-YGROSS_baseline), group=n, color=r5)) + theme_bw()+
  #   xlab("GDP per capita in 2100") + ylab("GDP damages (2005 trill USD)")+
  # #scale_y_continuous(trans = scales::pseudo_log_trans())+
  # geom_smooth(data=GR_Results[which(GR_Results$year==2100),],aes(x=YGROSS_b_0.5/pop,y=(YGROSS_b_0.5-YGROSS_baseline))) 

  # plot_by_GDP_2 <- ggplot(GR_Results[which(GR_Results$year==2100),])+
  #   geom_point(aes(x=YGROSS_b_0.5/pop,y=(YGROSS_b_0.5-YGROSS_baseline)/YGROSS_baseline, group=n, color=r5)) + theme_bw()+
  #   xlab("GDP per capita in 2100") + ylab("GDP damages (2005 trill USD)")+
  # #scale_y_continuous(trans = scales::pseudo_log_trans())+
  # geom_smooth(data=GR_Results[which(GR_Results$year==2100),],aes(x=YGROSS_b_0.5/pop,y=(YGROSS_b_0.5-YGROSS_baseline)/YGROSS_baseline))

  #   ggarrange(plot_by_GDP,plot_by_GDP_2,common.legend=TRUE,legend="bottom")

  #    ggplot(GR_Results[which(GR_Results$year==2100),])+
  #   geom_point(aes(x=pop,y=(YGROSS_b_0.5-YGROSS_baseline), group=n, color=r5)) + theme_bw()+
  #   xlab("Pop") + ylab("GDP damages (2005 trill USD)")+
  # #scale_y_continuous(trans = scales::pseudo_log_trans())+
  # geom_smooth(data=GR_Results[which(GR_Results$year==2100),],aes(x=pop,y=(YGROSS_b_0.5-YGROSS_baseline)))
    
    GR_Results <- GR_Results[which(GR_Results$year <2101),]
    GR_Results$r5 <- (factor(GR_Results$r5))
    levels(GR_Results$r5) <- c("ASIA","LAM","MAF","OECD","REF")
    
    pop_mean_gdp_lpj <- weighted.mean(x=GR_Results$GDP_change_perc[which(GR_Results$year %in% c(2100))],w=GR_Results$pop[which(GR_Results$year %in% c(2100))])
    pop_mean_es_lpj <- weighted.mean(x=GR_Results$ES_change[which(GR_Results$year %in% c(2100))],w=GR_Results$pop[which(GR_Results$year %in% c(2100))])
    pop_se_gdp_lpj <- sd(x=GR_Results$GDP_change_perc[which(GR_Results$year %in% c(2100))])
    pop_se_es_lpj <- sd(x=GR_Results$ES_change[which(GR_Results$year %in% c(2100))])


    glimpse(GR_Results)
    plot_trajectory <- ggplot(GR_Results,
     aes(x=ES_change,y=GDP_change_perc,color=r5,group=n))+
     #aes(x=es_change*100,y=gdp_change*100,group=countrycode))+
     #geom_hline(aes(yintercept=0),linetype="dashed")+
     #geom_vline(aes(xintercept=0),linetype="dashed")+
     geom_hline(aes(yintercept=0))+
     geom_vline(aes(xintercept=0))+
       geom_line()+
       geom_point(data = GR_Results[which(GR_Results$year %in% c(2050,2080,2100) ),], 
        aes(x=ES_change,y=GDP_change_perc,color=r5,shape=factor(year)),alpha=0.4)+
        #ymin=gdp_change-gdp_change_sd,ymax=gdp_change+gdp_change_sd))+
       theme_minimal()+
       geom_hline(aes(yintercept=pop_mean_gdp),linetype=2,alpha=0.9)+
       geom_vline(aes(xintercept=pop_mean_es),linetype=2,alpha=0.9)+
      geom_errorbar(data=GR_Results[which(GR_Results$year %in% c(2100)),],
        aes(ymin=(GDP_change_perc_lb), ymax=GDP_change_perc_ub),alpha=0.3)+
      geom_errorbar(data=GR_Results[which(GR_Results$year %in% c(2100)),],
        aes(xmin=(ES_change_lb), xmax=ES_change_ub),alpha=0.3)+
        #geom_errorbarh(data=all_sim[which(all_sim$exp=="RCP6" & all_sim$year %in% c(2100) ),],
        #aes(xmin=(es_change_minus_se)*100, xmax=(es_change_plus_se)*100),alpha=0.3)+
        theme(legend.position="bottom",legend.box="vertical")+
       labs(color="Region",shape="Year")+
       ylab("Annual GDP change (%)") + ggtitle("")  +
  #scale_y_continuous(trans = scales::pseudo_log_trans())
       xlab("Annual ecosystem services change (%)") +
       coord_cartesian(xlim=c(-45,10),ylim=c(-1.25,0))
       
    #    geom_text(data = all_sim[which(all_sim$exp=="RCP6" & all_sim$year %in% c(2100) ),], 
    #     aes(x=es_change*100,y=gdp_change*100,color=Continent,label=countrycode))
       
        #coord_cartesian(xlim=c(-100, 60),ylim=c(-15,6))
       plot_trajectory

         leg_trj <-  get_legend(plot_trajectory)

    maxyear <- aggregate(year~country, data=GR_Results,FUN="max")
    maxyear$countrymax <- paste0(maxyear$country,maxyear$year)
    GR_Results$countrymax <- paste0(GR_Results$country,GR_Results$year)

    all_simmax <- GR_Results[GR_Results$countrymax %in% maxyear$countrymax,]
    
plot_count_es <- ggplot(GR_Results[which(GR_Results$year==2100),], # & abs(all_sim_mean$es_change)>0.01
     aes(x=es_change*100,fill=Continent))+
     geom_histogram()+theme_minimal()+ xlab("")+ylab("count in 2100") #+xlim(-12,5)#+ xlim(-100,60)

     ordered_GR <- GR_Results[which(GR_Results$year==2100),]
ordered_GR <- ordered_GR[order(ordered_GR$ES_change),]

totpop <- sum(GR_Results$pop[which(GR_Results$year==2100)])

cumsum(100*ordered_GR$pop/totpop)
ordered_GR$ES_change

plot_count_es <- ggplot(ordered_GR, #& abs(all_sim_mean$gdp_change)>0.01
    aes(x=(ES_change),y=cumsum(100*pop/totpop)))+
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


plot_count_gdp <- ggplot(GR_Results[which(GR_Results$year==2100),], #& abs(all_sim_mean$gdp_change)>0.01
    aes(x=(GDP_change),y=(100*pop/totpop)))+
    #geom_bar(stat="identity")+
    geom_line(color="gray")+
    geom_point(aes(color=r5),shape=15)+
    theme_bw() + coord_flip() + ylab("Population in 2100 (%)")+
  scale_x_continuous(trans = scales::pseudo_log_trans())+theme_minimal()+xlab('')
plot_count_gdp

ordered_GR <- GR_Results[which(GR_Results$year==2100),]
ordered_GR <- ordered_GR[order(-ordered_GR$GDP_change_perc),]
plot_count_gdp_cum <- ggplot(ordered_GR, #& abs(all_sim_mean$gdp_change)>0.01
    aes(x=(GDP_change_perc),y=cumsum(100*pop/totpop)))+
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

        ggsave("Figures/Trajectory_percent.png",dpi=600)


