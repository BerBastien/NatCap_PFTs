#setup#

    x <- c('arrow','raster',"hacksaw", 'ggpubr','dplyr','ncdf4','ggplot2','countrycode','tidyverse','RColorBrewer','colorspace','spData','sf','lfe','marginaleffects','rgdal',"rnaturalearth","ddply")
    lapply(x, require, character.only = TRUE)
    '%notin%' <- Negate('%in%')

    setwd('C:\\Users\\basti\\Documents\\GitHub\\NatCap_PFTs')
    
    
    GR_Results <- read_parquet("Results/06132023/resultsLinearLPJ.parquet")
    #GR_Results <- read_parquet("Results/06132023_noS/resultsLinearLPJ.parquet")
    glimpse(GR_Results)    

    GR_Results <- GR_Results[-which(GR_Results$n=="rjan57" & GR_Results$r5=="r5oecd"),]
    GR_Results <- GR_Results[-which(GR_Results$n=="oeu" & GR_Results$r5=="r5oecd"),]
    length(GR_Results$n[which(GR_Results$t==18)])   
    YGross2020 <- GR_Results[which(GR_Results$t==2),]
    GR_Results <- merge(GR_Results,YGross2020[which(names(YGross2020) %in% c("n","YGROSS_baseline","pop"))],by="n",All=FALSE)
    
    GR_Results$YGROSS2020 <- GR_Results$YGROSS_baseline.y
    GR_Results$YGROSS_baseline <- GR_Results$YGROSS_baseline.x
    GR_Results$pop2020 <- GR_Results$pop.y
    GR_Results$pop <- GR_Results$pop.x
    GR_Results <- GR_Results[,which(names(GR_Results) %notin% c("YGROSS_baseline.x","YGROSS_baseline.y","pop.x","pop.y"))]
    GR_Results$year <- 2010 + GR_Results$t * (5)
    GR_Results$GDP_change_perc <- (GR_Results$YGROSS_damages-GR_Results$YGROSS_baseline)*100/GR_Results$YGROSS_baseline
    GR_Results$GDP_change_perc_ub <- (GR_Results$YGROSS_ub_mkt_damages-GR_Results$YGROSS_baseline)*100/GR_Results$YGROSS_baseline
    GR_Results$GDP_change_perc_lb <- (GR_Results$YGROSS_lb_mkt_damages-GR_Results$YGROSS_baseline)*100/GR_Results$YGROSS_baseline
    
    
    GR_Results$GDP_change <- (GR_Results$YGROSS_damages-GR_Results$YGROSS_baseline)
    
    

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

    GR_Results$disc_GDP <- GR_Results$dr * GR_Results$YGROSS_baseline

    isos <- levels(factor(GR_Results$n))
    for(i in 1:length(isos)){
      GR_d <- data.frame(approx(GR_Results$year[which(GR_Results$n==isos[i])],GR_Results$disc_GDP[which(GR_Results$n==isos[i])],n=57*5+1),n=isos[i])
      n_npv<-data.frame(n=isos[i],NPV=sum(GR_d$y[which(GR_d$x>2019)]))
      if(i==1){
        n_NPV <- n_npv
      }else{
        n_NPV <- rbind(n_NPV,n_npv)
      }

    }
    names(n_NPV)[2] <- "NPV_GDP"
    
    
 
    GR_Results<-merge(GR_Results,n_NPV,by="n",all=TRUE)
    
    GR_Results$r5 <- factor(GR_Results$r5)
    levels(GR_Results$r5) <- c("ASIA","LAM","MAF","OECD","REF")

    100*sum(GR_Results$NPV[which(GR_Results$t==18)])/sum(GR_Results$YGROSS2020[which(GR_Results$t==18)])

    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="MAF")] )/sum(GR_Results$YGROSS2020[which(GR_Results$t==18  & GR_Results$r5=="MAF")])
    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="OECD")] )/sum(GR_Results$YGROSS2020[which(GR_Results$t==18  & GR_Results$r5=="OECD")])
    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="LAM")] )/sum(GR_Results$YGROSS2020[which(GR_Results$t==18  & GR_Results$r5=="LAM")])
    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="REF")] )/sum(GR_Results$YGROSS2020[which(GR_Results$t==18  & GR_Results$r5=="REF")])
    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="ASIA")] )/sum(GR_Results$YGROSS2020[which(GR_Results$t==18  & GR_Results$r5=="ASIA")])

    glimpse(GR_Results)
    

    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="MAF")] )/sum(GR_Results$NPV_GDP[which(GR_Results$t==18 & GR_Results$r5=="MAF")] )
    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="OECD")] )/sum(GR_Results$NPV_GDP[which(GR_Results$t==18 & GR_Results$r5=="OECD")] )
    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="LAM")] )/sum(GR_Results$NPV_GDP[which(GR_Results$t==18 & GR_Results$r5=="LAM")] )
    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="REF")] )/sum(GR_Results$NPV_GDP[which(GR_Results$t==18 & GR_Results$r5=="REF")] )
    100*sum(GR_Results$NPV[which(GR_Results$t==18 & GR_Results$r5=="ASIA")] )/sum(GR_Results$NPV_GDP[which(GR_Results$t==18 & GR_Results$r5=="ASIA")] )

   

    GR_Results$NPV_perc <- 100*GR_Results$NPV/GR_Results$YGROSS2020
    
    
    GR_Results$disc_gdp <- GR_Results$dr * GR_Results$YGROSS_baseline

    isos <- levels(factor(GR_Results$n))
     
    
    GR_Results$ES_change <- (GR_Results$nat_omega_damages_nN-1)*100

    

    #ddply(GR_Results[which(GR_Results$t==18),], .(r5),   # so by asset class invoke following function
    #  function(x) data.frame(npvr5=weighted.mean(x$NPV_perc, x$pop)))

    GR_Results$ES_change[which(GR_Results$t==18)]>0



    
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


 
    GR_Results$ES_dam <- (GR_Results$nat_omega_damages_nN*0.03)*(1+(0.593*(100*((GR_Results$YGROSS_damages-GR_Results$YGROSS2020)/GR_Results$YGROSS2020)))/100)
    GR_Results$ES_nodam <- (GR_Results$nat_omega_baseline_nN*0.03)*(1+(0.593*(100*((GR_Results$YGROSS_baseline-GR_Results$YGROSS2020)/GR_Results$YGROSS2020)))/100)
    GR_Results$ES_change <- 100*(GR_Results$ES_dam-GR_Results$ES_nodam)/GR_Results$ES_nodam
    GR_Results$ES_change_val <- GR_Results$ES_dam-GR_Results$ES_nodam
    GR_Results$ES_change_val_percGDP <- GR_Results$ES_change_val / GR_Results$YGROSS_damages
    ggplot(GR_Results) + geom_line(aes(x=year,y=ES_change_val,group=n))
    glimpse(GR_Results)
    GR_Results %>% filter(t==18) %>% group_by(r5) %>% summarise(ES_change_val=sum(ES_change_val,na.rm=T),YGROSS_damages=sum(YGROSS_damages,na.rm=T),ES_change_val_percGDP=median(ES_change_val_percGDP,na.rm=T))

    GR_Results2100 <- GR_Results[which(GR_Results$t==18),] 
    GR_Results2100$ES_change[which(GR_Results2100$ES_change<0)]
    GR_Results$t[GR_Results$ES_nodam==0]

    ggplot(GR_Results[which(GR_Results$t!=3),])+
    geom_line(aes(x=t,y=ES_dam,group=n,color=r5))
    
    ggplot(GR_Results)+
    geom_line(aes(x=t,y=ES_nodam,group=n,color=r5))
    
    ggplot(GR_Results[which(GR_Results$t!=3),])+
    geom_line(aes(x=t,y=ES_change,group=n,color=r5))

    
    GR_Results$ES_dam_ub <- (GR_Results$nat_omega_ub_nonmkt_damages_nN*0.03)*(1+(0.593*(100*((GR_Results$YGROSS_damages-GR_Results$YGROSS2020)/GR_Results$YGROSS2020)))/100)
    GR_Results$ES_change_ub <- 100*(GR_Results$ES_dam_ub-GR_Results$ES_nodam)/GR_Results$ES_nodam

    GR_Results$ES_dam_lb <- (GR_Results$nat_omega_lb_nonmkt_damages_nN*0.03)*(1+(0.593*(100*((GR_Results$YGROSS_damages-GR_Results$YGROSS2020)/GR_Results$YGROSS2020)))/100)
    GR_Results$ES_change_lb <- 100*(GR_Results$ES_dam_lb-GR_Results$ES_nodam)/GR_Results$ES_nodam



    
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
     ylab("Non-market benefits  \nchange in 2100 (%)")+
     geom_hline(aes(yintercept=0),linetype="dashed")+
     xlab("Log GDP per capita in 2020")+ 
     guides(color=guide_legend(title="Region"))

    ES2100    
    
    GR_Results_lpj_noS <- GR_Results
    save(GR_Results_lpj_noS,file="Data/DataForFigures/GR_Results_lpj_noS.Rds")
     
     ggarrange(GDP2100,ES2100,common.legend=TRUE,legend="bottom")
     results2100lpj <- ggarrange(GDP2100,ES2100,common.legend=TRUE,legend="bottom")

     #ggsave("Figures/Final figures/Submission 3/Change_GDP_ES_in2100_submission3.png",dpi=600)


    GR_Results2100 <-  GR_Results[which(GR_Results$t==18),]
    GR_Results2100$ES_change[which(GR_Results2100$ES_change<0)]
    GR_Results2100$n[which(GR_Results2100$ES_change<0)]
     GR_Results2100$n[which(GR_Results2100$ES_change>0)]

     glimpse(world)

    GR_Results$country <-(GR_Results$n)
    GR_Results$Continent <-(GR_Results$r5)
    
    
    GR_Results <- GR_Results[which(GR_Results$year <2101),]
    GR_Results$r5 <- (factor(GR_Results$r5))
    levels(GR_Results$r5) <- c("ASIA","LAM","MAF","OECD","REF")
    
    
    x_gdp <- GR_Results$GDP_change_perc[which(GR_Results$year %in% c(2100))]
    x_es <- GR_Results$ES_change[which(GR_Results$year %in% c(2100))]
    w <- GR_Results$pop[which(GR_Results$year %in% c(2100))]
    pop_mean_gdp_lpj <- weighted.mean(x=x_gdp,w=w)
    pop_mean_es_lpj <- weighted.mean(x=x_es,w=w)
    weighted_var_es <- sum(w * (x_es - pop_mean_es_lpj)^2) / sum(w)
    pop_se_es_lpj <- sqrt(weighted_var_es)
    weighted_var_gdp <- sum(w * (x_gdp - pop_mean_gdp_lpj)^2) / sum(w)
    pop_se_gdp_lpj <- sqrt(weighted_var_gdp)

    aggdp <-aggregate(GDP_change_perc~n+r5,data=GR_Results,FUN="sum")
    aggregate(GDP_change_perc~r5,data=aggdp,FUN="mean")

    #install.packages("ggbreak")
    library(ggbreak) 
    
    glimpse(GR_Results)
    
    #GR_Results_lpj_traj_noS <- GR_Results
    #save(GR_Results_lpj_traj_noS,file="Data/DataForFigures/GR_Results_lpj_traj_noS.Rds")
    plot_trajectory <- ggplot(GR_Results,
     aes(x=ES_change,y=GDP_change_perc,color=r5,group=n))+
     geom_hline(aes(yintercept=0))+
     geom_vline(aes(xintercept=0))+
      geom_line()+
       #geom_text(data = GR_Results[which(GR_Results$year %in% c(2100) ),], aes(x=ES_change,y=GDP_change_perc,color=r5,label=n),alpha=0.4)+ 

       geom_point(data = GR_Results[which(GR_Results$year %in% c(2050,2080,2100) ),], 
        aes(x=ES_change,y=GDP_change_perc,color=r5,shape=factor(year)),alpha=0.4)+
        #ymin=gdp_change-gdp_change_sd,ymax=gdp_change+gdp_change_sd))+
       theme_minimal()+
       geom_hline(aes(yintercept=pop_mean_gdp_lpj),linetype=2,alpha=0.9)+
       geom_vline(aes(xintercept=pop_mean_es_lpj),linetype=2,alpha=0.9)+
      geom_errorbar(data=GR_Results[which(GR_Results$year %in% c(2100)),],
        aes(ymin=(GDP_change_perc_lb), ymax=GDP_change_perc_ub),alpha=0.3)+
      geom_errorbar(data=GR_Results[which(GR_Results$year %in% c(2100)),],
        aes(xmin=(ES_change_lb), xmax=ES_change_ub),alpha=0.3)+
        #geom_errorbarh(data=all_sim[which(all_sim$exp=="RCP6" & all_sim$year %in% c(2100) ),],
        #aes(xmin=(es_change_minus_se)*100, xmax=(es_change_plus_se)*100),alpha=0.3)+
        theme(legend.position="bottom",legend.box="vertical")+
       labs(color="Region",shape="Year")+
       ylab("Annual GDP change (%)\n") + ggtitle("")  +
       #scale_y_break(c(-0.6, 0.1))+
       #coord_cartesian(xlim=c(-45,10),ylim=c(-0.6, 0.1)) +
       xlab("Annual non-market benefits change (%)")+
       scale_color_scico_d()

       plot_trajectory
  
       
       
       leg_trj <-  get_legend(plot_trajectory)

        maxyear <- aggregate(year~country, data=GR_Results,FUN="max")
        maxyear$countrymax <- paste0(maxyear$country,maxyear$year)
        GR_Results$countrymax <- paste0(GR_Results$country,GR_Results$year)

        all_simmax <- GR_Results[GR_Results$countrymax %in% maxyear$countrymax,]
    
    # plot_count_es <- ggplot(GR_Results[which(GR_Results$year==2100),], # & abs(all_sim_mean$es_change)>0.01
    #     aes(x=es_change*100,fill=Continent))+
    #     xlim(c(-45,10))+
    #     geom_histogram()+theme_minimal()+ xlab("")+ylab("count in 2100") #+xlim(-12,5)#+ xlim(-100,60)
    # plot_count_es
        
    ordered_GR <- GR_Results[which(GR_Results$year==2100),]
    ordered_GR <- ordered_GR[order(ordered_GR$ES_change),]

    totpop <- sum(GR_Results$pop[which(GR_Results$year==2100)])

    ordered_GR_lpj <- ordered_GR
    #save(ordered_GR_lpj,file="Data/DataForFigures/ordered_GR_lpj.Rds")
    plot_count_es <- ggplot(ordered_GR, #& abs(all_sim_mean$gdp_change)>0.01
        aes(x=(ES_change),y=cumsum(100*pop/totpop)))+
        #geom_bar(stat="identity")+
        geom_line(color="gray")+
        geom_point(aes(color=r5),shape=15)+
        xlim(c(-45,10))+
        theme_minimal()+ ylab("Cumulative \npopulation in 2100 (%)")+xlab('')+
       scale_color_scico_d()
    plot_count_es


    ordered_GR <- GR_Results[which(GR_Results$year==2100),]
    #ordered_GR$GDPpc <- ordered_GR$YGROSS_baseline/ordered_GR$pop
    ordered_GR$GDPpc <- ordered_GR$YGROSS2020*10^6/ordered_GR$pop2020
    ordered_GR <- ordered_GR[order(ordered_GR$GDPpc),]

    totGDP_change <- sum(GR_Results$GDP_change[which(GR_Results$year==2100)])
    ordered_GR$cumdam <- cumsum(100*ordered_GR$GDP_change/totGDP_change)

    plot_distribution_damages <- ggplot(ordered_GR, #& abs(all_sim_mean$gdp_change)>0.01
        aes(x=(GDPpc),y=cumdam))+
        #geom_bar(stat="identity")+
        geom_line(color="gray")+
        geom_point(aes(color=r5),shape=15)+
        #geom_vline(aes(xintercept=0.0815))+
        #geom_hline(aes(yintercept=90),linetype="dashed")+
        geom_vline(aes(xintercept=18668.173))+
        geom_hline(aes(yintercept=88.74806),linetype="dashed")+
        #xlim(c(-45,10))+
        labs(color="Region")+
        theme_minimal()+ ylab("Cumulative global GDP \ndamages in 2100 (% of total)")+xlab('GDP per capita in 2020 ($ per person)')+
       scale_color_scico_d()
    plot_distribution_damages
    ordered_GR_dam_lpj <- ordered_GR
    #save(ordered_GR_dam_lpj,file="Data/DataForFigures/ordered_GR_dam_lpj.Rds")
    #ggsave("Figures/Final figures/Submission 3/Distribution_damages.png",dpi=600)


    plot_count_gdp <- ggplot(GR_Results[which(GR_Results$year==2100),], #& abs(all_sim_mean$gdp_change)>0.01
        aes(x=gdp_change*100,fill=Continent))+xlab('')+#xlim(-15,6)+
        ylab("count in 2100")+
        geom_histogram()+theme_minimal()+coord_flip() +
       scale_color_scico_d()

    glimpse(GR_Results[which(GR_Results$year==2100),])


    plot_count_gdp <- ggplot(GR_Results[which(GR_Results$year==2100),], #& abs(all_sim_mean$gdp_change)>0.01
        aes(x=(GDP_change),y=(100*pop/totpop)))+
        #geom_bar(stat="identity")+
        geom_line(color="gray")+
        geom_point(aes(color=r5),shape=15)+
        theme_bw() + coord_flip() + ylab("Population in 2100 (%)")+ 
          #scale_y_break(c(-2.8, -2))+
      scale_x_continuous(trans = scales::pseudo_log_trans())+theme_minimal()+xlab('')+
       scale_color_scico_d()
    plot_count_gdp

    ordered_GR <- GR_Results[which(GR_Results$year==2100),]
    ordered_GR <- ordered_GR[order(-ordered_GR$GDP_change_perc),]
    plot_count_gdp_cum <- ggplot(ordered_GR, #& abs(all_sim_mean$gdp_change)>0.01
    aes(x=(GDP_change_perc),y=cumsum(100*pop/totpop)))+
    #geom_bar(stat="identity")+
    geom_line(color="gray")+
    geom_point(aes(color=r5),shape=15)+
    #xlim(c(-0.65,0.1))+ 
    #xlim(c(-2.8,0))+ 
    theme_bw() + coord_flip() + ylab("Cumulative\npopulation \nin 2100 (%)")+
    theme_minimal()+xlab('')+
       scale_color_scico_d()

    ordered_GR_lpj_gdp <- ordered_GR
    #save(ordered_GR_lpj_gdp,file="Data/DataForFigures/ordered_GR_lpj_gdp.Rds")

    plot_count_gdp_cum
    #install.packages("ggridges")
    #library("ggridges")



    emptyplot <- ggplot()+theme_void()
        
    traj_plot <- ggarrange(ggarrange(plot_trajectory, plot_count_gdp_cum,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="bottom"),
        ncol=1,nrow=2,common.legend=TRUE,heights=c(3,1),legend="bottom",align="hv")
  

     ggarrange(ggarrange(plot_trajectory, plot_count_gdp_cum,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="none"),as_ggplot(leg_trj),
        ncol=1,nrow=3,common.legend=TRUE,heights=c(4,1,1),legend="none",align="hv")

        #ggsave("Figures/Final figures/Submission 3/Trajectory_percent_noS.png",dpi=600)
        #ggsave("Figures/Final figures/Submission 3/Fig4_pre.png",dpi=600)
    #ggarrange(plot_trajectory, results2100lpj_noS,ncol=1,heights=c(3,1))
    #ggsave("Figures/Final figures/Submission 3/Trajectory_percent_noS.png",dpi=600)
    
    trajlpj <- plot_trajectory+geom_text(data = GR_Results[which(GR_Results$year %in% c(2100) ),],   aes(x=ES_change,y=GDP_change_perc,color=r5,label=n))
        trajlpj
