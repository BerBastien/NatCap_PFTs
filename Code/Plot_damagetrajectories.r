plot_trajectory <- ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" ),],
     aes(x=es_change*100,y=gdp_change*100,color=Continent,group=country))+
     geom_hline(aes(yintercept=0),linetype="dashed")+
     geom_vline(aes(xintercept=0),linetype="dashed")+
       geom_line()+
       geom_point(data = all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year %in% c(2050,2080,2100) ),], 
        aes(x=es_change*100,y=gdp_change*100,color=Continent,shape=factor(year)))+
        #ymin=gdp_change-gdp_change_sd,ymax=gdp_change+gdp_change_sd))+
       theme_bw()+
        geom_errorbar(data=all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year %in% c(2050,2080,2100) ),],
        aes(ymin=(gdp_change-gdp_change_sd)*100, ymax=(gdp_change+gdp_change_sd)*100),alpha=0.2)+
        geom_errorbarh(data=all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year %in% c(2050,2080,2100) ),],
        aes(xmin=(es_change-es_change_sd)*100, xmax=(es_change+es_change_sd)*100),alpha=0.2)+
        theme(legend.position="bottom",legend.box="vertical")+
       labs(color="Region",shape="Year")+
       xlab("Ecosystem services change (%)") +
       ylab("GDP change (%)") + ggtitle("")  +
        coord_cartesian(xlim=c(-100, 60),ylim=c(-15,6))
       plot_trajectory

      leg_trj <-  get_legend(plot_trajectory)
    
plot_count_es <- ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100),], # & abs(all_sim_mean$es_change)>0.01
     aes(x=es_change*100,fill=Continent))+
     geom_histogram()+theme_minimal()+ xlab("")+ylab("count in 2100") + xlim(-100,60)


plot_count_gdp <- ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100 ),], #& abs(all_sim_mean$gdp_change)>0.01
     aes(x=gdp_change*100,fill=Continent))+xlab('')+xlim(-15,6)+ylab("count in 2100")+
     geom_histogram()+theme_minimal()+coord_flip() 

    emptyplot <- ggplot()+theme_void()
        
    traj_plot <- ggarrange(ggarrange(plot_trajectory, plot_count_gdp,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="bottom"),
        ncol=1,nrow=2,common.legend=TRUE,heights=c(3,1),legend="bottom",align="hv")
    
    annotate_figure(traj_plot, top = text_grob("Benefits change under SSP2-4.5", 
                face = "bold", size = 14))
    