#Plot Manuscript Figures
dir1 <- "C:/Users/basti/Box/VegetationData/"
setwd('C:\\Users\\basti\\Documents\\GitHub\\NatCap_PFTs')
x <- c('raster',"hacksaw", 'ggpubr',"tidyverse",'dplyr','ncdf4','ggplot2','countrycode','tidyverse','RColorBrewer','colorspace','spData','sf','lfe','marginaleffects','rgdal',"rnaturalearth","scico","writexl","ggsci","cowplot")
lapply(x, require, character.only = TRUE)
library("dplyr")
my.palette <- brewer.pal(n = 10, name = "BrBG")
`%notin%` <- Negate(`%in%`)
vector_lpj_names <- c("Boreal summergreen \n needleleaved",
        "Boreal evergreen \n needleleaved",
        "Boreal evergreen \n needleleaved (shade-intolerant)",
        "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
        "Boreal/Temperate grass" ,
        "Temperate broadleaved \n summergreen",   "Temperate  broadleaved \n  evergreen",
        "Tropical broadleaved \n  raingreen","Tropical Grass", "Tropical broadleaved \n  evergreen (shade-intolerant)", "Tropical broadleaved \n  evergreen")

vector_car_names

pal_boreal <- colorRampPalette(brewer.pal(4, "PuBu"))
                    pal_temp <- colorRampPalette(brewer.pal(4, "RdPu"))
                    pal_trop <- colorRampPalette(brewer.pal(4, "Greens"))
                    pal <- palette(brewer.pal(n = 3, name = "Spectral"))
pal <- brewer.pal(n = 3, name = "RdYlBu")
    
    ## Figure 1
        
        load(file="Data/DataForFigures/ww.Rds")
        scaleFUN <- function(x) sprintf("%.4f", x)
        ww_subset <- ww %>% filter(year==2018) %>% dplyr::select(mN,nN,r5,GDP,Population,countryname)
        ww_subset$nN <- ww_subset$nN*100
        ww_subset$mN <- ww_subset$mN*100

        plot <- ggplot(ww_subset,aes(x=nN,y=mN))+
        theme_bw() +
        geom_density_2d_filled(contour_var = "ndensity",aes(fill=r5,alpha=..level..),bins=4,size=1)+
        scale_alpha_discrete(range = c(0,0.9,1,1),guide = guide_none()) +
        geom_point(aes(x=nN,y=mN,size=GDP/Population),alpha=0.5)+
        scale_y_continuous(trans="log2",labels=scaleFUN)+
        scale_x_continuous(trans="log2",labels=scaleFUN)+
        xlab("Non-market natural capital \n(% of Total Wealth)") + 
        ylab("Market natural capital \n(% of Total Wealth)") + 
        #scale_fill_scico_d(name="Region")+
        scale_fill_npg(name="Region")+
        scale_size_continuous(name="GDP per \ncapita in 2018")

        #ggsave("Figures/ManuscriptFigures/F1_vector.png",dpi=600)
        combined_plot <- ggarrange(ggarrange(ggplot()+theme_void(),plot,ncol=2,widths=c(1,2)),ggplot()+theme_void(),ncol=1,heights=c(2,1))
        combined_plot
        ggsave(filename="Figures/ManuscriptFigures/F1_vector.pdf", plot=combined_plot, width=10, height=7)
        #write.csv(ww_subset,"Figures/ManuscriptFigures/SourceData/F1_NatCap_R5.csv")
    ## Figure 1


    ## Figure 2
        load(file="Data/DataForFigures/PFTs11_horizons3.Rds")
        #levels(factor(PFTs11_horizons3$PFT))
        
        
                    
        Present_dist_col <- ggplot(PFTs11_horizons3)+
            geom_col(aes(y,Percent*100,fill=PFT))+
            scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
            theme_minimal() + xlab("Latitude (Degrees)") + ylab("Average pixel cover (%)")+
            ggtitle("Present cover") + ggtitle(expression(bold("a.") ~ " Present cover"))+
            guides(fill=guide_legend(title="", ncol=3))+
            coord_flip()+theme(legend.text=element_text(size=7))+theme(legend.key.size = unit(0.5, "cm"))
        
        
        load("C:\\Users\\basti\\Documents\\GitHub\\BiomeShifts_NaturalCapital\\Data\\DataForFigures\\PFT_change.Rds")
        #write_xlsx(list(PanelA = PFTs11_horizons3, PanelB = PFT_change), "Figures/ManuscriptFigures/SourceData/Fig2.xlsx") 
        PFT_change <- read_xlsx("Figures/ManuscriptFigures/SourceData/Fig2.xlsx",sheet="PanelB") 
            
        pft_mov_change <- ggplot(PFT_change)+
            geom_col(aes(yPresent,Future_Change*100,fill=PFT2C))+
            geom_hline(yintercept=0)+
            ggtitle("Biome Cover Change")+ggtitle(expression(bold("b.") ~ " Biome Cover Change"))+
            theme_minimal() + xlab("") +
            theme(plot.title = element_text(hjust = 0.5)) + 
            ylab("Cover change (pp)") +
            scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
            guides(fill=guide_legend(title="", ncol=3))+
            coord_flip()+theme(legend.text=element_text(size=7))+theme(legend.key.size = unit(0.5, "cm"))
        
        ggarrange(Present_dist_col,pft_mov_change,ncol=2,common.legend=TRUE,legend="bottom")

        
        load(file="Data/DataForFigures/pft_sum_poly_rob4_lpj.Rds")
        world_coast <- ne_coastline(scale = "medium", returnclass = "sf")
        
        d_area <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob4_lpj, aes(fill = 100 * layer), color = NA) +
            geom_sf(data = world_coast) +
            scale_fill_gradientn(
                colours = pal, limits = c(-10, 10), na.value = "transparent",name="Percent Change",labels=c("-10<","-5","0","5",">10"),
                guide = guide_colorbar(
                title.position = "top", title.hjust = 0.5, title.vjust = 1,
                label.position = "bottom", label.hjust = 0.5, label.vjust = 0,
                label.theme = element_text(size = 10),
                title.theme = element_text(size = 12)
                )
            ) +
            ggtitle(expression(bold("c.") ~ " Total Covered Area Change")) +
            theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
        

        load("Data/DataForFigures/cveg_sum_poly_rob4_lpj.Rds")
        dcveg <- ggplot() +
            theme_void() +
            geom_sf(data = cveg_sum_poly_rob4_lpj,aes(fill = 100*layer), color = NA) +
            geom_sf(data = world_coast)+
            scale_fill_gradientn(colours = pal,limits=c(-10,10),
            na.value="transparent",name="Percent Change", labels=c("-10<","-5","0","5",">10"),
            oob = scales::squish)+
            geom_sf(data = world_coast)+
            ggtitle("Vegetation Carbon Content Change")+ggtitle(expression(bold("d.") ~ " Vegetation Carbon Content Change"))+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 
            

        combined_plot <- ggarrange(ggarrange(Present_dist_col,pft_mov_change,ncol=2,common.legend=TRUE,legend="bottom"),ggarrange(d_area,dcveg,nrow=2,common.legend=TRUE,legend="bottom"),ncol=2)
        combined_plot
        #ggsave("Figures/ManuscriptFigures/Fig2.png",dpi=600)
        #ggsave(filename="Figures/ManuscriptFigures/Fig2.pdf", plot=combined_plot, width=10, height=7)



    ## Figure 2

    ## Figure 3
        
        load("Data/DataForFigures/pft_es_lpj.Rds") 
        #levels(factor(pft_es_lpj$Biome))
        load("Data/DataForFigures/ES_val_lpj5.Rds")    
        load("Data/DataForFigures/ES_val_lpj6.Rds")      
        load("Data/DataForFigures/map_coef.Rds")  
        map_coef_lpj1 <- map_coef %>% filter(formula=="lin",dgvm=="lpj",capital=="mN")      
        map_coef_lpj2 <- map_coef %>% filter(formula=="lin",dgvm=="lpj",capital=="nN")      

        pft_es_lpj$Benefits <- pft_es_lpj$Benefits/100
        pft_es_lpj <- pft_es_lpj %>% dplyr::select(Benefits,Biome,Type)
        glimpse(pft_es_lpj)     
        glimpse(ES_val_lpj5)   
        
        ESValplot1 <- ggplot(pft_es_lpj)+
                geom_boxplot(aes(x=(Benefits),y=Biome,
                    fill=Type,middle=median((Benefits)))) +
                theme_bw()+ coord_cartesian(xlim=c(0,200))+
                theme(axis.title.y = element_blank(), 
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank())+xlab("")+ labs(title =expression(bold("a.") ~ " Benefits per area ($/ha/year)")) +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Non-Market benefits")+
                guides(fill = FALSE)
        ESValplot1


        
        levels(ES_val_lpj5$PFT_code) <- vector_lpj_names
        ES_val_lpj5$TotalValES_gdp <- 100*ES_val_lpj5$TotalValES_gdp
        ES_val_lpj5 <- ES_val_lpj5 %>% dplyr::select(PFT_code,TotalValES_gdp,region_un)
        
        ESValplot <-  ggplot(ES_val_lpj5,aes(x=region_un,y=TotalValES_gdp,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                                    geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
                                    geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("") + ggtitle("Ecosystem services")+
                                guides(fill = guide_legend(reverse=FALSE)) + ylim(c(0,26))+theme(legend.key.size = unit(0.3, "cm"),
                                    legend.title = element_blank())+theme(legend.text=element_text(size=6))+ labs(title ="") +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Non-Market benefits")
        
        levels(ES_val_lpj6$PFT_code) <- vector_lpj_names
        ES_val_lpj6$TotalValGDP_gdp <- 100*ES_val_lpj6$TotalValGDP_gdp
        ES_val_lpj6 <- ES_val_lpj6 %>% dplyr::select(PFT_code,TotalValGDP_gdp,region_un)

        GDPValplot <-  ggplot(ES_val_lpj6,aes(x=region_un,y=TotalValGDP_gdp,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                                    geom_bar(stat="identity")+ scale_fill_manual(labels=vector_lpj_names,values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
                                    geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("") + ggtitle("Market Goods")+
                                guides(fill = guide_legend(reverse=FALSE)) + ylim(c(0,26))+theme(legend.key.size = unit(0.4, "cm"),
                                    legend.title = element_blank())+theme(legend.text=element_text(size=9))+ labs(title = expression(bold("b.") ~ " Total Benefits ($/year)")) +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Market benefits")
        leg1<-get_legend(GDPValplot)
        
        
        write_xlsx(list(PanelA = pft_es_lpj, PanelB1 = ES_val_lpj5, PanelB2 = ES_val_lpj6), "Figures/ManuscriptFigures/SourceData/Fig3.xlsx") 
               
        Map_MarketDam <-  ggplot(map_coef_lpj1) +
                theme_void()+
                geom_sf(data=world,aes(geometry = geometry),fill="white")+
                geom_sf(aes(geometry = geom, fill =100* coef)) +
                scale_fill_gradientn(colours = pal,limits=c(-010,010),
                    na.value="transparent",name="Damage at 1C (%)",oob = scales::squish)+
                theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
                theme(legend.title = element_blank())+ 
                labs(title = expression(bold("c.") ~ " Percent Damage at 1 Degree of Warming")) +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Market Natural Capital")

        Map_NonmarketDam <-  ggplot(map_coef_lpj2) +
                theme_void()+
                geom_sf(data=world,aes(geometry = geometry),fill="white")+
                geom_sf(aes(geometry = geom, fill =100* coef)) +
                scale_fill_gradientn(colours = pal,limits=c(-010,010),
                    na.value="transparent",name="Damage at 1C (%)",oob = scales::squish)+
                theme(legend.position = c(0.1, 0.5), # Centered horizontally, at the very top vertically
                      legend.justification = c(0.1, 0.5), # Reference point: center of the legend box at its top
                      legend.background = element_rect(fill = "transparent", color = NA), # Remove legend box background
                      legend.key.size = unit(0.3, "cm"),legend.key = element_rect(fill = "transparent", color = "transparent"), # Remove key box background
                      plot.title = element_text(hjust = 0.5)) +
                      ggtitle("Non-market NC damage coefficient")+theme(legend.title = element_blank())+ 
                labs(title = "") +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Non-market Natural Capital")

        
        combined_plot <- ggarrange(ggarrange(ggarrange(ESValplot1,leg1,widths=c(1,1),align="v"),ggarrange(GDPValplot,ESValplot,ncol=2,common.legend=TRUE,legend="none")),
        ggarrange(Map_MarketDam,Map_NonmarketDam,ncol=2,widths=c(1,1)),heights=c(6,5),ncol=1)
        combined_plot


        

        
        #ggsave("Figures/ManuscriptFigures/Fig3.png",dpi=600)
        #ggsave(filename="Figures/ManuscriptFigures/Fig3.pdf", plot=combined_plot, width=10, height=6.8)
        
        
                
    ## Figure 3

    ## Figure 4
        
        load("Data/DataForFigures/GR_Results_lpj_traj.Rds")
        load("Data/DataForFigures/ordered_GR_lpj.Rds")
        load(file="Data/DataForFigures/ordered_GR_lpj_gdp.Rds")

        GR_Results <- GR_Results_lpj_traj
        totpop <- sum(GR_Results$pop[which(GR_Results$year==2100)])
        x_gdp <- GR_Results_lpj_traj$GDP_change_perc[which(GR_Results_lpj_traj$year %in% c(2100))]
        x_es <- GR_Results_lpj_traj$ES_change[which(GR_Results_lpj_traj$year %in% c(2100))]
        w <- GR_Results_lpj_traj$pop[which(GR_Results_lpj_traj$year %in% c(2100))]
        pop_mean_gdp_lpj <- weighted.mean(x=x_gdp,w=w)
        pop_mean_es_lpj <- weighted.mean(x=x_es,w=w)
    
        GR_Results_lpj_traj_source <- GR_Results_lpj_traj %>% select(ES_change,GDP_change_perc,r5,n,year,ES_change_lb,ES_change_ub,GDP_change_perc_ub,GDP_change_perc_lb)
        plot_trajectory <- ggplot(GR_Results_lpj_traj,
            aes(x=ES_change,y=GDP_change_perc,color=r5,group=n))+
            geom_hline(aes(yintercept=0))+
            geom_vline(aes(xintercept=0))+
            geom_line()+
            geom_point(data = GR_Results[which(GR_Results$year %in% c(2050,2080,2100) ),], 
                aes(x=ES_change,y=GDP_change_perc,color=r5,shape=factor(year)),alpha=0.4)+
            theme_minimal()+
            geom_hline(aes(yintercept=pop_mean_gdp_lpj),linetype=2,alpha=0.9)+
            geom_vline(aes(xintercept=pop_mean_es_lpj),linetype=2,alpha=0.9)+
            geom_errorbar(data=GR_Results[which(GR_Results$year %in% c(2100)),],
                aes(ymin=(GDP_change_perc_lb), ymax=GDP_change_perc_ub),alpha=0.3)+
            geom_errorbar(data=GR_Results[which(GR_Results$year %in% c(2100)),],
                aes(xmin=(ES_change_lb), xmax=ES_change_ub),alpha=0.3)+
                theme(legend.position="bottom",legend.box="vertical")+
            labs(color="Region",shape="Year")+
            ylab("Annual GDP change (%)\n") + ggtitle("")  +
            xlab("Annual non-market benefits change (%)")+
            #scale_color_scico_d()+
            scale_color_npg()+xlim(c(-45,10))
        plot_trajectory_lpj <- plot_trajectory
        leg_trj <- get_legend(plot_trajectory_lpj)
        trajlpj <- plot_trajectory_lpj+geom_text(data = GR_Results[which(GR_Results$year %in% c(2100) ),],   aes(x=ES_change,y=GDP_change_perc,color=r5,label=n))
        
        plot_count_es <- ggplot(ordered_GR_lpj,
            aes(x=(ES_change),y=cumsum(100*pop/totpop)))+
            geom_line(color="gray")+
            geom_point(aes(color=r5),shape=15)+
            xlim(c(-45,10))+
            theme_minimal()+ ylab("Cumulative \npopulation in 2100 (%)")+xlab('')+
            #scale_color_scico_d()+
            scale_color_npg()


        plot_count_gdp_cum <- ggplot(ordered_GR_lpj_gdp,
            aes(x=(GDP_change_perc),y=cumsum(100*pop/totpop)))+
            geom_line(color="gray")+
            geom_point(aes(color=r5),shape=15)+
            theme_bw() + coord_flip() + ylab("Cumulative\npopulation \nin 2100 (%)")+
            theme_minimal()+xlab('')+
            #scale_color_scico_d()+
            scale_color_npg()

        
        #write_xlsx(list(PanelA = GR_Results_lpj_traj_source, PanelB = ordered_GR_lpj_gdp, PanelC = ordered_GR_lpj), "Figures/ManuscriptFigures/SourceData/Fig4.xlsx") 


        emptyplot <- ggplot()+theme_void()
        
        traj_plot <- ggarrange(ggarrange(plot_trajectory, plot_count_gdp_cum,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
            ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="bottom"),
            ncol=1,nrow=2,common.legend=TRUE,heights=c(3,1),legend="bottom",align="hv")
    

        combined_plot <- ggarrange(ggarrange(plot_trajectory, plot_count_gdp_cum,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
            ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="none"),as_ggplot(leg_trj),
            ncol=1,nrow=3,common.legend=TRUE,heights=c(4,1,1),legend="none",align="hv")


        
        combined_plot   

        combined_plot1 <- ggarrange(plot_trajectory, plot_count_gdp_cum,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv")

        combined_plot2 <- 
            ggarrange(ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="none"),as_ggplot(leg_trj),
            ncol=1,nrow=2,common.legend=TRUE,legend="none",align="hv")


   


                # Create text labels
        labels <- list(
        c("a.", ""),
        c("b.", ""),
        c("c.", "")
        )

        # Create a list of empty plots with the labels as annotations
        label_plots <- lapply(labels, function(lbl) {
        ggdraw() +
            draw_label(lbl, size = 12, fontface = "bold")
        })

        # Combine label plots with the combined_plot
        final_plot <- plot_grid(
        plot_grid(label_plots[[1]],NULL, NULL,label_plots[[2]], ncol = 4, rel_widths = c(0.1, 0.9, 0.2, 0.8)),
        combined_plot,
        plot_grid(NULL, NULL, NULL, label_plots[[3]], ncol = 4, rel_widths = c(0.1, 0.9, 0.2, 0.8)),
        ncol = 1, rel_heights = c(0.05, 1, 0.05)
        )

        # Combine label plots with the combined_plot
        final_plot <- plot_grid(
        plot_grid(NULL, label_plots[[1]],NULL,label_plots[[2]], ncol = 4, rel_widths = c(0.05, 0.1, 0.25, 0.2)),
        combined_plot1,
        plot_grid(NULL,label_plots[[3]], NULL, NULL, ncol = 4, rel_widths = c(0.05, 0.1, 0.25, 0.2)),
        combined_plot2,
        ncol = 1, rel_heights = c(0.1, 3.1, 0.1,2)
        )

        print(final_plot)      
        #ggsave("Figures/ManuscriptFigures/Fig4.png",dpi=600)
        #ggsave(filename="Figures/ManuscriptFigures/Fig4.pdf", plot=final_plot, width=7, height=7)



    ## Figure 4

    ## Figure ED 2
        load(file="Data/DataForFigures/elas_data.Rds")
        dat <- elas_data
        datleft <- dat %>% filter(Cat2=="nonmarket") %>% select(cveg,Single.Value.Converted)
            # Create scatter plot with regression line
            elas_cveg <- ggplot(dat[which(dat$Cat2=="nonmarket" ),], aes(x = log(cveg), y = log(Single.Value.Converted))) +
            geom_point() +
            geom_smooth(method = "lm", color = "blue") +
            theme_bw()+
            xlab("Log vegetation carbon (kg/m2)") +
            ylab("Log value per hectare ($/ha)")

            
        datright<- dat %>% filter(Cat2=="nonmarket",Spatial.Extent>0) %>% select(cveg,Spatial.Extent)
        elas_area <- ggplot(dat[which(dat$Cat2=="nonmarket"& dat$Spatial.Extent>0),], aes(x = log(Spatial.Extent), y = log(Single.Value.Converted))) +
            geom_point() +
            geom_smooth(method = "lm", color = "blue") +
            theme_bw() +
            xlab("Log area (ha)") +
            ylab("Log value per hectare ($/ha)")

        #write_xlsx(list(Left = datleft, Right = datright), "Figures/ManuscriptFigures/SourceData/FigED2.xlsx") 


        final_plot <- ggarrange(elas_area,elas_cveg)
        final_plot
        #ggsave("Figures/ManuscriptFigures/FigED2.jpeg",dpi=600)
        #ggsave(filename="Figures/ManuscriptFigures/FigED2.pdf", plot=final_plot, width=10, height=3)
    ## Figure ED 2


    ## Figure ED 3
            results_df <- read.csv("Data/Damage_coef_Submission3v2_06052023.csv")
            
            
            results_df_sub_1<- results_df %>% filter(formula=="lin",capital=="mN",dgvm!="all") %>% select(pval,dgvm)
            pvalm <- ggplot(results_df_sub_1, aes(pval)) +
            geom_histogram(aes(fill=factor(dgvm)),breaks=c(0,0.01,0.05,0.1,(0.1+seq(1:90)*0.01)))+
            theme_bw() + ggtitle("Market NC damage function coefficient")+ xlab ("") + 
            geom_vline(aes(xintercept=0.01),linetype="dashed")+xlim(0,1)+
            annotate("text", x=0.025, y=140, label="0.01", angle=90,size=3)+ 
            geom_vline(aes(xintercept=0.05),linetype="dashed")+
            annotate("text", x=0.065, y=130, label="0.05", angle=90,size=3)+ 
            geom_vline(aes(xintercept=0.1),linetype="dashed")+
            annotate("text", x=0.115, y=110, label="0.1", angle=90,size=3) + ylab("count")+ xlab("p-value")+
            guides(fill=guide_legend(title = "DGVM"))+
            scale_fill_scico_d(palette = 'berlin')

            results_df_sub <- results_df %>% filter(formula=="lin",capital=="nN",dgvm!="all") %>% select(pval,dgvm)
            pvaln <- ggplot(results_df_sub, aes(pval))+
            geom_histogram(aes(fill=factor(dgvm)),breaks=c(0,0.01,0.05,0.1,(0.1+seq(1:90)*0.01)))+
            theme_bw() + ggtitle("Non-market NC damage function coefficient")+ xlab ("") + 
            geom_vline(aes(xintercept=0.01),linetype="dashed")+xlim(0,1)+
            annotate("text", x=0.025, y=140, label="0.01", angle=90,size=3)+ 
            geom_vline(aes(xintercept=0.05),linetype="dashed")+
            annotate("text", x=0.065, y=130, label="0.05", angle=90,size=3)+ 
            geom_vline(aes(xintercept=0.1),linetype="dashed")+
            annotate("text", x=0.115, y=110, label="0.1", angle=90,size=3) + ylab("count")+ xlab("p-value")+
            guides(fill=guide_legend(title = "DGVM"))+
            scale_fill_scico_d(palette = 'berlin')

            combined_plot <- ggarrange(pvalm,pvaln,common.legend=TRUE,nrow=2,legend="bottom")


            combined_plot 
            #ggsave("Figures/ManuscriptFigures/FigED3.jpeg",dpi=600)
            #ggsave(filename="Figures/ManuscriptFigures/FigED3.pdf", plot=combined_plot, width=5, height=7)
        
            #write_xlsx(list(Top = results_df_sub_1,Bottom = results_df_sub), "Figures/ManuscriptFigures/SourceData/FigED3.xlsx") 
            
    ## Figure ED 3
    
    
    ## Figure ED 4
        load(file="Data/DataForFigures/PFTs11_horizons3_car.Rds")
        load(file="Data/DataForFigures/PFT_change_car.Rds")      

        levels(factor(PFTs11_horizons3_car$PFT))
        


        Present_dist_col <- ggplot(PFTs11_horizons3_car)+
            geom_col(aes(y,Percent*100,fill=PFT))+
             scale_fill_manual(values = c(pal_boreal(10),pal_temp(6), pal_trop(10)))+
            theme_minimal() + xlab("Latitude (Degrees)") + ylab("Average pixel cover (%)")+
            ggtitle("Present cover") + ggtitle(expression(bold("a.") ~ " Present cover"))+
            guides(fill=guide_legend(title="", ncol=4))+
            coord_flip()+theme(legend.text=element_text(size=7))+theme(legend.key.size = unit(0.5, "cm"))
        
            
        pft_mov_change <- ggplot(PFT_change_car)+
            geom_col(aes(yPresent,Future_Change*100,fill=PFT2C))+
            geom_hline(yintercept=0)+
            ggtitle("Biome Cover Change")+ggtitle(expression(bold("b.") ~ " Biome Cover Change"))+
            theme_minimal() + xlab("") +
            theme(plot.title = element_text(hjust = 0.5)) + 
            ylab("Cover change (pp)") +
             scale_fill_manual(values = c(pal_boreal(10),pal_temp(6), pal_trop(10)))+
            guides(fill=guide_legend(title="", ncol=4))+
            coord_flip()+theme(legend.text=element_text(size=7))+theme(legend.key.size = unit(0.5, "cm"))
        ggarrange(Present_dist_col,pft_mov_change,ncol=2,common.legend=TRUE,legend="bottom")
        
        #write_xlsx(list(PanelA = PFTs11_horizons3_car, PanelB = PFT_change_car), "Figures/ManuscriptFigures/SourceData/FigED4.xlsx")


        
        load(file="Data/DataForFigures/pft_sum_poly_rob4_car.Rds")
        world_coast <- ne_coastline(scale = "medium", returnclass = "sf")
        d_area <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob4_car,aes(fill = 100*layer), color = NA) +
            geom_sf(data = world_coast)+
            scale_fill_gradientn(
                colours = pal, limits = c(-10, 10), na.value = "transparent",name="Percent Change",labels=c("-10<","-5","0","5",">10"),
                oob = scales::squish,
                guide = guide_colorbar(
                title.position = "top", title.hjust = 0.5, title.vjust = 1,
                label.position = "bottom", label.hjust = 0.5, label.vjust = 0,
                label.theme = element_text(size = 10),
                title.theme = element_text(size = 12)
                )
            )+geom_sf(data = world_coast)+
            ggtitle("Total Covered Area Change")+ggtitle(expression(bold("c.") ~ " Total Covered Area Change"))+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))
        

        load("Data/DataForFigures/cveg_sum_poly_rob4_car.Rds")
        dcveg <- ggplot() +
            theme_void() +
            geom_sf(data = cveg_sum_poly_rob4_car,aes(fill = 100*layer), color = NA) +
            geom_sf(data = world_coast)+
           scale_fill_gradientn(
                colours = pal, limits = c(-10, 10), na.value = "transparent",name="Percent Change",labels=c("-10<","-5","0","5",">10"),
                oob = scales::squish,
                guide = guide_colorbar(
                title.position = "top", title.hjust = 0.5, title.vjust = 1,
                label.position = "bottom", label.hjust = 0.5, label.vjust = 0,
                label.theme = element_text(size = 10),
                title.theme = element_text(size = 12)
                )
            )+geom_sf(data = world_coast)+
            ggtitle("Vegetation Carbon Content Change")+ggtitle(expression(bold("d.") ~ " Vegetation Carbon Content Change"))+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 
            

        combined_plot <- ggarrange(ggarrange(Present_dist_col,pft_mov_change,ncol=2,common.legend=TRUE,legend="bottom"),ggarrange(d_area,dcveg,nrow=2,common.legend=TRUE,legend="bottom"),ncol=2)
        combined_plot 
        #ggsave("Figures/ManuscriptFigures/FigED4.jpeg",dpi=600)
        #ggsave(filename="Figures/ManuscriptFigures/FigED4.pdf", plot=combined_plot, width=10, height=6)

    ## Figure ED 4


    ## Figure ED 5
        load(file="Data/DataForFigures/PFTs11_horizons3_orc.Rds")
        #levels(factor(PFTs11_horizons3_orc$PFT))
        #glimpse(PFTs11_horizons3_orc)
        load(file="Data/DataForFigures/PFT_change_orc.Rds")  
        #levels(factor(PFT_change_orc$PFT))   
        #glimpse(PFT_change_orc)
        
        write_xlsx(list(PanelA = PFTs11_horizons3_orc, PanelB = PFT_change_orc), "Figures/ManuscriptFigures/SourceData/FigED5.xlsx") 

        Present_dist_col <- ggplot(PFTs11_horizons3_orc)+
            geom_col(aes(y,Percent*100,fill=PFT))+
            scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
            theme_minimal() + xlab("Latitude (Degrees)") + ylab("Average pixel cover (%)")+
            ggtitle("Present cover") + ggtitle(expression(bold("a.") ~ " Present cover"))+
            guides(fill=guide_legend(title="", ncol=3))+
            coord_flip()+theme(legend.text=element_text(size=7))+theme(legend.key.size = unit(0.5, "cm"))
            
        pft_mov_change <- ggplot(PFT_change_orc)+
            geom_col(aes(yPresent,Future_Change*100,fill=PFT2C))+
            geom_hline(yintercept=0)+
            ggtitle("Biome Cover Change")+ggtitle(expression(bold("b.") ~ " Biome Cover Change"))+
            theme_minimal() + xlab("") +
            theme(plot.title = element_text(hjust = 0.5)) + 
            ylab("Cover change (pp)") +
            scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
            guides(fill=guide_legend(title="", ncol=3))+
            coord_flip()+theme(legend.text=element_text(size=7))+theme(legend.key.size = unit(0.5, "cm"))
        ggarrange(Present_dist_col,pft_mov_change,ncol=2,common.legend=TRUE,legend="bottom")

        
        load(file="Data/DataForFigures/pft_sum_poly_rob4_orc.Rds")
        world_coast <- ne_coastline(scale = "medium", returnclass = "sf")
        d_area <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob4_orc,aes(fill = 100*layer), color = NA) +
            geom_sf(data = world_coast)+
            scale_fill_gradientn(
                colours = pal, limits = c(-10, 10), na.value = "transparent",name="Percent Change",labels=c("-10<","-5","0","5",">10"),
                oob = scales::squish,
                guide = guide_colorbar(
                title.position = "top", title.hjust = 0.5, title.vjust = 1,
                label.position = "bottom", label.hjust = 0.5, label.vjust = 0,
                label.theme = element_text(size = 10),
                title.theme = element_text(size = 12)
                )
            )+geom_sf(data = world_coast)+
            ggtitle("Total Covered Area Change")+ggtitle(expression(bold("c.") ~ " Total Covered Area Change"))+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))
        

        load("Data/DataForFigures/cveg_sum_poly_rob4_orc.Rds")
        dcveg <- ggplot() +
            theme_void() +
            geom_sf(data = cveg_sum_poly_rob4_orc,aes(fill = 100*layer), color = NA) +
            geom_sf(data = world_coast)+
            scale_fill_gradientn(
                colours = pal, limits = c(-10, 10), na.value = "transparent",name="Percent Change",labels=c("-10<","-5","0","5",">10"),
                oob = scales::squish,
                guide = guide_colorbar(
                title.position = "top", title.hjust = 0.5, title.vjust = 1,
                label.position = "bottom", label.hjust = 0.5, label.vjust = 0,
                label.theme = element_text(size = 10),
                title.theme = element_text(size = 12)
                )
            )+geom_sf(data = world_coast)+
            ggtitle("Vegetation Carbon Content Change")+ggtitle(expression(bold("d.") ~ " Vegetation Carbon Content Change"))+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 
            

        combined_plot <- ggarrange(ggarrange(Present_dist_col,pft_mov_change,ncol=2,common.legend=TRUE,legend="bottom"),ggarrange(d_area,dcveg,nrow=2,common.legend=TRUE,legend="bottom"),ncol=2)
        combined_plot 
        #ggsave("Figures/ManuscriptFigures/FigED5.jpeg",dpi=600)
        #ggsave(filename="Figures/ManuscriptFigures/FigED5.pdf", plot=combined_plot, width=10, height=6.5)

    ## Figure ED 5

    ## Figure ED 6
        load("Data/DataForFigures/pft_es_car.Rds") 
        load("Data/DataForFigures/ES_val_car5.Rds")    
        load("Data/DataForFigures/ES_val_car6.Rds")      
        load("Data/DataForFigures/map_coef.Rds")  
        map_coef_car1 <- map_coef %>% filter(formula=="lin",dgvm=="car",capital=="mN")      
        map_coef_car2 <- map_coef %>% filter(formula=="lin",dgvm=="car",capital=="nN")      

        pft_es_car$Benefits <- pft_es_car$Benefits/100
        pft_es_car <- pft_es_car %>% select(Benefits,Biome,Type)
        
        ESValplot1 <- ggplot(pft_es_car)+
                geom_boxplot(aes(x=(Benefits),y=Biome,
                    fill=Type,middle=median((Benefits)))) +
                theme_bw()+ coord_cartesian(xlim=c(0,50))+
                theme(axis.title.y = element_blank(), 
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank())+xlab("")+ labs(title =expression(bold("a.") ~ " Benefits per area ($/ha/year)")) +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Non-Market benefits")+
                guides(fill = FALSE)
        ESValplot1


        
        #levels(ES_val_car5$PFT_code) <- vector_car_names
        ES_val_car5$TotalValES_gdp <- 100*ES_val_car5$TotalValES_gdp
        ES_val_car5 <- ES_val_car5 %>% select(PFT_code,TotalValES_gdp,region_un)
        
        ESValplot <-  ggplot(ES_val_car5,aes(x=region_un,y=TotalValES_gdp,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                                    geom_bar(stat="identity")+ scale_fill_manual(values =  c(pal_boreal(10),pal_temp(6), pal_trop(10)))+
                                    geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("") + ggtitle("Ecosystem services")+
                                guides(fill = guide_legend(reverse=FALSE)) + ylim(c(0,26))+theme(legend.key.size = unit(0.4, "cm"),
                                    legend.title = element_blank())+theme(legend.text=element_text(size=6))+ labs(title ="") +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Non-Market benefits")
        
        #levels(ES_val_car6$PFT_code) <- vector_car_names
        ES_val_car6$TotalValGDP_gdp <- 100*ES_val_car6$TotalValGDP_gdp
        ES_val_car6 <- ES_val_car6 %>% select(PFT_code,TotalValGDP_gdp,region_un)

        GDPValplot <-  ggplot(ES_val_car6,aes(x=region_un,y=TotalValGDP_gdp,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                                    geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(10),pal_temp(6), pal_trop(10)))+
                                    geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("") + ggtitle("Market Goods")+
                                    guides(fill = guide_legend(reverse=FALSE)) + ylim(c(0,26))+theme(legend.key.size = unit(0.28, "cm"),
                                    legend.title = element_blank())+theme(legend.text=element_text(size=6))+ labs(title = expression(bold("b.") ~ " Total Benefits ($/year)")) +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Market benefits")+guides(fill=guide_legend(title="", ncol=1))
        leg1<-get_legend(GDPValplot)
        
        
        write_xlsx(list(PanelA = pft_es_car, PanelB1 = ES_val_car5, PanelB2 = ES_val_car6), "Figures/ManuscriptFigures/SourceData/FigED6.xlsx") 
               
        Map_MarketDam <-  ggplot(map_coef_car1) +
                theme_void()+
                geom_sf(data=world,aes(geometry = geometry),fill="white")+
                geom_sf(aes(geometry = geom, fill =100* coef)) +
                scale_fill_gradientn(colours = pal,limits=c(-010,010),
                    na.value="transparent",name="Damage at 1C (%)",oob = scales::squish)+
                theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
                theme(legend.title = element_blank())+ 
                labs(title = expression(bold("c.") ~ " Percent Damage at 1 Degree of Warming")) +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Market Natural Capital")

        Map_NonmarketDam <-  ggplot(map_coef_car2) +
                theme_void()+
                geom_sf(data=world,aes(geometry = geometry),fill="white")+
                geom_sf(aes(geometry = geom, fill =100* coef)) +
                scale_fill_gradientn(colours = pal,limits=c(-010,010),
                    na.value="transparent",name="Damage at 1C (%)",oob = scales::squish)+
                theme(legend.position = c(0.1, 0.5), # Centered horizontally, at the very top vertically
                      legend.justification = c(0.1, 0.5), # Reference point: center of the legend box at its top
                      legend.background = element_rect(fill = "transparent", color = NA), # Remove legend box background
                      legend.key.size = unit(0.6, "cm"),legend.key = element_rect(fill = "transparent", color = "transparent"), # Remove key box background
                      plot.title = element_text(hjust = 0.5)) +
                      ggtitle("Non-market NC damage coefficient")+theme(legend.title = element_blank())+ 
                labs(title = "") +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Non-market Natural Capital")

        
        combined_plot <- ggarrange(ggarrange(ggarrange(ESValplot1,leg1,widths=c(3,1),align="v"),ggarrange(GDPValplot,ESValplot,ncol=2,common.legend=TRUE,legend="none")),
        ggarrange(Map_MarketDam,Map_NonmarketDam,ncol=2,widths=c(1,1)),heights=c(6,5),ncol=1)

        combined_plot      

        #ggsave("Figures/ManuscriptFigures/FigED6.jpeg",dpi=900)
        #ggsave(filename="Figures/ManuscriptFigures/FigED6.pdf", plot=combined_plot, width=10, height=7)  

    ## Figure ED 6


    ## Figure ED 7
        load("Data/DataForFigures/pft_es_orc.Rds") 
        load("Data/DataForFigures/ES_val_orc5.Rds")    
        load("Data/DataForFigures/ES_val_orc6.Rds")      
        load("Data/DataForFigures/map_coef.Rds")  
        #levels(factor(pft_es_orc$Biome))
        
        map_coef_orc1 <- map_coef %>% filter(formula=="lin",dgvm=="orc",capital=="mN")      
        map_coef_orc2 <- map_coef %>% filter(formula=="lin",dgvm=="orc",capital=="nN")      

        pft_es_orc$Benefits <- pft_es_orc$Benefits/100
        pft_es_orc <- pft_es_orc %>% select(Benefits,Biome,Type)
        
        ESValplot1 <- ggplot(pft_es_orc)+
                geom_boxplot(aes(x=(Benefits),y=Biome,
                    fill=Type,middle=median((Benefits)))) +
                theme_bw()+ coord_cartesian(xlim=c(0,200))+
                theme(axis.title.y = element_blank(), 
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank())+xlab("")+ labs(title =expression(bold("a.") ~ " Benefits per area ($/ha/year)")) +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Non-Market benefits")+
                guides(fill = FALSE)
        ESValplot1


        
        #levels(ES_val_orc5$PFT_code) <- vector_orc_names
        ES_val_orc5$TotalValES_gdp <- 100*ES_val_orc5$TotalValES_gdp
        ES_val_orc5 <- ES_val_orc5 %>% select(PFT_code,TotalValES_gdp,region_un)
        
        ESValplot <-  ggplot(ES_val_orc5,aes(x=region_un,y=TotalValES_gdp,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                                    geom_bar(stat="identity")+ scale_fill_manual(values =   c(pal_boreal(3),pal_temp(3), pal_trop(4)))+
                                    geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("") + ggtitle("Ecosystem services")+
                                guides(fill = guide_legend(reverse=FALSE)) + ylim(c(0,26))+theme(legend.key.size = unit(0.4, "cm"),
                                    legend.title = element_blank())+theme(legend.text=element_text(size=8))+ labs(title ="") +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Non-Market benefits")
        
        levels(ES_val_orc6$PFT_code) <- vector_orc_names
        ES_val_orc6$TotalValGDP_gdp <- 100*ES_val_orc6$TotalValGDP_gdp
        ES_val_orc6 <- ES_val_orc6 %>% select(PFT_code,TotalValGDP_gdp,region_un)

        GDPValplot <-  ggplot(ES_val_orc6,aes(x=region_un,y=TotalValGDP_gdp,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                                    geom_bar(stat="identity")+ scale_fill_manual(values =  c(pal_boreal(3),pal_temp(3), pal_trop(4)))+
                                    geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("") + ggtitle("Market Goods")+
                                    guides(fill = guide_legend(reverse=FALSE)) + ylim(c(0,26))+theme(legend.key.size = unit(0.6, "cm"),
                                    legend.title = element_blank())+theme(legend.text=element_text(size=9))+ labs(title = expression(bold("b.") ~ " Total Benefits ($/year)")) +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                        plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Market benefits")+guides(fill=guide_legend(title="", ncol=1))
        leg1<-get_legend(GDPValplot)
        
        
        write_xlsx(list(PanelA = pft_es_orc, PanelB1 = ES_val_orc5, PanelB2 = ES_val_orc6), "Figures/ManuscriptFigures/SourceData/FigED7.xlsx") 
               
        Map_MarketDam <-  ggplot(map_coef_orc1) +
                theme_void()+
                geom_sf(data=world,aes(geometry = geometry),fill="white")+
                geom_sf(aes(geometry = geom, fill =100* coef)) +
                scale_fill_gradientn(colours = pal,limits=c(-010,010),
                    na.value="transparent",name="Damage at 1C (%)",oob = scales::squish)+
                theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
                theme(legend.title = element_blank())+ 
                labs(title = expression(bold("c.") ~ " Percent Damage at 1 Degree of Warming")) +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Market Natural Capital")

        Map_NonmarketDam <-  ggplot(map_coef_orc2) +
                theme_void()+
                geom_sf(data=world,aes(geometry = geometry),fill="white")+
                geom_sf(aes(geometry = geom, fill =100* coef)) +
                scale_fill_gradientn(colours = pal,limits=c(-010,010),
                    na.value="transparent",name="Damage at 1C (%)",oob = scales::squish)+
                theme(legend.position = c(0.1, 0.5), # Centered horizontally, at the very top vertically
                      legend.justification = c(0.1, 0.5), # Reference point: center of the legend box at its top
                      legend.background = element_rect(fill = "transparent", color = NA), # Remove legend box background
                      legend.key.size = unit(0.6, "cm"),legend.key = element_rect(fill = "transparent", color = "transparent"), # Remove key box background
                      plot.title = element_text(hjust = 0.5)) +
                      ggtitle("Non-market NC damage coefficient")+theme(legend.title = element_blank())+ 
                labs(title = "") +
                                    theme(plot.title = element_text(size = 12, hjust = 0.5),
                                            plot.subtitle = element_text(size = 9, hjust = 0.5)) +
                                    labs(subtitle = "Non-market Natural Capital")

        
        combined_plot <- ggarrange(ggarrange(ggarrange(ESValplot1,leg1,widths=c(3,1),align="v"),ggarrange(GDPValplot,ESValplot,ncol=2,common.legend=TRUE,legend="none")),
        ggarrange(Map_MarketDam,Map_NonmarketDam,ncol=2,widths=c(1,1)),heights=c(6,5),ncol=1)

        combined_plot      

        #ggsave("Figures/ManuscriptFigures/FigED7.jpeg",dpi=600)
        #ggsave(filename="Figures/ManuscriptFigures/FigED7.pdf", plot=combined_plot, width=10, height=6)  

    ## Figure ED 7

    ## Figure ED 8

        
        load("Data/DataForFigures/GR_Results_traj_orc.Rds")
        
        GR_Results <- GR_Results_traj_orc
        pop_mean_gdp_orc <- weighted.mean(x=GR_Results$GDP_change_perc[which(GR_Results$t %in% c(18))],w=GR_Results$pop[which(GR_Results$t %in% c(18))])
        x_gdp <- GR_Results$GDP_change_perc[which(GR_Results$year %in% c(2100))]
        x_es <- GR_Results$ES_change[which(GR_Results$year %in% c(2100))]
        w <- GR_Results$pop[which(GR_Results$year %in% c(2100))]
        pop_mean_gdp_orc <- weighted.mean(x=x_gdp,w=w)
        pop_mean_es_orc <- weighted.mean(x=x_es,w=w)
        
        plot_trajectory_orc <- ggplot(GR_Results,
            aes(x=ES_change,y=GDP_change_perc,color=r5,group=n))+
            geom_hline(aes(yintercept=0))+
            geom_vline(aes(xintercept=0))+
            geom_line()+
            geom_point(data = GR_Results[which(GR_Results$year %in% c(2050,2080,2100) ),], 
                aes(x=ES_change,y=GDP_change_perc,color=r5,shape=factor(year)),alpha=0.4)+
                #ymin=gdp_change-gdp_change_sd,ymax=gdp_change+gdp_change_sd))+
            theme_minimal()+
            geom_hline(aes(yintercept=pop_mean_gdp_orc),linetype=2,alpha=0.9)+
            geom_vline(aes(xintercept=pop_mean_es_orc),linetype=2,alpha=0.9)+
            geom_errorbar(data=GR_Results[which(GR_Results$year %in% c(2100)),],
                aes(ymin=(GDP_change_perc_lb), ymax=GDP_change_perc_ub),alpha=0.3)+
            geom_errorbar(data=GR_Results[which(GR_Results$year %in% c(2100)),],
                aes(xmin=(ES_change_lb), xmax=ES_change_ub),alpha=0.3)+
                theme(legend.position="bottom",legend.box="vertical")+
            labs(color="Region",shape="Year")+
            ylab("Annual GDP change (%)") + ggtitle("")  +
            xlab("Annual non-market natural capital change (%)") +
                coord_cartesian(xlim=c(-50, 20))+
            #scale_color_scico_d()+
            scale_color_npg()

        trajorc <- plot_trajectory_orc+geom_text(data = GR_Results[which(GR_Results$year %in% c(2100) ),],   aes(x=ES_change,y=GDP_change_perc,color=r5,label=n))
        trajorc

        
        load(file="Data/DataForFigures/GR_Results_car.Rds")
        GR_Results <- GR_Results_car
        pop_mean_gdp_car <- weighted.mean(x=GR_Results$GDP_change_perc[which(GR_Results$t %in% c(18))],w=GR_Results$pop[which(GR_Results$t %in% c(18))])
        x_gdp <- GR_Results$GDP_change_perc[which(GR_Results$year %in% c(2100))]
        x_es <- GR_Results$ES_change[which(GR_Results$year %in% c(2100))]
        w <- GR_Results$pop[which(GR_Results$year %in% c(2100))]
        pop_mean_gdp_car <- weighted.mean(x=x_gdp,w=w)
        pop_mean_es_car <- weighted.mean(x=x_es,w=w)
        
        
        
        plot_trajectory_car <- ggplot(GR_Results,
            aes(x=ES_change,y=GDP_change_perc,color=r5,group=n))+
            geom_hline(aes(yintercept=0))+
            geom_vline(aes(xintercept=0))+
            geom_line()+
            geom_point(data = GR_Results[which(GR_Results$year %in% c(2050,2080,2100) ),], 
                aes(x=ES_change,y=GDP_change_perc,color=r5,shape=factor(year)),alpha=0.4)+
            theme_minimal()+
            geom_hline(aes(yintercept=pop_mean_gdp_car),linetype=2,alpha=0.9)+
            geom_vline(aes(xintercept=pop_mean_es_car),linetype=2,alpha=0.9)+
            geom_errorbar(data=GR_Results[which(GR_Results$year %in% c(2100)),],
                aes(ymin=(GDP_change_perc_lb), ymax=GDP_change_perc_ub),alpha=0.3)+
            geom_errorbar(data=GR_Results[which(GR_Results$year %in% c(2100)),],
                aes(xmin=(ES_change_lb), xmax=ES_change_ub),alpha=0.3)+
                #geom_errorbarh(data=all_sim[which(all_sim$exp=="RCP6" & all_sim$year %in% c(2100) ),],
                #aes(xmin=(es_change_minus_se)*100, xmax=(es_change_plus_se)*100),alpha=0.3)+
                theme(legend.position="bottom",legend.box="vertical")+
            labs(color="Region",shape="Year")+
            ylab("Annual GDP change (%)") + ggtitle("")  + 
            xlab("Annual non-market natural capital change (%)") +
            ##scale_color_scico_d()+
            scale_color_npg()

        trajcar <- plot_trajectory_car+geom_text(data = GR_Results[which(GR_Results$year %in% c(2100) ),],   aes(x=ES_change,y=GDP_change_perc,color=r5,label=n))
        trajcar


        
        #write_xlsx(list(Mid = GR_Results_traj_orc, Bottom = GR_Results_car), "Figures/ManuscriptFigures/SourceData/FigED8.xlsx") 
               

        

        combined_plot <- ggarrange(trajlpj+xlim(-50,20)+ggtitle("LPJ-GUESS")+ylab("Annual GDP change (%)")+xlab(""),
            trajorc+ggtitle("ORCHIDEE-DGVM")+xlab(""),
            trajcar+ggtitle("CARAIB")+xlab("Annual non-market benefits change (%)"),
            nrow=3,ncol=1,common.legend=TRUE,legend="bottom")

        combined_plot
        
        #ggsave("Figures/ManuscriptFigures/FigED8.jpeg",dpi=900)
        #ggsave(filename="Figures/ManuscriptFigures/FigED8.pdf", plot=combined_plot, width=6, height=10)  

        
        

    ## Figure ED 8

    ## Figure ED 9
        load(file="Data/DataForFigures/GR_Results_lpj.Rds")
        load(file="Data/DataForFigures/GR_Results_orc.Rds")
        load(file="Data/DataForFigures/GR_Results_car.Rds")

        GR_Results_lpj <- GR_Results_lpj %>% filter(t==18) %>% select(YGROSS2020,pop2020,YGROSS_damages,YGROSS_baseline,ES_change,r5)
        
        GDP2100 <- ggplot(GR_Results_lpj)+
            geom_point(aes(x=log(YGROSS2020/pop2020),y=(YGROSS_damages-YGROSS_baseline)*100/YGROSS_baseline,color=r5))+theme_bw() +
            geom_smooth(data=GR_Results_lpj,aes(x=log(YGROSS2020/pop2020),y=(YGROSS_damages-YGROSS_baseline)*100/YGROSS_baseline),formula="y~x",method="lm")+
            ylab("GDP change in 2100 (%)")+
            geom_hline(aes(yintercept=0),linetype="dashed")+
            xlab("Log GDP per capita in 2020")+ 
            guides(color=guide_legend(title="Region"))+
            #scale_color_scico_d()+
            scale_color_npg()
            

        ES2100 <- ggplot(GR_Results_lpj)+
            geom_point(aes(x=log(YGROSS2020/pop2020),y=ES_change,color=r5))+theme_bw() + 
            geom_smooth(data=GR_Results_lpj,
            aes(x=log(YGROSS2020/pop2020),y=ES_change),formula="y~x",method="lm")+
            ylab("Non-market benefits  \nchange in 2100 (%)")+
            geom_hline(aes(yintercept=0),linetype="dashed")+
            xlab("Log GDP per capita in 2020")+ 
            guides(color=guide_legend(title="Region"))+
            #scale_color_scico_d()+
            scale_color_npg()

        
        GR_Results_orc <- GR_Results_orc  %>% filter(t==18) %>% select(YGROSS2020,pop2020,YGROSS_damages,YGROSS_baseline,ES_change,r5)

        GDP2100_orc <- ggplot(GR_Results_orc)+
            geom_point(aes(x=log(YGROSS2020/pop2020),y=(YGROSS_damages-YGROSS_baseline)*100/YGROSS_baseline,color=r5))+theme_bw() +
            geom_smooth(data=GR_Results_orc,aes(x=log(YGROSS2020/pop2020),y=(YGROSS_damages-YGROSS_baseline)*100/YGROSS_baseline),formula="y~x",method="lm")+
            ylab("GDP change in 2100 (%)")+
            geom_hline(aes(yintercept=0),linetype="dashed")+
            xlab("Log GDP per capita in 2020")+ 
            guides(color=guide_legend(title="Region"))+
            #scale_color_scico_d()+
            scale_color_npg()
        
        ES2100_orc <- ggplot(GR_Results_orc)+
            geom_point(aes(x=log(YGROSS2020/pop2020),y=ES_change,color=r5))+theme_bw() + 
            geom_smooth(data=GR_Results_orc,
            aes(x=log(YGROSS2020/pop2020),y=ES_change),formula="y~x",method="lm")+
            ylab("Non-market benefits \nchange in 2100 (%)")+
            geom_hline(aes(yintercept=0),linetype="dashed")+
            xlab("Log GDP per capita in 2020")+ 
            guides(color=guide_legend(title="Region"))+
            #scale_color_scico_d()+
            scale_color_npg()

        


        
        GR_Results_car <- GR_Results_car  %>% filter(t==18) %>% select(YGROSS2020,pop2020,YGROSS_damages,YGROSS_baseline,ES_change,r5)
        
        GDP2100_car <- ggplot(GR_Results_car)+
            geom_point(aes(x=log(YGROSS2020/pop2020),y=(YGROSS_damages-YGROSS_baseline)*100/YGROSS_baseline,color=r5))+theme_bw() +
            geom_smooth(data=GR_Results_car,aes(x=log(YGROSS2020/pop2020),y=(YGROSS_damages-YGROSS_baseline)*100/YGROSS_baseline),formula="y~x",method="lm")+
            ylab("GDP change in 2100 (%)")+
            geom_hline(aes(yintercept=0),linetype="dashed")+
            xlab("Log GDP per capita in 2020")+ 
            guides(color=guide_legend(title="Region"))+
            #scale_color_scico_d()+
            scale_color_npg()
        
        ES2100_car <- ggplot(GR_Results_car)+
            geom_point(aes(x=log(YGROSS2020/pop2020),y=ES_change,color=r5))+theme_bw() + 
            geom_smooth(data=GR_Results_car,
            aes(x=log(YGROSS2020/pop2020),y=ES_change),formula="y~x",method="lm")+
            ylab("Non-market benefits \nchange in 2100 (%)")+
            geom_hline(aes(yintercept=0),linetype="dashed")+
            xlab("Log GDP per capita in 2020")+ 
            guides(color=guide_legend(title="Region"))+
            #scale_color_scico_d()+
            scale_color_npg()

        
        results2100orc <-ggarrange(GDP2100_orc,ES2100_orc,common.legend=TRUE,legend="bottom")
        results2100car <-ggarrange(GDP2100_car,ES2100_car,common.legend=TRUE,legend="bottom")
        results2100lpj <-ggarrange(GDP2100,ES2100,common.legend=TRUE,legend="bottom")
        combined_plot <- ggarrange(results2100lpj,results2100orc,results2100car,ncol=1,common.legend=TRUE,legend="bottom")
        combined_plot        
        #ggsave("Figures/ManuscriptFigures/FigED9.jpeg",dpi=900)
        #ggsave(filename="Figures/ManuscriptFigures/FigED9.pdf", plot=combined_plot, width=6, height=10)  

        write_xlsx(list(Top = GR_Results_lpj, Mid = GR_Results_orc, Bottom = GR_Results_car), "Figures/ManuscriptFigures/SourceData/FigED9.xlsx") 

    ## Figure ED 9

    ## Figure ED 10
            load("Data/DataForFigures/GR_Results_lpj_noS.Rds")
            GR_Results <- GR_Results_lpj_noS

            GDP2100 <- ggplot(GR_Results[which(GR_Results$t==18),])+
                    geom_point(aes(x=log(YGROSS2020/pop2020),y=(YGROSS_damages-YGROSS_baseline)*100/YGROSS_baseline,color=r5))+theme_bw() +
                    geom_smooth(data=GR_Results[which(GR_Results$t==18),],aes(x=log(YGROSS2020/pop2020),y=(YGROSS_damages-YGROSS_baseline)*100/YGROSS_baseline),formula="y~x",method="lm")+
                    ylab("GDP change in 2100 (%)")+
                    geom_hline(aes(yintercept=0),linetype="dashed")+
                xlab("Log GDP per capita in 2020")+ scale_color_npg()+
                guides(color=guide_legend(title="Region"))
            
            ES2100 <- ggplot(GR_Results[which(GR_Results$t==18),])+
                geom_point(aes(x=log(YGROSS2020/pop2020),y=ES_change,color=r5))+theme_bw() + 
                geom_smooth(data=GR_Results[which(GR_Results$t==18),],
                aes(x=log(YGROSS2020/pop2020),y=ES_change),formula="y~x",method="lm")+
                ylab("Non-market benefits  \nchange in 2100 (%)")+
                geom_hline(aes(yintercept=0),linetype="dashed")+
                xlab("Log GDP per capita in 2020")+ 
                guides(color=guide_legend(title="Region"))+scale_color_npg()
        results2100lpj_noS <- ggarrange(GDP2100,ES2100,common.legend=TRUE,legend="bottom")
        results2100lpj_noS

        GR_Results_bottom <- GR_Results%>% filter(t==18) %>% select(ES_change,YGROSS2020,pop2020,YGROSS_damages,YGROSS_baseline,r5,t)

        load("Data/DataForFigures/GR_Results_lpj_traj_noS.Rds")
        GR_Results  <- GR_Results_lpj_traj_noS
        totpop <- sum(GR_Results$pop[which(GR_Results$year==2100)])
        x_gdp <- GR_Results$GDP_change_perc[which(GR_Results$year %in% c(2100))]
        x_es <- GR_Results$ES_change[which(GR_Results$year %in% c(2100))]
        w <- GR_Resultsj$pop[which(GR_Results$year %in% c(2100))]
        pop_mean_gdp_lpj <- weighted.mean(x=x_gdp,w=w)
        pop_mean_es_lpj <- weighted.mean(x=x_es,w=w)
        
        GR_Results_lpj_traj_noS_source <- GR_Results %>% select(ES_change,GDP_change_perc,r5,n,year,ES_change_lb,ES_change_ub,GDP_change_perc_ub,GDP_change_perc_lb)
        plot_trajectory <- ggplot(GR_Results,
            aes(x=ES_change,y=GDP_change_perc,color=r5,group=n))+
            geom_hline(aes(yintercept=0))+
            geom_vline(aes(xintercept=0))+
            geom_line()+
            geom_point(data = GR_Results[which(GR_Results$year %in% c(2050,2080,2100) ),], 
                aes(x=ES_change,y=GDP_change_perc,color=r5,shape=factor(year)),alpha=0.4)+
            theme_minimal()+
            geom_hline(aes(yintercept=pop_mean_gdp_lpj),linetype=2,alpha=0.9)+
            geom_vline(aes(xintercept=pop_mean_es_lpj),linetype=2,alpha=0.9)+
            geom_errorbar(data=GR_Results[which(GR_Results$year %in% c(2100)),],
                aes(ymin=(GDP_change_perc_lb), ymax=GDP_change_perc_ub),alpha=0.3)+
            geom_errorbar(data=GR_Results[which(GR_Results$year %in% c(2100)),],
                aes(xmin=(ES_change_lb), xmax=ES_change_ub),alpha=0.3)+
                theme(legend.position="bottom",legend.box="vertical")+
            labs(color="Region",shape="Year")+
            ylab("Annual GDP change (%)\n") + ggtitle("")  +
            xlab("Annual non-market benefits change (%)")+
            #scale_color_scico_d()+
            scale_color_npg()+xlim(c(-45,10))
        
        combined_plot <- ggarrange(plot_trajectory,results2100lpj_noS,ncol=1,heights=c(2,1))
            combined_plot
        #ggsave("Figures/ManuscriptFigures/FigED10.jpeg",dpi=600)
        #ggsave(filename="Figures/ManuscriptFigures/FigED10.pdf", plot=combined_plot, width=6, height=10)  

        #write_xlsx(list(Top = GR_Results_lpj_traj_noS_source, Bottom = GR_Results_bottom), "Figures/ManuscriptFigures/SourceData/FigED10.xlsx") 
         
        


    ## Figure ED 10

    ## Figure ED 11
        
        load(file="Data/DataForFigures/ordered_GR_dam_lpj.Rds")
        
        
        ordered_GR_dam_lpj$perc50_gdp_dgvm <- ordered_GR_dam_lpj$GDPpc[ceiling(length(ordered_GR_dam_lpj$cumdam)/2)]
        ordered_GR_dam_lpj$perc_dam_dgvm <- (ordered_GR_dam_lpj$cumdam[ceiling(length(ordered_GR_dam_lpj$cumdam)/2)])
        
        
        
        ordered_GR_dam_lpj_source <-ordered_GR_dam_lpj%>% select(GDPpc,cumdam,r5,perc_dam_dgvm,perc50_gdp_dgvm)
        
        plot_distribution_damages_lpj <- ggplot(ordered_GR_dam_lpj_source, 
            aes(x=(GDPpc),y=cumdam))+
            geom_line(color="gray")+
            geom_point(aes(color=r5),shape=15)+
            geom_text(data=ordered_GR_dam_lpj_source,x=ordered_GR_dam_lpj_source$perc50_gdp_dgvm-10500,y=80,label=paste0(ceiling(ordered_GR_dam_lpj_source$perc_dam_dgvm),"% of \ndamage"))+
            geom_vline(aes(xintercept=ordered_GR_dam_lpj_source$perc50_gdp_dgvm))+
            geom_hline(aes(yintercept=ordered_GR_dam_lpj_source$perc_dam_dgvm),linetype="dashed")+
            labs(color="Region")+
            theme_minimal()+ ylab("Cumulative global GDP \ndamages in 2100 (% of total)")+xlab('GDP per capita in 2020 ($ per person)')+
            scale_color_npg()+ggtitle("LPJ-GUESS")

        plot_distribution_damages_lpj
        
       
    


        load("Data/DataForFigures/GR_Results_traj_orc.Rds")
        GR_Results <- GR_Results_traj_orc
        maxyear <- aggregate(year~country, data=GR_Results,FUN="max")
        maxyear$countrymax <- paste0(maxyear$country,maxyear$year)
        GR_Results$countrymax <- paste0(GR_Results$country,GR_Results$year)
        all_simmax <- GR_Results[GR_Results$countrymax %in% maxyear$countrymax,]
        ordered_GR <- GR_Results[which(GR_Results$year==2100),]
        ordered_GR <- ordered_GR[order(ordered_GR$ES_change),]
        totpop <- sum(GR_Results$pop[which(GR_Results$year==2100)])
        ordered_GR <- GR_Results[which(GR_Results$year==2100),]
        ordered_GR$GDPpc <- ordered_GR$YGROSS2020*10^6/ordered_GR$pop2020
        ordered_GR <- ordered_GR[order(ordered_GR$GDPpc),]

        totGDP_change <- sum(GR_Results$GDP_change[which(GR_Results$year==2100)])
        ordered_GR$cumdam <- cumsum(100*ordered_GR$GDP_change/totGDP_change)

        ordered_GR$perc50_gdp_dgvm <- ordered_GR$GDPpc[ceiling(length(ordered_GR$cumdam)/2)]
        ordered_GR$perc_dam_dgvm <- (ordered_GR$cumdam[ceiling(length(ordered_GR$cumdam)/2)])
        
        
        
        ordered_GR_dam_orc_source <- ordered_GR %>% select(GDPpc,cumdam,r5,perc_dam_dgvm,perc50_gdp_dgvm)
        
        plot_distribution_damages_orc <- ggplot(ordered_GR_dam_orc_source, 
            aes(x=(GDPpc),y=cumdam))+
            geom_line(color="gray")+
            geom_point(aes(color=r5),shape=15)+
            geom_text(data=ordered_GR_dam_orc_source,x=ordered_GR_dam_orc_source$perc50_gdp_dgvm-10500,y=70,label=paste0(ceiling(ordered_GR_dam_orc_source$perc_dam_dgvm),"% of \ndamage"))+
            geom_vline(aes(xintercept=ordered_GR_dam_orc_source$perc50_gdp_dgvm))+
            geom_hline(aes(yintercept=ordered_GR_dam_orc_source$perc_dam_dgvm),linetype="dashed")+
            labs(color="Region")+
            theme_minimal()+ ylab("Cumulative global GDP \ndamages in 2100 (% of total)")+xlab('GDP per capita in 2020 ($ per person)')+
            scale_color_npg()+ggtitle("ORCHIDEE-DGVM")

        plot_distribution_damages_orc
        

        load("Data/DataForFigures/GR_Results_car.Rds")
        GR_Results <- GR_Results_car



        maxyear <- aggregate(year~country, data=GR_Results,FUN="max")
        maxyear$countrymax <- paste0(maxyear$country,maxyear$year)
        GR_Results$countrymax <- paste0(GR_Results$country,GR_Results$year)
        all_simmax <- GR_Results[GR_Results$countrymax %in% maxyear$countrymax,]
        ordered_GR <- GR_Results[which(GR_Results$year==2100),]
        ordered_GR <- ordered_GR[order(ordered_GR$ES_change),]
        totpop <- sum(GR_Results$pop[which(GR_Results$year==2100)])



        ordered_GR <- GR_Results[which(GR_Results$year==2100),]
        ordered_GR$GDPpc <- ordered_GR$YGROSS2020*10^6/ordered_GR$pop2020
        ordered_GR <- ordered_GR[order(ordered_GR$GDPpc),]

        totGDP_change <- sum(GR_Results$GDP_change[which(GR_Results$year==2100)])
        ordered_GR$cumdam <- cumsum(100*ordered_GR$GDP_change/totGDP_change)
        ordered_GR$perc50_gdp_dgvm <- ordered_GR$GDPpc[ceiling(length(ordered_GR$cumdam)/2)]
        ordered_GR$perc_dam_dgvm <- (ordered_GR$cumdam[ceiling(length(ordered_GR$cumdam)/2)])
        
        
        
        ordered_GR_dam_car_source <- ordered_GR %>% select(GDPpc,cumdam,r5,perc_dam_dgvm,perc50_gdp_dgvm)
        
        plot_distribution_damages_car <- ggplot(ordered_GR_dam_car_source, 
            aes(x=(GDPpc),y=cumdam))+
            geom_line(color="gray")+
            geom_point(aes(color=r5),shape=15)+
            geom_text(data=ordered_GR_dam_car_source,x=ordered_GR_dam_car_source$perc50_gdp_dgvm-10500,y=70,label=paste0(ceiling(ordered_GR_dam_car_source$perc_dam_dgvm),"% of \ndamage"))+
            geom_vline(aes(xintercept=ordered_GR_dam_car_source$perc50_gdp_dgvm))+
            geom_hline(aes(yintercept=ordered_GR_dam_car_source$perc_dam_dgvm),linetype="dashed")+
            labs(color="Region")+
            theme_minimal()+ ylab("Cumulative global GDP \ndamages in 2100 (% of total)")+xlab('GDP per capita in 2020 ($ per person)')+
            scale_color_npg()+ggtitle("CARAIB")

        plot_distribution_damages_car


        write_xlsx(list(lpj=ordered_GR_dam_lpj_source,orc=ordered_GR_dam_orc_source,car=ordered_GR_dam_car_source), "Figures/ManuscriptFigures/SourceData/FigED11.xlsx") 
        plot_distribution_damages <- ggarrange(plot_distribution_damages_lpj,plot_distribution_damages_orc,plot_distribution_damages_car,ncol=1,common.legend=TRUE,legend="bottom")
         
        plot_distribution_damages
        #ggsave("Figures/ManuscriptFigures/FigED11.jpeg",dpi=900)
        #ggsave(filename="Figures/ManuscriptFigures/FigED11.pdf", plot=plot_distribution_damages, width=5.3, height=10)  

    ## Figure ED 11

    
    ## Extra Cover Figures 
    
        
        load("Data/a_h_3dgvms.Rda")
        world_coast <- ne_coastline(scale = "medium", returnclass = "sf")
        bonne_proj <- "+proj=bonne +lat_1=50"
        glimpse(a_h)

        # Convert a_h dataframe to an sf object
        a_h_sf <- st_as_sf(a_h %>% filter(DGVM=="lpj",soc=="2005soc",horizon=="Present",clim=="hadgem2-es",PFT=="bne",!is.na(dominant_pft)), coords = c("x", "y"), crs = 4326)
        a_h_sf_future <- st_as_sf(a_h %>% filter(DGVM=="lpj",soc=="2005soc",horizon=="2C",clim=="hadgem2-es",PFT=="bne",!is.na(dominant_pft)), coords = c("x", "y"), crs = 4326)
        
        ggplot() +
        theme_void() +
        geom_sf(data = a_h_sf, aes(color = dominant_pft),size=0.1) +
        labs(color = "Dominant PFT")+
        geom_sf(data = world_coast, fill = "lightgray") +
        coord_sf(crs = st_crs(bonne_proj))+
        scale_color_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
        guides(color = FALSE)

        ggsave("Figures/ManuscriptFigures/map2.png",dpi=600)


        ggplot() +
        # Background fill
        geom_sf(data = world_coast, fill = "#F5F5F5", color = "#E0E0E0", size = 0.2) +
        
        # Points with the dominant PFT
        geom_sf(data = a_h_sf, aes(color = dominant_pft), size = 0.01) +
        # Points with the dominant PFT
        #geom_sf(data = a_h_sf_future, aes(color = dominant_pft), size = 0.01,alpha=0.1) +
        
        # Aesthetic adjustments
        scale_color_manual(values = c(pal_boreal(4), pal_temp(3), pal_trop(4))) +
        
        # Use the Bonne projection
        coord_sf(crs = st_crs(bonne_proj)) +
        #coord_sf(crs = "+proj=moll +lon_0=0")
        #coord_sf(crs = "+proj=sinu")+
        #coord_sf(crs = "+proj=hammer +lat_0=0 +lon_0=0")+
        # Remove the legend
        guides(color = FALSE, shape=FALSE) +
        
        # Use theme_void for minimalism
        theme_void() +
        
        # Maybe add a subtle theme with minimal grid lines for artistic effect (optional)
        theme(panel.grid.major = element_line(color = "#EAEAEA", size = 0.2),
            panel.grid.minor = element_line(color = "#EAEAEA", size = 0.1))
    ## Extra Cover Figure

    ## Figure to Response to Reviewers
            glimpse(a_h)
            ymin <- -20
            ymax <- 10
            xmin <- -80
            xmax <- -45
            ah_tropics <- a_h %>% filter(y>ymin & y<ymax & x<xmax & x>xmin) %>% group_by(DGVM,PFT,horizon) %>%
            summarise(mean_p = mean(Percent,na.rm=TRUE)) 
            glimpse(ah_tropics)

            ggplot(ah_tropics %>% filter(DGVM=="lpj"))+
            geom_bar(aes(x=horizon,y=mean_p,fill=PFT),stat="identity")+
            scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))

            library(dplyr)

            ah_tropics_diff <- ah_tropics %>%
                group_by(DGVM, PFT) %>%
                spread(key = horizon, value = mean_p) %>%
                mutate(diff_mean_p = `2C` - Present) %>%
                select(DGVM, PFT, diff_mean_p)
            
            ah_tropics_diff$DGVM <- factor(ah_tropics_diff$DGVM,levels=c("lpj","car","orc"))
            ah_tropics_diff <- ah_tropics_diff %>% mutate(DGVM = recode(DGVM, "lpj"="LPJ-GUESS","car"="CARAIB","orc"="ORCHIDEE-DGVM"))
            
            car_order <-  (c("brsuas"   ,   "brsutecds" ,  "brevtecds" ,  "ndsutecdt"  , "ndevtecdt","brsutecdt",   "c3dh"   ,     "c3hh"   ,     "brsuteclt" ,  "ndevteclt","brsutewmt"  , "brsutewms"  , "brevtewms"  , "ndevstdit" ,  "ndsustswt", "brevstdit" ,  "brevdttht" ,  "c4h"      ,   "brevxs"    ,  "sds"   ,"ndevtedtt"  , "ndevtedttht" ,"brevdtt"  ,   "brrgtrt"    , "brevtrt" ,"trs"))
            orc_order <- (c( "bobrsu","bondev","bondsu","c3gra","tebrsu", "tebrev","tendev","c4gra","trbrev","trbrrg"))
            lpj_order <- rev(c("trbe","tribe", "trbr" , "c4g"  , "tebe" , "tebs" , "ibs",   "c3g"  , "bine","bne"  , "bns"))
            ah_tropics_diff$PFT <- factor(ah_tropics_diff$PFT,levels=c(lpj_order,car_order,orc_order))
            combined_order <- c(lpj_order,car_order,orc_order)
            
            color_vec <- c(pal_boreal(4),pal_temp(3), pal_trop(4),pal_boreal(10),pal_temp(6), pal_trop(10),pal_boreal(4),pal_temp(3), pal_trop(4))
            ggplot(ah_tropics_diff)+
            geom_bar(aes(x=DGVM,y=diff_mean_p,fill=PFT),stat="identity")+
            scale_fill_manual(values = color_vec)

            ah_tropics_diff_total <- ah_tropics_diff %>%
                group_by(DGVM) %>%
                summarise(total_p = sum(diff_mean_p,na.rm=TRUE))

            ggplot(ah_tropics_diff)+
            geom_bar(aes(x=DGVM,y=diff_mean_p*100,fill=PFT),stat="identity")+
            scale_fill_manual(values = color_vec, breaks = combined_order, guide = guide_legend(ncol=3))+
            #scale_fill_manual(values = color_vec)+
            geom_point(data=ah_tropics_diff_total,aes(x=DGVM,y=total_p*100),col="red",size=3) +
            theme_bw()+
            geom_hline(aes(yintercept=0),linetype="dashed")+
            xlab("DGVM")+ylab("Cover Change (%)")







                # 1. Plot subsets individually:
                plot_lpj <- ggplot(ah_tropics_diff[ah_tropics_diff$PFT %in% lpj_order,], 
                                aes(x = DGVM, y = diff_mean_p, fill = PFT)) +
                geom_bar(stat = "identity") +
                scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)), guide = guide_legend(ncol=1)) +
                theme_void() + 
                theme(legend.position = "right")+labs(fill="LPJ-GUESS")

                plot_car <- ggplot(ah_tropics_diff[ah_tropics_diff$PFT %in% car_order,], 
                                aes(x = DGVM, y = diff_mean_p, fill = PFT)) +
                geom_bar(stat = "identity") +
                scale_fill_manual(values = c(pal_boreal(10),pal_temp(6), pal_trop(10)), guide = guide_legend(ncol=1)) +
                theme_void() + 
                theme(legend.position = "right")+labs(fill="CARAIB")

                plot_orc <- ggplot(ah_tropics_diff[ah_tropics_diff$PFT %in% orc_order,], 
                                aes(x = DGVM, y = diff_mean_p, fill = PFT)) +
                geom_bar(stat = "identity") +
                scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)), guide = guide_legend(ncol=1)) +
                theme_void() + 
                theme(legend.position = "right")+labs(fill="ORCHIDEE-DGVM")

                # 2. Extract legends:
                legend_lpj <- cowplot::get_legend(plot_lpj)
                legend_car <- cowplot::get_legend(plot_car)
                legend_orc <- cowplot::get_legend(plot_orc)

                # 3. Combine main plot with legends:
                main_plot_tropam <- ggplot(ah_tropics_diff) +
                geom_bar(aes(x = DGVM, y = diff_mean_p, fill = PFT),stat = "identity") +
                scale_fill_manual(values = color_vec) +
                geom_point(data = ah_tropics_diff_total, aes(x = DGVM, y = total_p), col = "red", size = 3) +
                geom_hline(aes(yintercept=0),linetype="dashed")+
                theme_minimal() +
                theme(legend.position = "none") # Hide legend

                combined_plot <- ggarrange(main_plot, legend_lpj, legend_car, legend_orc, 
                                        ncol = 4, widths = c(3, 1, 1, 1))

                combined_plot



                # World map
                world <- ne_countries(scale = "medium", returnclass = "sf")

                # Midpoint of the Amazon
                # Midpoint of the Amazon
                amazon_midpoint_x <- (-70 + -55) / 2
                amazon_midpoint_y <- (-10 + 0) / 2

                # Fetching coastlines
                coastlines <- ne_coastline(scale = "medium", returnclass = "sf")


                # Plotting without country boundaries, just coastlines
                map_plot <- ggplot() +
                geom_sf(data = coastlines, fill = "transparent") +
                coord_sf(datum = NA) + # To show the entire world
                theme_minimal()

                # Add combined_plot over the Amazon's midpoint
                library(grid) # for `as.grob()`
                # Add combined_plot over the Amazon's midpoint
                final_plot <- map_plot + 
                annotation_custom(
                    grob = ggplotGrob(main_plot),
                    xmin = amazon_midpoint_x - 10,  # Adjust for size
                    xmax = amazon_midpoint_x + 10,  # Adjust for size
                    ymin = amazon_midpoint_y - 20,   # Adjust for size
                    ymax = amazon_midpoint_y + 20    # Adjust for size
                )

                final_plot

                ggarrange(final_plot, legend_lpj, legend_car, legend_orc, 
                                        ncol = 4, widths = c(10, 1, 1, 1))


                main_plot_amazon <- main_plot








            
            car_order <-  (c("brsuas"   ,   "brsutecds" ,  "brevtecds" ,  "ndsutecdt"  , "ndevtecdt","brsutecdt",   "c3dh"   ,     "c3hh"   ,     "brsuteclt" ,  "ndevteclt","brsutewmt"  , "brsutewms"  , "brevtewms"  , "ndevstdit" ,  "ndsustswt", "brevstdit" ,  "brevdttht" ,  "c4h"      ,   "brevxs"    ,  "sds"   ,"ndevtedtt"  , "ndevtedttht" ,"brevdtt"  ,   "brrgtrt"    , "brevtrt" ,"trs"))
            orc_order <- (c( "bobrsu","bondev","bondsu","c3gra","tebrsu", "tebrev","tendev","c4gra","trbrev","trbrrg"))
            lpj_order <- rev(c("trbe","tribe", "trbr" , "c4g"  , "tebe" , "tebs" , "ibs",   "c3g"  , "bine","bne"  , "bns"))
            combined_order <- c(lpj_order,car_order,orc_order)
            color_vec <- c(pal_boreal(4),pal_temp(3), pal_trop(4),pal_boreal(10),pal_temp(6), pal_trop(10),pal_boreal(4),pal_temp(3), pal_trop(4))
            
        ### Latin America Start
            ymin <- -20
                ymax <- 10
                xmin <- -80
                xmax <- -45

            ah_tropics <- a_h %>% filter(y>ymin & y<ymax & x<xmax & x>xmin) %>% group_by(DGVM,PFT,horizon) %>%
                summarise(mean_p = mean(Percent,na.rm=TRUE)) 
                
                ah_tropics_diff <- ah_tropics %>%
                    group_by(DGVM, PFT) %>%
                    spread(key = horizon, value = mean_p) %>%
                    mutate(diff_mean_p = `2C` - Present) %>%
                    select(DGVM, PFT, diff_mean_p)
                
                ah_tropics_diff$DGVM <- factor(ah_tropics_diff$DGVM,levels=c("lpj","car","orc"))
                ah_tropics_diff <- ah_tropics_diff %>% mutate(DGVM = recode(DGVM, "lpj"="LPJ-GUESS","car"="CARAIB","orc"="ORCHIDEE-DGVM"))
                ah_tropics_diff$PFT <- factor(ah_tropics_diff$PFT,levels=c(lpj_order,car_order,orc_order))

                ah_tropics_diff_total <- ah_tropics_diff %>%
                    group_by(DGVM) %>%
                    summarise(total_p = sum(diff_mean_p,na.rm=TRUE))

                main_plot_LA <- ggplot(ah_tropics_diff) +
                geom_bar(aes(x = DGVM, y = 100*diff_mean_p, fill = PFT),stat = "identity") +
                scale_fill_manual(values = color_vec) +
                geom_point(data = ah_tropics_diff_total, aes(x = DGVM, y = 100*total_p), col = "red", size = 3) +
                geom_hline(aes(yintercept=0),linetype="dashed")+
                theme_minimal() +
                ggtitle("Tropical and Subtropical Latin America")+
                theme(legend.position = "none") # Hide legend
        ### Latin AMerica End

        #### START NORTH AMERICA
                ymin <- 10     # Starting roughly above the Tropic of Cancer
                ymax <- 85     # Close to the North Pole for completeness
                xmin <- -170   # Including the westernmost parts of Alaska
                xmax <- -50    # Covering Greenland and the easternmost parts of Canada

                ah_tropics <- a_h %>% filter(y>ymin & y<ymax & x<xmax & x>xmin) %>% group_by(DGVM,PFT,horizon) %>%
                summarise(mean_p = mean(Percent,na.rm=TRUE)) 
                
                ah_tropics_diff <- ah_tropics %>%
                    group_by(DGVM, PFT) %>%
                    spread(key = horizon, value = mean_p) %>%
                    mutate(diff_mean_p = `2C` - Present) %>%
                    select(DGVM, PFT, diff_mean_p)
                
                ah_tropics_diff$DGVM <- factor(ah_tropics_diff$DGVM,levels=c("lpj","car","orc"))
                ah_tropics_diff <- ah_tropics_diff %>% mutate(DGVM = recode(DGVM, "lpj"="LPJ-GUESS","car"="CARAIB","orc"="ORCHIDEE-DGVM"))
                ah_tropics_diff$PFT <- factor(ah_tropics_diff$PFT,levels=c(lpj_order,car_order,orc_order))

                ah_tropics_diff_total <- ah_tropics_diff %>%
                    group_by(DGVM) %>%
                    summarise(total_p = sum(diff_mean_p,na.rm=TRUE))

                main_plot_NA <- ggplot(ah_tropics_diff) +
                geom_bar(aes(x = DGVM, y = diff_mean_p, fill = PFT),stat = "identity") +
                scale_fill_manual(values = color_vec) +
                geom_point(data = ah_tropics_diff_total, aes(x = DGVM, y = total_p), col = "red", size = 3) +
                geom_hline(aes(yintercept=0),linetype="dashed")+
                theme_void() +
                theme(legend.position = "none") # Hide legend

        ### END NORTH AMERICA

        ### Start Africa
                # ymin <- -35    # Roughly the southernmost tip of Africa
                # ymax <- 15     # Just north of the Equator, excluding North African countries
                # xmin <- 12     # Starting from the westernmost part of the African continent
                # xmax <- 51     # Including parts of the eastern coast, up to Somalia


                ymin <- -35   # below the Tropic of Capricorn to capture the southernmost subtropics
                ymax <- 23.5  # Tropic of Cancer
                xmin <- -17   # Westernmost point in Senegal
                xmax <- 51    # Including parts of Somalia in the east


                ah_tropics <- a_h %>% filter(y>ymin & y<ymax & x<xmax & x>xmin) %>% group_by(DGVM,PFT,horizon) %>%
                summarise(mean_p = mean(Percent,na.rm=TRUE)) 
                
                ah_tropics_diff <- ah_tropics %>%
                    group_by(DGVM, PFT) %>%
                    spread(key = horizon, value = mean_p) %>%
                    mutate(diff_mean_p = `2C` - Present) %>%
                    select(DGVM, PFT, diff_mean_p)
                
                ah_tropics_diff$DGVM <- factor(ah_tropics_diff$DGVM,levels=c("lpj","car","orc"))
                ah_tropics_diff <- ah_tropics_diff %>% mutate(DGVM = recode(DGVM, "lpj"="LPJ-GUESS","car"="CARAIB","orc"="ORCHIDEE-DGVM"))
                ah_tropics_diff$PFT <- factor(ah_tropics_diff$PFT,levels=c(lpj_order,car_order,orc_order))

                ah_tropics_diff_total <- ah_tropics_diff %>%
                    group_by(DGVM) %>%
                    summarise(total_p = sum(diff_mean_p,na.rm=TRUE))

                main_plot_Af <- ggplot(ah_tropics_diff) +
                geom_bar(aes(x = DGVM, y = 100*diff_mean_p, fill = PFT),stat = "identity") +
                scale_fill_manual(values = color_vec) +
                geom_point(data = ah_tropics_diff_total, aes(x = DGVM, y = 100*total_p), col = "red", size = 3) +
                geom_hline(aes(yintercept=0),linetype="dashed")+
                ylab("Average Pixel Cover Change (pp)")+ggtitle("a. Tropical and Subtropical Africa")+
                theme_minimal() +
                theme(legend.position = "none") # Hide legend
                main_plot_Af
        ### Africa END

        ### EUROPE
                ymin <- 35    # Including parts of southern Europe
                ymax <- 70    # Reaching towards the northernmost parts of Scandinavia
                xmin <- -25   # Including parts of western Europe, like Portugal
                xmax <- 40    # Covering up to Russia's Ural Mountains



                ah_tropics <- a_h %>% filter(y>ymin & y<ymax & x<xmax & x>xmin) %>% group_by(DGVM,PFT,horizon) %>%
                summarise(mean_p = mean(Percent,na.rm=TRUE)) 
                
                ah_tropics_diff <- ah_tropics %>%
                    group_by(DGVM, PFT) %>%
                    spread(key = horizon, value = mean_p) %>%
                    mutate(diff_mean_p = `2C` - Present) %>%
                    select(DGVM, PFT, diff_mean_p)
                
                ah_tropics_diff$DGVM <- factor(ah_tropics_diff$DGVM,levels=c("lpj","car","orc"))
                ah_tropics_diff <- ah_tropics_diff %>% mutate(DGVM = recode(DGVM, "lpj"="LPJ-GUESS","car"="CARAIB","orc"="ORCHIDEE-DGVM"))
                ah_tropics_diff$PFT <- factor(ah_tropics_diff$PFT,levels=c(lpj_order,car_order,orc_order))

                ah_tropics_diff_total <- ah_tropics_diff %>%
                    group_by(DGVM) %>%
                    summarise(total_p = sum(diff_mean_p,na.rm=TRUE))

                main_plot_eu <- ggplot(ah_tropics_diff) +
                geom_bar(aes(x = DGVM, y = diff_mean_p, fill = PFT),stat = "identity") +
                scale_fill_manual(values = color_vec) +
                geom_point(data = ah_tropics_diff_total, aes(x = DGVM, y = total_p), col = "red", size = 3) +
                geom_hline(aes(yintercept=0),linetype="dashed")+
                theme_void() +
                theme(legend.position = "none") # Hide legend

        ### Europe


        ### Tropical Asia 
                ymin <- -10    # Starting below the Equator to cover parts of Indonesia and other Pacific islands
                ymax <- 28     # Reaching northward to cover the majority of India
                xmin <- 60     # Western boundary to include the majority of India
                xmax <- 150    # Eastern boundary to cover Pacific islands and parts of the Philippines




                ah_tropics <- a_h %>% filter(y>ymin & y<ymax & x<xmax & x>xmin) %>% group_by(DGVM,PFT,horizon) %>%
                summarise(mean_p = mean(Percent,na.rm=TRUE)) 
                
                ah_tropics_diff <- ah_tropics %>%
                    group_by(DGVM, PFT) %>%
                    spread(key = horizon, value = mean_p) %>%
                    mutate(diff_mean_p = `2C` - Present) %>%
                    select(DGVM, PFT, diff_mean_p)
                
                ah_tropics_diff$DGVM <- factor(ah_tropics_diff$DGVM,levels=c("lpj","car","orc"))
                ah_tropics_diff <- ah_tropics_diff %>% mutate(DGVM = recode(DGVM, "lpj"="LPJ-GUESS","car"="CARAIB","orc"="ORCHIDEE-DGVM"))
                ah_tropics_diff$PFT <- factor(ah_tropics_diff$PFT,levels=c(lpj_order,car_order,orc_order))

                ah_tropics_diff_total <- ah_tropics_diff %>%
                    group_by(DGVM) %>%
                    summarise(total_p = sum(diff_mean_p,na.rm=TRUE))

                main_plot_as <- ggplot(ah_tropics_diff) +
                geom_bar(aes(x = DGVM, y = diff_mean_p*100, fill = PFT),stat = "identity") +
                scale_fill_manual(values = color_vec) +
                geom_point(data = ah_tropics_diff_total, aes(x = DGVM, y = total_p*100), col = "red", size = 3) +
                geom_hline(aes(yintercept=0),linetype="dashed")+
                theme_minimal() +
                ggtitle("Tropical and Subtropical Asia")+
                theme(legend.position = "none") # Hide legend

        ### Tropical Asia 

        ### Rusia
                ymin <- 35     # Northern parts of China
                ymax <- 75     # Extends to cover parts of Siberian Russia
                xmin <- 60     # Western boundary to cover Kazakhstan
                xmax <- 135    # Eastern boundary to cover the Russian Far East and parts of China





                ah_tropics <- a_h %>% filter(y>ymin & y<ymax & x<xmax & x>xmin) %>% group_by(DGVM,PFT,horizon) %>%
                summarise(mean_p = mean(Percent,na.rm=TRUE)) 
                
                ah_tropics_diff <- ah_tropics %>%
                    group_by(DGVM, PFT) %>%
                    spread(key = horizon, value = mean_p) %>%
                    mutate(diff_mean_p = `2C` - Present) %>%
                    select(DGVM, PFT, diff_mean_p)
                
                ah_tropics_diff$DGVM <- factor(ah_tropics_diff$DGVM,levels=c("lpj","car","orc"))
                ah_tropics_diff <- ah_tropics_diff %>% mutate(DGVM = recode(DGVM, "lpj"="LPJ-GUESS","car"="CARAIB","orc"="ORCHIDEE-DGVM"))
                ah_tropics_diff$PFT <- factor(ah_tropics_diff$PFT,levels=c(lpj_order,car_order,orc_order))

                ah_tropics_diff_total <- ah_tropics_diff %>%
                    group_by(DGVM) %>%
                    summarise(total_p = sum(diff_mean_p,na.rm=TRUE))

                main_plot_rus <- ggplot(ah_tropics_diff) +
                geom_bar(aes(x = DGVM, y = diff_mean_p, fill = PFT),stat = "identity") +
                scale_fill_manual(values = color_vec) +
                geom_point(data = ah_tropics_diff_total, aes(x = DGVM, y = total_p), col = "red", size = 3) +
                geom_hline(aes(yintercept=0),linetype="dashed")+
                theme_void() +
                theme(legend.position = "none") # Hide legend

        ### Rusia


        ### Start Map All


                amazon_midpoint_x <- -62.5
                amazon_midpoint_y <- -5

                north_america_midpoint_x <- -110
                north_america_midpoint_y <- 47.5

                african_midpoint_x <-  31.5
                african_midpoint_y <-  -10

                european_midpoint_x <- (-25 + 40) / 2   # 7.5
                european_midpoint_y <- (35 + 70) / 2   # 52.5

                tropical_asian_midpoint_x <- (60 + 150) / 2   # 105
                tropical_asian_midpoint_y <- (-10 + 28) / 2   # 9

                north_central_asian_midpoint_x <- (60 + 135) / 2   # 97.5
                north_central_asian_midpoint_y <- (35 + 75) / 2   # 55





                # Fetching coastlines
                coastlines <- ne_coastline(scale = "medium", returnclass = "sf")


                # Plotting without country boundaries, just coastlines
                map_plot <- ggplot() +
                geom_sf(data = coastlines, fill = "transparent") +
                coord_sf(datum = NA) + # To show the entire world
                theme_minimal()

                final_plot <- map_plot + 
                annotation_custom(
                    grob = ggplotGrob(main_plot_amazon),
                    xmin = amazon_midpoint_x - 10,  # Adjust for size
                    xmax = amazon_midpoint_x + 10,  # Adjust for size
                    ymin = amazon_midpoint_y - 20,   # Adjust for size
                    ymax = amazon_midpoint_y + 20    # Adjust for size
                )+ 
                annotation_custom(
                    grob = ggplotGrob(main_plot_NA),
                    xmin = north_america_midpoint_x - 10,  # Adjust for size
                    xmax = north_america_midpoint_x + 10,  # Adjust for size
                    ymin = north_america_midpoint_y - 20,   # Adjust for size
                    ymax = north_america_midpoint_y + 20    # Adjust for size
                )+ 
                annotation_custom(
                    grob = ggplotGrob(main_plot_Af),
                    xmin = african_midpoint_x - 10,  # Adjust for size
                    xmax = african_midpoint_x + 10,  # Adjust for size
                    ymin = african_midpoint_y - 20,   # Adjust for size
                    ymax = african_midpoint_y + 20    # Adjust for size
                )+ 
                annotation_custom(
                    grob = ggplotGrob(main_plot_eu),
                    xmin = european_midpoint_x - 10,  # Adjust for size
                    xmax = european_midpoint_x + 10,  # Adjust for size
                    ymin = european_midpoint_y - 20,   # Adjust for size
                    ymax = european_midpoint_y + 20    # Adjust for size
                )+ 
                annotation_custom(
                    grob = ggplotGrob(main_plot_as),
                    xmin = tropical_asian_midpoint_x - 10,  # Adjust for size
                    xmax = tropical_asian_midpoint_x + 10,  # Adjust for size
                    ymin = tropical_asian_midpoint_y - 20,   # Adjust for size
                    ymax = tropical_asian_midpoint_y + 20    # Adjust for size
                )+ 
                annotation_custom(
                    grob = ggplotGrob(main_plot_rus),
                    xmin = north_central_asian_midpoint_x - 10,  # Adjust for size
                    xmax = north_central_asian_midpoint_x + 10,  # Adjust for size
                    ymin = north_central_asian_midpoint_y - 20,   # Adjust for size
                    ymax = north_central_asian_midpoint_y + 20    # Adjust for size
                )

                final_plot

        ### END Map All

            ggarrange(main_plot_Af+ylab("Average Pixel Cover Change at 2C (percentage points)"),main_plot_LA+ylab("")+
                ggtitle("b. Tropical and Subtropical Latin America"),main_plot_as+ylab("")+
                ggtitle("c. Tropical and Subtropical Asia"), legend_lpj, legend_car, legend_orc, 
                                        ncol = 6, widths = c(4,4,4, 2, 2, 2))
                
            ggarrange(final_plot, legend_lpj, legend_car, legend_orc, 
                                        ncol = 4, widths = c(10, 1, 1, 1))

            ggsave("Figures/ManuscriptFigures/FigResponse2.png",dpi=900)
            


    ## Figure to Response to Reviewers

    # What countries gain?
    ## Figure 4
        
        load("Data/DataForFigures/GR_Results_lpj_traj.Rds")
        load("Data/DataForFigures/ordered_GR_lpj.Rds")
        load(file="Data/DataForFigures/GR_Results_orc.Rds")
        load(file="Data/DataForFigures/GR_Results_car.Rds")

        c_lpj <- GR_Results_lpj_traj %>% filter(year ==2100) %>% filter(ES_change >0) %>% select(country, ES_change)
        c_car <- GR_Results_car %>% filter(year ==2100) %>% filter(ES_change >0) %>% select(country, ES_change)
        c_orc <- GR_Results_orc %>% filter(year ==2100) %>% filter(ES_change >0) %>% select(n, ES_change)

        GR_Results_lpj_traj %>% filter(country =="mex")%>% filter(year ==2100)
        glimpse(GR_Results_lpj_traj )

        c_lpj %>% filter(country %in% c_car$country)  %>% filter(country %in% c_orc$country)      

        c_lpj %>% summarise(meanES = mean(ES_change))

        
        
        c_lpj <- GR_Results_lpj_traj %>% filter(year ==2100) %>% filter(ES_change <0) %>% select(country, ES_change)
        c_lpj %>% summarise(meanES = mean(ES_change))
        
    ## Figure 4



