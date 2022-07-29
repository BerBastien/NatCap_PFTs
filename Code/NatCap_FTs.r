#Setup
    x <- c('raster', 'reshape2','randomForestExplainer','randomForest','ggbreak','ggpubr','dplyr','ncdf4',"ggridges","forcats",'ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf','lfe','marginaleffects','rgdal',"rnaturalearth")
    lapply(x, require, character.only = TRUE)
    setwd('C:\\Users\\bastien\\Documents\\GitHub\\NatCap_PFTs')
    dirpfts <- "C:/Users/bastien/Documents/GitHub/NatCap_PFTs/Data"
    #install.packages("randomForestSRC")
            #library("randomForestSRC")
    world <- spData::world
    isos <- world$iso_a2
    clim_models <- c("hadgem2-es","gfdl-esm2m","ipsl-cm5a-lr","miroc5")
            

    #Useful strings (start)
        pfts <- c("bne","bine","bns","tebs","ibs","tebe","trbe","tribe","trbr","c3g","c4g") 
        pfts_names <- c("Boreal needleleaved evergreen","Boreal shade intolerant needleleaved evergreen", 
        "Boreal needleleved summergreen (BNS)", 
        "Temperate broadleaved summergreen (TeBS)", 
        "shade intolerant broadleaved summergreen (IBS)", 
        "Temperate broadleved evergreen (TeBE)",
        "Tropical broadleaved evergreen (TrBE)", 
        "Tropical shade intolerant broadleaved evergreen (TrIBE)", 
        "Tropical broadleaved raingreen (TrBR)",
        "C3 grass (C3G)", "C4 grass (C4G)")
        horizon <- c("Present","Near-term (2021-2040)","Mid-term (2041-2060)","Long-term (2081-2099)")
        `%notin%` <- Negate(`%in%`)
        
    #Useful strings (end)
    load("Data/dif_periods_tas_brick.Rdata")
    t <- dif_periods_tas_brick
    t <- t(t)
    all_t <- as.data.frame(t)
    all_a <- as.data.frame(area(t))
    Tshort <- sum(all_t[,1]*all_a[,1])/sum(all_a[,1])
    Tmed <- sum(all_t[,2]*all_a[,1])/sum(all_a[,1])
    Tlong <- sum(all_t[,3]*all_a[,1])/sum(all_a[,1])
    #Read ES database
        es_ac_notFood <- read.csv("es_ac_notFood_id.csv")
        
    #Read ES database

    #Read Natural Capital Wealth Accounts (start)
        wealth <- read.csv('Data\\WorldBank_WealthAccounts_2018USD.csv')
        colnames(wealth) <- c("Country.Name" ,"Country.Code",   "Series.Name" , "Series.Code",
        1995:2018)
        #glimpse(wealth)
        #table(wealth$countrycode)

        years <- c(1995:2018)
        for (i in 1:length(years)){
            yeari <- years[i]
            w2 <- wealth[,which(colnames(wealth) %in% 
                c("Country.Name","Country.Code","Series.Name",yeari))]
            w3 <- reshape(w2, idvar=c("Country.Name" ,"Country.Code"), 
                timevar="Series.Name", direction="wide")
            w3 <- w3[,c(1,2,3,4,34,37,38,39,41,47,49,51,53)]
            w3[,(dim(w3)[2]+1)] <- yeari
            colnames(w3) <- c("countryname","countrycode", "N","H","Nagg","Nfisheries","NforestES","NforestT","Nmangroves","Npa","Foreign", "K","TotalWealth","year")
            if (i==1){Wealth <- w3} else {
                Wealth <- rbind(Wealth,w3)
            }
        }
        #glimpse(Wealth)
        #levels(factor(Wealth$countrycode))[levels(factor(Wealth$countrycode)) %in% isos_missin3]
        
        #ggplot(data=Wealth, aes(x = year, y=log(N), group = countrycode)) + 
        #geom_line(aes(color=countrycode))  +
        #scale_colour_discrete(guide = 'none') +
        #scale_x_discrete(expand=c(0, 1)) +
        #geom_dl(aes(label = countrycode), method = list(dl.combine("first.points", "last.points")), cex = 0.8)

        gdp <- read.csv('Data\\GDP_2015USD_19952018.csv')
        colnames(gdp) <- c("countryname", "countrycode", "seriesname","seriescode",1995:2018)
        #glimpse(gdp)
        for (i in 1:length(years)){
            yeari <- years[i]
            gdp2 <- gdp[,which(colnames(gdp) %in% 
                c("countryname","countrycode","seriesname",yeari))]
            
            gdp3 <- reshape(gdp2, idvar=c("countryname","countrycode"), 
                timevar="seriesname", direction="wide")
            gdp3 <- gdp3[,c(1:3)]
            gdp3[,4] <- yeari
            colnames(gdp3) <- c("countryname","countrycode","GDP","year")
            if (i==1){Gdp <- gdp3} else {
                Gdp <- rbind(Gdp,gdp3)
            }
        }
        
        wealth <- merge(Wealth,Gdp, by = c("countrycode","year"))
        wealth <- wealth[-c(which(wealth[,1]=="")),] #no data in these rows
        glimpse(wealth)
        wealth$GDP <- as.numeric(as.character(wealth$GDP)) * 1.06 #converting 2015 to 2018 usd
        colnames(wealth)[3] <- "countryname"
        #table(wealth$countrycode)
        

        
        population  <- read.csv('Data\\Pop1995_2018.csv')
        colnames(population) <-  c("countryname", "countrycode", "seriesname","seriescode",1995:2018)
        #glimpse(population)
        years <- c(1995:2018)
        for (i in 1:length(years)){
            yeari <- years[i]
            w2 <- population[,which(colnames(population) %in% 
                c("countryname", "countrycode", "seriesname",yeari))]
            w3 <- reshape(w2, idvar=c("countryname", "countrycode"), 
                timevar="seriesname", direction="wide")
            #glimpse(w3)
            #w3 <- w3[,c(1,2,5)]
            w3[,4] <- yeari
            colnames(w3) <- c("countryname","countrycode","Population","year")
            if (i==1){Population <- w3} else {
                Population  <- rbind(Population,w3)
            }
        }
        wealth_data <- merge(wealth,Population,all.x=TRUE)
        wdata_1995_2018 <- wealth_data
        #glimpse(wealth_data)
        #table(wdata_1995_2018$countrycode)

        labor  <- read.csv('Data\\laborIncomeShare.csv') #Labour income share as a percent of GDP -- ILO modelled estimates, July 2019 (%). Downloaded from ILOSTAT. Last update on 10JAN21.
        colnames(labor) <- c("countryname", "source", "year", "labor_share")
        #glimpse(labor)
        b <- merge(wealth_data,labor, by = c("countryname","year"),all.x=T)
        wealth_data <- b

        wealth_data$H <- as.numeric(as.character(wealth_data$H))
        wealth_data$K <- as.numeric(as.character(wealth_data$K))
        wealth_data$N <- as.numeric(as.character(wealth_data$N))
        wealth_data$NforestES <- as.numeric(as.character(wealth_data$NforestES))
        wealth_data$NforestT <- as.numeric(as.character(wealth_data$NforestT))
        wealth_data$Nmangroves <- as.numeric(as.character(wealth_data$Nmangroves))
        wealth_data$Nfisheries <- as.numeric(as.character(wealth_data$Nfisheries))
        wealth_data$Nagg <- as.numeric(as.character(wealth_data$Nagg))
        wealth_data$Foreign <- as.numeric(as.character(wealth_data$Foreign))
        wealth_data$Npa <- as.numeric(as.character(wealth_data$Npa))
        wealth_data$TotalWealth <- as.numeric(as.character(wealth_data$TotalWealth))
        wealth_data$labor_share <- as.numeric(as.character(wealth_data$labor_share))
        wealth_data$Population <- as.numeric(as.character(wealth_data$Population))
        
        #glimpse(wealth_data)
        
        wealth_data <- wealth_data[,-which(names(wealth_data) %in% c("countryname.y","NA","source"))]
        #glimpse(wealth_data)
        #table(wealth_data$labor_share)
    #read datasets (end)

    # Forest Rents (start)
        Natrents  <- read.csv('Data\\ForestRents_2018.csv') #World Bank Data
        
        colnames(Natrents) <- c("countryname", "countrycode", "seriesname","seriescode",1995:2018)
        years <- c(1995:2018)
        for (i in 1:length(years)){
            yeari <- years[i]
            w2 <- Natrents[,which(colnames(Natrents) %in% 
                c("countryname", "countrycode", "seriesname",yeari))]
            w3 <- reshape(w2, idvar=c("countryname", "countrycode"), 
                timevar="seriesname", direction="wide")
            #glimpse(w3)
            w3 <- w3[,c(1,2,3)]
            w3[,4] <- yeari
            colnames(w3) <- c("countryname","countrycode","NatRents","year")
            if (i==1){Natrents2 <- w3} else {
                Natrents2  <- rbind(Natrents2,w3)
            }
        }
        #glimpse(Natrents2)
        #glimpse(wealth_data)    
        
        wealth_data <- merge(wealth_data,Natrents2, by = c("countrycode","year"),all.x=TRUE)
        #glimpse(wealth_data)
        wealth_data$NatRents <- as.numeric(as.character(wealth_data$NatRents))
        #table(wealth_data$countrycode)
    # Forest Rents (end)

    # Forest area (start)
        ForestArea  <- read.csv('Data\\ForestArea_2018.csv') #World Bank Data
        
        colnames(ForestArea) <- c("countryname", "countrycode", "seriesname","seriescode",1995:2020)
        years <- c(1995:2020)
        for (i in 1:length(years)){
            yeari <- years[i]
            w2 <- ForestArea[,which(colnames(ForestArea) %in% 
                c("countryname", "countrycode", "seriesname",yeari))]
            w3 <- reshape(w2, idvar=c("countryname", "countrycode"), 
                timevar="seriesname", direction="wide")
            #glimpse(w3)
            w3 <- w3[,c(1,2,3)]
            w3[,4] <- yeari
            colnames(w3) <- c("countryname","countrycode","ForestArea","year")
            if (i==1){ForestArea2 <- w3} else {
                ForestArea2  <- rbind(ForestArea2,w3)
            }
        }
        #glimpse(ForestArea2)
        #glimpse(wealth_data)    
        
        wealth_data <- merge(wealth_data,ForestArea2, by = c("countrycode","year"), all.x = T)
        #glimpse(wealth_data)
        wealth_data$ForestArea <- as.numeric(as.character(wealth_data$ForestArea))
    # Forest area (end)

    #Getting Continents
        library(countrycode)
        wealth_data$Continent <- countrycode(sourcevar = wealth_data[, "countryname.x"],
                                    origin = "country.name",
                                    destination = "continent")

        wealth_data$Ngreen <- wealth_data$Npa+wealth_data$NforestES+wealth_data$Nmangroves
        wdata1995 <- wealth_data[which(wealth_data$year==1995),]
        wdata2018 <- wealth_data[which(wealth_data$year==2018),]
        wdata2017 <- wealth_data[which(wealth_data$year==2017),]
        wdata2018$difN <- (wdata2018$Ngreen - wdata1995$Ngreen)/wdata1995$Ngreen
        wdata2018$changeN <- "Gain"
        wdata2018[wdata2018$difN<0,]$changeN <- "Loss"
        wdata2017 <- wdata2017[-which(is.na(wdata2017$GDP)),]
        #glimpse(wdata2018)
    #Getting Continents


    library("ggsci");library(wesanderson)
    pal <- wes_palette("Zissou1", 100, type = "continuous")
    #ggplot(data = wdata2018, aes(x=(NforestT + NforestES + Npa )/(TotalWealth),y=log(GDP/Population)))+
    # theme_bw() +
    # geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=4,size=1)+
    #   scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
    # geom_point(aes(size=(((NatRents*0.01)))),alpha=0.75)+
    # theme_bw() +
    # xlab("Natural Capital in Forests and Protected Areas \n(Fraction of Inclusive Wealth)") + 
    # ylab("Log GDP per capita (2014 USD)") + 
    # scale_colour_discrete(name="Continent")+
    # scale_x_continuous(trans='log2')+
    # #scale_shape_discrete(name="Natatural Capital change\nsince 1995")+
    # scale_size_continuous(name="Production elasticity to \nNatural Capital")
    #scale_fill_brewer(palette = "Set2")
    #scale_fill_tron()
    #scale_fill_manual(values = wes_palette("Darjeeling1", n = 5))
    
    wdata2018$Nprov <- (wdata2018$NforestT)/(wdata2018$TotalWealth)
    wdata2018$Nnonprov <- (wdata2018$NforestES + wdata2018$Npa)/(wdata2018$TotalWealth)
    
    wdata2018o <- wdata2018[which(wdata2018$Nprov > quantile(wdata2018$Nprov,0.06)),]
    wdata2018o <- wdata2018o[which(wdata2018o$Nnonprov > quantile(wdata2018o$Nnonprov,0.05)),]
    #levels(factor(wdata2018o$Continent))
    wdata2018o$Continent <- factor(wdata2018o$Continent, levels=c("Asia" ,"Oceania"  ,  "Europe"   , "Americas" ,"Africa" ))
    legend_ord <- factor(wdata2018o$Continent, levels=c( "Europe"   ,"Africa" ,"Oceania"  ,"Asia" ,  "Americas"))
    


    ggplot(data = wdata2018o, aes(y=Nprov,x=Nnonprov))+
    theme_bw() +
    geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=4,size=1)+
    scale_alpha_manual(values= c(0,0.6,0.7,0.8),guide = guide_none()) +
    geom_point(aes(size=pmax(pmin(log(GDP/Population),11),7)),alpha=0.7)+
    theme_bw() +
    ylab("Market Natural Capital \n(Fraction of Inclusive Wealth)") + 
    xlab("Nonmarket Natural Capital \n(Fraction of Inclusive Wealth)") + 
    scale_colour_discrete(name="Region")+
    scale_size_area()+
    #scale_y_continuous(trans='log',labels = scales::number_format(accuracy = 0.0001),position="right", sec.axis = dup_axis())+
    scale_y_continuous(trans='log',labels = scales::number_format(accuracy = 0.0001))+
    scale_x_continuous(trans='log',labels = scales::number_format(accuracy = 0.0001))+
    #theme(plot.title = element_text(hjust=0.5))+#,# remove titles
        #axis.title.y.left = element_blank(),
        #axis.text.y.right = element_blank(),
        #axis.ticks.y.right = element_blank())+
    #theme(legend.position="left")+
    scale_fill_discrete(name="Region",breaks=c("Africa","Oceania","Americas","Europe","Asia"))+
    #scale_shape_discrete(name="Natatural Capital change\nsince 1995")+
    scale_size_continuous(name="Log GDP \nper capita")
    ggsave("NaturalCapitals_InclusiveWealth_gdp.png",dpi=300)    
    
    
    #Read Natural Capital Wealth Accounts (start)
#Setup


    # Read PFTs at different time horizons (start)
        world <- spData::world
        isos <- world$iso_a2


        pfts <- c("bne","bine","bns","tebs","ibs","tebe","trbe","tribe","trbr","c3g","c4g") 
        clim_models <- c("hadgem2-es","gfdl-esm2m","ipsl-cm5a-lr","miroc5")
        
        for (climi in 1:length(clim_models)){
            for(h in 1:4){
                for (p in 1:length(pfts)){
                            v1 <- load(paste("Data/periods_",pfts[p],"_brick_",clim_models[climi],"_rcp6.Rdata",sep=""))
                            assign(paste("periods_",pfts[p],"_brick_",clim_models[climi],"_rcp6",sep=""), eval(parse(text=v1)))
                            v1 <- eval(parse(text=v1))
                            v1 <- t(v1)
                            if(p==1){
                                a <- as.data.frame(subset(v1,h), xy=TRUE)
                                names(a)[3] <- "Percent"
                                a$PFT <- pfts[p]
                                a$clim <-clim_models[climi]
                                a$IDcell <- seq(1:dim(a)[1])
                            }else{
                                b <- as.data.frame(subset(v1,h), xy=TRUE)
                                names(b)[3] <- "Percent"
                                b$PFT <- pfts[p]
                                b$clim <-clim_models[climi]
                                b$IDcell <- seq(1:dim(b)[1])
                                a <- rbind(a,b)
                            }
                }
                        cell_max <- aggregate(Percent~IDcell, data=a, FUN="max")
                        names(cell_max)[2] <- "maxpercent"
                        a2 <- merge(a,cell_max,by="IDcell",all=TRUE)
                        cell_max <- data.frame(IDcell=a2$IDcell[which(a2$Percent==a2$maxpercent)],dominant_pft=a2$PFT[which(a2$Percent==a2$maxpercent)])
                        cell_max <- cell_max[which(cell_max$IDcell %notin% cell_max$IDcell[duplicated(cell_max$IDcell)]),]
                        a <- merge(a,cell_max,by="IDcell",all.x=TRUE)
                        
                        a$ID <- seq(1:dim(a)[1])
                        if (climi==1 & h==1){
                            a_h <- a
                            a_h$horizon <- horizon[h]

                        }else{
                            a$horizon <- horizon[h]
                            a_h <- rbind(a_h,a)
                        }
            }
        }
    
        glimpse(a_h)
        #save(a_h,file="a_h.Rda")
        load("a_h.Rda")
        
        sw <- as_Spatial(st_cast(world))
        raster_isos <- raster::rasterize(sw[1],v1)
        isos_xy <- as.data.frame(raster_isos, xy=TRUE)
        names(isos_xy)[3] <- "countrycode"
        a_h$country <- isos_xy[,3]
        area_xy <- as.data.frame(area(raster_isos), xy=TRUE)
        names(area_xy)[3] <- "area"
        a_h$area <- area_xy[,3]
        a_h$areaPFT <- a_h$Percent * a_h$area
        PFTs11_Clims4_horizons3 <- a_h
        glimpse(PFTs11_Clims4_horizons3)
        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3[,which(names(PFTs11_Clims4_horizons3) %in% c("y","PFT","areaPFT","horizon"))]
        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3_simple[which(PFTs11_Clims4_horizons3_simple$horizon %in% c("Present","Long-term (2081-2099)")),]
        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3_simple[which(PFTs11_Clims4_horizons3_simple$areaPFT >0),]
        glimpse(PFTs11_Clims4_horizons3_simple)
        PFTs11_horizons3 <- aggregate(areaPFT~PFT+y+horizon, FUN="mean", data=PFTs11_Clims4_horizons3_simple)
        glimpse(PFTs11_horizons3)
        save(PFTs11_horizons3,file="PFTs11_horizons3.Rda")
        load("PFTs11_horizons3.Rda")

        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3[,which(names(PFTs11_Clims4_horizons3) %in% c("y","PFT","Percent","horizon"))]
        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3_simple[which(PFTs11_Clims4_horizons3_simple$horizon %in% c("Present","Long-term (2081-2099)")),]
        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3_simple[which(PFTs11_Clims4_horizons3_simple$Percent >0),]
        glimpse(PFTs11_Clims4_horizons3_simple)
        PFTs11_horizons3_perc <- aggregate(Percent~PFT+y+horizon, FUN="mean", data=PFTs11_Clims4_horizons3_simple)
        glimpse(PFTs11_horizons3_perc)
        save(PFTs11_horizons3_perc,file="PFTs11_horizons3_perc.Rda")
        load("PFTs11_horizons3_perc.Rda")
        
        save(PFTs11_Clims4_horizons3,file="PFTs11_Clims4_horizons3.Rda")
        load("PFTs11_Clims4_horizons3.Rda")
        glimpse(PFTs11_Clims4_horizons3)
    # Read PFTs at different time horizons (end)
    load("PFTs11_Clims4_horizons3.Rda")
    
    #Plot PFTs by latitude (start)
            a_h <- PFTs11_Clims4_horizons3
            a_h$horizon <- factor(a_h$horizon,levels=  c("Present","Near-term (2021-2040)","Mid-term (2041-2060)","Long-term (2081-2099)"))
            a_h$Latitude <- abs(a_h$y)
            PFT_movement <- a_h %>%
            mutate(dominant_pft=fct_reorder(.f = dominant_pft, .x = Latitude, .fun = mean))
            glimpse(a_h)
            glimpse(PFT_movement)
                
            #levels(factor(a_h$dominant_pft))
            
            #levels(factor(PFT_movement$dominant_pft))

            levels(PFT_movement$dominant_pft) <- c("Tropical broadleaved \n  evergreen",
            "Tropical broadleaved \n  evergreen (shade-intolerant)","Tropical Grass",
            "Tropical broadleaved \n  raingreen", "Temperate  broadleaved \n  evergreen","Temperate broadleaved \n summergreen",
                "Boreal/Temperate grass" , "Boreal/Temperate \n broadleaved (shade-intolerant)" , 
            "Boreal evergreen \n needleleaved (shade-intolerant)","Boreal evergreen \n needleleaved" ,"Boreal summergreen \n needleleaved")

            
            
            PFT_movement <- PFT_movement[which(!is.na(PFT_movement$dominant_pft)),]
            PFT_movement$horizon <- factor(PFT_movement$horizon,levels=  c("Long-term (2081-2099)","Mid-term (2041-2060)","Near-term (2021-2040)","Present"))
            PFT_movement$horizon <- factor(PFT_movement$horizon,levels=  c("Present","Near-term (2021-2040)","Mid-term (2041-2060)","Long-term (2081-2099)"))
            #levels(PFT_movement$horizon)
            #glimpse(PFT_movement)
            #write.csv(PFT_movement,"PFT_movement.csv")

            
            dom_mov <- ggplot(PFT_movement, aes(x = y, y = dominant_pft, fill=horizon))+
            theme_minimal()+
            geom_density_ridges2(alpha=0.6)
            
            xlab <- "Latitude (Degrees)"

            dom_mov_fin <- dom_mov + 
            labs(x=xlab,y="")+
            labs(fill="Time Horizon")+
            guides(fill=guide_legend(title.position="top", 
                                            title.hjust =0.5)) + 
            theme(legend.position='bottom')

            #dom_mov
            #ggsave("Figures/PFTDominant_Movement_NS.png",dpi=300)
            
        levels(factor(PFT_movement$PFT))
            PFT_movement_y <- aggregate(Percent ~ y + horizon + PFT, data=PFT_movement,FUN="mean")
            #glimpse(PFT_movement_y)
            PFT_movement_y$PFT <- factor(PFT_movement_y$PFT)
            PFT_movement_y$PFT <- factor(PFT_movement_y$PFT, levels=c("bns","bne","bine","ibs",
            "c3g","tebs","tebe","trbr","c4g","tribe","trbe"))

            levels(PFT_movement_y$PFT) <- c("Boreal summergreen \n needleleaved",
            "Boreal evergreen \n needleleaved",
            "Boreal evergreen \n needleleaved (shade-intolerant)",
            "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
            "Boreal/Temperate grass" ,
            "Temperate broadleaved \n summergreen",   "Temperate  broadleaved \n  evergreen",
            "Tropical broadleaved \n  raingreen","Tropical Grass", "Tropical broadleaved \n  evergreen (shade-intolerant)", "Tropical broadleaved \n  evergreen")

            

            Present_dist_col <- ggplot(PFT_movement_y[which(PFT_movement_y$horizon=="Present"),])+
            geom_col(aes(y,Percent,fill=PFT))+
            scale_fill_brewer(palette = "Paired") +
            theme_minimal() + xlab("Latitude (Degrees)") + ylab("Average pixel cover (%)")+
            ggtitle("Present") + guides(fill=guide_legend(title="Biome")) + coord_flip()

            

            PFT_movement_y_present <- PFT_movement_y[which(PFT_movement_y$horizon=="Present"),]
            glimpse(PFT_movement_y_present)

            PFT_movement_y_future <- PFT_movement_y[which(PFT_movement_y$horizon=="Long-term (2081-2099)"),]
            glimpse(PFT_movement_y_future)

            PFT_movement_y_change <- PFT_movement_y_present
            PFT_movement_y_change$Percent <- PFT_movement_y_future$Percent - PFT_movement_y_present$Percent

            pft_mov_change <- ggplot(PFT_movement_y_change)+
            geom_col(aes(y,Percent,fill=PFT))+
            geom_hline(yintercept=0)+
            ggtitle("Long-term (2081-2099)")+
            theme_minimal() + xlab("") + 
            ylab("Cover change (pp)") +
            scale_fill_brewer(palette = "Paired") +
            ylim(-30,30)+ 
            guides(fill=guide_legend(title="Biome")) + coord_flip()

            ggarrange(Present_dist_col,pft_mov_change,ncol=2,nrow=1,common.legend=TRUE,legend="right")
            #ggsave("Biome_change_latitude_vert.png",dpi=300)



            all_mov <- ggplot(PFT_movement, aes(x = y, y = dominant_pft, fill=horizon))+
            theme_minimal()+
            geom_density(alpha=0.6)
            
            xlab <- "Latitude (Degrees)"

            dom_mov_fin <- dom_mov + 
            labs(x=xlab,y="")+
            labs(fill="Time Horizon")+
            guides(fill=guide_legend(title.position="top", 
                                            title.hjust =0.5)) + 
            theme(legend.position='bottom')


            PFT_movement_y <- aggregate(Percent ~ Latitude + horizon + PFT, data=PFT_movement,FUN="mean")
            PFT_movement_y <- with(PFT_movement_y, PFT_movement_y[order(horizon, Latitude, PFT),])
            glimpse(PFT_movement_y)
            
            PFT_movement_y$changepct <- PFT_movement_y$Percent-PFT_movement_y$Percent[PFT_movement_y$horizon=="Present"]
            
            ggplot(PFT_movement_y, aes(x = changepct, y = PFT, color=horizon))+
            geom_boxplot()
            
            ggplot(PFT_movement_y, aes(x = changearea, y = PFT, fill=horizon))+
            theme_minimal()+
            geom_density_ridges2(rel_min_height = 0.03,alpha=0.6)+
            xlab("Percent Loss")+
            ylab("") +
            labs(fill="Time Horizon")+
            guides() + 
            #xlim(-2.5,2.5)+
            theme(legend.position='bottom')
            

            PFT_movement_ya <- aggregate(areaPFT ~ Latitude + horizon + PFT, data=PFT_movement,FUN="mean")
            glimpse(PFT_movement)
            
            
            PFT_movement_ya <- with(PFT_movement_ya, PFT_movement_ya[order(horizon, Latitude, PFT),])
            #PFT_movement_ya <- with(PFT_movement, PFT_movement[order(horizon),])
            glimpse(PFT_movement_ya)
            
            dim(PFT_movement_ya[which(PFT_movement_ya$horizon=="Present"),])
            PFT_movement_ya$changearea <- (PFT_movement_ya$areaPFT-PFT_movement_ya$areaPFT[PFT_movement_ya$horizon=="Present"])/PFT_movement_ya$areaPFT[PFT_movement_ya$horizon=="Present"]
            PFT_movement_ya <- PFT_movement_ya[which(!is.na(PFT_movement_ya$changearea)),]
            PFT_movement_ya <- PFT_movement_ya[which(PFT_movement_ya$changearea<quantile(PFT_movement_ya$changearea,0.9)),]
            
            
            hist(PFT_movement_ya$changearea)
            PFT_movement_ya <- PFT_movement_ya %>%
            mutate(PFT=fct_reorder(.f = PFT, .x = -Latitude, .fun = mean))
            
            levels(PFT_movement_ya$PFT) <- c("Boreal summergreen \n needleleaved" ,
            "Boreal evergreen \n needleleaved" ,
            "Boreal evergreen \n needleleaved (shade-intolerant)",
            
                "Boreal/Temperate grass" ,  
                "Boreal/Temperate \n broadleaved (shade-intolerant)" , 
            
                "Temperate broadleaved \n summergreen",
                "Temperate  broadleaved \n  evergreen",
                "Tropical Grass", "Tropical broadleaved \n  evergreen","Tropical broadleaved \n  evergreen (shade-intolerant)",
                "Tropical broadleaved \n  raingreen" )


            ggplot(PFT_movement_ya[PFT_movement_ya$horizon!="Present",], aes(x = changearea*100, y = PFT,fill=horizon))+
            theme_bw()+
            geom_density_ridges2(alpha=0.6,scale=1,rel_min_height = 0.03)+
            geom_vline(xintercept=0,linetype="dashed")+
            xlab("Area change (%)")+
            ylab("") +
            labs(fill="Time Horizon")+
            guides(fill = guide_legend(title.position="top")) + 
        
        theme_ridges() +
            theme(legend.position='bottom')+
        coord_cartesian(clip = "off")
        #ggsave("Figures/areachange_PFTs.png",dpi=300)
            


            #glimpse(wdata2018)
            #aaaa <- a_h[which(a_h$horizon=="Present"),]
            #levels(factor(aaaa$clim))
            a_h <- PFTs11_Clims4_horizons3
            
            glimpse(a_h$dominant_pft) 

            country_y <- aggregate(y~country,FUN=mean,data=a_h[which(a_h$horizon=="Present"),])
            glimpse(country_y)



            present_country_pft <- aggregate(areaPFT~PFT+country+clim,FUN=sum,data=a_h[which(a_h$horizon=="Present"),])
            #PFT_area_country <- aggregate(areaPFT~PFT+country+clim,FUN=sum,data=present_country_pft)
            #present_country_pft <- aggregate(areaPFT~PFT+country+horizon,FUN=sum,data=a_h)
            #present_country_pft[which(present_country_pft$horizon=="Present")]
            #glimpse(present_country_pft) 
            #levels(factor(present_country_pft$clim))
            PFT_area_country <- dcast(present_country_pft,country~PFT+clim, FUN=sum)
            glimpse(PFT_area_country)
            names(PFT_area_country)[2:45] <- paste("PFT.",names(PFT_area_country)[2:45],sep="")
            PFT_area_country$countrycode <- countrycode(PFT_area_country$country, origin = 'iso2c', destination = 'iso3c')
            #names(PFT_area_country)[2:12] <- paste("PFT.",names(PFT_area_country)[2:12],sep="")
            glimpse(PFT_area_country)
            
            wealth2018_pft <- merge(wdata2018,PFT_area_country,by="countrycode")
            glimpse(wealth2018_pft)
            wealth2018_pft_all <- wealth2018_pft
            
            near_country_pft <- aggregate(areaPFT~PFT+country+clim,FUN=sum,data=a_h[which(a_h$horizon=="Near-term (2021-2040)"),])
            PFT_area_country_near <- dcast(near_country_pft,country~PFT+clim)
            PFT_area_country_near$countrycode <- countrycode(PFT_area_country_near$country, origin = 'iso2c', destination = 'iso3c')
            names(PFT_area_country_near)[2:45] <- paste("PFT.",names(PFT_area_country_near)[2:45],sep="")
            glimpse(PFT_area_country_near)

            PFT_area_country_near_all <- PFT_area_country_near
            
            mid_country_pft <- aggregate(areaPFT~PFT+country+clim,FUN=sum,data=a_h[which(a_h$horizon=="Mid-term (2041-2060)"),])
            PFT_area_country_mid <- dcast(mid_country_pft,country~PFT+clim)
            PFT_area_country_mid$countrycode <- countrycode(PFT_area_country_mid$country, origin = 'iso2c', destination = 'iso3c')
            names(PFT_area_country_mid)[2:45] <- paste("PFT.",names(PFT_area_country_mid)[2:45],sep="")
            glimpse(PFT_area_country_mid)
            PFT_area_country_mid_all <- PFT_area_country_mid

            long_country_pft <- aggregate(areaPFT~PFT+country+clim,FUN=sum,data=a_h[which(a_h$horizon=="Long-term (2081-2099)"),])
            PFT_area_country_long <- dcast(long_country_pft,country~PFT+clim)
            PFT_area_country_long$countrycode <- countrycode(PFT_area_country_long$country, origin = 'iso2c', destination = 'iso3c')
            names(PFT_area_country_long)[2:45] <- paste("PFT.",names(PFT_area_country_long)[2:45],sep="")
            glimpse(PFT_area_country_long)
            PFT_area_country_long_all <- PFT_area_country_long


                dat<- es_ac_notFood
                #spatial extents are not standardized units - convert Hectares to Acres
                dat$Spatial.Extent[which(dat$Spatial.Unit.Published=="Hectares")]<-dat$Spatial.Extent[which(dat$Spatial.Unit.Published=="Hectares")]*2.471
                #limit to observations with spatial extent in either acres or hectares (now converted to acres)
                dat=dat%>%
                filter(Spatial.Unit.Published%in%c("Acres","Hectares"))​
                a=ggplot(dat,aes(x=log(Spatial.Extent),y=log(Single.Value.Converted)))+geom_point()
                a=a+geom_smooth(method="lm") + theme_bw()

                x11()
                a
                dat$Category=ifelse(dat$Ecosystem.Service.Category=="Provisioning","Provisioning","Non-Provisioning")
                b=ggplot(dat,aes(x=log(Spatial.Extent),y=log(Single.Value.Converted),col=Category))+geom_point()
                b=b+geom_smooth(method="lm")
                b+theme_bw()+xlab("Log Spatial extent") + ylab("Log value of benefits ($/ha)")
                #look at linear model controlling for GDP
                mod=lm(log(Single.Value.Converted)~log(Spatial.Extent)*Category+log(gdp_pc),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0))
                elasticity_area <- summary(mod)$coefficients[2]
                ggsave("Valuechange_byArea.png",dpi=300)
    #Plot PFT by latitude (end)
        save(a_h,file=".Rda")
    
    #Calculate NatCap loss based on PFT (start)   
   
        TypeES <- c("All","Provisioning","Non-provisioning")
            firstcountry =1
        for(j in 1:177){
                for(typeESi in 1:3){
                    for (climi in 1:length(clim_models)){
                        PFT_movement <- a_h[which(a_h$Percent>0 & a_h$clim==clim_models[climi]),]
                        PFT_movement$PFT <- factor(PFT_movement$PFT)
                        PFT_movement$PFT_code <- PFT_movement$PFT
                        levels(PFT_movement$PFT) <- c("Boreal evergreen \n needleleaved (shade-intolerant)",
                            "Boreal evergreen \n needleleaved",
                            "Boreal summergreen \n needleleaved",
                            "Boreal/Temperate grass",
                            "Tropical Grass",
                            "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
                            "Temperate  broadleaved \n  evergreen",
                            "Temperate broadleaved \n summergreen",
                            "Tropical broadleaved \n  evergreen" ,
                            "Tropical broadleaved \n  raingreen" ,
                            "Tropical broadleaved \n  evergreen (shade-intolerant)")
                            erase_clim <- clim_models[clim_models %notin% clim_models[climi]]
                            wealth2018_pft <- wealth2018_pft_all
                            PFT_area_country_mid <- PFT_area_country_mid_all
                            PFT_area_country_near <- PFT_area_country_near_all
                            PFT_area_country_long <- PFT_area_country_long_all
                            for (eclim in 1:length(erase_clim)){
                            wealth2018_pft <- wealth2018_pft[,-grep( erase_clim[eclim], colnames(wealth2018_pft))]
                            PFT_area_country_near <- PFT_area_country_near[,-grep( erase_clim[eclim], colnames(PFT_area_country_near))]
                            PFT_area_country_mid <- PFT_area_country_mid[,-grep( erase_clim[eclim], colnames(PFT_area_country_mid))]
                            PFT_area_country_long <- PFT_area_country_long[,-grep( erase_clim[eclim], colnames(PFT_area_country_long))]

                            }
                            names(wealth2018_pft)[grep("PFT.",names(wealth2018_pft))] <-  paste0("PFT.",levels(factor(PFT_movement$PFT_code)))
                            names(PFT_area_country_near)[grep("PFT.",names(PFT_area_country_near))] <-  paste0("PFT.",levels(factor(PFT_movement$PFT_code)))
                            names(PFT_area_country_mid)[grep("PFT.",names(PFT_area_country_mid))] <-  paste0("PFT.",levels(factor(PFT_movement$PFT_code)))
                            names(PFT_area_country_long)[grep("PFT.",names(PFT_area_country_long))] <-  paste0("PFT.",levels(factor(PFT_movement$PFT_code)))
                            #glimpse(PFT_area_country_near)
                    if(is.na(isos[j])){next}
                    PFT_movement2 <- PFT_movement[which(PFT_movement$country==isos[j]),]
                    if(isos[j]=="US"){
                        PFT_movement2 <- PFT_movement2[which(PFT_movement2$y<50 & PFT_movement2$y>25),]
                        }
                        
                    country_area <- world$area_km2[world$iso_a2==isos[j]][1]
                    #glimpse(PFT_movement2)

                    if(length(which(wealth2018_pft$country==isos[j]))==0){
                            print(paste("No World Bank Data for",isos[j]))
                            next
                        }

                    PFT_movement2 <- PFT_movement2 %>%
                    mutate(PFT=fct_reorder(.f = PFT, .x = -Percent, .fun = mean))

                    if(dim(PFT_movement2)[1]==0){next}

                


                    # plot1_ridge <- ggplot(PFT_movement2, aes(x = Percent, y = PFT, fill=horizon))+
                    # theme_bw()+
                    # geom_density_ridges2(alpha=0.5)+
                    # xlab("Land cover within gridcells (%)")+
                    # ylab("") +
                    # labs(fill="Time Horizon")+
                    # guides(fill = guide_legend(reverse=TRUE)) + 
                    # #scale_x_continuous(trans='log2') +
                    # theme(legend.position='bottom') +
                    # ggtitle("")
                    #ggsave(plots,filename=paste("Figures/Landcover", isos[j], ".png", sep=""),dpi=300)


                    PFT_movement2 <- droplevels(PFT_movement2)
                    
                    areasPFTs1 <- aggregate(areaPFT ~ horizon + PFT + country, data = PFT_movement2, FUN = sum)
                    areasPFTs1$pfth <- factor(paste(areasPFTs1$PFT,areasPFTs1$horizon))
                    #glimpse(areasPFTs1)

                    grid_pft_h <- expand.grid(PFT=levels(PFT_movement2$PFT),horizon=levels(factor(PFT_movement2$horizon)))
                    grid_pft_h$pfth <- factor(paste(grid_pft_h$PFT,grid_pft_h$horizon))
                    #glimpse(grid_pft_h)

                    areasPFTs <- merge(grid_pft_h,areasPFTs1[,3:5],by="pfth",all.x=TRUE)
                    #glimpse(areasPFTs)
                    areasPFTs$areaPFT[is.na(areasPFTs$areaPFT)] <- 0

                    areasPFTs$horizon <- factor(areasPFTs$horizon, levels=c('Present','Near-term (2021-2040)','Mid-term (2041-2060)','Long-term (2081-2099)'))
                    
                    areasPFTs<-areasPFTs[order(areasPFTs$horizon,areasPFTs$PFT),]

                    
                    areasPFTs$arealoss <- areasPFTs$areaPFT - areasPFTs$areaPFT[areasPFTs$horizon=="Present"]

                    areasPFTs$PFT <-  factor(areasPFTs$PFT,levels=levels(PFT_movement2$PFT))
                    areasPFTs$horizon <- factor(areasPFTs$horizon, levels=levels(factor(PFT_movement2$horizon)))



                    
                    # plot2_area <- ggplot(areasPFTs, aes(x=PFT,y=arealoss*0.01,fill=horizon))+
                    # geom_bar(stat="identity", position=position_dodge())+
                    #  ylab("Area change relative to Present (ha)")+xlab("")+
                    # theme_bw()+
                    # theme(legend.position='bottom',axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+ 
                    # coord_flip()

                    # plot <- ggarrange(plot1_ridge,plot2_area,
                    #     common.legend=TRUE,legend="bottom",
                    #     widths = 1,
                    #     heights = 1,align = "h")
                    
                    # plots <- annotate_figure(plot, top = text_grob(world$name_long[which(world$iso_a2==isos[j])], 
                    #        face = "bold", size = 14))

                    #ggsave(plots,filename=paste("Figures/LandcoverChange", isos[j], ".png", sep=""),dpi=300)

                    

                

                #  ggplot(PFT_movement2, aes(x = Percent, y = PFT,fill = factor(stat(quantile))))+
                #     theme_bw()+
                #     stat_density_ridges(
                #         geom = "density_ridges_gradient",
                #         calc_ecdf = TRUE,
                #         quantiles = 0.25) +
                #     scale_fill_manual(
                #         name = "Quantile", values = c("#FF0000A0", "#A0A0A0A0"),
                #         labels = c("(0, 0.01]", "(0.01,1]")
                #     ) +
                #     xlab("Land cover within gridcells (%)")+
                #     ylab("") +
                #     labs(fill="Time Horizon")+
                #     #guides(fill = guide_legend(reverse=TRUE)) + 
                #     #scale_x_continuous(trans='log2') +
                #     theme(legend.position='bottom') +
                #     ggtitle("")
                    
                    
                    #aggregate(Percent ~ PFT_code, PFT_movement2, function(x) quantile(x, probs = 0.25))


                        p <- levels(PFT_movement2$PFT_code)
                        ps <- paste0("PFT.",p)
                        
                        areasPFTs1 <- aggregate(areaPFT ~ horizon + PFT + country+PFT_code, data = PFT_movement2, FUN = sum)
                        
                        areasPFTs2 <- areasPFTs1[which(areasPFTs1$PFT_code %notin% c("c3g","c4g")),]

                        q25mx<-areasPFTs2$PFT_code[which(areasPFTs2$areaPFT==max(areasPFTs2$areaPFT))]
                        
                        #q10 <- aggregate(Percent ~ PFT_code, PFT_movement2, function(x) quantile(x, probs = 0.1))
                        #q25mx<- q10$PFT_code[which(q10$Percent==max(q10$Percent))]
                        #subset_es <- es_oa[which(eval(parse(text=paste("es_oa$PFT.",q25mx,sep="")))>min(q10$Percent)),]
                        
                extra_ind_vars <- c("pct_covered","logGDP")
                ps_all <- paste(c(ps,extra_ind_vars),collapse="+")
                #subset_es <- es_oa[which(eval(parse(text=paste("es_oa$PFT.",q25mx,sep="")))>0),]
                if(TypeES[typeESi]=="Provisioning"){
                    ESCat <- c("Provisioning")
                    typeES <- c("NforestT")}
                if(TypeES[typeESi]=="Non-provisioning"){
                    ESCat <- c("Information" ,"Regulating"  , "Supporting")
                    typeES <- c("NforestES","Npa")}
                if(TypeES[typeESi]=="All"){
                    ESCat <- c("Information" , "Provisioning" ,"Regulating"  , "Supporting")
                    typeES <- c("NforestES","NforestT","Npa")}


                        subset_es <-es_ac_notFood[which((eval(parse(text=paste("es_ac_notFood$PFT.",q25mx,sep="")))>0) & 
                        es_ac_notFood$Ecosystem.Service.Category %in% ESCat),]
                        
                        #subset_es <-es_ac_Noprov[which(eval(parse(text=paste("es_ac_Noprov$PFT.",q25mx,sep="")))>0),]
                        #subset_es
                        #subset_es <- subset_es[which(!is.na(subset_es[,names(subset_es)%in%c(ps,extra_ind_vars)])),]
                        #subset_es <- es_oa[]
                        #glimpse(subset_es)
                        if(dim(subset_es)[2] < 5){
                            print("subset less than 5 observations")
                            next
                        }
                        #lm(logESValue~PFT.ibs,data=subset_es)
                        
                        if(length(ps)==1){
                            print("Only one PFT")
                            next
                        }

                        ind_vars <- subset_es[,names(subset_es)%in%c(ps,extra_ind_vars)]
                        if(dim(ind_vars)[1]<6){
                            print(paste("Not enough observations in dataset for",isos[j]))
                            next
                        }
                        zer_ind <- lapply(ind_vars, function(x) all(x == 0))
                        zeropfts <- names(unlist(zer_ind))[unlist(zer_ind)]

                        

                        trf <- tuneRF(ind_vars, subset_es$logESValue)
                        #trf <- tuneRF(ind_vars, subset_es$Single.Value.Converted)
                        
                        
                        mt <- trf[which.min(trf[,2]), 1]
                        #data_rf <- cbind(ind_vars, ES = subset_es$logESValue)
                        #ES.rfsrc <- rfsrc(ES ~ ., data = data_rf)
                        #plot(vimp(ES.rfsrc)$importance)
                        
                        # for (part 1:(length(names(data_rf))-3)){
                        #     partial.obj <- partial.rfsrc(ES.rfsrc,partial.xvar=names(data_rf)[1],partial.values = ES.rfsrc$xvar[,1])
                        #     pdta <- get.partial.plot.data(partial.obj)
                        #     summary(lm(pdta$yhat ~ pdta$x))$coeff[2,1]

                        # }
                        # install.packages("ggRandomForests")
                        # library("ggRandomForests")

                        # airq.obj <- holdout.vimp(ES ~ ., data = data_rf, na.action = "na.impute")
                        # print(airq.obj$importance)


                        # v <- vimp(ES.rfsrc,joint=TRUE, csv = TRUE)
                        # csvimp <- get.mv.csvimp(v, standardize=TRUE)
                        # print(csvimp)


                        
                        # plot(pdta$x, pdta$yhat, type = "b", pch = 16,
                        #     xlab = "wind", ylab = "partial effect of wind")
                        # plot.variable(ES.rfsrc, partial = TRUE, smooth.lines = TRUE, sorted=TRUE)



                        Results_rf <- randomForest(ind_vars, subset_es$logESValue, importance = TRUE,tree = TRUE, mtry =mt, ntree = 200)
                        #summary(Results_rf)[3]

                        #getTree(Results_rf, labelVar=TRUE)
                        #MDSplot(Results_rf)
                        #partialPlot(Results_rf,cbind(ind_vars, subset_es$logESValue),x.var="PFT.trbe")
                        #partialPlot(Results_rf,cbind(ind_vars, subset_es$logESValue),x.var="PFT.tribe")
                        #partialPlot(Results_rf,cbind(ind_vars, subset_es$logESValue),x.var="PFT.c4g")
                        #partialPlot(Results_rf,cbind(ind_vars, subset_es$logESValue),x.var="PFT.c3g")
                        #partialPlot(Results_rf,cbind(ind_vars, subset_es$logESValue),x.var="PFT.trbr")
                        #partialPlot(Results_rf,cbind(ind_vars, subset_es$logESValue),x.var="PFT.bne")
                        #partialPlot(Results_rf,cbind(ind_vars, subset_es$logESValue),x.var="pct_covered")
                        #partialPlot(Results_rf,cbind(ind_vars, subset_es$logESValue),x.var="logGDP")

                        #varImp(Results_rf, conditional=TRUE)

                        #plot(Results_rf)
                        pft_pct_pred = aggregate(Percent  ~ horizon + PFT_code, data = PFT_movement2, FUN = "mean")

                        #levels(factor(pft_pct_pred[,2]))
                        pft_pct_pred_dgvm <- pft_pct_pred[which(pft_pct_pred$horizon=="Present"),which(names(pft_pct_pred)%in% c("PFT_code","Percent"))]
                        
                        
                        pft_pct_pred_database <- colMeans(ind_vars)
                        eff_pft <- data.frame(matrix(NA, nrow =(length(pft_pct_pred_database)[1]-2), ncol = 2))
                        names(eff_pft) <- c("variable","percent")
                        eff_pft[,1] <- names(pft_pct_pred_database)[1:(length(pft_pct_pred_database)-2)]
                        
                        for(n in 1:(length(pft_pct_pred_database)-length(extra_ind_vars))){
                            eff_pred <- pft_pct_pred_database
                            if (length(pft_pct_pred_dgvm$Percent[(which(paste0("PFT.",pft_pct_pred_dgvm$PFT_code)==names(eff_pred)[n]))])!=0){
                                eff_pred[n] <- pft_pct_pred_dgvm$Percent[(which(paste0("PFT.",pft_pct_pred_dgvm$PFT_code)==names(eff_pred)[n]))]
                            }
                            eff_pft[n,2] <- predict(Results_rf,eff_pred)
                        }

                        eff_pft$importance <- eff_pft[,2]/max(eff_pft[,2])
                        

                        


                        # pct_area_pred <- aggregate(Percent  ~ horizon + x+y, data = PFT_movement2, FUN = "sum")
                        # percent_covered <- pct_area_pred[which(pct_area_pred$horizon=="Present"),which(names(pct_area_pred)=="Percent")]
                        # Area_pred <- aggregate(area  ~ horizon + x+y, data = PFT_movement2, FUN = "mean")
                        # Area_pred <- Area_pred[which(Area_pred$horizon=="Present"),which(names(Area_pred)=="area")]
                        # pct_cover_pred <- weighted.mean(x=percent_covered, w=Area_pred)
                        # logGDP_pred <- log(wdata2018$GDP[wdata2018$countrycode==countrycode(isos[j],origin="iso2c",destination="iso3c")])
                        # Continent_pred <- wdata2018$Continent[wdata2018$countrycode==countrycode(isos[j],origin="iso2c",destination="iso3c")]
                        # year_pred <- 2018




                        # if(length(logGDP_pred)==0){
                        #     negative_pfts <- NA
                        # } else if(is.na(logGDP_pred))   {
                        #     negative_pfts <- NA
                            
                            
                        # }else{

                        #     if((length(pft_pct_pred)-length(extra_ind_vars))==length(ps)){
                        #     marginal_eff_pft <- data.frame(matrix(NA, nrow =(length(pft_pct_pred)[1]-2), ncol = 2))
                            
                            
                        #     #baseline <- predict(Results_rf,c(pft_pct_pred[,2],pct_cover_pred,logGDP_pred))
                        #     #marginal_eff_pft[,1] <- paste0("PFT.",pft_pct_pred[,1])

                        #     #partialPlot(Results_rf,c(pft_pct_pred[,2], pct_cover_pred,logGDP_pred,10),x.var="PFT.c3g")

                        #     pft_pct_pred <- colMeans(ind_vars)
                        #     marginal_eff_pft[,1] <- names(pft_pct_pred)[1:(length(pft_pct_pred)-2)]
                        #     #baseline <- predict(Results_rf,pft_pct_pred)
                            
                        #     for(n in 1:(length(pft_pct_pred)-length(extra_ind_vars))){
                        #             seq_pred <- seq(from=min(ind_vars[n]), to=max(ind_vars[n]),length.out=50)
                        #             new_pred <- rep(NA,50)
                        #         for(predi in 1:50){
                        #             pft_pct_pred[n] <- seq_pred[predi]
                        #             new_pred[predi] <- predict(Results_rf,pft_pct_pred)
                        #         }
                        #         marginal_eff_pft[n,2] <- summary(lm(new_pred~seq_pred))$coef[2,1]
                                
                        #         pft_pct_pred <- colMeans(ind_vars)
                        #     }
                        #     marginal_eff_pft
                        #     negative_pfts <- marginal_eff_pft[which(marginal_eff_pft[,2]<0),1]
                        #     } else{negative_pfts <- NA}}

                            #partialPlot(Results_rf,cbind(ind_vars, subset_es$logESValue),x.var="PFT.bne")
                        





                        # #if(j==94){
                        #    # print("Not enough trees in the RF")
                        #    # implemented since j=94
                        #  #   next}
                        # min_depth_frame <- min_depth_distribution(Results_rf)
                        
                        # plotmin <- plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15) + ggtitle(world$name_long[which(world$iso_a2==isos[j])])
                        # #ggsave(plotmin,filename=paste("Figures/RelativeImportance", isos[j], ".png", sep=""),dpi=300)
                        # #ggsave("Figures/Min_depth_RF.png", dpi=600)

                    
                    
                        # length_minval <- aggregate(minimal_depth~variable, data=min_depth_frame, FUN = length)
                        # length_minval <- length_minval[length_minval$variable %notin% extra_ind_vars,]
                        # order_pfts <- data.frame(names(ind_vars),seq(1:length(ind_vars)))
                        # names(order_pfts) <- c("variable","order")
                        # length_minval <- merge(order_pfts,length_minval,by="variable")
                        # length_minval <- length_minval[order(length_minval$order),]

                        # maxnum <- max(length_minval$minimal_depth)
                        
                        # mean_minval <- aggregate(minimal_depth~variable, data=min_depth_frame, FUN = "mean")
                        # mean_minval <- merge(order_pfts,mean_minval,by="variable")
                        # mean_minval <- mean_minval[order(mean_minval$order),]

                        
                        
                        # mean_minval <- mean_minval[mean_minval$variable %notin% extra_ind_vars,]
                        # #marginal_eff_pft[which(marginal_eff_pft[,2]<0),]

                        
                        

                        # mean_minval$minimal_depth[mean_minval$variable %in% negative_pfts] <-  abs(marginal_eff_pft[which(marginal_eff_pft[,2]<0),2]) * 
                        #     (max(min_depth_frame$minimal_depth) - max(mean_minval$minimal_depth))/max(abs(marginal_eff_pft[which(marginal_eff_pft[,2]<0),2]))

                        # #mean_minval$minimal_depth[mean_minval$variable %in% c("PFT.c4g","PFT.c3g")] <- max(mean_minval$minimal_depth)


                        


                        
                        # mean_minval$adjusted <- (mean_minval$minimal_depth * length_minval$minimal_depth + 
                        #     (maxnum - length_minval$minimal_depth)*max(min_depth_frame$minimal_depth))/maxnum

                        # relative_imp <- abs(mean_minval$adjusted- max(min_depth_frame$minimal_depth,na.rm=TRUE))
                        # mean_minval$percent <- relative_imp / sum(relative_imp)
                        # mean_minval$relative <- mean_minval$percent / max(mean_minval$percent)

                        # if(length(negative_pfts)>0){
                        #     new_order_plot <- ggplot(mean_minval, aes(x = reorder(variable, relative), y = relative, fill = variable)) + 
                        #     geom_bar(stat = "identity")+theme_bw()+coord_flip()
                            
                        #     plot_com_order <- ggarrange(plotmin,new_order_plot)
                        #     #ggsave(plot_com_order,filename=paste("Figures/Comparison_Importance_order/RelativeImportance_change_", isos[j], ".png", sep=""),dpi=300)
                        
                            
                        # }

                        ### Now the Natural Capital Estimates

                        
                        

                        #sum_areasPFTs <- sum(wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) %in% mean_minval$variable)]*mean_minval$relative)
                        areasPFTs_database <- wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) %in% eff_pft$variable)]
                        sum_areasPFTs <- sum(wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) %in% eff_pft$variable)]*eff_pft$importance[order(eff_pft$variable,names(areasPFTs_database))])
                        
                        
                        if(sum_areasPFTs==0){print("sum areas are zero")
                        next}
                        r = 0.03
                    
                        ES = sum(wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) %in% typeES)] * r / sum_areasPFTs)
                        
                    


                        areas <- c(unlist(wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) %in% eff_pft$variable)]),
                                unlist(PFT_area_country_near[which(PFT_area_country_near$country==isos[j]),which(names(PFT_area_country_near) %in% eff_pft$variable)]),
                                unlist(PFT_area_country_mid[which(PFT_area_country_mid$country==isos[j]),which(names(PFT_area_country_mid) %in% eff_pft$variable)]),
                                unlist(PFT_area_country_long[which(PFT_area_country_long$country==isos[j]),which(names(PFT_area_country_long) %in% eff_pft$variable)]))
                        horizons <- rep(c("Present", "Near-term (2021-2040)","Mid-term (2041-2060)","Long-term (2081-2099)"),each=length(eff_pft$variable))
                        
                        areas_change_pct <- 100*(areas - unlist(wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) %in% eff_pft$variable)]))/unlist(wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) %in% eff_pft$variable)])
                        
                        areas_change_pct_adjusted <- areas_change_pct
                        areas_change_pct_adjusted[which(!is.finite(areas_change_pct_adjusted))]<- 0
                        areas_change_pct_adjusted[which(areas_change_pct_adjusted< -99)]<- -99
                        areas_change_pct_adjusted[which(areas_change_pct_adjusted> 999)]<- 999

                        PFT_ES <-data.frame(PFT=rep(eff_pft$variable[order(eff_pft$variable,names(areasPFTs_database))],4),
                            relative_cont=rep(eff_pft$importance[order(eff_pft$variable,names(areasPFTs_database))],4),
                            area=areas,horizon=horizons,areas_change_pct = areas_change_pct,areas_change_pct_adjusted=areas_change_pct_adjusted)
                        
                        PFT_ES$ES_rel <- PFT_ES$relative_cont*ES     
                        PFT_ES$ES_rel_areachange <- PFT_ES$ES_rel + 0.01*PFT_ES$ES_rel*PFT_ES$areas_change_pct_adjusted * elasticity_area
                        
                        PFT_ES$ES_total <-  PFT_ES$ES_rel *PFT_ES$area
                        PFT_ES$ES_total_areachange <-  PFT_ES$ES_rel_areachange *PFT_ES$area
                        PFT_ES$country <- isos[j]
                        PFT_ES$clim <- clim_models[climi]
                        PFT_ES$TypeES <- TypeES[typeESi]


                        # plot_benefits <- ggplot(PFT_ES, aes(x=PFT,y=ES_total,size=(ES_rel_areachange-ES_rel),color=horizon))+
                        # geom_point()+theme_bw()+
                        # ylab("Total benefits per PFT") +ggtitle(world$name_long[which(world$iso_a2==isos[j])]) + 
                        # guides(size=guide_legend(title="Unit benefits ($/ha)"),color=guide_legend(title="Time horizon"))
                        # ggsave(plot_benefits,filename=paste("Figures/Benefit_change", isos[j],clim_models[climi] ,".png", sep=""),dpi=300)
                        



                        NaturalCapital <- aggregate(ES_total~horizon,data=PFT_ES,FUN="sum")
                        NaturalCapital$N <- NaturalCapital$ES_total/r
                        NaturalCapital_areachange <- aggregate(ES_total_areachange~horizon,data=PFT_ES,FUN="sum")
                        NaturalCapital$N_areachange <- NaturalCapital_areachange$ES_total_areachange/r
                        NaturalCapital$country <- isos[j]
                        NaturalCapital$GDP <- wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) == "GDP")]
                        NaturalCapital$TotalWealth <- wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) == "TotalWealth")]
                        NaturalCapital$NatRents <- wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) == "NatRents")]
                        NaturalCapital$temp <- c(Tlong,Tmed,Tshort,0)
                        NaturalCapital$Damage <- NaturalCapital$N/NaturalCapital$N[which(NaturalCapital$horizon=="Present")] - 1
                        NaturalCapital$Damage_areachange <- NaturalCapital$N_areachange/NaturalCapital$N_areachange[which(NaturalCapital$horizon=="Present")] - 1
                        NaturalCapital$NLoss <- NaturalCapital$N-NaturalCapital$N[which(NaturalCapital$horizon=="Present")] 
                        NaturalCapital$NLoss_areachange <- NaturalCapital$N_areachange-NaturalCapital$N_areachange[which(NaturalCapital$horizon=="Present")] 
                        NaturalCapital$Population <- wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) == "Population")]
                        
                        
                        if(sum(complete.cases(NaturalCapital$Damage))!=0){
                            NaturalCapital$DamageEstimateT2 <- coef(summary(lm(Damage ~ 0 + I(temp^2), data = NaturalCapital )))[1]
                            NaturalCapital$DamageEstimateT2_areachange <- NA
                            if(dim(NaturalCapital[which(is.finite(NaturalCapital$Damage_areachange)),])[1]>0){
                                NaturalCapital$DamageEstimateT2_areachange <- coef(summary(lm(Damage_areachange ~ 0 + I(temp^2), data = NaturalCapital[which(is.finite(NaturalCapital$Damage_areachange)),] )))[1]
                            }
                        } else {
                            NaturalCapital$DamageEstimateT2_areachange <- NA
                            NaturalCapital$DamageEstimateT2 <- NA
                        }
                        
                        NaturalCapital$clim <- clim_models[climi]
                        NaturalCapital$TypeES <- TypeES[typeESi]
                    
                        if(firstcountry == 1){
                            NC_change <- NaturalCapital
                            PFT_ES_all <- PFT_ES
                            firstcountry<-0
                        } else{
                            NC_change <- rbind(NC_change,NaturalCapital)
                            PFT_ES_all <- rbind(PFT_ES_all,PFT_ES)
                        }

                        print(paste("country =",j,"/177.  Clim =",climi,"/4"))
                }
                }
        }

    #Calculate NatCap based on PFT movement

    save(NC_change,file="NC_change_controllingGDPandArea_es_ac_notFood_adjustedImportance_allClimProjections.Rda")
    save(PFT_ES_all,file="PFT_ES_all_controllingGDPandArea_es_ac_notFood_adjustedImportance_allClimProjections.Rda")
   
## ES values per hectare

    load("NC_change_controllingGDPandArea_es_ac_notFood_adjustedImportance_allClimProjections.Rda")
    load("PFT_ES_all_controllingGDPandArea_es_ac_notFood_adjustedImportance_allClimProjections.Rda")

    PFT_ES_all$PFT <- factor(PFT_ES_all$PFT)
    
    levels(PFT_ES_all$PFT) <- c("Boreal evergreen \n needleleaved (shade-intolerant)",
                    "Boreal evergreen \n needleleaved",
                    "Boreal summergreen \n needleleaved",
                    "Boreal/Temperate grass",
                    "Tropical Grass",
                    "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
                    "Temperate  broadleaved \n  evergreen",
                    "Temperate broadleaved \n summergreen",
                    "Tropical broadleaved \n  evergreen" ,
                    "Tropical broadleaved \n  raingreen" ,
                    "Tropical broadleaved \n  evergreen (shade-intolerant)")

                PFT_ES_all$PFT <-  factor(PFT_ES_all$PFT, levels=c(
                    "Boreal summergreen \n needleleaved",
                    "Boreal evergreen \n needleleaved",
                    "Boreal evergreen \n needleleaved (shade-intolerant)",
                    "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
                    "Boreal/Temperate grass",
                    "Temperate broadleaved \n summergreen",
                    "Temperate  broadleaved \n  evergreen",
                    "Tropical broadleaved \n  raingreen" ,
                    "Tropical Grass",
                    "Tropical broadleaved \n  evergreen (shade-intolerant)",
                     "Tropical broadleaved \n  evergreen"))

    glimpse(PFT_ES_all)
    #ggplot(PFT_ES_all[which(PFT_ES_all$horizon=="Present" & PFT_ES_all$TypeES !="All"),]) +
    ggplot(PFT_ES_all) +
    geom_boxplot(aes(x=(ES_rel),y=PFT,fill=TypeES)) +
    theme_bw() 
 
    glimpse(world)
    
    country_continent_dataframe <- as.data.frame(world[,which(names(world) %in% c("continent","iso_a2"))])
    PFT_ES_all <- merge(country_continent_dataframe[,1:2],PFT_ES_all,by.x="iso_a2",by.y="country")
    names(PFT_ES_all)[2] <- "continent"
    names(PFT_ES_all)[1] <- "country"

    country_GDP_dataframe <- as.data.frame(wdata2018o[,which(names(wdata2018o) %in% c("countrycode","GDP"))])
    country_GDP_dataframe$country <- countrycode(country_GDP_dataframe$countrycode, origin = "iso3c", destination="iso2c")
    PFT_ES_all <- merge(country_GDP_dataframe[,2:3],PFT_ES_all,by.x="country",by.y="country")
    
    
    
    PFT_ES_all$continent[which(PFT_ES_all$iso_a2=="US")] <- "United States"
    #PFT_ES_all$continent[which(PFT_ES_all$iso_a2=="CA")] <- "Canada"
    
    glimpse(PFT_ES_all)
    ggplot(PFT_ES_all[which(PFT_ES_all$horizon=="Long-term (2081-2099)" & PFT_ES_all$TypeES =="Non-provisioning" & PFT_ES_all$clim =="hadgem2-es"),]) +
    geom_point(aes(x=log(ES_total),y=(areas_change_pct_adjusted),size=area,color=continent,shape=TypeES)) +
    theme_bw() #
    #+ ylab("Area change in long term horizon (%)") +
    #xlab("Log ecosystem benefits ($/year)")+ ylim(c(-100,250))


    PFT_ES_all$ES_tot <- PFT_ES_all$ES_total_areachange
    PFT_ES_all$ES_tot[which(PFT_ES_all$TypeES=="Provisioning")] <- PFT_ES_all$ES_total[which(PFT_ES_all$TypeES=="Provisioning")]
    
    glimpse(PFT_ES_all)
    PFT_ES_all$ES_totfrac <- PFT_ES_all$ES_tot/PFT_ES_all$GDP
    PFT_ES_all_mean <- aggregate(ES_totfrac~TypeES+PFT+horizon+continent, FUN="mean", data=PFT_ES_all)
    glimpse(PFT_ES_all_mean)

    
    PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~TypeES+PFT+horizon+continent, FUN="mean", data=PFT_ES_all)
    glimpse(PFT_ES_all_mean2)


    PFT_ES_all_mean <-cbind(PFT_ES_all_mean,PFT_ES_all_mean2$areas_change_pct_adjusted)
    names(PFT_ES_all_mean)[length(PFT_ES_all_mean)] <- "areas_change_pct_adjusted"

    glimpse(PFT_ES_all_mean)
    PFT_ES_all_mean$PFT <- factor(PFT_ES_all_mean$PFT)
                PFT_ES_all_mean$PFT_code <- PFT_ES_all_mean$PFT
                
                levels(PFT_ES_all_mean$PFT) <- c("Boreal evergreen \n needleleaved (shade-intolerant)",
                    "Boreal evergreen \n needleleaved",
                    "Boreal summergreen \n needleleaved",
                    "Boreal/Temperate grass",
                    "Tropical Grass",
                    "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
                    "Temperate  broadleaved \n  evergreen",
                    "Temperate broadleaved \n summergreen",
                    "Tropical broadleaved \n  evergreen" ,
                    "Tropical broadleaved \n  raingreen" ,
                    "Tropical broadleaved \n  evergreen (shade-intolerant)")

                PFT_ES_all_mean$PFT <-  factor(PFT_ES_all_mean$PFT, levels=c(
                    "Boreal summergreen \n needleleaved",
                    "Boreal evergreen \n needleleaved",
                    "Boreal evergreen \n needleleaved (shade-intolerant)",
                    "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
                    "Boreal/Temperate grass",
                    "Temperate broadleaved \n summergreen",
                    "Temperate  broadleaved \n  evergreen",
                    "Tropical broadleaved \n  raingreen" ,
                    "Tropical Grass",
                    "Tropical broadleaved \n  evergreen (shade-intolerant)",
                     "Tropical broadleaved \n  evergreen"))

                    # PFT_ES_all_mean$PFT <-  factor(PFT_ES_all_mean$PFT, levels=c(
                    # "Tropical broadleaved \n  evergreen",
                    # "Tropical broadleaved \n  evergreen (shade-intolerant)",
                    # "Tropical Grass",
                    # "Tropical broadleaved \n  raingreen" ,
                    # "Temperate  broadleaved \n  evergreen",
                    # "Temperate broadleaved \n summergreen",
                    # "Boreal/Temperate grass",
                    # "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
                    # "Boreal evergreen \n needleleaved (shade-intolerant)",
                    # "Boreal evergreen \n needleleaved",
                    # "Boreal summergreen \n needleleaved" ))

            ggplot(PFT_ES_all_mean[PFT_ES_all_mean$TypeES!="All",], aes(x = PFT, y = log(ES_tot), fill= continent)) +
                coord_polar(theta = "x", start = -.13) +
                geom_bar(stat = "identity", width = .9) + theme_minimal() + facet_wrap(~TypeES)
                
                
                
                pal_boreal <- colorRampPalette(brewer.pal(4, "PuBu"))
                pal_temp <- colorRampPalette(brewer.pal(4, "RdPu"))
                pal_trop <- colorRampPalette(brewer.pal(4, "Greens"))

                PFT_ES_all_mean_np <- PFT_ES_all_mean[PFT_ES_all_mean$TypeES=="Non-provisioning" & PFT_ES_all_mean$horizon=="Present",]
                
                PFT_ES_all_mean_np$continent <- reorder(PFT_ES_all_mean_np$continent, PFT_ES_all_mean_np$ES_totfrac, FUN = sum)
                levels(factor(PFT_ES_all_mean_np$continent))

                polar_rel_es <- ggplot(PFT_ES_all_mean_np,
                 aes(x = continent, y = (ES_totfrac*100), fill= PFT)) + ylim(0,5)+
                 #scale_y_log10()+
                #coord_polar(theta = "x", start = -.13) +
                #scale_fill_manual(values = c(pal_trop(4),pal_temp(3), pal_boreal(4)))+
                scale_fill_brewer(palette = "Paired",direction=1)+
                geom_bar(stat = "identity", width = .9) +theme_bw() +
                theme(
                    panel.grid = element_blank(),
                    #axis.text = element_blank(),
                    #axis.ticks = element_blank(),
                    #panel.border = element_blank(),
                    legend.title = element_blank(),
                    plot.title = element_text( hjust = 0.5 )
                ) + xlab("") +ylab("") + ggtitle("Ecosystem services")+
                guides(fill = guide_legend(reverse=FALSE)) 

                polar_rel_es
                
                PFT_ES_all_mean_p <- PFT_ES_all_mean[PFT_ES_all_mean$TypeES=="Provisioning" & PFT_ES_all_mean$horizon=="Present",]
                
                PFT_ES_all_mean_p$continent <- reorder(PFT_ES_all_mean_p$continent, PFT_ES_all_mean_p$ES_tot, FUN = sum)
                levels(factor(PFT_ES_all_mean_p$continent))

                polar_rel_prov <- ggplot(PFT_ES_all_mean_p,
                 aes(x = continent, y = (ES_totfrac)*100, fill= PFT)) + 
                #geom_text(aes(y = ES_relative + 0.5,label = continent))+
                geom_bar(stat = "identity", width = .9) +ylim(0,5) +
                #coord_polar(theta = "x", start = -.13) +
                theme_bw() +
                scale_fill_brewer(palette = "Paired",direction=1)+
                theme(
                    panel.grid = element_blank(),
                    #axis.text = element_blank(),
                    #axis.ticks = element_blank(),
                    #panel.border = element_blank(),
                    legend.title = element_blank(),
                    plot.title = element_text( hjust = 0.5 )
                ) + xlab("") +ylab("Value of benefits (% of GDP)") + ggtitle("Market goods")+
                guides(fill = guide_legend(reverse=FALSE))

                ggarrange(polar_rel_prov,polar_rel_es,common.legend=TRUE,legend="right")
                ggsave("Barplots_ESvalues.png",dpi=300)
                ggsave("Barplots_ESvalues.pdf",device="pdf",dpi=300)

    
    PFT_ES_all$ES_tot <- PFT_ES_all$ES_total_areachange
    PFT_ES_all$ES_tot[which(PFT_ES_all$TypeES=="Provisioning")] <- PFT_ES_all$ES_total[which(PFT_ES_all$TypeES=="Provisioning")]
    
    glimpse(PFT_ES_all)
    PFT_ES_all_mean <- aggregate(ES_tot~TypeES+PFT+horizon+continent, FUN="mean", data=PFT_ES_all)
    glimpse(PFT_ES_all_mean)

    
    PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~TypeES+PFT+horizon+continent, FUN="mean", data=PFT_ES_all)
    glimpse(PFT_ES_all_mean2)


    PFT_ES_all_mean <-cbind(PFT_ES_all_mean,PFT_ES_all_mean2$areas_change_pct_adjusted)
    names(PFT_ES_all_mean)[length(PFT_ES_all_mean)] <- "areas_change_pct_adjusted"

    glimpse(PFT_ES_all_mean)
    PFT_ES_all_mean$PFT <- factor(PFT_ES_all_mean$PFT)
                PFT_ES_all_mean$PFT_code <- PFT_ES_all_mean$PFT
                levels(PFT_ES_all_mean$PFT_code) <- reorder(levels(PFT_ES_all_mean$PFT_code), c(2,3,1,5,9,4,7,6,11,8,10))
                levels(PFT_ES_all_mean$PFT) <- c("Boreal evergreen \n needleleaved (shade-intolerant)",
                    "Boreal evergreen \n needleleaved",
                    "Boreal summergreen \n needleleaved",
                    "Boreal/Temperate grass",
                    "Tropical Grass",
                    "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
                    "Temperate  broadleaved \n  evergreen",
                    "Temperate broadleaved \n summergreen",
                    "Tropical broadleaved \n  evergreen" ,
                    "Tropical broadleaved \n  raingreen" ,
                    "Tropical broadleaved \n  evergreen (shade-intolerant)")

            ggplot(PFT_ES_all_mean[PFT_ES_all_mean$TypeES!="All",], aes(x = continent, y = log(ES_tot), fill= PFT)) +
                coord_polar(theta = "x", start = -.13) +
                geom_bar(stat = "identity", width = .9) + theme_minimal() + facet_wrap(~TypeES)


    #This is the order by latitude: 
        PFT_ES_all_mean$PFT <- factor(PFT_ES_all_mean$PFT, levels=c("Boreal summergreen \n needleleaved" ,"Boreal evergreen \n needleleaved (shade-intolerant)",
        "Boreal evergreen \n needleleaved" ,"Boreal/Temperate \n broadleaved (shade-intolerant)" , "Boreal/Temperate grass" , 
        "Temperate broadleaved \n summergreen","Temperate  broadleaved \n  evergreen","Tropical broadleaved \n  raingreen" ,
        "Tropical Grass", "Tropical broadleaved \n  evergreen (shade-intolerant)","Tropical broadleaved \n  evergreen"))
    glimpse(PFT_ES_all_mean)
    
    levels(factor(PFT_ES_all$horizon))

    #PFT_ES_all <- PFT_ES_all[which(PFT_ES_all$areas_change_pct<300),]
    #PFT_ES_all <- PFT_ES_all[which(PFT_ES_all$ES_rel<quantile(PFT_ES_all$ES_rel, 0.99)),]
    PFT_ES_all$sign <- 1
    PFT_ES_all$sign[PFT_ES_all$areas_change_pct_adjusted <0] <- -1

library("ggpattern")
library("ggridges")

    PFT_ES_all$ES_relative <- PFT_ES_all$ES_rel_areachange
    PFT_ES_all$ES_relative[which(PFT_ES_all$TypeES=="Provisioning")] <- PFT_ES_all$ES_rel[which(PFT_ES_all$TypeES=="Provisioning")]
    
    glimpse(PFT_ES_all)
    PFT_ES_all_mean <- aggregate(ES_relative~TypeES+PFT+horizon, FUN="mean", data=PFT_ES_all)
    glimpse(PFT_ES_all_mean)

    
    PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~TypeES+PFT+horizon, FUN="mean", data=PFT_ES_all)
    glimpse(PFT_ES_all_mean2)


    PFT_ES_all_mean <-cbind(PFT_ES_all_mean,PFT_ES_all_mean2$areas_change_pct_adjusted)
    names(PFT_ES_all_mean)[length(PFT_ES_all_mean)] <- "areas_change_pct_adjusted"

    glimpse(PFT_ES_all_mean)
    PFT_ES_all_mean$PFT <- factor(PFT_ES_all_mean$PFT)
                PFT_ES_all_mean$PFT_code <- PFT_ES_all_mean$PFT
                levels(PFT_ES_all_mean$PFT_code) <- reorder(levels(PFT_ES_all_mean$PFT_code), c(2,3,1,5,9,4,7,6,11,8,10))
                levels(PFT_ES_all_mean$PFT) <- c("Boreal evergreen \n needleleaved (shade-intolerant)",
                    "Boreal evergreen \n needleleaved",
                    "Boreal summergreen \n needleleaved",
                    "Boreal/Temperate grass",
                    "Tropical Grass",
                    "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
                    "Temperate  broadleaved \n  evergreen",
                    "Temperate broadleaved \n summergreen",
                    "Tropical broadleaved \n  evergreen" ,
                    "Tropical broadleaved \n  raingreen" ,
                    "Tropical broadleaved \n  evergreen (shade-intolerant)")


    #This is the order by latitude: 
        PFT_ES_all_mean$PFT <- factor(PFT_ES_all_mean$PFT, levels=c("Boreal summergreen \n needleleaved" ,"Boreal evergreen \n needleleaved (shade-intolerant)",
        "Boreal evergreen \n needleleaved" ,"Boreal/Temperate \n broadleaved (shade-intolerant)" , "Boreal/Temperate grass" , 
        "Temperate broadleaved \n summergreen","Temperate  broadleaved \n  evergreen","Tropical broadleaved \n  raingreen" ,
        "Tropical Grass", "Tropical broadleaved \n  evergreen (shade-intolerant)","Tropical broadleaved \n  evergreen"))
    glimpse(PFT_ES_all_mean)
    ggplot(PFT_ES_all_mean[PFT_ES_all_mean$TypeES!="All",])+
    geom_point(aes(x=(PFT),size=pmax(pmin(areas_change_pct_adjusted,100),-100),y=ES_relative, color=horizon, shape=TypeES))+
    scale_size_continuous(limits=c(-50,90),breaks=c(-50,0,50,100))+
    scale_y_break(c(45,80))+theme_bw()+ylim(c(5,155)) + labs(size="Area change (%)",color="Time horizon",shape="Ecosystem Service \n Category")+
    xlab("")+ylab("Ecosysem Service Value ($/ha)")+scale_x_discrete(limits = rev(levels(PFT_ES_all_mean$PFT)))+coord_flip()
    #ggsave("ESValue_byPFT.png",dpi=600)
    # RIDGES of ES VALUE CHANGE
        glimpse(PFT_ES_all)
        PFT_all_meanclim <- aggregate(ES_relative~TypeES+PFT+horizon+country, FUN="mean", data=PFT_ES_all[which(PFT_ES_all$Type != "All"),])
        glimpse(PFT_all_meanclim)
        table(PFT_all_meanclim$horizon)

        PFT_all_meanclim <- PFT_all_meanclim[which(PFT_all_meanclim$ES_relative >0),]
        PFT_all_meanclim <- PFT_all_meanclim[which(PFT_all_meanclim$ES_relative  >quantile(PFT_all_meanclim$ES_relative ,0.05)),]
        
        ggplot(PFT_all_meanclim)+
        geom_density_ridges(aes(x=ES_relative,y=PFT,fill=TypeES))+
         scale_x_continuous(trans = 'log2') + theme_bw()
         ggsave("PFT_rel_ridgesbycountry.png",dpi=300)


         PFT_all_meanclim <- aggregate(ES_total~TypeES+PFT+horizon+country, FUN="mean", data=PFT_ES_all[which(PFT_ES_all$Type != "All"),])
        glimpse(PFT_all_meanclim)
        table(PFT_all_meanclim$horizon)

        PFT_all_meanclim <- PFT_all_meanclim[which(PFT_all_meanclim$ES_total >0),]
        PFT_all_meanclim <- PFT_all_meanclim[which(PFT_all_meanclim$ES_total >quantile(PFT_all_meanclim$ES_total,0.05)),]
        
        
        pal_teal <- sequential_hcl(11, palette = "Teal")
        pal_blue <- sequential_hcl(11, palette = "Blue")
        
        ggplot(PFT_all_meanclim[which(PFT_all_meanclim$horizon %in% c("Present","Long-term (2081-2099)")),])+
        geom_density_ridges(aes(x=ES_total,y=PFT,fill=TypeES,linetype=horizon),alpha=0.5)+
         scale_x_continuous(trans = 'log2') + theme_bw()+ #working now

         ggsave("ridges_pfts_country.png",dpi=300)
         

        PFT_all_meanclim <- aggregate(ES_total~TypeES+PFT+horizon+country, FUN="mean", data=PFT_ES_all[which(PFT_ES_all$Type == "Provisioning"),])
        glimpse(PFT_all_meanclim)
        
        PFT_all_meanclim <- PFT_all_meanclim[order(PFT_all_meanclim$horizon),]
        
        PFT_all_meanclim$change <- PFT_all_meanclim$ES_total[which(PFT_all_meanclim$horizon=="Long-term (2081-2099)")] - PFT_all_meanclim$ES_total[which(PFT_all_meanclim$horizon=="Present")]


        ggplot(PFT_all_meanclim[which(PFT_all_meanclim$horizon %in% c("Long-term (2081-2099)")),])+
        geom_density_ridges(aes(x=change,y=PFT,fill=TypeES),alpha=0.5) + theme_bw()+xlim(-1000000,1000000)+geom_vline(xintercept=0,linetype="dashed")

         ggsave("ridges_pfts_country_ESchange.png",dpi=300)
         
        


    #RIDGES OF ES VALUE CHANGE
    # HEX ES RELATIVE Start
                PFT_ES_all_1 <- PFT_ES_all[which(PFT_ES_all$TypeES == "Provisioning" & PFT_ES_all$horizon == "Long-term (2081-2099)" ),]
                
                PFT_ES_all_mean <- aggregate(ES_relative~PFT+country, FUN="mean", data=PFT_ES_all_1)
                glimpse(PFT_ES_all_mean)

                pft_order <- PFT_ES_all_mean %>% 
                        group_by(country) %>% 
                        mutate(es_importance = order(-ES_relative))

                pft_order <- as.data.frame(pft_order)
                glimpse(pft_order)
                pft_order$id <- paste0(pft_order$country,pft_order$PFT)

                PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~PFT+country, FUN="mean", data=PFT_ES_all_1)
                PFT_ES_all_mean2$id <- paste0(PFT_ES_all_mean2$country,PFT_ES_all_mean2$PFT)

                pft_order <- merge(pft_order,PFT_ES_all_mean2[,which(names(PFT_ES_all_mean2) %in% c("id","areas_change_pct_adjusted"))],by="id")
                glimpse(pft_order)
                glimpse(wdata2018o)

                
                PFT_ES_all_mean3 <- aggregate(area~PFT+country, FUN="mean", data=PFT_ES_all_1)          
                PFT_ES_all_1 <- PFT_ES_all[which(PFT_ES_all$TypeES == "Provisioning" & PFT_ES_all$horizon == "Present" ),]
                PFT_ES_all_mean3_p <- aggregate(area~PFT+country, FUN="mean", data=PFT_ES_all_1)

                wdata2018o <- wdata2018o[order(wdata2018o$GDP),]
                wdata2018o$Nprovorder <- seq(1:dim(wdata2018o)[1])
                pft_order$countrycode <- countrycode(pft_order$country, origin="iso2c", destination = "iso3c" )
            b<-  merge(pft_order,wdata2018o[,which(names(wdata2018o) %in% c("countrycode","Nprovorder","Continent"))],by="countrycode",all=FALSE)
                glimpse(b)

                
                pal <- palette(brewer.pal(n = 3, name = "Spectral"))
            
                    hex_plot_prov <-   ggplot(b, aes(x=ES_relative, y=Nprovorder, weight=areas_change_pct_adjusted )) + 
            geom_hex(binwidth = c(20, 1))+
            #geom_tile(aes(fill=pmax(pmin(areas_change_pct_adjusted,100),-100)))+
            scale_fill_gradientn(colours = pal,limits=c(-200,200),
                    na.value="transparent",name="Within-country \narea change",oob = scales::squish)+
                        theme_minimal()  + xlab("Provisioning value ($/ha)")+
                        theme(axis.text.y=element_blank())

                        ggplot(b, aes(x=es_importance, y=Nprovorder, fill=areas_change_pct_adjusted )) + 
                            geom_tile()+
                            #geom_tile(aes(fill=pmax(pmin(areas_change_pct_adjusted,100),-100)))+
                            scale_fill_gradientn(colours = pal,limits=c(-300,300),
                            na.value="transparent",name="Within-country \narea change",oob = scales::squish)
                            #theme_minimal()  + xlab("Provisioning value ($/ha)")+
                            #theme(axis.text.y=element_blank())

                    fixed <- felm(areas_change_pct_adjusted~ es_importance+Continent|country|0|0, data=b)
                    summary(fixed)
                        
                        ggplot(b, aes(x=es_importance, y=areas_change_pct_adjusted )) + 
            geom_point(aes())+facet_wrap(~Continent)+theme_bw()+geom_hline(yintercept=0,linetype="dashed")+
            geom_smooth(method="lm")  +xlab("Relative importance of PFT within country")+ylab("PFT area change (%)")+#ylim(-100,200)+
        coord_cartesian(ylim=c(-100, 100))
        ggsave("AreaChange_by_importance.png",dpi=300)

                   
         ggplot(b, aes(x=log(ES_relative), y=areas_change_pct_adjusted )) + 
            geom_point(aes())+facet_wrap(~Continent)+theme_bw()+geom_hline(yintercept=0,linetype="dashed")+
            geom_smooth(method="lm")  +xlab("Log PFT market value ($/ha)")+ylab("PFT area change (%)")+#ylim(-100,200)+
        coord_cartesian(ylim=c(-100, 100))
        ggsave("AreaChange_by_ES_rel.png",dpi=300)



                        PFT_ES_all_1 <- PFT_ES_all[which(PFT_ES_all$TypeES == "Non-provisioning" & PFT_ES_all$horizon == "Long-term (2081-2099)" ),]
                
                PFT_ES_all_mean <- aggregate(ES_relative~PFT+country, FUN="mean", data=PFT_ES_all_1)
                glimpse(PFT_ES_all_mean)

                pft_order <- PFT_ES_all_mean %>% 
                        group_by(country) %>% 
                        mutate(es_importance = order(ES_relative))

                pft_order <- as.data.frame(pft_order)
                glimpse(pft_order)
                pft_order$id <- paste0(pft_order$country,pft_order$PFT)

                PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~PFT+country, FUN="mean", data=PFT_ES_all_1)
                PFT_ES_all_mean2$id <- paste0(PFT_ES_all_mean2$country,PFT_ES_all_mean2$PFT)

                pft_order <- merge(pft_order,PFT_ES_all_mean2[,which(names(PFT_ES_all_mean2) %in% c("id","areas_change_pct_adjusted"))],by="id")
                glimpse(pft_order)
                glimpse(wdata2018o)



                wdata2018o <- wdata2018o[order(wdata2018o$Nnonprov/wdata2018o$TotalWealth),]
                wdata2018o$NonprovOrder <- seq(1:dim(wdata2018o)[1])

                pft_order$countrycode <- countrycode(pft_order$country, origin="iso2c", destination = "iso3c" )
            c<-  merge(pft_order,wdata2018o[,which(names(wdata2018o) %in% c("countrycode","NonprovOrder","Continent"))],by="countrycode",all=FALSE)

                    hex_plot_nonprov <-   ggplot(c, aes(x=ES_relative, y=NonprovOrder, weight=areas_change_pct_adjusted )) + 
            geom_hex(binwidth = c(50, 1))+
            #geom_tile(aes(fill=pmax(pmin(areas_change_pct_adjusted,100),-100)))+
            scale_fill_gradientn(colours = pal,limits=c(-300,300),
                    na.value="transparent",name="Within-country \narea change",oob = scales::squish)+
                        theme_minimal()  + xlab("Non-provisioning value ($/ha)")+
                        theme(axis.text.y=element_blank())
                        

                        ggarrange(hex_plot_prov,hex_plot_nonprov,common.legend=TRUE,legend="bottom")
                        ggsave("hex_eschange.png",dpi=300)

                ggplot(c, aes(x=log(ES_relative), y=areas_change_pct_adjusted )) + 
            geom_point(aes())+facet_wrap(~Continent)+theme_bw()+geom_hline(yintercept=0,linetype="dashed")+
            geom_smooth(method="lm")  +xlab("Log PFT market value ($/ha)")+ylab("PFT area change (%)")+#ylim(-100,200)+
        coord_cartesian(ylim=c(-100, 100))
        b$type="prov"
        c$type="nonprov"
        d <- rbind(c[,-9],b[,-9])
                ggplot(d, aes(x=log(ES_relative), y=areas_change_pct_adjusted )) + 
                geom_point(aes(color=type))+facet_wrap(~Continent)+theme_bw()+geom_hline(yintercept=0,linetype="dashed")+
                geom_smooth(method="lm", aes(color=type))  +xlab("Log PFT market value ($/ha)")+ylab("PFT area change (%)")+#ylim(-100,200)+
        coord_cartesian(ylim=c(-100, 100))
    # HEX ES RELATIVE END

    # HEX ES RELATIVE Start
        PFT_ES_all_l <- PFT_ES_all[which(PFT_ES_all$TypeES == "Provisioning" & PFT_ES_all$horizon == "Long-term (2081-2099)" ),]
        PFT_ES_all_mean_l <- aggregate(ES_total~PFT+country, FUN="mean", data=PFT_ES_all_l)
        PFT_ES_all_mean_l <- aggregate(ES_total~country, FUN="sum", data=PFT_ES_all_mean_l)
        
        PFT_ES_all_p <- PFT_ES_all[which(PFT_ES_all$TypeES == "Provisioning" & PFT_ES_all$horizon == "Present" ),]        
        PFT_ES_all_mean_p <- aggregate(ES_total~PFT+country, FUN="mean", data=PFT_ES_all_p)     
        PFT_ES_all_mean_p <- aggregate(ES_total~country, FUN="sum", data=PFT_ES_all_mean_p)
        
        glimpse(PFT_ES_all_mean_p)
        glimpse(PFT_ES_all_mean_l)

        PFT_ES_all_mean_l$ES_totalchange <- (PFT_ES_all_mean_l$ES_total - PFT_ES_all_mean_p$ES_total)/PFT_ES_all_mean_p$ES_total

        PFT_ES_all_mean_l$id <- paste0(PFT_ES_all_mean_l$country,PFT_ES_all_mean_l$PFT)

        PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~PFT+country, FUN="mean", data=PFT_ES_all_l)
        PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~country, FUN="sum", data=PFT_ES_all_mean2)
        PFT_ES_all_mean2$id <- paste0(PFT_ES_all_mean2$country,PFT_ES_all_mean2$PFT)

        pft_order <- merge(PFT_ES_all_mean_l,PFT_ES_all_mean2[,which(names(PFT_ES_all_mean2) %in% c("id","areas_change_pct_adjusted"))],by="id")
        glimpse(pft_order)
        glimpse(wdata2018o)

        PFT_ES_all_mean3 <- aggregate(ES_relative~PFT+country, FUN="mean", data=PFT_ES_all_l)
        PFT_ES_all_mean3 <- aggregate(ES_relative~country, FUN="sum", data=PFT_ES_all_mean3)
        PFT_ES_all_mean3$id <- paste0(PFT_ES_all_mean3$country,PFT_ES_all_mean2$PFT)
        pft_order <- merge(pft_order,PFT_ES_all_mean3[,which(names(PFT_ES_all_mean3) %in% c("id","ES_relative"))],by="id")
        


                wdata2018o <- wdata2018o[order(wdata2018o$GDP),]
                wdata2018o$Nprovorder <- seq(1:dim(wdata2018o)[1])
                pft_order$countrycode <- countrycode(pft_order$country, origin="iso2c", destination = "iso3c" )
            b<-  merge(pft_order,wdata2018o[,which(names(wdata2018o) %in% c("countrycode","Nprovorder","Continent","GDP"))],by="countrycode",all=FALSE)
                glimpse(b)

                
                pal <- palette(brewer.pal(n = 3, name = "Spectral"))

                ggplot(b, aes(x=log(GDP), y=areas_change_pct_adjusted,color=Continent )) +
                geom_point()
                        
                        
                        ggplot(b, aes(x=ES_relative, y=ES_totalchange )) + 
            geom_hex(bins=15)+geom_hline(yintercept=0,linetype="dashed")+facet_wrap(~Continent)+theme_bw()+ylim(-1,1)



                        PFT_ES_all_1 <- PFT_ES_all[which(PFT_ES_all$TypeES == "Non-provisioning" & PFT_ES_all$horizon == "Long-term (2081-2099)" ),]
                
                PFT_ES_all_mean <- aggregate(ES_relative~PFT+country, FUN="mean", data=PFT_ES_all_1)
                glimpse(PFT_ES_all_mean)

                pft_order <- PFT_ES_all_mean %>% 
                        group_by(country) %>% 
                        mutate(es_importance = order(ES_relative))

                pft_order <- as.data.frame(pft_order)
                glimpse(pft_order)
                pft_order$id <- paste0(pft_order$country,pft_order$PFT)

                PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~PFT+country, FUN="mean", data=PFT_ES_all_1)
                PFT_ES_all_mean2$id <- paste0(PFT_ES_all_mean2$country,PFT_ES_all_mean2$PFT)

                pft_order <- merge(pft_order,PFT_ES_all_mean2[,which(names(PFT_ES_all_mean2) %in% c("id","areas_change_pct_adjusted"))],by="id")
                glimpse(pft_order)
                glimpse(wdata2018o)



                wdata2018o <- wdata2018o[order(wdata2018o$Nnonprov/wdata2018o$TotalWealth),]
                wdata2018o$NonprovOrder <- seq(1:dim(wdata2018o)[1])

                pft_order$countrycode <- countrycode(pft_order$country, origin="iso2c", destination = "iso3c" )
            c<-  merge(pft_order,wdata2018o[,which(names(wdata2018o) %in% c("countrycode","NonprovOrder"))],by="countrycode",all=FALSE)

                    hex_plot_nonprov <-   ggplot(c, aes(x=ES_relative, y=NonprovOrder, weight=areas_change_pct_adjusted )) + 
            geom_hex(binwidth = c(50, 1))+
            #geom_tile(aes(fill=pmax(pmin(areas_change_pct_adjusted,100),-100)))+
            scale_fill_gradientn(colours = pal,limits=c(-300,300),
                    na.value="transparent",name="Within-country \narea change",oob = scales::squish)+
                        theme_minimal()  + xlab("Non-provisioning value ($/ha)")+
                        theme(axis.text.y=element_blank())
                        

                        ggarrange(hex_plot_prov,hex_plot_nonprov,common.legend=TRUE,legend="bottom")
                        ggsave("hex_eschange.png",dpi=300)
    # HEX ES RELATIVE END

    #PFT ES ALL OTHER FIGURES (start)

                pft_order <- PFT_ES_all_mean %>% 
                        group_by(country) %>% 
                        mutate(es_importance = order(ES_relative))


                


                PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~PFT+country, FUN="mean", data=PFT_ES_all_1)
                glimpse(PFT_ES_all_mean2)

                PFT_ES_all_mean3 <- aggregate(re~TypeES+PFT+horizon+country, FUN="mean", data=PFT_ES_all)
                glimpse(PFT_ES_all_mean3)


                glimpse(PFT_ES_all)
                PFT_ES_all$PFT <- factor(PFT_ES_all$PFT)
                            PFT_ES_all$PFT_code <- PFT_ES_all$PFT
                            levels(PFT_ES_all$PFT_code) <- reorder(levels(PFT_ES_all$PFT_code), c(2,3,1,5,9,4,7,6,11,8,10))
                            levels(PFT_ES_all$PFT) <- c("Boreal evergreen \n needleleaved (shade-intolerant)",
                                "Boreal evergreen \n needleleaved",
                                "Boreal summergreen \n needleleaved",
                                "Boreal/Temperate grass",
                                "Tropical Grass",
                                "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
                                "Temperate  broadleaved \n  evergreen",
                                "Temperate broadleaved \n summergreen",
                                "Tropical broadleaved \n  evergreen" ,
                                "Tropical broadleaved \n  raingreen" ,
                                "Tropical broadleaved \n  evergreen (shade-intolerant)")
            PFT_ES_all$PFT <- factor(PFT_ES_all$PFT, levels=c("Boreal summergreen \n needleleaved" ,"Boreal evergreen \n needleleaved (shade-intolerant)",
                    "Boreal evergreen \n needleleaved" ,"Boreal/Temperate \n broadleaved (shade-intolerant)" , "Boreal/Temperate grass" , 
                    "Temperate broadleaved \n summergreen","Temperate  broadleaved \n  evergreen","Tropical broadleaved \n  raingreen" ,
                    "Tropical Grass", "Tropical broadleaved \n  evergreen (shade-intolerant)","Tropical broadleaved \n  evergreen"))

            ggplot(data = PFT_ES_all[which(PFT_ES_all$TypeES!="All"),], aes(y=PFT,x=ES_relative))+
                #geom_violin(data = PFT_ES_all[which(PFT_ES_all$TypeES=="Provisioning"),],aes(fill=horizon)) + 
                geom_boxplot(data = PFT_ES_all[which(PFT_ES_all$TypeES=="Non-provisioning"),],aes(y=PFT,x=ES_relative,fill=horizon)) +
                xlim(40,200)
                
                glimpse(NC_dam)

                #Experimental figure (start)
                    glimpse(country_y)

                    glimpse(PFT_ES_all)
                    
                    PFT_ES_all_y <- merge(PFT_ES_all,country_y,by=c("country"))
                    min(PFT_ES_all_y$ES_relative)
                    min(PFT_ES_all_y$area)
                    
                    PFT_ES_all_y <-  PFT_ES_all_y [,which(names(PFT_ES_all_y) %notin% c("clim"))]
                    PFT_ES_all_y  <- aggregate(.~TypeES+country+horizon+PFT_code+PFT, FUN="mean", data=PFT_ES_all_y ,na.rm=TRUE, na.action=NULL)
                    
                    install.packages("ks")
                    library("ks")
                    PFT_ES_all_y$ID <- seq(1:dim(PFT_ES_all_y)[1])
                    
                    PFT_ES_all_y$absy <- abs(PFT_ES_all_y$y)
                    pft_codes <- levels(factor(PFT_ES_all_y$PFT_code))
                    horizons <- c("Present","Long-term (2081-2099)")
                    esi <- c("Provisioning","Non-provisioning")
                    pft_codes <- levels(factor(PFTs11_horizons3$PFT))

                    for (i in 1:length(pft_codes)){
                        for (j in 1:length(horizons)){
                            for (e in 1:2){

                                d <- PFT_ES_all_y[which(PFT_ES_all_y$PFT_code == pft_codes[i] & PFT_ES_all_y$horizon==horizons[j]  & PFT_ES_all_y$TypeES==esi[e] ),] #Long-term (2081-2099)
                                d <- d[which(d$area > quantile(d$area, 0.1)),]

                                d <- d[,which(names(d) %in% c("area","absy"))]
                                kd <- ks::kde(d, compute.cont=TRUE)
                                contour_50 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                            z=estimate, levels=cont["95%"])[[1]])
                                contour_50 <- data.frame(contour_50)
                                contour_50$horizon <- horizons[j]
                                contour_50$PFT_code <- pft_codes[i]
                                contour_50$TypeES <- esi[e]
                                if(i==1 & j==1 & e==1){
                                    contour_all <- contour_50
                                }else{
                                    contour_all <- rbind(contour_all,contour_50)
                                }
                            }
                        }
                    }

                    PFTs11_horizons3$y <- abs(PFTs11_horizons3$y)
                    min(PFTs11_horizons3$y)
                    
                    for (i in 1:length(pft_codes)){
                        for (j in 1:length(horizons)){

                                d <- PFTs11_horizons3[which(PFTs11_horizons3$PFT == pft_codes[i] & PFTs11_horizons3$horizon==horizons[j]  ),] #Long-term (2081-2099)
                                d <- d[which(d$areaPFT > quantile(d$areaPFT, 0.1)),]

                                d <- d[,which(names(d) %in% c("areaPFT","y"))]
                                kd <- ks::kde(d, compute.cont=TRUE)
                                contour_50 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                            z=estimate, levels=cont["90%"])[[1]])
                                contour_50 <- data.frame(contour_50)
                                contour_50$horizon <- horizons[j]
                                contour_50$PFT_code <- pft_codes[i]
                                if(i==1 & j==1 ){
                                    contour_all <- contour_50
                                }else{
                                    contour_all <- rbind(contour_all,contour_50)
                                }
                            
                        }
                    }
                    
                    
                    ggplot(data=contour_all, aes(group=interaction(PFT_code,horizon))) +
                    geom_path(aes(y,x, color=horizon, linetype=horizon)) +
                    theme_bw()+xlab("Area")+ylab("Latitude (N | S)")+
                    labs(color="Horizon",linetype="Horizon")+
                    ggtitle("Density contour (inner 10%)")

                    ggsave("LatitudeChange_AreaChange.png",dpi=300)
                    
                load("PFTs11_horizons3.Rda")
                glimpse(PFTs11_horizons3)
                        for (yi in 1:2){
                        for (i in 1:length(pft_codes)){
                            for (j in 1:length(horizons)){
                                    
                                    d <- PFTs11_horizons3[which(PFTs11_horizons3$PFT == pft_codes[i] & PFTs11_horizons3$horizon==horizons[j]  ),] #Long-term (2081-2099)
                                    d <- d[which(d$areaPFT > quantile(d$areaPFT, 0.1)),]

                                    d <- d[,which(names(d) %in% c("areaPFT","y"))]
                                    if(yi==1){
                                        d <- d[which(d$y>0),]
                                    } else{
                                        d <- d[which(d$y<0),]
                                    }
                                    if(dim(d)[1]==0){next}
                                    kd <- ks::kde(d, compute.cont=TRUE)
                                    contour_50 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                                z=estimate, levels=cont["90%"])[[1]])
                                    contour_50 <- data.frame(contour_50)
                                    contour_50$horizon <- horizons[j]
                                    contour_50$PFT_code <- pft_codes[i]
                                    contour_50$lat <- yi
                                    if(i==1 & j==1 & yi==1){
                                        contour_all <- contour_50
                                    }else{
                                        contour_all <- rbind(contour_all,contour_50)
                                    }
                                
                            }
                        }}
                        
                        
                        ggplot(data=contour_all, aes(group=interaction(PFT_code,horizon,lat))) +
                        geom_path(aes(y,x, color=horizon, linetype=horizon)) +
                        theme_bw()+xlab("Area")+ylab("Latitude (N | S)")+
                        labs(color="Horizon",linetype="Horizon")+
                        ggtitle("Density contour (inner 10%)")


                    
                load("PFTs11_horizons3.Rda")
                glimpse(PFTs11_horizons3)
                    for (yi in 1:2){
                    for (i in 1:length(pft_codes)){
                        for (j in 1:length(horizons)){
                                
                                d <- PFTs11_horizons3_perc[which(PFTs11_horizons3_perc$PFT == pft_codes[i] & PFTs11_horizons3_perc$horizon==horizons[j]  ),] #Long-term (2081-2099)
                                d <- d[which(d$Percent > quantile(d$Percent, 0.1)),]

                                d <- d[,which(names(d) %in% c("Percent","y"))]
                                if(yi==1){
                                    d <- d[which(d$y>0),]
                                } else{
                                    d <- d[which(d$y<0),]
                                }
                                if(dim(d)[1]==0){next}
                                kd <- ks::kde(d, compute.cont=TRUE)
                                contour_50 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                            z=estimate, levels=cont["90%"])[[1]])
                                contour_50 <- data.frame(contour_50)
                                contour_50$horizon <- horizons[j]
                                contour_50$PFT_code <- pft_codes[i]
                                contour_50$lat <- yi
                                if(i==1 & j==1 & yi==1){
                                    contour_all <- contour_50
                                }else{
                                    contour_all <- rbind(contour_all,contour_50)
                                }
                            
                        }
                    }}
                    
                    
                    ggplot(data=contour_all, aes(group=interaction(PFT_code,horizon,lat))) +
                    geom_path(aes(y,x, color=horizon, linetype=horizon)) +
                    theme_bw()+xlab("Percent covered")+ylab("Latitude (N | S)")+
                    labs(color="Horizon",linetype="Horizon")+
                    ggtitle("Density contour (inner 10%)")

                PFTs11_horizons3_perc$y <- abs(PFTs11_horizons3_perc$y)
                    for (i in 1:length(pft_codes)){
                        for (j in 1:length(horizons)){
                                
                                d <- PFTs11_horizons3_perc[which(PFTs11_horizons3_perc$PFT == pft_codes[i] & PFTs11_horizons3_perc$horizon==horizons[j]  ),] #Long-term (2081-2099)
                                d <- d[which(d$Percent > quantile(d$Percent, 0.1)),]

                                d <- d[,which(names(d) %in% c("Percent","y"))]
                                
                                if(dim(d)[1]==0){next}
                                kd <- ks::kde(d, compute.cont=TRUE)
                                contour_50 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                            z=estimate, levels=cont["90%"])[[1]])
                                contour_50 <- data.frame(contour_50)
                                contour_50$horizon <- horizons[j]
                                contour_50$PFT_code <- pft_codes[i]
                                if(i==1 & j==1 ){
                                    contour_all <- contour_50
                                }else{
                                    contour_all <- rbind(contour_all,contour_50)
                                }
                            
                        }
                    }
                    
                    
                    ggplot(data=contour_all, aes(group=interaction(PFT_code,horizon))) +
                    geom_path(aes(y,x, color=horizon, linetype=horizon)) +
                    theme_bw()+xlab("Percent covered")+ylab("Latitude (N | S)")+
                    labs(color="Horizon",linetype="Horizon")+
                    ggtitle("Density contour (inner 10%)")



                    ggsave("LatitudeChange_AreaChange.png",dpi=300)
                    


                    ggplot(data=contour_all[which(contour_all$TypeES=="Non-provisioning"),], aes(group=interaction(PFT_code,horizon,TypeES))) +
                    geom_path(aes(x,y, color=horizon)) +
                    theme_bw()

                    ggplot(data=contour_all[which(contour_all$TypeES=="Provisioning"),], aes(group=interaction(PFT_code,horizon,TypeES))) +
                    geom_path(aes(x,y, color=horizon,linetype=TypeES)) +
                    theme_bw()



                    d <- PFT_ES_all_y[which(PFT_ES_all_y$PFT_code == "PFT.trbe" & PFT_ES_all_y$horizon=="Long-term (2081-2099)" & PFT_ES_all_y$clim == "hadgem2-es" ),]
                    d <- d[which(d$area > quantile(d$area, 0.1)),]
                    glimpse(d)
                    
                    d <- d[,which(names(d) %in% c("ES_relative","y"))]
                    kd <- ks::kde(d, compute.cont=TRUE)
                    glimpse(d)
                    glimpse(kd)
                    contour_95_long <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                z=estimate, levels=cont["50%"])[[1]])
                    contour_95_long <- data.frame(contour_95_long)
                    glimpse(contour_95)
                    
                    d50_present +
                    geom_path(aes(x,y), data=contour_95_long, color="blue") +
                    theme_bw()
                    
                    
                    
                    
                    PFT_ES_all_mean_y <- aggregate(ES_relative~TypeES+PFT+horizon+y, FUN="mean", data=PFT_ES_all_y)
                    glimpse(PFT_ES_all_mean_y)

                    
                    PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~TypeES+PFT+horizon, FUN="mean", data=PFT_ES_all)
                    # glimpse(PFT_ES_all_mean2)
                        
                    PFT_ES_all_mean_y <-cbind(PFT_ES_all_mean_y,PFT_ES_all_mean2$areas_change_pct_adjusted)
                    names(PFT_ES_all_mean_y)[length(PFT_ES_all_mean_y)] <- "areas_change_pct_adjusted"

                    # glimpse(PFT_ES_all_mean)
                    PFT_ES_all_mean_y$PFT <- factor(PFT_ES_all_mean_y$PFT)
                                PFT_ES_all_mean_y$PFT_code <- PFT_ES_all_mean_y$PFT

                    PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~TypeES+PFT+horizon, FUN="length", data=PFT_ES_all)
                    PFT_ES_all_mean_y <-cbind(PFT_ES_all_mean_y,PFT_ES_all_mean2$areas_change_pct_adjusted)
                    names(PFT_ES_all_mean_y)[length(PFT_ES_all_mean_y)] <- "n"

                                levels(PFT_ES_all_mean_y$PFT_code) <- reorder(levels(PFT_ES_all_mean_y$PFT_code), c(2,3,1,5,9,4,7,6,11,8,10))
                                levels(PFT_ES_all_mean_y$PFT) <- c("Boreal evergreen \n needleleaved (shade-intolerant)",
                                    "Boreal evergreen \n needleleaved",
                                    "Boreal summergreen \n needleleaved",
                                    "Boreal/Temperate grass",
                                    "Tropical Grass",
                                    "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
                                    "Temperate  broadleaved \n  evergreen",
                                    "Temperate broadleaved \n summergreen",
                                    "Tropical broadleaved \n  evergreen" ,
                                    "Tropical broadleaved \n  raingreen" ,
                                    "Tropical broadleaved \n  evergreen (shade-intolerant)")


                    # #This is the order by latitude: 
                        #  PFT_ES_all_mean_y$PFT <- factor(PFT_ES_all_mean$PFT, levels=c("Boreal summergreen \n needleleaved" ,"Boreal evergreen \n needleleaved (shade-intolerant)",
                        #  "Boreal evergreen \n needleleaved" ,"Boreal/Temperate \n broadleaved (shade-intolerant)" , "Boreal/Temperate grass" , 
                        #  "Temperate broadleaved \n summergreen","Temperate  broadleaved \n  evergreen","Tropical broadleaved \n  raingreen" ,
                        #  "Tropical Grass", "Tropical broadleaved \n  evergreen (shade-intolerant)","Tropical broadleaved \n  evergreen"))

                        PFT_ES_all_mean_y$BroadPFT <- PFT_ES_all_mean_y$PFT
                        levels(PFT_ES_all_mean_y$BroadPFT) <- c("Boreal",
                                    "Boreal",
                                    "Boreal",
                                    "Temperate",
                                    "Tropical",
                                    "Temperate" ,
                                    "Temperate",
                                    "Temperate",
                                    "Tropical" ,
                                    "Tropical" ,
                                    "Tropical")

                        PFT_ES_all_mean_y$LabelPFT <- PFT_ES_all_mean_y$PFT
                        levels(PFT_ES_all_mean_y$LabelPFT) <- c("eg-nl-si",
                                    "eg-nl",
                                    "sg-nl",
                                    "grass",
                                    "grass",
                                    "bl-si" ,
                                    "eg-bl",
                                    "sg-bl",
                                    "eg-bl" ,
                                    "rg-bl" ,
                                    "eg-bl-si")

                    #PFT_ES_all_y <- merge(PFT_ES_all_mean,country_y,by="country")
                    glimpse(PFT_ES_all_mean_y)
                    #PFT_ES_all_mean_y <- PFT_ES_all_mean_y[-which(PFT_ES_all_mean_y$ES_relative>quantile(PFT_ES_all_mean_y$ES_relative,0.9)),]
                    #PFT_ES_all_mean_y <- PFT_ES_all_mean_y[-which(PFT_ES_all_mean_y$ES_relative<quantile(PFT_ES_all_mean_y$ES_relative,0.01)),]
                    
                    
                    pal_teal <- sequential_hcl(11, palette = "Teal")
                    
                    ggplot(PFT_ES_all_mean_y[PFT_ES_all_mean_y$TypeES!="All" ,], aes(x=ES_relative,y=abs(y), group=interaction(PFT)))+
                    #geom_density_2d_filled(contour_var = "ndensity",aes(fill=PFT,alpha=..level..,group=TypeES),bins=3,size=1)+
                #scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
                geom_point(aes(color=BroadPFT,shape=TypeES,size=areas_change_pct_adjusted,alpha=horizon))+
                scale_size_continuous(limits=c(-50,80),breaks=c(-50,0,50,100))+
                geom_text(data=PFT_ES_all_mean_y[PFT_ES_all_mean_y$TypeES=="Non-provisioning"&
                PFT_ES_all_mean_y$horizon=="Present" & abs(PFT_ES_all_mean_y$y) <30,],
                aes(label=LabelPFT,color=BroadPFT,x=110),nudge_y=0.5)+
                geom_text(data=PFT_ES_all_mean_y[PFT_ES_all_mean_y$TypeES=="Non-provisioning"&
                PFT_ES_all_mean_y$horizon=="Present" & abs(PFT_ES_all_mean_y$y) >30,],
                aes(label=LabelPFT,color=BroadPFT,x=100),nudge_y=0.5)+
                scale_x_break(c(40,80))+theme_bw()+#ylim(c(5,155)) + 
                labs(size="Area change (%)",color="",shape="Ecosystem Service \n Category")+
                ylab("Latitude")+xlab("Ecosysem Service Value ($/ha)")+
                scale_color_manual(values=pal_teal,guide = guide_none())
                #geom_smooth(aes(group=TypeES,color=TypeES,weight=PFT_ES_all_mean_y$n[PFT_ES_all_mean_y$TypeES!="All"]), formula = y~x, method="lm") #weigth on count of obs

                #     ?geom_smooth        

                #     ?geom_text
                #     geom_density_2d_filled(contour_var = "ndensity",bins=3,size=1,alpha=0., aes(group=PFT,subgroup=horizon))
                #     #geom_density_2d(contour_var = "count",bins=2,size=1,aes(color=PFT,linetype=horizon))

                #     scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) 

                #     ?geom_filled
                #     ?ggplot2
                #     ?geom_density_2d_filled    
                #     ?ndensity

                #     geom_point(aes(x=(PFT),size=pmax(pmin(areas_change_pct_adjusted,100),-100),y=ES_relative, color=horizon, shape=TypeES))+
                #     scale_size_continuous(limits=c(-50,90),breaks=c(-50,0,50,100))+
                #     scale_y_break(c(45,80))+theme_bw()+ylim(c(5,155)) + labs(size="Area change (%)",color="Time horizon",shape="Ecosystem Service \n Category")+
                #     xlab("")+ylab("Ecosysem Service Value ($/ha)")+scale_x_discrete(limits = rev(levels(PFT_ES_all_mean$PFT)))+coord_flip()
                    
                #Experimental Figure (end)

                #Experimental figure (start)
                    glimpse(PFT_ES_all_mean)
                    glimpse(pft_y)
                    glimpse(PFT_ES_all)
                    
                    pft_y$PFT_code <- paste0("PFT.",pft_y$dominant_pft)
                    names(pft_y)[3] <- "y"
                    
                    pft_y$PFT <- paste0("PFT.",pft_y$dominant_pft)
                    
                    PFT_ES_all_y <- merge(PFT_ES_all,pft_y,by=c("horizon","PFT"))
                    
                    glimpse(PFT_ES_all_y)
                    PFT_ES_all_mean_y <- aggregate(ES_relative~TypeES+PFT+horizon+y, FUN="mean", data=PFT_ES_all_y)
                    glimpse(PFT_ES_all_mean_y)

                    
                    PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~TypeES+PFT+horizon, FUN="mean", data=PFT_ES_all)
                    # glimpse(PFT_ES_all_mean2)
                        
                    PFT_ES_all_mean_y <-cbind(PFT_ES_all_mean_y,PFT_ES_all_mean2$areas_change_pct_adjusted)
                    names(PFT_ES_all_mean_y)[length(PFT_ES_all_mean_y)] <- "areas_change_pct_adjusted"

                    # glimpse(PFT_ES_all_mean)
                    PFT_ES_all_mean_y$PFT <- factor(PFT_ES_all_mean_y$PFT)
                                PFT_ES_all_mean_y$PFT_code <- PFT_ES_all_mean_y$PFT

                    PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~TypeES+PFT+horizon, FUN="length", data=PFT_ES_all)
                    PFT_ES_all_mean_y <-cbind(PFT_ES_all_mean_y,PFT_ES_all_mean2$areas_change_pct_adjusted)
                    names(PFT_ES_all_mean_y)[length(PFT_ES_all_mean_y)] <- "n"

                                levels(PFT_ES_all_mean_y$PFT_code) <- reorder(levels(PFT_ES_all_mean_y$PFT_code), c(2,3,1,5,9,4,7,6,11,8,10))
                                levels(PFT_ES_all_mean_y$PFT) <- c("Boreal evergreen \n needleleaved (shade-intolerant)",
                                    "Boreal evergreen \n needleleaved",
                                    "Boreal summergreen \n needleleaved",
                                    "Boreal/Temperate grass",
                                    "Tropical Grass",
                                    "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
                                    "Temperate  broadleaved \n  evergreen",
                                    "Temperate broadleaved \n summergreen",
                                    "Tropical broadleaved \n  evergreen" ,
                                    "Tropical broadleaved \n  raingreen" ,
                                    "Tropical broadleaved \n  evergreen (shade-intolerant)")


                    # #This is the order by latitude: 
                        #  PFT_ES_all_mean_y$PFT <- factor(PFT_ES_all_mean$PFT, levels=c("Boreal summergreen \n needleleaved" ,"Boreal evergreen \n needleleaved (shade-intolerant)",
                        #  "Boreal evergreen \n needleleaved" ,"Boreal/Temperate \n broadleaved (shade-intolerant)" , "Boreal/Temperate grass" , 
                        #  "Temperate broadleaved \n summergreen","Temperate  broadleaved \n  evergreen","Tropical broadleaved \n  raingreen" ,
                        #  "Tropical Grass", "Tropical broadleaved \n  evergreen (shade-intolerant)","Tropical broadleaved \n  evergreen"))

                        PFT_ES_all_mean_y$BroadPFT <- PFT_ES_all_mean_y$PFT
                        levels(PFT_ES_all_mean_y$BroadPFT) <- c("Boreal",
                                    "Boreal",
                                    "Boreal",
                                    "Temperate",
                                    "Tropical",
                                    "Temperate" ,
                                    "Temperate",
                                    "Temperate",
                                    "Tropical" ,
                                    "Tropical" ,
                                    "Tropical")

                        PFT_ES_all_mean_y$LabelPFT <- PFT_ES_all_mean_y$PFT
                        levels(PFT_ES_all_mean_y$LabelPFT) <- c("eg-nl-si",
                                    "eg-nl",
                                    "sg-nl",
                                    "grass",
                                    "grass",
                                    "bl-si" ,
                                    "eg-bl",
                                    "sg-bl",
                                    "eg-bl" ,
                                    "rg-bl" ,
                                    "eg-bl-si")

                    #PFT_ES_all_y <- merge(PFT_ES_all_mean,country_y,by="country")
                    glimpse(PFT_ES_all_mean_y)
                    #PFT_ES_all_mean_y <- PFT_ES_all_mean_y[-which(PFT_ES_all_mean_y$ES_relative>quantile(PFT_ES_all_mean_y$ES_relative,0.9)),]
                    #PFT_ES_all_mean_y <- PFT_ES_all_mean_y[-which(PFT_ES_all_mean_y$ES_relative<quantile(PFT_ES_all_mean_y$ES_relative,0.01)),]
                    
                    
                    pal_teal <- sequential_hcl(11, palette = "Teal")
                    
                    ggplot(PFT_ES_all_mean_y[PFT_ES_all_mean_y$TypeES!="All" ,], aes(x=ES_relative,y=abs(y), group=interaction(PFT)))+
                    #geom_density_2d_filled(contour_var = "ndensity",aes(fill=PFT,alpha=..level..,group=TypeES),bins=3,size=1)+
                #scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
                geom_point(aes(color=BroadPFT,shape=TypeES,size=areas_change_pct_adjusted,alpha=horizon))+
                scale_size_continuous(limits=c(-50,80),breaks=c(-50,0,50,100))+
                geom_text(data=PFT_ES_all_mean_y[PFT_ES_all_mean_y$TypeES=="Non-provisioning"&
                PFT_ES_all_mean_y$horizon=="Present" & abs(PFT_ES_all_mean_y$y) <30,],
                aes(label=LabelPFT,color=BroadPFT,x=110),nudge_y=0.5)+
                geom_text(data=PFT_ES_all_mean_y[PFT_ES_all_mean_y$TypeES=="Non-provisioning"&
                PFT_ES_all_mean_y$horizon=="Present" & abs(PFT_ES_all_mean_y$y) >30,],
                aes(label=LabelPFT,color=BroadPFT,x=100),nudge_y=0.5)+
                scale_x_break(c(40,80))+theme_bw()+#ylim(c(5,155)) + 
                labs(size="Area change (%)",color="",shape="Ecosystem Service \n Category")+
                ylab("Latitude")+xlab("Ecosysem Service Value ($/ha)")+
                scale_color_manual(values=pal_teal,guide = guide_none())
                #geom_smooth(aes(group=TypeES,color=TypeES,weight=PFT_ES_all_mean_y$n[PFT_ES_all_mean_y$TypeES!="All"]), formula = y~x, method="lm") #weigth on count of obs

                    ?geom_smooth        

                    ?geom_text
                    geom_density_2d_filled(contour_var = "ndensity",bins=3,size=1,alpha=0., aes(group=PFT,subgroup=horizon))
                    #geom_density_2d(contour_var = "count",bins=2,size=1,aes(color=PFT,linetype=horizon))

                    scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) 

                    ?geom_filled
                    ?ggplot2
                    ?geom_density_2d_filled    
                    ?ndensity

                    geom_point(aes(x=(PFT),size=pmax(pmin(areas_change_pct_adjusted,100),-100),y=ES_relative, color=horizon, shape=TypeES))+
                    scale_size_continuous(limits=c(-50,90),breaks=c(-50,0,50,100))+
                    scale_y_break(c(45,80))+theme_bw()+ylim(c(5,155)) + labs(size="Area change (%)",color="Time horizon",shape="Ecosystem Service \n Category")+
                    xlab("")+ylab("Ecosysem Service Value ($/ha)")+scale_x_discrete(limits = rev(levels(PFT_ES_all_mean$PFT)))+coord_flip()
                    
                #Experimental Figure (end)
    #PFT ES ALL OTHER FIGURES (end)
    
    #NAT CAP Figures (start)
    glimpse( NC_change)
    #hist( NC_change$DamageEstimateT2_areachange)
    NC_change$DamCoeffT2 <- NC_change$DamageEstimateT2_areachange
    NC_change$DamCoeffT2[which(NC_change$TypeES=="Provisioning")] <- NC_change$DamageEstimateT2[which(NC_change$TypeES=="Provisioning")]
    
    glimpse(NC_change)
    NC_dam <- NC_change[,which(names(NC_change) %notin% c("clim"))]
    NC_dam <- aggregate(.~TypeES+country+horizon, FUN="mean", data=NC_dam,na.rm=TRUE, na.action=NULL)
    NC_damse <- NC_change[,which(names(NC_change) %notin% c("DamageEstimateT2_areachange","clim"))]
    NC_damse <- aggregate(.~TypeES+country+horizon, FUN="sd", data=NC_damse,na.rm=TRUE, na.action=NULL)

    glimpse(NC_dam) 
    glimpse(NC_damse)   
    names(NC_damse) <- paste0(names(NC_damse),"_se")
    NC_dam <- cbind(NC_dam, NC_damse)


    NC_dam$fct_dam <- NC_dam$Damage_areachange
    NC_dam$fct_dam[which(NC_dam$TypeES=="Provisioning")] <- NC_dam$Damage[which(NC_dam$TypeES=="Provisioning")]
    NC_dam$fct_damse <- NC_dam$Damage_areachange_se
    
    NC_dam$fct_damse[which(NC_dam$TypeES=="Provisioning")] <- NC_dam$Damage_se[which(NC_dam$TypeES=="Provisioning")]
    

    NC_dam$horizon <- factor(NC_dam$horizon, levels=c("Present","Near-term (2021-2040)", "Mid-term (2041-2060)","Long-term (2081-2099)"))
    
    

    ggplot(data=NC_dam[which(NC_dam$TypeES!="All" & NC_dam$horizon=="Long-term (2081-2099)"),], aes(group=TypeES))+
    geom_pointrange(aes(y=(100*fct_dam*N/TotalWealth),x=N/TotalWealth,ymin=(fct_dam-fct_damse)*100*N/TotalWealth,ymax=(fct_dam+fct_damse)*N*100/TotalWealth,shape=TypeES, color=TypeES),alpha=0.25,size=0.5)+
    geom_hline(yintercept=0,linetype="dashed")+
    theme_bw()+ylab("Damages on Natural Capital \n(% of Inclusive Wealth)") + xlab("Share of Natural Capital")+
    labs(col="Ecosystem Service \nCategory", shape="Ecosystem Service \nCategory") +
    ggtitle("Natural Capital damages at 3.5C above pre-industrial")+
    scale_colour_hue(direction=-1)+
    scale_x_continuous(trans="log")+
    coord_cartesian(ylim=c(-1,1))+
    geom_smooth(formula= y ~x ,method="loess",
        aes(y=(100*fct_dam*N/TotalWealth), x =N/TotalWealth, color=TypeES, weight=1/NC_dam$fct_damse[which(NC_dam$TypeES!="All"& NC_dam$horizon=="Long-term (2081-2099)")]))
    #ggsave("Damages_35C_NC_loess.png",dpi=300)




    # ggplot(data=NC_dam[which(NC_dam$TypeES!="All" & NC_dam$horizon=="Long-term (2081-2099)"),], aes(group=TypeES))+
    # geom_linerange(aes(y=fct_dam,x=log(GDP/Population),ymin=fct_dam-fct_damse,ymax=fct_dam+fct_damse,shape=TypeES, color=TypeES))+
    # theme_bw()+ylab("Damages on Natural Capital") + xlab("Log GDP per capita")+
    # labs(size="Natural Capital \n(% of Inclusive Wealth)",col="Ecosystem Service \nCategory",shape="Ecosystem Service \nCategory") +
    # scale_colour_hue(direction=-1)+
    # geom_smooth(formula= y ~ poly(x, 2),method="lm",aes(y=fct_dam, x =log(GDP/Population), color=TypeES, weight=1/NC_dam$fct_damse[which(NC_dam$TypeES!="All"& NC_dam$horizon=="Long-term (2081-2099)")]))+
    # ylim(c(-0.2,0.2))
    
    # ggplot(data=NC_dam[which(NC_dam$TypeES!="All" & NC_dam$horizon=="Long-term (2081-2099)"),], aes(group=TypeES))+
    # geom_linerange(aes(y=fct_dam,x=log(N/TotalWealth),ymin=fct_dam-fct_damse,ymax=fct_dam+fct_damse,shape=TypeES, color=TypeES),alpha=0.5)+
    # theme_bw()+ylab("Damages on Natural Capital (%)") + xlab("Log share of Natural Capital")+
    # labs(col="Ecosystem Service \nCategory") +
    # scale_colour_hue(direction=-1)+
    # geom_smooth(formula= y ~ poly(x, 2),method="lm",aes(y=fct_dam, x =log(N/TotalWealth), color=TypeES, weight=1/NC_dam$fct_damse[which(NC_dam$TypeES!="All"& NC_dam$horizon=="Long-term (2081-2099)")]))+
    # ylim(c(-0.5,0.5))
    # ggsave("Figures/DamagesNC_LogshareNC.png",dpi=300)


    # ggplot(data=NC_dam[which(NC_dam$TypeES!="All" & NC_dam$horizon=="Long-term (2081-2099)"),], aes(group=TypeES))+
    # geom_linerange(aes(y=fct_dam,x=N/TotalWealth,ymin=fct_dam-fct_damse,ymax=fct_dam+fct_damse,shape=TypeES, color=TypeES),alpha=0.5)+
    # theme_bw()+ylab("Damages on Natural Capital (%)") + xlab("Log share of Natural Capital")+
    # labs(col="Ecosystem Service \nCategory") +
    # scale_colour_hue(direction=-1)+
    # geom_smooth(formula= y ~ poly(x, 2),method="lm",aes(y=fct_dam, x =N/TotalWealth, color=TypeES, weight=1/NC_dam$fct_damse[which(NC_dam$TypeES!="All"& NC_dam$horizon=="Long-term (2081-2099)")]))


    # ggplot(data=NC_dam[which(NC_dam$TypeES!="All" & NC_dam$horizon=="Long-term (2081-2099)"),], aes(group=TypeES))+
    # geom_linerange(aes(y=fct_dam,x=log(N/TotalWealth),ymin=fct_dam-fct_damse,ymax=fct_dam+fct_damse,shape=TypeES, color=TypeES),alpha=0.5)+
    # theme_bw()+ylab("Damages on Natural Capital (%)") + xlab("Log share of Natural Capital")+
    # labs(col="Ecosystem Service \nCategory") +
    # scale_colour_hue(direction=-1)+
    # geom_smooth(formula= y ~ x,method="loess",aes(y=fct_dam, x =log(N/TotalWealth), color=TypeES, weight=1/NC_dam$fct_damse[which(NC_dam$TypeES!="All"& NC_dam$horizon=="Long-term (2081-2099)")]))+
    # ylim(c(-0.5,0.5))
    
    # ggplot(data=NC_dam[which(NC_dam$TypeES!="All"),])+
    # geom_point(aes(x=fct_dam*N/TotalWealth,y=log(GDP/Population),shape=TypeES, color=TypeES,size=100*N/TotalWealth))+
    # facet_wrap(~horizon)+theme_bw()+xlab("Damages on Natural Capital") + ylab("Log GDP per capita")+
    # labs(size="Natural Capital \n(% of Inclusive Wealth)",col="Ecosystem Service \nCategory",shape="Ecosystem Service \nCategory") +
    # scale_colour_hue(direction=-1)
    # #ggsave("NC_Damage_horizon.png",dpi=300)
    # #geom_smooth(method="lm",aes(y=fct_dam,x=log(GDP/Population), group=TypeES))
    glimpse(NC_dam)
    damcoef <- NC_dam[which(NC_dam$horizon=="Present"),which(names(NC_dam) %in% c("country","DamCoeffT2","TypeES"))]
    glimpse(damcoef)
    world <- spData::world
    glimpse(world)
    names(damcoef)[2] <- "iso_a2"
    world <- merge(world,damcoef,by="iso_a2",all=TRUE)
    glimpse(world)
    
    pal <- palette(brewer.pal(n = 3, name = "Spectral"))
    
    world2 <- world[which(!is.na(world$DamCoeffT2)),]
    glimpse(world2)
    ggplot(world2[which(world2$TypeES!="All"),]) +
    theme_void()+
    geom_sf(aes(geometry = geometry, fill = DamCoeffT2*100)) +
    scale_fill_gradientn(colours = pal,limits=c(-3,3),
        na.value="transparent",name="T^2 damage estimate (%)",oob = scales::squish)+
    theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) + facet_wrap(~TypeES)+
    ggtitle("Estimated damages based on simulated PFTs")
    ggsave("map_DamCoef_prov_noprov.png",dpi=600)

    glimpse(PFT_ES_all)


































    PFT_ES_all_mean2 <- aggregate(areas_change_pct_adjusted~TypeES+PFT+horizon, FUN="mean", data=PFT_ES_all)
    glimpse(PFT_ES_all_mean2)

    PFT_ES_all_mean <-cbind(PFT_ES_all_mean,PFT_ES_all_mean2$areas_change_pct_adjusted)
    names(PFT_ES_all_mean)[length(PFT_ES_all_mean)] <- "areas_change_pct_adjusted"














    PFT_ES_all_mean2 <- aggregate(ES_relative~TypeES+PFT, FUN="mean", data=PFT_ES_all)
    
    ggplot(PFT_ES_all[PFT_ES_all$horizon!="Present" & PFT_ES_all$TypeES=="Provisioning",])+#,aes(group=interaction(TypeES,horizon,sign)))+
    geom_point(aes(y=ES_rel_areachange, x=areas_change_pct_adjusted,color=horizon))+
    xlim(c(-100,100))+ylim(c(0,100))






    ggplot(PFT_ES_all[PFT_ES_all$horizon!="Present" & PFT_ES_all$TypeES!="All",])+#,aes(group=interaction(TypeES,horizon,sign)))+
    #geom_point(aes(x=areas_change_pct, y=log(ES_total_areachange),col=TypeES))
    #geom_point(aes(x=areas_change_pct, y=ES_rel,col=horizon,shape=TypeES))+
    #geom_violin(aes(x=areas_change_pct, y=ES_rel,fill=horizon,col=TypeES))+
    #geom_boxplot(aes(x=areas_change_pct, y=ES_rel_areachange,fill=horizon,col=TypeES))+
    geom_density_ridges(aes(x=ES_rel_areachange,y=factor(PFT),fill=TypeES))
    #geom_boxplot_pattern(pattern_color = "white",pattern_fill = "black",aes(x=areas_change_pct, y=ES_rel_areachange,fill=horizon,pattern =TypeES))+
                       #facet_wrap(~PFT)+
    ylim(0,300)

    ggplot(PFT_ES_all[PFT_ES_all$horizon!="Present" & PFT_ES_all$TypeES!="All",],aes(group=interaction(TypeES,horizon,sign)))+
    #geom_point(aes(x=areas_change_pct, y=log(ES_total_areachange),col=TypeES))
    #geom_point(aes(x=areas_change_pct, y=ES_rel,col=horizon,shape=TypeES))+
    #geom_violin(aes(x=areas_change_pct, y=ES_rel,fill=horizon,col=TypeES))+
    #geom_boxplot(aes(x=areas_change_pct, y=ES_rel_areachange,fill=horizon,col=TypeES))+
    geom_violin_pattern(pattern_color = "white",pattern_fill = "black",aes(x=areas_change_pct, y=ES_rel_areachange,fill=horizon,pattern =TypeES))+
    #geom_boxplot_pattern(pattern_color = "white",pattern_fill = "black",aes(x=areas_change_pct, y=ES_rel_areachange,fill=horizon,pattern =TypeES))+
                       facet_wrap(~PFT)+
    ylim(0,300)
    
    
    ggplot(NC_change)+
    #geom_point(aes(x=areas_change_pct, y=log(ES_total_areachange),col=TypeES))
    geom_point(aes(x=Damage, y=NLoss_areachange,col=TypeES))
    #xlim(c(-1,4))+ylim(0,25)

    ggplot(PFT_ES_all[PFT_ES_all$horizon!="Present",])+
    #geom_point(aes(x=areas_change_pct, y=log(ES_total_areachange),col=TypeES))+
    geom_boxplot(aes(x=areas_change_pct, y=log(ES_total_areachange),col=TypeES))
    #xlim(c(-100,400))+ylim(0,25)
    
    library(treemapify)
    treeMapCoordinates <- treemapify(PFT_ES_all,
                                 area = "ES_rel",
                                 fill = "horizon",
                                 label = "PFT")
                                 #install.packages("ggplotify")
                                 #library("ggplotify")
    
    ggplot(PFT_ES_all, aes(area = ES_rel, fill = horizon)) +
        geom_treemap()
    treeMapPlot <- ggplotify(treeMapCoordinates) + 
                  scale_x_continuous(expand = c(0, 0)) +
                  scale_y_continuous(expand = c(0, 0)) +
                  scale_fill_brewer(palette = "Dark2")

    print(treeMapPlot)

    ggplot(data = PFT_ES_all[which(PFT_ES_all$TypeES!="All"),], aes(y=PFT,x=ES_relative))+
    #geom_violin(data = PFT_ES_all[which(PFT_ES_all$TypeES=="Provisioning"),],aes(fill=horizon)) + 
    geom_boxplot(data = PFT_ES_all[which(PFT_ES_all$TypeES=="Non-provisioning"),],aes(y=PFT,x=ES_relative,fill=horizon)) +
    xlim(40,200)
    glimpse(PFT_ES_all)




    ggplot(data = NC_change, aes(x=(N)/(TotalWealth),y=log(GDP)))+
    theme_bw() +
    #geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=4,size=1)+
    #  scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
    geom_point(aes(color=horizon,size=(((NatRents*0.01)))),alpha=0.75)+
    theme_bw() +
    xlab("Natural Capital in Forests and Protected Areas \n(Fraction of Inclusive Wealth)") + 
    ylab("Log GDP (2014 USD)") + 
    scale_colour_discrete(name="Continent")+
    facet_wrap(~TypeES)+
    scale_x_continuous(trans='log2')+
    #scale_shape_discrete(name="Natatural Capital change\nsince 1995")+
    scale_size_continuous(name="Production elasticity to \nNatural Capital")
    ggsave("Figures/NaturalCapital_change_clim.png")

    ggplot(data = NC_change, aes(x=Damage*100,y=log(GDP)))+
    theme_bw() +
    #geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=4,size=1)+
    #  scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
    geom_point(aes(color=horizon,size=(((NatRents*0.01)))),alpha=0.75)+
    theme_bw() +
    xlab("Damage NC (%)") + 
    ylab("Log GDP (2014 USD)") + 
    scale_colour_discrete(name="Continent")+
    #scale_x_continuous(trans='log2')+
    #scale_shape_discrete(name="Natatural Capital change\nsince 1995")+
    scale_size_continuous(name="Production elasticity to \nNatural Capital")+
    facet_wrap(~clim+TypeES)
    #facet_wrap(~)
    ggsave("Figures/NaturalCapital_change_damage_clim.png")



    

    #NaturalCapital$NLoss <- NaturalCapital$N/NaturalCapital$N[which(NaturalCapital$horizon=="Present")] - 1

    ggplot(data = NC_change, aes(x=(NLoss)/TotalWealth,y=log(GDP)))+
    theme_bw() +
    #geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=4,size=1)+
    #  scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
    geom_point(aes(color=horizon,size=(((NatRents*0.01)))),alpha=0.75)+
    theme_bw() +
    xlab("NC Loss (% of Total Wealth)") + 
    ylab("Log GDP (2014 USD)") + 
    scale_colour_discrete(name="Continent")+
    #scale_x_continuous(trans='log2')+
    #scale_shape_discrete(name="Natatural Capital change\nsince 1995")+
    scale_size_continuous(name="Production elasticity to \nNatural Capital")+facet_wrap(~clim)
    ggsave("Figures/NaturalCapital_change_damageTotalWealth_clim.png")

    

    world <- spData::world
    #world$coefDamage <- NA

    damcoef <- NC_change[which(NC_change$horizon=="Present"),which(names(NC_change) %in% c("country","DamageEstimateT2","clim"))]
    glimpse(world)
    names(damcoef)[1] <- "iso_a2"
    world <- merge(world,damcoef,by="iso_a2",all=TRUE)
    glimpse(world)
    
    pal <- palette(brewer.pal(n = 3, name = "Spectral"))
    
    world2 <- world[which(!is.na(world$DamageEstimateT2)),]
    ggplot(world2) +
    theme_void()+
    geom_sf(aes(geometry = geometry, fill = DamageEstimateT2*100)) +
    scale_fill_gradientn(colours = pal,limits=c(-3,3),
        na.value="transparent",name="T^2 damage estimate (%)",oob = scales::squish)+
    theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) + facet_wrap(~clim)+
    ggtitle("Estimated damages based on simulated PFTs")
    ggsave("Figures/NaturalCapital_estimatedamage_clim.png")

    missing_isos <- which(isos %in% world$iso_a2[is.na(world$DamageEstimateT2)])
    isos_missin3 <- countrycode(isos[missing_isos], origin = "iso2c", destination ="iso3c")
    world$name_long[is.na(world$DamageEstimateT2)]
    table(Wealth$countrycode)
    glimpse(world)
    
    ggplot(data = NC_change, aes(y=Damage*100,x=temp,colour=country))+
    theme_bw() +
    geom_smooth(formula=y~0 +poly(x, 2, raw=TRUE),method="lm",se=FALSE)+
    theme_bw() + facet_wrap(~clim)+
    xlab("Temperature change from present (C)") + 
    ylab("Damage NC (%)") 
    #scale_colour_discrete(name="Continent")+
    #scale_x_continuous(trans='log2')+
    #scale_shape_discrete(name="Natatural Capital change\nsince 1995")+
    #scale_size_continuous(name="Production elasticity to \nNatural Capital")
    ggsave("Figures/NaturalCapital_damage_temp_clim.png")

    # Read PFTs at different time horizons (end)



































#Simulations of Economic Growth with Climate Damages (start)
        glimpse(wealth_data)
        w <- wealth_data[-which(is.na(wealth_data)),]
        w <- w[(!is.na(w$NforestT)),]
        w <- w[(is.finite(w$NforestT)),]
        w <- w[w$NforestT!=0,]
        w <- w[(!is.na(w$K)),]
        w <- w[(!is.na(w$H)),]
        w <- w[(!is.na(w$GDP)),]
        w$logGDP <- log(w$GDP)
        w$logK <- log(w$K)
        w$logH <- log(w$H)
        w$logNforestT <- log(w$NforestT)
        w$logNforestES <- log(w$NforestES)
        w$logNnonProv <- log(w$NforestES+w$Npa)
        w$logNgreen <- log(w$Ngreen)
        hist(w$logNforestT)
        summary(felm(logGDP~logK+logH+logNforestT|year + countrycode|0|0,data=w, weights = (w$logGDP)))
        K_elas_world <- summary(felm(logGDP~logK+logH+logNforestT|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[1]
        L_elas_world <- summary(felm(logGDP~logK+logH+logNforestT|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[2]
        
        
        summary(felm(logGDP~logK+logH+logNforestT+logNnonProv|year + countrycode|0|0,data=w, weights = (w$logGDP)))
        K_elas_world <- summary(felm(logGDP~logK+logH+logNforestT+logNnonProv|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[1]
        L_elas_world <- summary(felm(logGDP~logK+logH+logNforestT+logNnonProv|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[2]
        NnonProv_elas_world <- summary(felm(logGDP~logK+logH+logNforestT+logNnonProv|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[4]
        
        L_elas_world <- summary(felm(logGDP~logK+logH+logNforestT|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[2]
        r1 <- felm(logGDP~logK+logH+logNforestT|year + countrycode|0|Continent,data=w, weights = (w$logGDP))
        r2 <- felm(logGDP~logK+logH+logNforestT|year + countrycode|0|Continent,data=w)
        r3 <- felm(logGDP~logK+logH+logNforestT+log(Nagg)|year + countrycode|0|0,data=w, weights = (w$logGDP))
    
    summary(felm(logGDP~logK+logH+logNforestT|year + countrycode|0|Continent,data=w, weights = (w$logGDP)))
    stargazer(r1,r2,r3, type="html",out="elas_panels.html")
        
    # Fixed Investments on Manufactured Capital + NatCap damages (exp2)
        glimpse(wdata2018)
        wdata2018$year <- as.numeric(as.character(wdata2018$year))
        wealth_sim <- wdata2018[,which(names(wdata2018) %in% c("countrycode",
        "year","H","N","Nagg","NforestT","NforestES","Npa","K","TotalWealth","GDP","Population","NatRents","Continent","labor_share"))]
        wealth_sim$n <- 0 #net natural capital
        wealth_sim$gy <- 0 #output growth
        wealth_sim$meangy <- 0 #mean output growth
        wealth_sim$exp <- NA #experiment name
        country <- levels(factor(wealth_sim$countrycode))
        #lm(c(0,Tshort,Tmed,Tlong)~c(0,10,30,70)) 0.03 per year
        Temp_ssps <- read.csv("Data/Temp_CMIP6_mean.csv")
        library("readxl")
        Temp_ssps <- read_excel("Data/Temp_ssps_ndc.xlsx", sheet = "data")
        names(Temp_ssps)[1] <- "year"      
        NC_change$countryiso3 <- countrycode(NC_change$country, origin="iso2c", destination="iso3c" )
        NC_dam$countryiso3 <- countrycode(NC_dam$country, origin="iso2c", destination="iso3c" )
        NC_damse$countryiso3 <- countrycode(NC_damse$country, origin="iso2c", destination="iso3c" )
        simname <- c("nodam","10p","SSP245","NDC_cond")
        TypeESi <- c("All","Provisioning","Non-provisioning")
        for(ss in 1:length(simname)){
            for(climi in 1:length(clim_models)){
                
            for (i in 1:length(country)){
                s=0.2 #fixed savings rate
                d=0.1 #depreciation rate
                t=83 #number of years of simulation
                #a1 = wealth_sim$labor_share[wealth_sim$countrycode==country[i]] *0.01
                a1 <- L_elas_world
                a3 = wealth_sim$NatRents[wealth_sim$countrycode==country[i]] *0.01
                
                if(is.na(a1)){a1 <- L_elas_world}
                if (is.na(a3)){a3 <- 0}
                a2 = 1-a1-a3
                if (a2<0) {
                a2 <- 0.0001
                a3 <- a3-a2}
                y0 = wealth_sim$GDP[wealth_sim$countrycode==country[i]]  
                K0 = wealth_sim$K[wealth_sim$countrycode==country[i]] 
                T0 = wealth_sim$TotalWealth[wealth_sim$countrycode==country[i]]
                h0 = wealth_sim$H[wealth_sim$countrycode==country[i]]
                NforestES = wealth_sim$NforestES[wealth_sim$countrycode==country[i]]
                NforestT = wealth_sim$NforestT[wealth_sim$countrycode==country[i]] 
                Nagg = wealth_sim$Nagg[wealth_sim$countrycode==country[i]]
                Npa = wealth_sim$Npa[wealth_sim$countrycode==country[i]]

                n0 = NforestT

                n0_es <- NforestES + Npa
                
                #n0 = wealth_sim$N[wealth_sim$countrycode==country[i]]
                yt=numeric(length=t);yt[1]=y0
                est=numeric(length=t);est[1]=n0_es*0.03
                kt=numeric(length=t);kt[1]=K0
                Tt=numeric(length=t);Tt[1]=T0
                pi1 <- y0/(h0^a1*K0^a2*n0^a3)    #normalizing param
                year = 1:83
                #df <- wealth_sim[0,]

                    nt=numeric(length=t);nt[1]=n0
                    nt_es=numeric(length=t);nt_es[1]=n0_es
                    yt=numeric(length=t);yt[1]=y0
                    kt=numeric(length=t);kt[1]=K0
                    for(j in 2:t){
                    kt[j]=kt[j-1]*(1-0)+(yt[j-1])*s

                    if (simname[ss]=="nodam"){
                        nt[j] = nt[j-1]
                        nt_es[j] = nt_es[j-1]
                    }else if(simname[ss]=="10p"){
                        if(j==6){nt[j] = nt[j-1]*0.9}else{
                            nt[j] = nt[j-1]
                            nt_es[j] = nt_es[j-1]}
                            if(j==7){
                                nt[j] = nt[j-1]/0.9
                                nt_es[j] = nt_es[j-1]/0.9
                            }
                        } else if(simname[ss]=="SSP245"){
                        if(j>5){
                            nt[j] = nt[j-1]*(1+
                                NC_change$DamageEstimateT2[NC_change$countryiso3==country[i] & NC_change$clim == clim_models[climi]& NC_change$TypeES=="Provisioning"  ][1]*
                                (Temp_ssps[j+2,2]-Temp_ssps[1,2])^2)[[1]]
                            nt_es[j] = nt_es[j-1]*(1+
                                NC_change$DamageEstimateT2_areachange[NC_change$countryiso3==country[i] & NC_change$clim == clim_models[climi]& NC_change$TypeES=="Non-provisioning" ][1]*
                                (Temp_ssps[j+2,2]-Temp_ssps[1,2])^2)[[1]]
                        
                        }else{
                            nt[j] = nt[j-1]
                            nt_es[j] = nt_es[j-1]
                        }

                    } else if(simname[ss]=="NDC_cond"){
                        if(j>5){
                            nt[j] = nt[j-1]*(1+
                                NC_change$DamageEstimateT2[NC_change$countryiso3==country[i] & NC_change$clim == clim_models[climi]& NC_change$TypeES=="Provisioning"  ][1]*
                                (Temp_ssps[j+2,3]-Temp_ssps[1,3])^2)[[1]]
                            nt_es[j] = nt_es[j-1]*(1+
                                NC_change$DamageEstimateT2_areachange[NC_change$countryiso3==country[i] & NC_change$clim == clim_models[climi] & NC_change$TypeES=="Non-provisioning" ][1]*
                                (Temp_ssps[j+2,3]-Temp_ssps[1,3])^2)[[1]]
                        
                        }else{
                            nt[j] = nt[j-1]
                            nt_es[j] = nt_es[j-1]
                        }
                        
                    }
                    
                    yt[j]=pi1*(h0^a1)*(kt[j]^(a2))*nt[j]^a3
                    est[j]=nt_es[j]*0.03  #Because Natural Capital is the net present value of the flow of benefits
                    }
                    gy <- (yt - lag(yt))/lag(yt) #output growth
                    meangy <- mean(gy[3:length(gy)]) #mean output growth 
                    npv <- sum(yt*(1/((1+0.03)^seq(1:length(yt)))))
                    dam_estimate_prov <- NC_change$DamageEstimateT2[NC_change$countryiso3==country[i] & NC_change$clim == clim_models[climi] & NC_change$TypeES=="Provisioning" ][1]
                    dam_estimate_nonprov <- NC_change$DamageEstimateT2_areachange[NC_change$countryiso3==country[i] & NC_change$clim == clim_models[climi] & NC_change$TypeES=="Non-provisioning" ][1]
                    

                    
                    if(i==1 & ss==1 & climi==1 ){
                        df <- data.frame(countrycode=country[i], year=year+2017, H=h0, N=wealth_sim$N[wealth_sim$countrycode==country[i]], 
                                Nagg=Nagg,
                                NforestT=wealth_sim$NforestT[wealth_sim$countrycode==country[i]],
                                NforestES=wealth_sim$NforestES[wealth_sim$countrycode==country[i]],
                                Npa=Npa,
                                K=kt,
                                TotalWealth=wealth_sim$TotalWealth[wealth_sim$countrycode==country[i]], 
                                GDP=yt,
                                Population=wealth_sim$Population[wealth_sim$countrycode==country[i]],
                                labor_share=wealth_sim$labor_share[wealth_sim$countrycode==country[i]],
                                NatRents=wealth_sim$NatRents[wealth_sim$countrycode==country[i]],
                                Continent=wealth_sim$Continent[wealth_sim$countrycode==country[i]],n=nt,
                                gy = gy, meangy = meangy,exp=simname[ss],clim=clim_models[climi],npv=npv,dam_prov=dam_estimate_prov, dam_nonprov = dam_estimate_nonprov ,
                                es = est, n_es = nt_es,elasH=a1,elasN=a3,elasK=a2,tfp=pi1 )
                    }else{
                        df <- rbind(df,data.frame(countrycode=country[i], year=year+2017, H=h0, N=wealth_sim$N[wealth_sim$countrycode==country[i]], 
                                Nagg=Nagg,
                                NforestT=wealth_sim$NforestT[wealth_sim$countrycode==country[i]],
                                NforestES=wealth_sim$NforestES[wealth_sim$countrycode==country[i]],
                                Npa=Npa,
                                K=kt,
                                TotalWealth=wealth_sim$TotalWealth[wealth_sim$countrycode==country[i]], 
                                GDP=yt,
                                Population=wealth_sim$Population[wealth_sim$countrycode==country[i]],
                                labor_share=wealth_sim$labor_share[wealth_sim$countrycode==country[i]],
                                NatRents=wealth_sim$NatRents[wealth_sim$countrycode==country[i]],
                                Continent=wealth_sim$Continent[wealth_sim$countrycode==country[i]],n=nt,
                                gy = gy, meangy = meangy,exp=simname[ss],clim=clim_models[climi],npv=npv,dam_prov=dam_estimate_prov, dam_nonprov = dam_estimate_nonprov ,
                                es = est, n_es = nt_es ,elasH=a1,elasN=a3,elasK=a2,tfp=pi1 )) 
                    }
                #wealth_sim <- rbind(wealth_sim,df)
                }
            }
            }
        

        
        save(df,file="sim_nc.Rda")
        load("sim_nc.Rda")
        glimpse(df)
       

        wealth_sim3 <- df[df$exp=="10p",]

       natcap_estimates  <- df[which(df$exp=="SSP245" & df$year==2018),]
       natcap_estimates  <- natcap_estimates[,which(names(natcap_estimates) %in% c("countrycode","year","H","K","n","dam_prov","elasH","elasK","elasN"))]
       dam_estimates  <-aggregate(dam_prov~countrycode,data=natcap_estimates,FUN="mean")
       glimpse(dam_estimates)
       dam_estimatessd  <-aggregate(dam_prov~countrycode,data=natcap_estimates,FUN="sd")
       glimpse(dam_estimatessd)

       dam_estimates<- cbind(dam_estimates,dam_estimatessd[,2])
       names(dam_estimates)[2:3] <- c("DamCoeff_T2","DamCoeff_T2_sd")
       
       natcap_estimates  <- df[which(df$exp=="SSP245" & df$year==2018 & df$clim=="hadgem2-es"),]
       natcap_estimates  <- natcap_estimates[,which(names(natcap_estimates) %in% c("countrycode","year","H","K","n","elasH","elasK","elasN"))]
       natcap_estimates <- merge(natcap_estimates,dam_estimates,by="countrycode",all=TRUE)
       glimpse(natcap_estimates)
       write.csv(natcap_estimates,"NatCap_Estimates.csv")



    pal <- palette(brewer.pal(n = 3, name = "Spectral"))
        ggplot(data= wealth_sim3[wealth_sim3$Continent == "Americas",], 
            aes(x=year,y=gy*100,group=countrycode))+
        theme_bw()+
        geom_line(aes(color=NatRents*0.01, size=(NforestT+NforestES+Npa)/TotalWealth))+                   #(NforestT+NforestES+Npa)/TotalWealth)))  +
        xlab("Year") + 
        ylab("GDP growth (pp)") + 
        xlim(c(2020,2040))+
        ggtitle("Simulation with Natural capital damaged on 2023")+
        #geom_text(data = subset(wealth_sim1[wealth_sim1$Continent == "Americas",], year == "2035"), 
         #   aes(label = countrycode, colour = countrycode, x = 2038, y = log(GDP/Population)), hjust = -.1) +
            #scale_colour_discrete(guide = 'none')  +   
            scale_color_gradientn(colours = pal,limits=c(0,0.01),
                name="Natural Capital Elasticity",oob = scales::squish) +   
            scale_size_area(max_size = 3, name="Natural Capital \n(fraction of Total Wealth)")
            #theme(plot.margin = unit(c(1,3,1,1), "lines")) 
        #ggsave("Figures/Simulation_10percent_damages.png")

        #df$Ngreen <- df$NforestES+df$NforestT+df$Npa
        glimpse(df$exp)
        all_sim <- data.frame(gdp_change = (df$GDP - df$GDP[df$exp=="nodam"])/df$GDP[df$exp=="nodam"],
        es_change = (df$es - df$es[df$exp=="nodam"])/df$es[df$exp=="nodam"],
         growth_change = df$gy - df$gy[df$exp=="nodam"],
        npv_change =  df$npv - df$npv[df$exp=="nodam"], 
        Nchange =  df$n - df$n[df$exp=="nodam"], 
        Nchange_es =  df$n_es - df$n_es[df$exp=="nodam"], 
        country = df$country, year=df$year, H=df$H,
        N=df$N   ,  Nagg=df$Nagg,
        NforestT = df$NforestT,   
        NforestES = df$NforestES,   Npa=df$Npa,
        K=df$K,          TotalWealth=df$TotalWealth,
        GDP=df$GDP,      Population=df$Population,  
        labor_share=df$labor_share, NatRents=df$NatRents,    
        Continent = df$Continent, 
        n=df$n, n_es=df$n_es, gy=df$gy,          meangy=df$meangy,      
        exp=df$exp , clim=df$clim    , npv = df$npv , es=df$es )

        glimpse(all_sim)
        
        sim_2100 <- all_sim[all_sim$exp %notin% c("10p","nodam") & all_sim$year %in% c(2100),]
        meanGrowthChange <- aggregate(growth_change~country+exp,FUN="mean",data=sim_2100)
        names(meanGrowthChange)[3] <- "MeanGrowthChange"
        
        sim_2100 <- merge(sim_2100,meanGrowthChange,by=c("country","exp"))
        glimpse(sim_2100)

        
    ggplot(data = sim_2100[which(sim_2100$clim==clim_models[1]),], 
            aes(x=MeanGrowthChange*100,y=log(GDP/Population),color=Continent,shape=exp))+
            xlab("Mean GDP growth change (pp)")+ylab("Log GDP per capita")+
        theme_bw()+
        geom_point(aes(size=NatRents*0.01))+ggtitle("Change in mean GDP growth (2020-2100)")+
        guides(color=guide_legend(title="Region"),size=guide_legend(title="Natural capital\n elasticity"),shape=guide_legend(title="Scenario"))
       #ggsave("Change_GDP_growth_ssps_ndc.png")

       all_sim_af <- all_sim[all_sim$country=="MEX",]
       ggplot(all_sim_af, aes(x=year,y=gdp_change,linetype=clim,color=exp,group=interaction(exp,clim)))+
       geom_line()+theme_bw()

       glimpse(all_sim)
       all_sim <-all_sim[,-which(names(all_sim) %in% c("clim"))]
       all_sim <-all_sim[which(all_sim$exp %in% c("SSP245","NDC_cond")),]
       all_sim2 <- all_sim[,which(names(all_sim) %in% c("es_change","gdp_change","country","year",'exp',"Continent"))]
    
       all_sim_mean <- stats::aggregate(.~country+year+exp+Continent,data=all_sim2, FUN="mean")
       all_sim_sd <- aggregate(.~country+year+exp+Continent,data=all_sim2, FUN="sd")

    save(all_sim_mean,file= "all_sim_mean.Rda")
    load("all_sim_mean.Rda")
    glimpse(all_sim_mean)
           
    

     
    ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year==2099),],
     aes(x=es_change,y=gdp_change,color=Continent))+
       geom_point()+theme_bw()
       
       glimpse(all_sim_mean)
       glimpse(all_sim_sd)
       names(all_sim_sd)[5:6] <- c("gdp_change_sd","es_change_sd")
       all_sim_sd$id <- paste0(all_sim_sd$country,all_sim_sd$year,all_sim_sd$exp)
       all_sim_mean$id <- paste0(all_sim_mean$country,all_sim_mean$year,all_sim_mean$exp)

        all_sim_mean <- all_sim_mean[which(all_sim_mean$es_change<quantile(all_sim_mean$es_change,0.95)),]
        all_sim_mean <- all_sim_mean[which(all_sim_mean$es_change>quantile(all_sim_mean$es_change,0.05)),]
        glimpse(all_sim_mean)
        
        all_sim_mean <- merge(all_sim_mean,all_sim_sd[,c(5,6,7)],by="id",all=FALSE)

    
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
       ylab("GDP change (%)") + ggtitle("") + 
        coord_cartesian(xlim=c(-100, 25),ylim=c(-25,10))
       plot_trajectory

      leg_trj <-  get_legend(plot_trajectory)

    maxyear <- aggregate(year~country, data=all_sim_mean[which(all_sim_mean$exp=="SSP245" ),],FUN="max")
    maxyear$countrymax <- paste0(maxyear$country,maxyear$year)
    all_sim_mean$countrymax <- paste0(all_sim_mean$country,all_sim_mean$year)

    all_simmax <- all_sim_mean[all_sim_mean$countrymax %in% maxyear$countrymax,]
    
plot_count_es <- ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100),], # & abs(all_sim_mean$es_change)>0.01
     aes(x=es_change*100,fill=Continent))+
     geom_histogram()+theme_minimal()+ xlab("")+ylab("count in 2100") + xlim(-100,25)

     mean(all_sim_mean$es_change[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100)])
     
     
     aggregate(gdp_change~Continent,data=all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100),],FUN="mean")

     

plot_count_gdp <- ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100 ),], #& abs(all_sim_mean$gdp_change)>0.01
     aes(x=gdp_change*100,fill=Continent))+xlab('')+xlim(-25,10)+ylab("count in 2100")+
     geom_histogram()+theme_minimal()+coord_flip() 
mean(all_sim_mean$gdp_change[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100 )])
mean(all_sim_mean$gdp_change[which(all_sim_mean$exp=="NDC_cond" & all_sim_mean$year ==2100 )])
mean(all_sim_mean$es_change[which(all_sim_mean$exp=="NDC_cond" & all_sim_mean$year ==2100 )])

    emptyplot <- ggplot()+theme_void()
        
    traj_plot <- ggarrange(ggarrange(plot_trajectory, plot_count_gdp,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="bottom"),
        ncol=1,nrow=2,common.legend=TRUE,heights=c(3,1),legend="bottom",align="hv")
    
    annotate_figure(traj_plot, top = text_grob("Benefits change under SSP2-4.5", 
                face = "bold", size = 14))
    ggsave("Figures/Trajectory_completeHist.png",dpi=300)

     ggarrange(ggarrange(plot_trajectory, plot_count_gdp,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="none"),as_ggplot(leg_trj),
        ncol=1,nrow=3,common.legend=TRUE,heights=c(4,1,1),legend="none",align="hv")
ggsave("Figures/Trajectory_shapesYear_ncp.png",dpi=300)


plot_gdp_gdpchange <- ggplot(data=all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100),])+
geom_point(aes(x=log(GDP),y=gdp_change*100,color=Continent))+
geom_smooth(aes(x=log(GDP),y=gdp_change*100))+
theme_bw() + xlab("Log GDP in 2100") + ylab("GDP change in 2100 (%)")+ 
geom_hline(aes(yintercept=0),linetype="dashed")+
theme(legend.position="bottom")

plot_gdp_eschange <- ggplot(data=all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100),])+
geom_point(aes(x=log(GDP),y=es_change*100,color=Continent))+
geom_smooth(aes(x=log(GDP),y=es_change*100))+
theme_bw() + xlab("Log GDP in 2100") + ylab("Ecosystem services change in 2100 (%)") + 
geom_hline(aes(yintercept=0),linetype="dashed") +
theme(legend.position="bottom")

ggarrange(plot_gdp_eschange,plot_gdp_gdpchange,nrow=1,ncol=2,common.legend=TRUE)
ggsave("logGDP_esgdpchange.png",dpi=300)

es_c2100 <- all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year %in% c(2100) ),]
es_c2100 <- es_c2100[which(es_c2100$es_change<max(es_c2100$es_change)),]
es_c2100$es_change <- es_c2100$es_change * es_c2100$es
es_c2100 <- es_c2100[order(es_c2100$es_change),]
es_c2100$cumes <- cumsum(es_c2100$es_change)
es_c2100$cumes <- cumsum(es_c2100$es_change)
es_c2100$cumes_perc <- es_c2100$cumes / max(es_c2100$cumes)
es_c2100$cumpop <- cumsum(es_c2100$Population)
es_c2100$cumpop_perc <- es_c2100$cumpop / max(es_c2100$cumpop)
glimpse(es_c2100)

lorenzES <- ggplot(es_c2100)+
geom_line(aes(x=cumpop_perc,y=cumes_perc)) + theme_bw() +
geom_point(aes(x=cumpop_perc,y=cumes_perc,color=Continent)) +
geom_abline(intercept = 0, slope = 1,color="gray",lty=2)+
xlab("Cumulative share of Population") + ylab("Cumulative ecosystem services change")



gdp_c2100 <- all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year %in% c(2100) ),]
#gdp_c2100 <- gdp_c2100[which(es_c2100$gdp_change<max(es_c2100$gdp_change)),]
gdp_c2100$gdp_change <-  gdp_c2100$gdp_change*gdp_c2100$GDP
gdp_c2100$gdp_change <-  gdp_c2100$gdp_change
gdp_c2100 <- gdp_c2100[order(gdp_c2100$gdp_change),]
gdp_c2100$cumgdp <- cumsum(gdp_c2100$gdp_change)
gdp_c2100$cumgdp_perc <- gdp_c2100$cumgdp / abs(gdp_c2100$cumgdp[length(gdp_c2100$cumgdp)])
gdp_c2100$cumpop <- cumsum(gdp_c2100$Population)
gdp_c2100$cumpop_perc <- gdp_c2100$cumpop / max(gdp_c2100$cumpop)
glimpse(gdp_c2100)

lorenzGDP <- ggplot(gdp_c2100)+
geom_line(aes(x=cumpop_perc,y=cumgdp_perc)) + theme_bw() +
geom_point(aes(x=cumpop_perc,y=cumgdp_perc,color=Continent)) +
geom_abline(intercept = 0, slope = 1,color="gray",lty=2)+
xlab("Cumulative share of population") + ylab("Cumulative GDP loss")
ggarrange(lorenzGDP,lorenzES,common.legend=TRUE,legend="bottom")
ggsave("Lorenzcurves.png",dpi=300)

all_sim_mean_cont <- all_sim_mean[which(all_sim_mean$country %in% all_sim_mean$country[all_sim_mean$year==2100 & all_sim_mean$exp=="SSP245"]),]
glimpse(all_sim_mean_cont)
all_sim_mean_cont <- all_sim_mean_cont[,-which(names(all_sim_mean_cont) %in% c("country","countrymax"))]
       all_sim_mean_cont <- aggregate(.~year+exp+Continent,data=all_sim_mean_cont, FUN="mean")


        all_sim_mean_cont$utility <- (-1/(1-1.45))*(0.9*all_sim_mean_cont$GDP^0.6 + 0.1*all_sim_mean_cont$es^0.6)^((1-1.45)/0.6)
        
        
        init_u <- data.frame(initial_u = all_sim_mean_cont$utility[which(all_sim_mean_cont$year==2019)])
        init_u$expcont <- paste0(all_sim_mean_cont$exp[which(all_sim_mean_cont$year==2019)],all_sim_mean_cont$Continent[which(all_sim_mean_cont$year==2019)])
        all_sim_mean_cont$expcont <- paste0(all_sim_mean_cont$exp,all_sim_mean_cont$Continent)
        
        all_sim_mean_cont <- merge(all_sim_mean_cont,init_u,by="expcont")
        all_sim_mean_cont$utility_frac <- all_sim_mean_cont$utility/all_sim_mean_cont$initial_u

        ggplot(all_sim_mean_cont)+
        geom_line(aes(x=year,y=utility,linetype=exp,color=Continent))+theme_bw()
        glimpse(all_sim_mean_cont)



        all_sim_mean_cont <- rbind(all_sim_mean_cont,all_sim_mean_cont)
       all_sim_mean_cont$damages <- c(all_sim_mean_cont$gdp_change[1:(dim(all_sim_mean_cont)[1]/2)],c(all_sim_mean_cont$es_change[((dim(all_sim_mean_cont)[1]/2)+1):dim(all_sim_mean_cont)[1]]))
   all_sim_mean_cont$damage_type <- rep(c("Economic production","Ecosystem Services"),each=1250)
    
    glimpse(all_sim_mean_cont)
   ggplot(all_sim_mean_cont[which(all_sim_mean_cont$exp %in% c("SSP245" )),])+
      geom_line(aes(x=year,y=damages*100,color=Continent,linetype=damage_type))+
      xlab("Year") + ylab("GDP change (%)")+
      labs(linetype="Damage Type",color="Region")+theme_bw()
      ggsave("Figures/GDP_Damages_SSP245.png")

        all_sim_mean_cont$Temp <- NA
        all_sim_mean_cont$Temp[which(all_sim_mean_cont$exp=="10p")] <- 0
        all_sim_mean_cont$Temp[which(all_sim_mean_cont$exp=="NDC_cond")] <- Temp_ssps$NDC_cond[5:dim(Temp_ssps)[1]]
        
        all_sim_mean_cont$Temp[which(all_sim_mean_cont$exp=="SSP245")] <- Temp_ssps$SSP245[5:dim(Temp_ssps)[1]]
        glimpse(all_sim_mean_cont)





        







        ggplot(all_sim_mean_cont[which(all_sim_mean_cont$year==2100 & all_sim_mean_cont$exp %in% c("SSP245","NDC_cond" )),])+
            geom_point(aes(x=N,y=damages*100,color=Continent,shape=damage_type))+
            xlab("Year") + ylab("GDP change (%)")+
            labs(linetype="Damage Type",color="Region")+theme_bw()
 



      glimpse(all_sim_mean_cont)
   ggplot(all_sim_mean_cont[which(all_sim_mean_cont$exp %in% c("SSP245","NDC_cond" )),])+
      geom_line(aes(x=year,y=damages*100,color=Continent,linetype=damage_type))+
      xlab("Year") + ylab("GDP change (%)")+
      labs(linetype="Damage Type",color="Region")+theme_bw()
      
   ggplot(all_sim_mean_cont[which(all_sim_mean_cont$exp=="SSP245" ),])+
      geom_line(aes(x=year,y=gdp_change,color=Continent))+
      geom_line(aes(x=year,y=es_change,color=Continent),linetype="dashed")






glimpse(all_sim)
ggplot(all_sim)+
geom_line(x=year, y=GDP)







     for(ss in 1:length(simname)){
                
            for (i in 1:length(country)){
                s=0.2 #fixed savings rate
                d=0.1 #depreciation rate
                t=83 #number of years of simulation
                #a1 = wealth_sim$labor_share[wealth_sim$countrycode==country[i]] *0.01
                a1 <- L_elas_world
                a3 = wealth_sim$NatRents[wealth_sim$countrycode==country[i]] *0.01
                
                if(is.na(a1)){a1 <- L_elas_world}
                if (is.na(a3)){a3 <- 0}
                a2 = 1-a1-a3
                if (a2<0) {
                a2 <- 0.0001
                a3 <- a3-a2}
                y0 = wealth_sim$GDP[wealth_sim$countrycode==country[i]]  
                K0 = wealth_sim$K[wealth_sim$countrycode==country[i]] 
                T0 = wealth_sim$TotalWealth[wealth_sim$countrycode==country[i]]
                h0 = wealth_sim$H[wealth_sim$countrycode==country[i]]
                NforestES = wealth_sim$NforestES[wealth_sim$countrycode==country[i]]
                NforestT = wealth_sim$NforestT[wealth_sim$countrycode==country[i]] 
                Nagg = wealth_sim$Nagg[wealth_sim$countrycode==country[i]]
                Npa = wealth_sim$Npa[wealth_sim$countrycode==country[i]]

                n0 = NforestT

                n0_es <- NforestES + Npa
                
                #n0 = wealth_sim$N[wealth_sim$countrycode==country[i]]
                yt=numeric(length=t);yt[1]=y0
                est=numeric(length=t);est[1]=n0_es*0.03
                kt=numeric(length=t);kt[1]=K0
                Tt=numeric(length=t);Tt[1]=T0
                pi1 <- y0/(h0^a1*K0^a2*n0^a3)    #normalizing param
                year = 1:83
                #df <- wealth_sim[0,]

                    nt=numeric(length=t);nt[1]=n0
                    nt_es=numeric(length=t);nt_es[1]=n0_es
                    yt=numeric(length=t);yt[1]=y0
                    kt=numeric(length=t);kt[1]=K0
                for(j in 2:t){
                        kt[j]=kt[j-1]*(1-0)+(yt[j-1])*s

                        if (simname[ss]=="nodam"){
                            nt[j] = nt[j-1]
                            nt_es[j] = nt_es[j-1]
                        }else if(simname[ss]=="10p"){
                            if(j==6){nt[j] = nt[j-1]*0.9}else{
                                nt[j] = nt[j-1]
                                nt_es[j] = nt_es[j-1]}
                                if(j==7){
                                    nt[j] = nt[j-1]/0.9
                                    nt_es[j] = nt_es[j-1]/0.9
                                }
                            } else if(simname[ss]=="SSP245"){
                            if(j>5){
                                nt[j] = nt[j-1]*(1+
                                    NC_dam$DamageEstimateT2[NC_dam$countryiso3==country[i]& NC_dam$TypeES=="Provisioning"  ][1]*
                                    (Temp_ssps[j+2,2]-Temp_ssps[1,2])^2)[[1]]
                                nt_es[j] = nt_es[j-1]*(1+
                                    NC_dam$DamageEstimateT2_areachange[NC_dam$countryiso3==country[i] &  NC_dam$TypeES=="Non-provisioning" ][1]*
                                    (Temp_ssps[j+2,2]-Temp_ssps[1,2])^2)[[1]]
                            
                            }else{
                                nt[j] = nt[j-1]
                                nt_es[j] = nt_es[j-1]
                            }

                        } else if(simname[ss]=="NDC_cond"){
                            if(j>5){
                                nt[j] = nt[j-1]*(1+
                                    NC_dam$DamageEstimateT2[NC_dam$countryiso3==country[i] & NC_dam$TypeES=="Provisioning"  ][1]*
                                    (Temp_ssps[j+2,3]-Temp_ssps[1,3])^2)[[1]]
                                nt_es[j] = nt_es[j-1]*(1+
                                    NC_dam$DamageEstimateT2_areachange[NC_dam$countryiso3==country[i] & NC_dam$TypeES=="Non-provisioning" ][1]*
                                    (Temp_ssps[j+2,3]-Temp_ssps[1,3])^2)[[1]]
                            
                            }else{
                                nt[j] = nt[j-1]
                                nt_es[j] = nt_es[j-1]
                            }
                            
                    }
                    
                    yt[j]=pi1*(h0^a1)*(kt[j]^(a2))*nt[j]^a3
                    est[j]=nt_es[j]*0.03  #Because Natural Capital is the net present value of the flow of benefits
                }
                    gy <- (yt - lag(yt))/lag(yt) #output growth
                    meangy <- mean(gy[3:length(gy)]) #mean output growth 
                    npv <- sum(yt*(1/((1+0.03)^seq(1:length(yt)))))
                    dam_estimate_prov <- NC_dam$DamageEstimateT2[NC_dam$countryiso3==country[i] & NC_dam$TypeES=="Provisioning" ][1]
                    dam_estimate_nonprov <- NC_dam$DamageEstimateT2_areachange[NC_dam$countryiso3==country[i] & NC_dam$TypeES=="Non-provisioning" ][1]
                    

                    
                    if(i==1 & ss==1   ){
                        df <- data.frame(countrycode=country[i], year=year+2017, H=h0, N=wealth_sim$N[wealth_sim$countrycode==country[i]], 
                                Nagg=Nagg,
                                NforestT=wealth_sim$NforestT[wealth_sim$countrycode==country[i]],
                                NforestES=wealth_sim$NforestES[wealth_sim$countrycode==country[i]],
                                Npa=Npa,
                                K=kt,
                                TotalWealth=wealth_sim$TotalWealth[wealth_sim$countrycode==country[i]], 
                                GDP=yt,
                                Population=wealth_sim$Population[wealth_sim$countrycode==country[i]],
                                labor_share=wealth_sim$labor_share[wealth_sim$countrycode==country[i]],
                                NatRents=wealth_sim$NatRents[wealth_sim$countrycode==country[i]],
                                Continent=wealth_sim$Continent[wealth_sim$countrycode==country[i]],n=nt,
                                gy = gy, meangy = meangy,exp=simname[ss],npv=npv,dam_prov=dam_estimate_prov, dam_nonprov = dam_estimate_nonprov ,
                                es = est, n_es = nt_es )
                    }else{
                        df <- rbind(df,data.frame(countrycode=country[i], year=year+2017, H=h0, N=wealth_sim$N[wealth_sim$countrycode==country[i]], 
                                Nagg=Nagg,
                                NforestT=wealth_sim$NforestT[wealth_sim$countrycode==country[i]],
                                NforestES=wealth_sim$NforestES[wealth_sim$countrycode==country[i]],
                                Npa=Npa,
                                K=kt,
                                TotalWealth=wealth_sim$TotalWealth[wealth_sim$countrycode==country[i]], 
                                GDP=yt,
                                Population=wealth_sim$Population[wealth_sim$countrycode==country[i]],
                                labor_share=wealth_sim$labor_share[wealth_sim$countrycode==country[i]],
                                NatRents=wealth_sim$NatRents[wealth_sim$countrycode==country[i]],
                                Continent=wealth_sim$Continent[wealth_sim$countrycode==country[i]],n=nt,
                                gy = gy, meangy = meangy,exp=simname[ss],npv=npv,dam_prov=dam_estimate_prov, dam_nonprov = dam_estimate_nonprov ,
                                es = est, n_es = nt_es  )) 
                    }
                #wealth_sim <- rbind(wealth_sim,df)
                }
            }
            
        

        df_sim_nc_dam <- df
        save(df_sim_nc_dam,file="df_sim_nc_dam.Rda")
        load("df_sim_nc_dam.Rda")
       df <- df_sim_nc_dam 

        wealth_sim3 <- df[df$exp=="10p",]

       
    pal <- palette(brewer.pal(n = 3, name = "Spectral"))
        ggplot(data= wealth_sim3[wealth_sim3$Continent == "Americas",], 
            aes(x=year,y=gy*100,group=countrycode))+
        theme_bw()+
        geom_line(aes(color=NatRents*0.01, size=(NforestT+NforestES+Npa)/TotalWealth))+                   #(NforestT+NforestES+Npa)/TotalWealth)))  +
        xlab("Year") + 
        ylab("GDP growth (pp)") + 
        xlim(c(2020,2040))+
        ggtitle("Simulation with Natural capital damaged on 2023")+
        #geom_text(data = subset(wealth_sim1[wealth_sim1$Continent == "Americas",], year == "2035"), 
         #   aes(label = countrycode, colour = countrycode, x = 2038, y = log(GDP/Population)), hjust = -.1) +
            #scale_colour_discrete(guide = 'none')  +   
            scale_color_gradientn(colours = pal,limits=c(0,0.01),
                name="Natural Capital Elasticity",oob = scales::squish) +   
            scale_size_area(max_size = 3, name="Natural Capital \n(fraction of Total Wealth)")
            #theme(plot.margin = unit(c(1,3,1,1), "lines")) 
        #ggsave("Figures/Simulation_10percent_damages.png")

        #df$Ngreen <- df$NforestES+df$NforestT+df$Npa
        all_sim <- data.frame(gdp_change = (df$GDP - df$GDP[df$exp=="nodam"])/df$GDP[df$exp=="nodam"],
        es_change = (df$es - df$es[df$exp=="nodam"])/df$es[df$exp=="nodam"],
         growth_change = df$gy - df$gy[df$exp=="nodam"],
        npv_change =  df$npv - df$npv[df$exp=="nodam"], 
        Nchange =  df$n - df$n[df$exp=="nodam"], 
        Nchange_es =  df$n_es - df$n_es[df$exp=="nodam"], 
        country = df$country, year=df$year, H=df$H,
        N=df$N   ,  Nagg=df$Nagg,
        NforestT = df$NforestT,   
        NforestES = df$NforestES,   Npa=df$Npa,
        K=df$K,          TotalWealth=df$TotalWealth,
        GDP=df$GDP,      Population=df$Population,  
        labor_share=df$labor_share, NatRents=df$NatRents,    
        Continent = df$Continent, 
        n=df$n, n_es=df$n_es, gy=df$gy,          meangy=df$meangy,      
        exp=df$exp    , npv = df$npv  )

       all_sim_mean <- all_sim # aggregate(.~country+year+exp+Continent,data=all_sim, FUN="mean")
    #save(all_sim_mean,file= "all_sim_mean.Rda")
    #load("all_sim_mean.Rda")
    glimpse(all_sim_mean)
    all_sim_mean <- all_sim_mean[which(all_sim_mean$es_change<quantile(all_sim_mean$es_change,0.95,na.rm=TRUE)),]
    all_sim_mean <- all_sim_mean[which(all_sim_mean$es_change>quantile(all_sim_mean$es_change,0.05,na.rm=TRUE)),]
    
    ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year==2099),],
     aes(x=es_change,y=gdp_change,color=Continent))+
       geom_point()+theme_bw()
       
    plot_trajectory <-
     ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" ),],
     aes(x=es_change*100,y=gdp_change*100,color=Continent,group=country))+
     geom_hline(aes(yintercept=0),linetype="dashed")+
     geom_vline(aes(xintercept=0),linetype="dashed")+
       geom_line()+
       geom_point(data = all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year %in% c(2050,2080,2100) ),], aes(x=es_change*100,y=gdp_change*100,color=Continent,shape=factor(year)))+
       theme_bw() +
       theme(legend.position="bottom",legend.box="vertical")+
       
       labs(color="Region",shape="Year")+
       xlab("Ecosystem services change (%)") +
       ylab("GDP change (%)") + ggtitle("")

      leg_trj <-  get_legend(plot_trajectory)

    maxyear <- aggregate(year~country, data=all_sim_mean[which(all_sim_mean$exp=="SSP245" ),],FUN="max")
    maxyear$countrymax <- paste0(maxyear$country,maxyear$year)
    all_sim_mean$countrymax <- paste0(all_sim_mean$country,all_sim_mean$year)

    all_simmax <- all_sim_mean[all_sim_mean$countrymax %in% maxyear$countrymax,]
    
    plot_count_es <- ggplot(all_simmax[which(all_simmax$exp=="SSP245"),],
     aes(x=es_change*100,fill=Continent))+
     geom_histogram()+theme_minimal()+ xlab("")+ylab("count in 2100")

plot_count_es <- ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100),], # & abs(all_sim_mean$es_change)>0.01
     aes(x=es_change*100,fill=Continent))+
     geom_histogram()+theme_minimal()+ xlab("")+ylab("count in 2100")


       plot_count_gdp <- ggplot(all_simmax[which(all_simmax$exp=="SSP245"),],
     aes(x=gdp_change*100,fill=Continent))+xlab('')+ylab("count in 2100")+
     geom_histogram()+theme_minimal()+coord_flip()

plot_count_gdp <- ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100 ),], #& abs(all_sim_mean$gdp_change)>0.01
     aes(x=gdp_change*100,fill=Continent))+xlab('')+ylab("count in 2100")+
     geom_histogram()+theme_minimal()+coord_flip()


    emptyplot <- ggplot()+theme_void()
        
    traj_plot <- ggarrange(ggarrange(plot_trajectory, plot_count_gdp,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="bottom"),
        ncol=1,nrow=2,common.legend=TRUE,heights=c(3,1),legend="bottom",align="hv")
    
    annotate_figure(traj_plot, top = text_grob("Benefits change under SSP2-4.5", 
                face = "bold", size = 14))
    ggsave("Figures/Trajectory_completeHist_meancoeff.png",dpi=300)

     ggarrange(ggarrange(plot_trajectory, plot_count_gdp,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="none"),as_ggplot(leg_trj),
        ncol=1,nrow=3,common.legend=TRUE,heights=c(4,1,1),legend="none",align="hv")
ggsave("Figures/Trajectory_shapesYear_meancoeff.png",dpi=300)
      

       all_sim <- data.frame(gdp_change = (df$GDP - df$GDP[df$exp=="nodam"])/df$GDP[df$exp=="nodam"],
        es_change = (df$es - df$es[df$exp=="nodam"])/df$GDP[df$exp=="nodam"],
         growth_change = df$gy - df$gy[df$exp=="nodam"],
        npv_change =  df$npv - df$npv[df$exp=="nodam"], 
        Nchange =  df$n - df$n[df$exp=="nodam"], 
        Nchange_es =  df$n_es - df$n_es[df$exp=="nodam"], 
        country = df$country, year=df$year, H=df$H,
        N=df$N   ,  Nagg=df$Nagg,
        NforestT = df$NforestT,   
        NforestES = df$NforestES,   Npa=df$Npa,
        K=df$K,          TotalWealth=df$TotalWealth,
        GDP=df$GDP,      Population=df$Population,  
        labor_share=df$labor_share, NatRents=df$NatRents,    
        Continent = df$Continent, 
        n=df$n, n_es=df$n_es, gy=df$gy,          meangy=df$meangy,      
        exp=df$exp    , npv = df$npv  )

       all_sim_mean <- all_sim # aggregate(.~country+year+exp+Continent,data=all_sim, FUN="mean")
       all_sim_mean <- all_sim_mean[,-which(names(all_sim_mean) %in% c("country"))]
       all_sim_mean <- aggregate(.~year+exp+Continent,data=all_sim_mean, FUN="mean")
       
       glimpse(all_sim_mean)
   
   ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" ),])+
      geom_line(aes(x=year,y=gdp_change,color=Continent))+
      geom_line(aes(x=year,y=es_change+gdp_change,color=Continent),linetype="dashed")
      
        all_sim_mean <- all_sim_mean[all_sim_mean$gdp_change< quantile(all_sim_mean$gdp_change,0.99,na.rm=TRUE),]
        all_sim_mean <- all_sim_mean[all_sim_mean$gdp_change> quantile(all_sim_mean$gdp_change,0.01,na.rm=TRUE),]
       
       ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year %in% c(2050,2100)),])+
      geom_violin(aes(x=year,y=gdp_change),scale="width")
      
      +
      geom_line(aes(x=year,y=es_change+gdp_change,color="blue"))


      glimpse(all_sim_mean)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       ggplot(data=all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$country=="MEX"),],
     aes(x=year,y=gdp_change+es_change))+
     geom_line()
     NC_change[which(NC_change$country=="MX"),]

     glimpse(all_sim_mean)

      all_sim <- data.frame(gdp_change = df$GDP - df$GDP[df$exp=="SSP245"], growth_change = df$gy - df$gy[df$exp=="SSP245"],
        npv_change =  df$npv - df$npv[df$exp=="SSP245"], Nchange =  df$n - df$n[df$exp=="SSP245"], country = df$country, year=df$year,
        H=df$H,           N=df$N   ,       Nagg=df$Nagg,
        NforestT = df$NforestT,   NforestES = df$NforestES,   Npa=df$Npa,         K=df$K,          TotalWealth=df$TotalWealth,
        GDP=df$GDP,      Population=df$Population,  labor_share=df$labor_share, NatRents=df$NatRents,    Continent = df$Continent, 
        n=df$n,           gy=df$gy,          meangy=df$meangy,      exp=df$exp , clim=df$clim    , npv = df$npv , dam=df$dam )

        sim_2100 <- all_sim[all_sim$exp %notin% c("10p","nodam","SSP245"),] #& all_sim$year %in% c(2100),]
        meangrowthChange <- aggregate(growth_change~country+year,FUN=mean,data=sim_2100)
        names(meangrowthChange)[3] <- "meangrowthChange"
        sim_2100 <- merge(sim_2100,meangrowthChange,by=c("country","year"))
        glimpse(sim_2100)
        
        hist(sim_2100$Nchange)
        sim_2100$perc_n_change <- sim_2100$Nchange/sim_2100$n
        hist(sim_2100$n)
        
        #sim_2100 <- sim_2100[-which(sim_2100$perc_n_change>quantile(sim_2100$perc_n_change,na.rm=TRUE,0.90)),]
        #sim_2100 <- sim_2100[-which(sim_2100$perc_n_change<quantile(sim_2100$perc_n_change,na.rm=TRUE,0.10)),]
        
        ggplot(data = sim_2100[which(sim_2100$clim==clim_models[1]),], 
            aes(y=log(GDP/Population),x=growth_change*100,group=interaction(country,clim),color=Continent))+
            xlab("GDP growth change (pp)")+ylab("Log GDP per capita")+
            ggtitle("Avoided Damages: GDP growth change resulting from limiting temperature below 2C \n(Conditional NDCs relative to SSP2-4.5)")+
        theme_bw()+
        geom_line()+
        xlim(c(-0.5,0.5))
        ggsave("Figures/growthchange_logGDP.png")


        ggplot(data = sim_2100[which(sim_2100$clim==clim_models[1]),], 
            aes(y=gdp_change/GDP,x=growth_change*100,group=interaction(country,clim),color=Continent))+
            xlab("GDP growth change (pp)")+ylab("Log GDP per capita")+
            ggtitle("GDP growth change under conditional NDCs relative to SSP2-4.5")+
        theme_bw()+
        geom_line()
        scale_x_continuous(trans=ggallin::pseudolog10_trans)
        
        
        geom_point(aes(size=n/TotalWealth))+# theme(legend.position='top')+
        #facet_wrap(~clim)+
        #geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=5,size=2)+
        #scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
        
        scale_x_continuous(trans=ggallin::pseudolog10_trans)+ #library(ggallin)+
        scale_y_continuous(trans=ggallin::pseudolog10_trans)+ #library(ggallin)+
        
        guides(color=guide_legend(title="Continent"),size=guide_legend(title="Natural capital elasticity"),
        shape=guide_legend("Climate change scenario"))
        #ggsave("Figures/NPVchange2100_SSPs_v3.png",dpi=300)
    
      
      
      
      
      
      
      
      
      
      
      
        hist(sim_2100$Nchange)
        sim_2100$perc_n_change <- sim_2100$Nchange/sim_2100$n
        hist(sim_2100$n)
        
        sim_2100 <- sim_2100[-which(sim_2100$perc_n_change>quantile(sim_2100$perc_n_change,na.rm=TRUE,0.99)),]
        #sim_2100 <- sim_2100[-which(sim_2100$perc_n_change<quantile(sim_2100$perc_n_change,na.rm=TRUE,0.10)),]
        
        ggplot(data = sim_2100, 
            aes(x=(npv_change/npv),y=(Nchange/n),color=Continent,shape=exp))+
            #xlab("NPV change (%)")+ylab("")+
        theme_bw()+
        geom_point(aes(size=n/TotalWealth))+
        facet_wrap(~clim)+
        #geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=5,size=2)+
        #scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
        
        scale_x_continuous(trans=ggallin::pseudolog10_trans)+ #library(ggallin)+
        scale_y_continuous(trans=ggallin::pseudolog10_trans)+ #library(ggallin)+
        
        guides(color=guide_legend(title="Continent"),size=guide_legend(title="Natural capital elasticity"),
        shape=guide_legend("Climate change scenario"))
        #ggsave("Figures/GDPchange2100_SSPs_v2.png",dpi=300)



        
        ggplot(data = sim_2100, 
            aes(x=(npv_change/npv),y=(dam),color=Continent,shape=exp))+
            #xlab("NPV change (%)")+ylab("")+
        theme_bw()+
        geom_point(aes(size=n/TotalWealth))+ theme(legend.position='top')+
        #facet_wrap(~clim)+
        #geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=5,size=2)+
        #scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
        
        scale_x_continuous(trans=ggallin::pseudolog10_trans)+ #library(ggallin)+
        scale_y_continuous(trans=ggallin::pseudolog10_trans)+ #library(ggallin)+
        
        guides(color=guide_legend(title="Continent"),size=guide_legend(title="Natural capital elasticity"),
        shape=guide_legend("Climate change scenario"))
        #ggsave("Figures/NPVchange2100_SSPs_v3.png",dpi=300)
    

        ggplot(data = sim_2100, 
            aes(x=(npv_change/npv),y=log(GDP),color=Continent,shape=exp))+
            #xlab("NPV change (%)")+ylab("")+
        theme_bw()+
        geom_point(aes(size=n/TotalWealth))+
        facet_wrap(~clim)+
        #geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=5,size=2)+
        #scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
        
        scale_x_continuous(trans=ggallin::pseudolog10_trans)+ #library(ggallin)+
        scale_y_continuous(trans=ggallin::pseudolog10_trans)+ #library(ggallin)+
        
        guides(color=guide_legend(title="Continent"),size=guide_legend(title="Natural capital elasticity"),
        shape=guide_legend("Climate change scenario"))+
        ggtitle("NPV loss w.r.t. SSP2-4.5")
        #ggsave("Figures/NPVchange2100_SSPs.png",dpi=300)

        
        ggplot(data = sim_2100, 
            aes(x=(MeanGrowthChange),y=log(GDP),color=Continent))+
            #xlab("NPV change (%)")+ylab("")+
        theme_bw()+
        geom_point(aes(size=n/TotalWealth))+ theme(legend.position='top')+
        facet_wrap(~clim)+
        #geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=5,size=2)+
        #scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
        
        scale_x_continuous(trans=ggallin::pseudolog10_trans)+ #library(ggallin)+
        scale_y_continuous(trans=ggallin::pseudolog10_trans)+ #library(ggallin)+
        
        guides(color=guide_legend(title="Continent"),size=guide_legend(title="Natural capital elasticity"),
        shape=guide_legend("Climate change scenario"))

    
    ggplot(data = sim_2100[which(sim_2100$clim==clim_models[1]),], 
            aes(x=MeanGrowthChange*100,y=log(GDP/Population),color=Continent))+
            xlab("Mean GDP growth change (pp)")+ylab("Log GDP per capita")+
        theme_bw()+
        geom_point(aes(size=NatRents*0.01))+ggtitle("Change in mean GDP growth (2020-2100) under SSP3-7.0 w.r.t. SSP2-4.5")+
        guides(color=guide_legend(title="Region"),size=guide_legend(title="Natural capital\n elasticity"))
        ggsave("Figures/Growthchange_SSPs.png",dpi=300)
        
        + theme(legend.position='top')+
        
        #geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=5,size=2)+
        #scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
        
        scale_x_continuous(trans=ggallin::pseudolog10_trans)+ #library(ggallin)+
        guides(color=guide_legend(title="Continent"),size=guide_legend(title="Natural capital elasticity"),
        shape=guide_legend("Climate change scenario"))
        #ggsave("Figures/GDPchange2100_SSPs_v3.png",dpi=300)
 

        sim_2100 <- all_sim[all_sim$exp %notin% c("10p","nodam") & all_sim$year %in% c(2100),]
        ggplot(data = sim_2100, 
            aes(x=growth_change,y=log(GDP/Population),color=Continent,shape=exp))+
            xlab("GDP change in 2100 (pp)")+ylab("Log GDP per capita")+
        theme_bw()+
        geom_point(aes(size=NatRents*0.01))+ theme(legend.position='top')+
        #geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=5,size=2)+
        #scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
        
        scale_x_continuous(trans=ggallin::pseudolog10_trans)+ #library(ggallin)+
        guides(color=guide_legend(title="Continent"),size=guide_legend(title="Natural capital elasticity"),
        shape=guide_legend("Climate change scenario"))


































        ggplot(data = all_sim[all_sim$exp %notin% c("10p","nodam") & all_sim$year %in% c(2100),], 
            aes(x=(gdp_change/GDP),y=log(GDP/Population),color=exp))+
        theme_bw()+
        geom_point()+
        scale_x_continuous(trans=ggallin::pseudolog10_trans) #library(ggallin)



























































































    # Fixed investments on manufactured capital (start)

        wdata2017 <- wealth_data[which(wealth_data$year==2017),]
        wdata2017$difN <- (wdata2017$Ngreen - wdata1995$Ngreen)/wdata1995$Ngreen
        wdata2017$changeN <- "Gain"
        wdata2017[wdata2017$difN<0,]$changeN <- "Loss"
        #wdata2017 <- wdata2017[-which(is.na(wdata2017$GDP)),]
        glimpse(wdata2017)
        
        glimpse(wdata2017$labor_share)
        wdata2017$year <- as.numeric(as.character(wdata2017$year))
        wealth_sim <- wdata2017[,which(names(wdata2017) %in% c("countrycode",
        "year","H","N","Nagg","NforestT","NforestES","Npa","K","TotalWealth","GDP","Population","NatRents","Continent","labor_share"))]
        wealth_sim$n <- 0 #net natural capital
        wealth_sim$gy <- 0 #output growth
        wealth_sim$meangy <- 0 #mean output growth
        wealth_sim$exp <- "exp1" #experiment name
        country <- levels(factor(wealth_sim$countrycode))
        for (i in 1:length(country)){
          s=0.2 #fixed savings rate
          d=0.01 #depreciation rate
          t=25 #number of years of simulation
          a1 = wealth_sim$labor_share[wealth_sim$countrycode==country[i]] *0.01 #wealth_sim[wealth_sim$countrycode==country[i],] 
          a3 = wealth_sim$NatRents[wealth_sim$countrycode==country[i]] *0.01
          if(is.na(a1)){a1 <- L_elas_world}
          if (is.na(a3)){a3 <- 0}
          a2 = 1-a1-a3
          if (a2<0) {a2 <- 0.001}
          y0 = wealth_sim$GDP[wealth_sim$countrycode==country[i]]  
          K0 = wealth_sim$K[wealth_sim$countrycode==country[i]] 
          T0 = wealth_sim$TotalWealth[wealth_sim$countrycode==country[i]]
          h0 = wealth_sim$H[wealth_sim$countrycode==country[i]]
          n0 = wealth_sim$NforestT[wealth_sim$countrycode==country[i]]  
          #wealth_sim$NforestES[wealth_sim$countrycode==country[i]] 
          #wealth_sim$Nagg[wealth_sim$countrycode==country[i]] + 
          #wealth_sim$Npa[wealth_sim$countrycode==country[i]]
          #n0 = wealth_sim$N[wealth_sim$countrycode==country[i]]
          yt=numeric(length=t);yt[1]=y0
          kt=numeric(length=t);kt[1]=K0
          Tt=numeric(length=t);Tt[1]=T0
          pi1 <- y0/(h0^a1*K0^a2*n0^a3)    #normalizing param
          year = 1:25
          df <- wealth_sim[0,]

          nt=numeric(length=t);nt[1]=n0
          yt=numeric(length=t);yt[1]=y0
          kt=numeric(length=t);kt[1]=K0
            for(j in 2:t){
              kt[j]=kt[j-1]*(1-d)+(yt[j-1])*s
              nt[j] = nt[j-1]
              yt[j]=pi1*(h0^a1)*(kt[j]^(a2))*nt[1]^a3
            }
          gy <- (yt - lag(yt))/lag(yt) #output growth
          meangy <- mean(gy[3:length(gy)]) #mean output growth 
          
          df <- rbind(df,data.frame(countrycode=country[i], year=year+2017, H=h0, N=wealth_sim$N[wealth_sim$countrycode==country[i]], 
          Nagg=wealth_sim$Nagg[wealth_sim$countrycode==country[i]],
          NforestT=wealth_sim$NforestT[wealth_sim$countrycode==country[i]],
          NforestES=wealth_sim$NforestES[wealth_sim$countrycode==country[i]],
          Npa=wealth_sim$Npa[wealth_sim$countrycode==country[i]],
          K=kt,
          TotalWealth=wealth_sim$TotalWealth[wealth_sim$countrycode==country[i]], 
          GDP=yt,
          Population=wealth_sim$Population[wealth_sim$countrycode==country[i]],
          labor_share=wealth_sim$labor_share[wealth_sim$countrycode==country[i]],
          NatRents=wealth_sim$NatRents[wealth_sim$countrycode==country[i]],
          Continent=wealth_sim$Continent[wealth_sim$countrycode==country[i]],
          n=nt,
          gy = gy, meangy = meangy,exp="exp1")) 
          wealth_sim <- rbind(wealth_sim,df)
        }

        wealth_sim1 <- wealth_sim
        ggplot(data= wealth_sim1[wealth_sim1$Continent == "Americas",], 
            aes(x=year,y=log(GDP/Population),group=countrycode,color=countrycode))+
        theme_bw()+
        geom_line()  +
        xlab("Year") + 
        ylab("Log GDP per capita") + 
        xlim(c(2022,2040))+
        ggtitle("Simulation with Fixed Investments in Manufactured Capital")+
        geom_text(data = subset(wealth_sim1[wealth_sim1$Continent == "Americas",], year == "2035"), aes(label = countrycode, colour = countrycode, x = 2038, y = log(GDP/Population)), hjust = -.1) +
            scale_colour_discrete(guide = 'none')  +    
            theme(plot.margin = unit(c(1,3,1,1), "lines")) 


        ggplot(data= wealth_sim1, aes(y=log(GDP/Population),x=NatRents*0.01,group=countrycode,color=Continent))+
        theme_bw()+
        geom_point(aes(size=K/(K+H+N)))  +
        xlab("Production Elasticity to Natural Capital") + 
        ylab("Log GDP per capita") + 
        scale_colour_discrete(name="Continent")+
        #scale_shape_discrete(name="Natatural Capital change\nsince 1995")+
        scale_size_continuous(name="Manufactured Capital \n(Fraction of Wealth)")

        wealth_sim2 <- wealth_sim1[is.finite(wealth_sim1$meangy),]
        wealth_sim2 <-  wealth_sim2[which(wealth_sim2$year==2022),]
        wealth_sim2$a3 <- (100-wealth_sim2$labor_share-wealth_sim2$NatRents)*.01
        Wealth_out <- wealth_sim2[which(wealth_sim2$meangy > quantile(wealth_sim2$meangy,0.95) | wealth_sim2$meangy < quantile(wealth_sim2$meangy,0.05)),]

        ggplot(data=wealth_sim2, aes(x=meangy,y=a3,group=countrycode,color=Continent))+
        theme_bw()+
        geom_point(aes(size=(NforestT+NforestES+Npa)/(TotalWealth)))  +
        xlab("Average Economic Growth Rate") + 
        ylab("Production Elasticity to Manufactured Capital") + 
        scale_colour_discrete(name="Continent")+
        #scale_shape_discrete(name="Natatural Capital change\nsince 1995")+
        scale_size_continuous(name="Natural Capital \n(Fraction of Wealth)") +
        geom_text(data=Wealth_out,aes(x=meangy+0.0017,y=a3+0.05,label=countrycode))
    # Fixed Investments on Manufactured Capital











        ggplot(data= wealth_sim3, aes(x=meangy,y=NatRents,group=countrycode,color=Continent))+
        theme_bw()+
        geom_point(aes(size=K/(K+H+N)))  +
        xlab("Average Economic Growth Rate") + 
        ylab("Production Elasticity to Natural Capital") + 
        scale_colour_discrete(name="Continent")+
        #scale_shape_discrete(name="Natatural Capital change\nsince 1995")+
        scale_size_continuous(name="Manufactured Capital \n(Fraction of Wealth)")



        wealth_sim3 <- wealth_sim3[is.finite(wealth_sim3$meangy),]
        wealth_sim3$a3 <- (100-wealth_sim3$labor_share-wealth_sim3$NatRents)*.01
        wealth_sim3$dif <- (wealth_sim3$GDP - wealth_sim2$GDP)/wealth_sim2$GDP
        ggplot(data= wealth_sim3[wealth_sim3$year == 2016,], aes(y=dif,x=NatRents*.01,group=interaction(countrycode, exp),color=exp))+
        theme_bw()+
        geom_point() +
        xlab("Production Elasticity to Natural Capital") + 
        ylab("Fraction Damaged of Economic Output") 


        wealth_sim3$difg <- (wealth_sim3$meangy - wealth_sim2$meangy)*100
        ggplot(data= wealth_sim3[wealth_sim3$year == 2016,], aes(y=difg,x=NatRents*.01,label=countrycode,group=interaction(countrycode, exp)))+
        theme_bw()+
        geom_text(aes(size=(Nforest+Npa+Nagg)/TotalWealth, color = labor_share)) +
        xlab("Production Elasticity to Natural Capital") + 
        ylab("Change in Growth (pp)") 

        glimpse(wealth_sim3)
        wealth_sim3$difg <- (wealth_sim3$meangy - wealth_sim2$meangy)*100

        wealth_sim3$difn <- 100*(wealth_sim3$n - wealth_sim2$n)/wealth_sim2$n
        ggplot(data=wealth_sim3, aes(x=year,y=difn,group=countrycode))+
        geom_line()

        
        wealth_sim3$difgdp <- 100*(wealth_sim3$GDP - wealth_sim2$GDP)/wealth_sim2$GDP
        ggplot(data= wealth_sim3[wealth_sim3$Continent == "Americas",], 
            aes(x=year,y=difgdp,group=countrycode,color=countrycode))+
        theme_bw()+
        geom_line()  +
        xlab("Year") + 
        ylab("GDP change w.r.t. baseline(pp)") + 
        xlim(c(2015,2023))+
        ggtitle("Simulation with Fixed Investments in Manufactured Capital")+
        geom_text(data = subset(wealth_sim3[wealth_sim3$Continent == "Americas",], year == "2018"), aes(label = countrycode, colour = countrycode, x = 2020, y = difgdp), hjust = -.1) +
            scale_colour_discrete(guide = 'none')  +    
            theme(plot.margin = unit(c(1,3,1,1), "lines")) 

        ggplot(data= subset(wealth_sim3[wealth_sim3$Continent == "Americas",], year == "2018"), 
            aes(x=NatRents*0.01,y=difgdp,group=countrycode,color=countrycode))+
        theme_bw()+
        #geom_point()  +
        xlab("Production elasticity to NC") + 
        ylab("GDP change in the year of the impact (pp)") + 
        ggtitle("Simulation with Fixed Investments in Manufactured Capital")+
        geom_text(data = subset(wealth_sim3[wealth_sim3$Continent == "Americas",], year == "2018"), aes(label = countrycode, colour = countrycode, x = NatRents*0.01, y = difgdp), hjust = -.1) +
            scale_colour_discrete(guide = 'none')  +    
            theme(plot.margin = unit(c(1,3,1,1), "lines")) 

        ggplot(data= subset(wealth_sim3[wealth_sim3$countrycode %in% c("NIC","HTI"),]), 
            aes(x=year,y=log(GDP),group=countrycode,color=countrycode))+
        theme_bw()+
        geom_line()  

        xlab("Production elasticity to NC") + 
        ylab("GDP change in the year of the impact (pp)") + 
        ggtitle("Simulation with Fixed Investments in Manufactured Capital")+
        geom_text(data = subset(wealth_sim3[wealth_sim3$Continent == "Americas",], year == "2018"), aes(label = countrycode, colour = countrycode, x = NatRents*0.01, y = difgdp), hjust = -.1) +
            scale_colour_discrete(guide = 'none')  +    
            theme(plot.margin = unit(c(1,3,1,1), "lines")) 
                               
    # Fixed Investments on Manufactured Capital + NatCap damages (exp2)

    # Using SSPs

        wdata2017$year <- as.numeric(as.character(wdata2017$year))
        wealth_sim <- wdata2017[,which(names(wdata2017) %in% c("countrycode",
        "year","H","N","Nagg","NforestT","NforestES","Npa","K","TotalWealth","GDP","Population","NatRents","Continent","labor_share"))]
        wealth_sim$n <- 0 #net natural capital
        wealth_sim$gy <- 0 #output growth
        wealth_sim$meangy <- 0 #mean output growth
        wealth_sim$exp <- "exp2" #experiment name
        country <- levels(factor(wealth_sim$countrycode))
        #lm(c(0,Tshort,Tmed,Tlong)~c(0,10,30,70)) 0.03 per year
        glimpse(NC_change)  
        

        
    for (i in 1:length(country)){
        s=0.2 #fixed savings rate
        d=0.1 #depreciation rate
        t=25 #number of years of simulation
        a1 = wealth_sim$labor_share[wealth_sim$countrycode==country[i]] *0.01
        a3 = wealth_sim$NatRents[wealth_sim$countrycode==country[i]] *0.01
        
        if(is.na(a1)){a1 <- L_elas_world}
          if (is.na(a3)){a3 <- 0}
          a2 = 1-a1-a3
          if (a2<0) {
          a2 <- 0.0001
          a3 <- a3-a2}
        y0 = wealth_sim$GDP[wealth_sim$countrycode==country[i]]  
        K0 = wealth_sim$K[wealth_sim$countrycode==country[i]] 
        T0 = wealth_sim$TotalWealth[wealth_sim$countrycode==country[i]]
        h0 = wealth_sim$H[wealth_sim$countrycode==country[i]]
        Nforest = wealth_sim$NforestT[wealth_sim$countrycode==country[i]] + wealth_sim$NforestES[wealth_sim$countrycode==country[i]]
        Nagg = wealth_sim$Nagg[wealth_sim$countrycode==country[i]]
        Npa = wealth_sim$Npa[wealth_sim$countrycode==country[i]]

        n0 = Nforest  + Npa
        
        #n0 = wealth_sim$N[wealth_sim$countrycode==country[i]]
        yt=numeric(length=t);yt[1]=y0
        kt=numeric(length=t);kt[1]=K0
        Tt=numeric(length=t);Tt[1]=T0
        pi1 <- y0/(h0^a1*K0^a2*n0^a3)    #normalizing param
        year = 1:25
        df <- wealth_sim[0,]

            nt=numeric(length=t);nt[1]=n0
            yt=numeric(length=t);yt[1]=y0
            kt=numeric(length=t);kt[1]=K0
            for(j in 2:t){
            kt[j]=kt[j-1]*(1-0)+(yt[j-1])*s
            if(j>6){
                nt[j] = nt[j-1]*(1-
                    NC_change$DamageEstimateT2[NC_change$countryiso3==country[i]][1]*
                    (Temp_ssps[j+168,2]-Temp_ssps[168,2])^2)
            
            }else{
                nt[j] = nt[j-1]
                
            }
            yt[j]=pi1*(h0^a1)*(kt[j]^(a2))*nt[j]^a3
            }
            gy <- (yt - lag(yt))/lag(yt) #output growth
            meangy <- mean(gy[3:length(gy)]) #mean output growth 
            
            df <- rbind(df,data.frame(countrycode=country[i], year=year+2017, H=h0, N=wealth_sim$N[wealth_sim$countrycode==country[i]], 
            Nagg=Nagg,
            NforestT=wealth_sim$NforestT[wealth_sim$countrycode==country[i]],
            NforestES=wealth_sim$NforestES[wealth_sim$countrycode==country[i]],
            Npa=Npa,
            K=kt,
            TotalWealth=wealth_sim$TotalWealth[wealth_sim$countrycode==country[i]], 
            GDP=yt,
            Population=wealth_sim$Population[wealth_sim$countrycode==country[i]],
            labor_share=wealth_sim$labor_share[wealth_sim$countrycode==country[i]],
            NatRents=wealth_sim$NatRents[wealth_sim$countrycode==country[i]],
            Continent=wealth_sim$Continent[wealth_sim$countrycode==country[i]],n=nt,
            gy = gy, meangy = meangy,exp="exp2")) 
        wealth_sim <- rbind(wealth_sim,df)
        }
        glimpse(wealth_sim)


        wealth_sim3 <- wealth_sim[wealth_sim$year != 2014 & wealth_sim$year != 2015,]

        ggplot(data= wealth_sim3[wealth_sim3$Continent == "Americas",], 
            aes(x=year,y=gy*100,group=countrycode,color=countrycode))+
        theme_bw()+
        geom_line()  +
        xlab("Year") + 
        ylab("GDP growth (pp)") + 
        xlim(c(2020,2040))+
        ggtitle("Simulation with Natural capital damaged on 2023")+
        #geom_text(data = subset(wealth_sim1[wealth_sim1$Continent == "Americas",], year == "2035"), 
         #   aes(label = countrycode, colour = countrycode, x = 2038, y = log(GDP/Population)), hjust = -.1) +
            scale_colour_discrete(guide = 'none')  +    
            theme(plot.margin = unit(c(1,3,1,1), "lines")) 
        ggsave("Simulation_PFTdamages_SSP270.png")

        hist(wealth_sim3$NatRents)
        ggplot(data= wealth_sim3[wealth_sim3$Continent == "Americas",], 
            aes(x=year,y=gy*100,group=countrycode))+
        theme_bw()+
        geom_line(aes(color=NatRents*0.01, size=(NforestT+NforestES+Npa)/TotalWealth))+                   #(NforestT+NforestES+Npa)/TotalWealth)))  +
        xlab("Year") + 
        ylab("GDP growth (pp)") + 
        xlim(c(2020,2040))+
        ggtitle("Simulation with Natural capital damaged on 2023")+
        #geom_text(data = subset(wealth_sim1[wealth_sim1$Continent == "Americas",], year == "2035"), 
         #   aes(label = countrycode, colour = countrycode, x = 2038, y = log(GDP/Population)), hjust = -.1) +
            #scale_colour_discrete(guide = 'none')  +   
            scale_color_gradientn(colours = pal,limits=c(0,0.01),
                name="Natural Capital Elasticity",oob = scales::squish) +   
            scale_size_area(max_size = 3, name="Natural Capital \n(fraction of Total Wealth)")
            #theme(plot.margin = unit(c(1,3,1,1), "lines")) 
        ggsave("Figures/Simulation_PFTdamages_ssp270.png")

        ggplot(data= wealth_sim3[wealth_sim3$Continent == "Americas",], 
            aes(x=year,y=log(GDP),group=countrycode))+
        theme_bw()+
        geom_line(aes(color=NatRents*0.01, size=(NforestT+NforestES+Npa)/TotalWealth))+                   #(NforestT+NforestES+Npa)/TotalWealth)))  +
        xlab("Year") + 
        ylab("GDP growth (pp)") + 
        xlim(c(2020,2040))+
        ggtitle("Simulation with Natural capital damaged on 2023")+
        #geom_text(data = subset(wealth_sim1[wealth_sim1$Continent == "Americas",], year == "2035"), 
         #   aes(label = countrycode, colour = countrycode, x = 2038, y = log(GDP/Population)), hjust = -.1) +
            #scale_colour_discrete(guide = 'none')  +   
            scale_color_gradientn(colours = pal,limits=c(0,0.01),
                name="Natural Capital Elasticity",oob = scales::squish) +   
            scale_size_area(max_size = 3, name="Natural Capital \n(fraction of Total Wealth)")
            #theme(plot.margin = unit(c(1,3,1,1), "lines")) 
        ggsave("Figures/Simulation_PFTdamages_ssp270.png")



    # USING SSPs
     
        world <- spData::world
        isos <- world$iso_a2
        world$elasH <- NA
        world$elasK <- NA
        world$elasN <- NA
        world <- world[-which(is.na(world$iso_a2)),]
        for (i in 1:length(isos)){
        if(sum(w$iso_a2==isos[i]) == 0){next} 
        we <- w[which(w$iso_a2==isos[i]),]
        reg_elas <- felm(logGDP~logK+logH+log(N)+log(Foreign)+year|0|0|0,data=we)
        summary(reg_elas)

        
        world$changeN[world$iso_a2==isos[i]] <- wdata2018$difN[wdata2018$iso_a2==isos[i]]
        }
        
    # Sim of Econ with Clim Ch (end)
    