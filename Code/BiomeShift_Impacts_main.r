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
        dim(table(wealth$countrycode))

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
        
        wealth <- merge(Wealth,Gdp, by = c("countrycode","year"),all=TRUE)
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

        glimpse(wealth_data)
        which(wealth_data$countryname.y == "Alegria")
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
    
    wdata2018o <- wdata2018[which(wdata2018$Nprov > quantile(wdata2018$Nprov,0.06,na.rm=TRUE)),]
    wdata2018o <- wdata2018o[which(wdata2018o$Nnonprov > quantile(wdata2018o$Nnonprov,0.05,na.rm=TRUE)),]
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
        memory.limit(size=250000)
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
        
        
        sw <- as_Spatial(st_cast(world))
        raster_isos <- raster::rasterize(sw[1],v1)
        isos_xy <- as.data.frame(raster_isos, xy=TRUE)
        names(isos_xy)[3] <- "countrycode"
        glimpse(isos_xy)
        isos_xy$xy <- paste0(isos_xy$x,isos_xy$y)
        area <- as.data.frame(area(raster_isos), xy=TRUE)   
        glimpse(area)
        isos_xy$area <- area[,3]

        a_h$xy <- paste0(a_h$x,a_h$y)
        a_h <- merge(a_h,isos_xy,by="xy")
        save(a_h,file="a_h.Rda")
        load("a_h.Rda")
        a_h$areaPFT <- a_h$Percent * a_h$area
        PFTs11_Clims4_horizons3 <- a_h
        a_h$x <- a_h$x.x
        a_h$y <- a_h$y.x
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
    
    #Get PFTs by latitude (start)
            a_h <- PFTs11_Clims4_horizons3
            a_h$horizon <- factor(a_h$horizon,levels=  c("Present","Near-term (2021-2040)","Mid-term (2041-2060)","Long-term (2081-2099)"))
            a_h$Latitude <- abs(a_h$y)
            PFT_movement <- a_h %>%
            mutate(dominant_pft=fct_reorder(.f = dominant_pft, .x = Latitude, .fun = mean))
            glimpse(a_h)
            glimpse(PFT_movement)
                

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

            

            
            

            PFT_movement_y_present <- PFT_movement_y[which(PFT_movement_y$horizon=="Present"),]
            glimpse(PFT_movement_y_present)

            PFT_movement_y_future <- PFT_movement_y[which(PFT_movement_y$horizon=="Long-term (2081-2099)"),]
            glimpse(PFT_movement_y_future)

            PFT_movement_y_change <- PFT_movement_y_present
            PFT_movement_y_change$Percent <- PFT_movement_y_future$Percent - PFT_movement_y_present$Percent

            glimpse(PFT_movement_y_change)
        
        save(PFT_movement,file="Data/PFT_movement.Rda")
        save(PFT_movement_y,file="Data/PFT_movement_y.Rda")
        save(PFT_movement_y_change,file="Data/PFT_movement_y_change.Rda")

        PFT_change_horizon <- aggregate(areaPFT ~ horizon + PFT, data=PFT_movement,FUN="sum")
        
        areachange <- PFT_change_horizon$areaPFT[which(PFT_change_horizon$horizon=="Long-term (2081-2099)")]- PFT_change_horizon$areaPFT[which(PFT_change_horizon$horizon=="Present")]
        name <-  PFT_change_horizon$PFT[which(PFT_change_horizon$horizon=="Long-term (2081-2099)")] 
        ac <- data.frame(name,areachange)         
        ac$name[which(ac$areachange==max(ac$areachange) )]

    #Get PFTs by latitude (end)

    load("Data/PFT_movement_y_change.Rda")
    load("Data/PFT_movement_y.Rda")
    load("Data/PFT_movement.Rda")

    #Plot PFTs by latitude (start)
            Present_dist_col <- ggplot(PFT_movement_y[which(PFT_movement_y$horizon=="Present"),])+
            geom_col(aes(y,Percent,fill=PFT))+
            scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
            #scale_fill_brewer(palette = "Paired") +
            theme_minimal() + xlab("Latitude (Degrees)") + ylab("Average pixel cover (%)")+
            ggtitle("Present") + guides(fill=guide_legend(title="Biome")) + coord_flip()
            
            pft_mov_change <- ggplot(PFT_movement_y_change)+
            geom_col(aes(y,Percent,fill=PFT))+
            geom_hline(yintercept=0)+
            ggtitle("Long-term (2081-2099)")+
            theme_minimal() + xlab("") + 
            ylab("Cover change (pp)") +
            scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
            #scale_fill_brewer(palette = "Paired") +
            ylim(-30,30)+ 
            guides(fill=guide_legend(title="Biome")) + coord_flip()

            ggarrange(Present_dist_col,pft_mov_change,ncol=2,nrow=1,common.legend=TRUE,legend="right")
            ggsave("Biome_change_latitude_color2.png",dpi=300)
            #ggsave("Biome_change_latitude_vert.png",dpi=300)
    #Plot PFTs by latitude (end)

    #Get PFTs by country by time horizon (start)

            glimpse(PFT_movement)
            PFT_movement$country <- PFT_movement$countrycode
            present_country_area <- aggregate(area~PFT+country+clim,FUN=sum,data=PFT_movement[which(PFT_movement$horizon=="Present"),])
            present_country_area <- merge(present_country_area,world[,c(1,3)],by.x="country",by.y="iso_a2")
            ggplot(present_country_area, aes(x=continent,y=area,fill=PFT))+
            scale_fill_brewer(palette = "Paired",direction=1)+
                geom_bar(stat = "identity", width = .9)
           
            present_country_pft <- aggregate(areaPFT~PFT+country+clim,FUN=sum,data=PFT_movement[which(PFT_movement$horizon=="Present"),])
            glimpse(present_country_pft)
            #present_country_pft$countrycode <- countrycode(present_country_pft$countrycode, origin = 'iso2c', destination = 'iso3c')
            present_country_pft <- merge(present_country_pft,world[,c(1,3)],by.x="country",by.y="iso_a2")
            
            glimpse(present_country_pft[which(present_country_pft$continent=="Europe"),])
            
            ggplot(present_country_pft, aes(x=continent,y=areaPFT,fill=PFT))+
            scale_fill_brewer(palette = "Paired",direction=1)+
                geom_bar(stat = "identity", width = .9)


            #PFT_area_country <- aggregate(areaPFT~PFT+country+clim,FUN=sum,data=present_country_pft)
            #present_country_pft <- aggregate(areaPFT~PFT+country+horizon,FUN=sum,data=a_h)
            #present_country_pft[which(present_country_pft$horizon=="Present")]
            #glimpse(present_country_pft) 
            #levels(factor(present_country_pft$clim))
            
            PFT_area_country <- dcast(present_country_pft,country~PFT+clim, value.var="areaPFT",FUN=sum)
            glimpse(PFT_area_country)
            names(PFT_area_country)[2:45] <- paste("PFT.",names(PFT_area_country)[2:45],sep="")
            PFT_area_country$countrycode <- countrycode(PFT_area_country$country, origin = 'iso2c', destination = 'iso3c')
            #names(PFT_area_country)[2:12] <- paste("PFT.",names(PFT_area_country)[2:12],sep="")
            glimpse(PFT_area_country)
            
            
            wealth2018_pft <- merge(wdata2018,PFT_area_country,by="countrycode")
            glimpse(wealth2018_pft)
            wealth2018_pft_all <- wealth2018_pft

            
            near_country_pft <- aggregate(areaPFT~PFT+country+clim,FUN=sum,data=PFT_movement[which(PFT_movement$horizon=="Near-term (2021-2040)"),])
            PFT_area_country_near <- dcast(near_country_pft,country~PFT+clim, value.var="areaPFT")
            PFT_area_country_near$countrycode <- countrycode(PFT_area_country_near$country, origin = 'iso2c', destination = 'iso3c')
            names(PFT_area_country_near)[2:45] <- paste("PFT.",names(PFT_area_country_near)[2:45],sep="")
            glimpse(PFT_area_country_near)

            PFT_area_country_near_all <- PFT_area_country_near
            
            mid_country_pft <- aggregate(areaPFT~PFT+country+clim,FUN=sum,data=PFT_movement[which(PFT_movement$horizon=="Mid-term (2041-2060)"),])
            PFT_area_country_mid <- dcast(mid_country_pft,country~PFT+clim)
            PFT_area_country_mid$countrycode <- countrycode(PFT_area_country_mid$country, origin = 'iso2c', destination = 'iso3c')
            names(PFT_area_country_mid)[2:45] <- paste("PFT.",names(PFT_area_country_mid)[2:45],sep="")
            glimpse(PFT_area_country_mid)
            PFT_area_country_mid_all <- PFT_area_country_mid

            long_country_pft <- aggregate(areaPFT~PFT+country+clim,FUN=sum,data=PFT_movement[which(PFT_movement$horizon=="Long-term (2081-2099)"),])
            PFT_area_country_long <- dcast(long_country_pft,country~PFT+clim)
            PFT_area_country_long$countrycode <- countrycode(PFT_area_country_long$country, origin = 'iso2c', destination = 'iso3c')
            names(PFT_area_country_long)[2:45] <- paste("PFT.",names(PFT_area_country_long)[2:45],sep="")
            glimpse(PFT_area_country_long)
            PFT_area_country_long_all <- PFT_area_country_long

            save(PFT_area_country_long_all,file="Data/PFT_area_country_long_all.Rda")
            save(PFT_area_country_mid_all,file="Data/PFT_area_country_mid_all.Rda")
            save(PFT_area_country_near_all,file="Data/PFT_area_country_near_all.Rda")
            save(wealth2018_pft_all,file="Data/wealth2018_pft_all.Rda")
    
    #Get PFTs by country by time horizon (end)
            load("Data/PFT_area_country_long_all.Rda")
            load("Data/PFT_area_country_mid_all.Rda")
            load("Data/PFT_area_country_near_all.Rda")
            load("Data/wealth2018_pft_all.Rda")




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
                summary(mod)
                library("stargazer")
                stargazer(mod,type="html",out="value-area.html")
                #ggsave("Valuechange_byArea.png",dpi=300)
    #Plot PFT by latitude (end)
        save(a_h,file=".Rda")
    
    #Calculate NatCap loss based on PFT (start)   
   
        a_h <- PFT_movement
        a_h <- a_h[which(a_h$Percent>0),]
        TypeES <- c("All","Provisioning","Non-provisioning")
            firstcountry =1
        for(j in 1:177){
            
                #for(typeESi in 1:3){
                    for (climi in 1:length(clim_models)){
                        PFT_movement <- a_h[which(a_h$clim==clim_models[climi]),]
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
                            if(length(which(wealth2018_pft$country==isos[j]))==0){
                            print(paste("No World Bank Data for",isos[j]))
                            next
                        }
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

                    

                    PFT_movement2 <- PFT_movement2 %>%
                    mutate(PFT=fct_reorder(.f = PFT, .x = -Percent, .fun = mean))

                    if(dim(PFT_movement2)[1]==0){next}


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

                


                        p <- levels(PFT_movement2$PFT_code)
                        ps <- paste0("PFT.",p)
                        
                        areasPFTs1 <- aggregate(areaPFT ~ horizon + PFT + country+PFT_code, data = PFT_movement2, FUN = sum)
                        
                        areasPFTs2 <- areasPFTs1[which(areasPFTs1$PFT_code %notin% c("c3g","c4g")),]

                        q25mx<-areasPFTs2$PFT_code[which(areasPFTs2$areaPFT==max(areasPFTs2$areaPFT))]
                        
                        
                        
                        extra_ind_vars <- c("pct_covered","logGDP")
                        ps_all <- paste(c(ps,extra_ind_vars),collapse="+")
                       
                        for(typeESi in 1:3){

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

                        #glimpse(es_ac_notFood)
                      
                        if(dim(subset_es)[2] < 5){
                            print("subset less than 5 observations")
                            next
                        }
                      
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
                      
                        
                        mt <- trf[which.min(trf[,2]), 1]
                        
                        Results_rf <- randomForest(ind_vars, subset_es$logESValue, importance = TRUE,tree = TRUE, mtry =mt, ntree = 200)
                        
                        pft_pct_pred = aggregate(Percent  ~ horizon + PFT_code, data = PFT_movement2, FUN = "mean")

                     
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
                        

                      areasPFTs_database <- wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) %in% eff_pft$variable)]
                        sum_areasPFTs <- sum(wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) %in% eff_pft$variable)]*eff_pft$importance[order(eff_pft$variable,names(areasPFTs_database))],na.rm=TRUE)
                        
                        
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



                        NaturalCapital <- aggregate(ES_total~horizon,data=PFT_ES,FUN="sum")
                        NaturalCapital$N <- NaturalCapital$ES_total/r
                        NaturalCapital_areachange <- aggregate(ES_total_areachange~horizon,data=PFT_ES,FUN="sum")
                        NaturalCapital$N_areachange <- NaturalCapital_areachange$ES_total_areachange/r
                        NaturalCapital$country <- isos[j]
                        NaturalCapital$GDP <- wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) == "GDP")]
                        NaturalCapital$TotalWealth <- wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) == "TotalWealth")]
                        NaturalCapital$NatRents <- wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) == "NatRents")]
                        #temphorizon <- data.frame(horizon = c("Present", "Near-term (2021-2040)","Mid-term (2041-2060)","Long-term (2081-2099)"),
                                        #temp=c(0,Tshort,Tmed,Tlong))
                        #NaturalCapital <- merge(NaturalCapital,temphorizon,by="horizon")
                        if((dim(NaturalCapital)[1]<4)){next}
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
                        }

                        print(paste("country =",j,"/177.  Clim =",climi,"/4"))
                }
                
        }

    #Calculate NatCap based on PFT movement

    save(NC_change,file="NC_change_controllingGDPandArea_es_ac_notFood_adjustedImportance_allClimProjections.Rda")
    save(PFT_ES_all,file="PFT_ES_all_controllingGDPandArea_es_ac_notFood_adjustedImportance_allClimProjections.Rda")
   
    
    load("NC_change_controllingGDPandArea_es_ac_notFood_adjustedImportance_allClimProjections.Rda")
    load("PFT_ES_all_controllingGDPandArea_es_ac_notFood_adjustedImportance_allClimProjections.Rda")
    ## ES values per hectare

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
                        "Tropical broadleaved \n  evergreen",
                        "Tropical broadleaved \n  evergreen (shade-intolerant)",
                        "Tropical Grass",
                        "Tropical broadleaved \n  raingreen" ,
                        "Temperate  broadleaved \n  evergreen",
                        "Temperate broadleaved \n summergreen",
                        "Boreal/Temperate grass",
                        "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
                        "Boreal evergreen \n needleleaved (shade-intolerant)",
                        "Boreal evergreen \n needleleaved",
                        "Boreal summergreen \n needleleaved" ))
        glimpse(PFT_ES_all)
        ggplot(PFT_ES_all[which(PFT_ES_all$horizon=="Present" & PFT_ES_all$TypeES !="All"),]) +
        #ggplot(PFT_ES_all) +
        geom_boxplot(outlier.shape = NA,aes(x=(ES_rel),y=PFT,fill=TypeES,middle=mean((ES_rel)))) +
        theme_bw() + coord_cartesian(xlim=c(0,300))
        ggsave("Relative_Values.png",dpi=300)
    
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
        
        PFT_ES_all$area[which(PFT_ES_all$horizon=="Present" & PFT_ES_all$TypeES =="Non-provisioning")]
        ggplot(PFT_ES_all[which(PFT_ES_all$horizon=="Present" & PFT_ES_all$TypeES =="Non-provisioning"),],
                    aes(x = continent, y = (area), fill= PFT)) + 
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

        # PFT_ES_all_mean <- aggregate(ES_tot~TypeES+PFT+horizon+continent, FUN="sum", data=PFT_ES_all)
        # PFT_ES_all_mean2 <- aggregate(GDP~TypeES+PFT+horizon+continent, FUN="sum", data=PFT_ES_all)
        # PFT_ES_all_mean <-cbind(PFT_ES_all_mean,PFT_ES_all_mean2$GDP)
        # names(PFT_ES_all_mean)[length(PFT_ES_all_mean)] <- "GDP"
        # PFT_ES_all_mean$ES_totfrac <- PFT_ES_all_mean$ES_tot/PFT_ES_all_mean$GDP

        
        

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
                    scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
                    #scale_fill_brewer(palette = "Paired",direction=1)+
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
                    
                    PFT_ES_all_mean_p$continent <- reorder(PFT_ES_all_mean_p$continent, PFT_ES_all_mean_p$ES_totfrac, FUN = sum)
                    levels(factor(PFT_ES_all_mean_p$continent))

                    polar_rel_prov <- ggplot(PFT_ES_all_mean_p,
                    aes(x = continent, y = (ES_totfrac)*100, fill= PFT)) + 
                    #geom_text(aes(y = ES_relative + 0.5,label = continent))+
                    geom_bar(stat = "identity", width = .9) +ylim(0,5) +
                    #coord_polar(theta = "x", start = -.13) +
                    theme_bw() +
                    scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
                    #scale_fill_brewer(palette = "Paired",direction=1)+
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
                    ggsave("Barplots_ESvalues_updateddata_newcolorscheme.png",dpi=300)
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
    ## ES values per hectare
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
        world2 <- merge(world,damcoef,by="iso_a2",all=TRUE)
        glimpse(world2)
        
        pal <- palette(brewer.pal(n = 3, name = "Spectral"))
        
        world2$name_long[which(is.na(world2$DamCoeffT2))]
        names(table(wealth$countrycode))[which(countrycode(names(table(wealth$countrycode)),origin="iso3c",destination="iso2c") %notin% names(table(world$iso_a2)))]

        dim(table(world$name_long[which(!is.na(world$DamCoeffT2))]))
        dim(table(world$name_long))
        
        world3 <- world2[which(!is.na(world2$DamCoeffT2)),]
        glimpse(world3[which(world3$TypeES!="All"),])
        
        wealth$iso_a2 <- countrycode(wealth$countrycode,origin="iso3c",destination="iso2c")
        world4 <- merge(world2,wealth,by="iso_a2",all=TRUE)
        worldna <- world4[which(is.na(world4$DamCoeffT2)),]
        worldna <- worldna[which(worldna$year==2018),]
        worldna[is.na(worldna$name_long) & !is.na(worldna$N),]
        worldna$name_long[!is.na(worldna$name_long) & is.na(worldna$N)]
        worldna$TypeES="Non-provisioning"
        worldna <- rbind(worldna,worldna)
        worldna$TypeES[((dim(worldna)[1]/2)+1):dim(worldna)[1]]<-"Provisioning"
        
        ggplot(world3[which(world3$TypeES!="All"),]) +
        theme_void()+
        geom_sf(aes(geometry = geometry, fill = DamCoeffT2*100)) +
        scale_fill_gradientn(colours = pal,limits=c(-3,3),
            na.value="transparent",name="T^2 damage estimate (%)",oob = scales::squish)+
        theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) + facet_wrap(~TypeES)+
        geom_sf(data=worldna[which(!is.na(worldna$name_long)),],aes(geometry = geometry))+
        ggtitle("Estimated damages based on simulated PFTs")
        ggsave("map_DamCoef_prov_noprov.png",dpi=600)

    # Nat Cap Figures (end)



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

        sum(df$meangy[which(df$exp=="nodam"&df$year==2100)]<0,na.rm=TRUE)
       

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
       all_sim2 <- all_sim[,which(names(all_sim) %in% c("es_change","gdp_change","country","year",'exp',"Continent","GDP"))]
    
       all_sim_mean <- stats::aggregate(.~country+year+exp+Continent,data=all_sim2, FUN="mean")
       all_sim_sd <- aggregate(.~country+year+exp+Continent,data=all_sim2, FUN="sd")
        glimpse(all_sim_mean)
    
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
        glimpse(all_sim_mean)
        glimpse(all_sim_sd)
        
        all_sim_mean <- merge(all_sim_mean,all_sim_sd[,c(5,6,8)],by="id",all=FALSE)

    
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

    maxyear <- aggregate(year~country, data=all_sim_mean[which(all_sim_mean$exp=="SSP245" ),],FUN="max")
    maxyear$countrymax <- paste0(maxyear$country,maxyear$year)
    all_sim_mean$countrymax <- paste0(all_sim_mean$country,all_sim_mean$year)

    all_simmax <- all_sim_mean[all_sim_mean$countrymax %in% maxyear$countrymax,]
    
plot_count_es <- ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100),], # & abs(all_sim_mean$es_change)>0.01
     aes(x=es_change*100,fill=Continent))+
     geom_histogram()+theme_minimal()+ xlab("")+ylab("count in 2100") + xlim(-100,60)

     mean(all_sim_mean$es_change[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100)])
     
     
     aggregate(gdp_change~Continent,data=all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100),],FUN="mean")

     

plot_count_gdp <- ggplot(all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100 ),], #& abs(all_sim_mean$gdp_change)>0.01
     aes(x=gdp_change*100,fill=Continent))+xlab('')+xlim(-15,6)+ylab("count in 2100")+
     geom_histogram()+theme_minimal()+coord_flip() 
mean(all_sim_mean$gdp_change[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100 )])
mean(all_sim_mean$gdp_change[which(all_sim_mean$exp=="NDC_cond" & all_sim_mean$year ==2100 )])
mean(all_sim_mean$es_change[which(all_sim_mean$exp=="NDC_cond" & all_sim_mean$year ==2100 )])


mean(all_sim_mean$gdp_change[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100 )]) * 
mean(all_sim_mean$GDP[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100 )]) /
mean(all_sim_mean$GDP[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2018 )]) 



mean(all_sim_mean$gdp_change[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100 & all_sim_mean$Continent=="Africa")]) * 
mean(all_sim_mean$GDP[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100& all_sim_mean$Continent=="Africa" )]) /
mean(all_sim_mean$GDP[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2018& all_sim_mean$Continent=="Africa" )]) 

mean(all_sim_mean$gdp_change[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100 & all_sim_mean$Continent=="Europe")]) * 
mean(all_sim_mean$GDP[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100& all_sim_mean$Continent=="Europe" )]) /
mean(all_sim_mean$GDP[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2018& all_sim_mean$Continent=="Europe" )]) 


    emptyplot <- ggplot()+theme_void()
        
    traj_plot <- ggarrange(ggarrange(plot_trajectory, plot_count_gdp,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="bottom"),
        ncol=1,nrow=2,common.legend=TRUE,heights=c(3,1),legend="bottom",align="hv")
    
    annotate_figure(traj_plot, top = text_grob("Benefits change under SSP2-4.5", 
                face = "bold", size = 14))
    #ggsave("Figures/Trajectory_completeHist.png",dpi=300)

     ggarrange(ggarrange(plot_trajectory, plot_count_gdp,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="none"),as_ggplot(leg_trj),
        ncol=1,nrow=3,common.legend=TRUE,heights=c(4,1,1),legend="none",align="hv")
ggsave("Figures/Trajectory_shapesYear_ncp.png",dpi=300)

glimpse(all_sim_mean)
plot_gdp_gdpchange <- ggplot(data=all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100),])+
geom_point(aes(x=log(GDP),y=gdp_change*100,color=Continent))+
geom_smooth(aes(x=log(GDP),y=gdp_change*100))+
theme_bw() + xlab("Log GDP in 2100") + ylab("GDP change in 2100 (%)")+ 
geom_hline(aes(yintercept=0),linetype="dashed")+
theme(legend.position="bottom")
plot_gdp_gdpchange

plot_gdp_eschange <- ggplot(data=all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year ==2100),])+
geom_point(aes(x=log(GDP),y=es_change*100,color=Continent))+
geom_smooth(aes(x=log(GDP),y=es_change*100))+
theme_bw() + xlab("Log GDP in 2100") + ylab("Ecosystem services change in 2100 (%)") + 
geom_hline(aes(yintercept=0),linetype="dashed") +
theme(legend.position="bottom")

ggarrange(plot_gdp_eschange,plot_gdp_gdpchange,nrow=1,ncol=2,common.legend=TRUE)
ggsave("logGDP_esgdpchange.png",dpi=300)



es_c2100 <- all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year %in% c(2100) ),]
glimpse(es_c2100)
es_c2018 <- all_sim_mean[which(all_sim_mean$exp=="SSP245" & all_sim_mean$year %in% c(2018) ),]
names(es_c2018) <- paste0(names(es_c2018),"2018")
glimpse(es_c2018)
es_20182100 <- merge(es_c2018,es_c2100,by.x="country2018",by.y="country")

plot_gdp_gdpchange <- ggplot(data=es_20182100)+
geom_point(aes(x=log(GDP2018),y=100*gdp_change*GDP/GDP2018,color=Continent))+
geom_smooth(aes(x=log(GDP2018),y=100*gdp_change*GDP/GDP2018))+
theme_bw() + xlab("Log GDP in 2018") + ylab("GDP change in 2100\n(% of 2018 GDP)")+ 
geom_hline(aes(yintercept=0),linetype="dashed")+
theme(legend.position="bottom")
plot_gdp_gdpchange

plot_gdp_eschange <- ggplot(data=es_20182100)+
geom_point(aes(x=log(GDP2018),y=es_change*100,color=Continent))+
geom_smooth(aes(x=log(GDP2018),y=es_change*100))+
theme_bw() + xlab("Log GDP in 2018") + ylab("ES change in 2100\n(% of ES in 2018)") + 
geom_hline(aes(yintercept=0),linetype="dashed") +
theme(legend.position="bottom")

ggarrange(plot_gdp_eschange,plot_gdp_gdpchange,nrow=1,ncol=2,common.legend=TRUE)
ggsave("logGDP_esgdpchange2018.png",dpi=300)

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


