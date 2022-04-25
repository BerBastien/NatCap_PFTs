#Setup
    x <- c('raster', 'reshape2','randomForestExplainer','randomForest','ggpubr','dplyr','ncdf4',"ggridges","forcats",'ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf','lfe','marginaleffects','rgdal',"rnaturalearth")
    lapply(x, require, character.only = TRUE)
    #install.packages('marginaleffects')
    setwd('C:\\Users\\bastien\\Documents\\GitHub\\NatCap_PFTs')
    dirpfts <- "C:/Users/bastien/Documents/GitHub/NatCap_PFTs/Data"
    #install.packages("randomForestSRC")
            #library("randomForestSRC")
     world <- spData::world
         isos <- world$iso_a2
            

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
        es <- read.csv('C:/Users/bastien/Documents/GitHub/es_database/database/es_database_march2022.csv')
        df_es <- es[es$Single.Value.Converted>0,]
        df_es <- df_es[df_es$pop_density != Inf,]
        df_es <- df_es[is.na(df_es$gdp_pc) == FALSE,]
        df_es <- df_es[df_es$gdp_pc >0,]
        df_es$logESValue <- log(df_es$Single.Value.Converted)
        df_es$logGDP <- log(df_es$gdp_pc)
        df_es$ES_frct <- 100*df_es$Single.Value.Converted/(df_es$gdp_pc * df_es$totpop)
        df_es$ES_frct <- 100*(df_es$area*247105)*df_es$Single.Value.Converted/(df_es$gdp_pc * df_es$totpop)
        df_es$pct_covered <- df_es$PFT.bne+df_es$PFT.bine+df_es$PFT.bns+df_es$PFT.tebs+
            df_es$PFT.ibs+df_es$PFT.tebe+df_es$PFT.trbe+df_es$PFT.tribe+
            df_es$PFT.trbr+df_es$PFT.c3g+df_es$PFT.c4g
        pfts <- c("PFT.bne","PFT.bine","PFT.bns","PFT.tebs","PFT.ibs","PFT.tebe","PFT.trbe","PFT.tribe","PFT.trbr","PFT.c3g","PFT.c4g") 
    
        
        df_es$dominant_pft  <- colnames(df_es[,which(names(df_es)%in%pfts)])[max.col(df_es[,which(names(df_es)%in%pfts)],ties.method="first")]

        df_es_inf <- df_es[which(df_es$Ecosystem.Service.Category == 'Information'),]
        df_es_prov <- df_es[which(df_es$Ecosystem.Service.Category == 'Provisioning'),]
        df_es_reg <- df_es[which(df_es$Ecosystem.Service.Category == 'Regulating'),]
        df_es_sup <- df_es[which(df_es$Ecosystem.Service.Category == 'Supporting'),]
        df_es_nonuse <- df_es[which(df_es$Value.Type.General == 'nonuse'),]
        df_es_use <- df_es[which(df_es$Value.Type.General == 'use'),]
        df_es_use_market <- df_es[which(df_es$Value.Type.General == 'use' & df_es$Value.Type.Specific == 'market'),]
        df_es_use_service <- df_es[which(df_es$Value.Type.General == 'use'& df_es$Value.Type.Specific == 'service'),]
        df_es_nonmarket <- rbind(df_es_use_service,df_es_nonuse)
        #Creating a dataset without meta-analysis or transfer value studies; only original studies
        es_or <- df_es[which(df_es$Valuation.Methodology.General == 'Biophysical Measurement' | 
                    df_es$Valuation.Methodology.General == 'Biophysical Modeling' | 
                    df_es$Valuation.Methodology.General == 'Market Based' | 
                    df_es$Valuation.Methodology.General == 'Revealed Preference'| 
                    df_es$Valuation.Methodology.General == 'Stated Preference'),]
        es_or_inf <- es_or[which(es_or$Ecosystem.Service.Category == 'Information'),]
        es_or_prov <- es_or[which(es_or$Ecosystem.Service.Category == 'Provisioning'),]
        es_or_reg <- es_or[which(es_or$Ecosystem.Service.Category == 'Regulating'),]
        es_or_sup <- es_or[which(es_or$Ecosystem.Service.Category == 'Supporting'),]
        es_or_nonuse <- es_or[which(es_or$Value.Type.General == 'nonuse'),]
        es_or_use <- es_or[which(es_or$Value.Type.General == 'use'),]
        es_or_use_market <- es_or[which(es_or$Value.Type.General == 'use' & es_or$Value.Type.Specific == 'market'),]
        es_or_use_service <- es_or[which(es_or$Value.Type.General == 'use'& es_or$Value.Type.Specific == 'service'),]
        es_or_nonmarket <- rbind(es_or_use_service,es_or_nonuse)

        #Creating a datasrt with only accepted studies
        es_ac <- df_es[which(df_es$State=='accepted'),]
        es_ac_inf <- es_ac[which(es_ac$Ecosystem.Service.Category == 'Information'),]
        es_ac_prov <- es_ac[which(es_ac$Ecosystem.Service.Category == 'Provisioning'),]
        es_ac_Noprov <- es_ac[which(es_ac$Ecosystem.Service.Category != 'Provisioning'),]
        es_ac_reg <- es_ac[which(es_ac$Ecosystem.Service.Category == 'Regulating'),]
        es_ac_sup <- es_ac[which(es_ac$Ecosystem.Service.Category == 'Supporting'),]
        es_ac_nonuse <- es_ac[which(es_ac$Value.Type.General == 'nonuse'),]
        es_ac_use <- es_ac[which(es_ac$Value.Type.General == 'use'),]
        es_ac_use_market <- es_ac[which(es_ac$Value.Type.General == 'use' & es_ac$Value.Type.Specific == 'market'),]
        es_ac_use_service <- es_ac[which(es_ac$Value.Type.General == 'use'& es_ac$Value.Type.Specific == 'service'),]
        es_ac_nonmarket <- rbind(es_ac_use_service,es_ac_nonuse)
        es_ac_notFood <- es_ac[which(es_ac$Ecosystem.Service.General != 'Food'),]
        write.csv(es_ac_notFood,"es_ac_notFood.csv")
 
        #Creating a dataset with only original and accepted studies
        es_oa <- es_or[which(es_or$State=='accepted'),]
        es_oa_inf <- es_oa[which(es_oa$Ecosystem.Service.Category == 'Information'),]
        es_oa_prov <- es_oa[which(es_oa$Ecosystem.Service.Category == 'Provisioning'),]
        es_oa_reg <- es_oa[which(es_oa$Ecosystem.Service.Category == 'Regulating'),]
        es_oa_sup <- es_oa[which(es_oa$Ecosystem.Service.Category == 'Supporting'),]
        es_oa_nonuse <- es_oa[which(es_oa$Value.Type.General == 'nonuse'),]
        es_oa_use <- es_oa[which(es_oa$Value.Type.General == 'use'),]
        es_oa_use_market <- es_oa[which(es_oa$Value.Type.General == 'use' & es_oa$Value.Type.Specific == 'market'),]
        es_oa_use_service <- es_oa[which(es_oa$Value.Type.General == 'use'& es_oa$Value.Type.Specific == 'service'),]
        es_oa_nonmarket <- rbind(es_oa_use_service,es_oa_nonuse)
        es_oa_marketbased <- es_oa[which(es_oa$Valuation.Methodology.General == 'Market Based'),]
        es_oa_marketbased_notfood <- es_oa_marketbased[which(es_oa_marketbased$Ecosystem.Service.General != 'Food'),]
        es_oa_notFood <- es_oa[which(es_oa$Ecosystem.Service.General != 'Food'),]
        write.csv(es_oa_notFood,"es_oa_notFood.csv")
    #Read ES database

    #Read Natural Capital Wealth Accounts (start)
        wealth <- read.csv('Data\\WorldBank_WealthAccounts_2018USD.csv')
        colnames(wealth) <- c("Country.Name" ,"Country.Code",   "Series.Name" , "Series.Code",
        1995:2018)
        glimpse(wealth)
        table(wealth$countrycode)

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
        glimpse(Wealth)
        levels(factor(Wealth$countrycode))[levels(factor(Wealth$countrycode)) %in% isos_missin3]
        
        #ggplot(data=Wealth, aes(x = year, y=log(N), group = countrycode)) + 
        #geom_line(aes(color=countrycode))  +
        #scale_colour_discrete(guide = 'none') +
        #scale_x_discrete(expand=c(0, 1)) +
        #geom_dl(aes(label = countrycode), method = list(dl.combine("first.points", "last.points")), cex = 0.8)

        gdp <- read.csv('Data\\GDP_2015USD_19952018.csv')
        colnames(gdp) <- c("countryname", "countrycode", "seriesname","seriescode",1995:2018)
        glimpse(gdp)
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
        table(wealth$countrycode)
        

        
        population  <- read.csv('Data\\Pop1995_2018.csv')
        colnames(population) <-  c("countryname", "countrycode", "seriesname","seriescode",1995:2018)
        glimpse(population)
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
        glimpse(wealth_data)
        table(wdata_1995_2018$countrycode)

        labor  <- read.csv('Data\\laborIncomeShare.csv') #Labour income share as a percent of GDP -- ILO modelled estimates, July 2019 (%). Downloaded from ILOSTAT. Last update on 10JAN21.
        colnames(labor) <- c("countryname", "source", "year", "labor_share")
        glimpse(labor)
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
        
        glimpse(wealth_data)
        
        wealth_data <- wealth_data[,-which(names(wealth_data) %in% c("countryname.y","NA","source"))]
        glimpse(wealth_data)
        table(wealth_data$labor_share)
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
        glimpse(Natrents2)
        glimpse(wealth_data)    
        
        wealth_data <- merge(wealth_data,Natrents2, by = c("countrycode","year"),all.x=TRUE)
        glimpse(wealth_data)
        wealth_data$NatRents <- as.numeric(as.character(wealth_data$NatRents))
        table(wealth_data$countrycode)
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
        glimpse(ForestArea2)
        glimpse(wealth_data)    
        
        wealth_data <- merge(wealth_data,ForestArea2, by = c("countrycode","year"), all.x = T)
        glimpse(wealth_data)
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
        wdata2018$difN <- (wdata2018$Ngreen - wdata1995$Ngreen)/wdata1995$Ngreen
        wdata2018$changeN <- "Gain"
        wdata2018[wdata2018$difN<0,]$changeN <- "Loss"
        #wdata2017 <- wdata2017[-which(is.na(wdata2017$GDP)),]
        glimpse(wdata2018)
    #Getting Continents




    ggplot(data = wdata2018, aes(x=(NforestT + NforestES + Npa)/(TotalWealth),y=log(GDP/Population)))+
    theme_bw() +
    geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=4,size=1)+
      scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
    geom_point(aes(size=(((NatRents*0.01)))),alpha=0.75)+
    theme_bw() +
    xlab("Natural Capital in Forests and Protected Areas \n(Fraction of Inclusive Wealth)") + 
    ylab("Log GDP per capita (2014 USD)") + 
    scale_colour_discrete(name="Continent")+
    scale_x_continuous(trans='log2')+
    #scale_shape_discrete(name="Natatural Capital change\nsince 1995")+
    scale_size_continuous(name="Production elasticity to \nNatural Capital")
    
    #Read Natural Capital Wealth Accounts (start)
#Setup


    # Read PFTs at different time horizons (start)
        # world <- spData::world
        # isos <- world$iso_a2


        # pfts <- c("bne","bine","bns","tebs","ibs","tebe","trbe","tribe","trbr","c3g","c4g") 
        # clim_models <- c("hadgem2-es","gfdl-esm2m","ipsl-cm5a-lr","miroc5")
        # for (climi in 1:length(clim_models)){
        #     for(h in 1:4){
        #         for (p in 1:length(pfts)){
        #                     v1 <- load(paste("Data/periods_",pfts[p],"_brick_",clim_models[climi],"_rcp6.Rdata",sep=""))
        #                     assign(paste("periods_",pfts[p],"_brick_",clim_models[climi],"_rcp6",sep=""), eval(parse(text=v1)))
        #                     v1 <- eval(parse(text=v1))
        #                     v1 <- t(v1)
        #                     if(p==1){
        #                         a <- as.data.frame(subset(v1,h), xy=TRUE)
        #                         names(a)[3] <- "Percent"
        #                         a$PFT <- pfts[p]
        #                         a$clim <-clim_models[climi]
        #                     }else{
        #                         b <- as.data.frame(subset(v1,h), xy=TRUE)
        #                         names(b)[3] <- "Percent"
        #                         b$PFT <- pfts[p]
        #                         b$clim <-clim_models[climi]
        #                         a <- rbind(a,b)
        #                     }
        #         }
        #                 a$dominant_pft  <- factor(colnames(a[,which(names(a)%in%pfts)])[max.col(a[,which(names(a)%in%pfts)],ties.method="first")])
        #                 a$ID <- seq(1:dim(a)[1])
        #                 if (climi==1 & h==1){
        #                     a_h <- a
        #                     a_h$horizon <- horizon[h]

        #                 }else{
        #                     a$horizon <- horizon[h]
        #                     a_h <- rbind(a_h,a)
        #                 }
        #     }
        # }
    
        # glimpse(a_h)
        # world <- spData::world
        # sw <- as_Spatial(st_cast(world))
        # raster_isos <- raster::rasterize(sw[1],v1)
        # isos_xy <- as.data.frame(raster_isos, xy=TRUE)
        # names(isos_xy)[3] <- "countrycode"
        # a_h$country <- isos_xy[,3]
        # area_xy <- as.data.frame(area(raster_isos), xy=TRUE)
        # names(area_xy)[3] <- "area"
        # a_h$area <- area_xy[,3]
        # a_h$areaPFT <- a_h$Percent * a_h$area
        # PFTs11_Clims4_horizons3 <- a_h
        # save(PFTs11_Clims4_horizons3,file="PFTs11_Clims4_horizons3.Rda")
        # load("PFTs11_Clims4_horizons3.Rda")
        # glimpse(PFTs11_Clims4_horizons3)
    # Read PFTs at different time horizons (start)
    load("PFTs11_Clims4_horizons3.Rda")


       


    #glimpse(wdata2018)
    #aaaa <- a_h[which(a_h$horizon=="Present"),]
    #levels(factor(aaaa$clim))
    a_h <- PFTs11_Clims4_horizons3
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
        a=a+geom_smooth(method="lm")
        x11()
        a
        dat$Category=ifelse(dat$Ecosystem.Service.Category=="Provisioning","Provisioning","Non-Provisioning")
        b=ggplot(dat,aes(x=log(Spatial.Extent),y=log(Single.Value.Converted),col=Category))+geom_point()
        b=b+geom_smooth(method="lm")
        b
        #look at linear model controlling for GDP
        mod=lm(log(Single.Value.Converted)~log(Spatial.Extent)*Category+log(gdp_pc),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0))
        elasticity_area <- summary(mod)$coefficients[2]
   
   
   TypeES[typeESi]
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
                    eff_pred[n] <- pft_pct_pred_dgvm$Percent[(which(paste0("PFT.",pft_pct_pred_dgvm$PFT_code)==names(eff_pred)[n]))]
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

                
                if(length(which(wealth2018_pft$country==isos[j]))==0){
                    print(paste("No World Bank Data for",isos[j]))
                    next
                }

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
                
                

                PFT_ES <-data.frame(PFT=rep(eff_pft$variable[order(eff_pft$variable,names(areasPFTs_database))],4),
                    relative_cont=rep(eff_pft$importance[order(eff_pft$variable,names(areasPFTs_database))],4),
                    area=areas,horizon=horizons,areas_change_pct = areas_change_pct)
                
                PFT_ES$ES_rel <- PFT_ES$relative_cont*ES     
                PFT_ES$ES_rel_areachange <- PFT_ES$ES_rel + 0.01*PFT_ES$ES_rel*PFT_ES$areas_change_pct* elasticity_area
                
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
                
                

                NaturalCapital$DamageEstimateT2 <- coef(summary(lm(Damage ~ 0 + I(temp^2), data = NaturalCapital )))[1]
                NaturalCapital$DamageEstimateT2_areachange <- coef(summary(lm(Damage_areachange ~ 0 + I(temp^2), data = NaturalCapital )))[1]
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

    save(NC_change,file="NC_change_controllingGDPandArea_es_ac_notFood_adjustedImportance_allClimProjections.Rda")
    save(PFT_ES_all,file="PFT_ES_all_controllingGDPandArea_es_ac_notFood_adjustedImportance_allClimProjections.Rda")
    load("NC_change.Rda")
    load("NC_change_controllingGDPandArea_es_ac_notFood_adjustedImportance_allClimProjections.Rda")

    

    ggplot(data = NC_change, aes(x=(N)/(TotalWealth),y=log(GDP)))+
    theme_bw() +
    #geom_density_2d_filled(contour_var = "ndensity",aes(fill=Continent,alpha=..level..),bins=4,size=1)+
    #  scale_alpha_discrete(range = c(0,0.5,0.7,0.7),guide = guide_none()) +
    geom_point(aes(color=horizon,size=(((NatRents*0.01)))),alpha=0.75)+
    theme_bw() +
    xlab("Natural Capital in Forests and Protected Areas \n(Fraction of Inclusive Wealth)") + 
    ylab("Log GDP (2014 USD)") + 
    scale_colour_discrete(name="Continent")+
    facet_wrap(~clim)+
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
    scale_size_continuous(name="Production elasticity to \nNatural Capital")+facet_wrap(~clim)
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
        w$logNgreen <- log(w$Ngreen)
        hist(w$logNforestT)
        summary(felm(logGDP~logK+logH+logNforestT|year + countrycode|0|0,data=w, weights = (w$logGDP)))
        K_elas_world <- summary(felm(logGDP~logK+logH+logNforestT|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[1]
        L_elas_world <- summary(felm(logGDP~logK+logH+logNforestT|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[2]
        
        
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

    # Fixed Investments on Manufactured Capital + NatCap damages (exp2)
        wdata2017$year <- as.numeric(as.character(wdata2017$year))
        wealth_sim <- wdata2017[,which(names(wdata2017) %in% c("countrycode",
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
        simname <- c("nodam","10p","SSP245","NDC_cond")
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
                Nforest = wealth_sim$NforestT[wealth_sim$countrycode==country[i]] + wealth_sim$NforestES[wealth_sim$countrycode==country[i]]
                Nagg = wealth_sim$Nagg[wealth_sim$countrycode==country[i]]
                Npa = wealth_sim$Npa[wealth_sim$countrycode==country[i]]

                n0 = Nforest  + Npa
                
                #n0 = wealth_sim$N[wealth_sim$countrycode==country[i]]
                yt=numeric(length=t);yt[1]=y0
                kt=numeric(length=t);kt[1]=K0
                Tt=numeric(length=t);Tt[1]=T0
                pi1 <- y0/(h0^a1*K0^a2*n0^a3)    #normalizing param
                year = 1:83
                #df <- wealth_sim[0,]

                    nt=numeric(length=t);nt[1]=n0
                    yt=numeric(length=t);yt[1]=y0
                    kt=numeric(length=t);kt[1]=K0
                    for(j in 2:t){
                    kt[j]=kt[j-1]*(1-0)+(yt[j-1])*s

                    if (simname[ss]=="nodam"){
                        nt[j] = nt[j-1]
                    }else if(simname[ss]=="10p"){
                        if(j==6){nt[j] = nt[j-1]*0.9}else{
                            nt[j] = nt[j-1]
                            if(j==7){nt[j] = nt[j-1]/0.9}
                        }

                    } else if(simname[ss]=="SSP245"){
                        if(j>5){
                            nt[j] = nt[j-1]*(1+
                                NC_change$DamageEstimateT2[NC_change$countryiso3==country[i] & NC_change$clim == clim_models[climi] ][1]*
                                (Temp_ssps[j+2,2]-Temp_ssps[1,2])^2)[[1]]
                        
                        }else{
                            nt[j] = nt[j-1]
                        }

                    } else if(simname[ss]=="NDC_cond"){
                        if(j>5){
                            nt[j] = nt[j-1]*(1+
                                NC_change$DamageEstimateT2[NC_change$countryiso3==country[i] & NC_change$clim == clim_models[climi] ][1]*
                                (Temp_ssps[j+2,3]-Temp_ssps[1,3])^2)[[1]]
                        
                        }else{
                            nt[j] = nt[j-1]
                        }
                        
                    }
                    
                    yt[j]=pi1*(h0^a1)*(kt[j]^(a2))*nt[j]^a3
                    }
                    gy <- (yt - lag(yt))/lag(yt) #output growth
                    meangy <- mean(gy[3:length(gy)]) #mean output growth 
                    npv <- sum(yt*(1/((1+0.03)^seq(1:length(yt)))))
                    dam_estimate <- NC_change$DamageEstimateT2[NC_change$countryiso3==country[i] & NC_change$clim == clim_models[climi] ][1]
                    

                    
                    if(i==1 & ss==1 & climi==1){
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
                                gy = gy, meangy = meangy,exp=simname[ss],clim=clim_models[climi],npv=npv,dam=dam_estimate)
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
                                gy = gy, meangy = meangy,exp=simname[ss],clim=clim_models[climi],npv=npv,dam=dam_estimate)) 
                    }
                #wealth_sim <- rbind(wealth_sim,df)
                }
            }
        }
       

        wealth_sim3 <- df[df$exp=="10p",]

       
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
        all_sim <- data.frame(gdp_change = df$GDP - df$GDP[df$exp=="nodam"], growth_change = df$gy - df$gy[df$exp=="nodam"],
        npv_change =  df$npv - df$npv[df$exp=="nodam"], Nchange =  df$n - df$n[df$exp=="nodam"], country = df$country, year=df$year,       H=df$H,           N=df$N   ,       Nagg=df$Nagg,
        NforestT = df$NforestT,   NforestES = df$NforestES,   Npa=df$Npa,         K=df$K,          TotalWealth=df$TotalWealth,
        GDP=df$GDP,      Population=df$Population,  labor_share=df$labor_share, NatRents=df$NatRents,    Continent = df$Continent, 
        n=df$n,           gy=df$gy,          meangy=df$meangy,      exp=df$exp , clim=df$clim    , npv = df$npv  )

        sim_2100 <- all_sim[all_sim$exp %notin% c("10p","nodam") & all_sim$year %in% c(2100),]
        meanGrowthChange <- aggregate(growth_change~country+exp,FUN=mean,data=sim_2100)
        names(meanGrowthChange)[3] <- "MeanGrowthChange"
        
        sim_2100 <- merge(sim_2100,meanGrowthChange,by=c("country","exp"))
        glimpse(sim_2100)

        
    ggplot(data = sim_2100[which(sim_2100$clim==clim_models[1]),], 
            aes(x=MeanGrowthChange*100,y=log(GDP/Population),color=Continent,shape=exp))+
            xlab("Mean GDP growth change (pp)")+ylab("Log GDP per capita")+
        theme_bw()+
        geom_point(aes(size=NatRents*0.01))+ggtitle("Change in mean GDP growth (2020-2100)")+
        guides(color=guide_legend(title="Region"),size=guide_legend(title="Natural capital\n elasticity"),shape=guide_legend(title="Scenario"))
       ggsave("Change_GDP_growth_ssps_ndc.png")
        
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
    