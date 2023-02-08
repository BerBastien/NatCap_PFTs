#setup#

    x <- c('raster',"hacksaw", 'ggpubr','dplyr','ncdf4','ggplot2','countrycode','tidyverse','RColorBrewer','colorspace','spData','sf','lfe','marginaleffects','rgdal',"rnaturalearth")
    lapply(x, require, character.only = TRUE)
    #install.packages('marginaleffects')
    setwd('C:\\Users\\basti\\Documents\\GitHub\\NatCap_PFTs')
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

#READ WORLD BANK DATA
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
        wealth_data <- merge(wealth,Population,all=TRUE,by=c("countrycode","year"))
        wdata_1995_2018 <- wealth_data
        glimpse(Population[Population$countrycode=="RUS",])
        glimpse(wealth_data[wealth_data$countrycode=="RUS",])
        #glimpse(wealth_data)
        #table(wdata_1995_2018$countrycode)

        #labor  <- read.csv('Data\\laborIncomeShare.csv') #Labour income share as a percent of GDP -- ILO modelled estimates, July 2019 (%). Downloaded from ILOSTAT. Last update on 10JAN21.
        #colnames(labor) <- c("countryname", "source", "year", "labor_share")
        #glimpse(labor)
        #b <- merge(wealth_data,labor, by = c("countrycode","year"),all.x=T)
        #wealth_data <- b

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
        #wealth_data$labor_share <- as.numeric(as.character(wealth_data$labor_share))
        wealth_data$Population <- as.numeric(as.character(wealth_data$Population))
        
        #glimpse(wealth_data)
        
        wealth_data <- wealth_data[,-which(names(wealth_data) %in% c("countryname.y","NA","source"))]
        #glimpse(wealth_data)
        #table(wealth_data$labor_share)

        #ggplot(wealth_data[])+
        #geom_point(aes(x=log(GDP),y=log(Nfisheries/K),color=countrycode,group=countrycode))

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
        save(file="Data/wealth_data.Rda",wealth_data)

        r5 <- read.csv("Data/r5regions.csv")
        names(r5)<-c("r5","countrycode")

        wealth_data <- merge(wealth_data,r5,by="countrycode")
        

        #table(wealth_data$countrycode)
    # Forest Rents (end)

    wealth2018 <- wealth_data[which(wealth_data$year==2018),]
    wealth_data$nN <- (wealth_data$NforestES+wealth_data$Npa)/wealth_data$TotalWealth
    wealth_data$mN <- (wealth_data$NforestT)/wealth_data$TotalWealth


    ww <- wealth_data[which(wealth_data$nN>quantile(wealth_data$nN,0.05,na.rm=TRUE) &
    wealth_data$mN>quantile(wealth_data$mN,0.06,na.rm=TRUE) ),]

               ww$r5 <- factor(ww$r5)
                levels(ww$r5) <- c("ASIA","LAM", "MAF","OECD","REF")

    Npa_year <- aggregate(Npa ~ year, data = wealth_data, FUN = "sum")
        names(Npa_year)[2] <- "Total_Npa"
        NES_year <- aggregate(NforestES ~ year, data = wealth_data, FUN = "sum")
        names(NES_year)[2] <- "Total_NES"
        
        w2 <- merge(wealth_data,Npa_year,by="year")        
        w2 <- merge(w2,NES_year,by="year")
        w2$Global_nN <- w2$Total_Npa + w2$Total_NES
        glimpse(w2)
        w2 <- w2[which(w2$r5!=""),]
        modelCD <- felm(log(GDP)~log(K)+log(NforestT)+log(H)+log(Global_nN):r5+year|countrycode+r5|0|0,data=w2[which(w2$NforestT>0&w2$GDP>0),])
        
        summary(modelCD)

        table_coefficients <- w2[which(w2$year==2018),]
        table_coefficients$mN <-table_coefficients$NforestT
        table_coefficients$nN <- table_coefficients$NforestES + table_coefficients$Npa
        table_coefficients <- table_coefficients[,which(names(table_coefficients) %in% c("mN","nN","NatRents","countrycode"))]
        dam <- read.csv("Data/Damage_coef.csv")
        glimpse(dam)
        dam2 <- dam[which(dam$formula=="lin" & dam$dgvm=="lpj" & dam$capital=="mN"),]
        dam2 <- dam2[,which(names(dam2) %in% c("coef","iso3","pval"))]
        glimpse(table_coefficients)
        table_coefficients <- merge(table_coefficients,dam2, by.x = "countrycode", by.y="iso3")
        table_coefficients$coef_mN <- table_coefficients$coef
        table_coefficients$pval_mN <- table_coefficients$pval
        table_coefficients <- table_coefficients[,-which(names(table_coefficients) %in% c("pval","coef"))]

        dam2 <- dam[which(dam$formula=="lin" & dam$dgvm=="lpj" & dam$capital=="nN"),]
        dam2 <- dam2[,which(names(dam2) %in% c("coef","iso3","pval"))]
        glimpse(table_coefficients)
        table_coefficients <- merge(table_coefficients,dam2, by.x = "countrycode", by.y="iso3")
        table_coefficients$coef_nN <- table_coefficients$coef
        table_coefficients$pval_nN <- table_coefficients$pval
        table_coefficients <- table_coefficients[,-which(names(table_coefficients) %in% c("coef","pval"))]

        names(table_coefficients)[2] <- "gamma_3"
        table_coefficients$gamma_3 <- table_coefficients$gamma_3*0.01

        write.csv(table_coefficients,"Data/table_coefficients.csv")

  

#READ WORLD BANK DATA (end)

## Plot Figure 1 (start)

    levels(factor(ww$r5))
    scaleFUN <- function(x) sprintf("%.4f", x)
    ggplot(ww[which(ww$year==2018),],aes(x=nN*100,y=mN*100))+
    theme_bw() +
    geom_density_2d_filled(contour_var = "ndensity",aes(fill=r5,alpha=..level..),bins=4,size=1)+
    scale_alpha_discrete(range = c(0,0.9,1,1),guide = guide_none()) +
    geom_point(aes(x=100*nN,y=100*mN,size=log(GDP)),alpha=0.5)+
    scale_y_continuous(trans="log2",labels=scaleFUN)+
    scale_x_continuous(trans="log2",labels=scaleFUN)+
        xlab("Nonmarket natural capital \n(% of Inclusive Wealth)") + 
        ylab("Market natural capital \n(% of Inclusive Wealth)") + 
        scale_fill_discrete(name="Region")+
        #scale_shape_discrete(name="Natatural Capital change\nsince 1995")+
        scale_size_continuous(name="Log GDP \nin 2018")
        ggsave("Figures/F1_NatCap_R5.png",dpi=600)

## Plot Figure 1 (END)


#Read NetCDF of PFTs (start)

    veg_iso_year <- data.frame(iso2=factor(),
                    var = character(),
                    value=double(),
                    year=integer())
    clim_models <- c("hadgem2-es","gfdl-esm2m","ipsl-cm5a-lr","miroc5")
    #clim_models <- c("gfdl-esm2m","ipsl-cm5a-lr")
    clim_scen <- c("rcp60","rcp26","rcp85")
    soc_scen <- c("2005soc","rcp26soc","recp60soc")
    soc_scen <- c("2005soc")
    dgvms <- c("lpj","orc","car")
    dgvms_long <- c("lpj-guess","orchidee-dgvm","caraib")
    pfts_lpj <- c("bne","bine","bns","tebs","ibs","tebe","trbe","tribe","trbr","c3g","c4g") 
    pfts_orc <- c("tebrsu",
    "trbrev",
    "trbrrg","tendev","tebrev",
    "bondev","bobrsu","bondsu","c3gra","c4gra")
    #pfts_lpj <- c("bne","bine","bns","tebs","ibs","tebe","trbe","tribe","trbr") 
    #pfts_orc <- c("tebrsu","trbrev","trbrrg","tendev","tebrev","tebrsu","bondev","bobrsu","bondsu")


    pfts_names_lpj <- c("Boreal needleleaved evergreen","Boreal shade intolerant needleleaved evergreen", 
    "Boreal needleleved summergreen (BNS)", 
    "Temperate broadleaved summergreen (TeBS)", 
    "shade intolerant broadleaved summergreen (IBS)", 
    "Temperate broadleved evergreen (TeBE)",
    "Tropical broadleaved evergreen (TrBE)", 
    "Tropical shade intolerant broadleaved evergreen (TrIBE)", 
    "Tropical broadleaved raingreen (TrBR)",
    "C3 grass (C3G)", "C4 grass (C4G)")
    
    pfts_car <- c("c3hh",
        "c3dh",
        "c4h",
        "brsuas",
        "brsutecds",
        "brsutewms",
        "brevtecds",
        "brevtewms","brevxs",
        "sds","trs",
        "ndevtecdt",
        "ndevteclt",
        "ndevtedtt",
        "ndevtedttht",
        "ndevstdit",
        "ndsutecdt",
        "ndsustswt",
        "brevdtt",
        "brevdttht","brevstdit","brsutecdt","brsuteclt","brsutewmt","brrgtrt","brevtrt")

    #Boreal needleleaved evergreen (BNE); 
    #Boreal shade intolerant needleleaved evergreen (BINE); 
    #Boreal needleleved summergreen (BNS); 
    #Temperate broadleaved summergreen (TeBS); 
    #shade intolerant broadleaved summergreen (IBS); 
    #Temperate broadleved evergreen (TeBE); 
    #Tropical broadleaved evergreen (TrBE); 
    #Tropical shade intolerant broadleaved evergreen (TrIBE); 
    #Tropical broadleaved raingreen (TrBR); 
    #C3 grass (C3G); C4 grass (C4G); 
    #C3 agricultural grass (C3G_agr); C4 agricultural grass (C4G_agr); 
    #* The last two PFTs are physiologically identical to the previous two but output separately*
    df_biome_area_change <- data.frame(decade=double(),dif_biome_area=double(),area_initial=double(),scen=character(),dgvms_model=character(),gcm_model=character(),soc_scen=character(),PFT=character())
    first <- 1
    for (soci in 1:1){
        for (sceni in 1:length(clim_scen)){
            soc_scen <- c("2005soc",paste0(clim_scen[sceni],"soc"))
            for (dgvmsi in 3:length(dgvms)){
            #for (dgvmsi in 1:length(dgvms)){
                pfts <- eval(parse(text=paste0("pfts_",dgvms[dgvmsi])))
                for (climi in 1:length(clim_models)){
                    for (p in 1:length(pfts)){
                        dname <- paste("pft-",pfts[p],sep="")
                        listoffiles <- list.files(paste0(dir1,"PFTs_",dgvms[dgvmsi],"/"))
                        ncname <- paste(dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_pft-",pfts[p],"_global_annual_2006_2099.nc4",sep="")
                        if(ncname %notin% listoffiles){
                            next
                        }


                        ncname <- paste(dir1,"PFTs_",dgvms[dgvmsi],"/",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_pft-",pfts[p],"_global_annual_2006_2099.nc4",sep="")
                        #ncname <- paste0(dir1,"PFTs_",dgvms[dgvmsi],"/caraib_gfdl-esm2m_ewembi_rcp60_2005soc_co2_pft-brevdttht_global_annual_2006_2099.nc4")
                     
                        ncin <- nc_open(ncname)
                        #print(ncin)    
                        #get units
                        lon <- ncvar_get(ncin,"lon")
                        lat <- ncvar_get(ncin,"lat")
                        time_veg <- ncvar_get(ncin,"time")
                        tunits_veg <- ncatt_get(ncin,"time","units")
                        years_veg <- 1661 + time_veg

                        
                        var_array <- ncvar_get(ncin,dname, start = c(1,1,1), count=c(-1,-1,-1)) #start, number in dimension to start. count: hopw many in that dimension
                        #glimpse(var_array)
                        #var_array <- var_array[,,16:85] #2021 to 2090
                        #numdecades <- floor(dim(var_array)[3]/10)
                        #decadal <- array(numeric(),c(720,360,numdecades)) 
                        #dif_decadal <- array(numeric(),c(720,360,numdecades-1)) 
                        #numperiods <- 4
                        numperiods <- 8
                        periods <- array(numeric(),c(360,720,numperiods)) 
                        dif_periods <- array(numeric(),c(360,720,numperiods-1)) 
                        #periods_minyear <- c(1,16,36,76) #years 2006,2021,2041,2081
                        #periods_maxyear <- c(15,35,55,94) #years 2020,2040,2060,2099
                        
                        periods_minyear <- c(1,25,35,45,55,65,75,85) #years 2006,2030,2040,2050,2060,2070,2080,2090
                        periods_maxyear <- c(15,34,44,54,64,74,84,94) #years 2020,2039,2949,2959,2969,2979,2989,2099
                        
                        for (d in (1:numperiods)){ 
                            period_array <- var_array[,,periods_minyear[d]:periods_maxyear[d]] #decade d of data
                            period_mean <- rowMeans(period_array, dims = 2, na.rm = TRUE)
                            if (dgvms[dgvmsi]=="lpj" | dgvms[dgvmsi]=="car"){
                                periods[,,d] <- t(period_mean*0.01)
                            }else{
                                periods[,,d] <- t(period_mean)
                            }
                            
                            if (d>1){
                                dif_periods[,,d-1] <- periods[,,d] - periods[,,1]

                            }
                        }
                       
                        periods_veg_brick <- brick(periods, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
                        #plot(subset(periods_veg_brick,8))     
                        #plot(brick(var_array[,,c(1,56)]))                   
                        #assign(paste("periods_",pfts[p],"_brick_",clim_models[climi],"_rcp6",sep=""), periods_veg_brick)
                        dif_periods_veg_brick <- brick(dif_periods, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
                        #plot(subset(dif_periods_veg_brick,5))
                        
                        if(first==1){
                        area_raster <- (area((subset(dif_periods_veg_brick,1))))
                        first<-0
                        }

                        for (d in (1:(numperiods-1))){ 
                            area_change <- cellStats((subset(dif_periods_veg_brick,d))*area_raster,'sum')
                            area_initial <- cellStats((subset(periods_veg_brick,1))*area_raster,'sum')
                            df_biome_area_change2 <- data.frame(decade=2020+(d*10),dif_biome_area=area_change,area_initial=area_initial,scen=clim_scen[sceni],dgvms_model=dgvms_long[dgvmsi],gcm_model=clim_models[climi],soc_scen=soc_scen[soci],PFT=pfts[p])
                            df_biome_area_change <-  rbind(df_biome_area_change,df_biome_area_change2)
                            
                        }
                        #assign(paste("dif_periods_",pfts[p],"_brick_rcp6",sep=""), dif_periods_veg_brick)
                        #years_veg[16:85]
                        #plot(t(subset(yearly_veg_brick,1)), main = "Boreal needleleaved evergreen 2021 (% in gridcell)")
                        #dpft <- t(subset(decadal_veg_brick,7)) - t(subset(decadal_veg_brick,1))
                        #plot( dpft, main = "% change in Boreal needleleaved evergreen \n under rcp6 (2090's-2020's)")

                        #yearly_veg_brick_10 <- brick(var_array[,,1:10], xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
                        save(periods_veg_brick,file=paste("Data/PFT_rasters/periods_",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_pft-",pfts[p],"_global_annual_2006_2099.Rdata",sep=""))
                        save(dif_periods_veg_brick,file=paste("Data/PFT_rasters/dif_periods_",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_pft-",pfts[p],"_global_annual_2006_2099.Rdata",sep=""))
                        #save(periods_veg_brick,file=paste("Data/PFT_rasters/periods_caraib_gfdl-esm2m_ewembi_rcp60_2005soc_co2_pft-brevdttht_global_annual_2006_2099.Rdata",sep=""))
                        #save(dif_periods_veg_brick,file=paste("Data/PFT_rasters/dif_periods_caraib_gfdl-esm2m_ewembi_rcp60_2005soc_co2_pft-brevdttht_global_annual_2006_2099.Rdata",sep=""))
                        print( ncname)
                    } 
                }
            }  
        }
        
    }
    
    write.csv(df_biome_area_change,"Data/biome_area.csv")
    #write.csv(df_biome_area_change,"Data/biome_area_nograss.csv")
    df_biome_area_change <- read.csv("Data/biome_area.csv")
    glimpse(df_biome_area_change)

    for (soci in 1:1){
        for (sceni in 1:length(clim_scen)){
            soc_scen <- c("2005soc",paste0(clim_scen[sceni],"soc"))
            #for (dgvmsi in 3:length(dgvms)){
            for (dgvmsi in 1:length(dgvms)){
                pfts <- eval(parse(text=paste0("pfts_",dgvms[dgvmsi])))
                for (climi in 1:length(clim_models)){
                    for (p in 1:length(pfts)){
                        dname <- paste("cveg-",pfts[p],sep="")
                        listoffiles <- list.files(paste0(dir1,"cveg_",dgvms[dgvmsi],"/"))
                        ncname <- paste(dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_cveg-",pfts[p],"_global_annual_2006_2099.nc4",sep="")
                        if(ncname %notin% listoffiles){
                            next
                        }


                        ncname <- paste(dir1,"cveg_",dgvms[dgvmsi],"/",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_cveg-",pfts[p],"_global_annual_2006_2099.nc4",sep="")
                        #ncname <- paste0(dir1,"cveg_",dgvms[dgvmsi],"/caraib_gfdl-esm2m_ewembi_rcp60_2005soc_co2_cvegbrevdttht_global_annual_2006_2099.nc4")
                     
                        ncin <- nc_open(ncname)
                        #print(ncin)    
                        #get units
                        lon <- ncvar_get(ncin,"lon")
                        lat <- ncvar_get(ncin,"lat")
                        time_veg <- ncvar_get(ncin,"time")
                        tunits_veg <- ncatt_get(ncin,"time","units")
                        years_veg <- 1661 + time_veg

                        
                        var_array <- ncvar_get(ncin,dname, start = c(1,1,1), count=c(-1,-1,-1)) #start, number in dimension to start. count: hopw many in that dimension
                        #glimpse(var_array)
                        #var_array <- var_array[,,16:85] #2021 to 2090
                        #numdecades <- floor(dim(var_array)[3]/10)
                        #decadal <- array(numeric(),c(720,360,numdecades)) 
                        #dif_decadal <- array(numeric(),c(720,360,numdecades-1)) 
                        #numperiods <- 4
                        numperiods <- 8
                        periods <- array(numeric(),c(360,720,numperiods)) 
                        dif_periods <- array(numeric(),c(360,720,numperiods-1)) 
                        #periods_minyear <- c(1,16,36,76) #years 2006,2021,2041,2081
                        #periods_maxyear <- c(15,35,55,94) #years 2020,2040,2060,2099
                        
                        periods_minyear <- c(1,25,35,45,55,65,75,85) #years 2006,2030,2040,2050,2060,2070,2080,2090
                        periods_maxyear <- c(15,34,44,54,64,74,84,94) #years 2020,2039,2949,2959,2969,2979,2989,2099
                        
                        for (d in (1:numperiods)){ 
                            period_array <- var_array[,,periods_minyear[d]:periods_maxyear[d]] #decade d of data
                            period_mean <- rowMeans(period_array, dims = 2, na.rm = TRUE)
                            if (dgvms[dgvmsi]=="lpj"){
                                periods[,,d] <- t(period_mean)
                            }else{
                                periods[,,d] <- t(period_mean)
                            }
                            
                            if (d>1){
                                dif_periods[,,d-1] <- periods[,,d] - periods[,,1]

                            }
                        }
                       
                        periods_veg_brick <- brick(periods, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
                        #plot(subset(periods_veg_brick,8))     
                        #plot(brick(var_array[,,c(1,56)]))                   
                        #assign(paste("periods_",pfts[p],"_brick_",clim_models[climi],"_rcp6",sep=""), periods_veg_brick)
                        dif_periods_veg_brick <- brick(dif_periods, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
                        #plot(subset(dif_periods_veg_brick,5))
                        
                        if(first==1){
                        area_raster <- (area((subset(dif_periods_veg_brick,1))))
                        first<-0
                        }

                        for (d in (1:(numperiods-1))){ 
                            area_change <- cellStats((subset(dif_periods_veg_brick,d))*area_raster,'sum')
                            area_initial <- cellStats((subset(periods_veg_brick,1))*area_raster,'sum')
                            df_biome_area_change2 <- data.frame(decade=2020+(d*10),dif_biome_area=area_change,area_initial=area_initial,scen=clim_scen[sceni],dgvms_model=dgvms_long[dgvmsi],gcm_model=clim_models[climi],soc_scen=soc_scen[soci],PFT=pfts[p])
                            df_biome_area_change <-  rbind(df_biome_area_change,df_biome_area_change2)
                            
                        }
                        #assign(paste("dif_periods_",pfts[p],"_brick_rcp6",sep=""), dif_periods_veg_brick)
                        #years_veg[16:85]
                        #plot(t(subset(yearly_veg_brick,1)), main = "Boreal needleleaved evergreen 2021 (% in gridcell)")
                        #dpft <- t(subset(decadal_veg_brick,7)) - t(subset(decadal_veg_brick,1))
                        #plot( dpft, main = "% change in Boreal needleleaved evergreen \n under rcp6 (2090's-2020's)")

                        #yearly_veg_brick_10 <- brick(var_array[,,1:10], xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
                        save(periods_veg_brick,file=paste("Data/cveg_rasters/periods_",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_cveg",pfts[p],"_global_annual_2006_2099.Rdata",sep=""))
                        save(dif_periods_veg_brick,file=paste("Data/cveg_rasters/dif_periods_",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_cveg",pfts[p],"_global_annual_2006_2099.Rdata",sep=""))
                        #save(periods_veg_brick,file=paste("Data/cveg_rasters/periods_caraib_gfdl-esm2m_ewembi_rcp60_2005soc_co2_cvegbrevdttht_global_annual_2006_2099.Rdata",sep=""))
                        #save(dif_periods_veg_brick,file=paste("Data/cveg_rasters/dif_periods_caraib_gfdl-esm2m_ewembi_rcp60_2005soc_co2_cvegbrevdttht_global_annual_2006_2099.Rdata",sep=""))
                        print( ncname)
                    } 
                }
            }  
        }
        
    }
#Read NetCDF of PFTs (close)

#Get temperature timeseries (start)

    df_tas = data.frame(decade=double(),scen=character(0),gcm_model=character(0),dif_t=double())
    first <- 1
    for (sceni in 1:3){
        
        for(climi in 1:length(clim_models)){

            tas_df <- read.table(paste0("Data/",clim_models[climi],"_",clim_scen[sceni],"_tas.txt"), header = FALSE, sep = "", dec = ".")

            for (i in 1:7){
                
                #meant <- tas_df$V2[c(which(tas_df$V1==(2020+(i-1)*10)):which(tas_df$V1==(2020+(i)*10)))]
                
                    base_t <- mean(tas_df$V2[c(which(tas_df$V1==(2006)):which(tas_df$V1==(2020)))])
                    mean_t <- mean(tas_df$V2[c(which(tas_df$V1==(2030+(i-1)*10)):which(tas_df$V1==(2030+(i)*9)))])
                    dif_t <- mean_t - base_t
                
                df_tas2 <- data.frame(decade=2030+(i-1)*10,scen=clim_scen[sceni],gcm_model=clim_models[climi],dif_t=dif_t)
                df_tas <- rbind(df_tas,df_tas2)


            }
            if(first==1){
                first <- 0
                Temp_all_mod <- data.frame(year=tas_df[,1],temp=tas_df[,2],model=clim_models[climi],scen=clim_scen[sceni])
            } else{
                Temp_all_mod <- rbind(Temp_all_mod,data.frame(year=tas_df[,1],temp=tas_df[,2],model=clim_models[climi],scen=clim_scen[sceni]))
            }
            
        }
    }

    Temp_all_scen <- aggregate(temp~year+scen,data=Temp_all_mod,FUN="mean")
    ggplot(Temp_all_scen,aes(x=year,y=temp,color=scen))+geom_line()

    ggplot(data=df_tas)+
    geom_line(aes(x=decade,y=dif_t,color=scen,linetype=factor(gcm_model)))+
    geom_hline(aes(yintercept=0),linetype="dashed",color="gray")+
    theme_bw()

#Get temperature timeseries (end)


# Plot biome area cover change against temperature (start)
    df_biome <- df_biome_area_change
    df_biome  <- aggregate(dif_biome_area ~ decade + scen + dgvms_model + gcm_model + soc_scen, data = df_biome, sum)

    df_biome2 <- df_biome_area_change
    df_biome2  <- aggregate(area_initial ~ decade + scen + dgvms_model + gcm_model + soc_scen, data = df_biome2, sum)
    glimpse(df_biome2)

    df_tas$id <- paste0(df_tas$gcm_model,df_tas$decade,df_tas$scen)



    df_biome$id <- paste0(df_biome$gcm_model,df_biome$decade,df_biome$scen)
    glimpse(df_tas)


    df_biome <- merge(df_biome,df_tas[,which(names(df_tas)%in%c("dif_t","id"))],all=FALSE)
    glimpse(df_biome)


    df_biome <- cbind(df_biome,df_biome2$area_initial)
    names(df_biome)[9] <- "area_initial"

    worldareasum <- cellStats(area_raster,'sum')
    df_biome$runs <- paste0(df_biome$gcm_model,df_biome$dgvms_model,df_biome$soc_scen,df_biome$scen)
    

    ggplot(df_biome,aes(x=dif_t,y=(100*dif_biome_area/area_initial),linetype=dgvms_model,color=scen,group=runs))+
    geom_line()+ylab("Biomes cover change (%)")+theme_bw()+xlab("Temperature change w.r.t. 2006-2020")+geom_hline(aes(yintercept=0),linetype="dashed")+
    geom_point(data=df_biome[which(df_biome$decade==2090),],aes(x=dif_t,y=(100*dif_biome_area/area_initial),shape=gcm_model))+
    ggtitle("")


    ggplot(df_biome,aes(x=dif_t,y=(100*dif_biome_area),color=scen,shape=dgvms_model))+
    geom_point()+theme_bw()+geom_hline(aes(yintercept=0),linetype="dashed")+
    ylab("Biomes cover change (%)")+xlab("Temperature change w.r.t. 2006-2020")+
    ggtitle("")


    model1 <- felm(dif_biome_area ~ 0+ dif_t + I(dif_t^2) + scen + soc_scen + gcm_model + dgvms_model| 0  | 0 | 0  , data=df_biome)
    summary(model1)
    df_estimates <- sqest(df_biome,model1,"dif_t","all")

    ggplot(df_estimates,aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Temperature change",y="Estimated Impact in Fisheries (%)")+
                    scale_color_manual(values=c("#d3818c","#7375a4")) + ggtitle("Impacts on fisheries")

    df_biome$area_change_percent <- 100*df_biome$dif_biome_area/df_biome$area_initial

    model_soc2005 <- felm(area_change_percent~ 0+ dif_t + I(dif_t^2) + gcm_model + dgvms_model| 0  | 0 | 0  , data=df_biome[which(df_biome$soc_scen=="2005soc" & df_biome$scen=="rcp60"),])
    df_estimates <- sqest(df_biome[which(df_biome$soc_scen=="2005soc" & df_biome$scen=="rcp60"),],model_soc2005,"dif_t","2005Soc_RCP60")

    model_socrcp60 <- felm(area_change_percent~ 0+ dif_t + I(dif_t^2) + gcm_model + dgvms_model| 0  | 0 | 0  , data=df_biome[which(df_biome$soc_scen=="rcp60soc" & df_biome$scen=="rcp60"),])
    df_estimates2 <- sqest(df_biome[which(df_biome$soc_scen=="rcp60soc" & df_biome$scen=="rcp60"),],model_socrcp60,"dif_t","RCP60Soc")

    model_soc2005_rcp26 <- felm(area_change_percent~ 0+ dif_t + I(dif_t^2) + gcm_model + dgvms_model| 0  | 0 | 0  , data=df_biome[which(df_biome$soc_scen=="2005soc" & df_biome$scen=="rcp26"),])
    df_estimates3 <- sqest(df_biome[which(df_biome$soc_scen=="2005soc" & df_biome$scen=="rcp26"),],model_soc2005_rcp26,"dif_t","2005Soc_RCP26")

    model_socrcp26 <- felm(area_change_percent~ 0+ dif_t + I(dif_t^2) + gcm_model + dgvms_model| 0  | 0 | 0  , data=df_biome[which(df_biome$soc_scen=="rcp26soc" & df_biome$scen=="rcp26"),])
    df_estimates4 <- sqest(df_biome[which(df_biome$soc_scen=="rcp26soc" & df_biome$scen=="rcp26"),],model_socrcp26,"dif_t","RCP26Soc")


    df_estimates <- rbind(df_estimates,df_estimates2,df_estimates3,df_estimates4)

    ggplot(df_estimates,aes(x=temp,y=gestimated,color=exp))+
                    geom_line(lwd=1.25)+ theme_bw()+
                    geom_ribbon(aes(ymin=ci2,ymax=ci1,fill=exp),color=NA,alpha=0.3)+
                    geom_hline(yintercept=0,lty=3)+
                    labs(x="Temperature change",y="Estimated Impact in Biomes area (%)")+
                    #scale_color_manual(values=c("#d3818c","#7375a4")) + 
                    ggtitle("Warming impacts on biomes cover")


# Plot biome area cover change against temperature (start)



# Map of cover change at temperature horizons (start)

        pft_file_names <- list.files("Data/PFT_rasters")
        class(pft_file_names)
        length(pft_file_names)
        pft_file_names <- discard_pattern(pft_file_names,c("rcp60soc"))
        #pft_file_names <- discard_pattern(pft_file_names,c("c3"))
        #pft_file_names <- discard_pattern(pft_file_names,c("c4"))
        pft_file_names <- discard_pattern(pft_file_names,c("rcp26soc"))
        #pft_file_names <- discard_pattern(pft_file_names,c("lpj"))
        pft_file_names <- keep_pattern(pft_file_names,c("dif"))
        pft_file_names <- keep_pattern(pft_file_names,c("car"))
        #pft_file_names <- discard_pattern(pft_file_names,c("orc"))
        temp_levels <- c(2)
        #temp_levels <- c(0.5)
        for (temp_levels_i in 1:length(temp_levels)){
        first <- 1
        temp_level <- temp_levels[temp_levels_i]
        for (i in 1:length(pft_file_names)){
            name <- paste0("Data/PFT_rasters/",pft_file_names[i])
            load(name)
            
            for (ii in 1:4){
                if(grepl(clim_models[ii],name)){clim_i <- ii}
                }

            for (ii in 1:3){
                if(grepl(clim_scen[ii],name)){scen_i <- ii}
                }
            

            temp <- df_tas[which(df_tas$scen==clim_scen[scen_i] & df_tas$gcm_model==clim_models[clim_i]),]

           
            # if(temp$dif_t[which.max(temp$dif_t-temp_level)]<0){
            #     decade_i <- which.max(temp$dif_t-temp_level)
            # } else{
            #     which.min((abs(temp$dif_t-temp_level)))

            # }

            decade_i <- which.min((abs(temp$dif_t-temp_level)))

        if (abs(temp$dif_t[decade_i]-temp_level) > 0.3){
            print(paste("next",i,"-temp:",temp_level,"-scen:",clim_scen[scen_i],"-model:",clim_models[clim_i]))
            next
            }
            
            #decade_i <- which(abs(temp$dif_t-temp_level)<0.2)
            if(identical(decade_i, integer(0))){
                    print(paste("identical next",i))
                    next
                    
                }else{
                    v1 <- subset(dif_periods_veg_brick,decade_i)
                    }

            for (ii in 1:3){
                if(grepl(dgvms[ii],name)){dgvm_ii <- ii}
                }
            

            if (first==0){
                        pft_sum <- pft_sum + v1
                        dgvm_count <- c(dgvm_count,dgvms[dgvm_ii])
                        } else {
                            first <- 0
                            pft_sum <-  v1
                            dgvm_count <- dgvms[dgvm_ii]
                        }
                        print(i)
        }


                    runs <- sum(table(dgvm_count)[which(names(table(dgvm_count))=="car")][1]/26,
                            table(dgvm_count)[which(names(table(dgvm_count))=="lpj")][1]/11,
                            table(dgvm_count)[which(names(table(dgvm_count))=="orc")][1]/10, na.rm=TRUE)
                    pft_sum <- pft_sum/(runs)
                    assign(paste0("pft_change_",temp_level,"C"),pft_sum)
        }


            #crs(pft_sum_rcp26) <- "+init=EPSG:4326"
            #pft_sum_rcp26<- projectRaster(pft_sum_rcp26, crs='+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
            
            
            
            # pft_sum_poly_rob1 <- 
            # raster::subset(pft_change_0.5C,1) %>% 
            # raster::rasterToPolygons() %>% 
            # sf::st_as_sf() 

            # pft_sum_poly_rob2 <- 
            # raster::subset(pft_change_1C,1) %>% 
            # raster::rasterToPolygons() %>% 
            # sf::st_as_sf() 

            # pft_sum_poly_rob3 <- 
            # raster::subset(pft_change_1.5C,1) %>% 
            # raster::rasterToPolygons() %>% 
            # sf::st_as_sf() 

            pft_sum_poly_rob4 <- 
            raster::subset(pft_change_2C,1) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 
            
            
            world_coast <- ne_coastline(scale = "medium", returnclass = "sf")
            
            library(wesanderson)
            pal <- palette(brewer.pal(n = 3, name = "Spectral"))

            # a <- ggplot() +
            # theme_void() +
            #  geom_sf(data = pft_sum_poly_rob1,aes(fill =100* layer.1), color = NA)+
            # scale_fill_gradientn(colours = pal,limits=c(-10,10),na.value="transparent",name="Land cover change (%)", oob = scales::squish)+
            
            # #scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
            # #                high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            # geom_sf(data = world_coast)+
            # ggtitle("+0.5C")+
            # theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            # b <- ggplot() +
            # theme_void() +
            # geom_sf(data = pft_sum_poly_rob2,aes(fill =100* layer), color = NA) +
            # scale_fill_gradientn(colours = pal,limits=c(-10,10),na.value="transparent",name="Land cover change (%)", oob = scales::squish)+
            # #scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
            #                 #high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            # geom_sf(data = world_coast)+
            # ggtitle("+1C")+
            # theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            # c <- ggplot() +
            # theme_bw() +
            # geom_sf(data = pft_sum_poly_rob2,aes(fill = 100*layer), color = NA) +
            # scale_fill_gradientn(colours = pal,limits=c(-10,10),na.value="transparent",name="Land cover change (%)", oob = scales::squish)+
            # geom_sf(data = world_coast)+
            # ggtitle("+1.5C")+
            # theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 
            
            d_area <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob4,aes(fill = 10000*layer), color = NA) +
            geom_sf(data = world_coast)+
            #scale_fill_gradient2(midpoint=0, low ="red" , mid = "white",high = "green",na.value="transparent",name="Land cover change (%)")+
            scale_fill_gradientn(colours = pal,limits=c(-10,10),na.value="transparent",name="Land cover change (%)", labels=c("<-10","-05","0","05",">10"),oob = scales::squish)+
            #scale_fill_distiller(palette = "Spectral", direction = 1,name="Land cover change (%)")+
            #scale_fill_npg()+
            geom_sf(data = world_coast)+
            ggtitle("+2C")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 
            d_area
            ggsave("Figures/Map_Areachange_2C_MEAN.png",dpi=600)

            leg <- get_legend(c)

            c <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob2,aes(fill =100* layer), color = NA) +
            scale_fill_gradientn(colours = pal,limits=c(-10,10),na.value="transparent",name="Land cover change (%)", oob = scales::squish)+
            #scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
             #               high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            geom_sf(data = world_coast)+
            ggtitle("+1.5C")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 
            
            ggarrange(a,b,c,d,ncol=1,nrow=4,legend.grob = leg,common.legend = TRUE, legend="bottom")
            #ggsave("Map_BiomeChange_TempHorizons_onlyClimate.png",dpi=600)
            
            ggarrange(b,c,d,ncol=1,nrow=3,legend.grob = leg,common.legend = TRUE, legend="bottom")
            #ggsave("Map_BiomeChange_TempHorizons_10_15_20.png",dpi=600)




        pft_file_names <- list.files("Data/cveg_rasters")
        class(pft_file_names)
        length(pft_file_names)
        pft_file_names <- discard_pattern(pft_file_names,c("rcp60soc"))
        #pft_file_names <- discard_pattern(pft_file_names,c("c3"))
        #pft_file_names <- discard_pattern(pft_file_names,c("c4"))
        pft_file_names <- discard_pattern(pft_file_names,c("rcp26soc"))
        #pft_file_names <- discard_pattern(pft_file_names,c("lpj"))
        pft_file_names <- keep_pattern(pft_file_names,c("dif"))
        pft_file_names <- keep_pattern(pft_file_names,c("orc"))
        #pft_file_names <- discard_pattern(pft_file_names,c("orc"))
        temp_levels <- c(2)
        #temp_levels <- c(0.5)
        for (temp_levels_i in 1:length(temp_levels)){
        first <- 1
        temp_level <- temp_levels[temp_levels_i]
        for (i in 1:length(pft_file_names)){
            name <- paste0("Data/cveg_rasters/",pft_file_names[i])
            load(name)
            
            for (ii in 1:4){
                if(grepl(clim_models[ii],name)){clim_i <- ii}
                }

            for (ii in 1:3){
                if(grepl(clim_scen[ii],name)){scen_i <- ii}
                }
            

            temp <- df_tas[which(df_tas$scen==clim_scen[scen_i] & df_tas$gcm_model==clim_models[clim_i]),]

           
            # if(temp$dif_t[which.max(temp$dif_t-temp_level)]<0){
            #     decade_i <- which.max(temp$dif_t-temp_level)
            # } else{
            #     which.min((abs(temp$dif_t-temp_level)))

            # }

            decade_i <- which.min((abs(temp$dif_t-temp_level)))

        if (abs(temp$dif_t[decade_i]-temp_level) > 0.3){
            print(paste("next",i,"-temp:",temp_level,"-scen:",clim_scen[scen_i],"-model:",clim_models[clim_i]))
            next
            }
            
            #decade_i <- which(abs(temp$dif_t-temp_level)<0.2)
            if(identical(decade_i, integer(0))){
                    print(paste("identical next",i))
                    next
                    
                }else{
                    v1 <- subset(dif_periods_veg_brick,decade_i)
                    }

            for (ii in 1:3){
                if(grepl(dgvms[ii],name)){dgvm_ii <- ii}
                }
            

            if (first==0){
                        pft_sum <- pft_sum + v1
                        dgvm_count <- c(dgvm_count,dgvms[dgvm_ii])
                        } else {
                            first <- 0
                            pft_sum <-  v1
                            dgvm_count <- dgvms[dgvm_ii]
                        }
                        print(i)
        }


                    runs <- sum(table(dgvm_count)[which(names(table(dgvm_count))=="car")][1]/26,
                            table(dgvm_count)[which(names(table(dgvm_count))=="lpj")][1]/11,
                            table(dgvm_count)[which(names(table(dgvm_count))=="orc")][1]/10, na.rm=TRUE)
                    pft_sum <- pft_sum/(runs)
                    assign(paste0("cveg_change_",temp_level,"C"),pft_sum)
        }


            #crs(pft_sum_rcp26) <- "+init=EPSG:4326"
            #pft_sum_rcp26<- projectRaster(pft_sum_rcp26, crs='+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
            
            
       

            cveg_sum_poly_rob4 <- 
            raster::subset(cveg_change_2C,1) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 
            
            dcveg <- ggplot() +
            theme_void() +
            geom_sf(data = cveg_sum_poly_rob4,aes(fill = 100*layer), color = NA) +
            geom_sf(data = world_coast)+
            #scale_fill_gradient2(midpoint=0, low ="red" , mid = "white",high = "green",na.value="transparent",name="Land cover change (%)")+
            scale_fill_gradientn(colours = pal,limits=c(-40,40),
            na.value="transparent",name="", labels=c("<-40","-20","0","20",">40"),
            oob = scales::squish)+
            #scale_fill_distiller(palette = "Spectral", direction = 1,name="Land cover change (%)")+
            #scale_fill_npg()+
            geom_sf(data = world_coast)+
            ggtitle("+2C")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 
            dcveg
            ggsave("Figures/Map_Cveg_2C_ORC.png",dpi=600)

            leg <- get_legend(c)

            c <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob2,aes(fill =100* layer), color = NA) +
            scale_fill_gradientn(colours = pal,limits=c(-10,10),na.value="transparent",name="Land cover change (%)", oob = scales::squish)+
            #scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
             #               high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            geom_sf(data = world_coast)+
            ggtitle("+1.5C")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 
            
            #ggsave("Map_BiomeChange_TempHorizons_onlyClimate.png",dpi=600)
            
            ggarrange(b,c,d,ncol=1,nrow=3,legend.grob = leg,common.legend = TRUE, legend="bottom")
# Map of cover change at different time horizons (end)


# Plot latitudes
    first <- 1
    horizon <- c("Present","2C")
        for (soci in 1:1){
            for (sceni in 1:length(clim_scen)){
                soc_scen <- c("2005soc",paste0(clim_scen[sceni],"soc"))
                #for (dgvmsi in 1:length(dgvms)){
                for (dgvmsi in 3:3){
                    pfts <- eval(parse(text=paste0("pfts_",dgvms[dgvmsi])))
                    for (climi in 1:length(clim_models)){

                        for(h in 1:2){

                            for (p in 1:length(pfts)){
                                dname <- paste("pft-",pfts[p],sep="")
                                listoffiles <- list.files("Data/PFT_rasters/")
                                ncname <- paste("periods_",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_pft-",pfts[p],"_global_annual_2006_2099.Rdata",sep="")
                                
                                
                                if(ncname %notin% listoffiles){
                                    skip_next <- 1
                                    next
                                }


                                v1 <- load(paste0("Data/PFT_rasters/",ncname))
                                print(ncname)
                                #assign(paste("periods_",pfts[p],"_brick_",clim_models[climi],"_rcp6",sep=""), eval(parse(text=v1)))
                                v1 <- eval(parse(text=v1))
                                #v1 <- t(v1)
                                
                                temp <- df_tas[which(df_tas$scen==clim_scen[sceni] & df_tas$gcm_model==clim_models[climi]),]

                                #decade_i <- which.min((abs(temp$dif_t-h)))
                                if(h==1){decade_i <- 1}else{
                                    
                                    temp_h<-h
                                    x <- temp$dif_t-temp_h
                                    if (min(x[x>0])==Inf){
                                        print(paste("next"))
                                        skip_next <- 1
                                        next
                                    }
                                    decade_i <- which(x==min(x[x>0]))
                                    
                                    }
                                
                                skip_next <- 0



                                # if (abs(temp$dif_t[decade_i]-temp_level) > 0.1){
                                #     print(paste("next",i))
                                #     next
                                # }
                                
                                # decade_i <- which(abs(temp$dif_t-temp_level)<0.1)
                                if(p==1){
                                    a <- as.data.frame(subset(v1,decade_i), xy=TRUE)
                                    names(a)[3] <- "Percent"
                                    a$PFT <- pfts[p]
                                    a$clim <-clim_models[climi]
                                    a$IDcell <- seq(1:dim(a)[1])
                                    a$DGVM <- dgvms[dgvmsi]
                                    a$soc <- soc_scen[soci]
                                }else{
                                    b <- as.data.frame(subset(v1,h), xy=TRUE)
                                    names(b)[3] <- "Percent"
                                    b$PFT <- pfts[p]
                                    b$clim <-clim_models[climi]
                                    b$IDcell <- seq(1:dim(b)[1])
                                    b$DGVM <- dgvms[dgvmsi]
                                    b$soc <- soc_scen[soci]
                                    a <- rbind(a,b)
                                }
                    }
                            if(skip_next==1){next}
                            cell_max <- aggregate(Percent~IDcell, data=a, FUN="max")
                            names(cell_max)[2] <- "maxpercent"
                            a2 <- merge(a,cell_max,by="IDcell",all=TRUE)
                            cell_max <- data.frame(IDcell=a2$IDcell[which(a2$Percent==a2$maxpercent)],dominant_pft=a2$PFT[which(a2$Percent==a2$maxpercent)])
                            cell_max <- cell_max[which(cell_max$IDcell %notin% cell_max$IDcell[duplicated(cell_max$IDcell)]),]
                            a <- merge(a,cell_max,by="IDcell",all.x=TRUE)
                            
                            a$ID <- seq(1:dim(a)[1])
                            if (first==1){
                                a_h <- a
                                a_h$horizon <- horizon[h]
                                first <- 0

                            }else{
                                a$horizon <- horizon[h]
                                a_h <- bind_rows(a_h,a)
                            }
                        }
                    }
                }
            }
        }
    
        glimpse(a_h)

        a_h_car <- a_h
        a_h <- bind_rows(a_h,a_h_car)
        save(a_h,file="Data/a_h_3dgvms.Rda")

        
        load("Data/a_h.Rda")

        levels(factor(a_h$horizon))
        levels(factor(a_h$DGVM))




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

        a_lpj_2005soc_present <- a_h[which(a_h$soc=="2005soc" & a_h$DGVM=="lpj"& a_h$h=="Present"),]
        a_lpj_2005soc_present$Latitude <- abs(a_lpj_2005soc_present$y)
        
        a_lpj_2005soc_present <- a_lpj_2005soc_present %>%
            mutate(dominant_pft=fct_reorder(.f = dominant_pft, .x = Latitude, .fun = mean))
            glimpse(a_lpj_2005soc_present)
                

        levels(a_lpj_2005soc_present$dominant_pft) <- c("Tropical broadleaved \n  evergreen",
        "Tropical broadleaved \n  evergreen (shade-intolerant)","Tropical Grass",
        "Tropical broadleaved \n  raingreen", "Temperate  broadleaved \n  evergreen","Temperate broadleaved \n summergreen",
        "Boreal/Temperate grass" , "Boreal/Temperate \n broadleaved (shade-intolerant)" , 
        "Boreal evergreen \n needleleaved (shade-intolerant)","Boreal evergreen \n needleleaved" ,"Boreal summergreen \n needleleaved")

        a_lpj_2005soc_present$PFT <- factor(a_lpj_2005soc_present$PFT)
        a_lpj_2005soc_present$PFT <- factor(a_lpj_2005soc_present$PFT, levels=c("bns","bne","bine","ibs",
        "c3g","tebs","tebe","trbr","c4g","tribe","trbe"))

        levels(a_lpj_2005soc_present$PFT) <- c("Boreal summergreen \n needleleaved",
        "Boreal evergreen \n needleleaved",
        "Boreal evergreen \n needleleaved (shade-intolerant)",
        "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
        "Boreal/Temperate grass" ,
        "Temperate broadleaved \n summergreen",   "Temperate  broadleaved \n  evergreen",
        "Tropical broadleaved \n  raingreen","Tropical Grass", "Tropical broadleaved \n  evergreen (shade-intolerant)", "Tropical broadleaved \n  evergreen")


        glimpse(a_lpj_2005soc_present)
        PFTs11_Clims4_horizons3_simple <- a_lpj_2005soc_present[,which(names(a_lpj_2005soc_present) %in% c("y","PFT","Percent"))]
        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3_simple[which(!is.nan(PFTs11_Clims4_horizons3_simple$Percent)),]
        glimpse(PFTs11_Clims4_horizons3_simple)
        PFTs11_horizons3 <- aggregate(Percent~PFT+y, FUN="mean", data=PFTs11_Clims4_horizons3_simple)
        glimpse(PFTs11_horizons3)

        pal_boreal <- colorRampPalette(brewer.pal(4, "PuBu"))
                    pal_temp <- colorRampPalette(brewer.pal(4, "RdPu"))
                    pal_trop <- colorRampPalette(brewer.pal(4, "Greens"))


        Present_dist_col <- ggplot(PFTs11_horizons3)+
            geom_col(aes(y,Percent*100,fill=PFT))+
            scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
            #scale_fill_brewer(palette = "Paired") +
            theme_minimal() + xlab("Latitude (Degrees)") + ylab("Average pixel cover (%)")+
            ggtitle("Present") + guides(fill=guide_legend(title="Biome")) + coord_flip()


        a_lpj_2005soc_2c <- a_h[which(a_h$soc=="2005soc" & a_h$DGVM=="lpj"& a_h$h=="2C"),]
        glimpse(a_lpj_2005soc_2c)

        a_lpj_2005soc_2c$PFT <- factor(a_lpj_2005soc_2c$PFT)
        a_lpj_2005soc_2c$PFT <- factor(a_lpj_2005soc_2c$PFT, levels=c("bns","bne","bine","ibs",
        "c3g","tebs","tebe","trbr","c4g","tribe","trbe"))

        levels(a_lpj_2005soc_2c$PFT) <- c("Boreal summergreen \n needleleaved",
        "Boreal evergreen \n needleleaved",
        "Boreal evergreen \n needleleaved (shade-intolerant)",
        "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
        "Boreal/Temperate grass" ,
        "Temperate broadleaved \n summergreen",   "Temperate  broadleaved \n  evergreen",
        "Tropical broadleaved \n  raingreen","Tropical Grass", "Tropical broadleaved \n  evergreen (shade-intolerant)", "Tropical broadleaved \n  evergreen")

        PFTs11_Clims4_horizons3_simple_2c <- a_lpj_2005soc_2c[,which(names(a_lpj_2005soc_2c) %in% c("y","PFT","Percent"))]
        PFTs11_Clims4_horizons3_simple_2c <- PFTs11_Clims4_horizons3_simple_2c[which(!is.nan(PFTs11_Clims4_horizons3_simple_2c$Percent)),]
        glimpse(PFTs11_Clims4_horizons3_simple_2c)
        PFTs11_horizons3_2c <- aggregate(Percent~PFT+y, FUN="mean", data=PFTs11_Clims4_horizons3_simple_2c)
        glimpse(PFTs11_horizons3_2c)
        glimpse(PFTs11_horizons3)

        


        PFTs11_horizons3$ID <- paste0(PFTs11_horizons3$PFT,PFTs11_horizons3$y)
        PFTs11_horizons3_2c$ID <- paste0(PFTs11_horizons3_2c$PFT,PFTs11_horizons3_2c$y)

        PFT_change <- merge(PFTs11_horizons3,PFTs11_horizons3_2c,by="ID",all=TRUE,suffixes=c("Present","2C"))
        glimpse(PFT_change)

        PFT_change$Future_Change <- PFT_change$Percent2C - PFT_change$PercentPresent
        
        

        total_change <-aggregate(Future_Change~y2C, FUN="sum", data=PFT_change)
        ggplot(total_change)+
        geom_smooth(aes(x=y2C,y=Future_Change))


            pft_mov_change <- ggplot(PFT_change)+
            geom_col(aes(yPresent,Future_Change*100,fill=PFT2C))+
            geom_hline(yintercept=0)+
            ggtitle("2 Degrees Warming")+
            theme_minimal() + xlab("") +
            theme(plot.title = element_text(hjust = 0.5)) + 
            ylab("Cover change (pp)") +
            scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
            #scale_fill_brewer(palette = "Paired") +
            #ylim(-30,30)+ 
            guides(fill=guide_legend(title="Biome"))+
            #geom_smooth(aes(x=y2C,y=Future_Change*100*10),se=F,color="black") + 
            coord_flip()

            

            ggarrange(Present_dist_col,pft_mov_change,ncol=2,nrow=1,common.legend=TRUE,legend="right")
            ggsave("Figures/Latitudinal_Distribution.png",dpi=600)
        
        a_orc_2005soc_present <- a_h[which(a_h$soc=="2005soc" & a_h$DGVM=="orc"& a_h$h=="Present"),]
        a_orc_2005soc_present$Latitude <- abs(a_orc_2005soc_present$y)
        
        a_orc_2005soc_present <- a_orc_2005soc_present %>%
            mutate(dominant_pft=fct_reorder(.f = dominant_pft, .x = Latitude, .fun = mean))
            glimpse(a_orc_2005soc_present)
                

       
        a_orc_2005soc_present$PFT <- factor(a_orc_2005soc_present$PFT)
        a_orc_2005soc_present$PFT <- factor(a_orc_2005soc_present$PFT, levels=c("bobrsu","bondev","bondsu","c3gra","tebrsu","tebrev","tendev",
                                           "c4gra","trbrev" ,"trbrrg"))

        levels(a_orc_2005soc_present$PFT) <- c("Boreal summergreen \n broadleleaved",
        "Boreal evergreen \n needleleaved",
        "Boreal summergreen \n needleleaved",
        "Boreal/Temperate grass" ,
        "Temperate broadleaved \n summergreen",   "Temperate  broadleaved \n  evergreen",
        "Temperate needleleaved \n  evergreen","Tropical Grass", "Tropical broadleaved \n  evergreen", "Tropical broadleaved \n raingreen")

        # Bare soil (bare); tropical broad-leaved evergreen (trbrev); tropical broad-leaved raingreen (trbrrg); 
        #temperate needleleaf evergreen (tendev); temperate broad-leaved evergreen (tebrev); temperate broad-leaved summergreen (tebrsu); 
        #boreal needleleaf evergreen (bondev); boreal broad-leaved summergreen (bobrsu); boreal needleleaf summergreen (bondsu); C3 natural grass (c3gra); 
        #C4 natural grass (c4gra); C3 winter crop (c3win); C3 summer crop (c3sum); C4 maize (c4mai); C4 other crops (c4oth); C3 pasture (c3pas); C4 pasture (c4pas)
        

        glimpse(a_orc_2005soc_present)
        PFTs11_Clims4_horizons3_simple <- a_orc_2005soc_present[,which(names(a_orc_2005soc_present) %in% c("y","PFT","Percent"))]
        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3_simple[which(!is.nan(PFTs11_Clims4_horizons3_simple$Percent)),]
        glimpse(PFTs11_Clims4_horizons3_simple)
        PFTs11_horizons3 <- aggregate(Percent~PFT+y, FUN="mean", data=PFTs11_Clims4_horizons3_simple)

    
        glimpse(PFTs11_horizons3)

        pal_boreal <- colorRampPalette(brewer.pal(4, "PuBu"))
                    pal_temp <- colorRampPalette(brewer.pal(4, "RdPu"))
                    pal_trop <- colorRampPalette(brewer.pal(4, "Greens"))


        Present_dist_col <- ggplot(PFTs11_horizons3)+
            geom_col(aes(y,Percent*100,fill=PFT))+
            scale_fill_manual(values = c(pal_boreal(3),pal_temp(4), pal_trop(3)))+
            #scale_fill_brewer(palette = "Paired") +
            theme_minimal() + xlab("Latitude (Degrees)") + ylab("Average pixel cover (%)")+
            ggtitle("Present (ORCHEED)") + guides(fill=guide_legend(title="Biome")) + coord_flip()


        a_orc_2005soc_2c <- a_h[which(a_h$soc=="2005soc" & a_h$DGVM=="orc"& a_h$h=="2C"),]
        glimpse(a_orc_2005soc_2c)

        a_orc_2005soc_2c$PFT <- factor(a_orc_2005soc_2c$PFT)
        a_orc_2005soc_2c$PFT <- factor(a_orc_2005soc_2c$PFT)
        a_orc_2005soc_2c$PFT <- factor(a_orc_2005soc_2c$PFT, levels=c("bobrsu","bondev","bondsu","c3gra","tebrsu","tebrev","tendev",
                                           "c4gra","trbrev" ,"trbrrg"))

        levels(a_orc_2005soc_2c$PFT) <- c("Boreal summergreen \n broadleleaved",
        "Boreal evergreen \n needleleaved",
        "Boreal summergreen \n needleleaved",
        "Boreal/Temperate grass" ,
        "Temperate broadleaved \n summergreen",   "Temperate  broadleaved \n  evergreen",
        "Temperate needleleaved \n  evergreen","Tropical Grass", "Tropical broadleaved \n  evergreen", "Tropical broadleaved \n raingreen")

        
        PFTs11_Clims4_horizons3_simple_2c <- a_orc_2005soc_2c[,which(names(a_orc_2005soc_2c) %in% c("y","PFT","Percent"))]
        PFTs11_Clims4_horizons3_simple_2c <- PFTs11_Clims4_horizons3_simple_2c[which(!is.nan(PFTs11_Clims4_horizons3_simple_2c$Percent)),]
        glimpse(PFTs11_Clims4_horizons3_simple_2c)
        PFTs11_horizons3_2c <- aggregate(Percent~PFT+y, FUN="mean", data=PFTs11_Clims4_horizons3_simple_2c)
        glimpse(PFTs11_horizons3_2c)
        glimpse(PFTs11_horizons3)

        


        PFTs11_horizons3$ID <- paste0(PFTs11_horizons3$PFT,PFTs11_horizons3$y)
        PFTs11_horizons3_2c$ID <- paste0(PFTs11_horizons3_2c$PFT,PFTs11_horizons3_2c$y)

        PFT_change <- merge(PFTs11_horizons3,PFTs11_horizons3_2c,by="ID",all=TRUE,suffixes=c("Present","2C"))
        glimpse(PFT_change)

         PFT_change$Future_Change <- PFT_change$Percent2C - PFT_change$PercentPresent
        
        

        total_change <-aggregate(Future_Change~y2C, FUN="sum", data=PFT_change)
        ggplot(total_change)+
        geom_smooth(aes(x=y2C,y=Future_Change))


            pft_mov_change <- ggplot(PFT_change)+
            geom_col(aes(yPresent,Future_Change*100,fill=PFT2C))+
            geom_hline(yintercept=0)+
            ggtitle("2 Degrees Warming (ORCHEED)")+
            theme_minimal() + xlab("") +
            theme(plot.title = element_text(hjust = 0.5)) + 
            ylab("Cover change (pp)") +
            scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
            #scale_fill_brewer(palette = "Paired") +
            #ylim(-30,30)+ 
            guides(fill=guide_legend(title="Biome"))+
            #geom_smooth(aes(x=y2C,y=Future_Change*100*10),se=F,color="black") + 
            coord_flip()

            

            ggarrange(Present_dist_col,pft_mov_change,ncol=2,nrow=1,common.legend=TRUE,legend="right")
            ggsave("Figures/Latitudinal_Distribution_orcheed.png",dpi=600)


                a_car_2005soc_present <- a_h[which(a_h$soc=="2005soc" & a_h$DGVM=="car"& a_h$horizon=="Present"),]
        a_car_2005soc_present$Latitude <- abs(a_car_2005soc_present$y)
        
        a_car_2005soc_present <- a_car_2005soc_present %>%
            mutate(dominant_pft=fct_reorder(.f = dominant_pft, .x = Latitude, .fun = mean))
            glimpse(a_car_2005soc_present)


            head(a_car_2005soc_present)
                

       
        a_car_2005soc_present$PFT <- factor(a_car_2005soc_present$PFT)
        levels(a_car_2005soc_present$PFT)
        a_car_2005soc_present$PFT <- factor(a_car_2005soc_present$PFT, levels=c("brsuas", "brsutecds","brevtecds","ndsutecdt","ndevtecdt","brsutecdt", "c3dh","c3hh","brsuteclt","ndevteclt",
        "brsutewmt","brsutewms","brevtewms","ndevstdit","ndsustswt","brevstdit","brevdttht",
        "c4h","brevxs","sds","ndevtedtt","ndevtedttht","brevdtt",
        "brrgtrt","brevtrt","trs"))
        #levels(a_car_2005soc_present$PFT) <-  a_car_2005soc_present$PFT

        # Bare soil (bare); tropical broad-leaved evergreen (trbrev); tropical broad-leaved raingreen (trbrrg); 
        #temperate needleleaf evergreen (tendev); temperate broad-leaved evergreen (tebrev); temperate broad-leaved summergreen (tebrsu); 
        #boreal needleleaf evergreen (bondev); boreal broad-leaved summergreen (bobrsu); boreal needleleaf summergreen (bondsu); C3 natural grass (c3gra); 
        #C4 natural grass (c4gra); C3 winter crop (c3win); C3 summer crop (c3sum); C4 maize (c4mai); C4 other crops (c4oth); C3 pasture (c3pas); C4 pasture (c4pas)
        
        PFTs11_Clims4_horizons3_simple <- a_car_2005soc_present[,which(names(a_car_2005soc_present) %in% c("y","PFT","Percent"))]
        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3_simple[which(!is.nan(PFTs11_Clims4_horizons3_simple$Percent)),]
        glimpse(PFTs11_Clims4_horizons3_simple)


        PFTs11_horizons3 <- aggregate(Percent~PFT+y, FUN="mean", data=PFTs11_Clims4_horizons3_simple)
        

        pal_boreal <- colorRampPalette(brewer.pal(4, "PuBu"))
                    pal_temp <- colorRampPalette(brewer.pal(4, "RdPu"))
                    pal_trop <- colorRampPalette(brewer.pal(4, "Greens"))


        Present_dist_col <- ggplot(PFTs11_horizons3)+
            geom_col(aes(y,Percent*100/2,fill=PFT))+
            scale_fill_manual(values = c(pal_boreal(10),pal_temp(6), pal_trop(10)))+
            #scale_fill_brewer(palette = "Paired") +
            theme_minimal() + xlab("Latitude (Degrees)") + ylab("Average pixel cover (%)")+
            ggtitle("Present (CARAIB)") + guides(fill=guide_legend(title="Biome")) + coord_flip()
      


        a_car_2005soc_2c <- a_h[which(a_h$soc=="2005soc" & a_h$DGVM=="car"& a_h$horizon=="2C"),]
      
        
        PFTs11_Clims4_horizons3_simple_2c <- a_car_2005soc_2c[,which(names(a_car_2005soc_2c) %in% c("y","PFT","Percent"))]
        PFTs11_Clims4_horizons3_simple_2c <- PFTs11_Clims4_horizons3_simple_2c[which(!is.nan(PFTs11_Clims4_horizons3_simple_2c$Percent)),]
        PFTs11_horizons3_2c <- aggregate(Percent~PFT+y, FUN="mean", data=PFTs11_Clims4_horizons3_simple_2c)
        
        PFTs11_horizons3$ID <- paste0(PFTs11_horizons3$PFT,PFTs11_horizons3$y)
        PFTs11_horizons3_2c$ID <- paste0(PFTs11_horizons3_2c$PFT,PFTs11_horizons3_2c$y)

        PFT_change <- merge(PFTs11_horizons3,PFTs11_horizons3_2c,by="ID",all=TRUE,suffixes=c("Present","2C"))
         twoc_dist_col <- ggplot(PFTs11_horizons3_2c)+
            geom_col(aes(y,Percent*100/2,fill=PFT))+
            scale_fill_manual(values = c(pal_boreal(10),pal_temp(6), pal_trop(10)))+
            #scale_fill_brewer(palette = "Paired") +
            theme_minimal() + xlab("Latitude (Degrees)") + ylab("Average pixel cover (%)")+
            ggtitle("2C (CARAIB)") + guides(fill=guide_legend(title="Biome")) + coord_flip()

         

         PFT_change$Future_Change <- PFT_change$Percent2C - PFT_change$PercentPresent
         
        

        total_change <-aggregate(Future_Change~y2C, FUN="sum", data=PFT_change)
    
        PFT_change$PFT2C <- factor(PFT_change$PFT2C)
        PFT_change$PFT2C <- factor(PFT_change$PFT2C, levels=c("brsuas", "brsutecds","brevtecds","ndsutecdt","ndevtecdt","brsutecdt", "c3dh","c3hh","brsuteclt","ndevteclt",
        "brsutewmt","brsutewms","brevtewms","ndevstdit","ndsustswt","brevstdit","brevdttht",
        "c4h","brevxs","sds","ndevtedtt","ndevtedttht","brevdtt",
        "brrgtrt","brevtrt","trs"))


            pft_mov_change <- ggplot(PFT_change)+
            geom_col(aes(yPresent,Future_Change*100,fill=PFT2C))+
            geom_hline(yintercept=0)+
            ggtitle("2 Degrees Warming (CARAIB)")+
            theme_minimal() + xlab("") +
            theme(plot.title = element_text(hjust = 0.5)) + 
            ylab("Cover change (pp)") +
            scale_fill_manual(values = c(pal_boreal(10),pal_temp(6), pal_trop(10)))+
            #scale_fill_brewer(palette = "Paired") +
            #ylim(-30,30)+ 
            guides(fill=guide_legend(title="Biome"))+
            #geom_smooth(aes(x=y2C,y=Future_Change*100*10),se=F,color="black") + 
            coord_flip()

            

           
            ggarrange(Present_dist_col ,pft_mov_change,ncol=2,nrow=1,common.legend=TRUE,legend="right")
            #ggsave("Figures/CARAIB_Latitude_change.png",dpi=500)





# Plot Latidudes

# Get Country Level Data
        
        first <- 1
        skip_next <- 0
        new_area_calculation<-0
        first_time_areacalc2 <- 0
        first_time_areacalc3 <- 0
        listoffiles <- list.files("Data/PFT_rasters/")
        counter <- 1
        #a <- as.data.frame(matrix(NA, nrow = (59199*220*11 + 80*75438*10), ncol = 13))
        
        for (dgvmsi in 1:3){
            if(dgvmsi ==2 & first_time_areacalc2 == 0){
                new_area_calculation <- 1
            }

            if(dgvmsi ==3 & first_time_areacalc3 == 0){
                new_area_calculation <- 1
            }

            for (soci in 1:1){
                for (sceni in 1:length(clim_scen)){
                    soc_scen <- c("2005soc",paste0(clim_scen[sceni],"soc"))
                
                    pfts <- eval(parse(text=paste0("pfts_",dgvms[dgvmsi])))
                    
                    for (climi in 1:length(clim_models)){

                        

                            for (p in 1:length(pfts)){
                                dname <- paste("pft-",pfts[p],sep="")
                                ncname <- paste("periods_",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_pft-",pfts[p],"_global_annual_2006_2099.Rdata",sep="")
                                
                                dnamecveg <- paste("cveg-",pfts[p],sep="")
                                ncnamecveg <- paste("periods_",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_cveg",pfts[p],"_global_annual_2006_2099.Rdata",sep="")
                                

                                if(ncname %notin% listoffiles){
                                    skip_next <- 1
                                    next
                                }


                                v1 <- load(paste0("Data/PFT_rasters/",ncname))
                                #assign(paste("periods_",pfts[p],"_brick_",clim_models[climi],"_rcp6",sep=""), eval(parse(text=v1)))
                                v1 <- eval(parse(text=v1))
                                #v1 <- t(v1)
                                v1cveg <- load(paste0("Data/cveg_rasters/",ncnamecveg))
                                #assign(paste("periods_",pfts[p],"_brick_",clim_models[climi],"_rcp6",sep=""), eval(parse(text=v1)))
                                v1cveg <- eval(parse(text=v1cveg))
                                
                                temp <- df_tas[which(df_tas$scen==clim_scen[sceni] & df_tas$gcm_model==clim_models[climi]),]

                                if(first==1){
                                    sw <- as_Spatial(st_cast(world))
                                    raster_isos <- raster::rasterize(sw[1],v1)
                                    isos_xy <- as.data.frame(raster_isos, xy=TRUE)
                                    names(isos_xy)[3] <- "countrycode"
                                    isos_xy$xy <- paste0(isos_xy$x,isos_xy$y)
                                    area <- as.data.frame(area(raster_isos), xy=TRUE)   
                                    isos_xy$area <- area[,3]
                                } else if (new_area_calculation == 1 & first_time_areacalc2==0){
                                    new_area_calculation<-0
                                    sw <- as_Spatial(st_cast(world))
                                    raster_isos <- raster::rasterize(sw[1],v1)
                                    isos_xy <- as.data.frame(raster_isos, xy=TRUE)
                                    names(isos_xy)[3] <- "countrycode"
                                    isos_xy$xy <- paste0(isos_xy$x,isos_xy$y)
                                    area <- as.data.frame(area(raster_isos), xy=TRUE)   
                                    isos_xy$area <- area[,3]
                                    first_time_areacalc2 <- 1
                                }  else if (new_area_calculation == 1 & first_time_areacalc3==0){
                                    new_area_calculation<-0
                                    sw <- as_Spatial(st_cast(world))
                                    raster_isos <- raster::rasterize(sw[1],v1)
                                    isos_xy <- as.data.frame(raster_isos, xy=TRUE)
                                    names(isos_xy)[3] <- "countrycode"
                                    isos_xy$xy <- paste0(isos_xy$x,isos_xy$y)
                                    area <- as.data.frame(area(raster_isos), xy=TRUE)   
                                    isos_xy$area <- area[,3]
                                    first_time_areacalc2 <- 1
                                }

                            for(h in 1:8){
                                decade_i <- h
                                skip_next <- 0



                                # if (abs(temp$dif_t[decade_i]-temp_level) > 0.1){
                                #     print(paste("next",i))
                                #     next
                                # }
                                
                                # decade_i <- which(abs(temp$dif_t-temp_level)<0.1)
                                if(first==1){
                                    a <- as.data.frame(subset(v1,decade_i), xy=TRUE)
                                    names(a)[3] <- "Percent"
                                    acveg <- as.data.frame(subset(v1cveg,decade_i), xy=TRUE)
                                    names(acveg)[3] <- "cveg"
                                    a$cveg <- acveg[,3]
                                    a$PFT <- pfts[p]
                                    a$clim <-clim_models[climi]
                                    a$IDcell <- seq(1:dim(a)[1])
                                    a$DGVM <- dgvms[dgvmsi]
                                    a$soc <- soc_scen[soci]
                                    a$scen <- clim_scen[sceni]
                                    a$temp <- c(0,temp$dif_t)[h]
                                    a$decade <- 2010+h*10
                                    a$area <- isos_xy$area
                                    a$countrycode <- isos_xy$countrycode
                                    a <- a[which(!is.nan(a$Percent)),]
                                    
                                    first <- 0
                                    counter <- counter + 1
                                }else{
                                    b <- as.data.frame(subset(v1,h), xy=TRUE)
                                    names(b)[3] <- "Percent"
                                    acveg <- as.data.frame(subset(v1cveg,decade_i), xy=TRUE)
                                    names(acveg)[3] <- "cveg"
                                    b$cveg <- acveg[,3]
                                    b$PFT <- pfts[p]
                                    b$clim <-clim_models[climi]
                                    b$IDcell <- seq(1:dim(b)[1])
                                    b$DGVM <- dgvms[dgvmsi]
                                    b$soc <- soc_scen[soci]
                                    b$scen <- clim_scen[sceni]
                                    b$temp <- c(0,temp$dif_t)[h]
                                    b$decade <- 2010+h*10
                                    b$area <- isos_xy$area
                                    b$countrycode <- isos_xy$countrycode
                                    b <- b[which(!is.nan(b$Percent)),]
                                    a <- bind_rows(a,b)
                                    print(counter)
                                    #print(dim(b)[2])
                                    # if(dgvmsi==1){
                                    #     a[(1+59199*(counter-1)):(59199*(counter)),] <- b

                                    # }else{
                                    #     a[(59199*220*11+1+75438*(counter-1)):(59199*220*11+75438*(counter)),] <- b
                                    # }
                                    
                                    counter <- counter +1
                                }
                    }



                    
                            #if(skip_next==1){next}
                            #cell_max <- aggregate(Percent~IDcell, data=a, FUN="max")
                            #names(cell_max)[2] <- "maxpercent"
                            #a2 <- merge(a,cell_max,by="IDcell",all=TRUE)
                            #cell_max <- data.frame(IDcell=a2$IDcell[which(a2$Percent==a2$maxpercent)],dominant_pft=a2$PFT[which(a2$Percent==a2$maxpercent)])
                            #cell_max <- cell_max[which(cell_max$IDcell %notin% cell_max$IDcell[duplicated(cell_max$IDcell)]),]
                            #a <- merge(a,cell_max,by="IDcell",all.x=TRUE)
                            
                            #a$ID <- seq(1:dim(a)[1])
                            
                        }
                    }
                }
            }
        }
        save(a,file="Data/PFT_all_data_3dgvms_cveg.Rda")
# Get Country Level Data



        # Read Database
            
            es_ac_notFood <- read.csv("es_ac_notFood_id.csv")
            dat<- es_ac_notFood
            glimpse(dat)
            levels(factor(dat$Ecosystem.Service.Category))
            #spatial extents are not standardized units - convert Hectares to Acres
            dat$Spatial.Extent[which(dat$Spatial.Unit.Published=="Hectares")]<-dat$Spatial.Extent[which(dat$Spatial.Unit.Published=="Hectares")]*2.471
            #limit to observations with spatial extent in either acres or hectares (now converted to acres)
            dat<- dat[which(dat$Spatial.Unit.Published%in% c("Acres","Hectares")),]
            
            # a1=ggplot(dat,aes(x=log(Spatial.Extent),y=log(Single.Value.Converted)))+geom_point()
            # a1=a1+geom_smooth(method="lm") + theme_bw()

            # x11()
            # a1
            # #dat$Category=ifelse(dat$Ecosystem.Service.Category=="Provisioning","Provisioning","Non-Provisioning")
            # b=ggplot(dat,aes(x=log(Spatial.Extent),y=log(Single.Value.Converted),col=Category))+geom_point()
            # b=b+geom_smooth(method="lm")
            # b+theme_bw()+xlab("Log Spatial extent") + ylab("Log value of benefits ($/ha)")
            # b
            # #look at linear model controlling for GDP
            # mod=lm(log(Single.Value.Converted)~log(Spatial.Extent)*Category+log(gdp_pc),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0))
            # elasticity_area <- summary(mod)$coefficients[2]
            # summary(mod)
            # # library("stargazer")
            # stargazer(mod,type="html",out="value-area.html")

            #mod=lm(log(Single.Value.Converted)~log(cveg_perarea)+log(gdp_pc),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0&Category=="Non-Provisioning"))
         
          
            dat$cveg <- (dat$cveg_total + dat$cveg_total_car + dat$cveg_total_orc)/3
            dat$pct_covered_mean <- (dat$pct_covered + dat$pct_covered_car + dat$pct_covered_orc)/3
            dat$cveg_perarea <- dat$cveg/dat$Spatial.Extent

            #   #summary(mod)
            # ggplot(dat)+
            # geom_point(aes(y=log(Single.Value.Converted),x=cveg_total),color="blue")+
            # geom_point(aes(y=log(Single.Value.Converted),x=cveg_total_car),color="red")+
            # geom_point(aes(y=log(Single.Value.Converted),x=cveg_total_orc),color="cyan")+
            # geom_point(aes(y=log(Single.Value.Converted),x=cveg),color="black",alpha=0.3)+geom_smooth(aes(y=log(Single.Value.Converted),x=cveg_total),method="lm")

            
            ggplot(dat)+geom_point(aes(y=log(Single.Value.Converted),x=log(cveg)),color="black",alpha=0.3)+
            geom_smooth(aes(y=log(Single.Value.Converted),x=log(cveg)),method="lm")
           
            # c1 <- hist(dat$cveg_total)
            # c2 <- hist(dat$cveg_total_car,add=TRUE)
            # c3 <- hist(dat$cveg_total_orc)
            # ggarrange(c1,c2,c3)

            
            max(dat$cveg,na.rm=TRUE)/min(dat$cveg,na.rm=TRUE)
            dat$area_covered <- dat$pct_covered_mean * dat$Spatial.Extent
            # modcveg=lm(log(Single.Value.Converted)~log(cveg)*Category+log(area_covered)*Category+log(gdp_pc),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0))
            # summary(modcveg)
            # elasticity_area_prov <- 0
            # elasticity_area_nonprov <- summary(modcveg)$coefficients[4]
            # elasticity_cveg_prov <- 0
            # elasticity_cveg_nonprov <- summary(modcveg)$coefficients[2]


            glimpse(dat)
            table(dat$Ecosystem.Service.Category)
            dat$Cat2 <- dat$Category
            dat$Cat2[dat$Cat2=="services"] <- "nonmarket"
            dat$Cat2[dat$Cat2=="nonuse"] <- "nonmarket"
           
            
            
            modcveg=lm(log(Single.Value.Converted)~log(cveg)*Cat2+log(Spatial.Extent)*Cat2+log(pct_covered_mean)+log(gdp_pc),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0))
            summary(modcveg)

            elasticity_area_market <- 0
            elasticity_area_non_use <- summary(modcveg)$coefficients[4]
            elasticity_cveg_market <- 0
            elasticity_cveg_non_use <-  summary(modcveg)$coefficients[2]
            dat_c <- dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0&Cat2=="nonmarket")
            dat_c <- dat_c[order(dat_c$Single.Value.Converted),]
            
            dat_c$ES_percinc <- dat_c$Single.Value.Converted/dat_c$Single.Value.Converted[1]
            
            dat_c <- dat_c[order(dat_c$cveg),]
            dat_c$cveg_percinc <- dat_c$cveg/dat_c$cveg[1]

            
            dat_c <- dat_c[order(dat_c$Spatial.Extent),]
            dat_c$area_percinc <- dat_c$Spatial.Extent/dat_c$Spatial.Extent[1]

            dat_c <- dat_c[which(dat_c$ES_percinc<quantile(dat_c$ES_percinc,0.95)),]
            
            
            elastcap_cveg <-ggplot(dat_c,aes(x=(cveg_percinc),y=(ES_percinc)))+geom_point()+geom_smooth(method="loess")+theme_bw()+
            geom_vline(aes(xintercept=40),linetype="dashed")+
            xlab("Percent increase in vegetation carbon")+
            ylab("Percent increase in ecosystem benefits value")+
            annotate("text", x=37, y=15000000, label="40% Increase", angle=90,size=5)
            elastcap_cveg
            ggsave("Figures/Cveg_Cap.png",dpi=400)

            elastcap_area <- ggplot(dat_c,aes(x=(area_percinc),y=(ES_percinc)))+geom_point()+geom_smooth(method="loess")+theme_bw()+
            #geom_vline(aes(xintercept=40),linetype="dashed")+
            xlab("Percent increase in Area")+
            ylab("Percent increase in ecosystem benefits value")+scale_x_continuous(trans = 'log')#+scale_y_continuous(trans = 'log')
            #annotate("text", x=33, y=15000000, label="40% Increase", angle=90,size=5)
            ggsave("Figures/Area_Cap.png",dpi=400)

            ggarrange(elastcap_area,elastcap_cveg,nrow=1,ncol=2)
             ggsave("Figures/AreaCveg_Cap.png",dpi=400)



        # Read Database



# Calculate Nat Cap Damage
            load(file="Data/PFT_all_data_3dgvms_cveg.Rda")
            
            levels(factor(a$DGVM))
            wealth2018 <- wealth_data[which(wealth_data$year==2018),]
                                    extra_ind_vars <- c("pct_covered","logGDP")
            
            wealth2018$country <- countrycode(wealth2018$countrycode,origin="iso3c",destination="iso2c")
            glimpse(wealth2018)
            a$PFT_area <- a$Percent * a$area #The variable "Percent" in reality is a fraction
            a$PFT <- factor(a$PFT)
            a$PFT_code <- a$PFT
            glimpse(a)
            
                        PFT_movement_lpj <- a[a$DGVM=="lpj",]
                        
                        PFT_movement_orc <- a[a$DGVM=="orc",]
                        
                        PFT_movement_car <- a[a$DGVM=="car",]

                     
            first <- 1
            first_rf <- 1
            
            for(j in 1:177){
            
                #for(typeESi in 1:3){
                    for (dgvmsi in 1:3){
                        if(length(which(wealth2018$country==isos[j]))==0){
                            print(paste("No World Bank Data for",isos[j]))
                            next
                                }
                        if(is.na(wealth2018$NforestES[which(wealth2018$country==isos[j])]) | is.na(wealth2018$GDP[which(wealth2018$country==isos[j])])){
                            print(paste("No World Bank Data for",isos[j]))
                            next
                                }
                                

                        if(dgvms[dgvmsi]=="lpj"){
                            PFT_movement <- PFT_movement_lpj
                        } else if(dgvms[dgvmsi]=="car"){
                            PFT_movement <- PFT_movement_car
                        } else if(dgvms[dgvmsi]=="orc"){
                             PFT_movement <- PFT_movement_orc
                        }
                        #glimpse(PFT_movement)
                        #PFT_movement <- a
                        if(isos[j]=="US"){
                        PFT_movement <- PFT_movement[which(PFT_movement$y<50 & PFT_movement$y>25),]
                        }

                        PFT_movement$PFT <- droplevels(PFT_movement$PFT)
                       


                        # if (dgvmsi ==1){
                        #     levels(PFT_movement$PFT) <- c("Boreal evergreen \n needleleaved (shade-intolerant)",
                        #     "Boreal evergreen \n needleleaved",
                        #     "Boreal summergreen \n needleleaved",
                        #     "Boreal/Temperate grass",
                        #     "Tropical Grass",
                        #     "Boreal/Temperate \n broadleaved (shade-intolerant)" ,
                        #     "Temperate  broadleaved \n  evergreen",
                        #     "Temperate broadleaved \n summergreen",
                        #     "Tropical broadleaved \n  evergreen" ,
                        #     "Tropical broadleaved \n  raingreen" ,
                        #     "Tropical broadleaved \n  evergreen (shade-intolerant)")
                        # }else {
                        #         levels(PFT_movement$PFT) <- c("Boreal summergreen \n broadleleaved",
                        #         "Boreal evergreen \n needleleaved",
                        #         "Boreal summergreen \n needleleaved",
                        #         "Boreal/Temperate grass" ,"Tropical Grass",
                        #         "Temperate  broadleaved \n  evergreen","Temperate broadleaved \n summergreen",   
                        #         "Temperate needleleaved \n  evergreen", "Tropical broadleaved \n  evergreen", "Tropical broadleaved \n raingreen")

                        # }
                            #erase_clim <- clim_models[clim_models %notin% clim_models[climi]]
                            #wealth2018_pft <- wealth2018_pft_all
                            
                            
                            
                            PFT_movement_c <- PFT_movement[which(PFT_movement$countrycode==isos[j]),]
                            PFT_movement_c_present <- PFT_movement_c[which(PFT_movement_c$decade==2020),]

                            levels_clim <- levels(factor(PFT_movement_c_present$clim))
                            #levels(factor(PFT_movement_c_present$soc))
                            levels_scen <- levels(factor(PFT_movement_c_present$scen))

                            # PFT_movement_c_present <- PFT_movement_c[which(PFT_movement_c_run$decade==2020),]
                            # areasPFT_present <- aggregate(PFT_area ~ PFT +PFT_code, data = PFT_movement_c_present, FUN = sum)
                            # condition_to_match <-areasPFT_present$PFT_code[which(areasPFT_present$PFT_area==max(areasPFT_present$PFT_area))]

                            # ps <- as.character(areasPFT_present$PFT_code[which(areasPFT_present$PFT_area>0)])
                            # if(length(ps)==0){
                            #     print("no present area")
                            #     next
                            # }

                            # ps <- paste0("PFT.",ps)
                            # ps_all <- paste(c(ps,extra_ind_vars),collapse="+")


                            # subset_es <-es_ac_notFood[which((eval(parse(text=paste("es_ac_notFood$PFT.",condition_to_match,sep="")))>0) & 
                            # es_ac_notFood$Ecosystem.Service.Category %in% "Provisioning"),]

                            # subset_es_np <-es_ac_notFood[which((eval(parse(text=paste("es_ac_notFood$PFT.",condition_to_match,sep="")))>0) & 
                            # es_ac_notFood$Ecosystem.Service.Category %in% c("Information" ,"Regulating"  , "Supporting")),]


                            # ind_vars <- subset_es[,names(subset_es)%in%c(ps,extra_ind_vars)]
                            # ind_vars_np <- subset_es_np[,names(subset_es_np)%in%c(ps,extra_ind_vars)]
                            # if(dim(ind_vars)[1]<6){
                            #     print(paste("Not enough observations in dataset for",isos[j]))
                            #     next
                            # }
                            # zer_ind <- lapply(ind_vars, function(x) all(x == 0))
                            # zeropfts <- names(unlist(zer_ind))[unlist(zer_ind)]
                            # trf <- tuneRF(ind_vars, subset_es$logESValue, trace=FALSE,plot=FALSE)
                            # trf_np <- tuneRF(ind_vars_np, subset_es_np$logESValue, trace=FALSE,plot=FALSE)
                        
                            
                            # mt <- trf[which.min(trf[,2]), 1]
                            # mt_np <- trf_np[which.min(trf_np[,2]), 1]
                            
                            # ntrees=200
                            
                            # Results_rf <- randomForest(ind_vars, subset_es$logESValue, importance = TRUE,tree = TRUE, mtry =mt, ntree = ntrees)
                            
                            
                            
                            # Results_rf_np <- randomForest(ind_vars_np, subset_es_np$logESValue, importance = TRUE,tree = TRUE, mtry =mt_np, ntree = ntrees)
                            # RF_rmse_run <- data.frame(rbind(data.frame(ntrees=seq(1:ntrees),mse=Results_rf$mse,type="Market"),data.frame(ntrees=seq(1:ntrees),mse=Results_rf_np$mse,type="Nonmarket")))
                            



                            # if(first_rf==1){
                                
                            #             RF_rmse <- RF_rmse_run
                            #             first_rf=0
                            # }else{
                                
                            #             RF_rmse <- bind_rows(RF_rmse,RF_rmse_run)

                            # }


                            #glimpse(es_ac_notFood)
                        
                            # if(dim(subset_es)[1] < 5){
                            #     print("subset less than 5 observations")
                            #     next
                            # }

                            # if(dim(subset_es_np)[1] < 5){
                            #     print("subset less than 5 observations")
                            #     next
                            # }
                                    

                            for (climi in 1:length(levels_clim)){
                                for (sceni in 1:length(levels_scen)){
                                    PFT_movement_c_run <- PFT_movement_c[which(PFT_movement_c$clim==levels_clim[climi] & PFT_movement_c$scen == levels_scen[sceni]),]
                                    if(dim(PFT_movement_c_run)[1]==0){
                                        print("next")
                                        next
                                    }
                                    
                                    PFT_movement_c_run_present <- PFT_movement_c_run[which(PFT_movement_c_run$decade==2020),]
                                    areasPFT_present <- aggregate(PFT_area ~ PFT +PFT_code, data = PFT_movement_c_run_present, FUN = sum)
                                    cvegPFT_present <- aggregate(cveg ~ PFT +PFT_code, data = PFT_movement_c_run_present, FUN = sum)
                                    condition_to_match <-areasPFT_present$PFT_code[which(areasPFT_present$PFT_area==max(areasPFT_present$PFT_area))]

                                    areasPFT_present_prov <- aggregate(PFT_area ~ PFT +PFT_code, data = PFT_movement_c_run_present, FUN = sum)
                                    #areasPFT_present_prov <- areasPFT_present_prov[-which(areasPFT_present_prov$PFT %in% c("c3g","c4g","c3gra","c4gra","c3hh","c3dh","c4h")),]
                                    condition_to_match_prov <-areasPFT_present_prov$PFT_code[which(areasPFT_present_prov$PFT_area==max(areasPFT_present_prov$PFT_area))]




                                    
                                    ps <- as.character(areasPFT_present$PFT_code[which(areasPFT_present$PFT_area>0)])
                                    ps_prov <- as.character(areasPFT_present_prov$PFT_code[which(areasPFT_present_prov$PFT_area>0)])
                                    if(length(ps)==0){
                                        print("no present area")
                                        next
                                    }

                                    ps <- paste0("PFT.",ps)
                                    ps_all <- paste(c(ps,extra_ind_vars),collapse="+")
                                    ps_prov <- paste0("PFT.",ps_prov)
                                    ps_all_prov <- paste(c(ps_prov,extra_ind_vars),collapse="+")

                                    pft_pct_pred_dgvm <- aggregate(Percent ~ PFT_code, data = PFT_movement_c_run_present, FUN = "mean")
                                    pft_pct_pred_dgvm <- pft_pct_pred_dgvm[which(paste0("PFT.",pft_pct_pred_dgvm$PFT_code) %in% c(ps_prov,extra_ind_vars)),]
                                    pft_pct_pred_dgvm$PFT_code<-paste0("PFT.",pft_pct_pred_dgvm$PFT_code)
                                    pft_pct_pred_dgvm<-pft_pct_pred_dgvm[order((c(ps_prov,extra_ind_vars))[1:(length(ps_prov))]),]
                                    input_var_model <- c(pft_pct_pred_dgvm[,2],log(wealth2018$GDP[which(wealth2018$country==isos[j])]),sum(pft_pct_pred_dgvm[,2]))

                                    subset_es <-es_ac_notFood[which(es_ac_notFood$Ecosystem.Service.Category %in% "Provisioning"),]
                                    
                                    dep_var <- subset_es[,names(subset_es)%in%c("logESValue")]

                                    ind_vars <- subset_es[,names(subset_es)%in%c(ps_prov,extra_ind_vars)]

                                    for (jj in 1:dim(ind_vars)[1]){
                                        
                                        if(jj==1){
                                            euclidian_distance_prov <- data.frame(obs=jj,dist=dist(rbind(input_var_model[1:(length(input_var_model)-2)],ind_vars[jj,1:(length(input_var_model)-2)]))[1])
                                        }else{
                                            euclidian_distance_prov <- rbind(euclidian_distance_prov,data.frame(obs=jj,dist=dist(rbind(input_var_model[1:(length(input_var_model)-2)],ind_vars[jj,1:(length(input_var_model)-2)]))[1]))

                                        }
                                    }

                                    ind_vars <-ind_vars[euclidian_distance_prov$obs[order(euclidian_distance_prov$dist)][1:100],]
                                    dep_var <- dep_var[euclidian_distance_prov$obs[order(euclidian_distance_prov$dist)][1:100]]
                                    complete <- complete.cases(ind_vars)
                                    ind_vars<-ind_vars[complete,]
                                    dep_var <- dep_var[complete]



                                    subset_es_np <-es_ac_notFood[which(es_ac_notFood$Ecosystem.Service.Category %in% c("Information" ,"Regulating"  , "Supporting")),]
                                    
                                    ind_vars_np <- subset_es_np[,names(subset_es_np)%in%c(ps_prov,extra_ind_vars)]
                                    
                                    dep_var_np <- subset_es_np[,names(subset_es_np)%in%c("logESValue")]

                                    for (jj in 1:dim(ind_vars_np)[1]){
                                        
                                        if(jj==1){
                                            euclidian_distance_np <- data.frame(obs=jj,dist=dist(rbind(input_var_model[1:(length(input_var_model)-2)],ind_vars_np[jj,1:(length(input_var_model)-2)]))[1])
                                        }else{
                                            euclidian_distance_np <- rbind(euclidian_distance_np,data.frame(obs=jj,dist=dist(rbind(input_var_model[1:(length(input_var_model)-2)],ind_vars_np[jj,1:(length(input_var_model)-2)]))[1]))

                                        }
                                    }

                                    ind_vars_np <-ind_vars_np[euclidian_distance_np$obs[order(euclidian_distance_np$dist)][1:300],]
                                    
                                    dep_var_np <- dep_var_np[euclidian_distance_np$obs[order(euclidian_distance_np$dist)]][1:300]
                                    # glimpse(ind_vars_np)
                                    # glimpse(dep_var_np)
                                    complete <- complete.cases(ind_vars_np)
                                    ind_vars_np<-ind_vars_np[complete,]
                                    dep_var_np<-dep_var_np[complete]






                                    # subset_es <-es_ac_notFood[which((eval(parse(text=paste("es_ac_notFood$PFT.",condition_to_match_prov,sep="")))>0) & 
                                    # es_ac_notFood$Ecosystem.Service.Category %in% "Provisioning"),]

                                    

                                    # subset_es_np <-es_ac_notFood[which((eval(parse(text=paste("es_ac_notFood$PFT.",condition_to_match,sep="")))>0) & 
                                    # es_ac_notFood$Ecosystem.Service.Category %in% c("Information" ,"Regulating"  , "Supporting")),]


                                    # #glimpse(es_ac_notFood)
                                
                                    # if(dim(subset_es)[1] < 5){
                                    #     print("subset less than 5 observations")
                                    #     next
                                    # }

                                    # if(dim(subset_es_np)[1] < 5){
                                    #     print("subset less than 5 observations")
                                    #     next
                                    # }
                                
                                    # if(length(ps)==1){
                                    #    print("Only one PFT")
                                    #    next
                                    # }

                                    #ind_vars_np <- subset_es_np[,names(subset_es_np)%in%c(ps,extra_ind_vars)]
                                    # if(dim(ind_vars)[1]<6){
                                    #     print(paste("Not enough observations in dataset for",isos[j]))
                                    #     next
                                    # }
                                    # #zer_ind <- lapply(ind_vars, function(x) all(x == 0))
                                    #zeropfts <- names(unlist(zer_ind))[unlist(zer_ind)]
                                    trf <- tuneRF(ind_vars, dep_var, trace=FALSE,plot=FALSE)
                                    trf_np <- tuneRF(ind_vars_np, dep_var_np, trace=FALSE,plot=FALSE)
                                
                                    
                                    mt <- trf[which.min(trf[,2]), 1]
                                    mt_np <- trf_np[which.min(trf_np[,2]), 1]
                                    
                                    Results_rf <- randomForest(ind_vars, dep_var, importance = TRUE,tree = TRUE, mtry =mt, ntree = 200)
                                    
                                    
                                    
                                    Results_rf_np <- randomForest(ind_vars_np, dep_var_np, importance = TRUE,tree = TRUE, mtry =mt_np, ntree = 200)
                                    
                                    #pft_pct_pred = aggregate(Percent  ~ horizon + PFT_code, data = PFT_movement2, FUN = "mean")
                                    #pft_pct_pred_dgvm <- pft_pct_pred[which(pft_pct_pred$horizon=="Present"),which(names(pft_pct_pred)%in% c("PFT_code","Percent"))]
                                    # pft_pct_pred_dgvm <- aggregate(Percent ~ PFT_code, data = PFT_movement_c_run_present, FUN = "mean")
                                    # pft_pct_pred_dgvm <- pft_pct_pred_dgvm[which(paste0("PFT.",pft_pct_pred_dgvm$PFT_code) %in% names(ind_vars)),]
                                    # pft_pct_pred_dgvm$PFT_code<-paste0("PFT.",pft_pct_pred_dgvm$PFT_code)
                                    # pft_pct_pred_dgvm<-pft_pct_pred_dgvm[order(names(ind_vars)[1:(length(ind_vars)-2)]),]

                                    # input_var_model <- c(pft_pct_pred_dgvm[,2],log(wealth2018$GDP[which(wealth2018$country==isos[j])]),sum(pft_pct_pred_dgvm[,2]))
                                    

                                    
                                    pft_pct_pred_database <- colMeans(ind_vars)
                                    
                                    eff_pft <- data.frame(matrix(NA, nrow =(length(pft_pct_pred_database)[1]-2), ncol = 2))
                                    names(eff_pft) <- c("variable","percent")
                                    eff_pft[,1] <- names(pft_pct_pred_database)[1:(length(pft_pct_pred_database)-2)]
                                    
                                    for(n in 1:(length(pft_pct_pred_database)-length(extra_ind_vars))){
                                        eff_pred <- pft_pct_pred_database
                                        eff_pred[n] <- eff_pred[n]+0.1
                                        eff_pft[n,2] <- exp(predict(Results_rf,eff_pred))
                                    }

                                    eff_pft$importance <- eff_pft[,2]/max(eff_pft[,2])


                                    pft_pct_pred_database <- colMeans(ind_vars_np)
                                    
                                    eff_pft_np <- data.frame(matrix(NA, nrow =(length(pft_pct_pred_database)[1]-2), ncol = 2))
                                    names(eff_pft_np) <- c("variable","percent")
                                    eff_pft_np[,1] <- names(pft_pct_pred_database)[1:(length(pft_pct_pred_database)-2)]
                                    
                                    for(n in 1:(length(pft_pct_pred_database)-length(extra_ind_vars))){
                                        eff_pred <- pft_pct_pred_database
                                        eff_pred[n] <- eff_pred[n]+0.1
                                        eff_pft_np[n,2] <- exp(predict(Results_rf_np,eff_pred))

                                    }

                                    eff_pft_np$importance_np <- eff_pft_np[,2]/max(eff_pft_np[,2])

                                    #pft_pct_pred_dgvm <- aggregate(Percent ~ PFT_code, data = PFT_movement_c_run_present, FUN = "mean")
                                    #pft_pct_pred_dgvm <- pft_pct_pred_dgvm[pft_pct_pred_dgvm$Percent>0,]
                                    #input_var_model <- c(pft_pct_pred_dgvm[,2],log(wealth2018$GDP[which(wealth2018$country==isos[j])]),sum(pft_pct_pred_dgvm[,2]))
                                    #pft_pct_pred_database <- colMeans(ind_vars)
                                    # pft_pct_pred_dgvm <- aggregate(Percent ~ PFT_code, data = PFT_movement_c_run_present, FUN = "mean")
                                    # pft_pct_pred_dgvm <- pft_pct_pred_dgvm[which(paste0("PFT.",pft_pct_pred_dgvm$PFT_code) %in% names(ind_vars_np)),]
                                    # pft_pct_pred_dgvm$PFT_code<-paste0("PFT.",pft_pct_pred_dgvm$PFT_code)
                                    # pft_pct_pred_dgvm<-pft_pct_pred_dgvm[order(names(ind_vars_np)[1:(length(ind_vars_np)-2)]),]

                                    # input_var_model <- c(pft_pct_pred_dgvm[,2],log(wealth2018$GDP[which(wealth2018$country==isos[j])]),sum(pft_pct_pred_dgvm[,2]))
                                    
                                    # pft_pct_pred_database <- colMeans(ind_vars_np)
                                    
                                    # eff_pft_np <- data.frame(matrix(NA, nrow =(length(input_var_model)[1]-2), ncol = 2))
                                    # names(eff_pft_np) <- c("variable","percent")
                                    # eff_pft_np[,1] <- names(pft_pct_pred_database)[1:(length(pft_pct_pred_database)-2)]
                                    
                                    # for(n in 1:(length(pft_pct_pred_database)-length(extra_ind_vars))){
                                    #     eff_pred_np <- input_var_model
                                    #     eff_pred_np[n] <- eff_pred_np[n]+0.1
                                    #     eff_pft_np[n,2] <- predict(Results_rf_np,eff_pred_np)
                                    # }

                                    # eff_pft_np$importance_np <- eff_pft_np[,2]/max(eff_pft_np[,2])




                                    # Get relative contribution
                                    NK_ForestT <- wealth2018$NforestT[which(wealth2018$country==isos[j])]
                                    NK_ForestES <- wealth2018$NforestES[which(wealth2018$country==isos[j])]
                                    NK_Npa <- wealth2018$Npa[which(wealth2018$country==isos[j])]
                                    pft_area_model <- aggregate(PFT_area ~ PFT_code, data = PFT_movement_c_run_present, FUN = sum)
                                    
                                    aggregate(area ~ temp+PFT_code, data = PFT_movement_c_run_present, FUN = sum)
                                    aggregate(PFT_area ~ temp, data = PFT_movement_c_run_present, FUN = sum)
                                    aggregate(Percent ~ temp+PFT_code, data = PFT_movement_c_run_present, FUN = max)
                                    max(PFT_movement_c_run_present$Percent)
                                    pft_area_model <-pft_area_model[which(paste0("PFT.",pft_area_model$PFT_code) %in% names(ind_vars)),]
                                    #pft_area_model <- pft_area_model[which(pft_area_model$PFT_area>0),]

                                    eff_pft <- eff_pft[order(eff_pft$variable,paste0("PFT.",pft_area_model$PFT_code)),]
                                    eff_pft$areas <- pft_area_model$PFT_area
                                    eff_pft$relative_contribution <- (eff_pft$areas * eff_pft$importance) / sum(eff_pft$areas * eff_pft$importance)
                                    eff_pft$es_val_per_area <- eff_pft$relative_contribution * NK_ForestT * 0.03 / sum(eff_pft$areas)

                                    #log(eff_pft$es_val_per_area)
                                    #sum(eff_pft$relative_contribution)
                                    
                                    pft_area_model <- aggregate(PFT_area ~ PFT_code, data = PFT_movement_c_run_present, FUN = sum)
                                    pft_area_model <-pft_area_model[which(paste0("PFT.",pft_area_model$PFT_code) %in% names(ind_vars_np)),]
                                     eff_pft_np <-  eff_pft_np[order( eff_pft_np$variable,paste0("PFT.",pft_area_model$PFT_code)),]
                                     eff_pft_np$areas <- pft_area_model$PFT_area
                                    #eff_pft$areas <- pft_area_model$PFT_area
                                    eff_pft_np$relative_contribution_np <- (eff_pft_np$areas * eff_pft_np$importance_np) / sum(eff_pft_np$areas * eff_pft_np$importance_np)
                                    eff_pft_np$es_val_per_area_np <- eff_pft_np$relative_contribution_np * (NK_ForestES+NK_Npa) * 0.03 / sum(eff_pft_np$areas)

                                    #+ 0.01*PFT_ES$ES_rel*PFT_ES$areas_change_pct_adjusted * elasticity_area


                                    Future_areas <- aggregate(PFT_area ~ PFT_code + temp, data = PFT_movement_c_run, FUN = "sum")
                                    #Future_cveg <- aggregate(cveg ~ PFT_code + temp, data = PFT_movement_c_run, FUN = "sum")
                                    Future_cveg <- aggregate(cveg ~ PFT_code + temp, data = PFT_movement_c_run, FUN = "mean")
                                    Future_areas$variable <- paste0("PFT.",Future_areas$PFT_code)
                                    Future_cveg$variable <- paste0("PFT.",Future_cveg$PFT_code)
                                    Future_areas <- merge(Future_areas,eff_pft,by="variable")
                                    Future_areas <- merge(Future_areas,Future_cveg[,3:4],by="variable")
                                    #glimpse(Future_areas)
                                    
                                    Future_areas$NK_Provisioning <- Future_areas$es_val_per_area * Future_areas$PFT_area / 0.03
                                    Future_areas_prov <- Future_areas
                                    FutureProv <- (aggregate(NK_Provisioning ~ temp , data=Future_areas, FUN = "sum")) 
                                    FutureProv$NK_Provisioning <- FutureProv$NK_Provisioning/FutureProv$NK_Provisioning[FutureProv$temp==0] 

                                    FutureProv_cveg <- (aggregate(cveg ~ temp , data=Future_cveg, FUN = "mean"))
                                    if(dgvms[dgvmsi]=="lpj"){FutureProv_cveg$cveg<-FutureProv_cveg$cveg*100}
                                    
                                    FutureProv_cveg$cveg_percchange <- 100*FutureProv_cveg$cveg/FutureProv_cveg$cveg[which(FutureProv_cveg$temp==0)]
                                    FutureProv_cveg$cveg_percchange[which(FutureProv_cveg$cveg_percchange>140)]<-140 #thats the range of the data used in the regression 
                                    

                                    FutureNonprov_area <- (aggregate(PFT_area ~ temp , data=Future_areas, FUN = "sum"))
                                    
                                    FutureNonprov_area$area_percchange <- 100*FutureNonprov_area$PFT_area/FutureNonprov_area$PFT_area[which(FutureNonprov_area$temp==0)]
                                    
                                    #FutureNonprov_area$area_percchange[which(FutureNonprov_area$area_percchange>40)]<-40

                                    Change_ES_np_area <- data.frame(temp=FutureProv_cveg$temp,changes_es_np_area = (1 + 0.01*((FutureNonprov_area$area_percchange-100) * elasticity_area_nonprov)+
                                                                        0.01*( FutureProv_cveg$cveg_percchange-100) * elasticity_cveg_nonprov))

                                    
                                    #
                                    Future_areas <- aggregate(PFT_area ~ PFT_code + temp, data = PFT_movement_c_run, FUN = "sum")
                                    #Future_cveg <- aggregate(cveg ~ PFT_code + temp, data = PFT_movement_c_run, FUN = "sum")
                                    Future_cveg <- aggregate(cveg ~ PFT_code + temp, data = PFT_movement_c_run, FUN = "mean")
                                    Future_areas$variable <- paste0("PFT.",Future_areas$PFT_code)
                                    Future_cveg$variable <- paste0("PFT.",Future_cveg$PFT_code)
                                    Future_areas <- merge(Future_areas,eff_pft_np,by="variable")
                                    Future_areas <- merge(Future_areas,Future_cveg[,3:4],by="variable")
                                    #
                                    Future_areas <- merge(Future_areas,Change_ES_np_area ,by="temp",all.x=TRUE)
                                    Future_areas$es_val_per_area_np_final <- Future_areas$es_val_per_area_np * Future_areas$changes_es_np_area
                                    
                                    Future_areas$NK_NonProvisioning <- Future_areas$es_val_per_area_np_final* Future_areas$PFT_area / 0.03
                                    FutureNonProv <- (aggregate(NK_NonProvisioning ~ temp , data=Future_areas, FUN = "sum")) 
                                    FutureNonProv$NK_NonProvisioning <- FutureNonProv$NK_NonProvisioning/FutureNonProv$NK_NonProvisioning[FutureNonProv$temp==0] 

                                    Future_areas[which(Future_areas$temp==0 & Future_areas$PFT_code=="brevdtt" ),]

                                    
                                    xadj <- NK_ForestT*0.03/sum(PFT_ES$es_val_per_area*PFT_ES$PFT_area)
                                    xadj_np <- (NK_ForestES+NK_Npa)*0.03/sum(PFT_ES$es_val_per_area_np_final*PFT_ES$PFT_area)
                                    
                                    PFT_ES <- aggregate(es_val_per_area_np_final~PFT_code,data=Future_areas[which(Future_areas$temp==0),],FUN="mean")
                                    PFT_ES$es_val_per_area_np_final <- PFT_ES$es_val_per_area_np_final*xadj_np
                                    PFT_ES[,(3)] <- aggregate(es_val_per_area~PFT_code,data=Future_areas_prov[which(Future_areas_prov$temp==0),],FUN="mean")[2]*xadj
                                    PFT_ES[,(4)] <- aggregate(PFT_area~PFT_code,data=Future_areas[which(Future_areas$temp==0),],FUN="mean")[2]
                                    PFT_ES[,(5)] <- aggregate(area ~ PFT_code , data = PFT_movement_c_run[which(PFT_movement_c_run$temp==0),], FUN = "sum")[2]
                                    PFT_ES$country <- isos[j]
                                    PFT_ES$dgvm <- dgvms[dgvmsi]
                    
                                    #PResent value of areas


                                    
                                    FutureProvFinal <- FutureProv[,c(1,2)]
                                    Future_N <- cbind(FutureProvFinal,NK_NonProvisioning = FutureNonProv[,2])
                                    Future_N$country <- isos[j]
                                    Future_N$dgvm <- dgvms[dgvmsi]
                                    Future_N$clim <- levels_clim[climi]
                                    Future_N$sceni <- levels_scen[sceni]
                                    Future_N$error_rf_p <- Results_rf$mse[200]^0.5
                                    Future_N$error_rf_np <- Results_rf_np$mse[200]^0.5
                                    Future_N$FractionDamage_NonProv_area <- 0.01*((FutureNonprov_area$area_percchange-100) * elasticity_area_nonprov)
                                    Future_N$FractionDamage_NonProv_cveg <- 0.01*( FutureProv_cveg$cveg_percchange-100) * elasticity_cveg_nonprov
                                    RF_rmse_run <- data.frame(rbind(data.frame(ntrees=seq(1:200),mse=Results_rf$mse,type="Market",run=first),data.frame(ntrees=seq(1:200),mse=Results_rf_np$mse,type="Nonmarket",run=first)))
                            
                                    


                                    if(first==1){
                                        NK_allcountries <- Future_N
                                        RF_rmse <- RF_rmse_run
                                        PFT_ES_all <- PFT_ES

                                        
                                        first <- first+1
                                    } else{
                                        NK_allcountries <- bind_rows(NK_allcountries,Future_N)
                                        RF_rmse <- bind_rows(RF_rmse,RF_rmse_run)
                                        PFT_ES_all <- bind_rows(PFT_ES_all,PFT_ES)
                                        first <- first+1

                                    }
                                    print(paste0(isos[j],j))
                                }
                            }
                    }
            }

            #sum(PFT_movement_c_run$area[which(PFT_movement_c_run$temp==0 & PFT_movement_c_run$PFT_code=="c3hh" & PFT_movement_c_run$clim=="miroc5" & PFT_movement_c_run$scen=="rcp60")])
            #save(NK_allcountries,file="Data/NK_allcountries_3dgvms_CVEGcap_FixedRF_euclidean_exp.Rda")
           # save(PFT_ES_all,file="Data/PFT_ES_allcountries_3dgvms_CVEGcap_FixedRF_euclidean_exp.Rda")
            load("Data/PFT_ES_allcountries_3dgvms_CVEGcap_FixedRF_euclidean_exp.Rda")
            load("Data/NK_allcountries_3dgvms_CVEGcap_FixedRF_euclidean_exp.Rda")
            
            NK <- merge(NK_allcountries,world[,which(names(world) %in% c("iso_a2","region_un"))], by.x="country",by.y="iso_a2")
            NK <- NK[which(NK$NK_Provisioning<quantile(NK$NK_NonProvisioning,0.9999)),]

            Regional_damagefunction_Market <- ggplot(NK)+
            #geom_point(aes(x=temp,y=NK_Provisioning,color=continent))+
            #geom_smooth(aes(x=temp,y=NK_Provisioning,color=region_un),method="lm",fomrula="y~ poly(x,2)")+
            geom_smooth(aes(x=temp,y=NK_Provisioning,color=region_un),method="loess")+
            geom_hline(aes(yintercept=1),linetype="dashed")+
            theme_bw()+
            xlab("Temperature Change (C)")+
            ylab("Change from intial value")+
            ggtitle("Market Natural Capital")+ labs(color= "Region")+
            coord_cartesian(ylim=c(0.75,1.1))

            Regional_damagefunction_NonMarket <- ggplot(NK)+
            #geom_point(aes(x=temp,y=NK_Provisioning,color=region_un),alpha=0.02)+
            geom_smooth(aes(x=temp,y=NK_NonProvisioning,color=region_un),method="loess")+
            geom_hline(aes(yintercept=1),linetype="dashed")+
            theme_bw()+
            xlab("Temperature Change (C)")+
            ylab("Change from intial value")+
            ggtitle("Nonmarket Natural Capital") + labs(color= "Region")+
            coord_cartesian(ylim=c(0.75,1.1))


            ggarrange(Regional_damagefunction_Market,Regional_damagefunction_NonMarket,common.legend=TRUE,legend="right")
            ggsave("Figures/RegionalDamageFunctions_FINAL1_2.png",dpi=600)

            # Figure ES values per ha
                 pft_es_lpj <- PFT_ES_all[which(PFT_ES_all$dgvm=="lpj"),]
                glimpse( pft_es_lpj)
                pft_es_lpj <- aggregate(es_val_per_area_np_final~country+PFT_code,data=pft_es_lpj,FUN="mean")
                pft_es_lpj$es_val_per_area <- aggregate(es_val_per_area~country+PFT_code,data=PFT_ES_all[which(PFT_ES_all$dgvm=="lpj"),],FUN="mean")[,3]

                pft_es_lpj$PFT_code <- droplevels(pft_es_lpj$PFT_code)
                pft_es_lpj$PFT_code <- factor(pft_es_lpj$PFT_code,levels=c("trbe","tribe","trbr","c4g","tebe","tebs","ibs","c3g","bine","bne", "bns"))
                
                pft_es_lpj<-rbind(data.frame(Biome=pft_es_lpj$PFT_code,Benefits=pft_es_lpj$es_val_per_area_np_final,Type="Non-market"),
                                data.frame(Biome=pft_es_lpj$PFT_code,Benefits=pft_es_lpj$es_val_per_area,Type="Market"))

                ggplot(pft_es_lpj)+
                geom_boxplot(aes(x=(Benefits/100),y=Biome,
                    fill=Type,middle=median((Benefits/100)))) +
                theme_bw()+ coord_cartesian(xlim=c(0,200))
                ggsave("Figures/ValperHa_LPJ.png",dpi=600)

                pft_es_car<- PFT_ES_all[which(PFT_ES_all$dgvm=="car"),]
                pft_es_car$PFT_code <- droplevels(pft_es_car$PFT_code)
                pft_es_car$PFT_code <- factor(pft_es_car$PFT_code, levels=c("brsuas", "brsutecds","brevtecds","ndsutecdt","ndevtecdt","brsutecdt", "c3dh","c3hh","brsuteclt","ndevteclt",
                "brsutewmt","brsutewms","brevtewms","ndevstdit","ndsustswt","brevstdit","brevdttht",
                "c4h","brevxs","sds","ndevtedtt","ndevtedttht","brevdtt",
                "brrgtrt","brevtrt","trs"))
                
                glimpse(PFT_ES_all)
                pft_es_car<-rbind(data.frame(Biome=pft_es_car$PFT_code,Benefits=pft_es_car$es_val_per_area_np_final,Type="Non-market"),data.frame(Biome=pft_es_car$PFT_code,Benefits=pft_es_car$es_val_per_area,Type="Market"))
                
                glimpse(pft_es_car)

                ggplot(pft_es_car)+
                geom_boxplot(,aes(x=(Benefits/100),y=Biome,
                    fill=factor(Type),middle=mean((Benefits/100)))) +
                theme_bw()+ coord_cartesian(xlim=c(0,300))


                pft_es_orc<- PFT_ES_all[which(PFT_ES_all$dgvm=="orc"),]
                pft_es_orc$PFT_code <- droplevels(pft_es_orc$PFT_code)
                pft_es_orc$PFT_code <- factor(pft_es_orc$PFT_code, levels=c("bobrsu","bondev","bondsu","c3gra","tebrsu","tebrev","tendev",
                                            "c4gra","trbrev" ,"trbrrg"))

                
                pft_es_orc<-rbind(data.frame(Biome=pft_es_orc$PFT_code,Benefits=pft_es_orc$es_val_per_area_np_final,Type="Non-market"),data.frame(Biome=pft_es_orc$PFT_code,Benefits=pft_es_orc$es_val_per_area,Type="Market"))

                glimpse(pft_es_orc)
                ggplot(pft_es_orc)+
                geom_boxplot(aes(x=(Benefits/100),y=Biome,
                    fill=factor(Type),middle=mean((Benefits/10)))) +
                theme_bw()+ coord_cartesian(xlim=c(0,300))




                glimpse(NK)
                glimpse(PFT_ES_all)
                
                ggplot(PFT_ES_all[which(PFT_ES_all$horizon=="Present" & PFT_ES_all$TypeES !="All"),]) +
                #ggplot(PFT_ES_all) +
                geom_boxplot(outlier.shape = NA,aes(x=(ES_rel),y=PFT,fill=TypeES,middle=mean((ES_rel)))) +
                theme_bw() + coord_cartesian(xlim=c(0,300))
            # Figure ES values per ha

            #Figure Total Values per Continent
                glimpse(PFT_ES_all)
                glimpse(NK)

                PFT_ES_all <- PFT_ES_all[which(PFT_ES_all$es_val_per_area_np_final<quantile(PFT_ES_all$es_val_per_area_np_final,0.95)),]
                PFT_ES_all <- PFT_ES_all[which(PFT_ES_all$es_val_per_area<quantile(PFT_ES_all$es_val_per_area,0.95)),]
                PFT_ES_all <- merge(PFT_ES_all,world[,which(names(world) %in% c("iso_a2","region_un"))], by.x="country",by.y="iso_a2")
                PFT_ES_all$TotalValES <- PFT_ES_all$es_val_per_area_np_final*PFT_ES_all$PFT_area
                PFT_ES_all$TotalValGDP <- PFT_ES_all$es_val_per_area*PFT_ES_all$PFT_area
                
                val_lpj <- aggregate(TotalValES~PFT_code+country,data= PFT_ES_all[which(PFT_ES_all$dgvm=="lpj"),], FUN="sum")
                val_lpj$TotalValGDP <- aggregate(TotalValGDP~PFT_code+country,data= PFT_ES_all[which(PFT_ES_all$dgvm=="lpj"),], FUN="sum")[,3]
                
                glimpse(wealth2018)
                glimpse(val_lpj)
                wealth2018$countrycode2 <- countrycode(wealth2018$countrycode,origin="iso3c",destination="iso2c")
                val_lpj <- merge(val_lpj,wealth2018[,which(names(wealth2018) %in% c("countrycode2","r5","GDP"))], by.x="country",by.y="countrycode2")
                
                val_lpj$TotalValES_gdp <- val_lpj$TotalValES/val_lpj$GDP
                
                 
                #  glimpse(PFT_movement_lpj)
                #  sum(PFT_movement_lpj$area[which(PFT_movement_lpj$countrycode=="MX" & 
                #  PFT_movement_lpj$PFT=="bne" &
                #  PFT_movement_lpj$clim=="hadgem2-es"&
                #  PFT_movement_lpj$soc=="2005soc"&
                #  PFT_movement_lpj$scen=="rcp60"&
                #  PFT_movement_lpj$decade==2020
                #  )]) # area is in km^2 not ha


                val_lpj$TotalValGDP_gdp <- val_lpj$TotalValGDP/val_lpj$GDP
                val_lpj$region_un <- val_lpj$r5

                glimpse(val_lpj)

                ES_val_lpj <- aggregate(TotalValES_gdp~PFT_code+region_un,data= val_lpj, FUN="mean")
                    ES_val_lpj$TotalValGDP_gdp <- aggregate(TotalValGDP_gdp~PFT_code+region_un,data= val_lpj, FUN="mean")[,3]

                    glimpse(ES_val_lpj)
                    ES_val_lpj$PFT_code <- factor(ES_val_lpj$PFT_code,levels=c("bns","bne","bine", "c3g","ibs",
                "tebs","tebe","c4g","trbr","tribe","trbe"))

                ES_val_lpj$region_un <- factor(ES_val_lpj$region_un)
                levels(ES_val_lpj$region_un) <- c("ASIA","LAM", "MAF","OECD","REF")
                ES_val_lpj$region_un <- factor(ES_val_lpj$region_un,levels=c("OECD","REF","LAM", "ASIA", "MAF" ))
              
                    ESValplot <-  ggplot(ES_val_lpj,aes(x=region_un,y=100*TotalValES_gdp,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                        geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
                geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("") + ggtitle("Ecosystem services")+
                    guides(fill = guide_legend(reverse=FALSE)) 
                       ESValplot

                ES_val_lpj2 <- ES_val_lpj
                ES_val_lpj2$region_un <- factor(ES_val_lpj2$region_un)
                ES_val_lpj2$region_un <- factor(ES_val_lpj2$region_un,levels=c("OECD","REF","LAM", "ASIA", "MAF" ))
            

                GDPValplot <-  ggplot(ES_val_lpj2,aes(x=region_un,y=TotalValGDP_gdp*100,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
                geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("Value of benefits (as % of GDP)") + ggtitle("Market goods")+
                    guides(fill = guide_legend(reverse=FALSE))

                ggarrange(GDPValplot,ESValplot,common.legend=TRUE,legend="right")
                ggsave("Figures/Benefits_per_continent_lpj_r5.png",dpi=600)
                    
                #         ES_val_car <- aggregate(TotalValES_gdp~PFT_code+region_un,data= PFT_ES_all[which(PFT_ES_all$dgvm=="car"),], FUN="mean")
                #         ES_val_car$TotalValGDP_gdp <- aggregate(TotalValGDP_gdp~PFT_code+region_un,data= PFT_ES_all[which(PFT_ES_all$dgvm=="car"),], FUN="mean")[,3]

                #         glimpse(ES_val_car)
                #         ES_val_car$PFT_code <- factor(ES_val_car$PFT_code,levels=c("brsuas", "brsutecds","brevtecds","ndsutecdt","ndevtecdt","brsutecdt", "c3dh","c3hh","brsuteclt","ndevteclt",
                #     "brsutewmt","brsutewms","brevtewms","ndevstdit","ndsustswt","brevstdit","brevdttht",
                #     "c4h","brevxs","sds","ndevtedtt","ndevtedttht","brevdtt",
                #     "brrgtrt","brevtrt","trs"))

                #     ES_val_car$region_un <- factor(ES_val_car$region_un,levels=c("Europe","Oceania", "Americas","Asia","Africa"))
                
                #     ESValplot <-  ggplot(ES_val_car,aes(x=region_un,y=100*TotalValES_gdp/2,fill=PFT_code))+ #Divided by two becase CAR has 2 levels of vegetation
                #     geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(10),pal_temp(6), pal_trop(10)))+
                #     geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("") + ggtitle("Ecosystem services")+
                #         guides(fill = guide_legend(reverse=FALSE)) 

                #     ES_val_car2<- ES_val_car
                #     ES_val_car2$region_un <- factor(ES_val_car2$region_un,levels=c("Europe","Asia", "Americas","Oceania", "Africa" ))
            

                #   GDPValplot <-  ggplot(ES_val_car2,aes(x=region_un,y=TotalValGDP_gdp*100,fill=PFT_code))+ #Divided by two becase CAR has 2 levels of vegetation
                #     geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(10),pal_temp(6), pal_trop(10)))+
                #     geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("Value of benefits (as % of GDP)") + ggtitle("Market goods")+
                #         guides(fill = guide_legend(reverse=FALSE))

                #     ggarrange(GDPValplot,ESValplot,common.legend=TRUE,legend="right")
                #     ggsave("Benefits_per_continent_car.png",dpi=600)


                #     ES_val_orc <- aggregate(TotalValES_gdp~PFT_code+region_un,data= PFT_ES_all[which(PFT_ES_all$dgvm=="orc"),], FUN="mean")
                #     ES_val_orc$TotalValGDP_gdp <- aggregate(TotalValGDP_gdp~PFT_code+region_un,data= PFT_ES_all[which(PFT_ES_all$dgvm=="orc"),], FUN="mean")[,3]

                #     glimpse(ES_val_orc)
                #     ES_val_orc$PFT_code <- factor(ES_val_orc$PFT_code,levels=c("bobrsu","bondev","bondsu","c3gra","tebrsu","tebrev","tendev",
                #                                "c4gra","trbrev" ,"trbrrg"))

                #     ES_val_orc$region_un <- factor(ES_val_orc$region_un,levels=c("Europe","Asia", "Americas", "Africa","Oceania"))
                
                #     ESValplot <-  ggplot(ES_val_orc,aes(x=region_un,y=100*TotalValES_gdp,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                #     geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(3),pal_temp(3), pal_trop(4)))+
                #     geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("") + ggtitle("Ecosystem services")+
                #         guides(fill = guide_legend(reverse=FALSE)) 

                #     ES_val_orc2<- ES_val_orc
                #     ES_val_orc2$region_un <- factor(ES_val_orc2$region_un,levels=c("Europe","Asia","Oceania","Americas", "Africa"))
                

                #   GDPValplot <-  ggplot(ES_val_orc2,aes(x=region_un,y=TotalValGDP_gdp*100,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                #     geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(3),pal_temp(4), pal_trop(3)))+
                #     geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("Value of benefits (as % of GDP)") + ggtitle("Market goods")+
                #         guides(fill = guide_legend(reverse=FALSE))

                #     ggarrange(GDPValplot,ESValplot,common.legend=TRUE,legend="right")
                #     ggsave("Benefits_per_continent_orc.png",dpi=600)
                

            #Figure Total Values per Continent

        


            isos <- (levels(factor(NK$country))) #139
            NK$Omega_NProv <- NK$NK_NonProvisioning - 1
            NK$Omega_Prov <- NK$NK_Provisioning - 1
            
            NK$error_rf_p_w <- 1/NK$error_rf_p
            NK$error_rf_np_w <- 1/NK$error_rf_np
            isos <- levels(factor(NK$country))
            glimpse(NK)  
            glimpse(a)          
            
            tempdecade <- a[a$DGVM=="lpj" & a$PFT=="bne" & a$IDcell==9635,]
            glimpse(tempdecade)
            NK$tempid <- paste0(NK$clim,NK$scen,NK$temp)
            tempdecade$tempid <- paste0(tempdecade$clim,tempdecade$scen,tempdecade$temp)
            tempdecade <- tempdecade[,c(15,12)]
            NK <- merge(NK,tempdecade,by="tempid",all.x=TRUE)
            glimpse(NK)  
            save(NK,file="Data/DamageNK_Temp.Rda")

            NK_temp <- NK[,c(2,3,6,7,8,9,10,19,18,17,16,15)]
            glimpse(NK_temp)
            names(NK_temp)[c(6,7,9,10,11,12)] <- c("error_rf_mN","error_rf_nN","error_rf_nN_w","error_rf_mN_w","Omega_mN","Omega_nN")
            
            save(NK_temp,file="Data/DamageNK_Temp.Rda")
           
           load(file=paste0(dir,"DamageNK_Temp.Rda"))


            results_df <- data.frame(coef = numeric(), se = numeric(),pval=numeric(), country = character(), formula = character(), capital = character(), dgvm=character(),stringsAsFactors = F)
            formulas <- c("I(temp^2)", "log(temp + 1)", " temp")
            acronyms <- c("sq", "log", "lin")
            NK_temp_all <- NK_temp
            country_list <- levels(factor(NK_temp_all$country))
            dgvmodel <- c("lpj","orc","car","all")
            
        for (c in country_list){
            NK_temp_c <- NK_temp_all[NK_temp_all$country==c,]
            country <- c
            if(is.nan(NK_temp_c$Omega_nN[1]) | is.nan(NK_temp_c$Omega_mN[1])){next}

                # country_damagefunction <- ggplot(NK_temp_c,aes(color=country))+
                # geom_point(aes(x=temp,y=Omega_mN,color=country,shape=dgvm),alpha=0.5)+
                # geom_point(aes(x=temp,y=Omega_mN,color=country,shape=dgvm),color="darkblue",alpha=0.5)+
                # geom_smooth(aes(x=temp,y=Omega_mN,color=country,weight = error_rf_mN),method="lm",formula="y~0+I(x^2)",color="darkblue",linetype=2)+
                # geom_point(aes(x=temp,y=Omega_nN,color=country,shape=dgvm),color="indianred",alpha=0.5)+
                # geom_smooth(aes(x=temp,y=Omega_nN,color=country,weight = error_rf_nN),method="lm",formula="y~ 0+ I(x^2)",color="indianred",linetype=2)+
                # geom_smooth(aes(x=temp,y=Omega_mN,color=country,weight = error_rf_mN),method="lm",formula="y~0+log(x+1)",color="darkblue")+
                # geom_smooth(aes(x=temp,y=Omega_nN,color=country,weight = error_rf_nN),method="lm",formula="y~ 0+log(x+1)",color="indianred")+
                # geom_smooth(aes(x=temp,y=Omega_mN,color=country,weight = error_rf_mN),method="lm",formula="y~0+x",color="darkblue",linetype=3)+
                # geom_smooth(aes(x=temp,y=Omega_nN,color=country,weight = error_rf_nN),method="lm",formula="y~ 0+x",color="indianred",linetype=3)+
                # theme_bw() +
                # scale_color_manual(name='Type of Natural Capital',
                #      labels=c('Market', 'Nonmarket'),
                #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
                # ggtitle(paste("Damage Functions. Country = ",country))+
                # xlab("Temperature Change (relative to 20006-2020)")+
                # ylab("Natural Capital Change")
                # country_damagefunction
                # ggsave(country_damagefunction, file=paste0("Figures/CountriesDamFun/All_data_damagefunction_", country,".png"), dpi=300)
                


            for(i in 1:length(formulas)){
                for (j in 1:length(dgvmodel)){

                    if(dgvmodel[j]=="all"){
                        fe <- "| decade+clim+sceni+dgvm|0|0"
                        NK_temp <- NK_temp_c
                    }else{
                        NK_temp <- NK_temp_c[which(NK_temp_c$dgvm==dgvmodel[j]),]
                        fe <- "| decade+clim+sceni|0|0"
                    }

                    

                    model_NK_temp_m <- felm(formula(paste("Omega_mN ~", formulas[i],fe)),weights=(1/NK_temp$error_rf_mN),data=NK_temp)
            
                
                    coef_m <- coef(model_NK_temp_m)
                    se_m <- sqrt(diag(vcov(model_NK_temp_m)))
                    results_df <- rbind(results_df, data.frame(coef=coef_m[1], se=se_m[1],pval=summary(model_NK_temp_m)$coef[4], country=country, formula=acronyms[i], capital="mN", dgvm=dgvmodel[j], stringsAsFactors = F))
                    
                    model_NK_temp_n <- felm(formula(paste("Omega_nN ~", formulas[i],fe)),weights=(1/NK_temp$error_rf_nN),data=NK_temp)
                    coef_n <- coef(model_NK_temp_n)
                    se_n <- sqrt(diag(vcov(model_NK_temp_n)))
                    
                    results_df <- rbind(results_df, data.frame(coef=coef_n[1], se=se_n[1],pval=summary(model_NK_temp_n)$coef[4], country=country, formula=acronyms[i], capital="nN", dgvm=dgvmodel[j],stringsAsFactors = F))
                
                    
                }
                }
        }

        results_df$iso3 <- countrycode(results_df$country, origin="iso2c",destination="iso3c")
        glimpse(results_df)
        
        write.csv(results_df,"Damage_coef.csv")
            
            # for (i in 1:length(levels(factor(NK$country)))){
            #     #for (i in 1:15){
            #     NK_c <- NK[NK$country==isos[i],]
            #     if(is.nan(NK_c$Omega_NProv[1]) | is.nan(NK_c$Omega_Prov[1])){next}
            #     if(isos[i]=="CZ"){
            #         NK_c <- NK_c[which(abs(NK_c$Omega_Prov)<quantile(abs(NK_c$Omega_Prov),0.9)),]

            #     }

            #     #NK_c <- NK_c[which(NK_c$sceni!="rcp85" & NK_c$clim %in% c("gfdl-esm2m","ipsl-cm5a-lr")),]
            #     country_damagefunction <- ggplot(NK_c,aes(color=country))+
            #     geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),alpha=0.5)+
            #     geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue",alpha=0.5)+
            #     #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~0+ I(x^2)",color="darkblue")+
            #     geom_smooth(aes(x=temp,y=Omega_Prov,color=country,weight = error_rf_p_w),method="lm",formula="y~0+I(x^2)",color="darkblue",linetype=2)+
            #     #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
            #     geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred",alpha=0.5)+
            #     #geom_smooth(aes(x=temp,y=Omega_NProv,color=country,weight = error_rf_np_w,linetype=dgvm),method="lm",formula="y~ 0+ I(x^2)",color="indianred")+
            #     geom_smooth(aes(x=temp,y=Omega_NProv,color=country,weight = error_rf_np_w),method="lm",formula="y~ 0+ I(x^2)",color="indianred",linetype=2)+
            #     geom_smooth(aes(x=temp,y=Omega_Prov,color=country,weight = error_rf_p_w),method="lm",formula="y~0+log(x+1)",color="darkblue")+
            #     geom_smooth(aes(x=temp,y=Omega_NProv,color=country,weight = error_rf_np_w),method="lm",formula="y~ 0+log(x+1)",color="indianred")+
            #     #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
            #     theme_bw() +
            #     scale_color_manual(name='Type of Natural Capital',
            #          labels=c('Market', 'Nonmarket'),
            #          values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
            #     # scale_linetype_manual(name='Function',
            #     #     labels=c('Squared', 'Logarithmic'),
            #     #    values=c("dashed","dashed"))+
            #     ggtitle(paste("Damage Functions. Squared (dashed) and Log (solid) Functions. Country = ",isos[i]))+
            #     xlab("Temperature Change (relative to 20006-2020)")+
            #     ylab("Natural Capital Change")
            #     country_damagefunction

            #     # country_damagefunction <- ggplot(NK_c,aes(color=country))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
            #     # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~0+ I(x^2)",color="darkblue")+
            #     # geom_smooth(aes(x=temp,y=Omega_Prov,color=country,weight = error_rf_p_w),method="lm",formula="y~0+I(x^2)",color="darkblue")+
            #     # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
            #     # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
            #     # geom_smooth(aes(x=temp,y=Omega_NProv,color=country,weight = error_rf_np_w),method="lm",formula="y~ 0+ I(x^2)",color="indianred")+
            #     # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
            #     # theme_bw() +
            #     # scale_color_manual(name='Type of Natural Capital',
            #     #      breaks=c('Market', 'Nonmarket'),
            #     #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
            #     # ggtitle(paste("Natural Capital Damage Function. Squared 2 coefficients. Country = ",isos[i]))+
            #     # xlab("Temperature Change (relative to 20006-2020)")+
            #     # ylab("Natural Capital Change")



            #     # country_damagefunction_Market2coeff <- ggplot(NK_c,aes(color=country))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
            #     # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~0+ I(x^2)",color="darkblue")+
            #     # geom_smooth(aes(x=temp,y=Omega_Prov,color=country,weight = error_rf_p_w),method="lm",formula="y~x+I(x^2)",color="darkblue")+
            #     # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
            #     # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
            #     # geom_smooth(aes(x=temp,y=Omega_NProv,color=country,weight = error_rf_np_w),method="lm",formula="y~ x+ I(x^2)",color="indianred")+
            #     # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
            #     # theme_bw() +
            #     # scale_color_manual(name='Type of Natural Capital',
            #     #      breaks=c('Market', 'Nonmarket'),
            #     #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
            #     # ggtitle(paste("Natural Capital Damage Function. Squared 2 coefficients. Country = ",isos[i]))+
            #     # xlab("Temperature Change (relative to 20006-2020)")+
            #     # ylab("Natural Capital Change")


            #     # country_damagefunction_LOG2coeff <- ggplot(NK_c[NK_c$temp>0,],aes(color=country))+
            #     # #country_damagefunction_LOG <- ggplot(NK_c,aes(color=country))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
            #     # geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~1+(x)+log(x+1)",color="darkblue")+
            #     # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
            #     # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
            #     # geom_smooth(aes(x=temp,y=Omega_NProv,color=country),method="lm",formula="y~ 1+(x)+log(x+1)",color="indianred")+
            #     # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
            #     # theme_bw() +
            #     # scale_color_manual(name='Type of Natural Capital',
            #     #      breaks=c('Market', 'Nonmarket'),
            #     #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
            #     # ggtitle(paste("Natural Capital Damage Function. Log 2 coefficients. Country = ",isos[i]))+
            #     # xlab("Temperature Change (relative to 20006-2020)")+
            #     # ylab("Natural Capital Change")

            #     # country_damagefunction_LOG <- ggplot(NK_c[NK_c$temp>0,],aes(color=country))+
            #     # #country_damagefunction_LOG <- ggplot(NK_c,aes(color=country))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
            #     # geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~0+log(x+1)",color="darkblue")+
            #     # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
            #     # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
            #     # geom_smooth(aes(x=temp,y=Omega_NProv,color=country),method="lm",formula="y~ 0+log(x+1)",color="indianred")+
            #     # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
            #     # theme_bw() +
            #     # scale_color_manual(name='Type of Natural Capital',
            #     #      breaks=c('Market', 'Nonmarket'),
            #     #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
            #     # ggtitle(paste("Natural Capital Damage Function. Log 2 coefficients. Country = ",isos[i]))+
            #     # xlab("Temperature Change (relative to 20006-2020)")+
            #     # ylab("Natural Capital Change")




            #     # country_damagefunction_Market_loess <- ggplot(NK_c,aes(color=country))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
            #     # geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="loess",formula="y~0+ x",color="darkblue")+
            #     # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
            #     # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
            #     # geom_smooth(aes(x=temp,y=Omega_NProv,color=country),method="loess",formula="y~ 0+ x",color="indianred")+
            #     # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
            #     # theme_bw() +
            #     # scale_color_manual(name='Type of Natural Capital',
            #     #      breaks=c('Market', 'Nonmarket'),
            #     #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
            #     # ggtitle(paste("Natural Capital Damage Function. Country = ",isos[i]))+
            #     # xlab("Temperature Change (relative to 20006-2020)")+
            #     # ylab("Natural Capital Change")

            #     NK_c_equal <- NK_c[which(NK_c$sceni!="rcp85" & NK_c$clim %in% c("gfdl-esm2m","ipsl-cm5a-lr")),]
            #     # NK_c_equal <- NK_c[which(NK_c$sceni!="rcp85" ),]
            #     # #max(NK_c_equal$temp)
            #     # #NK_c_equal <- NK_c[which(NK_c$temp <3),]
            #     # country_damagefunction_Market_equal <- ggplot(NK_c_equal,aes(color=country))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
            #     # geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~0 + I(x^2)",color="darkblue")+
            #     # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
            #     # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
            #     # geom_smooth(aes(x=temp,y=Omega_NProv,color=country),method="lm",formula="y~ 0+ I(x^2)",color="indianred")+
            #     # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
            #     # theme_bw() +
            #     # scale_color_manual(name='Type of Natural Capital',
            #     #      breaks=c('Market', 'Nonmarket'),
            #     #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
            #     # ggtitle(paste("Natural Capital Damage Function. Country = ",isos[i]))+
            #     # xlab("Temperature Change (relative to 20006-2020)")+
            #     # ylab("Natural Capital Change")


            #     # country_damagefunction_Market_equal_quadratic <- ggplot(NK_c_equal,aes(color=country))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
            #     # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
            #     # geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~0+ I(x^2)",color="darkblue")+
            #     # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
            #     # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
            #     # geom_smooth(aes(x=temp,y=Omega_NProv,color=country),method="lm",formula="y~0+ I(x^2)",color="indianred")+
            #     # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
            #     # theme_bw() +
            #     # scale_color_manual(name='Type of Natural Capital',
            #     #      breaks=c('Market', 'Nonmarket'),
            #     #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
            #     # ggtitle(paste("Natural Capital Damage Function. Country = ",isos[i]))+
            #     # xlab("Temperature Change (relative to 20006-2020)")+
            #     # ylab("Natural Capital Change")


                

            #     model_NKMarket <- felm(Omega_Prov ~ 0 + I(temp^2)|clim+sceni+dgvm|0|0,data=NK_c)
            #     model_NKMarket <- felm(Omega_Prov ~ 0 + I(temp^2)|decade+clim+sceni+dgvm|0|0,data=NK_c)
            #     #summary(model_NKMarket)
            #     #model_NKMarket <- lm(Omega_Prov ~ 0 + I(temp^2),weights=(1/NK_c$error_rf_p),data=NK_c)

                
            #     model_NKMarket_eq <- felm(Omega_Prov ~ 0 + I(temp^2)|decade+clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_p),data=NK_c_equal)
            #     #summary(model_NKMarket_eq)
                
            #     #model_NKNonMarket <- lm(Omega_NProv ~ 0 + I(temp^2),weights=(1/NK_c$error_rf_np),data=NK_c)
            #     model_NKNonMarket <- felm(Omega_NProv ~ 0 + I(temp^2)|decade+clim+sceni+dgvm|0|0,weights=(1/NK_c$error_rf_np),data=NK_c)
            #     #summary(model_NKNonMarket)

                
            #     model_NKNonMarket_eq <- felm(Omega_NProv ~ 0 + I(temp^2)|decade+clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_p),data=NK_c_equal)
            #     #summary(model_NKNonMarket_eq)


                
            #     #model_NKMarket_log <- lm(Omega_Prov ~ 0 + log(temp+1),weights=(1/NK_c$error_rf_p),data=NK_c)
            #     model_NKMarket_log <- felm(Omega_Prov ~ 0 + log(temp+1)|decade+clim+sceni+dgvm|0|0,weights=(1/NK_c$error_rf_p),data=NK_c)
            #     #summary(model_NKMarket_log)
                
            #     model_NKNonMarket_log <- felm(Omega_NProv ~ 0 + log(temp+1)|decade+clim+sceni+dgvm|0|0,weights=(1/NK_c$error_rf_np),data=NK_c)
            #     #model_NKNonMarket_log <- lm(Omega_NProv ~ 0 + log(temp+1)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_np),data=NK_c)

                
            #     model_NKMarket_2sq <- felm(Omega_Prov ~ 0 + temp + I(temp^2)|decade+clim+sceni+dgvm|0|0,weights=(1/NK_c$error_rf_p),data=NK_c)
            #     #summary(model_NKMarket_2sq)
            #     #model_NKMarket_2sq <- lm(Omega_Prov ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_p),data=NK_c)

            #     model_NKMarket_2sq_eq <- felm(Omega_Prov ~ 0 + temp + I(temp^2)|decade+clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_p),data=NK_c_equal)
            #     #summary(model_NKMarket_2sq_eq)
            #     #model_NKMarket_2sq <- lm(Omega_Prov ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_p),data=NK_c)
                
                
            #     model_NKNonMarket_2sq <- felm(Omega_NProv ~ 0 + temp + I(temp^2)|decade+clim+sceni+dgvm|0|0,weights=(1/NK_c$error_rf_np),data=NK_c)
            #     #model_NKNonMarket_2sq <- lm(Omega_NProv ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_np),data=NK_c)

                
            #     model_NKMarket_log_2 <- felm(Omega_Prov ~ 0 + temp + log(temp+1)|decade+clim+sceni+dgvm|0|0,weights=(1/NK_c$error_rf_p),data=NK_c)
            #     #model_NKMarket_log_2 <- lm(Omega_Prov ~ 0 + temp + log(temp+1)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_p),data=NK_c)
                
            #     model_NKNonMarket_log_2 <- felm(Omega_NProv ~ 0 + temp + log(temp+1)|decade+clim+sceni+dgvm|0|0,weights=(1/NK_c$error_rf_np),data=NK_c)
            #     #model_NKNonMarket_log_2 <- lm(Omega_NProv ~ 0 + temp + log(temp+1)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_np),data=NK_c)

                
            #     model_NKMarket_2sq <- felm(Omega_Prov ~ 0 + temp + I(temp^2)|decade+clim+sceni+dgvm|0|0,weights=(1/NK_c$error_rf_p),data=NK_c)
            #     #model_NKMarket_2sq <- lm(Omega_Prov ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_p),data=NK_c)
                
            #     model_NKNonMarket_2sq <- felm(Omega_NProv ~ 0 + temp + I(temp^2)|decade+clim+sceni+dgvm|0|0,weights=(1/NK_c$error_rf_np),data=NK_c)
            #     #model_NKNonMarket_2sq <- lm(Omega_NProv ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_np),data=NK_c)


            #     Omega_NK_data <- rbind(data.frame(country = isos[i], 
            #         Omega_NK_c1 = NA, 
            #         Omega_NK_c1_se = NA, 
            #         Omega_NK_c1_pval = NA,
            #         Omega_NK_c2 = summary(model_NKMarket)$coefficient[1], 
            #         Omega_NK_c2_se = summary(model_NKMarket)$coefficient[2],
            #         Omega_NK_c2_pval = summary(model_NKMarket)$coefficient[4],
            #         Omega_NK_rsq = summary(model_NKMarket)$adj.r.squared,
            #         NK_type="Market",
            #         Function = "Sq"),
            #         data.frame(country = isos[i], 
            #         Omega_NK_c1 = NA, 
            #         Omega_NK_c1_se = NA,
            #         Omega_NK_c1_pval = NA,
            #         Omega_NK_c2 = summary(model_NKNonMarket)$coefficient[1], 
            #         Omega_NK_c2_se = summary(model_NKNonMarket)$coefficient[2],
            #         Omega_NK_c2_pval = summary(model_NKNonMarket)$coefficient[4],
            #         Omega_NK_rsq = summary(model_NKNonMarket)$adj.r.squared,
            #         NK_type="NonMarket",
            #         Function = "Sq"),
            #         data.frame(country = isos[i], 
            #         Omega_NK_c1 = NA, 
            #         Omega_NK_c1_se = NA, 
            #         Omega_NK_c1_pval = NA,
            #         Omega_NK_c2 = summary(model_NKMarket_eq)$coefficient[1], 
            #         Omega_NK_c2_se = summary(model_NKMarket_eq)$coefficient[2],
            #         Omega_NK_c2_pval = summary(model_NKMarket_eq)$coefficient[4],
            #         Omega_NK_rsq = summary(model_NKMarket_eq)$adj.r.squared,
            #         NK_type="Market",
            #         Function = "SqEq"),
            #         data.frame(country = isos[i], 
            #         Omega_NK_c1 = NA, 
            #         Omega_NK_c1_se = NA,
            #         Omega_NK_c1_pval = NA,
            #         Omega_NK_c2 = summary(model_NKNonMarket_eq)$coefficient[1], 
            #         Omega_NK_c2_se = summary(model_NKNonMarket_eq)$coefficient[2],
            #         Omega_NK_c2_pval = summary(model_NKNonMarket_eq)$coefficient[4],
            #         Omega_NK_rsq = summary(model_NKNonMarket_eq)$adj.r.squared,
            #         NK_type="NonMarket",
            #         Function = "SqEq"),
            #         data.frame(country = isos[i], 
            #         Omega_NK_c1 = NA,
            #         Omega_NK_c1_se = NA,
            #         Omega_NK_c1_pval = NA,
            #         Omega_NK_c2 = summary(model_NKMarket_log)$coefficient[1], 
            #         Omega_NK_c2_se = summary(model_NKMarket_log)$coefficient[2],
            #         Omega_NK_c2_pval = summary(model_NKMarket_log)$coefficient[4],
            #         Omega_NK_rsq = summary(model_NKMarket_log)$adj.r.squared,
            #         NK_type="Market",
            #         Function = "Log"),
            #         data.frame(country = isos[i], 
            #         Omega_NK_c1 = NA,
            #         Omega_NK_c1_se = NA,
            #         Omega_NK_c1_pval = NA,
            #         Omega_NK_c2 = summary(model_NKNonMarket_log)$coefficient[1], 
            #         Omega_NK_c2_se = summary(model_NKNonMarket_log)$coefficient[2],
            #         Omega_NK_c2_pval = summary(model_NKNonMarket_log)$coefficient[4],
            #         Omega_NK_rsq = summary(model_NKNonMarket_log)$adj.r.squared,
            #         NK_type="NonMarket",
            #         Function = "Log"),
            #         data.frame(country = isos[i], 
            #         Omega_NK_c1 = summary(model_NKMarket_2sq)$coefficient[1], 
            #         Omega_NK_c1_se = summary(model_NKMarket_2sq)$coefficient[3],
            #         Omega_NK_c1_pval = summary(model_NKMarket_2sq)$coefficient[7],
            #         Omega_NK_c2 = summary(model_NKMarket_2sq)$coefficient[2], 
            #         Omega_NK_c2_se = summary(model_NKMarket_2sq)$coefficient[4],
            #         Omega_NK_c2_pval = summary(model_NKMarket_2sq)$coefficient[8],
            #         Omega_NK_rsq = summary(model_NKMarket_2sq)$adj.r.squared,
            #         NK_type="Market",
            #         Function = "Sq 2 coeff"),
            #         data.frame(country = isos[i], 
            #         Omega_NK_c1 = summary(model_NKNonMarket_2sq)$coefficient[1], 
            #         Omega_NK_c1_se = summary(model_NKNonMarket_2sq)$coefficient[3],
            #         Omega_NK_c1_pval = summary(model_NKNonMarket_2sq)$coefficient[7],
            #         Omega_NK_c2 = summary(model_NKNonMarket_2sq)$coefficient[2], 
            #         Omega_NK_c2_se = summary(model_NKNonMarket_2sq)$coefficient[4],
            #         Omega_NK_c2_pval = summary(model_NKNonMarket_2sq)$coefficient[8],
            #         Omega_NK_rsq = summary(model_NKNonMarket_2sq)$adj.r.squared,
            #         NK_type="NonMarket",
            #         Function = "Sq 2 coeff"),
            #         data.frame(country = isos[i], 
            #         Omega_NK_c1 = summary(model_NKMarket_log_2)$coefficient[1], 
            #         Omega_NK_c1_se = summary(model_NKMarket_log_2)$coefficient[3],
            #         Omega_NK_c1_pval = summary(model_NKMarket_log_2)$coefficient[7],
            #         Omega_NK_c2 = summary(model_NKMarket_log_2)$coefficient[2], 
            #         Omega_NK_c2_se = summary(model_NKMarket_log_2)$coefficient[4],
            #         Omega_NK_c2_pval = summary(model_NKMarket_log_2)$coefficient[8],
            #         Omega_NK_rsq = summary(model_NKMarket_log_2)$adj.r.squared,
            #         NK_type="Market",
            #         Function = "Log 2 coeff"),
            #         data.frame(country = isos[i], 
            #         Omega_NK_c1 = summary(model_NKNonMarket_log_2)$coefficient[1], 
            #         Omega_NK_c1_se = summary(model_NKNonMarket_log_2)$coefficient[3],
            #         Omega_NK_c1_pval = summary(model_NKNonMarket_log_2)$coefficient[7],
            #         Omega_NK_c2 = summary(model_NKNonMarket_log_2)$coefficient[2], 
            #         Omega_NK_c2_se = summary(model_NKNonMarket_log_2)$coefficient[4],
            #         Omega_NK_c2_pval = summary(model_NKNonMarket_log_2)$coefficient[8],
            #         Omega_NK_rsq = summary(model_NKNonMarket_log_2)$adj.r.squared,
            #         NK_type="NonMarket",
            #         Function = "Log 2 coeff")
            #         )
                
            #     if (i ==1){
            #         Omega_NK <- Omega_NK_data 
            #     } else {
                    
            #         Omega_NK <- rbind(Omega_NK,Omega_NK_data)
            #     }
                






             
            #       ggsave(country_damagefunction, file=paste0("Figures/CountriesDamFun_LogSq/All_data_damagefunction_", isos[i],".png"), dpi=300)
            #     # # ggsave(country_damagefunction_Market_loess, file=paste0("Figures/Countries/LOESS_damagefunction_", isos[i],".png"), dpi=300)
            #      #ggsave(country_damagefunction_Market_equal, file=paste0("Figures/Countries/Equal_damagefunction_", isos[i],".png"), dpi=300)          
            #     #  ggsave(country_damagefunction_Market_equal, file=paste0("Figures/Countries/EqualQuad_damagefunction_", isos[i],".png"), dpi=300)
            #       #ggsave(country_damagefunction_LOG, file=paste0("Figures/Countries/LOG_damagefunction_", isos[i],".png"), dpi=300)
            #       #ggsave(country_damagefunction_LOG2coeff, file=paste0("Figures/Countries/2coeff_log_damagefunction_", isos[i],".png"), dpi=300)

            #       #ggsave(country_damagefunction_Market2coeff, file=paste0("Figures/Countries/2coeff_quad_damagefunction_", isos[i],".png"), dpi=300)
            #     # ggsave(country_damagefunction_Market_loess, file=paste0("Figures/3dgvm_Countries/LOESS_damagefunction_", isos[i],".png"), dpi=300)
            #     # ggsave(country_damagefunction_Market_equal, file=paste0("Figures/3dgvm_Countries/Equal_damagefunction_", isos[i],".png"), dpi=300)          
            #     # ggsave(country_damagefunction_Market_equal_quadratic, file=paste0("Figures/3dgvm_Countries/EqualQuad_damagefunction_", isos[i],".png"), dpi=300)
            #     # ggsave(country_damagefunction_LOG, file=paste0("Figures/3dgvm_Countries/2coeff_LOG_damagefunction_", isos[i],".png"), dpi=300)
            # }

            # #DGVM one by one
            #     for (i in 1:length(levels(factor(NK$country)))){
            #         #for (i in 1:15){
            #         NK_c <- NK[NK$country==isos[i],]
            #         if(is.nan(NK_c$Omega_NProv[1]) | is.nan(NK_c$Omega_Prov[1])){next}
            #         if(isos[i]=="CZ"){
            #             NK_c <- NK_c[which(abs(NK_c$Omega_Prov)<quantile(abs(NK_c$Omega_Prov),0.9)),]

            #         }

            #             dgvms <- levels(factor(NK_c$dgvm))
            #         for (di in 1:length(dgvms)){
            #             NK_cd <- NK_c[which(NK_c$dgvm==dgvms[di]),]
                        
            #             country_damagefunction <- ggplot(NK_cd,aes(color=country))+
            #             geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=clim))+
            #             geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=clim),color="darkblue")+
            #             #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~0+ I(x^2)",color="darkblue")+
            #             geom_smooth(aes(x=temp,y=Omega_Prov,color=country,weight = error_rf_p_w),method="lm",formula="y~0+I(x^2)",color="darkblue")+
            #             #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
            #             geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=clim),color="indianred")+
            #             geom_smooth(aes(x=temp,y=Omega_NProv,color=country,weight = error_rf_np_w),method="lm",formula="y~ 0+ I(x^2)",color="indianred")+
            #             #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
            #             theme_bw() +
            #             scale_color_manual(name='Type of Natural Capital',
            #                 breaks=c('Market', 'Nonmarket'),
            #                 values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
            #             ggtitle(paste("Natural Capital Damage Function. Squared 2 coefficients. Country = ",isos[i]))+
            #             xlab("Temperature Change (relative to 20006-2020)")+
            #             ylab("Natural Capital Change")

                        
            #             model_NKMarket <- felm(Omega_Prov ~ 0 + I(temp^2)|clim+sceni|0|0,weights=(1/NK_cd$error_rf_p),data=NK_cd)
            #             model_NKNonMarket <- felm(Omega_NProv ~ 0 + I(temp^2)|clim+sceni|0|0,weights=(1/NK_cd$error_rf_np),data=NK_cd)

            #             Omega_NK_data <- rbind(data.frame(country = isos[i], 
            #             Omega_NK_c1 = NA, 
            #             Omega_NK_c1_se = NA, 
            #             Omega_NK_c1_pval = NA,
            #             Omega_NK_c2 = summary(model_NKMarket)$coefficient[1], 
            #             Omega_NK_c2_se = summary(model_NKMarket)$coefficient[2],
            #             Omega_NK_c2_pval = summary(model_NKMarket)$coefficient[4],
            #             Omega_NK_rsq = summary(model_NKMarket)$adj.r.squared,
            #             NK_type="Market",
            #             Function = "Sq",
            #             DGVM=dgvms[di]),
            #             data.frame(country = isos[i], 
            #             Omega_NK_c1 = NA, 
            #             Omega_NK_c1_se = NA,
            #             Omega_NK_c1_pval = NA,
            #             Omega_NK_c2 = summary(model_NKNonMarket)$coefficient[1], 
            #             Omega_NK_c2_se = summary(model_NKNonMarket)$coefficient[2],
            #             Omega_NK_c2_pval = summary(model_NKNonMarket)$coefficient[4],
            #             Omega_NK_rsq = summary(model_NKNonMarket)$adj.r.squared,
            #             NK_type="NonMarket",
            #             Function = "Sq",
            #             DGVM=dgvms[di]))

            #             if (i ==1){
            #             Omega_NK <- Omega_NK_data 
            #                         } else {
                                        
            #                             Omega_NK <- rbind(Omega_NK,Omega_NK_data)
            #                         }
                    
            #             ggsave(country_damagefunction, file=paste0("Figures/CountriesDamFun/",dgvms[di],"/quad_damagefunction_",dgvms[di],isos[i],".png"), dpi=300)

            #         }
            #     }
            

            #     write.csv(Omega_NK,file="Data/Omega_NK_damage_function_Different_DGVMs.csv")
            #     Omega_NK <- read.csv("Data/Omega_NK_damage_function_Different_DGVMs.csv")
            #     glimpse(Omega_NK)               

            #     ggplot(Omega_NK[which(Omega_NK$Function=="Sq"&Omega_NK$NK_type=="Market"),], aes(Omega_NK_c2_pval)) +
            #         geom_histogram(fill="indianred",breaks=c(0,0.01,0.05,seq(1:100)*0.1))+
            #         theme_bw() + ggtitle("Market NC damage function coefficient")+ xlab ("") + 
            #         geom_vline(aes(xintercept=0.01),linetype="dashed")+xlim(0,1)+
            #         annotate("text", x=0.02, y=60, label="0.01", angle=90,size=3)+ 
            #         geom_vline(aes(xintercept=0.05),linetype="dashed")+
            #         annotate("text", x=0.06, y=40, label="0.05", angle=90,size=3)+ 
            #         geom_vline(aes(xintercept=0.1),linetype="dashed")+
            #         annotate("text", x=0.11, y=20, label="0.1", angle=90,size=3) + ylab("count")+ xlab("p-value") 

            #     #Omega_NK_all <- read.csv("Data/Omega_NK_damage_function_sq_FINAL.csv")
            #     glimpse(Omega_NK_all)
            #     Omega_NK_all$iso <- countrycode(Omega_NK_all$iso3,origin="iso3c",destination="iso2c")


            #     Omega_NK_all$id <- paste0(Omega_NK_all$iso,Omega_NK_all$NK_type)
            #     Omega_NK$id <- paste0(Omega_NK$country,Omega_NK$NK_type)
            #     Omega_NK<-merge(Omega_NK,Omega_NK_all[,which(names(Omega_NK_all) %in% c("id","Omega_NK_T2coeff"))],by="id",all.x=TRUE)
            #     glimpse(Omega_NK)

            #     Omega_NK$Change_coeff <- Omega_NK$Omega_NK_c2/Omega_NK$Omega_NK_T2coeff

            #     ggplot(Omega_NK)+
            #     geom_point(aes(x=Omega_NK_c2,y=Change_coeff,color=DGVM))+ylim(-10,10)

            #     Omega_NK_dgvms_mean <- aggregate(Omega_NK_c2~id,data=Omega_NK,FUN="mean")
            #     glimpse(Omega_NK_dgvms_mean)
            #     Omega_NK<-merge(Omega_NK,Omega_NK_dgvms_mean,by="id",all.x=TRUE)
            #     glimpse(Omega_NK)

            #     Omega_NK$Change_Mean_All <- Omega_NK$Omega_NK_c2.y / Omega_NK$Omega_NK_T2coeff

            #     ggplot(Omega_NK)+
            #     geom_point(aes(x=Omega_NK_T2coeff,y=Omega_NK$Omega_NK_c2.y))+#+ylim(-10,10)+
            #     xlab("Coefficient using all data")+ylab("Mean of three coefficients")+theme_bw()

            #     ggsave("Figures/ComparisonCoefficients.png",dpi=300)

            # #DGVM one by one

         
            results_df <- read.csv("Data/Damage_coef.csv")
            glimpse(results_df)
            levels(factor(results_df$formula))
            
            pvalm <- ggplot(results_df[which(results_df$formula=="lin" & results_df$capital=="mN" & results_df$dgvm!="all" ),], aes(pval)) +
            geom_histogram(aes(fill=factor(dgvm)),breaks=c(0,0.01,0.05,0.1,(0.1+seq(1:90)*0.01)))+
            theme_bw() + ggtitle("Market NC damage function coefficient")+ xlab ("") + 
            geom_vline(aes(xintercept=0.01),linetype="dashed")+xlim(0,1)+
            annotate("text", x=0.025, y=140, label="0.01", angle=90,size=3)+ 
            geom_vline(aes(xintercept=0.05),linetype="dashed")+
            annotate("text", x=0.065, y=130, label="0.05", angle=90,size=3)+ 
            geom_vline(aes(xintercept=0.1),linetype="dashed")+
            annotate("text", x=0.115, y=110, label="0.1", angle=90,size=3) + ylab("count")+ xlab("p-value")+
            guides(fill=guide_legend(title = "DGVM"))

            pvaln <- ggplot(results_df[which(results_df$formula=="lin" & results_df$capital=="nN" & results_df$dgvm!="all"),], aes(pval))+
            geom_histogram(aes(fill=factor(dgvm)),breaks=c(0,0.01,0.05,0.1,(0.1+seq(1:90)*0.01)))+
            theme_bw() + ggtitle("Non-market NC damage function coefficient")+ xlab ("") + 
            geom_vline(aes(xintercept=0.01),linetype="dashed")+xlim(0,1)+
            annotate("text", x=0.025, y=140, label="0.01", angle=90,size=3)+ 
            geom_vline(aes(xintercept=0.05),linetype="dashed")+
            annotate("text", x=0.065, y=130, label="0.05", angle=90,size=3)+ 
            geom_vline(aes(xintercept=0.1),linetype="dashed")+
            annotate("text", x=0.115, y=110, label="0.1", angle=90,size=3) + ylab("count")+ xlab("p-value")+
            guides(fill=guide_legend(title = "DGVM"))

            ggarrange(pvalm,pvaln,common.legend=TRUE,nrow=2,legend="bottom")
            


            # Omega_NK$iso3 <- countrycode(Omega_NK$country, origin="iso2c",destination="iso3c")
            # save(file="Data/Omega_NK_raw_final.Rda",Omega_NK)

            # load("Data/Omega_NK_raw_final.Rda")
            # glimpse(Omega_NK)

            # ggplot(Omega_NK[which(Omega_NK$Function%in%c("Log","Sq")),],aes(Omega_NK_rsq))+
            # geom_density(aes(color=Function))+
            # geom_vline(aes(xintercept=mean(Omega_NK$Omega_NK_rsq[which(Omega_NK$Function%in%c("Log"))], na.rm=T)),   # Ignore NA values for mean
            #    color="darkred", size=1,linetype="dashed")+
            # geom_vline(aes(xintercept=mean(Omega_NK$Omega_NK_rsq[which(Omega_NK$Function%in%c("Sq"))], na.rm=T)),   # Ignore NA values for mean
            #    color="cyan", size=1,linetype="dashed") + theme_bw()+xlab("Adjusted R-squared")


            #    mean(Omega_NK$Omega_NK_rsq[which(Omega_NK$Function%in%c("Log"))], na.rm=T)
            #    mean(Omega_NK$Omega_NK_rsq[which(Omega_NK$Function%in%c("Sq"))], na.rm=T)

          
           

            # pvalm <- ggplot(Omega_NK[which(Omega_NK$Function=="Log"&Omega_NK$NK_type=="Market"),], aes(Omega_NK_c2_pval)) +
            # geom_histogram(fill="indianred",breaks=c(0,0.01,0.05,0.1,(0.1+seq(1:90)*0.01)))+
            # theme_bw() + ggtitle("Market NC damage function coefficient")+ xlab ("") + 
            # geom_vline(aes(xintercept=0.01),linetype="dashed")+xlim(0,1)+
            # annotate("text", x=0.02, y=50, label="0.01", angle=90,size=3)+ 
            # geom_vline(aes(xintercept=0.05),linetype="dashed")+
            # annotate("text", x=0.06, y=40, label="0.05", angle=90,size=3)+ 
            # geom_vline(aes(xintercept=0.1),linetype="dashed")+
            # annotate("text", x=0.11, y=20, label="0.1", angle=90,size=3) + ylab("count")+ xlab("p-value")

            # pvaln <- ggplot(Omega_NK[which(Omega_NK$Function=="Log"&Omega_NK$NK_type=="NonMarket"),], aes(Omega_NK_c2_pval)) +
            # geom_histogram(fill="darkcyan",breaks=c(0,0.01,0.05,0.1,(0.1+seq(1:90)*0.01)))+
            # theme_bw() + ggtitle("Non-market NC damage function coefficient")+ xlab ("") + 
            # geom_vline(aes(xintercept=0.01),linetype="dashed")+xlim(0,1)+
            # annotate("text", x=0.02, y=50, label="0.01", angle=90,size=3)+ 
            # geom_vline(aes(xintercept=0.05),linetype="dashed")+
            # annotate("text", x=0.06, y=40, label="0.05", angle=90,size=3)+ 
            # geom_vline(aes(xintercept=0.1),linetype="dashed")+
            # annotate("text", x=0.11, y=20, label="0.1", angle=90,size=3) + ylab("count")+ xlab("p-value")

            # ggarrange(pvalm,pvaln,nrow=2)
            
            ggsave("Figures/pval_estimates_Lin.png",dpi=300)

       

            w2018 <- data.frame(iso3=wealth2018$countrycode,K=wealth2018$K, 
                    TotalWealth=wealth2018$TotalWealth, 
                    NK_Market=wealth2018$NforestT,
                    NK_NonMarket=(wealth2018$NforestES + wealth2018$Npa),year=2018)
                    glimpse(w2018)
            write.csv(w2018,file="Data/w2018.csv")







# Calculate Nat Cap Damage
