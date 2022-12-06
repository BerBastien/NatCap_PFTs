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

        #table(wealth_data$countrycode)
    # Forest Rents (end)
#READ WORLD BANK DATA (end)


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
#Map of cover change at different time horizons (end)


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

#Get Country Level Data
        
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
#Get Country Level Data



        # Read Database
            
            es_ac_notFood <- read.csv("es_ac_notFood_id.csv")
            dat<- es_ac_notFood
            #spatial extents are not standardized units - convert Hectares to Acres
            dat$Spatial.Extent[which(dat$Spatial.Unit.Published=="Hectares")]<-dat$Spatial.Extent[which(dat$Spatial.Unit.Published=="Hectares")]*2.471
            #limit to observations with spatial extent in either acres or hectares (now converted to acres)
            dat<- dat[which(dat$Spatial.Unit.Published%in% c("Acres","Hectares")),]
            
            a1=ggplot(dat,aes(x=log(Spatial.Extent),y=log(Single.Value.Converted)))+geom_point()
            a1=a1+geom_smooth(method="lm") + theme_bw()

            x11()
            a1
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

            #mod=lm(log(Single.Value.Converted)~log(cveg_perarea)+log(gdp_pc),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0&Category=="Non-Provisioning"))
            #summary(mod)
            ggplot(dat)+
            geom_point(aes(y=log(Single.Value.Converted),x=cveg_total),color="blue")+
            geom_point(aes(y=log(Single.Value.Converted),x=cveg_total_car),color="red")+
            geom_point(aes(y=log(Single.Value.Converted),x=cveg_total_orc),color="cyan")+
            geom_point(aes(y=log(Single.Value.Converted),x=cveg),color="black",alpha=0.3)+geom_smooth(aes(y=log(Single.Value.Converted),x=cveg_total),method="lm")

            
            ggplot(dat)+geom_point(aes(y=log(Single.Value.Converted),x=log(cveg)),color="black",alpha=0.3)+
            geom_smooth(aes(y=log(Single.Value.Converted),x=log(cveg)),method="lm")
           
            c1 <- hist(dat$cveg_total)
            c2 <- hist(dat$cveg_total_car,add=TRUE)
            c3 <- hist(dat$cveg_total_orc)
            ggarrange(c1,c2,c3)

            dat$cveg <- (dat$cveg_total + dat$cveg_total_car + dat$cveg_total_orc)/3
            dat$pct_covered_mean <- (dat$pct_covered + dat$pct_covered_car + dat$pct_covered_orc)/3
            dat$cveg_perarea <- dat$cveg/dat$Spatial.Extent
            
            max(dat$cveg,na.rm=TRUE)/min(dat$cveg,na.rm=TRUE)
            
            modcveg=lm(log(Single.Value.Converted)~log(cveg)*Category+log(Spatial.Extent)*Category+log(gdp_pc)+log(pct_covered_mean),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0))
            summary(modcveg)
            elasticity_area_prov <- 0
            elasticity_area_nonprov <- summary(modcveg)$coefficients[4]
            elasticity_cveg_prov <- 0
            elasticity_cveg_nonprov <- summary(modcveg)$coefficients[2]


            modcveg=lm(log(Single.Value.Converted)~log(cveg_total_orc)*Category+log(Spatial.Extent)*Category+log(gdp_pc)+log(pct_covered_orc),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0&cveg_total_orc>0))
            summary(modcveg)


            modcveg=lm(log(Single.Value.Converted)~log(cveg)+log(Spatial.Extent)+log(gdp_pc)+log(pct_covered_mean),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0&Category!="Provisioning"))
            summary(modcveg)

            modcveg=lm(log(Single.Value.Converted)~log(cveg)+log(Spatial.Extent)+log(gdp_pc)+log(pct_covered_mean),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0&Category=="Provisioning"))
            summary(modcveg)

            modcveg=lm(log(Single.Value.Converted)~log(cveg)*Category+log(Spatial.Extent)*Category+log(gdp_pc)+log(pct_covered),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0))
            summary(modcveg)


            dat$cveg_total_orc
            
            dat_c <- dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0&Category!="Provisioning")
            dat_c <- dat_c[order(dat_c$Single.Value.Converted),]
            glimpse(dat_c)
            
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
            annotate("text", x=33, y=15000000, label="40% Increase", angle=90,size=5)
            ggsave("Figures/Cveg_Cap.png",dpi=400)

            elastcap_area <- ggplot(dat_c,aes(x=(area_percinc),y=(ES_percinc)))+geom_point()+geom_smooth(method="loess")+theme_bw()+
            #geom_vline(aes(xintercept=40),linetype="dashed")+
            xlab("Percent increase in Area")+
            ylab("Percent increase in ecosystem benefits value")+scale_x_continuous(trans = 'log')#+scale_y_continuous(trans = 'log')
            #annotate("text", x=33, y=15000000, label="40% Increase", angle=90,size=5)
            ggsave("Figures/Area_Cap.png",dpi=400)

            ggarrange(elastcap_area,elastcap_cveg,nrow=1,ncol=2)
             ggsave("Figures/AreaCveg_Cap.png",dpi=400)


            
            dat_c <- dat_c[which(dat_c$area_percinc<quantile(dat_c$area_percinc,0.95)),]
            
            ggplot(dat_c,aes(x=(area_percinc),y=(ES_percinc)))+geom_point()+geom_smooth(method="lm")




            ggplot(dat_c,aes(x=log(cveg_percinc),y=log(ES_percinc)))+geom_point()+geom_smooth(method="lm")






            ggplot(dat_c)+geom_point(aes(x=log(cveg_percinc),y=log(ES_percinc)))

            ggplot(dat_c,aes(y=log(Single.Value.Converted),x=log(cveg)))+
            geom_point()+theme_bw()+
            geom_smooth(method="lm")

            ggplot(dat_c,aes(y=log(Single.Value.Converted),x=log(Spatial.Extent)))+
            geom_point()+theme_bw()+
            geom_smooth(method="lm")
            
            ggplot(dat_c)+geom_point(aes(y=log(Single.Value.Converted),x=log(Spatial.Extent)))


            summary(lm(log(ES_percinc)~log(cveg_percinc)*Category,data=dat_c))

           

            stargazer(modcveg,type="html",out="value-areacveg.html")

            # modcveg=lm(log(Single.Value.Converted)~(cveg_total)+log(Spatial.Extent)+log(gdp_pc),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0&Category=="Provisioning"))
            # summary(modcveg)

            # modcveg=lm(log(Single.Value.Converted)~(cveg_total)+log(Spatial.Extent)+log(gdp_pc),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0&Category=="Non-Provisioning"))
            # summary(modcveg)

            #elasticity_cveg <- summary(modcveg)$coefficients[2]
            #summary(modcveg)

            #modcveg=lm(log(Single.Value.Converted)~cveg_total*Category+log(gdp_pc),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0))
            #summary(modcveg)
            #elasticity_cveg
            
            

            #a1cveg=ggplot(dat,aes(x=log(cveg_perarea),y=log(Single.Value.Converted)))+geom_point()
            #a1cveg=a1cveg+geom_smooth(method="lm") + theme_bw()

            #a1cveg


            #dat$cveg <- (dat$cveg_total + dat$cveg_total_car + dat$cveg_total_orc)/3
            #dat$cveg_perarea <- dat$cveg/dat$Spatial.Extent
            
            #modcveg=lm(log(Single.Value.Converted)~log(cveg_perarea)+log(Spatial.Extent)+log(gdp_pc),data=dat%>%filter(Single.Value.Converted>0&Spatial.Extent>0&gdp_pc>0&Category=="Non-Provisioning"))
            
            #summary(modcveg)

            #b=ggplot(dat,aes(x=log(cveg_perarea),y=log(Single.Value.Converted),col=Category))+geom_point()
            #b=b+geom_smooth(method="lm")
            #b+theme_bw()+xlab("Log Carbon vegetation") + ylab("Log value of benefit")
            

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
            load("Data/PFT_ES_allcountries_3dgvms_CVEGcap_FixedRF_euclidean_exp.Rda")
            
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

                ggplot(pft_es_car)+
                geom_boxplot(,aes(x=(Benefits/100),y=Biome,
                    fill=Type,middle=mean((Benefits/100)))) +
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
                PFT_ES_all$TotalVal <- PFT_ES_all$es_val_per_area_np_final*PFT_ES_all$PFT_area
                glimpse(wealth2018)
                PFT_ES_all <- merge(PFT_ES_all,wealth2018[,which(names(wealth2018) %in% c("country","GDP"))], by.x="country",by.y="country")
                PFT_ES_all$TotalValES_gdp <- PFT_ES_all$TotalVal/PFT_ES_all$GDP
                
            
                PFT_ES_all$TotalValGDP_gdp <- PFT_ES_all$es_val_per_area*PFT_ES_all$PFT_area/PFT_ES_all$GDP
                ES_val_lpj <- aggregate(TotalValES_gdp~PFT_code+region_un,data= PFT_ES_all[which(PFT_ES_all$dgvm=="lpj"),], FUN="mean")
                    ES_val_lpj$TotalValGDP_gdp <- aggregate(TotalValGDP_gdp~PFT_code+region_un,data= PFT_ES_all[which(PFT_ES_all$dgvm=="lpj"),], FUN="mean")[,3]

                    glimpse(ES_val_lpj)
                    ES_val_lpj$PFT_code <- factor(ES_val_lpj$PFT_code,levels=c("bns","bne","bine", "c3g","ibs",
                "tebs","tebe","c4g","trbr","tribe","trbe"))

                ES_val_lpj$region_un <- factor(ES_val_lpj$region_un,levels=c("Europe","Oceania", "Americas","Asia","Africa"))
                    
                    ESValplot <-  ggplot(ES_val_lpj,aes(x=region_un,y=100*TotalValES_gdp,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                        geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
                geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("") + ggtitle("Ecosystem services")+
                    guides(fill = guide_legend(reverse=FALSE)) 

                ES_val_lpj2<- ES_val_lpj
                ES_val_lpj2$region_un <- factor(ES_val_lpj2$region_un,levels=c("Europe","Oceania","Asia", "Americas", "Africa" ))
            

                GDPValplot <-  ggplot(ES_val_lpj2,aes(x=region_un,y=TotalValGDP_gdp*100,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(4),pal_temp(3), pal_trop(4)))+
                geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("Value of benefits (as % of GDP)") + ggtitle("Market goods")+
                    guides(fill = guide_legend(reverse=FALSE))

                ggarrange(GDPValplot,ESValplot,common.legend=TRUE,legend="right")
                ggsave("Figures/Benefits_per_continent_lpj.png",dpi=600)
                    
                    ES_val_car <- aggregate(TotalValES_gdp~PFT_code+region_un,data= PFT_ES_all[which(PFT_ES_all$dgvm=="car"),], FUN="mean")
                    ES_val_car$TotalValGDP_gdp <- aggregate(TotalValGDP_gdp~PFT_code+region_un,data= PFT_ES_all[which(PFT_ES_all$dgvm=="car"),], FUN="mean")[,3]

                    glimpse(ES_val_car)
                    ES_val_car$PFT_code <- factor(ES_val_car$PFT_code,levels=c("brsuas", "brsutecds","brevtecds","ndsutecdt","ndevtecdt","brsutecdt", "c3dh","c3hh","brsuteclt","ndevteclt",
                "brsutewmt","brsutewms","brevtewms","ndevstdit","ndsustswt","brevstdit","brevdttht",
                "c4h","brevxs","sds","ndevtedtt","ndevtedttht","brevdtt",
                "brrgtrt","brevtrt","trs"))

                ES_val_car$region_un <- factor(ES_val_car$region_un,levels=c("Europe","Oceania", "Americas","Asia","Africa"))
            
                ESValplot <-  ggplot(ES_val_car,aes(x=region_un,y=100*TotalValES_gdp/2,fill=PFT_code))+ #Divided by two becase CAR has 2 levels of vegetation
                geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(10),pal_temp(6), pal_trop(10)))+
                geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("") + ggtitle("Ecosystem services")+
                    guides(fill = guide_legend(reverse=FALSE)) 

                ES_val_car2<- ES_val_car
                ES_val_car2$region_un <- factor(ES_val_car2$region_un,levels=c("Europe","Asia", "Americas","Oceania", "Africa" ))
           

              GDPValplot <-  ggplot(ES_val_car2,aes(x=region_un,y=TotalValGDP_gdp*100,fill=PFT_code))+ #Divided by two becase CAR has 2 levels of vegetation
                geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(10),pal_temp(6), pal_trop(10)))+
                geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("Value of benefits (as % of GDP)") + ggtitle("Market goods")+
                    guides(fill = guide_legend(reverse=FALSE))

                ggarrange(GDPValplot,ESValplot,common.legend=TRUE,legend="right")
                ggsave("Benefits_per_continent_car.png",dpi=600)


                ES_val_orc <- aggregate(TotalValES_gdp~PFT_code+region_un,data= PFT_ES_all[which(PFT_ES_all$dgvm=="orc"),], FUN="mean")
                ES_val_orc$TotalValGDP_gdp <- aggregate(TotalValGDP_gdp~PFT_code+region_un,data= PFT_ES_all[which(PFT_ES_all$dgvm=="orc"),], FUN="mean")[,3]

                glimpse(ES_val_orc)
                ES_val_orc$PFT_code <- factor(ES_val_orc$PFT_code,levels=c("bobrsu","bondev","bondsu","c3gra","tebrsu","tebrev","tendev",
                                           "c4gra","trbrev" ,"trbrrg"))

                ES_val_orc$region_un <- factor(ES_val_orc$region_un,levels=c("Europe","Asia", "Americas", "Africa","Oceania"))
            
                ESValplot <-  ggplot(ES_val_orc,aes(x=region_un,y=100*TotalValES_gdp,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(3),pal_temp(3), pal_trop(4)))+
                geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("") + ggtitle("Ecosystem services")+
                    guides(fill = guide_legend(reverse=FALSE)) 

                ES_val_orc2<- ES_val_orc
                ES_val_orc2$region_un <- factor(ES_val_orc2$region_un,levels=c("Europe","Asia","Oceania","Americas", "Africa"))
            

              GDPValplot <-  ggplot(ES_val_orc2,aes(x=region_un,y=TotalValGDP_gdp*100,fill=PFT_code))+ #2.32 is adjustement for making up for a bug in the code
                geom_bar(stat="identity")+ scale_fill_manual(values = c(pal_boreal(3),pal_temp(4), pal_trop(3)))+
                geom_bar(stat = "identity", width = .9) +theme_bw()+ xlab("") +ylab("Value of benefits (as % of GDP)") + ggtitle("Market goods")+
                    guides(fill = guide_legend(reverse=FALSE))

                ggarrange(GDPValplot,ESValplot,common.legend=TRUE,legend="right")
                ggsave("Benefits_per_continent_orc.png",dpi=600)
            

            #Figure Total Values per Continent

        


            isos <- (levels(factor(NK$country))) #139
            NK$Omega_NProv <- NK$NK_NonProvisioning - 1
            NK$Omega_Prov <- NK$NK_Provisioning - 1
            
            NK$error_rf_p_w <- 1/NK$error_rf_p
            NK$error_rf_np_w <- 1/NK$error_rf_np
            isos <- levels(factor(NK$country))
            
            
            for (i in 1:length(levels(factor(NK$country)))){
                #for (i in 1:15){
                NK_c <- NK[NK$country==isos[i],]
                if(is.nan(NK_c$Omega_NProv[1]) | is.nan(NK_c$Omega_Prov[1])){next}
                if(isos[i]=="CZ"){
                    NK_c <- NK_c[which(abs(NK_c$Omega_Prov)<quantile(abs(NK_c$Omega_Prov),0.9)),]

                }

                # country_damagefunction <- ggplot(NK_c,aes(color=country))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
                # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~0+ I(x^2)",color="darkblue")+
                # geom_smooth(aes(x=temp,y=Omega_Prov,color=country,weight = error_rf_p_w),method="lm",formula="y~0+I(x^2)",color="darkblue")+
                # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
                # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
                # geom_smooth(aes(x=temp,y=Omega_NProv,color=country,weight = error_rf_np_w),method="lm",formula="y~ 0+ I(x^2)",color="indianred")+
                # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
                # theme_bw() +
                # scale_color_manual(name='Type of Natural Capital',
                #      breaks=c('Market', 'Nonmarket'),
                #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
                # ggtitle(paste("Natural Capital Damage Function. Squared 2 coefficients. Country = ",isos[i]))+
                # xlab("Temperature Change (relative to 20006-2020)")+
                # ylab("Natural Capital Change")

                # country_damagefunction <- ggplot(NK_c,aes(color=country))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
                # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~0+ I(x^2)",color="darkblue")+
                # geom_smooth(aes(x=temp,y=Omega_Prov,color=country,weight = error_rf_p_w),method="lm",formula="y~0+I(x^2)",color="darkblue")+
                # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
                # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
                # geom_smooth(aes(x=temp,y=Omega_NProv,color=country,weight = error_rf_np_w),method="lm",formula="y~ 0+ I(x^2)",color="indianred")+
                # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
                # theme_bw() +
                # scale_color_manual(name='Type of Natural Capital',
                #      breaks=c('Market', 'Nonmarket'),
                #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
                # ggtitle(paste("Natural Capital Damage Function. Squared 2 coefficients. Country = ",isos[i]))+
                # xlab("Temperature Change (relative to 20006-2020)")+
                # ylab("Natural Capital Change")



                # country_damagefunction_Market2coeff <- ggplot(NK_c,aes(color=country))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
                # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~0+ I(x^2)",color="darkblue")+
                # geom_smooth(aes(x=temp,y=Omega_Prov,color=country,weight = error_rf_p_w),method="lm",formula="y~x+I(x^2)",color="darkblue")+
                # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
                # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
                # geom_smooth(aes(x=temp,y=Omega_NProv,color=country,weight = error_rf_np_w),method="lm",formula="y~ x+ I(x^2)",color="indianred")+
                # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
                # theme_bw() +
                # scale_color_manual(name='Type of Natural Capital',
                #      breaks=c('Market', 'Nonmarket'),
                #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
                # ggtitle(paste("Natural Capital Damage Function. Squared 2 coefficients. Country = ",isos[i]))+
                # xlab("Temperature Change (relative to 20006-2020)")+
                # ylab("Natural Capital Change")


                # country_damagefunction_LOG2coeff <- ggplot(NK_c[NK_c$temp>0,],aes(color=country))+
                # #country_damagefunction_LOG <- ggplot(NK_c,aes(color=country))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
                # geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~1+(x)+log(x+1)",color="darkblue")+
                # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
                # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
                # geom_smooth(aes(x=temp,y=Omega_NProv,color=country),method="lm",formula="y~ 1+(x)+log(x+1)",color="indianred")+
                # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
                # theme_bw() +
                # scale_color_manual(name='Type of Natural Capital',
                #      breaks=c('Market', 'Nonmarket'),
                #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
                # ggtitle(paste("Natural Capital Damage Function. Log 2 coefficients. Country = ",isos[i]))+
                # xlab("Temperature Change (relative to 20006-2020)")+
                # ylab("Natural Capital Change")

                # country_damagefunction_LOG <- ggplot(NK_c[NK_c$temp>0,],aes(color=country))+
                # #country_damagefunction_LOG <- ggplot(NK_c,aes(color=country))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
                # geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~0+log(x+1)",color="darkblue")+
                # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
                # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
                # geom_smooth(aes(x=temp,y=Omega_NProv,color=country),method="lm",formula="y~ 0+log(x+1)",color="indianred")+
                # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
                # theme_bw() +
                # scale_color_manual(name='Type of Natural Capital',
                #      breaks=c('Market', 'Nonmarket'),
                #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
                # ggtitle(paste("Natural Capital Damage Function. Log 2 coefficients. Country = ",isos[i]))+
                # xlab("Temperature Change (relative to 20006-2020)")+
                # ylab("Natural Capital Change")




                # country_damagefunction_Market_loess <- ggplot(NK_c,aes(color=country))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
                # geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="loess",formula="y~0+ x",color="darkblue")+
                # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
                # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
                # geom_smooth(aes(x=temp,y=Omega_NProv,color=country),method="loess",formula="y~ 0+ x",color="indianred")+
                # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
                # theme_bw() +
                # scale_color_manual(name='Type of Natural Capital',
                #      breaks=c('Market', 'Nonmarket'),
                #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
                # ggtitle(paste("Natural Capital Damage Function. Country = ",isos[i]))+
                # xlab("Temperature Change (relative to 20006-2020)")+
                # ylab("Natural Capital Change")

                #NK_c_equal <- NK_c[which(NK_c$sceni!="rcp85" & NK_c$clim %in% c("gfdl-esm2m","ipsl-cm5a-lr")),]
                # NK_c_equal <- NK_c[which(NK_c$sceni!="rcp85" ),]
                # #max(NK_c_equal$temp)
                # #NK_c_equal <- NK_c[which(NK_c$temp <3),]
                # country_damagefunction_Market_equal <- ggplot(NK_c_equal,aes(color=country))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
                # geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~0 + I(x^2)",color="darkblue")+
                # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
                # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
                # geom_smooth(aes(x=temp,y=Omega_NProv,color=country),method="lm",formula="y~ 0+ I(x^2)",color="indianred")+
                # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
                # theme_bw() +
                # scale_color_manual(name='Type of Natural Capital',
                #      breaks=c('Market', 'Nonmarket'),
                #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
                # ggtitle(paste("Natural Capital Damage Function. Country = ",isos[i]))+
                # xlab("Temperature Change (relative to 20006-2020)")+
                # ylab("Natural Capital Change")


                # country_damagefunction_Market_equal_quadratic <- ggplot(NK_c_equal,aes(color=country))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm))+
                # geom_point(aes(x=temp,y=Omega_Prov,color=country,shape=dgvm),color="darkblue")+
                # geom_smooth(aes(x=temp,y=Omega_Prov,color=country),method="lm",formula="y~0+ I(x^2)",color="darkblue")+
                # #geom_smooth(aes(x=temp,y=Omega_Prov,color=country),color="darkblue")+
                # geom_point(aes(x=temp,y=Omega_NProv,color=country,shape=dgvm),color="indianred")+
                # geom_smooth(aes(x=temp,y=Omega_NProv,color=country),method="lm",formula="y~0+ I(x^2)",color="indianred")+
                # #geom_smooth(aes(x=temp,y=Omega_NProv,color=country),color="indianred")+
                # theme_bw() +
                # scale_color_manual(name='Type of Natural Capital',
                #      breaks=c('Market', 'Nonmarket'),
                #      values=c('Market'='darkblue', 'Nonmarket'='indianred'))+
                # ggtitle(paste("Natural Capital Damage Function. Country = ",isos[i]))+
                # xlab("Temperature Change (relative to 20006-2020)")+
                # ylab("Natural Capital Change")


                

                model_NKMarket <- felm(Omega_Prov ~ 0 + I(temp^2)|clim+sceni+dgvm|0|0,data=NK_c)
                #summary(model_NKMarket)
                #model_NKMarket <- lm(Omega_Prov ~ 0 + I(temp^2),weights=(1/NK_c$error_rf_p),data=NK_c)

                
                model_NKMarket_eq <- felm(Omega_Prov ~ 0 + I(temp^2)|clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_p),data=NK_c_equal)
                #summary(model_NKMarket_eq)
                
                #model_NKNonMarket <- lm(Omega_NProv ~ 0 + I(temp^2),weights=(1/NK_c$error_rf_np),data=NK_c)
                model_NKNonMarket <- felm(Omega_NProv ~ 0 + I(temp^2)|clim+sceni+dgvm|0|0,weights=(1/NK_c$error_rf_np),data=NK_c)
                #summary(model_NKNonMarket)

                
                model_NKNonMarket_eq <- felm(Omega_NProv ~ 0 + I(temp^2)|clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_p),data=NK_c_equal)
                #summary(model_NKNonMarket_eq)


                
                #model_NKMarket_log <- lm(Omega_Prov ~ 0 + log(temp+1),weights=(1/NK_c$error_rf_p),data=NK_c)
                model_NKMarket_log <- felm(Omega_Prov ~ 0 + log(temp+1)|clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_p),data=NK_c_equal)
                #summary(model_NKMarket_log)
                
                model_NKNonMarket_log <- felm(Omega_NProv ~ 0 + log(temp+1)|clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_np),data=NK_c_equal)
                #model_NKNonMarket_log <- lm(Omega_NProv ~ 0 + log(temp+1)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_np),data=NK_c)

                
                model_NKMarket_2sq <- felm(Omega_Prov ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_p),data=NK_c_equal)
                summary(model_NKMarket_2sq)
                #model_NKMarket_2sq <- lm(Omega_Prov ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_p),data=NK_c)

                model_NKMarket_2sq_eq <- felm(Omega_Prov ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_p),data=NK_c_equal)
                #summary(model_NKMarket_2sq_eq)
                #model_NKMarket_2sq <- lm(Omega_Prov ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_p),data=NK_c)
                
                
                model_NKNonMarket_2sq <- felm(Omega_NProv ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_np),data=NK_c_equal)
                #model_NKNonMarket_2sq <- lm(Omega_NProv ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_np),data=NK_c)

                
                model_NKMarket_log_2 <- felm(Omega_Prov ~ 0 + temp + log(temp+1)|clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_p),data=NK_c_equal)
                #model_NKMarket_log_2 <- lm(Omega_Prov ~ 0 + temp + log(temp+1)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_p),data=NK_c)
                
                model_NKNonMarket_log_2 <- felm(Omega_NProv ~ 0 + temp + log(temp+1)|clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_np),data=NK_c_equal)
                #model_NKNonMarket_log_2 <- lm(Omega_NProv ~ 0 + temp + log(temp+1)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_np),data=NK_c)

                
                model_NKMarket_2sq <- felm(Omega_Prov ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_p),data=NK_c_equal)
                #model_NKMarket_2sq <- lm(Omega_Prov ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_p),data=NK_c)
                
                model_NKNonMarket_2sq <- felm(Omega_NProv ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|0,weights=(1/NK_c_equal$error_rf_np),data=NK_c_equal)
                #model_NKNonMarket_2sq <- lm(Omega_NProv ~ 0 + temp + I(temp^2)|clim+sceni+dgvm|0|clim+sceni+dgvm,weights=(1/NK_c$error_rf_np),data=NK_c)


                Omega_NK_data <- rbind(data.frame(country = isos[i], 
                    Omega_NK_c1 = NA, 
                    Omega_NK_c1_se = NA, 
                    Omega_NK_c1_pval = NA,
                    Omega_NK_c2 = summary(model_NKMarket)$coefficient[1], 
                    Omega_NK_c2_se = summary(model_NKMarket)$coefficient[2],
                    Omega_NK_c2_pval = summary(model_NKMarket)$coefficient[4],
                    Omega_NK_rsq = summary(model_NKMarket)$adj.r.squared,
                    NK_type="Market",
                    Function = "Sq"),
                    data.frame(country = isos[i], 
                    Omega_NK_c1 = NA, 
                    Omega_NK_c1_se = NA,
                    Omega_NK_c1_pval = NA,
                    Omega_NK_c2 = summary(model_NKNonMarket)$coefficient[1], 
                    Omega_NK_c2_se = summary(model_NKNonMarket)$coefficient[2],
                    Omega_NK_c2_pval = summary(model_NKNonMarket)$coefficient[4],
                    Omega_NK_rsq = summary(model_NKNonMarket)$adj.r.squared,
                    NK_type="NonMarket",
                    Function = "Sq"),
                    data.frame(country = isos[i], 
                    Omega_NK_c1 = NA, 
                    Omega_NK_c1_se = NA, 
                    Omega_NK_c1_pval = NA,
                    Omega_NK_c2 = summary(model_NKMarket_eq)$coefficient[1], 
                    Omega_NK_c2_se = summary(model_NKMarket_eq)$coefficient[2],
                    Omega_NK_c2_pval = summary(model_NKMarket_eq)$coefficient[4],
                    Omega_NK_rsq = summary(model_NKMarket_eq)$adj.r.squared,
                    NK_type="Market",
                    Function = "SqEq"),
                    data.frame(country = isos[i], 
                    Omega_NK_c1 = NA, 
                    Omega_NK_c1_se = NA,
                    Omega_NK_c1_pval = NA,
                    Omega_NK_c2 = summary(model_NKNonMarket_eq)$coefficient[1], 
                    Omega_NK_c2_se = summary(model_NKNonMarket_eq)$coefficient[2],
                    Omega_NK_c2_pval = summary(model_NKNonMarket_eq)$coefficient[4],
                    Omega_NK_rsq = summary(model_NKNonMarket_eq)$adj.r.squared,
                    NK_type="NonMarket",
                    Function = "SqEq"),
                    data.frame(country = isos[i], 
                    Omega_NK_c1 = NA,
                    Omega_NK_c1_se = NA,
                    Omega_NK_c1_pval = NA,
                    Omega_NK_c2 = summary(model_NKMarket_log)$coefficient[1], 
                    Omega_NK_c2_se = summary(model_NKMarket_log)$coefficient[2],
                    Omega_NK_c2_pval = summary(model_NKMarket_log)$coefficient[4],
                    Omega_NK_rsq = summary(model_NKMarket_log)$adj.r.squared,
                    NK_type="Market",
                    Function = "Log"),
                    data.frame(country = isos[i], 
                    Omega_NK_c1 = NA,
                    Omega_NK_c1_se = NA,
                    Omega_NK_c1_pval = NA,
                    Omega_NK_c2 = summary(model_NKNonMarket_log)$coefficient[1], 
                    Omega_NK_c2_se = summary(model_NKNonMarket_log)$coefficient[2],
                    Omega_NK_c2_pval = summary(model_NKNonMarket_log)$coefficient[4],
                    Omega_NK_rsq = summary(model_NKNonMarket_log)$adj.r.squared,
                    NK_type="NonMarket",
                    Function = "Log"),
                    data.frame(country = isos[i], 
                    Omega_NK_c1 = summary(model_NKMarket_2sq)$coefficient[1], 
                    Omega_NK_c1_se = summary(model_NKMarket_2sq)$coefficient[3],
                    Omega_NK_c1_pval = summary(model_NKMarket_2sq)$coefficient[7],
                    Omega_NK_c2 = summary(model_NKMarket_2sq)$coefficient[2], 
                    Omega_NK_c2_se = summary(model_NKMarket_2sq)$coefficient[4],
                    Omega_NK_c2_pval = summary(model_NKMarket_2sq)$coefficient[8],
                    Omega_NK_rsq = summary(model_NKMarket_2sq)$adj.r.squared,
                    NK_type="Market",
                    Function = "Sq 2 coeff"),
                    data.frame(country = isos[i], 
                    Omega_NK_c1 = summary(model_NKNonMarket_2sq)$coefficient[1], 
                    Omega_NK_c1_se = summary(model_NKNonMarket_2sq)$coefficient[3],
                    Omega_NK_c1_pval = summary(model_NKNonMarket_2sq)$coefficient[7],
                    Omega_NK_c2 = summary(model_NKNonMarket_2sq)$coefficient[2], 
                    Omega_NK_c2_se = summary(model_NKNonMarket_2sq)$coefficient[4],
                    Omega_NK_c2_pval = summary(model_NKNonMarket_2sq)$coefficient[8],
                    Omega_NK_rsq = summary(model_NKNonMarket_2sq)$adj.r.squared,
                    NK_type="NonMarket",
                    Function = "Sq 2 coeff"),
                    data.frame(country = isos[i], 
                    Omega_NK_c1 = summary(model_NKMarket_log_2)$coefficient[1], 
                    Omega_NK_c1_se = summary(model_NKMarket_log_2)$coefficient[3],
                    Omega_NK_c1_pval = summary(model_NKMarket_log_2)$coefficient[7],
                    Omega_NK_c2 = summary(model_NKMarket_log_2)$coefficient[2], 
                    Omega_NK_c2_se = summary(model_NKMarket_log_2)$coefficient[4],
                    Omega_NK_c2_pval = summary(model_NKMarket_log_2)$coefficient[8],
                    Omega_NK_rsq = summary(model_NKMarket_log_2)$adj.r.squared,
                    NK_type="Market",
                    Function = "Log 2 coeff"),
                    data.frame(country = isos[i], 
                    Omega_NK_c1 = summary(model_NKNonMarket_log_2)$coefficient[1], 
                    Omega_NK_c1_se = summary(model_NKNonMarket_log_2)$coefficient[3],
                    Omega_NK_c1_pval = summary(model_NKNonMarket_log_2)$coefficient[7],
                    Omega_NK_c2 = summary(model_NKNonMarket_log_2)$coefficient[2], 
                    Omega_NK_c2_se = summary(model_NKNonMarket_log_2)$coefficient[4],
                    Omega_NK_c2_pval = summary(model_NKNonMarket_log_2)$coefficient[8],
                    Omega_NK_rsq = summary(model_NKNonMarket_log_2)$adj.r.squared,
                    NK_type="NonMarket",
                    Function = "Log 2 coeff")
                    )
                
                if (i ==1){
                    Omega_NK <- Omega_NK_data 
                } else {
                    
                    Omega_NK <- rbind(Omega_NK,Omega_NK_data)
                }
                






             
                  #ggsave(country_damagefunction_Market, file=paste0("Figures/CountriesDamFun/quad_damagefunction_", isos[i],".png"), dpi=300)
                # # ggsave(country_damagefunction_Market_loess, file=paste0("Figures/Countries/LOESS_damagefunction_", isos[i],".png"), dpi=300)
                 #ggsave(country_damagefunction_Market_equal, file=paste0("Figures/Countries/Equal_damagefunction_", isos[i],".png"), dpi=300)          
                #  ggsave(country_damagefunction_Market_equal, file=paste0("Figures/Countries/EqualQuad_damagefunction_", isos[i],".png"), dpi=300)
                  #ggsave(country_damagefunction_LOG, file=paste0("Figures/Countries/LOG_damagefunction_", isos[i],".png"), dpi=300)
                  #ggsave(country_damagefunction_LOG2coeff, file=paste0("Figures/Countries/2coeff_log_damagefunction_", isos[i],".png"), dpi=300)

                  #ggsave(country_damagefunction_Market2coeff, file=paste0("Figures/Countries/2coeff_quad_damagefunction_", isos[i],".png"), dpi=300)
                # ggsave(country_damagefunction_Market_loess, file=paste0("Figures/3dgvm_Countries/LOESS_damagefunction_", isos[i],".png"), dpi=300)
                # ggsave(country_damagefunction_Market_equal, file=paste0("Figures/3dgvm_Countries/Equal_damagefunction_", isos[i],".png"), dpi=300)          
                # ggsave(country_damagefunction_Market_equal_quadratic, file=paste0("Figures/3dgvm_Countries/EqualQuad_damagefunction_", isos[i],".png"), dpi=300)
                # ggsave(country_damagefunction_LOG, file=paste0("Figures/3dgvm_Countries/2coeff_LOG_damagefunction_", isos[i],".png"), dpi=300)
            }

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

         

            Omega_NK$iso3 <- countrycode(Omega_NK$country, origin="iso2c",destination="iso3c")
            save(file="Data/Omega_NK_raw_final.Rda",Omega_NK)

          
           

            pvalm <- ggplot(Omega_NK[which(Omega_NK$Function=="Sq"&Omega_NK$NK_type=="Market"),], aes(Omega_NK_c2_pval)) +
            geom_histogram(fill="indianred",breaks=c(0,0.01,0.05,seq(1:100)*0.1))+
            theme_bw() + ggtitle("Market NC damage function coefficient")+ xlab ("") + 
            geom_vline(aes(xintercept=0.01),linetype="dashed")+xlim(0,1)+
            annotate("text", x=0.02, y=60, label="0.01", angle=90,size=3)+ 
            geom_vline(aes(xintercept=0.05),linetype="dashed")+
            annotate("text", x=0.06, y=40, label="0.05", angle=90,size=3)+ 
            geom_vline(aes(xintercept=0.1),linetype="dashed")+
            annotate("text", x=0.11, y=20, label="0.1", angle=90,size=3) + ylab("count")+ xlab("p-value")

            pvaln <- ggplot(Omega_NK[which(Omega_NK$Function=="Sq"&Omega_NK$NK_type=="NonMarket"),], aes(Omega_NK_c2_pval)) +
            geom_histogram(fill="darkcyan",breaks=c(0,0.01,0.05,seq(1:100)*0.1))+
            theme_bw() + ggtitle("Non-market NC damage function coefficient")+ xlab ("") + 
            geom_vline(aes(xintercept=0.01),linetype="dashed")+xlim(0,1)+
            annotate("text", x=0.02, y=60, label="0.01", angle=90,size=3)+ 
            geom_vline(aes(xintercept=0.05),linetype="dashed")+
            annotate("text", x=0.06, y=40, label="0.05", angle=90,size=3)+ 
            geom_vline(aes(xintercept=0.1),linetype="dashed")+
            annotate("text", x=0.11, y=20, label="0.1", angle=90,size=3) + ylab("count")+ xlab("p-value")

            ggarrange(pvalm,pvaln,nrow=2)
            
            ggsave("Figures/pval_estimates_Sq.png",dpi=300)

       


            Omega_NK[Omega_NK$NK_type=="Market" & Omega_NK$Function=="Sq",]
        
            c <- data.frame(iso3 = countrycode(world$iso_a2,origin="iso2c", destination="iso3c"),Continent=world$region_un,geom=world$geom)

            Omega_NK <- merge(Omega_NK,c,by="iso3",all.x=TRUE)
            glimpse(Omega_NK)
            
            worldna <- merge(c,Omega_NK,by="iso3",all.x=TRUE)
     

           Map_MarketDam <-  ggplot(Omega_NK[which(Omega_NK$Function=="Sq" & Omega_NK$NK_type=="Market"),]) +
                theme_void()+
                geom_sf(aes(geometry = geometry, fill = Omega_NK_c2)) +
                scale_fill_gradientn(colours = pal,limits=c(-0.03,0.03),
                    na.value="transparent",name="T^2 damage estimate (%)",oob = scales::squish)+
                theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) + #facet_wrap(~NK_type)+
                #geom_sf(data=worldna[which(!is.na(worldna$name_long)),],aes(geometry = geometry))+
                geom_sf(data=worldna,aes(geometry = geometry.x))+
                ggtitle("Non-market NC damage coefficient")

            Map_NonMarketDam <-    ggplot(Omega_NK[which(Omega_NK$Function=="Sq"& Omega_NK$NK_type=="NonMarket"),]) +
                theme_void()+
                geom_sf(aes(geometry = geometry, fill = Omega_NK_c2)) +
                scale_fill_gradientn(colours = pal,limits=c(-0.03,0.03),
                    na.value="transparent",name="T^2 damage estimate (%)",oob = scales::squish)+
                theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) + #facet_wrap(~NK_type)+
                #geom_sf(data=worldna[which(!is.na(worldna$name_long)),],aes(geometry = geometry))+
                geom_sf(data=worldna,aes(geometry = geometry.x))+
                ggtitle("Non-market NC damage coefficient")

                ggarrange(Map_MarketDam,Map_NonMarketDam)
                ggsave("Figures/DamCoeffs_FINAL.png",dpi=600)
            
            Omega_NK_damage_function <- Omega_NK[which(Omega_NK$Function=="Sq"),]
            Omega_NK_damage_function <- Omega_NK_damage_function[,c(1,6,7,10)]
            
            
            head(Omega_NK_damage_function)
            write.csv(Omega_NK_damage_function,file="Data/Omega_NK_damage_function_sq_FINAL.csv")

            glimpse(wealth2018)


            w2018 <- data.frame(iso3=wealth2018$countrycode,K=wealth2018$K, 
                    TotalWealth=wealth2018$TotalWealth, 
                    NK_Market=wealth2018$NforestT,
                    NK_NonMarket=(wealth2018$NforestES + wealth2018$Npa),year=2018)
                    glimpse(w2018)
            write.csv(w2018,file="Data/w2018.csv")







# Calculate Nat Cap Damage
            
# Projections (start)

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
        wdata2018 <- w[which(w$year==2018),]
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
        
        #NC_change$countryiso3 <- countrycode(NC_change$country, origin="iso2c", destination="iso3c" )
        #NC_dam$countryiso3 <- countrycode(NC_dam$country, origin="iso2c", destination="iso3c" )
        #NC_damse$countryiso3 <- countrycode(NC_damse$country, origin="iso2c", destination="iso3c" )
        simname <- c("nodam","10p","SSP245","NDC_cond")
        simname <- c("nodam","RCP6")
        glimpse(Omega_NK)
        TypeESi <- c("Provisioning","Non-provisioning")


        gama_3 <- data.frame(iso3=wealth_sim$countrycode,gamma_3 = wealth_sim$NatRents*0.01)
        write.csv(gama_3, file = "Data/gamma3.csv")

        
        #df_tas<-aggregate(df_tas)

        

        first <- 1
        Function <- "Sq"
        for(ss in 1:length(simname)){
            
                
            for (i in 1:length(country)){
                s=0.3 #fixed savings rate
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
                    nt_dam=numeric(length=t);nt_dam[1]=n0
                    nt_es_dam=numeric(length=t);nt_es_dam[1]=n0_es
                for(j in 2:t){
                    kt[j]=kt[j-1]*(1-0)+(yt[j-1])*s

                    if (simname[ss]=="nodam"){
                        nt_dam[j] = nt[1]
                        nt_es_dam[j] = nt_es[1]
                    }else if(simname[ss]=="10p"){
                        if(j==6){nt_dam[j] = nt[1]*0.9}else{
                            nt_dam[j] = nt[1]
                            nt_es_dam[j] = nt_es[1]}
                            
                        } else if(simname[ss]=="RCP6"){
                        if(j>5){
                            nt_dam[j] = nt[1]*(1+

                                # sum(Omega_NK$Omega_NK_c1[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function=="Log")]*
                                # (Temp_ssps[j+2,2]-Temp_ssps[1,2])  ,

                                # Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function=="Log")]*
                                # log(Temp_ssps[j+2,4]-Temp_ssps[1,4]+1), na.rm=TRUE))

                                sum(Omega_NK$Omega_NK_c1[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)]*
                                #(Temp_ssps[j+2,2]-Temp_ssps[1,2])  ,
                                (Temp_ssps[j+2,4]-Temp_ssps[1,4])  ,

                                Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)]*
                                #(Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)]-Omega_NK$Omega_NK_c2_se[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)])*
                                (Temp_ssps[j+2,4]-Temp_ssps[1,4])^2, na.rm=TRUE))
                                #(Temp_ssps[j+2,4])^2, na.rm=TRUE))
                                
                                
                            nt_es_dam[j] = nt_es[1]*(1+

                                # sum(Omega_NK$Omega_NK_c1[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function=="Log")]*
                                # (Temp_ssps[j+2,4]-Temp_ssps[1,4])  ,

                                # Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function=="Log")]*
                                # log(Temp_ssps[j+2,4]-Temp_ssps[1,4]+1), na.rm=TRUE))

                                sum(Omega_NK$Omega_NK_c1[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function==Function)]*
                                (Temp_ssps[j+2,4]-Temp_ssps[1,4])  ,
                                #(Temp_ssps[j+2,4])  ,

                                (Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function==Function)])*
                                #(Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function==Function)] - Omega_NK$Omega_NK_c2_se[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function==Function)] )*
                                (Temp_ssps[j+2,4]-Temp_ssps[1,4])^2, na.rm=TRUE))
                                #(Temp_ssps[j+2,4])^2, na.rm=TRUE))
                                
                                
                        
                        }else{
                            nt_dam[j] = nt[1]
                            nt_es_dam[j] = nt_es[1]
                        }

                    } else if(simname[ss]=="NDC_cond"){
                        if(j>5){
                            nt_dam[j] = nt[1]*(1+

                                sum(Omega_NK$Omega_NK_c1[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)]*
                                (Temp_ssps[j+2,2]-Temp_ssps[1,2])  ,

                                Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)]*
                                log(Temp_ssps[j+2,2]-Temp_ssps[1,2]+1), na.rm=TRUE))

                                # sum(Omega_NK$Omega_NK_c1[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)]*
                                # (Temp_ssps[j+2,3]-Temp_ssps[1,3])  ,
                                # #(Temp_ssps[j+2,3])  ,

                                # Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)]*
                                # (Temp_ssps[j+2,3]-Temp_ssps[1,3])^2, na.rm=TRUE))
                                # #(Temp_ssps[j+2,3])^2, na.rm=TRUE))
                                
                              
                            nt_es_dam[j] = nt_es[1]*(1+

                                sum(Omega_NK$Omega_NK_c1[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function=="Log")]*
                                (Temp_ssps[j+2,2]-Temp_ssps[1,2])  ,

                                Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function=="Log")]*
                                log(Temp_ssps[j+2,2]-Temp_ssps[1,2]+1) , na.rm=TRUE))

                                # sum(Omega_NK$Omega_NK_c1[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function==Function)]*
                                # (Temp_ssps[j+2,3]-Temp_ssps[1,3])  ,
                                # #(Temp_ssps[j+2,3])  ,


                                # Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function==Function)]*
                                # (Temp_ssps[j+2,3]-Temp_ssps[1,3])^2 , na.rm=TRUE))
                                # #(Temp_ssps[j+2,3])^2 , na.rm=TRUE))


                                
                                
                               
                        
                        }else{
                            nt_dam[j] = nt[1]
                            nt_es_dam[j] = nt_es[1]
                        }
                        
                    }
                    if(length(nt_dam[j][[1]])==0){next}
                    yt[j]=pi1*(h0^a1)*(kt[j]^(a2))*nt_dam[j][[1]]^a3
                    est[j]=nt_es_dam[j][[1]]*0.03  #Because Natural Capital is the net present value of the flow of benefits
                    }
                    gy <- (yt - lag(yt))/lag(yt) #output growth
                    meangy <- mean(gy[3:length(gy)]) #mean output growth 
                    npv <- sum(yt*(1/((1+0.03)^seq(1:length(yt)))))
                    dam_estimate_prov <- Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function=="Sq 2 coeff")]
                    dam_estimate_nonprov <- Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function=="Sq 2 coeff")]
                    if(length(dam_estimate_nonprov)==0){
                        print(paste("next",i))
                        next}
                    

                    
                    if(first==1){
                        first <- 0
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
                                #Continent=wealth_sim$Continent[wealth_sim$countrycode==country[i]],
                                n=nt,
                                gy = gy, meangy = meangy,exp=simname[ss],npv=npv,dam_prov=dam_estimate_prov, dam_nonprov = dam_estimate_nonprov ,
                                es = est, n_es = nt_es,elasH=a1,elasN=a3,elasK=a2,tfp=pi1 )
                    }else{
                        df <- bind_rows(df,data.frame(countrycode=country[i], year=year+2017, H=h0, N=wealth_sim$N[wealth_sim$countrycode==country[i]], 
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
                                #Continent=wealth_sim$Continent[wealth_sim$countrycode==country[i]],
                                n=nt,
                                gy = gy, meangy = meangy,exp=simname[ss],npv=npv,dam_prov=dam_estimate_prov, dam_nonprov = dam_estimate_nonprov ,
                                es = est, n_es = nt_es ,elasH=a1,elasN=a3,elasK=a2,tfp=pi1 )) 
                    }
                #wealth_sim <- rbind(wealth_sim,df)
                }
            }
            
        

        
        save(df,file="Results/sim_nc_se.Rda")
        #load("Results/sim_nc.Rda")
        df_se_central <- df


        

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
        #ggplot(data= wealth_sim3, 
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
        glimpse(df)
        df <- df
        all_sim <- data.frame(gdp_change = (df$GDP - df$GDP[df$exp=="nodam"])/df$GDP[df$exp=="nodam"],
        es_change = (df$es - df$es[df$exp=="nodam"])/df$es[df$exp=="nodam"],
         growth_change = df$gy - df$gy[df$exp=="nodam"],
        npv_change =  df$npv - df$npv[df$exp=="nodam"], 
        Nchange =  df$n - df$n[df$exp=="nodam"], 
        Nchange_es =  df$n_es - df$n_es[df$exp=="nodam"], 
        countrycode = df$country, year=df$year, H=df$H,
        N=df$N   ,  Nagg=df$Nagg,
        NforestT = df$NforestT,   
        NforestES = df$NforestES,   Npa=df$Npa,
        K=df$K,          TotalWealth=df$TotalWealth,
        GDP=df$GDP,      Population=df$Population,  
        labor_share=df$labor_share, NatRents=df$NatRents,    
        #Continent = df$Continent, 
        n=df$n, n_es=df$n_es, gy=df$gy,          meangy=df$meangy,      
        exp=df$exp   , npv = df$npv , es=df$es )

        c <- data.frame(countrycode = countrycode(world$iso_a2,origin="iso2c", destination="iso3c"),Continent=world$region_un)

        all_sim <- merge(all_sim,c,by="countrycode",all.x=TRUE)
        all_sim$country <- all_sim$countrycode

        glimpse(all_sim)

        all_sim_pos <- all_sim
        
    #     sim_2100 <- all_sim[all_sim$exp %notin% c("10p","nodam") & all_sim$year %in% c(2100),]
    #     meanGrowthChange <- aggregate(growth_change~country+exp,FUN="mean",data=sim_2100)
    #     names(meanGrowthChange)[3] <- "MeanGrowthChange"
        
    #     sim_2100 <- merge(sim_2100,meanGrowthChange,by=c("country","exp"))
    #     glimpse(sim_2100)

        
    # ggplot(data = sim_2100, 
    #         #aes(x=MeanGrowthChange*100,y=log(GDP/Population),color=Continent,shape=exp))+
    #         aes(x=MeanGrowthChange*100,y=log(GDP/Population),shape=exp))+
    #         xlab("Mean GDP growth change (pp)")+ylab("Log GDP per capita")+
    #     theme_bw()+
    #     geom_point(aes(size=NatRents*0.01))+ggtitle("Change in mean GDP growth (2020-2100)")+
    #     guides(color=guide_legend(title="Region"),size=guide_legend(title="Natural capital\n elasticity"),shape=guide_legend(title="Scenario"))
    #    #ggsave("Change_GDP_growth_ssps_ndc.png")

    #    all_sim_af <- all_sim[all_sim$country=="MEX",]
    #    ggplot(all_sim_af, aes(x=year,y=gdp_change,color=exp,group=interaction(exp)))+
    #    geom_line()+theme_bw()

    #    glimpse(all_sim)
       #all_sim <-all_sim[,-which(names(all_sim) %in% c("clim"))]
       all_simrcp6<-all_sim[which(all_sim$exp %in% c("RCP6")),]
    #    all_sim2 <- all_sim[,which(names(all_sim) %in% c("es_change","gdp_change","country","year",'exp',"Continent","GDP"))]
    #    write.csv(all_sim2,file= "Data/Trajectories_Plot_Data.csv")
    # save(all_sim2,file= "Trajectories_Plot_Data.Rda")
    
    #    #all_sim_mean <- stats::aggregate(.~country+year+exp+Continent,data=all_sim2, FUN="mean")
    #    all_sim_mean <- stats::aggregate(.~country+year+exp+Continent,data=all_sim2, FUN="mean")
    #    all_sim_sd <- aggregate(.~country+year+exp+Continent,data=all_sim2, FUN="sd")
    #     glimpse(all_sim_mean)
    
    # save(all_sim_mean,file= "all_sim_mean.Rda")
    # load("all_sim_mean.Rda")
    # glimpse(all_sim)
           
    #        write.csv(all_sim, file="all_sim.csv")
    
    all_sim <- all_sim_central
     
    GDP2100 <- ggplot(all_sim[which(all_sim$exp=="RCP6" & all_sim$year==2100),])+
       geom_point(aes(x=log(GDP),y=gdp_change*100,color=Continent))+theme_bw() + 
       geom_smooth(data=all_sim[which(all_sim$exp=="RCP6" & all_sim$year==2100),],aes(x=log(GDP),y=gdp_change*100))+ylab("GDP change (%)")
        #geom_smooth(data=all_sim[which(all_sim$exp=="SSP245" & all_sim$year==2100),],aes(x=log(GDP),y=gdp_change*100),method="lm",formula="y~x+I(x^2)")


    ES2100 <- ggplot(all_sim[which(all_sim$exp=="RCP6" & all_sim$year==2100),])+
       geom_point(aes(x=log(GDP),y=es_change*100,color=Continent))+theme_bw() + geom_smooth(data=all_sim[which(all_sim$exp=="RCP6" & all_sim$year==2100),],
     aes(x=log(GDP),y=es_change*100))+ylab("Ecosystem benefits change (%)")
    
     ggarrange(GDP2100,ES2100,common.legend=TRUE,legend="bottom")

    

       
    #    glimpse(all_sim_mean)
    #    glimpse(all_sim_sd)
    #    names(all_sim_sd)[5:6] <- c("gdp_change_sd","es_change_sd")
    #    all_sim_sd$id <- paste0(all_sim_sd$country,all_sim_sd$year,all_sim_sd$exp)
    #    all_sim_mean$id <- paste0(all_sim_mean$country,all_sim_mean$year,all_sim_mean$exp)

    #     all_sim_mean <- all_sim_mean[which(all_sim_mean$es_change<quantile(all_sim_mean$es_change,0.95)),]
    #     glimpse(all_sim_mean)
    #     glimpse(all_sim)
        
    #     all_sim_mean <- merge(all_sim_mean,all_sim_sd[,c(5,6,8)],by="id",all=FALSE)

    # #all_sim[which(all_sim$exp=="SSP245" & all_sim$es_change==max(all_sim$es_change[which(all_sim$exp=="SSP245" )])),]
    
    #     #all_sim <- all_sim[which(all_sim$es_change<quantile(all_sim$es_change,0.99,na.rm=TRUE)),]
    #     #all_sim <- all_sim[which(all_sim$gdp_change>quantile(all_sim$gdp_change,0.01,na.rm=TRUE)),]

        
    #    #all_sim <- all_sim[-which(all_sim$country%in%all_sim$country[which.min(all_sim$gdp_change)]),]
    #    #min(all_simssp245$gdp_change)
    #     all_sim <-all_sim[-which(all_sim$country%in%all_simrcp6$country[which.max(all_simrcp6$es_change)]),]
    #     all_sim <-all_sim[-which(all_sim$country%in%all_sim$country[which.min(all_sim$gdp_change)]),]

    #     #allsim2100 <- all_sim[which(all_sim$exp=="SSP245" & all_sim$year ==2100),]
    #     #allsim2100$country[which(allsim2100$es_change > quantile(allsim2100$es_change,0.99,na.rm=TRUE))]

    #     # all_sim$country[which(all_sim$exp=="SSP245" & all_sim$es_change>quantile(all_sim$es_change,0.99,na.rm=TRUE))]

    glimpse(all_sim_central)

    all_sim_central$gdp_change_sd_low <- all_sim_neg$gdp_change
    all_sim_central$gdp_change_sd_high <- all_sim_pos$gdp_change

    
    all_sim_central$es_change_sd_low <- all_sim_neg$es_change
    all_sim_central$es_change_sd_high <- all_sim_pos$es_change



     plot_trajectory <- ggplot(all_sim[which(all_sim$exp=="RCP6" & all_sim$country!="SWZ"),],
     aes(x=es_change*100,y=gdp_change*100,color=Continent,group=country))+
     #aes(x=es_change*100,y=gdp_change*100,group=countrycode))+
     geom_hline(aes(yintercept=0),linetype="dashed")+
     geom_vline(aes(xintercept=0),linetype="dashed")+
       geom_line()+
       geom_point(data = all_sim[which(all_sim$exp=="RCP6" & all_sim$year %in% c(2050,2080,2100) ),], 
        aes(x=es_change*100,y=gdp_change*100,color=Continent,shape=factor(year)))+
        #ymin=gdp_change-gdp_change_sd,ymax=gdp_change+gdp_change_sd))+
       theme_bw()+
        geom_errorbar(data=all_sim_central[which(all_sim_central$exp=="RCP6" & all_sim_central$year %in% c(2100) ),],
        aes(ymin=(gdp_change_sd_low)*100, ymax=(gdp_change_sd_high)*100),alpha=0.3)+
        geom_errorbarh(data=all_sim_central[which(all_sim_central$exp=="RCP6" & all_sim_central$year %in% c(2100) ),],
        aes(xmin=(es_change_sd_low)*100, xmax=(es_change_sd_high)*100),alpha=0.3)+
        theme(legend.position="bottom",legend.box="vertical")+
       labs(color="Region",shape="Year")+
       xlab("Ecosystem services change (%)") +
       ylab("GDP change (%)") + ggtitle("")  
        #coord_cartesian(xlim=c(-100, 60),ylim=c(-15,6))
       plot_trajectory

      leg_trj <-  get_legend(plot_trajectory)

    maxyear <- aggregate(year~country, data=all_sim[which(all_sim$exp=="RCP6" ),],FUN="max")
    maxyear$countrymax <- paste0(maxyear$country,maxyear$year)
    all_sim$countrymax <- paste0(all_sim$country,all_sim$year)

    all_simmax <- all_sim[all_sim$countrymax %in% maxyear$countrymax,]
    
plot_count_es <- ggplot(all_sim[which(all_sim$exp=="RCP6" & all_sim$year ==2100),], # & abs(all_sim_mean$es_change)>0.01
     aes(x=es_change*100,fill=Continent))+
     geom_histogram()+theme_minimal()+ xlab("")+ylab("count in 2100") #+xlim(-12,5)#+ xlim(-100,60)

     mean(all_sim$es_change[which(all_sim$exp=="SSP245" & all_sim$year ==2100)])
     
     
     aggregate(gdp_change~Continent,data=all_sim[which(all_sim$exp=="SSP245" & all_sim$year ==2100),],FUN="mean")
    aggregate(es_change~Continent,data=all_sim[which(all_sim$exp=="SSP245" & all_sim$year ==2100),],FUN="mean")

     
     

plot_count_gdp <- ggplot(all_sim[which(all_sim$exp=="RCP6" & all_sim$year ==2100 ),], #& abs(all_sim_mean$gdp_change)>0.01
     aes(x=gdp_change*100,fill=Continent))+xlab('')+#xlim(-15,6)+
     ylab("count in 2100")+
     geom_histogram()+theme_minimal()+coord_flip() 
mean(all_sim$gdp_change[which(all_sim$exp=="SSP245" & all_sim$year ==2100 )])
mean(all_sim$gdp_change[which(all_sim$exp=="NDC_cond" & all_sim$year ==2100 )])
mean(all_sim$es_change[which(all_sim$exp=="NDC_cond" & all_sim$year ==2100 )])
mean(all_sim$es_change[which(all_sim$exp=="SSP245" & all_sim$year ==2100 )])


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
    
    annotate_figure(traj_plot, top = text_grob("Ecosystem Benefits change under RCP6.0", 
                face = "bold", size = 14))
    #ggsave("Figures/Trajectory_completeHist.png",dpi=300)

     ggarrange(ggarrange(plot_trajectory, plot_count_gdp,nrow=1,ncol=2,widths=c(3,1),legend="none",align="hv"),
        ggarrange(plot_count_es,emptyplot,nrow=1,ncol=2,widths=c(3,1),common.legend=TRUE,align="hv",legend="none"),as_ggplot(leg_trj),
        ncol=1,nrow=3,common.legend=TRUE,heights=c(4,1,1),legend="none",align="hv")
ggsave("Figures/Trajectories_RCP6_SQ_FINAL.png",dpi=300)

# Projections (end)

            
                                    










                                    
                                    areasPFTs_database <- wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) %in% eff_pft$variable)]
                                    sum_areasPFTs <- sum(wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) %in% eff_pft$variable)]*eff_pft$importance[order(eff_pft$variable,names(areasPFTs_database))],na.rm=TRUE)
                        
                        
                                    if(sum_areasPFTs==0){print("sum areas are zero")
                                    next}
                                    r = 0.03
                                
                                    ES = sum(wealth2018_pft[which(wealth2018_pft$country==isos[j]),which(names(wealth2018_pft) %in% typeES)] * r / sum_areasPFTs)
                                    

                                    glimpse(PFT_movement_c_run_present)

                                    
                                    
                                }
                            }













                            glimpse(PFT_movement_c_run_present)
                            glimpse(PFT_movement_c_run)
                            
                            glimpse(a)
                            glimpse()

                            
                            
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

# Calculate Nat Cap Damage









































        
        
        a_lpj_2005soc <- merge(a_lpj_2005soc,isos_xy,by="xy",all.x=TRUE)
        glimpse(a_lpj_2005soc)
        #save(a_h,file="Data/a_h.Rda")
        #load("Data/a_h.Rda")
        a_h$areaPFT <- a_h$Percent * a_h$area
        PFTs11_Clims4_horizons3 <- a_h
        a_h$x <- a_h$x.x
        a_h$y <- a_h$y.x
        glimpse(PFTs11_Clims4_horizons3)
        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3[,which(names(PFTs11_Clims4_horizons3) %in% c("y.y","PFT","areaPFT","horizon"))]
        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3_simple[which(PFTs11_Clims4_horizons3_simple$horizon %in% c("2C")),]
        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3_simple[which(PFTs11_Clims4_horizons3_simple$areaPFT >0),]
        glimpse(PFTs11_Clims4_horizons3_simple)
        PFTs11_horizons3 <- aggregate(areaPFT~PFT+y.y+horizon, FUN="mean", data=PFTs11_Clims4_horizons3_simple)
        glimpse(PFTs11_horizons3)
        save(PFTs11_horizons3,file="Data/PFTs11_horizons3.Rda")
        load("PFTs11_horizons3.Rda")

        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3[,which(names(PFTs11_Clims4_horizons3) %in% c("y.y","PFT","Percent","horizon"))]
        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3_simple[which(PFTs11_Clims4_horizons3_simple$horizon %in% c("2C")),]
        PFTs11_Clims4_horizons3_simple <- PFTs11_Clims4_horizons3_simple[which(PFTs11_Clims4_horizons3_simple$Percent >0),]
        glimpse(PFTs11_Clims4_horizons3_simple)
        PFTs11_horizons3_perc <- aggregate(Percent~PFT+y.y+horizon, FUN="mean", data=PFTs11_Clims4_horizons3_simple)
        glimpse(PFTs11_horizons3_perc)
        save(PFTs11_horizons3_perc,file="Data/PFTs11_horizons3_perc.Rda")
        load("PFTs11_horizons3_perc.Rda")
        
        save(PFTs11_Clims4_horizons3,file="Data/PFTs11_Clims4_horizons3.Rda")
        load("PFTs11_Clims4_horizons3.Rda")
        glimpse(PFTs11_Clims4_horizons3)
    # Read PFTs at different time horizons (end)
    
    load("PFTs11_Clims4_horizons3.Rda")
    
    #Get PFTs by latitude (start)
            a_h <- PFTs11_Clims4_horizons3
            a_h$horizon <- factor(a_h$horizon,levels=  c("2C"))
            a_h$Latitude <- abs(a_h$y.y)
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
            PFT_movement$horizon <- factor(PFT_movement$horizon,levels=  c("2C"))
            #PFT_movement$horizon <- factor(PFT_movement$horizon,levels=  c("Present","Near-term (2021-2040)","Mid-term (2041-2060)","Long-term (2081-2099)"))
            #levels(PFT_movement$horizon)
            #glimpse(PFT_movement)
            #write.csv(PFT_movement,"PFT_movement.csv")

        
            
        levels(factor(PFT_movement$PFT))
            PFT_movement_y <- aggregate(Percent ~ y.y + horizon + PFT, data=PFT_movement,FUN="mean")
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
        
    


# Plot latitudes

        world <- spData::world
        isos <- levels(factor(world$iso_a2))
        df_v <- as.data.frame(periods_tas_brick)
        t <- dif_periods_tas_brick
        t <- t(t)
        plot(mean(t))
        
            for (p in 1:length(pfts)){
                load(paste("dif_periods_",pfts[p],"_brick_rcp6.Rdata",sep=""))
                v1 <- eval(parse(text =paste("dif_periods_",pfts[p],"_brick_rcp6",sep="")))
                
            if (p>1){
                pft_sum <- pft_sum + v1} else {
                    pft_sum <-  v1
                }
                print(p)
            }

            pft_sum <- t(pft_sum)
            a_iso <- area(pft_sum)
            pft_sum_or <- pft_sum
# Computing damage in PFTs (end)


######## OLD
#Read NetCDF of TAS (start)

   
    tas_iso_year <- data.frame(iso2=factor(),
                    var = character(),
                    value=double(),
                    year=integer())
    
        annual <- array(numeric(),c(720,360,100)) 
        for (i in 1:10){ #NOTE i starts in 2 (i.e. from the year 2021)
        dname <- "tas"
        if (i==1){
            ncname <- paste(dir1,"tas_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_20060101-20101231.nc4",sep="")
        } else if (i <10){
            ncname <- paste(dir1,"tas_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_",toString(2001+((i-1)*10)),"0101-",
            toString(2010+((i-1)*10)),"1231.nc4",sep="")
        } else{
            ncname <- paste(dir1,"tas_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_20910101-20991231.nc4",sep="")
        }
        
        ncin <- nc_open(ncname)
        #print(ncin)    
        #get units
        lon <- ncvar_get(ncin,"lon")
        lat <- ncvar_get(ncin,"lat")
        time_tas <- ncvar_get(ncin,"time")
        tunits_tas <- ncatt_get(ncin,"time","units") #"days since 2006-1-1 00:00:00"
        

        numyears <- floor((length(time_tas)/(365)))
        years_tas <- 2006 + (time_tas[time_tas%%365==0])/365
        
        for (d in (1:numyears)){ 
            annual_array <- ncvar_get(ncin,dname, start = c(1,1,(1+(365)*(d-1))), count=c(-1,-1,365)) #start, number in dimension to start. count: how many in that dimension
            annual_mean <- rowMeans(annual_array, dims = 2, na.rm = TRUE)
            annual[,,years_tas[d]-2005] <- annual_mean
            print(d)
        }
        print(i)
    }
    yearly_tas_brick <- brick(annual, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
    save(yearly_tas_brick,file="yearly_tas_brick.Rdata")
#Read NetCDF of TAS (close)   
    #load("yearly_tas_brick.Rdata")

#decadal average TAS (start)

    glimpse(yearly_tas_brick)
    dim(yearly_tas_brick)
    var_array <- as.array(yearly_tas_brick)
    numperiods <- 4
    periods <- array(numeric(),c(720,360,numperiods)) 
    dif_periods <- array(numeric(),c(720,360,numperiods-1)) 
    periods_minyear <- c(1,16,36,76) #years 2016,2021,2041,2081
    periods_maxyear <- c(15,35,55,94) #years 2020,2040,2060,2099
        for (d in (1:numperiods)){ 
            period_array <- var_array[,,periods_minyear[d]:periods_maxyear[d]] #decade d of data
            period_mean <- rowMeans(period_array, dims = 2, na.rm = TRUE)
            periods[,,d] <- period_mean
            if (d>1){
                dif_periods[,,d-1] <- periods[,,d] - periods[,,1]
            }
        }
    periods_tas_brick <- brick(periods, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
    
    dif_periods_tas_brick <- brick(dif_periods, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs")) #2021-2090
    save(periods_tas_brick,file="periods_tas_brick.Rdata")    
    save(dif_periods_tas_brick,file="dif_periods_tas_brick.Rdata")    
    plot(t(dif_periods_tas_brick))


#decadal average TAS (end)

#load("periods_tas_brick.Rdata")
#load("dif_periods_tas_brick.Rdata")

# Computing damage in PFTs (start)

        world <- spData::world
        isos <- levels(factor(world$iso_a2))
        df_v <- as.data.frame(periods_tas_brick)
        t <- dif_periods_tas_brick
        t <- t(t)
        plot(mean(t))
        
            for (p in 1:length(pfts)){
                load(paste("dif_periods_",pfts[p],"_brick_rcp6.Rdata",sep=""))
                v1 <- eval(parse(text =paste("dif_periods_",pfts[p],"_brick_rcp6",sep="")))
                
            if (p>1){
                pft_sum <- pft_sum + v1} else {
                    pft_sum <-  v1
                }
                print(p)
            }

            pft_sum <- t(pft_sum)
            a_iso <- area(pft_sum)
            pft_sum_or <- pft_sum
# Computing damage in PFTs (end)

#Plotting maps at different time horizons (start)
            
            crs(pft_sum) <- "+init=EPSG:4326"
            pft_sum <- projectRaster(pft_sum, crs='+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
            
            
            
            pft_sum_poly_rob1 <- 
            raster::subset(pft_sum,1) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 

            pft_sum_poly_rob2 <- 
            raster::subset(pft_sum,2) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 

            pft_sum_poly_rob3 <- 
            raster::subset(pft_sum,3) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 
            
            
            world_coast <- ne_coastline(scale = "medium", returnclass = "sf")
            library(wesanderson)
            pal <- palette(brewer.pal(n = 3, name = "Spectral"))

            a <- ggplot() +
            theme_void() +
             geom_sf(data = pft_sum_poly_rob1,aes(fill = layer.1), color = NA)+
            scale_fill_gradientn(colours = pal,limits=c(-20,20),na.value="transparent",name="Land cover change (%)", oob = scales::squish)+
            
            #scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
            #                high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            geom_sf(data = world_coast)+
            ggtitle("Near-term (2021-2040)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            b <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob2,aes(fill = layer.2), color = NA) +
            scale_fill_gradientn(colours = pal,limits=c(-20,20),na.value="transparent",name="Land cover change (%)", oob = scales::squish)+
            #scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
                            #high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            geom_sf(data = world_coast)+
            ggtitle("Mid-term (2041-2060)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            c <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob3,aes(fill = layer.3), color = NA) +
            geom_sf(data = world_coast)+
            #scale_fill_gradient2(midpoint=0, low ="red" , mid = "white",high = "green",na.value="transparent",name="Land cover change (%)")+
            scale_fill_gradientn(colours = pal,limits=c(-20,20),na.value="transparent",name="Land cover change (%)", labels=c("<-20","-10","0","10",">20"),oob = scales::squish)+
            #scale_fill_distiller(palette = "Spectral", direction = 1,name="Land cover change (%)")+
            #scale_fill_npg()+
            geom_sf(data = world_coast)+
            ggtitle("Long-term (2081-2099)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            leg <- get_legend(c)

            c <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob3,aes(fill = layer.3), color = NA) +
            #scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
            #                high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            scale_fill_gradientn(colours = pal,,na.value="transparent",name="Land cover change (%)")+
            geom_sf(data = world_coast)+
            ggtitle("Long-term (2081-2099)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            ggarrange(a,b,c,ncol=1,nrow=3 ,legend.grob = leg,common.legend = TRUE, legend="bottom")
            ggsave("landcover_change.png",dpi=300)
#plotting maps at different time horizons (end)

#Getting country-level estimate (start)
            world <- spData::world
            world$coefDamage <- NA
            world$meanDamage <- NA
            isos <- world$iso_a2

            all_t <- as.data.frame(t)
            all_a <- as.data.frame(area(t))
            sum(all_t[,1]*all_a[,1])/sum(all_a[,1])
            sum(all_t[,2]*all_a[,1])/sum(all_a[,1])
            sum(all_t[,3]*all_a[,1])/sum(all_a[,1])



            for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                if(is.na(isos[j])){next}
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)


                #plot(crop(pft_sum,geom_iso))
                


                veg_iso <- raster::extract(pft_sum_or,geom_iso,metdod='simple', na.rm=TRUE)
                merged_veg_iso <- veg_iso[[1]]
                temp_iso <- raster::extract(t,geom_iso,metdod='simple', na.rm=TRUE)
                merged_temp_iso <- temp_iso[[1]]
                area_iso <- raster::extract(a_iso,geom_iso,metdod='simple', na.rm=TRUE)
                merged_area_iso <- area_iso[[1]]
                    
                        if (length(veg_iso)>1){
                            for(l in 2:length(veg_iso)){
                                merged_temp_iso <- rbind(merged_temp_iso,temp_iso[[l]])
                                merged_veg_iso <- rbind(merged_veg_iso,veg_iso[[l]])
                                merged_area_iso <- c(merged_area_iso,area_iso[[l]])
                            }
                        }
                veg_iso <- as.data.frame(merged_veg_iso)
                temp_iso <- as.data.frame(merged_temp_iso)
                area_iso <- as.data.frame(merged_area_iso)
                if (sum(complete.cases(veg_iso))<3){next}
                

                num_pix <- dim(veg_iso)[1]
                numyears <- length(veg_iso)
                colnames(veg_iso) <-  c("near-term (2021-2040)", "mid-term (2041-2060)", "long-term (2081-2099)")
                veg_iso <- stack(veg_iso)
                veg_iso$id <- rep(seq(1:num_pix),numyears)
                colnames(veg_iso) <- c("pftchange","period","id")

                num_pix <- dim(temp_iso)[1]
                colnames(temp_iso) <-   c("near-term (2021-2040)", "mid-term (2041-2060)", "long-term (2081-2099)")
                temp_iso <- stack(temp_iso)
                temp_iso$id <- rep(seq(1:num_pix),numyears)
                colnames(temp_iso) <- c("temp","period","id")

                

                cveg_temp <- cbind(temp_iso,veg_iso[,1])
                colnames(cveg_temp)[4] <- c("pfts_damage")
                cveg_temp$area <- c(area_iso[,1],area_iso[,1],area_iso[,1])
                #glimpse(cveg_temp)
                weightvar <- cveg_temp$area
                model <- felm(pfts_damage~temp|id|0|0,data=cveg_temp, weights = weightvar)
                #library('stargazer')
                #stargazer(model, type = "html", out="Figures/model_mex.html")


                

                ggplot(data=cveg_temp, aes(x = temp, y = pfts_damage,group=id,color=period))+
                theme_bw()+
                geom_boxplot(aes(group=period),outlier.shape = NA)+
                geom_point(alpha=0.05)+
                #coord_trans(y="log10") +
                xlab("Gridcell temperature change (C)") +
                ylab("Land cover change (%)") + 
                ggtitle(paste("PFTs change in",world$name_long[j],"under RCP6.0"))
                ggsave(paste("Figures/BoxplotsChange/",isos[j],"_temp_landcover_boxplot.png",sep=""),dpi=300)

                #aggregate(id~period, cveg_temp, function(i) weighted.mean(cveg_temp$temp[i], cveg_temp$area[i]))

                
                #sapply(split(cveg_temp, cveg_temp$period), function(d) weighted.mean(cveg_temp$temp, w = cveg_temp$area))
                
                # library('Hmisc')
                # library('dplyr')
                # wt <- plyr::ddply(cveg_temp, ~period, plyr::summarize, weighted.mean(temp, w=area))
                # wsdt <- plyr::ddply(cveg_temp, ~period, plyr::summarize, wtd.var(temp, w=area))
                # wp <- plyr::ddply(cveg_temp, ~period, plyr::summarize, weighted.mean(pfts_damage, w=area, na.rm = TRUE))
                # wsdp <- plyr::ddply(cveg_temp, ~period, plyr::summarize, wtd.var(pfts_damage, w=area))

                # w_mean <- cbind(wt,wsdt[,2]^0.5,wp[,2],wsdp[,2]^0.5)
                # colnames(w_mean) <- c("period","meanT","sdT","meanPFTs","sdPFTs")


                meanP <- plyr::summarize(cveg_temp, weighted.mean(pfts_damage, w=area, na.rm = TRUE))
                meanT <- plyr::summarize(cveg_temp, weighted.mean(temp, w=area, na.rm = TRUE))
                
                
                
                # ggplot()+
                # theme_bw()+
                # geom_point(data=cveg_temp, aes(x = temp, y = pfts_damage,group=id,color=period),alpha=0.05)+
                # #coord_trans(y="log10") +
                # xlab("Gridcell temperature change (C)") +
                # ylab("Land cover change (%)") + 
                # ggtitle("PFTs change in Mexico under RCP6.0")+
                # geom_point(data=w_mean, aes(x=meanT,y=meanPFTs, color = period))+
                # geom_errorbar(data=w_mean,aes(x = meanT, ymin=meanPFTs-sdPFTs, ymax=meanPFTs+sdPFTs), width=.2,
                #  position=position_dodge(.9))

                world$coefDamage[j] <- summary(model)$coef[1]
                world$meanDamage[j] <- as.double(unlist(meanP/meanT)[1])

            }
            world_damageestimates_pfts <- world
            save(world_damageestimates_pfts,file="world_damageestimates_pfts.Rdata")    
#Getting country-level estimate (end)

#Getting Warming Pattern (start)

            load("world_damageestimates_pfts.Rdata")    
            all_t <- as.data.frame(t)
            all_a <- as.data.frame(area(t))
            Tshort <- sum(all_t[,1]*all_a[,1])/sum(all_a[,1])
            Tmed <- sum(all_t[,2]*all_a[,1])/sum(all_a[,1])
            Tlong <- sum(all_t[,3]*all_a[,1])/sum(all_a[,1])
            isos <- unique(world_damageestimates_pfts$iso_a2)

        for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                if(is.na(isos[j])){next}
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)

                temp_iso <- raster::extract(t,geom_iso,metdod='simple', na.rm=TRUE)
                merged_temp_iso <- temp_iso[[1]]
                area_iso <- raster::extract(a_iso,geom_iso,metdod='simple', na.rm=TRUE)
                merged_area_iso <- area_iso[[1]]
                    
                        if (length(temp_iso)>1){
                            for(l in 2:length(temp_iso)){
                                merged_temp_iso <- rbind(merged_temp_iso,temp_iso[[l]])
                                merged_area_iso <- c(merged_area_iso,area_iso[[l]])
                            }
                        }
                temp_iso <- as.data.frame(merged_temp_iso)
                area_iso <- as.data.frame(merged_area_iso)
                if (sum(complete.cases(temp_iso))<3){next}

                patternshort <- weighted.mean(x=(temp_iso[,1] / Tshort),w=area_iso[,1])
                patternmed <- weighted.mean(x=(temp_iso[,2] / Tmed),w=area_iso[,1])
                patternlong <- weighted.mean(x=(temp_iso[,3] / Tlong),w=area_iso[,1])

                Tpattern <- mean(patternlong,patternshort,patternmed)

                world_damageestimates_pfts$Tpattern[j] <- Tpattern

            }
            save(world_damageestimates_pfts,file="world_damageestimates_pfts.Rdata")    
#Getting Warming Pattern (start)

# Plot map of estimates 

    ggplot(world_damageestimates_pfts) +
    theme_void()+
    geom_sf(aes(geometry = geom, fill = Tpattern)) +
    scale_fill_gradient(name="Temperature pattern (C/C)")+
    theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) +
    ggtitle("Country-level temperature pattern by global degree watming")
    ggsave("TempPattern.png",dpi=300)


    ggplot(world_damageestimates_pfts) +
    theme_void()+
    geom_sf(aes(geometry = geom, fill = coefDamage)) +
    scale_fill_gradientn(colours = pal,limits=c(-3,3),na.value="transparent",name="Land cover change (%)",oob = scales::squish)+
    theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) +
    ggtitle("Estimated damages based on simulated PFTs")

    ggplot(world_damageestimates_pfts) +
    theme_void()+
    geom_sf(aes(geometry = geom, fill = coefDamage*Tpattern)) +
    scale_fill_gradientn(colours = pal,limits=c(-3,3),na.value="transparent",name="Land cover change (%)",oob = scales::squish)+
    theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) +
    ggtitle("Estimated damages based on simulated PFTs")



    w <- st_transform(world_damageestimates_pfts, sp::CRS("+proj=robin +over"))
    ggplot(w) +
    theme_void()+
    geom_sf(aes(geometry = geom, fill = coefDamage)) +
    scale_fill_gradientn(colours = pal,limits=c(-2,2),na.value="transparent",name="Estimated change per Global degree C (pp)",oob = scales::squish)+
    theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) +
    ggtitle("Temperature effect on natural land cover area")

    ggsave("Figures/map_estimates_areachange_LocalC.png")

    library("writexl")
    write_xlsx(world_damageestimates_pfts,"world_damageestimates_pfts.xlsx")
# Plot map of estimates



            
            coords <- xyFromCell(pft_sum, seq_len(ncell(pft_sum)))
            pftchange <- stack(as.data.frame(getValues(pft_sum)))
            names(pftchange) <- c('percentage', 'period')
            pftchange <- cbind(coords, pftchange)
            levels(pftchange$period) <- c("near-term (2021-2040)", "mid-term (2041-2060)", "long-term (2081-2099)")

            ggplot(pftchange) + 
            geom_tile(aes(x, y, fill = percentage)) +
            facet_wrap(~ period) + 
            theme_void()+
            scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
                            high = "black",na.value="transparent",name="Land cover change (%)") +
            coord_equal()+
            ggtitle("PFTs disturbance")+
            guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                label.position = "bottom"))+
            theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))
            
            #ggsave("Figures/PFTs_disturbance_global.png",dpi=300)

            
            library("rnaturalearth")
            library("rnaturalearthdata")
            #world <- ne_countries(scale = "medium", returnclass = "sf", continent != "Antarctica")
            world_coast <- ne_coastline(scale = "small", returnclass = "sf")

            #box = c(xmin = -180, ymin = -60, xmax = 180, ymax = 90)
            #world_coast <- st_crop(world_coast, box)
            
            # pft_sum_poly1 <- 
            # raster::subset(pft_sum,1) %>% 
            # raster::rasterToPolygons() %>% 
            # sf::st_as_sf() 
            # pft_sum_poly1 <- rotate_sf(pft_sum_poly1)
            # world1 <- rotate_sf(world)

            # pft_sum_poly2 <- 
            # raster::subset(pft_sum,2) %>% 
            # raster::rasterToPolygons() %>% 
            # sf::st_as_sf() 
            # pft_sum_poly2 <- rotate_sf(pft_sum_poly2,y_add=165)
            # world2 <- rotate_sf(world,y_add=165)

            # pft_sum_poly3 <- 
            # raster::subset(pft_sum,3) %>% 
            # raster::rasterToPolygons() %>% 
            # sf::st_as_sf() 
            # pft_sum_poly3 <- rotate_sf(pft_sum_poly3,y_add=330)
            # world3 <- rotate_sf(world,y_add=330)
            # # https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/geospatial-data/
            
            # ggplot() +
            # theme_void() +
            # geom_sf(data = pft_sum_poly1,aes(fill = layer.1), color = NA) +
            # geom_sf(data = world1, fill=NA,alpha = 0.5)+
            # #scale_fill_distiller(palette = "Reds",direction = 1,guide = FALSE)+
            
            # geom_sf(data = pft_sum_poly2,aes(fill = layer.2), color = NA) +
            # geom_sf(data = world2, fill=NA,alpha = 0.5)+
            # #scale_fill_distiller(palette = "Reds",direction = 1,guide = FALSE) +
            # geom_sf(data = pft_sum_poly3,aes(fill = layer.3), color = NA) +
            # geom_sf(data = world3, fill=NA,alpha = 0.5)+
            # scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
            #                 high = "black",na.value="transparent",name="Land cover change (%)") +
            # #scale_fill_distiller(palette = "Reds",direction = 1, title= "PFTs") +
            # annotate(geom="text", x=350, y=-120, label="Near-term (2021-2040)")+
            # annotate(geom="text", x=350, y=60, label="Mid-term (2041-2060)")+
            # annotate(geom="text", x=350, y=220, label="Long-term (2081-2099)") +
            # xlim(c(-260,440)) + 
            # ggtitle("PFTs disturbance under RCP6.0") +
            # theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 
            # ggsave("Figures/global_pft_change.png",dpi=300)

            

            #Other projections
            #https://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot
            library(rgdal)
            crs(pft_sum) <- "+init=EPSG:4326"
            pft_sum <- projectRaster(pft_sum, crs='+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
            
            
            
            pft_sum_poly_rob1 <- 
            raster::subset(pft_sum,1) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 

            pft_sum_poly_rob2 <- 
            raster::subset(pft_sum,2) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 

            pft_sum_poly_rob3 <- 
            raster::subset(pft_sum,3) %>% 
            raster::rasterToPolygons() %>% 
            sf::st_as_sf() 
            #pft_sum_poly1 <- rotate_sf(pft_sum_poly1)
            
            
            world_coast <- ne_coastline(scale = "medium", returnclass = "sf")
            #world_coast <- st_transform(world_coast, crs = st_crs(pft_sum))

            #st_crs(world_coast) <- "+init=EPSG:4326"
            #pft_sum <- projectRaster(pft_sum, crs='+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
            

            a <- ggplot() +
            theme_void() +
             geom_sf(data = pft_sum_poly_rob1,aes(fill = layer.1), color = NA)+
            scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
                            high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            geom_sf(data = world_coast)+
            ggtitle("Near-term (2021-2040)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            b <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob2,aes(fill = layer.2), color = NA) +
            scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
                            high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            geom_sf(data = world_coast)+
            ggtitle("Mid-term (2041-2060)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            c <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob3,aes(fill = layer.3), color = NA) +
            geom_sf(data = world_coast)+scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
                            high = "black",na.value="transparent",name="Land cover change (%)")+
            geom_sf(data = world_coast)+
            ggtitle("Long-term (2081-2099)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            leg <- get_legend(c)

            c <- ggplot() +
            theme_void() +
            geom_sf(data = pft_sum_poly_rob3,aes(fill = layer.3), color = NA) +
            scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
                            high = "black",na.value="transparent",name="Land cover change (%)",guide = FALSE)+
            geom_sf(data = world_coast)+
            ggtitle("Long-term (2081-2099)")+
            theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 

            library('ggpubr')
            ggarrange(a,b,c,ncol=1,nrow=3 ,legend.grob = leg,common.legend = TRUE, legend="bottom")
            #ggsave("Figures/global_pftchange_temp.png",dpi=300)


            #geom_sf(data = world1, fill=NA,alpha = 0.5)+
            #scale_fill_distiller(palette = "Reds",direction = 1,guide = FALSE)+
            
            #geom_sf(data = pft_sum_poly_rob2,aes(fill = layer.2), color = NA) +
            #geom_sf(data = world2, fill=NA,alpha = 0.5)+
            #scale_fill_distiller(palette = "Reds",direction = 1,guide = FALSE) +
            #geom_sf(data = pft_sum_poly_rob3,aes(fill = layer.3), color = NA) +
            #geom_sf(data = world3, fill=NA,alpha = 0.5)+
            #scale_fill_gradient2(midpoint=50, low ="#ffffbf" , mid = "red",
             #               high = "black",na.value="transparent",name="Land cover change (%)") +
            #scale_fill_distiller(palette = "Reds",direction = 1, title= "PFTs") +
            #annotate(geom="text", x=350, y=-120, label="Near-term (2021-2040)")+
            #annotate(geom="text", x=350, y=60, label="Mid-term (2041-2060)")+
            #annotate(geom="text", x=350, y=220, label="Long-term (2081-2099)") +
            #xlim(c(-260,440)) + 
            #ggtitle("PFTs disturbance under RCP6.0") +
            #theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5)) 
            
            
            
            world <- spData::world
            world$coefDamage <- NA
            world$meanDamage <- NA
            #a_iso <- area(pft_sum)
            for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)


                #plot(crop(pft_sum,geom_iso))
                


                veg_iso <- raster::extract(pft_sum,geom_iso,metdod='simple', na.rm=TRUE)
                merged_veg_iso <- veg_iso[[1]]
                temp_iso <- raster::extract(t,geom_iso,metdod='simple', na.rm=TRUE)
                merged_temp_iso <- temp_iso[[1]]
                area_iso <- raster::extract(a_iso,geom_iso,metdod='simple', na.rm=TRUE)
                merged_area_iso <- area_iso[[1]]
                    
                        if (length(veg_iso)>1){
                            for(l in 2:length(veg_iso)){
                                merged_temp_iso <- rbind(merged_temp_iso,temp_iso[[l]])
                                merged_veg_iso <- rbind(merged_veg_iso,veg_iso[[l]])
                                merged_area_iso <- rbind(merged_area_iso,area_iso[[l]])
                            }
                        }
                veg_iso <- as.data.frame(merged_veg_iso)
                temp_iso <- as.data.frame(merged_temp_iso)
                area_iso <- as.data.frame(merged_area_iso)
                if (sum(complete.cases(veg_iso))<3){next}
                

                num_pix <- dim(veg_iso)[1]
                numyears <- length(veg_iso)
                colnames(veg_iso) <-  c("near-term (2021-2040)", "mid-term (2041-2060)", "long-term (2081-2099)")
                veg_iso <- stack(veg_iso)
                veg_iso$id <- rep(seq(1:num_pix),numyears)
                colnames(veg_iso) <- c("pftchange","period","id")

                
                colnames(temp_iso) <-   c("near-term (2021-2040)", "mid-term (2041-2060)", "long-term (2081-2099)")
                temp_iso <- stack(temp_iso)
                temp_iso$id <- rep(seq(1:num_pix),numyears)
                colnames(temp_iso) <- c("temp","period","id")

                

                cveg_temp <- cbind(temp_iso,veg_iso[,1])
                colnames(cveg_temp)[4] <- c("pfts_damage")
                cveg_temp$area <- c(area_iso[,1],area_iso[,1],area_iso[,1])
                #glimpse(cveg_temp)
                weightvar <- cveg_temp$area
                model <- felm(pfts_damage~temp|id|0|0,data=cveg_temp, weights = weightvar)
                #library('stargazer')
                #stargazer(model, type = "html", out="Figures/model_mex.html")




                


                

                # ggplot(data=cveg_temp, aes(x = temp, y = pfts_damage,group=id,color=period))+
                # theme_bw()+
                # geom_boxplot(aes(group=period),outlier.shape = NA)+
                # geom_point(alpha=0.2)+
                # coord_trans(y="log10") +
                # xlab("Gridcell temperature change (C)") +
                # ylab("Land cover change (%)") + 
                # ggtitle("PFTs change in Mexico under RCP6.0")
                #ggsave("Figures/mex_temp_landcover_boxplot.png",dpi=300)

                #aggregate(id~period, cveg_temp, function(i) weighted.mean(cveg_temp$temp[i], cveg_temp$area[i]))

                
                #sapply(split(cveg_temp, cveg_temp$period), function(d) weighted.mean(cveg_temp$temp, w = cveg_temp$area))
                
                # library('Hmisc')
                # library('dplyr')
                # wt <- plyr::ddply(cveg_temp, ~period, plyr::summarize, weighted.mean(temp, w=area))
                # wsdt <- plyr::ddply(cveg_temp, ~period, plyr::summarize, wtd.var(temp, w=area))
                # wp <- plyr::ddply(cveg_temp, ~period, plyr::summarize, weighted.mean(pfts_damage, w=area, na.rm = TRUE))
                # wsdp <- plyr::ddply(cveg_temp, ~period, plyr::summarize, wtd.var(pfts_damage, w=area))

                # w_mean <- cbind(wt,wsdt[,2]^0.5,wp[,2],wsdp[,2]^0.5)
                # colnames(w_mean) <- c("period","meanT","sdT","meanPFTs","sdPFTs")


                meanP <- plyr::summarize(cveg_temp, weighted.mean(pfts_damage, w=area, na.rm = TRUE))
                meanT <- plyr::summarize(cveg_temp, weighted.mean(temp, w=area, na.rm = TRUE))
                
                
                
                # ggplot()+
                # theme_bw()+
                # geom_point(data=cveg_temp, aes(x = temp, y = pfts_damage,group=id,color=period),alpha=0.05)+
                # #coord_trans(y="log10") +
                # xlab("Gridcell temperature change (C)") +
                # ylab("Land cover change (%)") + 
                # ggtitle("PFTs change in Mexico under RCP6.0")+
                # geom_point(data=w_mean, aes(x=meanT,y=meanPFTs, color = period))+
                # geom_errorbar(data=w_mean,aes(x = meanT, ymin=meanPFTs-sdPFTs, ymax=meanPFTs+sdPFTs), width=.2,
                #  position=position_dodge(.9))

                world$coefDamage[j] <- summary(model)$coef[1]
                world$meanDamage[j] <- meanP/meanT
                

            }

            
                
            library(ggspatial)
            

            plot(t(pft_sum))
            
            isimip_betas <- data.frame(iso_a2=factor(),coef_y=double())
            for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)
                
                for (p in 1:length(pfts)){
                    
                    v1 <- eval(parse(text =paste("dif_decadal_",pfts[p],"_brick_rcp6",sep="")))
                    iso_v <- crop(v1,geom_iso) # Cropping the gridcells for each country
                    if (p>1){
                        veg_iso_sum <- veg_iso_sum + iso_v} else {
                            veg_iso_sum <- iso_v
                        }

                
                    plot(iso_v)                    
                    
                    plot(crop(v1,geom_iso))
                    
                    

                    plot(v1)
                    plot(crop(v1,geom_iso))
                    dim(crop(v1,geom_iso))
                    c <- crop(v1,geom_iso) + crop(v1,geom_iso)
                    plot(c)
                    #plot(crop(v1_6,geom_iso))
                    v1 <- t(v1)
                    #plot(geom_iso)
                    #plot(v1, add=T)
                    veg_iso <- raster::extract(v1,geom_iso,metdod='simple', na.rm=TRUE)
                    merged_veg_iso <- veg_iso[[1]]
                    
                        if (length(veg_iso)>1){
                            for(l in 2:length(veg_iso)){
                                merged_temp_iso <- rbind(merged_temp_iso,temp_iso[[l]])
                                merged_veg_iso <- rbind(merged_veg_iso,veg_iso[[l]])
                            }
                        }
                    veg_iso <- abs(as.data.frame(merged_veg_iso))
                    
                    veg_iso[is.na(veg_iso)] = 0
                    if (p>1){
                        veg_iso_sum <- veg_iso_sum + veg_iso} else {
                            veg_iso_sum <- veg_iso
                        }
            }

                temp_iso <- raster::extract(t,geom_iso,metdod='simple', na.rm=TRUE)
                merged_temp_iso <- temp_iso[[1]]
                if (length(temp_iso)>1){
                            for(l in 2:length(veg_iso)){
                                merged_temp_iso <- rbind(merged_temp_iso,temp_iso[[l]])
                                
                            }
                        }
                
                temp_iso <- as.data.frame(merged_temp_iso)
                if (sum(complete.cases(veg_iso_sum))<3){next}
                

                #glimpse(veg_iso)
                num_pix <- dim(veg_iso_sum)[1]
                numyears <- length(veg_iso_sum)
                colnames(veg_iso_sum) <- c(1:numyears)*10 + 2020
                veg_iso_sum <- stack(veg_iso_sum)
                veg_iso_sum$id <- rep(seq(1:num_pix),numyears)
                colnames(veg_iso_sum) <- c("cveg","year","id")
                #veg_iso$cveg_pct <- veg_iso$cveg / veg_iso$cveg[1:num_pix]

                colnames(temp_iso) <-  c(1:numyears)
                temp_iso <- stack(temp_iso)
                temp_iso$id <- rep(seq(1:num_pix),numyears)
                colnames(temp_iso) <- c("temp","year","id")

                cveg_temp <- cbind(temp_iso,veg_iso_sum[,1]/2) #divided by two to avoid double counting replacement of PFTs in a gridcell
                colnames(cveg_temp)[4] <- c("pfts_damage")
                glimpse(cveg_temp)


                

                ggplot(data=cveg_temp, aes(x = temp, y = pfts_damage,group=id))+
                geom_line()

                #cveg_temp_c <- cveg_temp[is.finite(cveg_temp$cveg_pct),]
                #weightvar <- cveg_temp_c$cveg
                #m <- felm(cveg_pct~temp|id|0|0,data=cveg_temp_c, weights = weightvar)
                #coef_T <- summary(m)$coefficients[1]

                newdata <- data.frame(iso_a2=isos[j],coef_T=coef_T)

        isimip_betas <- rbind(isimip_betas,newdata)
        print(j)
    }
    
    world_betas <- merge(world,isimip_betas)
    
    my.palette <- brewer.pal(n = 10, name = "BrBG")
    library(wesanderson)
    pal <- wes_palette("Zissou1", 100, type = "continuous")
    ggplot(data = world_betas) + 
    theme_bw()+
    geom_sf(aes(fill = coef_T)) +
    ggtitle("Effect of 1C warming on biomass change  (pixel f.e.)")+ 
    scale_fill_continuous_diverging(palette = "Red-Green", 
    l1 = 10, l2 = 100, p1 = 0.1, p2 = 0.1,mid=0#,limits=c(-0.3,0.3)
    )
    ggsave("Figures/tempcoef_cveg_fepix.png",dpi=300)

    scale_fill_gradientn(colors = pal,limits=c(-0.05,0.05))
    
    
    
    
    







# Regression between two rasters (start)
    df_v <- as.data.frame(yearly_veg_brick)
    #for(i in 1:10){
    #  df_v[,10+i]<-df_v[,i] /df_v[,1] #get change w.r.t. year 1
    #}
    #df_v <- df_v[,-c(1:10)]
    colnames(df_v) <- c(2021:2090)
    ddf_v <- stack(df_v)
    ddf_v$id <- rep(seq(1:259200),length(df_v))
    colnames(ddf_v) <- c("cveg","year","id")

    df_t <- as.data.frame(yearly_tas_brick)
    colnames(df_t) <- c(2021:2090)
    ddf_t <- stack(df_t)
    ddf_t$id <- rep(seq(1:259200),length(df_t))
    colnames(ddf_t) <- c("temp","year","id")

    cveg_temp <- cbind(ddf_t,ddf_v$cveg)
    colnames(cveg_temp)[4] <- "cveg"
    #
    glimpse(cveg_temp)
    cveg_temp[c(1:100),]
    cveg_temp$cveg_pct <- cveg_temp$cveg / cveg_temp$cveg[1:259200]
    cveg_temp$temp_inc <- cveg_temp$temp - cveg_temp$temp[1:259200]
    cveg_temp <- cveg_temp[order(cveg_temp$id,cveg_temp$year),]
    cveg_temp$cveg_growth <- (cveg_temp$cveg-lag(cveg_temp$cveg)) / lag(cveg_temp$cveg)
    cveg_temp$temp_inc <- cveg_temp$temp-lag(cveg_temp$temp) 
    cveg_temp <- cveg_temp[which(cveg_temp$year!=2021),]
    #Calculating a global effect of temp on carbon in vegetation 
        cveg_temp_c <- cveg_temp[is.finite(cveg_temp$cveg_growth),]
        
    cveg_temp_c[c(1:100),]
        # Global temp coefficient
        #library('lfe')
        weightvar <- cveg_temp_c$cveg
        m <- felm(cveg_growth~temp_inc|id+year|0|0,data=cveg_temp_c)
        #, weights = weightvar)
        summary(m)    
        # fatal_fe_mod <- plm(cveg ~ temp + I(temp^2), 
        #             data = cveg_temp_c,
        #             index = c("id", "year"), 
        #             model = "within")
        # plot_cme(m,effect="temp",condition="temp")

            #plot quad 
            model <- m
            dataset <- cveg_temp_c
                    Sigma <- vcov(model)
                    coefT <- "temp"
                    start1 <- which(names(coef(model))==coefT)
                    end1 <- which(names(coef(model))==paste("I(",coefT,"^2)",sep=""))
                    
                    sigma = Sigma[c(1:end1),c(1:end1)]
                    beta.hat <- coef(model)[c(1:end1)]
                    x <- seq(from=min(dataset$temp),to=max(dataset$temp), length=20)
                    xmat <- cbind(x, x^2)
                    gestimated <- colSums(beta.hat*t(xmat)) 
                    ci12 <- gestimated + 1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
                    ci22 <- gestimated -  1.96*sqrt(diag((xmat %*% sigma) %*% t(xmat)))

                    glimpse(dataset)
                    dataset <- data.frame(temp=x,cveg=gestimated,ci12,ci22)
                    
                    cols=c("#7375a4")
                    ggplot(data=dataset, aes(x=temp,y=cveg))+
                    theme_bw()+
                    geom_line(col=cols[1])+
                    geom_ribbon(aes(ymin=ci22,ymax=ci12,x=temp),alpha=0.2,fill=cols[1])+
                    xlab("Temperature")+
                    ylab("Carbon in vegeation (kg/m^2)") + #xlim(-1.2,1.2)+
                    ggtitle("Temperature effect in within-gridcell carbon vegetation")
                    ggsave("Figures/global_temp_cveg_fepix.png",dpi=300)

    #Calculating a global effect of temp on carbon in vegetation 

    # One estimate per pixel
        f <- factor(cveg_temp_c$id)
        length(f) #588900
        #for (j in 1:length(f)){
        for (j in 1:length(f)){
            pix <- cveg_temp_c[which(cveg_temp_c$id==f[j]),]      
            cveg_temp_c$coeff_T[which(cveg_temp_c$id==f[j])]   <- summary(lm(cveg_pct~temp,data=pix))$coefficients[2]
            print(j)
        }

        glimpse(cveg_temp_c)

        #merge back with complete dataset
        cveg_coeffT <- cveg_temp[1:259200,]
        glimpse(cveg_coeffT)
        coef <- cveg_temp_c[cveg_temp_c$year==2006,which(colnames(cveg_temp_c)%in%c("id","coeff_T"))]
        merged_coef <- merge(cveg_coeffT,coef,by="id",all.x=TRUE)
        
    # One estimate per pixel

    # Raster of coefficients (start)


        coord_r <- as.data.frame(yearly_veg_brick,xy=TRUE)[,1:2] #get coordinates
        spg <- cbind(coord_r,merged_coef$coeff_T)
        coordinates(spg) <- ~x+y
        gridded(spg) <- TRUE
        # coerce to raster
        rasterDF <- raster(spg)
        rasterDF
        plot(rasterDF)
    # Raster of coefficients (end)

    





                if(class(veg_iso)=="list"){veg_iso <- unlist(veg_iso)}
                
                if (length(mean_var_iso)>1){mean_var_iso <- weighted.mean(mean_var_iso,area(geom_iso), na.rm=TRUE)}
                if(class(mean_var_iso)=="list"){mean_var_iso <- unlist(mean_var_iso)}
                if(is.null(mean_var_iso)){mean_var_iso <- NA}
                



        colnames(df_v) <- c(2006:2015)
        ddf_v <- stack(df_v)
        ddf_v$id <- rep(seq(1:259200),10)
        colnames(ddf_v) <- c("cveg","year","id")

        df_t <- as.data.frame(yearly_tas_brick)
        colnames(df_t) <- c(2006:2015)
        ddf_t <- stack(df_t)
        ddf_t$id <- rep(seq(1:259200),10)
        colnames(ddf_t) <- c("temp","year","id")

        cveg_temp <- cbind(ddf_t,ddf_v$cveg)
        colnames(cveg_temp)[4] <- "cveg"
        #
        glimpse(cveg_temp)
        cveg_temp$cveg_pct <- cveg_temp$cveg / cveg_temp$cveg[1:259200]
        
        #Calculating a global effect of temp on carbon in vegetation 
            cveg_temp_c <- cveg_temp[is.finite(cveg_temp$cveg_pct),]
            # Global temp coefficient
            #library('lfe')
            m <- felm(cveg_pct~temp|id+year|0|0,data=cveg_temp_c)
            summary(m)    
        #Calculating a global effect of temp on carbon in vegetation 
        numdecades <- 10
        for (ii in(1:numdecades)){
            r <- subset(decadal_brick,ii)
                r <- t(r)
                #r <- flip(r,'y')
                #r <-rotate(r)

                #plot(r)
            for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)
                
                #plot(geom_iso, add = T)               
                
                mean_var_iso <- raster::extract(r,geom_iso,metdod='simple',fun=mean, na.rm=TRUE)
                if (length(mean_var_iso)>1){mean_var_iso <- weighted.mean(mean_var_iso,area(geom_iso), na.rm=TRUE)}
                if(class(mean_var_iso)=="list"){mean_var_iso <- unlist(mean_var_iso)}
                if(is.null(mean_var_iso)){mean_var_iso <- NA}
                
                var_iso_decade <- rbind(var_iso_decade,c(isos[j],vars[i],mean_var_iso,2015+(ii*10)))
                
            #print(paste("country",j,isos[j]))

            }
            print(paste("decade",ii))

        }
    # One regression per country (end)


# Get country estimates

    yvb <- yearly_veg_brick <- brick(var_array[,,(1:10)], xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
    dyveg <- as.data.frame(yvb)
    colnames(dyveg) <- c(2006:2015)
    dyveg$id <- seq(1:259200)
    dd <- stack(dyveg)
    glimpse(dd)
    s <- stack(yearly_tas_brick, yvb)
    v <- data.frame(na.omit(values(s)))
    names(v) <- c('T', 'V')
    m <- lm(V ~ T, data=v)
    summary(m)
# Regression between two rasters (end)
   
    #Extract country-level data
        data(world)
        isos <- levels(factor(world$iso_a2))


        numdecades <- 94
        for (ii in(1:numdecades)){
            r <- subset(decadal_brick,ii)
                r <- t(r)
                #r <- flip(r,'y')
                #r <-rotate(r)

                #plot(r)
            for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)
                
                #plot(geom_iso, add = T)               
                
                mean_var_iso <- raster::extract(r,geom_iso,metdod='simple',fun=mean, na.rm=TRUE)
                if (length(mean_var_iso)>1){mean_var_iso <- weighted.mean(mean_var_iso,area(geom_iso), na.rm=TRUE)}
                if(class(mean_var_iso)=="list"){mean_var_iso <- unlist(mean_var_iso)}
                if(is.null(mean_var_iso)){mean_var_iso <- NA}
                
                var_iso_decade <- rbind(var_iso_decade,c(isos[j],vars[i],mean_var_iso,2015+(ii*10)))
                
            #print(paste("country",j,isos[j]))

            }
            print(paste("decade",ii))

        }
        
            
glimpse(var_iso_decade)
names(var_iso_decade) <- c("iso2","var","pct","year")
var_iso_decade$pct <- as.numeric(var_iso_decade$pct)

var_iso_decade$year <- as.numeric(var_iso_decade$year)
#var_iso_decade <- var_iso_decade[which(!is.na(var_iso_decade$value)),]
#var_iso_decade <- var_iso_decade[which(!is.nan(var_iso_decade$value)),]
#var_iso_decade <- var_iso_decade[which(var_iso_decade$value!=0),]
glimpse(var_iso_decade)
save(var_iso_decade,file="cveg_2005co2_iso_year_jpl.Rdata")
#load("lai_total_iso_year_jpl.Rdata")
ggplot(data=var_iso_decade, aes(x = year, y = pct, group = iso2,color = iso2) )+
geom_line()
#library('lfe')
summary(felm(pct~year|iso2|0|0,data = var_iso_decade))


data(world)
    isos <- levels(factor(world$iso_a2))
    isimip_betas <- data.frame(iso_a2=factor(),
                coef_y=double())
    for (j in 1:length(isos)){
        dat_iso <- var_iso_decade[which(var_iso_decade$iso2==isos[j]),]
        if(sum(is.nan(dat_iso$pct))==dim(dat_iso)[1]){next}
        dat_iso$pct <- 100*dat_iso$pct / dat_iso$pct[1]
        coef_y <- felm(pct~year|0|0|0,data = dat_iso)$coefficients[2]
        
        newdata <- data.frame(iso_a2=isos[j],
                coef_y=coef_y)

        isimip_betas <- rbind(isimip_betas,newdata)
    }
    world_betas <- merge(world,isimip_betas)
    glimpse(world)
    glimpse(world_betas)
    
    ggplot(data = world_betas) + 
    geom_sf(aes(fill = coef_T)) +
    scale_fill_gradientn(colors = my.palette,limits=c(-0.1,0.1))




#for (j in 1:length(isos)){
#    cc <- countrycode(isos[j], origin = 'iso2c', destination = 'country.name')
#pftplot <- ggplot(data = var_iso_decade[which(var_iso_decade$iso2==isos[j]),],
#            aes(y =pct, x = year)) +
#             geom_area(alpha=0.6 , size=1, colour="black")+
#             ggtitle(cc)
#    ggsave(pftplot, file=paste("pft/pft_",isos[j],".png",sep=""), width = 14, height = 10, units = "cm")
#}

#forest <- data.frame(pft = pfts , forest = c(rep("forest",8),rep("other",7)))

#var_iso_decade <- merge(var_iso_decade,forest,by="pft")


#  forest_iso <- aggregate(x = var_iso_decade[c("pct")], 
#                                by =  var_iso_decade[c("iso2", "decade","forest")], 
#                                 function(y){
#                                sum(y)
#                              })
#                              glimpse(forest_iso)

# for (j in 1:length(isos)){
#     cc <- countrycode(isos[j], origin = 'iso2c', destination = 'country.name')
# pftplot <- ggplot(data = forest_iso[which(forest_iso$iso2==isos[j]),],
#             aes(y =pct, x = decade, group = forest, fill = forest)) +
#              geom_area(alpha=0.6 , size=1, colour="black")+
#              ggtitle(cc)
#     ggsave(pftplot, file=paste("pft/forest/forest_",isos[j],".png",sep=""), width = 14, height = 10, units = "cm")
# }


########### Per PIXEL


glimpse(decadal)
dim(decadal)[1]
delta_pfti<-decadal[,,,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        for (pfti in 1:dim(decadal)[3]){
            delta_pfti[lon,lat,pfti] <- decadal[lon,lat,pfti,15] - decadal[lon,lat,pfti,1]
        }
    }
    print(paste("lon",lon,"/288"))
}

glimpse(delta_pfti)
delta_pfti <- brick(delta_pfti)
delta_pfti <- flip(t(delta_pfti),'y')
names(delta_pfti) <-  c("NET Temperate", "NET Boreal", "NDT Boreal", "BET Tropical", "BET Temperate",
    "BDT Tropical", "BDT Temperate", "BDT Boreal", "BES Temperate", "BDS Temperate", "BDS Boreal", "c3 arctic gras", "c3 grass", "c4 grass","crop")
plot(delta_pfti)    





glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:8){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        forest_loss <- delta_forest[lon,lat,1:8]
        #forest_loss <- forest_loss[which(forest_loss<0)]
        losses <- which(forest_loss<0)
        forest_loss <-  forest_loss[losses] 
        delta_forest_abs[lon,lat] <- 100* sum(abs(forest_loss)) / sum(abs(decadal[lon,lat,losses,1]))
    }
    print(paste("lon",lon,"/288"))
}

glimpse(delta_forest)
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
plot(delta_forest_abs, main = "PFT percent change") 
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("Percentage of Forest Loss (2015-2100)")
#####



### Change in forest area
glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:8){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        #forest_loss <- delta_forest[lon,lat,1:8]
        #forest_loss <- forest_loss[which(forest_loss<0)]
        #losses <- which(forest_loss<0)
        #forest_loss <-  forest_loss[losses] 
        change <- sum( delta_forest[lon,lat,1:8])
        delta_forest_abs[lon,lat] <- change
    }
    print(paste("lon",lon,"/288"))
}

hist(delta_forest_abs)

glimpse(delta_forest_abs)
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
plot(delta_forest_abs, main = "Change in Forest Coverage (% of pixel)") 
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  #scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  #scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent") +
  scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent",limits=c(-80,80)) +
  coord_equal() + theme_bw() +
  ggtitle("Change in Forest Coverage 2100-2020 (% of pixel)")
#####

### Change in forest area
glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:8){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        #forest_loss <- delta_forest[lon,lat,1:8]
        #forest_loss <- forest_loss[which(forest_loss<0)]
        #losses <- which(forest_loss<0)
        #forest_loss <-  forest_loss[losses] 
        change <- min(100* sum( delta_forest[lon,lat,1:8]) / sum(decadal[lon,lat,1:8,1]),100)
        delta_forest_abs[lon,lat] <- change
    }
    print(paste("lon",lon,"/288"))
}

hist(delta_forest_abs)

glimpse(delta_forest_abs)
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
plot(delta_forest_abs, main = "Change in Forest Coverage (% of initial coverage)") 
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  #scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("Change in Forest Coverage 2100-2020 (% of initial coverage)")
#####

### RMSE
glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:15){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        rmse<- (sum((delta_forest[lon,lat,1:8])^2))/15
        #forest_loss <- forest_loss[which(forest_loss<0)]
        #losses <- which(forest_loss<0)
        #forest_loss <-  forest_loss[losses] 
        #change <- min(100* sum( delta_forest[lon,lat,1:8]) / sum(decadal[lon,lat,1:8,1]),100)
        delta_forest_abs[lon,lat] <- rmse
    }
    print(paste("lon",lon,"/288"))
}
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  #scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("RMSE of change in PFTs coverage per gridcell (2020 - 2100)")
#####

#### Changes forest per pixel
glimpse(decadal)
dim(decadal)[1]
delta_pfti<-decadal[,,,1]
delta_pfti_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:dim(decadal)[3]){
            delta_pfti[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1]
        }
        delta_pfti_abs[lon,lat] <- sum(abs(delta_pfti[lon,lat,1:dim(decadal)[3]]))/2
    }
    print(paste("lon",lon,"/288"))
}

glimpse(delta_pfti)
delta_pfti_abs <- raster(delta_pfti_abs)
delta_pfti_abs <- flip(t(delta_pfti_abs),'y')
plot(delta_pfti_abs, main = "PFT percent change") 
gplot(delta_pfti_abs) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = cbp1, values = c(1.0, 0.05,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("Percentage of disruption (2015-2100)")

#######





####### Read Land Unit


x <- c('raster', 'dplyr','ncdf4','ggplot2','tidyverse','RColorBrewer','colorspace','spData','sf')
    lapply(x, require, character.only = TRUE)
setwd('C:\\Users\\bastien\\Documents\\GitHub\\vegetation')
dir1 <- "C:/Users/bastien/Box/VegetationData/"
#clm50_r270_1deg_GSWP3V1_iso_SSP3-7_Smooth.clm2.h0.NPP.201501-228212
simname1 <- "clm50_r270_1deg_GSWP3V1_iso_SSP3-7_Smooth.clm2.h0."
simname2 <- ".201501-228212.nc"
vars <- c("PCT_LANDUNIT","TSA","RAIN")
var_iso_decade <- data.frame(iso2=factor(),
                pft = character(),
                var = character(),
                 value=double(),
                 decade=integer())
    i <- 1
    dname <- vars[i]
    ncname <- paste(dir1,simname1,dname,simname2,sep="")
    ncin <- nc_open(ncname)
    print(ncin)    
    #get units
    lon <- ncvar_get(ncin,"lon")
    lat <- ncvar_get(ncin,"lat")
    time <- ncvar_get(ncin,"time")
    tunits <- ncatt_get(ncin,"time","units")

    # days since 2015 01 01
    #365 * 10
    #get variable
    #var_array <- ncvar_get(ncin,dname, start = c(1,1,1,1), count=c(-1,-1,-1,365)) #start, number in dimension to start. count: hopw many in that dimension
    #remove(var_array)
    #last one is time
    dim(var_array)
    numdecades <- 15
    decadal <- array(numeric(),c(288,192,9,numdecades)) 
    for (d in (1:numdecades)){ 
        decade_array <- ncvar_get(ncin,dname, start = c(1,1,1,(120*(d-1)+1)), count=c(-1,-1,-1,(120)))
        decadal_mean <- rowMeans(decade_array, dims = 3, na.rm = TRUE)
        decadal[,,,d] <- decadal_mean
        print(d)
    }
    glimpse(decadal)
    save(decadal,'decadal_pct_landtype.Rdata')
    #load('decadal_pct_pft.Rdata')
    ltypes <- c("Vegetated", "Crop", "Unused", "Ice", "Lake",
    "Wetland", "Urban TBD", "Urban HD", "Urban MD")

 var_iso_decade <- data.frame(iso2=factor(),
                ltype= character(),
                var = character(),
                 value=double(),
                 decade=integer())
    numdecades <- 10
for (jj in 1:9){
    dec <- decadal[,,jj,]
    decadal_brick <- brick(dec, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
    #decadal_brick <- brick(dec, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    #decadal_brick <- flip(t((subset(decadal_brick,1))),direction='y')


    #Extract country-level data
        data(world)
        isos <- levels(factor(world$iso_a2))
        
        
        for (ii in(1:numdecades)){
            r <- subset(decadal_brick,ii)
                r <- t(r)
                r <- flip(r,'y')
                r <-raster::rotate(r)

                plot(r)
             for (j in 1:length(isos)){
                geom_iso <- world$geom[world$iso_a2==isos[j]]
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)
                
                #plot(geom_iso, add = T)               
                
                mean_var_iso <- raster::extract(r,geom_iso,metdod='simple',fun=mean, na.rm=TRUE)
                if (length(mean_var_iso)>1){mean_var_iso <- weighted.mean(mean_var_iso,area(geom_iso), na.rm=TRUE)}
                if(class(mean_var_iso)=="list"){mean_var_iso <- unlist(mean_var_iso)}
                if(is.null(mean_var_iso)){mean_var_iso <- NA}
                
                var_iso_decade <- rbind(var_iso_decade,c(isos[j],vars[i],ltypes[jj],mean_var_iso,2015+(ii*10)))
                
            #print(paste("country",j,isos[j]))

            }
            print(paste("decade",ii))

        }
        
            print(paste("pft",jj))
    }
glimpse(var_iso_decade)
names(var_iso_decade) <- c("iso2","var","ltype","pct","decade")
var_iso_decade$pct <- as.numeric(var_iso_decade$pct)
var_iso_decade$decade <- as.numeric(var_iso_decade$decade)
#var_iso_decade <- var_iso_decade[which(!is.na(var_iso_decade$value)),]
#var_iso_decade <- var_iso_decade[which(!is.nan(var_iso_decade$value)),]
#var_iso_decade <- var_iso_decade[which(var_iso_decade$value!=0),]
glimpse(var_iso_decade)
save(var_iso_decade,file="ltype_iso_decade.Rdata")
#var_iso_decade = load("pft_iso_decade.Rdata")
#ggplot(data=var_iso_decade, aes(x = decade, y = value, group = iso2,color = iso2) )+
#geom_line()

for (j in 1:length(isos)){
    cc <- countrycode(isos[j], origin = 'iso2c', destination = 'country.name')
pftplot <- ggplot(data = var_iso_decade[which(var_iso_decade$iso2==isos[j]),],
            aes(y =pct, x = decade, group = ltype, fill = ltype)) +
             geom_area(alpha=0.6 , size=1, colour="black")+
             ggtitle(cc)
    ggsave(pftplot, file=paste("ltype/ltype_",isos[j],".png",sep=""), width = 14, height = 10, units = "cm")
}

forest <- data.frame(pft = pfts , forest = c(rep("forest",8),rep("other",7)))

var_iso_decade <- merge(var_iso_decade,forest,by="pft")


 forest_iso <- aggregate(x = var_iso_decade[c("pct")], 
                               by =  var_iso_decade[c("iso2", "decade","forest")], 
                                function(y){
                               sum(y)
                             })
                             glimpse(forest_iso)

for (j in 1:length(isos)){
    cc <- countrycode(isos[j], origin = 'iso2c', destination = 'country.name')
pftplot <- ggplot(data = forest_iso[which(forest_iso$iso2==isos[j]),],
            aes(y =pct, x = decade, group = forest, fill = forest)) +
             geom_area(alpha=0.6 , size=1, colour="black")+
             ggtitle(cc)
    ggsave(pftplot, file=paste("pft/forest/forest_",isos[j],".png",sep=""), width = 14, height = 10, units = "cm")
}


########### Per PIXEL


glimpse(decadal)
dim(decadal)[1]
delta_pfti<-decadal[,,,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        for (pfti in 1:dim(decadal)[3]){
            delta_pfti[lon,lat,pfti] <- decadal[lon,lat,pfti,15] - decadal[lon,lat,pfti,1]
        }
    }
    print(paste("lon",lon,"/288"))
}

glimpse(delta_pfti)
delta_pfti <- brick(delta_pfti)
delta_pfti <- flip(t(delta_pfti),'y')
names(delta_pfti) <-  c("NET Temperate", "NET Boreal", "NDT Boreal", "BET Tropical", "BET Temperate",
    "BDT Tropical", "BDT Temperate", "BDT Boreal", "BES Temperate", "BDS Temperate", "BDS Boreal", "c3 arctic gras", "c3 grass", "c4 grass","crop")
plot(delta_pfti)    





glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:8){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        forest_loss <- delta_forest[lon,lat,1:8]
        #forest_loss <- forest_loss[which(forest_loss<0)]
        losses <- which(forest_loss<0)
        forest_loss <-  forest_loss[losses] 
        delta_forest_abs[lon,lat] <- 100* sum(abs(forest_loss)) / sum(abs(decadal[lon,lat,losses,1]))
    }
    print(paste("lon",lon,"/288"))
}

glimpse(delta_forest)
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
plot(delta_forest_abs, main = "PFT percent change") 
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("Percentage of Forest Loss (2015-2100)")
#####



### Change in forest area
glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:8){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        #forest_loss <- delta_forest[lon,lat,1:8]
        #forest_loss <- forest_loss[which(forest_loss<0)]
        #losses <- which(forest_loss<0)
        #forest_loss <-  forest_loss[losses] 
        change <- sum( delta_forest[lon,lat,1:8])
        delta_forest_abs[lon,lat] <- change
    }
    print(paste("lon",lon,"/288"))
}

hist(delta_forest_abs)

glimpse(delta_forest_abs)
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
plot(delta_forest_abs, main = "Change in Forest Coverage (% of pixel)") 
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  #scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  #scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent") +
  scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent",limits=c(-80,80)) +
  coord_equal() + theme_bw() +
  ggtitle("Change in Forest Coverage 2100-2020 (% of pixel)")
#####

### Change in forest area
glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:8){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        #forest_loss <- delta_forest[lon,lat,1:8]
        #forest_loss <- forest_loss[which(forest_loss<0)]
        #losses <- which(forest_loss<0)
        #forest_loss <-  forest_loss[losses] 
        change <- min(100* sum( delta_forest[lon,lat,1:8]) / sum(decadal[lon,lat,1:8,1]),100)
        delta_forest_abs[lon,lat] <- change
    }
    print(paste("lon",lon,"/288"))
}

hist(delta_forest_abs)

glimpse(delta_forest_abs)
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
plot(delta_forest_abs, main = "Change in Forest Coverage (% of initial coverage)") 
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  #scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("Change in Forest Coverage 2100-2020 (% of initial coverage)")
#####

### RMSE
glimpse(decadal)
dim(decadal)[1]
delta_forest<-decadal[,,,1]
delta_forest_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:15){ #the first 8 are forests
            delta_forest[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1] #1 is 2015, 9 is 2100
        }
        rmse<- (sum((delta_forest[lon,lat,1:8])^2))/15
        #forest_loss <- forest_loss[which(forest_loss<0)]
        #losses <- which(forest_loss<0)
        #forest_loss <-  forest_loss[losses] 
        #change <- min(100* sum( delta_forest[lon,lat,1:8]) / sum(decadal[lon,lat,1:8,1]),100)
        delta_forest_abs[lon,lat] <- rmse
    }
    print(paste("lon",lon,"/288"))
}
delta_forest_abs <- raster(delta_forest_abs)
delta_forest_abs <- flip(t(delta_forest_abs),'y')
gplot(delta_forest_abs) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = c(cbp1,'gray'), values = c(1.0, 0.05,0.01,0),na.value="transparent") +
  #scale_fill_gradientn(colours = c('darkgreen','gray','red'), values = c(1.0, 0.55,0.45,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("RMSE of change in PFTs coverage per gridcell (2020 - 2100)")
#####

#### Changes forest per pixel
glimpse(decadal)
dim(decadal)[1]
delta_pfti<-decadal[,,,1]
delta_pfti_abs<-decadal[,,1,1]
#lon, lat, pfts, decade
for (lon in 1:dim(decadal)[1]){
    for (lat in 1:dim(decadal)[2]){
        if(is.na(decadal[lon,lat,3,1])){next}
        for (pfti in 1:dim(decadal)[3]){
            delta_pfti[lon,lat,pfti] <- decadal[lon,lat,pfti,9] - decadal[lon,lat,pfti,1]
        }
        delta_pfti_abs[lon,lat] <- sum(abs(delta_pfti[lon,lat,1:dim(decadal)[3]]))/2
    }
    print(paste("lon",lon,"/288"))
}

glimpse(delta_pfti)
delta_pfti_abs <- raster(delta_pfti_abs)
delta_pfti_abs <- flip(t(delta_pfti_abs),'y')
plot(delta_pfti_abs, main = "PFT percent change") 
gplot(delta_pfti_abs) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = cbp1, values = c(1.0, 0.05,0),na.value="transparent") +
  coord_equal() + theme_bw() +
  ggtitle("Percentage of disruption (2015-2100)")

#######

#### Read Land Unit

aggregate(delta_pfti,)

  cbp1 <- c( "#0072B2","#009E73","#E69F00")
gplot(delta_pfti) + 
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradientn(colours = c('green','red'), values = c(1.0, 0.51, 0.49, 0),na.value="transparent") +
  coord_equal() + theme_bw()

ggplot() +
  geom_raster(data = delta_pfti , aes(x = lon, y = lat, fill = BES.Temperate)) + 
  coord_quickmap()

ggplot(data = delta_pfti) +
    geom_raster() +
    geom_sf(data = dam_world, aes(fill = sqthreedegdam)) +
    scale_fill_gradient2(low = "red",
        mid = "white",
        high = "blue",
        midpoint = 0,
        space = "Lab",
        na.value = "grey50",
        guide = "colourbar",
        aesthetics = "fill")
 ggplot(data = delta_pfti[which(delta_pfti$iso2==isos[j]),],
            aes(y =pct, x = decade, group = forest, fill = forest)) +
             geom_area(alpha=0.6 , size=1, colour="black")+
             ggtitle(cc)

###END
















            #geom_line()
            geom_line(aes(color=value),position = position_stack(reverse = TRUE))+theme_bw()
            
            (aes(fill = pft), width = 1/12)

ggplot(var_iso_decade[which(var_iso_decade$iso2==isos[j]),],
            aes(y = value, axis1 = pft, axis2 = decade)) +
            geom_alluvium(aes(fill = pft), width = 1/12) +
            geom_stratum(width = 1/12, fill = "black", color = "grey") +
            geom_label(stat = "stratum", infer.label = TRUE) +
            scale_x_discrete(limits = c("Category","General"), expand = c(.05, .05)) +
            #scale_fill_brewer(type = "qual", palette = "Set2") +
            ggtitle("Data acquisition")
            #ggsave("Data_acquisition_flow2.png", path="C:/Users/bastien/Documents/Meta analysis Ecosystem Services/figures", dpi=600)



# Preindustrial levels (start)
    #clm50_cesm201R_1deg_GSWP3V1_phsoff_hist.clm2.h0.RAIN.185001-201412
    
    simname1 <- "clm50_cesm201R_1deg_GSWP3V1_phsoff_hist.clm2.h0."
    simname2 <- ".185001-201412.nc"
    vars <- c("NPP","TSA","RAIN")
    var_iso_preind <- data.frame(iso2=factor(),
                    var = character(),
                    value=double(),
                    decade=integer())
    for (i in 1:3){
        dname <- vars[i]
        ncname <- paste(dir1,simname1,dname,simname2,sep="")
        ncin <- nc_open(ncname)

        #get units
        lon <- ncvar_get(ncin,"lon")
        lat <- ncvar_get(ncin,"lat")
        time <- ncvar_get(ncin,"time")
        tunits <- ncatt_get(ncin,"time","units")

        #get variable
        var_array <- ncvar_get(ncin,dname)
        dlname <- ncatt_get(ncin,dname,"long_name")
        dunits <- ncatt_get(ncin,dname,"units")
        fillvalue <- ncatt_get(ncin,dname,"_FillValue")
        nc_close(ncin) 
        var_array[var_array == fillvalue$value] <- NA

        #get array of 1850-1860
        
            decade_array <- var_array[,,1:(10*12)] #1850 to 1860
            decadal_mean <- rowMeans(decade_array, dims = 2, na.rm = TRUE)
            decadal <- decadal_mean
        
        decadal_brick <- raster(decadal, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
        #decadal_brick <- brick(decadal, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
        #decadal_brick <- flip(t((subset(decadal_brick,1))),direction='y')


        #Extract country-level data
            data(world)
            isos <- levels(factor(world$iso_a2))
        
                    r <- t(decadal_brick)
                    r <- flip(r,'y')
                    r <-rotate(r)

                    #plot(r)
                for (j in 1:length(isos)){
                    geom_iso <- world$geom[world$iso_a2==isos[j]]
                    geom_iso <- st_cast(geom_iso, "POLYGON")
                    geom_iso <-as_Spatial(geom_iso)
                    
                    #plot(geom_iso, add = T)               
                    
                    mean_var_iso <- raster::extract(r,geom_iso,metdod='simple',fun=mean, na.rm=TRUE)
                    if (length(mean_var_iso)>1){mean_var_iso <- weighted.mean(mean_var_iso,area(geom_iso), na.rm=TRUE)}
                    if(class(mean_var_iso)=="list"){mean_var_iso <- unlist(mean_var_iso)}
                    if(is.null(mean_var_iso)){mean_var_iso <- NA}
                    
                    #if(vars[i]=="NPP"){var_iso_preind <- rbind(var_iso_preind,c(isos[j],"C13_NPP",mean_var_iso,1850))}else{
                        var_iso_preind <- rbind(var_iso_preind,c(isos[j],vars[i],mean_var_iso,1850))
                    #}
                }
                

            
    }

# Preindustrial levels (end)
glimpse(var_iso_preind)
names(var_iso_preind) <- c("iso2","var","value","decade")


#get first decade ref value

var_iso_preind$isovar <- paste(var_iso_preind$iso2,var_iso_preind$var,sep="")
glimpse(var_iso_preind)
var_iso_preind <- var_iso_preind[,which(names(var_iso_preind)%in%c("value","isovar"))]
names(var_iso_preind) <- c("refvalue","isovar")

#merge with dataset
var_iso_decade$isovar <- paste(var_iso_decade$iso2,var_iso_decade$var,sep="")
glimpse(var_iso_decade)
dam_df <- merge(var_iso_decade,var_iso_preind, by="isovar")
glimpse(dam_df)
dam_df$refvalue <- as.double(dam_df$refvalue)
dam_df$value_change <- dam_df$value - dam_df$refvalue
dam_df$value_change_pct <- log(dam_df$value / dam_df$refvalue)*100


ggplot(data = dam_df[which(dam_df$var=="NPP"),], aes(x=value,y=value_change_pct,color=iso2))+
geom_line()+
theme_bw()

dam_df$isodecade <- paste(dam_df$iso2,dam_df$decade,sep="")
npp_df <- dam_df[which(dam_df$var=="NPP"),]
names(npp_df)[4] <- "npp"
names(npp_df)[7] <- "npp_change"
names(npp_df)[8] <- "npp_change_pct"
glimpse(npp_df)
tsa_df <- dam_df[which(dam_df$var=="TSA"),]
names(tsa_df)[4] <- "tsa"
names(tsa_df)[7] <- "tsa_change"
names(tsa_df)[8] <- "tsa_change_pct"
rain_df <- dam_df[which(dam_df$var=="RAIN"),]
names(rain_df)[4] <- "rain"
names(rain_df)[7] <- "rain_change"
names(rain_df)[8] <- "rain_change_pct"
dam <- merge(npp_df,tsa_df,by="isodecade")
dam <- merge(dam,rain_df,by="isodecade")
glimpse(dam)


ggplot(data=dam, aes(x=tsa_change,y=npp_change_pct,colour=iso2))+geom_line()

mod_all <- felm(npp_change_pct~tsa_change_pct+I(tsa_change_pct^2)+rain_change|iso2+decade|0|0,data=dam[which(is.finite(dam$npp_change_pct)),])
summary(mod_all)
save(dam,file='npp_clim_iso.Rdata')
dam_coef <- data.frame(iso2=factor(),
                sqtemp_coef = double(),
                 sqtemp_sd=double(),
                sqtemp2_coef = double(),
                 sqtemp2_sd=double(),
                temp_coef = double(),
                 temp_sd=double(),
                 iso2=character())
for(i in 1:length(isos)){
    d <- dam[which(dam$iso2==isos[i]),]
    if(sum(is.na(d$npp_change_pct))==length(d$npp_change_pct)){
        print(isos[i])
        print(i)
        next}
    d <- d[which(is.finite(d$npp_change_pct)),]

    mod1 <- lm(npp_change_pct ~ 0 + tsa_change + I(tsa_change^2) + rain_change + I(rain_change^2) + decade, data=d )
    sqtemp_coef <- summary(mod1)$coefficients[1,1]
    sqtemp_sd <- summary(mod1)$coefficients[1,2]
    sqtemp2_coef <- summary(mod1)$coefficients[2,1]
    sqtemp2_sd <- summary(mod1)$coefficients[2,2]
    
    mod1 <- lm(npp_change_pct ~ tsa + rain_change + decade, data=d )
    temp_coef <- summary(mod1)$coefficients[1,1]
    temp_sd <- summary(mod1)$coefficients[1,2]    

    dam_coef <- rbind(dam_coef,c(sqtemp_coef,sqtemp_sd,sqtemp2_coef,sqtemp2_sd,temp_coef,temp_sd,isos[i]))

}
names(dam_coef) <- c("sqtemp_coef","sqtemp_sd","sqtemp2_coef","sqtemp2_sd","temp_coef","temp_sd","iso_a2")
save(dam_coef,file='dam_coefs.rdata')
glimpse(dam_coef)


glimpse(world)
dam_world <- merge(world,dam_coef,by="iso_a2")
dam_world$temp_coef <- as.double(dam_world$temp_coef)
glimpse(dam_world)
dam_world$sqthreedegdam <- as.double(dam_world$sqtemp_coef)*3 + as.double(dam_world$sqtemp2_coef)*9

dam_world$sqthreedegdam[which(dam_world$sqthreedegdam==max(dam_world$sqthreedegdam, na.rm=TRUE))] <- NA

ggplot(data = dam_world) +
    geom_sf() +
    geom_sf(data = dam_world, aes(fill = sqthreedegdam)) +
    scale_fill_gradient2(low = "red",
        mid = "white",
        high = "blue",
        midpoint = 0,
        space = "Lab",
        na.value = "grey50",
        guide = "colourbar",
        aesthetics = "fill")

names(dam)[which(names(dam)=="iso2.x")] <- "iso_a2"
dam_world <- merge(dam_world,dam, by = "iso_a2")

glimpse(dam_world)
ggplot(data = dam_world[which(dam_world$decade==2025),]) +
    geom_sf() +
    geom_sf(data = dam_world, aes(fill = tsa_change)) +
    scale_fill_gradient2(low = "red",
        mid = "white",
        high = "blue",
        midpoint = 0,
        space = "Lab",
        na.value = "grey50",
        guide = "colourbar",
        aesthetics = "fill")

ggplot(data=dam_world, aes(y=tsa_change,x=decade, color=iso_a2))+geom_line()
ggplot(data=dam_world, aes(y=npp_change_pct,x=decade, color=iso_a2))+geom_line()
#remove outliers
#dam_world$temp_coef[which(dam_world$temp_coef==min(dam_world$temp_coef, na.rm=TRUE))] <- NA
hist(dam_world$temp_coef)

glimpse(dam_world)
library(rworldmap)
library(ggplot2)
map.world <- map_data(map="world")
glimpse(map.world)

gg <- ggplot()
gg <- gg + theme(legend.position="none")
gg <- gg + geom_map(data=dam_world, map=dam_world, aes(map_id=region, x=long, y=lat, fill=temp_coef))

gg <- gg + scale_fill_gradient(low = "green", high = "brown3", guide = "colourbar")
gg <- gg + coord_equal()
gg

plot(dam_world[15])

geom_iso <- world



ggplot(data = dam, aes(x=tsa_change,y=npp_change_pct,color=iso2))+
geom_line()



glimpse(a)
glimpse(npp_df)
#order columns by country, then by decade
var_iso_decade <- var_iso_decade[order(var_iso_decade[,1], var_iso_decade[,4]),]
dam_df <- var_iso_decade[,]
for (k in (1:length(isos))){
    dam_df <- var_iso_decade[which(var_iso_decade$iso2==isos[k]),]
    dam_df 
}
#How to take the difference with respect to baseline?
#What baseline? I can use the first decade for now



class(r)
glimpse(r)
plot(world$geom[world$iso_a2==isos[i]])
class(r)
glimpse(r)
glimpse(geom_iso[[1]][[1]])


#plot(decadal_brick)


class(var_array)
#npp.slice <- npp_array[, , 1] 
#dim(npp.slice)
rr <- brick(npp_array)
r <- raster(t(npp.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot(r)


#Now temperature
ncname <- paste(dir1,"clm50_r270_1deg_GSWP3V1_iso_SSP3-7_Smooth.clm2.h0.TSA.201501-228212.nc",sep="")
ncin <- nc_open(ncname)
glimpse(ncin)
#get units
lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")
time <- ncvar_get(ncin,"time")
tunits <- ncatt_get(ncin,"time","units")
tunits
#get variable
dname <- "TSA"
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(npp_array)
nc_close(ncin) 
tmp_array[tmp_array == fillvalue$value] <- NA
tmp_array <- tmp_array - 273.15
tmp_slice <- tmp_array[, , 1] 
dim(tmp_slice)
r <- raster(t(tmp_slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot(r)

years <- rep(2015:2282,each = 12)
months <- rep(1:12,268)
damage_coef <- tmp_array[, , 1] 
damage_pval <- tmp_array[, , 1] 

dim(damage_coef)
le <- length(npp_cell)
for (j in 1:288){
    for (k in 1:192){
        tmp_cell <- tmp_array[j,k , ] 
        npp_cell <- npp_array[j,k , ] 
        if(all(is.na(tmp_cell))){next}
        #range(tmp_cell)
        
        npp_df <- data.frame(npp_cell,years)
        npp_yearly <- aggregate(.~years, data=npp_df, mean, na.rm=TRUE)
        
        tmp_df <- data.frame(tmp_cell,years)
        tmp_yearly <- aggregate(.~years, data=tmp_df, mean, na.rm=TRUE)
        npptmp <- data.frame(npp_yearly, tmp_yearly)

        npptmp_g <- npptmp[1:(dim(npptmp)[1]-1),]
        npptmp_g$npp_g <- npptmp$npp_cell[2:length(npptmp)] - npptmp$npp_cell[1:(length(npptmp)-1)]
        npptmp_g$tmp_g <- npptmp$tmp_cell[2:length(npptmp)] - npptmp$tmp_cell[1:(length(npptmp)-1)]
        
        damage <- lm(npp_g ~ tmp_cell + years, data = npptmp_g)
        damage <- lm(npp_g ~ tmp_cell + I(tmp_cell^2)+ years, data = npptmp_g)
        #summary(damage)

        damage_coef[j,k] <- damage$coefficients[2]
}}
save(damage_coef, file="damage_matrix.Rdata")
rtemp <- raster(t(damage_coef), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
rtemp <- flip(rtemp, direction='y')
plot(rtemp)

npp_2015 <- apply(npp_array[,,1020:1032], c(1,2), mean)
rnpp <- raster(t(npp_2015), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
rnpp <- flip(rnpp, direction='y')
nppdf <- as.data.frame(rnpp, xy = TRUE)
names(nppdf) <- c("lon", "lat", "gC13/m^2/s")
nppdf <- nppdf[which(!is.na(nppdf[,3])),]
npp_map <- ggplot()+ 
theme_bw()+
geom_raster(data=nppdf, aes(x=lon,y=lat, fill=`gC13/m^2/s`)) +
coord_quickmap()+
scale_fill_viridis_c(option = "D")+ 
ggtitle('Yearly avg. NPP (2100)')
npp_map
glimpse(nppdf)

dim(tmp_array)
tmp_2100 <- apply(tmp_array[,,1020:1032], c(1,2), mean)
rtmp <- raster(t(tmp_2100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
rtmp <- flip(rtmp, direction='y')
tmpdf <- as.data.frame(rtmp, xy = TRUE)
names(tmpdf) <- c("lon", "lat", "C")
tmpdf <- tmpdf[which(!is.na(tmpdf[,3])),]
tmp_map <- ggplot()+ 
theme_bw()+
geom_raster(data=tmpdf, aes(x=lon,y=lat, fill=C)) +
coord_quickmap()+
scale_fill_distiller(palette = "Spectral")+
ggtitle('Yearly avg. tmp (2100)')
tmp_map
glimpse(tmpdf)


glimpse(dam)
#gC13/m^2/s
damage_r <- log(rtemp/rnpp) #percent change
damage_r[damage_r==Inf] <- NA 
damage_r[damage_r==-Inf] <- NA 
plot(damage_r)
class(damage_r)
dam <- as.data.frame(damage_r, xy = TRUE)
names(dam) <- c("lon", "lat", "percent change")
dam <- dam[which(!is.na(dam[,3])),]
glimpse(dam)
world <- map_data("world")
npp_change_map <- ggplot()+ 
theme_bw()+
geom_raster(data=dam, aes(x=lon,y=lat, fill=`percent change`)) +
coord_quickmap()+
scale_fill_continuous_diverging(palette = "Blue-Red", trans = "reverse")+
ggtitle('Warming effect on NPP')

npp_change_map

ggarrange(ggarrange(npp_map,tmp_map),npp_change_map,nrow=2)
library('ggpubr')
scale_fill_viridis_c(option="B")
npp_change_map

scale_color_brewer(palette = "Spectral")
scale_fill_continuous(low="red", high="green", 
                       guide="colorbar",na.value="white") + 
ggtitle('Warming effect on NPP')

npp_change_map





scale_fill_viridis() 
scale_fill_viridis(option = "magma", direction = -1)
scale_fill_continuous(low="darkred", high="green", 
                       guide="colorbar",na.value="white")
scale_fill_viridis_c() 

class(npp_array)
mean(npp_array[,,1:12], na.rm=TRUE)
dim(npp_array)
 plot(r)

 plot(rtemp)
 plot(r/rtemp)
 plot(log(rtemp/r))
 
 class(damage_coef)
 class(tmp_array)
#damage_pval[j,k] <- damage$coefficients[1]

glimpse(damage)
glimpse(veg1[[2]][[1]][[1]])




#################### EXTRACT WORLD
if (requireNamespace("sf", quietly = TRUE)) {
  library(sf)
  data(world)
  # or
  world <- st_read(system.file("shapes/world.gpkg", package="spData"))

  plot(world)
                        }

isos <- levels(factor(world$iso_a2))
i <- 1

geom_iso <- world$geom[world$iso_a2==isos[i]]
a <- st_cast(geom_iso, "POLYGON")
a <-as_Spatial(a)
raster::extract(r,a,metdod='simple')
class(r)
glimpse(r)
plot(world$geom[world$iso_a2==isos[i]])
class(r)
glimpse(r)
glimpse(geom_iso[[1]][[1]])
