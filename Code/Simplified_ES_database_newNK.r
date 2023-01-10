#Read ES database
        es <- read.csv('C:/Users/basti/Documents/GitHub/es_database/database/VEGS_newClassification.csv')
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

        df_es$pct_covered_orc <-df_es$PFT.bobrsu+
        df_es$PFT.bondev +
        df_es$PFT.bondsu + 
        df_es$PFT.c3gra +
        df_es$PFT.tebrsu+
        df_es$PFT.tebrev+
        df_es$PFT.tendev+
        df_es$PFT.c4gra+
        df_es$PFT.trbrev+
        df_es$PFT.trbrrg

        df_es$pct_covered_car <- df_es$PFT.c3hh +
        df_es$PFT.c3dh +
        df_es$PFT.c4h +
        df_es$PFT.brsuas +
        df_es$PFT.brsutecds +
        df_es$PFT.brsutewms +
        df_es$PFT.brevtecds +
        df_es$PFT.brevtewms +
        df_es$PFT.brevxs +
        df_es$PFT.sds +
        df_es$PFT.trs +        
        df_es$PFT.ndevtecdt +        
        df_es$PFT.ndevteclt +        
        df_es$PFT.ndevtedtt +
        df_es$PFT.ndevtedttht +  
        df_es$PFT.ndevstdit + 
        df_es$PFT.ndsutecdt + 
        df_es$PFT.ndsustswt + 
        df_es$PFT.brevdtt + 
        df_es$PFT.brevdttht + 
        df_es$PFT.brevstdit+
        df_es$PFT.brsutecdt + 
        df_es$PFT.brsuteclt + 
        df_es$PFT.brsutewmt + 
        df_es$PFT.brrgtrt + 
        df_es$PFT.brevtrt

        df_es$cveg_total <- df_es$cveg.bne+df_es$cveg.bine+df_es$cveg.bns+df_es$cveg.tebs+
            df_es$cveg.ibs+df_es$cveg.tebe+df_es$cveg.trbe+df_es$cveg.tribe+
            df_es$cveg.trbr+df_es$cveg.c3g+df_es$cveg.c4g

        df_es$cveg_total_orc <-df_es$cveg.bobrsu+
        df_es$cveg.bondev +
        df_es$cveg.bondsu + 
        df_es$cveg.c3gra +
        df_es$cveg.tebrsu+
        df_es$cveg.tebrev+
        df_es$cveg.tendev+
        df_es$cveg.c4gra+
        df_es$cveg.trbrev+
        df_es$cveg.trbrrg

        df_es$cveg_total_car <- df_es$cveg.c3hh +
        df_es$cveg.c3dh +
        df_es$cveg.c4h +
        df_es$cveg.brsuas +
        df_es$cveg.brsutecds +
        df_es$cveg.brsutewms +
        df_es$cveg.brevtecds +
        df_es$cveg.brevtewms +
        df_es$cveg.brevxs +
        df_es$cveg.sds +
        df_es$cveg.trs +        
        df_es$cveg.ndevtecdt +        
        df_es$cveg.ndevteclt +        
        df_es$cveg.ndevtedtt +
        df_es$cveg.ndevtedttht +  
        df_es$cveg.ndevstdit + 
        df_es$cveg.ndsutecdt + 
        df_es$cveg.ndsustswt + 
        df_es$cveg.brevdtt + 
        df_es$cveg.brevdttht + 
        df_es$cveg.brevstdit+
        df_es$cveg.brsutecdt + 
        df_es$cveg.brsuteclt + 
        df_es$cveg.brsutewmt + 
        df_es$cveg.brrgtrt + 
        df_es$cveg.brevtrt


        #pfts <- c("PFT.bne","PFT.bine","PFT.bns","PFT.tebs","PFT.ibs","PFT.tebe","PFT.trbe","PFT.tribe","PFT.trbr","PFT.c3g","PFT.c4g") 
        #df_es$dominant_pft  <- colnames(df_es[,which(names(df_es)%in%pfts)])[max.col(df_es[,which(names(df_es)%in%pfts)],ties.method="first")]

        dim(table(df_es$id))

        df_es$EcosystemArea <- df_es$Spatial.Extent*df_es$pct_covered
        glimpse(df_es)

        
        levels(factor(df_es$Ecosystem.Service.General))
        levels(factor(df_es$Ecosystem.Service.Category))
        

        # df_es <- df_es[which(df_es$Ecosystem.Service.General=='Biological Control' |
        #                     df_es$Ecosystem.Service.General=='Habitat' |
        #                     df_es$Ecosystem.Service.General=="Pollination & Seed Dispersal" |
        #                     df_es$Ecosystem.Service.General=="Soil Formation" |
        #                     df_es$Ecosystem.Service.General=="Soil Quality" |
        #                     df_es$Ecosystem.Service.General=="Soil Retention"),]

        df_es <- df_es[which(df_es$Ecosystem.Service.Category=='Regulating' |
                            df_es$Ecosystem.Service.Category=='Supporting' ),]

        #df_es <- df_es[which(df_es$Ecosystem.Service.General=='Habitat') ,]


                            glimpse(df_es)


        #df_es <- df_es[which(df_es$Ecosystem.Service.General!='Food' &
         #                   df_es$Ecosystem.Service.General!='Climate Stability' &
          #                  df_es$Ecosystem.Service.Specific!="Mineral Products" & 
           #                 df_es$Ecosystem.Service.Sub.Specific!="Timber"),]


                            
        #use_market_levels <- levels(factor(df_es$Sub.Category[which(df_es$Category=="use_market")]))

        #use_market_levels <- use_market_levels[which(use_market_levels!="")]


        PFT_movement_c_present <- PFT_movement_lpj[which(PFT_movement_lpj$decade==2020),]

                            levels_clim <- levels(factor(PFT_movement_c_present$clim))
                            #levels(factor(PFT_movement_c_present$soc))
                            levels_scen <- levels(factor(PFT_movement_c_present$scen))
                            
        PFT_movement_c_present <- PFT_movement_c_present[which(PFT_movement_c_present$clim==levels_clim[1]),]
        PFT_movement_c_present <- PFT_movement_c_present[which(PFT_movement_c_present$scen==levels_scen[1]),]
        `%notin%` <- Negate(`%in%`)
            first <- 1
        
        glimpse(df_es)
        df_es$logpop <- log(df_es$pop_density)
        df_es$logcveg <- log(df_es$cveg_total)
        df_es$logpct <- log(df_es$pct_covered)
        df_es$loggdppc <- log(df_es$gdp_pc)

        #for (j in 3:length(use_market_levels)){
         #   es_use_market <- df_es[which(df_es$Category =="use_market" & df_es$Sub.Category==use_market_levels[j]),]
            
            model1 <- (lm(logESValue ~ logpct + logcveg+ logpop + loggdppc + Continent, data =df_es ))
            summary(model1)

            


        isos <- levels(factor(PFT_movement_c_present$countrycode))
        
        first <- 1
        for (i in 1:length(isos)){
            PFT_movement_c <- PFT_movement_c_present[which(PFT_movement_c_present$countrycode==isos[i]),]
            if(dim(PFT_movement_c_present)[1]==0){
                print("next")
                next}

            pft_pct_pred_dgvm <- aggregate(Percent ~ PFT_code, data = PFT_movement_c_present, FUN = "mean")
            areasPFT_present <- aggregate(PFT_area ~ PFT +PFT_code, data = PFT_movement_c_present, FUN = sum)
            areas_present <- aggregate(area ~ PFT +PFT_code, data = PFT_movement_c_present, FUN = sum)
            cvegPFT_present <- aggregate(cveg ~ PFT +PFT_code, data = PFT_movement_c_present, FUN = mean)


            
            data_country <- wealth_data[wealth_data$countrycode==countrycode(isos[i],origin="iso2c",destination="iso3c") & wealth_data$year==2018,]
            Continent <- world$continent[world$iso_a2==isos[i]][1]

            if( Continent %notin% levels(droplevels(factor(df_es$Continent)))){
                print(paste("not", Continent, "in", isos[i]))
                next}

            EA <- mean(pft_pct_pred_dgvm[,2])*areas_present[1,3]
            popdens <- data_country$Population/(areas_present[1,3])

            if(length(popdens)==0){next}
                                    
            predictedES <-predict(model1,data.frame(logpct=log(sum(pft_pct_pred_dgvm[,2])),logcveg=log(sum(cvegPFT_present[,3])),logpop=log(popdens),loggdppc=log(data_country$GDP/data_country$Population),Continent=Continent))
           # predictedES
            #exp(predictedES)

            #exp(predictedES)*mean(areasPFT_present[,3])*100
            
            #(exp(predictedES)*sum(areasPFT_present[,3])*100/0.03)/data_country$TotalWealth
            
            #exp(predictedES)*sum(areasPFT_present[,3])*100/data_country$GDP

            if (first == 1){
                newmN <- data.frame(countrycode=countrycode(isos[i],origin="iso2c",destination="iso3c"),es_perha=exp(predictedES),S=exp(predictedES)*EA*100/0.03,Continent=Continent)
                first <- 0
            }else{
                newmN <- rbind(newmN,data.frame(countrycode=countrycode(isos[i],origin="iso2c",destination="iso3c"),es_perha=exp(predictedES),S=exp(predictedES)*EA*100/0.03,Continent=Continent))
            }

        }



        



        glimpse(wealth_data)
        glimpse(newmN)
        new_wealth <- merge(wealth_data,newmN,by="countrycode",all=TRUE)

        glimpse(new_wealth)

        ggplot(new_wealth[new_wealth$year==2018,],aes(y=es_perha,x=log(GDP/Population),color=Continent))+
        geom_point()+ scale_y_continuous(trans = 'log2')

        ggplot(new_wealth[new_wealth$year==2018,],aes(y=(S/K),x=log(GDP),color=Continent))+
        #geom_point()+ 
        scale_y_continuous(trans = 'log2')+
        geom_text(aes(label=countrycode))+theme_bw()

        total_S <- sum(new_wealth[new_wealth$year==2018,]$S, na.rm=T)
        #write.csv(new_wealth,"Data/wealth_S.csv")

        ggplot(new_wealth[new_wealth$year==2018,],aes(y=(total_S/K),x=log(GDP),color=Continent))+
        #geom_point()+ 
        scale_y_continuous(trans = 'log2')+
        geom_text(aes(label=countrycode))+theme_bw()

        new_wealth <- read.csv("Data/wealth_S.csv")

        #total_K <- sum(new_wealth[new_wealth$year==2018,]$TotalWealth, na.rm=T)

        new_wealth$Stotal <- total_S
        glimpse(new_wealth)


        new_wealth <- new_wealth[new_wealth$GDP >0 & new_wealth$S >0 & new_wealth$NforestT>0,]
        levels(factor(new_wealth$Continent))
        # interaction rich/poor. dummy below or above median GDP
        # Optim with constraint on 0<S<1; g1+g2+g3=1

        new_wealth$poor[new_wealth$GDP < median(new_wealth$GDP,na.rm=TRUE)] 
        modelCD <- lm(log(GDP)~log(NforestT)+log(K)+log(H)+log(Npa),data=new_wealth[new_wealth$year==2018,])

        modelCD <- felm(log(GDP)~log(NforestT)+log(K)+log(H)+log(Npa)+year+I(year^2)|countrycode|0|0,data=new_wealth)
        summary(modelCD)


        

        mean(new_wealth$K,na.rm=TRUE)

        ggplot(new_wealth,aes(y=(S),x=year,color=Continent))+
        geom_point()+ scale_y_continuous(trans = 'log2')

        modelCD <- lm(log(GDP)~log(NforestT)+log(K)+log(H)+log(S)+year+I(year^2),data=new_wealth)
        summary(modelCD)



        PFT_movement_c_present <- PFT_movement_lpj[which(PFT_movement_lpj$decade==2020),]

                            levels_clim <- levels(factor(PFT_movement_c_present$clim))
                            #levels(factor(PFT_movement_c_present$soc))
                            levels_scen <- levels(factor(PFT_movement_c_present$scen))
                            
        PFT_movement_c_present <- PFT_movement_c_present[which(PFT_movement_c_present$clim==levels_clim[1]),]
        PFT_movement_c_present <- PFT_movement_c_present[which(PFT_movement_c_present$scen==levels_scen[1]),]

        
      
        mN_wealth <- merge(wealth_data,mN_wealth,by=c("countrycode"))

        mN_wealth$mNTotal <- mN_wealth$NforestT + mN_wealth$mN

        glimpse(mN_wealth)

        ggplot(mN_wealth[mN_wealth$year==2018,],aes(y=(mNTotal/K),x=log(K)))+
        #geom_point(aes(color=Continent.x))+
        geom_text(aes(label=countrycode,color=Continent))+ 
        geom_smooth()+
        scale_y_continuous(trans = 'log2')

        modelCD <- lm(log(GDP)~log(mNTotal)+log(K)+log(H)+log(N),data=mN_wealth[mN_wealth$year==2018,])
        summary(modelCD)

        modelCD <- lm(log(GDP)~log(mNTotal)+log(K)+log(H)+log(N),data=mN_wealth[mN_wealth$year==2018 & mN_wealth$Continent=="Europe",])
        modelCD <- lm(log(GDP)~log(mNTotal)+log(K)+log(H)+log(N),data=mN_wealth[mN_wealth$year==2018 & mN_wealth$Continent=="Africa",])
        modelCD <- lm(log(GDP)~log(mNTotal)+log(K)+log(H)+log(N),data=mN_wealth[mN_wealth$year==2018 & mN_wealth$Continent=="South America",])
        modelCD <- lm(log(GDP)~log(mNTotal)+log(K)+log(H)+log(N),data=mN_wealth[mN_wealth$year==2018 & mN_wealth$Continent=="North America",])
        modelCD <- lm(log(GDP)~log(mNTotal)+log(K)+log(H)+log(N),data=mN_wealth[mN_wealth$year==2018 & mN_wealth$Continent=="Asia",])
        summary(modelCD)


          country <- levels(factor(mN_wealth$countrycode))
        first=1
        library(CVXR)
        
          continent <- levels(factor(mN_wealth$Continent))
          w <- mN_wealth[mN_wealth$year==2018,]
          w$logK <- log(w$K)
          w$logPop <- log(w$Population)
          w$logmNK <- log(w$mNTotal)
          w$logGDP <- log(w$GDP)
    
    for (i in 1:length(continent)){
            
            data_w <- w[which(w$Continent==continent[i]),]
            #glimpse(data_w)
            
            #data_w$ForestValue <- data_w$NforestT*0.03
            #data_w$ForestShare <- data_w$ForestValue / data_w$GDP
            if(all(is.na(data_w$Population))){
                print(country[i])
                next}
            #model <- felm(logGDP~logK+logH+year|0|0|0,data=data_w)
            b <- Variable(3)
            #X <- cbind(1,as.matrix(data_w[names(data_w)%in%c("logK","logPop","logNforestT")]))
            #X <-as.matrix(data_w[names(data_w)%in%c("logK","logPop","logNforestT")])
            X <-as.matrix(data_w[names(data_w)%in%c("logK","logPop","logmNK")])
            obj <- Minimize(sum((data_w$logGDP - X %*% b)^2))
            constraints <- list(b[2] + b[3] + b[1] == 1, b[1]>=0,b[3]>=0,b[2]>=0)
            problem <- Problem(obj, constraints)
            soln <- solve(problem)

            if(soln$status=="solver_error"){next}

            bval <- soln$getValue(b)
            
            #model <- felm(logGDP~logNforestT+year+I(year^2)|0|0|0,data=data_w)
            #summary(model)
            #gammas_new <- data.frame(country=country[i], gk = summary(model)$coefficients[2],gh=summary(model)$coefficients[3], g_agg=summary(model)$coefficients[5],gn=summary(model)$coefficients[4])
            #gammas_new <- data.frame(country=country[i], gk = summary(model)$coefficients[2],gh=summary(model)$coefficients[3], g_agg=summary(model)$coefficients[5],gn=mean(data_w$ForestShare))
            gammas_new <- data.frame(country=country[i], gk = bval[1],gh=bval[3],gn=bval[2])
            if(first==1){
                 gammas <-  gammas_new
                first <- 0
            }else{
                gammas <- rbind(gammas,gammas_new)
            }
        }
      

        
        mN_wealth$new_gama <- mN_wealth$mN.y.y*0.03/mN_wealth$GDP
        ggplot(mN_wealth[mN_wealth$year==2018,],aes(y=new_gama,x=log(GDP/Population),color=Continent.x))+
        geom_point()+ scale_y_continuous(trans = 'log10')




        new_wealth$mNK <-rowSums(cbind(new_wealth$mN*1.02, new_wealth$NforestT),na.rm=TRUE)
        #new_wealth$mNK2 <- sum(new_wealth$mN ,new_wealth$NforestT, na.rm=TRUE)
        ggplot(new_wealth[new_wealth$year==2018,],aes(x=mNK,y=mNK2))+
        geom_point()
        
        new_wealth$mN_gama <- 0.01*new_wealth$NatRents + (new_wealth$mN*.03/new_wealth$GDP)
        new_wealth$mNK_gama <-rowSums(cbind(0.01*new_wealth$NatRents, (1.02*new_wealth$mN*.03/new_wealth$GDP)),na.rm=TRUE)
        new_wealth$nN <- new_wealth$NforestES + Npa
        natural_capital <- new_wealth[names(new_wealth) %in% c("year","countrycode","mNK","mNK_gama","nN")]
        natural_capital <- natural_capital[natural_capital$year==2018,]
        glimpse(natural_capital)
        glimpse(new_wealth[new_wealth$year==2018 & new_wealth$countrycode=="EGY",])

        hist(new_wealth$mN_gama[new_wealth$year==2018])
        hist(new_wealth$NatRents[new_wealth$year==2018])






        sold_in_market <- c('Energy & Raw Materials',"Medicinal Resources","Ornamental Resources")
        es_ac_notFood <- df_es[which(df_es$Ecosystem.Service.General=='Energy & Raw Materials' &
                            df_es$Ecosystem.Service.Specific!="Mineral Products" & 
                            df_es$Ecosystem.Service.Sub.Specific!="Timber"),]
        model1 <- (lm(logESValue ~ pct_covered+ cveg_total+ pop_density + log(gdp_pc) + Continent, data =es_ac_notFood ))
        summary(model1)

        glimpse(wealth_data)
        isos <- countrycode(levels(factor(wealth_data$countrycode)),origin="iso3c",destination="iso2c")
        first <- 1

        levels(factor(df_es$Continent))

        levels(factor(PFT_movement$countrycode))
        first <- 1
        for (i in 1:length(isos)){
            PFT_movement_c <- PFT_movement[which(PFT_movement$countrycode==isos[i]),]
            PFT_movement_c_present <- PFT_movement_c[which(PFT_movement_c$decade==2020),]
            #glimpse(PFT_movement_c_present)
            PFT_movement_c_present <-  PFT_movement_c_present[which(PFT_movement_c_present$clim==levels_clim[1] & PFT_movement_c_present$scen == levels_scen[1]),]
            if(dim(PFT_movement_c_present)[1]==0){next}

            pft_pct_pred_dgvm <- aggregate(Percent ~ PFT_code, data = PFT_movement_c_present, FUN = "mean")
            areasPFT_present <- aggregate(PFT_area ~ PFT +PFT_code, data = PFT_movement_c_present, FUN = sum)
            areas_present <- aggregate(area ~ PFT +PFT_code, data = PFT_movement_c_present, FUN = sum)
            cvegPFT_present <- aggregate(cveg ~ PFT +PFT_code, data = PFT_movement_c_present, FUN = mean)

            
            data_country <- wealth_data[wealth_data$countrycode==countrycode(isos[i],origin="iso2c",destination="iso3c") & wealth_data$year==2018,]
            Continent <- world$continent[world$iso_a2==isos[i]][1]

            EA <- sum(pft_pct_pred_dgvm[,2]*areasPFT_present[,3])
            popdens <- data_country$Population/(areas_present[1,3])
                                    
            predictedES <-predict(model1,data.frame(pct_covered=sum(pft_pct_pred_dgvm[,2]),cveg_total=sum(cvegPFT_present[,3]),pop_density=popdens,gdp_pc=log(data_country$GDP/data_country$Population),Continent=Continent))
            # predictedES
            # exp(predictedES)
            # exp(predictedES)*sum(areasPFT_present[,3])*100/0.03
            
            # (exp(predictedES)*sum(areasPFT_present[,3])*100/0.03)/data_country$TotalWealth
            
            # exp(predictedES)*sum(areasPFT_present[,3])*100/data_country$GDP

            if (first == 1){
                newmN <- data.frame(countrycode=countrycode(isos[i],origin="iso2c",destination="iso3c"),es_perha=exp(predictedES),mN=exp(predictedES)*sum(areasPFT_present[,3])*100/0.03,Continent=Continent)
                first <- 0
            }else{
                newmN <- rbind(newmN,data.frame(countrycode=countrycode(isos[i],origin="iso2c",destination="iso3c"),es_perha=exp(predictedES),mN=exp(predictedES)*sum(areasPFT_present[,3])*100/0.03,Continent=Continent))
            }

        }


        sum(areas_present[,3])

        
        
        glimpse(wealth_data)
        glimpse(newmN)
        new_wealth <- merge(wealth_data,newmN,by="countrycode")

        glimpse(new_wealth)
        new_wealth$mNK <-rowSums(cbind(new_wealth$mN*1.02, new_wealth$NforestT),na.rm=TRUE)
        #new_wealth$mNK2 <- sum(new_wealth$mN ,new_wealth$NforestT, na.rm=TRUE)
        ggplot(new_wealth[new_wealth$year==2018,],aes(x=mNK,y=mNK2))+
        geom_point()
        
        new_wealth$mN_gama <- 0.01*new_wealth$NatRents + (new_wealth$mN*.03/new_wealth$GDP)
        new_wealth$mNK_gama <-rowSums(cbind(0.01*new_wealth$NatRents, (1.02*new_wealth$mN*.03/new_wealth$GDP)),na.rm=TRUE)
        new_wealth$nN <- new_wealth$NforestES + Npa
        natural_capital <- new_wealth[names(new_wealth) %in% c("year","countrycode","mNK","mNK_gama","nN")]
        natural_capital <- natural_capital[natural_capital$year==2018,]
        glimpse(natural_capital)
        glimpse(new_wealth[new_wealth$year==2018 & new_wealth$countrycode=="EGY",])

        hist(new_wealth$mN_gama[new_wealth$year==2018])
        hist(new_wealth$NatRents[new_wealth$year==2018])


        write.csv(natural_capital,"Data/natural_capital.csv")

        
        ggplot(new_wealth[new_wealth$year==2018,])+
        geom_point(aes(y=es_perha,x=log(GDP),color=Continent))

        ggplot(new_wealth[new_wealth$year==2018,])+
        geom_point(aes(y=log(mN/K),x=log(NforestT/K),color=Continent))

        ggplot(new_wealth[new_wealth$year==2018,])+
        geom_point(aes(y=(mN/NforestT),x=log(NforestT),color=Continent))

        hist(log((new_wealth$mNK[new_wealth$year==2018])/new_wealth$NforestT[new_wealth$year==2018]))

        
        max(log((new_wealth$mNK[new_wealth$year==2018])/new_wealth$TotalWealth[new_wealth$year==2018]),na.rm=TRUE)

        
        ggplot(new_wealth[new_wealth$year==2018,])+
        geom_point(aes(y=log((mN+NforestT)/TotalWealth),x=log(NforestES/TotalWealth),color=Continent))

        
        ggplot(new_wealth[new_wealth$year==2018,])+
        geom_point(aes(y=(0.03*(mN)/GDP),x=NatRents/100,color=Continent))

        ggplot(new_wealth[new_wealth$year==2018,])+
        geom_point(aes(y=0.01*NatRents+(0.03*(mN)/GDP),x=log(GDP),color=Continent))

        mean((new_wealth$NatRents*0.01+0.03*(new_wealth$mN+new_wealth$NforestT)/new_wealth$GDP),na.rm=TRUE)
        mean((new_wealth$NatRents*0.01),na.rm=TRUE)
        
        exp(-361.514)
        exp(predictedES)       
        #Creating a datasrt with only accepted studies
        glimpse(df_es)
        
        es_ac <- df_es[which(df_es$State=='accepted'),]
        #es_ac <- df_es
        es_ac_notFood <- es_ac[which(es_ac$Ecosystem.Service.General != 'Food'),]
        write.csv(es_ac_notFood,"es_ac_notFood.csv")
        glimpse( es_ac_notFood)

        es_ac_notFood <- es_ac_notFood[,names(es_ac_notFood) %in% 
        c("id","Category","logESValue","Ecosystem.Service.Category","Spatial.Unit.Published","Spatial.Extent","pct_covered","pct_covered_orc","pct_covered_car","logGDP","PFT.bne","PFT.bine","PFT.bns",
        "PFT.tebs","PFT.ibs","PFT.tebe","PFT.trbe","PFT.tribe","PFT.trbr","PFT.c3g","PFT.c4g","Single.Value.Converted","gdp_pc","cveg_total","cveg_total_orc","cveg_total_car",
        "PFT.bobrsu",
        "PFT.bondev",
        "PFT.bondsu" ,
        "PFT.c3gra",
        "PFT.tebrsu",
        "PFT.tebrev",
        "PFT.tendev",
        "PFT.c4gra",
        "PFT.trbrev",
        "PFT.trbrrg",

        "PFT.c3hh",
        "PFT.c3dh",
        "PFT.c4h",
        "PFT.brsuas",
        "PFT.brsutecds",
        "PFT.brsutewms",
        "PFT.brevtecds",
        "PFT.brevtewms",
        "PFT.brevxs",
        "PFT.sds",
        "PFT.trs"        ,
        "PFT.ndevtecdt"        ,
        "PFT.ndevteclt" ,       
        "PFT.ndevtedtt",
        "PFT.ndevtedttht"  ,
        "PFT.ndevstdit" ,
        "PFT.ndsutecdt" ,
        "PFT.ndsustswt" ,
        "PFT.brevdtt" ,
        "PFT.brevdttht" ,
        "PFT.brevstdit",
        "PFT.brsutecdt" ,
        "PFT.brsuteclt" ,
        "PFT.brsutewmt" ,
        "PFT.brrgtrt" ,
        "PFT.brevtrt","cveg.bne","cveg.bine","cveg.bns",
        "cveg.tebs","cveg.ibs","cveg.tebe","cveg.trbe","cveg.tribe","cveg.trbr","cveg.c3g","cveg.c4g","Single.Value.Converted","gdp_pc",
        "cveg.bobrsu",
        "cveg.bondev",
        "cveg.bondsu" ,
        "cveg.c3gra",
        "cveg.tebrsu",
        "cveg.tebrev",
        "cveg.tendev",
        "cveg.c4gra",
        "cveg.trbrev",
        "cveg.trbrrg",

        "cveg.c3hh",
        "cveg.c3dh",
        "cveg.c4h",
        "cveg.brsuas",
        "cveg.brsutecds",
        "cveg.brsutewms",
        "cveg.brevtecds",
        "cveg.brevtewms",
        "cveg.brevxs",
        "cveg.sds",
        "cveg.trs"        ,
        "cveg.ndevtecdt"        ,
        "cveg.ndevteclt" ,       
        "cveg.ndevtedtt",
        "cveg.ndevtedttht"  ,
        "cveg.ndevstdit" ,
        "cveg.ndsutecdt" ,
        "cveg.ndsustswt" ,
        "cveg.brevdtt" ,
        "cveg.brevdttht" ,
        "cveg.brevstdit",
        "cveg.brsutecdt" ,
        "cveg.brsuteclt" ,
        "cveg.brsutewmt" ,
        "cveg.brrgtrt" ,
        "cveg.brevtrt")]
        write.csv(es_ac_notFood,"es_ac_notFood_id.csv")

        glimpse(es_ac_notFood)


    #Read ES database
