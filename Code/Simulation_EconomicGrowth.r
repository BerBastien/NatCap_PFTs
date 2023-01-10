        setwd('C:/Users/basti/Documents/GitHub/NatCap_PFTs')
        load("Data/wealth_data.Rda")
        #new_wealth <- read.csv("Data/new_wealth.csv")
        load("Data/Omega_NK_raw_final.Rda")
        x <- c('readxl','raster',"hacksaw", 'ggpubr','dplyr','ncdf4','ggplot2','countrycode','tidyverse','RColorBrewer','colorspace','spData','sf','lfe','marginaleffects','rgdal',"rnaturalearth")
        lapply(x, require, character.only = TRUE)

        glimpse(wealth_data)
        #wealth_data <- new_wealth

        
        glimpse(wealth_data[wealth_data$countrycode=="RUS",])
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
        #w$logmN <- log(w$mNK)
        w$logNforestES <- log(w$NforestES)
        w$logNnonProv <- log(w$NforestES+w$Npa)
        #w$mN <- log(w$mNK)
        summary(felm(logGDP~logK+logH+log(N)|year + countrycode|0|0,data=w, weights = (w$logGDP)))
        K_elas_world <- summary(felm(logGDP~logK+logH+logNforestT|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[1]
        L_elas_world <- summary(felm(logGDP~logK+logH+logNforestT|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[2]
        
        
        summary(felm(logGDP~logK+logH+logNforestT+logNnonProv|year + countrycode|0|0,data=w, weights = (w$logGDP)))
        K_elas_world <- summary(felm(logGDP~logK+logH+logNforestT+logNnonProv|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[1]
        L_elas_world <- summary(felm(logGDP~logK+logH+logNforestT+logNnonProv|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[2]
        NnonProv_elas_world <- summary(felm(logGDP~logK+logH+logNforestT+logNnonProv|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[4]
        
        L_elas_world <- summary(felm(logGDP~logK+logH+logNforestT|year + countrycode|0|0,data=w, weights = (w$logGDP)))$coef[2]


        glimpse(new_wealth)

        #summary(felm(log(GDP)~logK+logH+logNforestT|year + countrycode|0|0,data=new_wealth, weights = log(new_wealth$GDP)))
        
        #summary(felm(logGDP~logK+logH+logmNK|year + countrycode|0|0,data=w, weights = (w$logGDP)))

        #library(CVXR)
        w$logPop <- log(w$Population)




        #w$logmNK <- log(w$mNK)
        
        w2018 <- w[w$year==2018,]
        glimpse(w2018)
        #w2018$g3 <- log(1.0144)/log(w2018$K/w2018$mNK)
        #ggplot(w2018,aes(x=g3,y=mN_gama))+geom_point()

        #w2018$a0 <- w2018$GDP/(w2018$logH^0.54 * w2018$logK^0.46)
        
        country <- levels(factor(w$countrycode))
        first=1
    # for (i in 1:length(country)){
            
    #         data_w <- w[which(w$countrycode==country[i]),]
    #         #glimpse(data_w)
            
    #         #data_w$ForestValue <- data_w$NforestT*0.03
    #         #data_w$ForestShare <- data_w$ForestValue / data_w$GDP
    #         if(all(is.na(data_w$Population))){
    #             print(country[i])
    #             next}
    #         #model <- felm(logGDP~logK+logH+year|0|0|0,data=data_w)
    #         b <- Variable(3)
    #         #X <- cbind(1,as.matrix(data_w[names(data_w)%in%c("logK","logPop","logNforestT")]))
    #         #X <-as.matrix(data_w[names(data_w)%in%c("logK","logPop","logNforestT")])
    #         X <-as.matrix(data_w[names(data_w)%in%c("logK","logPop","logmNK")])
    #         obj <- Minimize(sum((data_w$logGDP - X %*% b)^2))
    #         constraints <- list(b[2] + b[3] + b[1] == 1, b[1]>=0,b[3]>=0,b[2]>=0)
    #         problem <- Problem(obj, constraints)
    #         soln <- solve(problem)

    #         if(soln$status=="solver_error"){next}

    #         bval <- soln$getValue(b)
            
    #         #model <- felm(logGDP~logNforestT+year+I(year^2)|0|0|0,data=data_w)
    #         #summary(model)
    #         #gammas_new <- data.frame(country=country[i], gk = summary(model)$coefficients[2],gh=summary(model)$coefficients[3], g_agg=summary(model)$coefficients[5],gn=summary(model)$coefficients[4])
    #         #gammas_new <- data.frame(country=country[i], gk = summary(model)$coefficients[2],gh=summary(model)$coefficients[3], g_agg=summary(model)$coefficients[5],gn=mean(data_w$ForestShare))
    #         gammas_new <- data.frame(country=country[i], gk = bval[1],gh=bval[3],gn=bval[2])
    #         if(first==1){
    #              gammas <-  gammas_new
    #             first <- 0
    #         }else{
    #             gammas <- rbind(gammas,gammas_new)
    #         }
    #     }
    #     #hist(gammas$gk)        
    #     gamma_comp<-merge(gammas,gama_3,by.x="country",by.y="iso3")
    #     glimpse(gamma_comp)
    #     gamma_comp$gh+gamma_comp$gn+gamma_comp$gk
    #     ggplot(gamma_comp,aes(x=(gamma_3),y=(gn)))+
    #     geom_text(aes(label=country))

    #     hist(gammas$gn)        
    # # Fixed Investments on Manufactured Capital + NatCap damages (exp2)
        wdata2018 <- w[which(w$year==2018),]
        glimpse(wdata2018)
        #mNK_data <- wdata2018[,names(wdata2018)%in%c("countrycode","mNK","mNK_gama")]
        #mNK_data
        #write.csv(mNK_data,"Data/mNK_data.csv")
        #wdata2018$year <- as.numeric(as.character(wdata2018$year))
        
        wealth_sim <- wdata2018[,which(names(wdata2018) %in% c("countrycode",
        "year","H","N","Nagg","NforestT","NforestES","Npa","K","TotalWealth","GDP","Population","NatRents","Continent","labor_share","mN_gama","mNK"))]
        #glimpse(mN_wealth)

#        wealth_sim <- mN_wealth[mN_wealth$year==2018,]
        
        wealth_sim$n <- 0 #net natural capital
        wealth_sim$gy <- 0 #output growth
        wealth_sim$meangy <- 0 #mean output growth
        wealth_sim$exp <- NA #experiment name
        country <- levels(factor(wealth_sim$countrycode))
        Temp_ssps <- read_excel("Data/Temp_ssps_ndc.xlsx", sheet = "data")
        names(Temp_ssps)[1] <- "year"      
        #simname <- c("nodam","10p","SSP245","NDC_cond")
        simname <- c("nodam","RCP6")
        glimpse(Omega_NK)
        

        gama_3 <- data.frame(iso3=wealth_sim$countrycode,gamma_3 = wealth_sim$NatRents*0.01)
        glimpse(gama_3)       
        
        #write.csv(gama_3, file = "Data/gamma3.csv")
        #gammas$gk+gammas$gh+gammas$gn
        #hist(gammas$gn)
        #write.csv(gammas, file = "Data/new_gammas.csv")
        #df_tas<-aggregate(df_tas)

        

        
        #wealth_sim$NforestT <- wealth_sim$mNKTotal
        wealth_sim$mN_gama <- wealth_sim$NatRents*0.01

       glimpse(Omega_NK)
       glimpse(wealth_sim)
       Omega_NK$countrycode <- countrycode(Omega_NK$country, origin="iso2c", destination="iso3c")
       all_data_sim <- merge(wealth_sim,Omega_NK[which(Omega_NK$Function=="Log"),],by="countrycode")
       glimpse(all_data_sim)
       
       x = all_data_sim$Omega_NK_c2[which(all_data_sim$NK_type=="NonMarket")]
       all_data_sim$nN <- all_data_sim$NforestES + all_data_sim$Npa
       w = all_data_sim$nN[which(all_data_sim$NK_type=="NonMarket")]
       mean_damage <- weighted.mean(x, w, na.rm = FALSE)
       mean_damage <- mean(x, na.rm = FALSE)
       
       mean_damage <- min(x, na.rm = FALSE)
       
       mean_damage <- -0.08

       sum_nN <- sum(all_data_sim$nN,na.rm=TRUE)
       sum_nN <- total_S

      
     
       first <- 1
        Function <- "Log"
        glimpse(wealth_sim)
        L_elas_world <-0.54
        elas_param_prod <- 1
        elas_prod <- c(-20,-10,-5,-1,-0.5,0,0.5,1,2)
        elas_prod <- c(-0.5,-0.1,0.1,0.5,0.9,1,1.1,2)
        #elas_prod <- c(-0.5,1)
        #elas_prod <- 1
        NatCap_elas_all <- 0.5
        NatCap_elas_all <- c(0,0.05,0.1,0.2,0.5,0.8,1)
        #NatCap_elas_all <- c(0,0.05,0.5)
        
            first <- 1
            first_elas <- 1
    for (NatCap_elas_i in 1:length(NatCap_elas_all)){
            
            NatCap_elas <- NatCap_elas_all[NatCap_elas_i]
        
        for (elas_i in 1:length(elas_prod)){
            elas_param_prod <- elas_prod[elas_i]
            first <- 1
            for(ss in 1:length(simname)){
                
                    
                for (i in 1:length(country)){
                    s=0.3 #fixed savings rate
                    d=0 #depreciation rate
                    t=83 #number of years of simulation
                    #a1 = wealth_sim$labor_share[wealth_sim$countrycode==country[i]] *0.01
                    a1 <- L_elas_world
                    
                    #a1 <-  gamma_comp$gh[gamma_comp$country==country[i]] 
                    #a3 = wealth_sim$NatRents[wealth_sim$countrycode==country[i]] *0.01     
                    
                    a3 = wealth_sim$mN_gama[wealth_sim$countrycode==country[i]]  
                    #a3 <- gamma_comp$gn[gamma_comp$country==country[i]] 
                    if(length(a3)==0){next}
                    if(is.na(a1)){a1 <- L_elas_world}
                    if (is.na(a3)){a3 <- 0}
                    a2 = 1-a1-a3
                    if (a2<0) {
                    a2 <- 0.0001
                    a3 <- a3-a2}
                    #a1 <- 0.49
                    #a3 <- 0.49
                    #a2 <- 0.02
                    y0 = wealth_sim$GDP[wealth_sim$countrycode==country[i]]  
                    K0 = wealth_sim$K[wealth_sim$countrycode==country[i]] 
                    T0 = wealth_sim$TotalWealth[wealth_sim$countrycode==country[i]]
                    h0 = wealth_sim$H[wealth_sim$countrycode==country[i]]
                    NforestES = wealth_sim$NforestES[wealth_sim$countrycode==country[i]]
                    NforestT = wealth_sim$NforestT[wealth_sim$countrycode==country[i]] 
                    #NforestT = wealth_sim$mNTotal[wealth_sim$countrycode==country[i]] 
                    Nagg = wealth_sim$Nagg[wealth_sim$countrycode==country[i]]
                    Npa = wealth_sim$Npa[wealth_sim$countrycode==country[i]]

                    n0 = NforestT

                    n0_es <- NforestES + Npa
                    
                    #n0 = wealth_sim$N[wealth_sim$countrycode==country[i]]
                    yt=numeric(length=t);yt[1]=y0
                    est=numeric(length=t);est[1]=n0_es*0.03
                    yt_plus_se=numeric(length=t);yt_plus_se[1]=y0
                    est_plus_se=numeric(length=t);est_plus_se[1]=n0_es*0.03
                    yt_minus_se=numeric(length=t);yt_minus_se[1]=y0
                    est_minus_se=numeric(length=t);est_minus_se[1]=n0_es*0.03
                    kt=numeric(length=t);kt[1]=K0
                    Tt=numeric(length=t);Tt[1]=T0
                    pi1_cd <- y0/(h0^a1*K0^a3*n0^a2*sum_nN^NatCap_elas)    #normalizing param
                    pi1 <- y0/(h0^a1*(a3*K0^elas_param_prod + a2*n0^elas_param_prod)^((1-a1)/elas_param_prod)*sum_nN^NatCap_elas)    #normalizing param
                    pi2 <- est[1]/(h0^a1*K0^a2*n0_es^a3) 
                    year = 1:83
                    #df <- wealth_sim[0,]

                        nt=numeric(length=t);nt[1]=n0
                        nt_es=numeric(length=t);nt_es[1]=n0_es
                        yt=numeric(length=t);yt[1]=y0
                        kt=numeric(length=t);kt[1]=K0
                        nt_dam=numeric(length=t);nt_dam[1]=n0
                        nt_es_dam=numeric(length=t);nt_es_dam[1]=n0_es
                        nt_dam_plus_se=numeric(length=t);nt_dam_plus_se[1]=n0
                        nt_es_dam_plus_se=numeric(length=t);nt_es_dam_plus_se[1]=n0_es
                        nt_dam_minus_se=numeric(length=t);nt_dam_minus_se[1]=n0
                        nt_es_dam_minus_se=numeric(length=t);nt_es_dam_minus_se[1]=n0_es
                        nN_total=numeric(length=t);nN_total[1]=sum_nN
                    for(j in 2:t){
                        kt[j]=kt[j-1]*(1-d)+(yt[j-1])*s
                        yt[j-1] = yt[j-1]*(1-s)

                        if (simname[ss]=="nodam"){
                                nt_dam[j] = nt[1]
                                nt_es_dam[j] = nt_es[1]
                                nt_dam_plus_se[j] = nt[1]
                                nt_es_dam_plus_se[j] = nt_es[1]
                                nt_dam_minus_se[j] = nt[1]
                                nt_es_dam_minus_se[j] = nt_es[1]
                                nN_total[j] = nN_total[1]
                        } else if(simname[ss]=="RCP6"){
                            if(j>5){
                                nt_dam[j] = nt[1]*(1+                  
                                    # Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)]*
                                    # (Temp_ssps[j+2,4]-Temp_ssps[1,4])^2)

                                    Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)]*
                                    log(Temp_ssps[j+2,4]-Temp_ssps[1,4]+1))
                                
                                    
                                nt_es_dam[j] = nt_es[1]*(1+
                                    # (Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function==Function)])*
                                    # (Temp_ssps[j+2,4]-Temp_ssps[1,4])^2)
                                    (Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function==Function)])*
                                    log(Temp_ssps[j+2,4]-Temp_ssps[1,4]+1))

                                nt_dam_plus_se[j] = nt[1]*(1+                  
                                    (Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)]+
                                    Omega_NK$Omega_NK_c2_se[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)])*
                                    #(Temp_ssps[j+2,4]-Temp_ssps[1,4])^2)
                                    log(Temp_ssps[j+2,4]-Temp_ssps[1,4]+1))
                                
                                    
                                nt_es_dam_plus_se[j] = nt_es[1]*(1+
                                    (Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function==Function)]+
                                    Omega_NK$Omega_NK_c2_se[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function==Function)])*
                                    #(Temp_ssps[j+2,4]-Temp_ssps[1,4])^2)
                                    log(Temp_ssps[j+2,4]-Temp_ssps[1,4]+1))


                                nt_dam_minus_se[j] = nt[1]*(1+                  
                                    (Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)]-
                                    Omega_NK$Omega_NK_c2_se[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function==Function)])*
                                    #(Temp_ssps[j+2,4]-Temp_ssps[1,4])^2)
                                    log(Temp_ssps[j+2,4]-Temp_ssps[1,4]+1))
                                
                                    
                                nt_es_dam_minus_se[j] = nt_es[1]*(1+
                                    (Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function==Function)]-
                                    Omega_NK$Omega_NK_c2_se[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function==Function)])*
                                    #(Temp_ssps[j+2,4]-Temp_ssps[1,4])^2)
                                    log(Temp_ssps[j+2,4]-Temp_ssps[1,4]+1))

                                nN_total[j] = nN_total[1]*(1+
                                    mean_damage*log(Temp_ssps[j+2,4]-Temp_ssps[1,4]+1))
                                
                                    
                                    
                            
                            }else{
                                nt_dam[j] = nt[1]
                                nt_es_dam[j] = nt_es[1]
                                nt_dam_plus_se[j] = nt[1]
                                nt_es_dam_plus_se[j] = nt_es[1]
                                nt_dam_minus_se[j] = nt[1]
                                nt_es_dam_minus_se[j] = nt_es[1]
                                nN_total[j] = nN_total[1]
                            }

                        } 
                        if(length(nt_dam[j][[1]])==0){next}

                        if(elas_param_prod==0){
                            #yt[j]=pi1*(h0^a1)*(kt[j]^(a3))*nt_dam[j][[1]]^a2
                            #yt_plus_se[j]=pi1_cd*(h0^a1)*(kt[j]^(a3))*nt_dam_plus_se[j][[1]]^a2
                            #yt_minus_se[j]=pi1_cd*(h0^a1)*(kt[j]^(a3))*nt_dam_minus_se[j][[1]]^a2
                            yt[j]=pi1_cd*(nN_total[j][[1]]^NatCap_elas)*(h0^a1)*(kt[j]^(a3))*nt_dam[j][[1]]^a2
                            yt_plus_se[j]=pi1_cd*(nN_total[j][[1]]^NatCap_elas)*(h0^a1)*(kt[j]^(a3))*nt_dam_plus_se[j][[1]]^a2
                            yt_minus_se[j]=pi1_cd*(nN_total[j][[1]]^NatCap_elas)*(h0^a1)*(kt[j]^(a3))*nt_dam_minus_se[j][[1]]^a2
                            
                        }else{
                            yt_plus_se[j]=pi1*(h0^a1)*sum_nN^NatCap_elas*(a3*(kt[j]^(elas_param_prod)) + a2*nt_dam_plus_se[j][[1]]^(elas_param_prod))^((1-a1)/(elas_param_prod))
                            yt_minus_se[j]=pi1*(h0^a1)*sum_nN^NatCap_elas*(a3*(kt[j]^(elas_param_prod)) + a2*nt_dam_minus_se[j][[1]]^(elas_param_prod))^((1-a1)/(elas_param_prod))
                            yt[j]=pi1*sum_nN^NatCap_elas*((a1*h0^elas_param_prod)+a3*(kt[j]^(elas_param_prod)) + a2*nt_dam[j][[1]]^(elas_param_prod))^((1)/(elas_param_prod))

                            # yt_plus_se[j]=pi1*(h0^a1)*sum_nN^NatCap_elas*(a3*(kt[j]^(elas_param_prod)) + a2*nt_dam_plus_se[j][[1]]^(elas_param_prod))^((1-a1)/(elas_param_prod))
                            # yt_minus_se[j]=pi1*(h0^a1)*sum_nN^NatCap_elas*(a3*(kt[j]^(elas_param_prod)) + a2*nt_dam_minus_se[j][[1]]^(elas_param_prod))^((1-a1)/(elas_param_prod))
                            # yt[j]=pi1*(h0^a1)*sum_nN^NatCap_elas*(a3*(kt[j]^(elas_param_prod)) + a2*nt_dam[j][[1]]^(elas_param_prod))^((1-a1)/(elas_param_prod))
                        

                        }
                        
                        #
                        
                        est[j]=nt_es_dam[j][[1]]*0.03  #Natural Capital as the net present value of the flow of benefits
                        #est_plus_se[j]=pi2*(h0^a1)*(kt[j]^(a2))*nt_es_dam_plus_se[j][[1]]^a3
                        #est_minus_se[j]=pi2*(h0^a1)*(kt[j]^(a2))*nt_es_dam_minus_se[j][[1]]^a3
                        #est[j]=pi2*(h0^a1)*(kt[j]^(a2))*nt_es_dam[j][[1]]^a3 #"Production function" of ecosystem services
                        
                        

                        est_plus_se[j]=nt_es_dam_plus_se[j][[1]]*0.03
                        #est_minus_se[j]=pi2*(h0^a1)*(kt[j]^(a2))*nt_es_dam_minus_se[j][[1]]^a3
                        #est_plus_se[j]=pi2*(h0^a1)*(kt[j]^(a2))*nt_es_dam_plus_se[j][[1]]^a3
                        est_minus_se[j]=nt_es_dam_minus_se[j][[1]]*0.03

                        }
                        yt[j] = yt[j]*(1-s)
                        gy <- (yt - lag(yt))/lag(yt) #output growth
                        meangy <- mean(gy[3:length(gy)]) #mean output growth 
                        npv <- sum(yt*(1/((1+0.03)^seq(1:length(yt)))))
                        dam_estimate_prov <- Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="Market" & Omega_NK$Function=="Sq")]
                        dam_estimate_nonprov <- Omega_NK$Omega_NK_c2[which(Omega_NK$iso3==country[i] & Omega_NK$NK_type=="NonMarket" & Omega_NK$Function=="Sq")]
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
                                    GDP=yt,GDP_plus_se=yt_plus_se,GDP_minus_se=yt_minus_se,
                                    Population=wealth_sim$Population[wealth_sim$countrycode==country[i]],
                                    #labor_share=wealth_sim$labor_share[wealth_sim$countrycode==country[i]],
                                    NatRents=wealth_sim$NatRents[wealth_sim$countrycode==country[i]],
                                    n=unlist(nt_dam),
                                    gy = gy, meangy = meangy,exp=simname[ss],npv=npv,dam_prov=dam_estimate_prov, dam_nonprov = dam_estimate_nonprov ,
                                    es = est,es_plus_se = est_plus_se,es_minus_se = est_minus_se, n_es = unlist(nt_es_dam),elasH=a1,elasN=a3,elasK=a2,tfp=pi1 , NatCap_elas=NatCap_elas)
                        }else{
                            df <- bind_rows(df,data.frame(countrycode=country[i], year=year+2017, H=h0, N=wealth_sim$N[wealth_sim$countrycode==country[i]], 
                                    Nagg=Nagg,
                                    NforestT=wealth_sim$NforestT[wealth_sim$countrycode==country[i]],
                                    NforestES=wealth_sim$NforestES[wealth_sim$countrycode==country[i]],
                                    Npa=Npa,
                                    K=kt,
                                    TotalWealth=wealth_sim$TotalWealth[wealth_sim$countrycode==country[i]], 
                                    GDP=yt,GDP_plus_se=yt_plus_se,GDP_minus_se=yt_minus_se,
                                    Population=wealth_sim$Population[wealth_sim$countrycode==country[i]],
                                    #labor_share=wealth_sim$labor_share[wealth_sim$countrycode==country[i]],
                                    NatRents=wealth_sim$NatRents[wealth_sim$countrycode==country[i]],
                                    n=unlist(nt_dam),
                                    gy = gy, meangy = meangy,exp=simname[ss],npv=npv,dam_prov=dam_estimate_prov, dam_nonprov = dam_estimate_nonprov ,
                                    es = est,es_plus_se = est_plus_se,es_minus_se = est_minus_se, n_es = unlist(nt_es_dam) ,elasH=a1,elasN=a3,elasK=a2,tfp=pi1, NatCap_elas=NatCap_elas )) 
                        }
                    #wealth_sim <- rbind(wealth_sim,df)
                    }
                }
        
            

            
            #save(df,file="Results/sim_nc_se.Rda")
            #load("Results/sim_nc.Rda")
            #df_se_central <- df


        
            pal <- palette(brewer.pal(n = 3, name = "Spectral"))
            all_sim <- data.frame(gdp_change = (df$GDP - df$GDP[df$exp=="nodam"])/df$GDP[df$exp=="nodam"],
            gdp_change_plus_se = (df$GDP_plus_se - df$GDP_plus_se[df$exp=="nodam"])/df$GDP_plus_se[df$exp=="nodam"],
            gdp_change_minus_se = (df$GDP_minus_se - df$GDP_minus_se[df$exp=="nodam"])/df$GDP_minus_se[df$exp=="nodam"],
            es_change = (df$es - df$es[df$exp=="nodam"])/df$es[df$exp=="nodam"],
            es_change_plus_se = (df$es_plus_se - df$es_plus_se[df$exp=="nodam"])/df$es_plus_se[df$exp=="nodam"],
            es_change_minus_se = (df$es_minus_se - df$es_minus_se[df$exp=="nodam"])/df$es_minus_se[df$exp=="nodam"],
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
            #labor_share=df$labor_share, NatRents=df$NatRents,    
            #Continent = df$Continent, 
            n=df$n, n_es=df$n_es, gy=df$gy,          meangy=df$meangy,      
            exp=df$exp   , npv = df$npv , es=df$es, elasN=df$elasN ,tfp=df$tfp, NatCap_elas=df$NatCap_elas)

            c <- data.frame(countrycode = countrycode(world$iso_a2,origin="iso2c", destination="iso3c"),Continent=world$region_un)

            all_sim <- merge(all_sim,c,by="countrycode",all.x=TRUE)
            all_sim$country <- all_sim$countrycode


            all_sim$elas_param_prod <- elas_param_prod

            if (first_elas == 1 ){
                all_sim_elas <- all_sim
                first_elas <- 0
            } else{
                all_sim_elas <- rbind(all_sim_elas, all_sim)
            }
        }
    }


    glimpse(all_sim_elas)
    write.csv(all_sim_elas,"Data/all_sim_elas.csv")
    levels(factor(all_sim_elas$elas_param_prod))
    levels(factor(all_sim_elas$NatCap_elas))

    plot_a <- ggplot(data=all_sim_elas[all_sim_elas$year==2100 & all_sim_elas$exp=='RCP6' & all_sim_elas$NatCap_elas==0.5& all_sim_elas$elas_param_prod!=0,])+
    geom_line(aes(x=elas_param_prod,y=gdp_change*100,color=Continent,group=countrycode)) +
    theme_bw()+
    geom_hline(aes(yintercept=0)) +
    geom_vline(aes(xintercept=0),linetype="dashed") +
    geom_text(aes(x=0.1,y=20,label="Cobb-Douglas",angle=90))+
    xlab("Substitutability Parameter")+
    ylab("GDP change (%)")+
    xlim(c(-5,2))
    plot_a    
    ggsave("Figures/Sensitivity_Subs.png")
    
    plot_b <- ggplot(data=all_sim_elas[all_sim_elas$year==2100 & all_sim_elas$exp=='RCP6' & all_sim_elas$elas_param_prod==1,],aes(group=countrycode))+
    geom_line(aes(x=NatCap_elas,y=gdp_change*100,color=Continent)) +
    theme_bw()+
    geom_hline(aes(yintercept=0)) +
    #geom_vline(aes(xintercept=1),linetype="dashed")
    xlab("Production elasticity to habitat and regulating")+
    ylab("GDP change (%)")

    ggarrange(plot_a,plot_b,common.legend=TRUE)    

    elas_gdp <- aggregate(gdp_change~elas_param_prod+NatCap_elas,data=all_sim_elas[all_sim_elas$year==2100 & all_sim_elas$exp=='RCP6'& all_sim_elas$elas_param_prod!=1,],FUN="mean")
    
    ggplot(data=elas_gdp)+
    geom_point(aes(x=elas_param_prod,y=NatCap_elas,size=abs(gdp_change*100))) +
    theme_bw()
    
    ggsave("Figures/GDPchange_elasprodfun.png")


    all_sim <- all_sim_elas
    all_sim <- all_sim_elas[ all_sim_elas$exp=='RCP6' & all_sim_elas$elas_param_prod==1,]
    GDP2100 <- ggplot(all_sim[which(all_sim$exp=="RCP6" & all_sim$year==2100),])+
       geom_point(aes(x=log(GDP),y=gdp_change*100,color=Continent))+theme_bw() + 
       geom_smooth(data=all_sim[which(all_sim$exp=="RCP6" & all_sim$year==2100),],aes(x=log(GDP),y=gdp_change*100))+ylab("GDP change (%)")+
     geom_hline(aes(yintercept=0),linetype="dashed")
        

    ES2100 <- ggplot(all_sim[which(all_sim$exp=="RCP6" & all_sim$year==2100),])+
       geom_point(aes(x=log(GDP),y=es_change*100,color=Continent))+theme_bw() + geom_smooth(data=all_sim[which(all_sim$exp=="RCP6" & all_sim$year==2100),],
     aes(x=log(GDP),y=es_change*100))+ylab("Ecosystem benefits change (%)")+
     geom_hline(aes(yintercept=0),linetype="dashed")
    
     ggarrange(GDP2100,ES2100,common.legend=TRUE,legend="bottom")
     
    ggsave("Figures/Impacts2100_RCP6_SQ_FINAL_elassub.png",dpi=300)


    # glimpse(all_sim_central)

    # all_sim_central$gdp_change_sd_low <- all_sim_neg$gdp_change
    # all_sim_central$gdp_change_sd_high <- all_sim_pos$gdp_change

    
    # all_sim_central$es_change_sd_low <- all_sim_neg$es_change
    # all_sim_central$es_change_sd_high <- all_sim_pos$es_change


    #all_sim <- all_sim[which(all_sim$gdp_change<quantile(all_sim$gdp_change,0.9)),]
    #all_sim <- all_sim[which(all_sim$es_change<quantile(all_sim$es_change,0.9)),]


     plot_trajectory <- ggplot(all_sim[which(all_sim$exp=="RCP6" ),],
     aes(x=es_change*100,y=gdp_change*100,color=Continent,group=country))+
     #aes(x=es_change*100,y=gdp_change*100,group=countrycode))+
     geom_hline(aes(yintercept=0),linetype="dashed")+
     geom_vline(aes(xintercept=0),linetype="dashed")+
       geom_line(alpha=0.5)+
       geom_point(data = all_sim[which(all_sim$exp=="RCP6" & all_sim$year %in% c(2050,2080,2100) ),], 
        aes(x=es_change*100,y=gdp_change*100,color=Continent,shape=factor(year)),alpha=0.3)+
        #ymin=gdp_change-gdp_change_sd,ymax=gdp_change+gdp_change_sd))+
       theme_bw()+
        #geom_errorbar(data=all_sim[which(all_sim$exp=="RCP6" & all_sim$year %in% c(2100) ),],
        #aes(ymin=(gdp_change_minus_se)*100, ymax=(gdp_change_plus_se)*100),alpha=0.3)+
        #geom_errorbarh(data=all_sim[which(all_sim$exp=="RCP6" & all_sim$year %in% c(2100) ),],
        #aes(xmin=(es_change_minus_se)*100, xmax=(es_change_plus_se)*100),alpha=0.3)+
        theme(legend.position="bottom",legend.box="vertical")+
       labs(color="Region",shape="Year")+
       xlab("Ecosystem services change (%)") +
       ylab("GDP change (%)") + ggtitle("")  
       geom_text(data = all_sim[which(all_sim$exp=="RCP6" & all_sim$year %in% c(2100) ),], 
        aes(x=es_change*100,y=gdp_change*100,color=Continent,label=countrycode))
       
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

plot_count_gdp <- ggplot(all_sim[which(all_sim$exp=="RCP6" & all_sim$year ==2100 ),], #& abs(all_sim_mean$gdp_change)>0.01
     aes(x=gdp_change*100,fill=Continent))+xlab('')+#xlim(-15,6)+
     ylab("count in 2100")+
     geom_histogram()+theme_minimal()+coord_flip() 

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
    ggsave("Figures/Trajectories_RCP6_log_NK_previousgammas_newELAS.png",dpi=300)




    ggplot(data=all_sim[all_sim$year %in% c(2018,2100) & all_sim$exp=="RCP6",],aes(x=(n/(n+K+H)),y=(K/(n+K+H))))+
    geom_point(aes(shape=factor(year),color=factor(year)))+
    #geom_line(aes(group=country),alpha=0.1)+
    theme_bw()
    ggsave("Figures/Capital2018_to_2100.png",dpi=300)


    ggplot(data=all_sim[ all_sim$exp=="RCP6",],aes(x=year,y=log(GDP)))+
    #geom_point(aes(shape=factor(year),color=factor(country)))+
    geom_line(aes(group=country),alpha=0.5)+
    theme_bw()


    dif_2100 <- all_sim[all_sim$year==2100 & all_sim$exp=="RCP6",which(names(all_sim)%in% c("year","n","K","TotalWealth"))]/all_sim[all_sim$year==2018 & all_sim$exp=="RCP6",which(names(all_sim)%in% c("year","n","K","TotalWealth"))]
    dif_2100 <- cbind(dif_2100,all_sim[all_sim$year==2100 & all_sim$exp=="RCP6",which(names(all_sim)%in% c("country","Continent","elasN","tfp"))])
    
    ggplot(data=dif_2100,aes(x=(n),y=(K)))+
    geom_point(aes(color=Continent,size=elasN))+
    #geom_text(aes(label=country))+
    xlab("Market NK change in 2100 w.r.t. 2018 level")+
    ylab("K change in 2100 w.r.t. 2018 level")+
    geom_hline(aes(yintercept=1),linetype="dashed")+
    geom_vline(aes(xintercept=1),linetype="dashed")+
    #geom_line(aes(),alpha=0.3)+
    theme_bw()
    ggsave("Figures/CapitalChange_2100.png",dpi=300)



    glimpse(df)
    df_zaf <- df[df$countrycode=="ZAF",]
    df_zaf_2100 <- df_zaf[df_zaf$year==2100,]
    df_zaf_2100

    glimpse(df_zaf_2100)
ggplot(df_zaf)+
geom_line(aes(x=year,y=GDP))    
