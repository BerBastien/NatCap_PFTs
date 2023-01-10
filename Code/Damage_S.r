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
            #Damage_S
            first <- 1
            first_rf <- 1
            
            #for(j in 1:177){
            
                #for(typeESi in 1:3){
                    for (dgvmsi in 1:3){
                        
                                

                        if(dgvms[dgvmsi]=="lpj"){
                            PFT_movement <- PFT_movement_lpj
                        } else if(dgvms[dgvmsi]=="car"){
                            PFT_movement <- PFT_movement_car
                        } else if(dgvms[dgvmsi]=="orc"){
                             PFT_movement <- PFT_movement_orc
                        }
                        #glimpse(PFT_movement)
                        #PFT_movement <- a
                        

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
                            
                            
                            
                            PFT_movement_c <- PFT_movement
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
                                    #ps <- ps[ps %notin% c("c3g","c3dh","c4g","c4h","c3hh")]
                                    if(length(ps)==0){
                                        print("no present area")
                                        next
                                    }

                                    ps <- paste0("PFT.",ps)
                                    ps_all <- paste(c(ps,extra_ind_vars),collapse="+")
                                    
                                    pft_pct_pred_dgvm <- aggregate(Percent ~ PFT_code, data = PFT_movement_c_run_present, FUN = "mean")
                                    pft_pct_pred_dgvm <- pft_pct_pred_dgvm[which(paste0("PFT.",pft_pct_pred_dgvm$PFT_code) %in% c(ps,extra_ind_vars)),]
                                    pft_pct_pred_dgvm$PFT_code<-paste0("PFT.",pft_pct_pred_dgvm$PFT_code)
                                    pft_pct_pred_dgvm<-pft_pct_pred_dgvm[order((c(ps,extra_ind_vars))[1:(length(ps))]),]
                                    #input_var_model <- c(pft_pct_pred_dgvm[,2],log(wealth2018$GDP[which(wealth2018$country==isos[j])]),sum(pft_pct_pred_dgvm[,2]))

                                    subset_es <-df_es[which(df_es$Ecosystem.Service.Category %in% c("Regulating","Supporting")),]
                                    
                                    dep_var <- subset_es[,names(subset_es)%in%c("logESValue")]

                                    ind_vars <- subset_es[,names(subset_es)%in%c(ps,extra_ind_vars)]

                                    complete <- complete.cases(ind_vars)
                                    ind_vars<-ind_vars[complete,]
                                    dep_var <- dep_var[complete]


                                    trf <- tuneRF(ind_vars, dep_var, trace=FALSE,plot=FALSE)
                                    
                                    
                                    mt <- trf[which.min(trf[,2]), 1]
                                    
                                    Results_rf <- randomForest(ind_vars, dep_var, importance = TRUE,tree = TRUE, mtry =mt, ntree = 200)
                                    
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

                                    # Get relative contribution
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
                                    eff_pft$es_val_per_area <- eff_pft$relative_contribution * total_S* 0.03 / sum(eff_pft$areas)

                                    #log(eff_pft$es_val_per_area)
                                    #sum(eff_pft$relative_contribution)
                                    
                                    pft_area_model <- aggregate(PFT_area ~ PFT_code, data = PFT_movement_c_run_present, FUN = sum)
                                    pft_area_model <-pft_area_model[which(paste0("PFT.",pft_area_model$PFT_code) %in% names(ind_vars)),]
                                     
                                    #+ 0.01*PFT_ES$ES_rel*PFT_ES$areas_change_pct_adjusted * elasticity_area


                                    Future_areas <- aggregate(PFT_area ~ PFT_code + temp, data = PFT_movement_c_run, FUN = "sum")
                                    #Future_cveg <- aggregate(cveg ~ PFT_code + temp, data = PFT_movement_c_run, FUN = "sum")
                                    Future_cveg <- aggregate(cveg ~ PFT_code + temp, data = PFT_movement_c_run, FUN = "mean")
                                    Future_areas$variable <- paste0("PFT.",Future_areas$PFT_code)
                                    Future_cveg$variable <- paste0("PFT.",Future_cveg$PFT_code)
                                    Future_areas <- merge(Future_areas,eff_pft,by="variable")
                                    Future_areas <- merge(Future_areas,Future_cveg[,3:4],by="variable")
                                    #glimpse(Future_areas)
                                    
                                    Future_areas$S <- Future_areas$es_val_per_area * Future_areas$PFT_area / 0.03
                                    Future_areas_prov <- Future_areas
                                    FutureS <- (aggregate(S ~ temp , data=Future_areas, FUN = "sum")) 
                                    FutureS$S <- FutureS$S/FutureS$S[FutureS$temp==0] 
                                    FutureS$dgvm <- dgvms[dgvmsi]
                                    FutureS$clim <- levels_clim[climi]
                                    FutureS$sceni <- levels_scen[sceni]
                                    
                                    


                                    if(first==1){
                                        NK_allcountries <- FutureS
                                        first <- first+1
                                    } else{
                                        NK_allcountries <- bind_rows(NK_allcountries,FutureS)

                                    }
                                }
                            }
                    }

                    ggplot(NK_allcountries[NK_allcountries$dgvm=="lpj",],aes(x=temp,y=S,color=dgvm))+
                    geom_point(aes(shape=sceni)) +
                    geom_smooth(method="lm",formula=y~log(x+1))+
                    theme_bw()

                    summary(lm(S~log(temp+1)+sceni+clim+dgvm,data=NK_allcountries))


                    write.csv(NK_allcountries,"Data/Future_S.csv")
                    NK_allcountries <- read.csv("Data/Future_S.csv")

            