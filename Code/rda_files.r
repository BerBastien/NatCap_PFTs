        #save(file="Data/wealth_data.Rda",wealth_data)
                        save(periods_veg_brick,file=paste("Data/PFT_rasters/periods_",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_pft-",pfts[p],"_global_annual_2006_2099.Rdata",sep=""))
                        save(dif_periods_veg_brick,file=paste("Data/PFT_rasters/dif_periods_",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_pft-",pfts[p],"_global_annual_2006_2099.Rdata",sep=""))
                        save(periods_veg_brick,file=paste("Data/cveg_rasters/periods_",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_cveg",pfts[p],"_global_annual_2006_2099.Rdata",sep=""))
                        save(dif_periods_veg_brick,file=paste("Data/cveg_rasters/dif_periods_",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_cveg",pfts[p],"_global_annual_2006_2099.Rdata",sep=""))
        #save(a_h,file="Data/a_h_3dgvms.Rda")
        load("Data/a_h_3dgvms.Rda")
                                ncname <- paste("periods_",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_pft-",pfts[p],"_global_annual_2006_2099.Rdata",sep="")
                                ncnamecveg <- paste("periods_",dgvms_long[dgvmsi],"_",clim_models[climi],"_ewembi_",clim_scen[sceni],"_",soc_scen[soci],"_co2_cveg",pfts[p],"_global_annual_2006_2099.Rdata",sep="")
        save(a,file="Data/PFT_all_data_3dgvms_cveg.Rda")
            load(file="Data/PFT_all_data_3dgvms_cveg.Rda")
            # save(NK_allcountries,file="Data/NK_allcountries_final_submission3v2_06052023.Rda")
            # save(PFT_ES_all,file="Data/PFT_ES_allcountries_final_submission3v2_06052023.Rda")
            
            load("Data/NK_allcountries_final_submission3v2_06052023.Rda")
            load("Data/PFT_ES_allcountries_final_submission3v2_06052023.Rda")

            #save(NK,file="Data/DamageNK_Temp.Rda")           
            #save(NK_temp,file="Data/DamageNK_Temp.Rda")
           
            #load(file=paste0(dir,"DamageNK_Temp.Rda"))
