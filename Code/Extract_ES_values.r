load("Data/PFT_ES_allcountries_final_submission3v2_06052023.Rda")
            glimpse(PFT_ES_all)
names(PFT_ES_all)[c(2,3,4)] <- c("NonMarket_USD_per_km2","Market_USD_per_km2","Biome_area_km2")
write.csv(PFT_ES_all,file="PFT_ES_all.csv")
