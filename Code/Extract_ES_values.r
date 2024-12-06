load("Data/PFT_ES_allcountries_final_submission3v2_06052023.Rda")
            glimpse(PFT_ES_all)
names(PFT_ES_all)[c(2,3,4)] <- c("NonMarket_USD_per_km2","Market_USD_per_km2","Biome_area_km2")
write.csv(PFT_ES_all,file="PFT_ES_all.csv")
1e-6

1203832102/(1*10^6)
2'780'000


            load(file="Data/PFT_all_data_3dgvms_cveg.Rda") #here
glimpse(a)
12038321/8
length(unique(a$decade))
a_arg <- a %>% filter(DGVM == "lpj",clim=="hadgem2-es",soc=="2005soc",scen=="rcp60",countrycode=="AR",decade==2020) %>% mutate(pft_area_km2 = Percent * area) 
glimpse(a_arg)
a_arg %>% summarise(total_area = sum(pft_area_km2))
hist(a_arg$area)
hist(a_arg$pft_area_km2)
hist(a_arg$Percent)
