
        wealth <- read.csv('Data\\WorldBank_WealthAccounts_2018USD.csv')
        gdp <- read.csv('Data\\GDP_2015USD_19952018.csv')
        population  <- read.csv('Data\\Pop1995_2018.csv') 
        Natrents  <- read.csv('Data\\ForestRents_2018.csv') #World Bank Data
        r5 <- read.csv("Data/r5regions.csv")
        dam <- read.csv("Data/Damage_coef.csv")
        #write.csv(table_coefficients,"Data/table_coefficients.csv")
        save(ww,file="Data/DataForFigures/ww.Rds")
    #write.csv(df_biome_area_change,"Data/biome_area.csv")
    df_biome_area_change <- read.csv("Data/biome_area.csv")    
            es_ac_notFood <- read.csv("Data/es_ac_notFood_id_05222023.csv")
        #write.csv(results_df,"Damage_coef_Submission3v2_06052023.csv")
        results_df <- read.csv("Data/Damage_coef_Submission3v2_06052023.csv")
            #write.csv(w2018,file="Data/w2018.csv")
            w2018 <- read.csv(w2018,file="Data/w2018.csv")