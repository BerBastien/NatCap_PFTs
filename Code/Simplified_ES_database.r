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

        dim(table(df_es$id))
        
        #Creating a datasrt with only accepted studies
        es_ac <- df_es[which(df_es$State=='accepted'),]
        es_ac_notFood <- es_ac[which(es_ac$Ecosystem.Service.General != 'Food'),]
        write.csv(es_ac_notFood,"es_ac_notFood.csv")

        es_ac_notFood <- es_ac_notFood[,names(es_ac_notFood) %in% 
        c("id","logESValue","Ecosystem.Service.Category","Spatial.Unit.Published","Spatial.Extent","pct_covered","logGDP","PFT.bne","PFT.bine","PFT.bns",
        "PFT.tebs","PFT.ibs","PFT.tebe","PFT.trbe","PFT.tribe","PFT.trbr","PFT.c3g","PFT.c4g","Single.Value.Converted","gdp_pc")]
        write.csv(es_ac_notFood,"es_ac_notFood_id.csv")

        glimpse(es_ac_notFood)
 
    #Read ES database
