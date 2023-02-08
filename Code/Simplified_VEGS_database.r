#Read ES database
        es <- read.csv('C:/Users/basti/Documents/GitHub/es_database/database/VEGS.csv')
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

        
        es_ac <- df_es[which(df_es$State=='accepted'),]
       
        #es_ac <- df_es
        es_ac_notFood <- es_ac[which(es_ac$Ecosystem.Service.General != 'Food'),]

        es_ac_notFood$Cat2 <- es_ac_notFood$Category
            es_ac_notFood$Cat2[es_ac_notFood$Cat2=="services"] <- "nonmarket"
            es_ac_notFood$Cat2[es_ac_notFood$Cat2=="nonuse"] <- "nonmarket"
            count(es_ac_notFood$Ecosystem.Service.General[which(es_ac_notFood$Spatial.Unit.Published%in% c("Acres","Hectares"))])
            count(es_ac_notFood$Ecosystem.Service.Category[which(es_ac_notFood$Spatial.Unit.Published%in% c("Acres","Hectares"))])
        

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
