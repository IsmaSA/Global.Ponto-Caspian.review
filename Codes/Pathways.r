
## Identify pathways of invasions for PC

pacman::p_load(readxl, dplyr,tidyr,stringr, xlsx)

df <- read_xlsx("Backbone.xlsx")


# First I need the GIATAR keys

sp <- unique(df$sp)
s<- "Dreissena polymorpha"
keys <- data.frame()
for(s in sp){
  print(s)
  key <- get_usageKey(s)
  if(is.null(key)){
    key <- NA 
    }
  keys <- rbind(keys, data.frame("Species" = s , "Giatar.Key" = key) )
}

keys1<- keys %>% left_join(df[,c(1,2)], by =c("Species"="sp"))
colnames(keys1)[3] <- "GBIF.Key"
  
# Lets check if missing species keys are really these

get_species_name(4417065)
  
keys1$Combined.Key <- ifelse(
  is.na(keys1$Giatar.Key), 
  as.character(keys1$GBIF.Key), 
  as.character(keys1$Giatar.Key))

write_xlsx(keys1,"Species.keys.xlsx")


# First: GIATAR_query_functions.R -----------
keys1 <- read_xlsx("Species.keys.xlsx")
keys1 <- keys1[!duplicated(keys1$Species),]

get_trait_table_list()

res<- data.frame()
for(k in keys1$Combined.Key){
  print(k)
 pathway<- get_trait_table(table_name = "CABI_pathway_vectors", usageKey = k)
 pathway <- pathway[!is.na(pathway$Vector),]
res <- rbind(res,pathway)
 }

for(k in keys1$Combined.Key){
  print(k)
  pathway<- get_trait_table(table_name = "DAISIE_pathways", usageKey = k)
  pathway <- pathway[!is.na(pathway$pathway),]
  res <- rbind(res,pathway)
}

# Now try the other database  ---------------------

df.pathways <- read_xls("/home/ismael-soto/Desktop/INTRODUCTION_PATHWAYS.xls")
df.pathways<- df.pathways[df.pathways$`Species name` %in% keys1$Species, ] 
df.pathways <- df.pathways[!duplicated(df.pathways[, c(1,4,5)]), ]
df.pathways <- df.pathways[,c(1:6)]
colnames(df.pathways) <- c("Species","Taxonomic","Environment","Main.Pathway",
                           "SubPathway","Intentionality")

unique(df.pathways$Main.Pathway)
df.pathways$Main.Pathway[df.pathways$Main.Pathway =="Release in Nature"] <- "Release in nature"
unique(df.pathways$SubPathway)
unique(df.pathways$Intentionality)


df.pathways1<- df.pathways %>% group_by(Species) %>% 
  summarise(pathways = paste(unique(Main.Pathway), collapse = ", "),
            Sub.pathways = paste(unique(SubPathway), collapse = ", "),
            Intentionality = paste(unique(Intentionality), collapse = ", ") )


df.pathways1 %>% left_join(keys1[,c(1,4)],  by="Species" )

pathway <- keys1[,c(1,4)]
pathway$Main.Pathway <- NA
pathway$Sub.Pathway <- NA
pathway$Intentionality <- NA

pathway<- pathway %>% left_join(df.pathways1,  by="Species" )
write_xlsx(pathway,"Pathways.xlsx")


### First records : ----------- 

keys1 <- read_xlsx("Species.keys.xlsx")
res<- data.frame()
for(k in keys1$Species){
  print(k)
records <- get_first_introductions(k, import_additional_native_info = FALSE)
records$species <- k
  res <- rbind(res, records)
  res <- res[!is.na(res$usageKey), ]
  
}
unique(res$usageKey)
unique(res$species)

hanno <- read_xlsx("/home/ismael-soto/Desktop/GlobalAlienSpeciesFirstRecordDatabase_v3.1_freedata.xlsx", sheet=2)
hanno <- hanno[hanno$TaxonName %in% keys1$Species, ]
hanno$ISO3 <- countrycode(hanno$Region, origin = "country.name", destination = "iso3c")

# combine both databases 
res <- res[!res$Native =="TRUE",]
res <- res[!is.na(res$usageKey),]
res1 <- res[,c(8,2,3)]

comb.giatar <- res1[,c(1,2)] 
comb.hanno <- hanno[,c(3,21)] 
comb.hanno1 <- hanno[,c(4,21)] 

colnames(comb.giatar) <- c("country", "iso3c")
colnames(comb.hanno) <- c("country", "iso3c")
colnames(comb.hanno1) <- c("country", "iso3c")

diff <- setdiff(comb.giatar, comb.hanno)
diff1 <- setdiff(comb.giatar, comb.hanno1)


write_xlsx(res,"First.records.xlsx")



### Impact : ----------- 

keys <- read_xlsx("Species.keys.xlsx")

get_trait_table_list()

res<- data.frame()
for(k in keys$Combined.Key){
  print(k)
  impact<- CABI_impact_summary[CABI_impact_summary$usageKey == k,]
  res <- rbind(res,impact)
}


#### Spatial data -------------- (In this case I am using all the data)
df <- read_xlsx("Backbone.xlsx")

keys <- read_xlsx("Species.keys.xlsx")
spn <- unique(keys$GBIF.Key)
sp <- spn[1]



  x <- occ_download(
      pred_in("taxonKey", spn),
      pred("hasCoordinate", TRUE),
      pred("occurrenceStatus","PRESENT"), 
      pred("hasGeospatialIssue", FALSE),
      pred("hasCoordinate", TRUE),
      pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","PRESERVED_SPECIMEN"))),
      user = "ismaelsoto",
      pwd = "Ismaputas123.",
      email = "isma-sa@hotmail.com"
    )
    status <- occ_download_meta(x)$status
    
    while (status != "SUCCEEDED") {
      Sys.sleep(30)  
      status <- occ_download_meta(x)$status  # Update status
    }
    
    cat("Download sp data", "\n")
    z <- occ_download_meta(x)
    z1 <- z %>% as.character()
    z2 <- z1[1]
    
    dat <- occ_download_get(z2, overwrite=T) %>%
      occ_download_import()
    


##### I need also to know the group (fishes, plants, etc)

df <- read_xlsx("Backbone.xlsx")
unique(df$class)    
unique(df$Group)    
unique(df$Group2)    






