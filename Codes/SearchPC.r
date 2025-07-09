
### List of Ponto-Caspian invaders ----
# Ismale Soto

library(pacman)
pacman::p_load(readxl, dplyr,tidyr,stringr, xlsx)

data <- data.frame(Species =as.character(), Native.range = as.character(),
                   Source = as.character())

# From native range datasets --
df = read_xlsx('/home/ismael-soto/Desktop/Native range/Final_data/Native_range_v.1.0.xlsx', sheet = 'Native_country')
df= df %>% filter(str_detect(`Country/Region`, regex("Ponto-Caspian|Caspian|Black|Azov", ignore_case = TRUE)) )
colnames(df)[1] ="Species" 
colnames(df)[5] ="Native.range"
data <- rbind(data,df[,c(1,5)])

df1 = read_xlsx('/home/ismael-soto/Desktop/Native range/native_ranges/Native_GBIF.xlsx')
df1= df1 %>% filter(str_detect(Countries, regex("Ponto-Caspian|Caspian|Black|Azov", ignore_case = TRUE)) )
colnames(df1)[1] ="Species" 
colnames(df1)[2] ="Native.range"
data <- rbind(data,df1[,c(1,2)])
data$Source = "Native range"


# Casties et al., 2016 --
library(pdftools)
remotes::install_github(c("ropensci/tabulapdf"))
library(tabulapdf)

df2 = extract_tables('/home/ismael-soto/Desktop/Ponto Caspian species list/Casties_2016.pdf')[[3]]
head(df2)
colnames(df2)[2] = 'Species'

sp <- c("Proterorhinus marmoratus",'Paranais frici','Potamothrix bedoti',"Cordylophora caspia",
        "Maeotias marginata","Maeotias marginata","Moerisia (=Ostroumovia) inkermanica",
        "Pachycordyle navis","Dreissena rostriformis bugensis","Dreissena polymorpha",
        "Lithoglyphus naticoides","Theodoxus pallasi","Theodoxus pallasi","Viviparus acerosus",
        "Sphaeromyxa sevastopoli","Ichthyocotylurus pileatus","Neascus brevicaudatus",
        "Psammonobiotus communis","Psammonobiotus dziwnowi","Psammonobiotus linearis","Acineta nitocrae")
df3 <- data.frame(Species = sp)
df2 <- rbind(df2[,2],df3)
df2$Native.range <- "Ponto-Caspian"
df2$Source = "Casties et al., 2016"

data <- rbind(data,df2)


# Pauli & Briski  2018 --
df3= read_xlsx('/home/ismael-soto/Desktop/Ponto Caspian species list/Pauli_Briski.xlsx', sheet= 'Table S1')
df3=df3[-2, ]
df3_sp = df3[,2] 
colnames(df3_sp)[1] = 'Species'
df3_sp <- df3_sp %>% mutate(Species = ifelse( is.na(Species),  NA, 
      sapply(strsplit(Species, "\\s+"), function(x) paste(x[1:min(2, length(x))], collapse = " "))) )

df3_sp <- df3_sp[!is.na(df3_sp$Species), ]
df3_sp$Native.range <- "Ponto-Caspian"
df3_sp$Source = "Pauli & Briski  2018"
data <- rbind(data,df3_sp)


# Cuthbert et al., 2021 --
df4= read.delim('/home/ismael-soto/Desktop/Ponto Caspian species list/Cuthbert-etal_2021.tab', sep = "\t", header = TRUE,  row.names = NULL)
df4= df4 %>% filter(Origin == "Ponto-Caspian")
df4<- df4[,c(4,5)]
colnames(df4)[2] ="Native.range"
df4$Source = "Cuthbert et al., 2021"

data <- rbind(data,df4)


# Gallardo --
head(data)
df5 = extract_tables('/home/ismael-soto/Desktop/Ponto Caspian species list/Gallardo.pdf')[[3]]

df5<-data.frame(Species =c("Chaetogammarus curvispinum", "Chelicorophium robustum",
"Chelicorophium sowinskyi","Echinogammarus trichiatus","Neogobius gymnotrachelus"), 
Native.range = c("Ponto-Caspian","Ponto-Caspian","Ponto-Caspian","Ponto-Caspian","Ponto-Caspian"),
Source = c("Gallardo 2015","Gallardo 2015","Gallardo 2015","Gallardo 2015","Gallardo 2015"))

data <- rbind(data,df5)


# Bij de Vaate --

df6 = extract_tables("/home/ismael-soto/Desktop/Ponto Caspian species list/Vaate_et al_2002_macroinvertebrate.pdf")



# Copilas --

df6 = extract_tables("/home/ismael-soto/Desktop/Ponto Caspian species list/2021.07.19.452907.full.pdf")[[2]]
df6 <- df6[-c(1,2),]
words_in_row <- function(x) {
  str_count(str_trim(x), "\\S+") # Count number of words (non-whitespace)
}
# 
df_prepared <- df6 %>%
  mutate(
    ...1 = str_trim(`...1`) # Remove any accidental leading/trailing spaces
  ) %>%
  filter(!is.na(`...1`))    # Remove empty rows if necessary
combined <- character()
buffer <- ""

for (i in seq_len(nrow(df_prepared))) {
  current <- df_prepared$`...1`[i]
  
  if (buffer == "") {
    buffer <- current
  } else {
    test <- paste(buffer, current)
    if (words_in_row(test) <= 2) {
      buffer <- test
    } else {
      combined <- c(combined, buffer)
      buffer <- current
    }
  }
}
if (buffer != "") {
  combined <- c(combined, buffer)
}

df_combined <- tibble(f6 = combined)
df66<- setdiff(df_combined$f6, data$Species) %>% as.data.frame()
colnames(df66)[1] ="Species"
df66$Native.range <- "Ponto-Caspian"
df66$Source = "Copilaș-Ciocianu et al., 2021"

data <- rbind(data,df66)


df7 = extract_tables("/home/ismael-soto/Desktop/Ponto Caspian species list/2021.07.19.452907.full.pdf")[[3]]

sp <- c("Paraniphargoides motasi",
"Pontogammarus abbreviatus",
"Pontogammarus aestuarius",
"Pontogammarus sarsi",
"Stenogammarus compressus",

"Stenogammarus deminutus",
"Stenogammarus macrurus",
"Stenogammarus similis",
"Trichogammarus cf. trichiatus",
"Turcogammarus aralensis",
"Uroniphargoides spinicaudatus",
"Wolgagammarus dzjubani",
"Yogmelina pusilla/limana",
"Zernovia volgensis")
df77<- sp %>% as.data.frame() %>% mutate (Native.range ="Native", Source ="Copilaș-Ciocianu et al., 2021")
colnames(df77)[1] ="Species"

data <- rbind(data,df77)

# https://doi.org/10.1016/j.jglr.2014.03.009
j <- c("Gymnocephalus cernuus",
"Clupeonella caspia",
"Benthophilus stellatus",
"Pomatoschistus minutus",
"Rutilus rutilus",
"Tinca tinca",
"Phoxinus phoxinus",
"Atherina boyeri",
"Perca fluviatilis")
j <- j %>% as.data.frame() %>% mutate (Native.range ="Native", Source ="Snyder et al., 2014")
colnames(j)[1] <-"Species"
 #combine also with this:
  
df8 = extract_tables("/home/ismael-soto/Desktop/Ponto Caspian species list/Fishes.pdf")[[1]]
a = df8[,2]
df8 = extract_tables("/home/ismael-soto/Desktop/Ponto Caspian species list/Fishes.pdf")[[4]]
b = df8[,2]
df8 = extract_tables("/home/ismael-soto/Desktop/Ponto Caspian species list/Fishes.pdf")[[5]]
c = df8[,2]
df8 = extract_tables("/home/ismael-soto/Desktop/Ponto Caspian species list/Fishes.pdf")[[7]]
d = df8[,2]
df8 = extract_tables("/home/ismael-soto/Desktop/Ponto Caspian species list/Fishes.pdf")[[8]]
e = df8[,2]
sp <- rbind(a,b,c,d,e)
colnames(sp)[1] = "Species"
sp$Native.range <- "Check FishBase"
sp$Source = "Snyder et al., 2014"
setdiff(sp$Species, data$Species)
data <- rbind(data,sp)
data <- rbind(data,j)




df9 = extract_tables("/home/ismael-soto/Desktop/Ponto Caspian species list/grigorovich.pdf")[[4]]
a<- "Scolex pleuronectis"


df10 = read_xlsx("/home/ismael-soto/Desktop/Ponto Caspian species list/Aldona Dobrzycka-Krahel.xlsx")
df10 <- df10 %>%
  mutate(Species = str_trim(Species),                     # remove leading/trailing spaces
         Species = str_replace_all(Species, "\\s+", " "), # normalize multiple spaces
         Species = str_extract(Species, "^\\S+\\s\\S+")) 

b<- setdiff(df10$Species, data$Species)  
b <- c(b,a)
b <- b %>% as.data.frame() %>% mutate (Native.range ="Native", Source ="Dobrzycka-Krahel et al., 2021")
colnames(b)[1] ="Species"
data <- rbind(data,b)


j <- c("Proterorhinus semipellucidus",
"Potamotrix moldaviensis",
"Piscicola haranti",
"Cyptorchestia garbinii",
"Trichogammarus trichiatus",
"Gmelina aestuarica",
"Diamysis pengoi",
"Paramysis bakuensis",
"Paramysis kessleri sarsi",
"Pseudocuma cercaroides",
"Pseudocuma laeve",
"Pseudocuma tenuicauda",
"Pterocuma pectinatum",
"Pterocuma rostratum",
"Schizorhamphus eudorelloides",
"Schizorhamphus scabriusculus",
"Volgacuma telmatophora")
#https://link.springer.com/article/10.1007/s10530-017-1375-7/tables/2

j<- j %>% as.data.frame() %>% mutate (Native.range ="Native", Source ="Borza et al., 2017")
colnames(j)[1] ="Species"
data <- rbind(data,j)


sp<- c("Orchestia cavimana") %>% as.data.frame() %>%
  mutate (Native.range ="Native", Source ="Boets et al., 2016")
colnames(sp)[1] ="Species"       
data <- rbind(data,sp)


#  Takhteev1
df11 = extract_tables("/home/ismael-soto/Desktop/Ponto Caspian species list/Rusia.pdf")
# need PC: Done 
# PC Done
sp <- c("Behningiella brachypus",
        "Cardiophilus baeri","",
        "Cardiophilus marisnigri","Zernovia volgensis",
        "Caspicola knipowitschi","Chelicorophium monodon","Chelicorophium mucronatum",
        "Chelicorophium nobile","Chelicorophium spinulosum","Chelicorophium spongicolum",
        "Corophium volutator","Corophium orientale","Akerogammarus contiguus","Akerogammarus knipowitschi",
        "Amathillina affinis","Amathillina maximovitschi","Amathillina spinosa","Axelboeckia spinosa",
        "Baku paradoxus","Cephalogammarus macrocephalus","Derzhavinella cava","Derzhavinella macrochelata",
        "Echinogammarus foxi","Echinogammarus placidus","Echinogammarus trichiatus","Gammarus aequicauda",
        "Gmelina aestuarica","Gmelina costata","Gmelinopsis aurita","Gmelinopsis tuberculata",
        "Kuzmelina kusnezowi","Lanceogammarus andrussovi","Scytaelina simplex","Shablogammarus chablensis",
        "Shablogammarus subnudus","Sowinskya macrocera","Yogmelina brachyura","Yogmelina cocolita",
        "Yogmelina laeviuscula","Yogmelina limana","Yogmelina ovata","Yogmelina pusilla","Iphigenella acanthopoda",
        "Dikerogammarus caspius","Dikerogammarus fluviatilis","Dikerogammarus gruberi","Dikerogammarus oskari",
        "Dikerogammarus palmatus","Niphargogammarus aequimanus","Niphargogammarus borodini","Niphargogammarus quadrimanus",
        "Niphargoides boltovskoi","Niphargoides caspius","Niphargoides corpulentus","Obesogammarus acuminatus",
        "Obesogammarus olvianus","Pandorites podoceroides","Paraniphargoides derzhavini","Paraniphargoides grimmi",
        "Pontogammarus weidemanni","Stenogammarus micrurus","Stenogammarus dzjubani","Onisimus caspius",
        "Onisimus platyceras")

sp<- sp %>% as.data.frame() %>%
  mutate (Native.range ="Native", Source ="Checklist of the Amphipoda (Crustacea) from continental waters of
Russia, with data on alien species")
colnames(sp)[1] ="Species"       
data <- rbind(data,sp)


sp1 <- c(
"Pontastacus leptodactylus",
"Acipenser stellatus" ,
"Acipenser ruthenus"  ,
"Acipenser gueldenstaedt" ,
"Alosa immaculata",
"Alosa tanaica" ,
"Clupeonella cultriventris",
"Ballerus sapa" ,
"Rutilus heckelii",
"Ponticola kessleri",
"Babka gymnotrachelus",
"Pungitius platygaster")%>% as.data.frame() %>%
  mutate (Native.range ="Native", Source ="Ichthyofauna of the Lower Dniester,
the Dniester Estuary and the Adjacent Black Sea")
colnames(sp1)[1] ="Species"       
data <- rbind(data,sp1)

data1 <- data[!duplicated(data$Species), ]
write_xlsx(data1, "RawPC.List.xlsx")

## Check Fishbase

df <- read_xlsx("RawPC.List.xlsx")
head(df)
df <- df[df$Native.range =="Check FishBase",]

check <- country(df$Species)
unique(check$Status)
check <- check %>% filter(Status == "introduced")

setdiff(df$Species, check$Species)


### Check based on GBIF backbone (keys)

df <- read_xlsx("RawPC.List.xlsx")
d <- "Chelon auratus"
res <- data.frame()
for(d in unique(df$Species)){
  sp <- d
  print(sp)
  species <- rgbif::name_backbone(name = d)
  
  acceptedKey <- ifelse(!is.null(species$speciesKey), species$speciesKey, NA)
  speciesKey <- ifelse(!is.null(species$speciesKey), species$speciesKey, NA)
  scientificName <- ifelse(!is.null(species$scientificName), species$scientificName, NA)
  kingdom <- ifelse(!is.null(species$kingdom), species$kingdom, NA)
  phylum <- ifelse(!is.null(species$phylum), species$phylum, NA)
  class <- ifelse(!is.null(species$class), species$class, NA)
  family <- ifelse(!is.null(species$family), species$family, NA)
  order <- ifelse(!is.null(species$order), species$order, NA)
  genus <- ifelse(!is.null(species$genus), species$genus, NA)
  
  temp_df <- data.frame(
    sp, acceptedKey, speciesKey, scientificName, 
    kingdom, phylum, class, family, order, genus)
  
  res <- rbind(res, temp_df)
}
check <- res[duplicated(res$acceptedKey),]
write_xlsx(res, "Backbone.xlsx")

# Spirogammarus major 
rgbif::occ_search(scientificName = "Chelon auratus", limit=5)

species <- rgbif::occ_search(taxonKey = 2367024)[["data"]]

## Clean species List: 
df <- read_xlsx("Backbone.xlsx")
check <- df[duplicated(df$acceptedKey),]
table(df$Group)

d <- "Eichhornia crassipes"
species <- rgbif::name_backbone(name = d)
species$acceptedUsageKey

species$acceptedKey
species$speciesKey

s<- "Ponticola gorlap"
key <- get_usageKey(s)
key

df = read.csv('C:/Users/IsmaSA/Downloads/Species_Taxon_Table.csv')
vector = c()
for(p in df$Species){
  print(p)
species <- rgbif::name_backbone(name = p)

sci = species$scientificName %>% as.vector()
vector = c(vector, sci)

}
df$sci = sci
write_xlsx(res, "Species_Taxon_Table.xlsx")


# Recheck species start with fish ---------

df <- read.table("clipboard", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
df$Species_trimmed <- word(df$Species, 1, 2)


distribution_info <- country(df$Species_trimmed)
native_countries_for_species <- distribution_info %>%
  filter(Status == "native") 

native_countries_for_species <- native_countries_for_species[,c("Species","country")]
