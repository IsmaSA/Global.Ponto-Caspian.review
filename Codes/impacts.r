
# Check impacts

setwd("C:/Users/IsmaSA/Desktop/My projects/Ponto Caspian species list")
pacman::p_load(readxl, dplyr,tidyr,stringr, xlsx,ggplot2, ggpubr, ggrepel,data.table,
               gridExtra, scales, viridis, ggsci, cowplot,plotly, viridis,webshot2, htmlwidgets,ggalluvial, 
               ggrepel, patchwork,sf,terra,raster,rnaturalearth, rnaturalearthdata )



df <-readxl::read_xlsx("C:/Users/IsmaSA/Desktop/My projects/Ponto Caspian species list/List.versions/9.6.2025.xlsx")
head(df)
colnames(df) <- c("Class", "Species", "GBIF.key")              
df <- df %>% mutate(Species = str_extract(Species, "^\\S+\\s\\S+"))

impacts <-readxl::read_xlsx("C:/Users/IsmaSA/Desktop/Databases/GIDIAS_20250417_Excel.xlsx")

impacts <- impacts[impacts$Verified.Name.GBIF.Taxon %in% df$Species, ]
unique(impacts$Verified.Name.GBIF.Taxon)
