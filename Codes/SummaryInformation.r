## Taxonomy Ponto-Caspian ------
pacman::p_load(readxl, dplyr,tidyr,stringr, xlsx,ggplot2, ggpubr, ggrepel,data.table,tidytable,
               gridExtra, scales, viridis, ggsci, cowplot,plotly, viridis,webshot2, htmlwidgets,ggalluvial, 
               ggrepel, patchwork,sf,terra,raster,rnaturalearth, rnaturalearthdata )


df <-readxl::read_xlsx("C:/Users/IsmaSA/Desktop/My projects/Ponto Caspian species list/List.versions/9.6.2025.xlsx")
head(df)
colnames(df) <- c("Class", "Species", "GBIF.key")

keys <- data.frame()
s<- "Chelicorophium curvispinum (G.O.Sars, 1895)"
for(s in unique(df$Species)){
  print(s)
  data <- rgbif::name_backbone(name=s, limit = 1 )
  if (!is.null(data) && length(data) > 0 && !is.null(data$usageKey)) {
    kingdom <- if (!is.null(data$kingdom)) data$kingdom else NA
    phylum  <- if (!is.null(data$phylum))  data$phylum else NA
    class   <- if (!is.null(data$class))   data$class else NA
    order   <- if (!is.null(data$order))   data$order else NA
    family  <- if (!is.null(data$family))  data$family else NA
    genus   <- if (!is.null(data$genus))   data$genus else NA
    species.key <- if (!is.null(data$speciesKey)) data$speciesKey else NA
    
    keys <- rbind(keys, data.frame(Species = s, GBIF.key = species.key,
                                   Kingdom = kingdom, Phylum = phylum,Class = class, Order = order,
                                   Family = family, Genus = genus,stringsAsFactors = FALSE))
    
  } else {
    keys <- rbind(keys, data.frame(Species = s, GBIF.key = NA, Kingdom = NA,Phylum = NA, 
                                   Class = NA, Order = NA,Family = NA, Genus = NA,stringsAsFactors = FALSE))
  }
}
colSums(is.na(keys))

writexl::write_xlsx(keys, "C:/Users/IsmaSA/Desktop/My projects/Ponto Caspian species list/List.versions/9.6.2025_Taxonomy.xlsx")




### Analyses of the number of species ----------------------------------
taxa <- readxl::read_xlsx("C:/Users/IsmaSA/Desktop/My projects/Ponto Caspian species list/List.versions/9.6.2025_Taxonomy.xlsx")
nrow(taxa)
unique(taxa$Group)

# Group: 
taxa %>% group_by(Group) %>% summarise(n = n()) %>% arrange(desc(n)) %>% knitr::kable()
# Phylum: 
taxa %>% group_by(Phylum) %>% summarise(n = n()) %>% arrange(desc(n)) %>% knitr::kable()
# Class: 
taxa %>% group_by(Class) %>% summarise(n = n()) %>% arrange(desc(n)) %>% knitr::kable()
# Order: 
taxa %>% group_by(Order) %>% summarise(n = n()) %>% arrange(desc(n)) %>% knitr::kable()
# Family: 
taxa %>% group_by(Family) %>% summarise(n = n()) %>% arrange(desc(n)) %>% knitr::kable()
# Genus: 
taxa %>% group_by(Genus) %>% summarise(n = n()) %>% arrange(desc(n)) %>% knitr::kable()



# Summary Pathways information -------------------
# Here we need three data generated in Figures.r

# Total species identified with pathways
path <- read_xlsx("Pathways.xlsx")
unique(path$pathways)
unique(path$Sub.pathways)
df <- read_xlsx("./List.versions/9.6.2025_Taxonomy.xlsx")
df <- df %>% mutate(Species = str_extract(Species, "^\\S+\\s\\S+"))
path <- path[path$Species %in% df$Species, ]
path <- path %>% mutate(Main.Path = map_chr(pathways, clean.main.pathway))
path <- path[!is.na(path$Main.Path), ]

unique(path$Main.Path)

path <- path %>% mutate(Sub.Path = map_chr(Sub.pathways, sub.pathways))
unique(path$Sub.Path)

path1  <- path %>%
  separate_rows(Sub.Path, sep = ",\\s*") %>%
  separate_rows(Main.Path, sep = ",\\s*") %>%
  distinct(Species, Sub.Path, Main.Path, .keep_all = TRUE)

# More than 1 pathwyays
path1 %>% group_by( Main.Path) %>% summarise(n = n_distinct(Species)) %>% arrange(desc(n)) %>% knitr::kable()
path1 %>% group_by( Sub.Path) %>% summarise(n = n_distinct(Species)) %>% arrange(desc(n)) %>% knitr::kable()

path1 %>% group_by( Main.Path, Group) %>% summarise(n = n_distinct(Species)) %>% arrange(desc(n)) %>% knitr::kable()
path1 %>% group_by( Sub.Path, Group) %>% summarise(n = n_distinct(Species)) %>% arrange(desc(n)) %>% knitr::kable()



### Ponto-Caspian taxonomy vs global NNS taxonomy ----------------------------------

taxa <- readxl::read_xlsx("C:/Users/IsmaSA/Desktop/My projects/Ponto Caspian species list/List.versions/9.6.2025_Taxonomy.xlsx")
str(taxa)

elza <-readxl::read_xlsx("C:/Users/IsmaSA/Desktop/ELZA/GLOBAL NNS DATA FINAL.xlsx") 
str(elza)
unique(elza$Habitat)
elza_fresh <- elza %>% filter(grepl("FRESHWATER", Habitat))

group_colors <- c(
  "Phylum" = "#1b9e77",   "Class"  = "#d95f02",  "Family" = "#7570b3"  )


phylum_comp <- full_join(
  taxa %>% count(Phylum, name = "Ponto_NNS"),
  elza_fresh %>% count(Phylum, name = "Global_NNS"),
  by = "Phylum"
) %>% 
  replace_na(list(Ponto_NNS = 0, Global_NNS = 0)) %>%
  mutate(Global_Prop = Global_NNS / sum(Global_NNS),
         Expected = sum(Ponto_NNS) * Global_Prop)    %>% filter(Ponto_NNS >0)
alpha <- 0.01
n_taxa <- nrow(phylum_comp)
alpha_corrected <- alpha / n_taxa
phylum_comp <- phylum_comp %>% rowwise() %>%mutate(
    Lower_CI = qbinom(p = alpha / 2, size = sum(Ponto_NNS), prob = Global_Prop),
    Upper_CI = qbinom(p = 1 - alpha / 2, size = sum(Ponto_NNS), prob = Global_Prop),
    Over_Rep = Ponto_NNS > Upper_CI,Under_Rep = Ponto_NNS < Lower_CI) %>% ungroup()
p1<- ggplot(phylum_comp, aes(x = Expected, y = Ponto_NNS)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 0.8) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "#1b9e77", alpha = 0.4) +
  geom_point(aes(color = Over_Rep | Under_Rep), size = 3) +
  geom_label_repel(data = subset(phylum_comp, Over_Rep | Under_Rep),
                   aes(label = Phylum), 
                   box.padding = 0.35, point.padding = 0.3,
                   segment.color = 'grey50', size = 3.5,
                   max.overlaps = 15) +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "#1b9e78")) +
  labs(x = "Global aquatic non-native species",
       y = "Observed aquatic non-native \nPonto-Caspian species") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90"))
p1
class_comp <- full_join(
  taxa %>% count(Class, name = "Ponto_NNS"),
  elza_fresh %>% count(Class, name = "Global_NNS"),
  by = "Class" ) %>%  
  replace_na(list(Ponto_NNS = 0, Global_NNS = 0)) %>%
  mutate(Global_Prop = Global_NNS / sum(Global_NNS),
         Expected = sum(Ponto_NNS) * Global_Prop)   %>% filter(Ponto_NNS >0)
class_comp <- class_comp %>% rowwise() %>%mutate(
  Lower_CI = qbinom(p = alpha / 2, size = sum(Ponto_NNS), prob = Global_Prop),
  Upper_CI = qbinom(p = 1 - alpha / 2, size = sum(Ponto_NNS), prob = Global_Prop),
  Over_Rep = Ponto_NNS > Upper_CI,Under_Rep = Ponto_NNS < Lower_CI) %>%ungroup()
p2<- ggplot(class_comp, aes(x = Expected, y = Ponto_NNS)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 0.8) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "#d95f02", alpha = 0.4) +
  geom_point(aes(color = Over_Rep | Under_Rep), size = 3) +
  geom_label_repel(data = subset(class_comp, Over_Rep | Under_Rep),
                   aes(label = Class), 
                   box.padding = 0.35, point.padding = 0.3,
                   segment.color = 'grey50', size = 3.5,
                   max.overlaps = 25) +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "#d95f02")) +
  labs(x = "Global aquatic non-native species",
       y = "Observed aquatic non-native \nPonto-Caspian species") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90"))
p2
family_comp <- full_join(
  taxa %>% count(Family, name = "Ponto_NNS"),
  elza_fresh %>% count(Family, name = "Global_NNS"),
  by = "Family" ) %>% 
  replace_na(list(Ponto_NNS = 0, Global_NNS = 0)) %>%
  mutate(Global_Prop = Global_NNS / sum(Global_NNS),
         Expected = sum(Ponto_NNS) * Global_Prop) %>% filter(Ponto_NNS >0) 
family_comp <- family_comp %>% rowwise() %>%mutate(
  Lower_CI = qbinom(p = alpha / 2, size = sum(Ponto_NNS), prob = Global_Prop),
  Upper_CI = qbinom(p = 1 - alpha / 2, size = sum(Ponto_NNS), prob = Global_Prop),
  Over_Rep = Ponto_NNS > Upper_CI,Under_Rep = Ponto_NNS < Lower_CI) %>%ungroup()
p3<- ggplot(family_comp, aes(x = Expected, y = Ponto_NNS)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 0.8) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "#7570b3", alpha = 0.4) +
  geom_point(aes(color = Over_Rep | Under_Rep), size = 3) +
  geom_label_repel(data = subset(family_comp, Over_Rep | Under_Rep),
                   aes(label = Family), 
                   box.padding = 0.35, point.padding = 0.3,
                   segment.color = 'grey50', size = 2.5,
                   max.overlaps = 30) +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "#7570b3")) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  labs(x = "Global aquatic non-native species",
       y = "Observed aquatic non-native \nPonto-Caspian species")  +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90"))

p3

library(patchwork)
c<- p1 + p2 + p3

ggsave(
  filename = "combined_plot.svg",  
  plot = c,
  width = 12,                       # adjust width in inches
  height = 6,                       # adjust height in inches
  dpi = 300,                        # 300 dpi for publication quality
  units = "in",
  bg = "white"                      # optional: ensures transparent background
)
