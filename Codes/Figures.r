
setwd("C:/Users/IsmaSA/Desktop/My projects/Ponto Caspian species list")
pacman::p_load(readxl, dplyr,tidyr,stringr, xlsx,ggplot2, ggpubr, ggrepel,data.table,tidytable,
               gridExtra, scales, viridis, ggsci, cowplot,plotly, viridis,webshot2, htmlwidgets,ggalluvial, 
               ggrepel, patchwork,sf,terra,raster,rnaturalearth, rnaturalearthdata )


## Figures -----------------
list.files()


# 1. Taxonomic information ----------------
list.files("./List.versions", pattern = ".xlsx")
df <- read_xlsx("./List.versions/9.6.2025_Taxonomy.xlsx")
df <- df %>% mutate(Species = str_extract(Species, "^\\S+\\s\\S+"))
head(df)

nas <- sapply(df[, c("Phylum", "Class",'Family','Order','Genus')], function(column) {
  sum(is.na(column))
})
nas

add_root <- TRUE
value_column <- "n"
df2 = df %>% group_by(Phylum, Class, Order,Family,Genus) %>% summarise(n=n_distinct(Species))
DF = df2

colNamesDF <- names(DF)
DT <- data.table(DF, stringsAsFactors = FALSE)
if(add_root){ DT[, root := "Total"]  }

colNamesDT <- names(DT)
hierarchy_columns <- setdiff(colNamesDT, value_column)
DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]

if(is.null(value_column) && add_root){
  setcolorder(DT, c("root", colNamesDF))
} else if(!is.null(value_column) && !add_root) {
  setnames(DT, value_column, "values", skip_absent=TRUE)
  setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
} else if(!is.null(value_column) && add_root) {
  setnames(DT, value_column, "values", skip_absent=TRUE)
  setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
}

hierarchyList <- list()
for(i in seq_along(hierarchy_columns)){
  current_columns <- colNamesDT[1:i]
  if(is.null(value_column)){
    currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
  } else {
    currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
  }
  setnames(currentDT, length(current_columns), "labels")
  hierarchyList[[i]] <- currentDT
}
hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)

parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
hierarchyDT[, c(parent_columns) := NULL]
unique(df2$Phylum)  

cols <- c(
  "Annelida" = "#E41A1C",       
  "Arthropoda" = "#377EB8",      
  "Bryozoa" = "#4DAF4A",         
  #"Cercozoa" = "#984EA3",      
  "Chordata" = "#FF7F00",       
  "Ciliophora" = "#FFFF33",      
  "Cnidaria" = "#984EA3",         
  "Mollusca" = "#F781BF",        
  "Platyhelminthes" = "#666666"  
  # "Tracheophyta" = "#66C2A5"     
)

hierarchyDT$phylum <- sapply(strsplit(hierarchyDT$ids, " - "), function(x) {
  if (length(x) >= 2) x[2] else NA_character_ 
})
hierarchyDT$color <- cols[hierarchyDT$phylum]

p1 <- plot_ly(
  data = hierarchyDT,
  ids = ~ids,
  labels = ~labels,
  parents = ~parents,
  values = ~values,
  type = 'sunburst',
  branchvalues = 'total',
  marker = list(
    colors = ~color,  # Use assigned colors
    line = list(color = "white", width = 1)
  ),
  insidetextfont = list(color = 'black', size = 16, family = 'Arial'),
  outsidetextfont = list(color = 'black', size = 16, family = 'Arial')
)
p1

htmlwidgets::saveWidget(widget = p1, 
                        file = file.path('C:/Users/IsmaSA/Desktop/My projects/Ponto Caspian species list', paste0("Taxonomy", '.html')), selfcontained = FALSE)

webshot2::webshot(
  file.path('C:/Users/IsmaSA/Desktop/My projects/Ponto Caspian species list/', paste0("Taxonomy", '.html')), 
  file = file.path('C:/Users/IsmaSA/Desktop/My projects/Ponto Caspian species list/', paste0("Taxonomy", '.pdf')),  
  selector = "body")


# 1.1 Groups ----------------
library(packcircles)

df <- read_xlsx("./List.versions/9.6.2025_Taxonomy.xlsx")

df2 <- df %>%group_by(Group) %>%
  summarise(Count = n()) %>% arrange(desc(Count))

df2$Group <- factor(df2$Group, levels = df2$Group)

cols <- c(
  "Annelida" = "#E41A1C",       
  "Crustaceans" = "#377EB8",      
  "Bryozoan " = "#4DAF4A",         
  "Fishes" = "#FF7F00",       
  "Chromista" = "#FFFF33",      
  "Cnidaria" = "#984EA3",         
  "Mollusks" = "#F781BF",        
  "Platyhelminthes" = "#666666"  
)

pacman::p_load(ggtreemap, treemapify)

p2 <- ggplot(df2, aes(area = Count, fill = Group, label = paste(Group, Count, sep="\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  theme(legend.position = "none") 

p2
unique(df2$Group)
ggsave("Groups.svg", p2, width = 8, height = 6, dpi = 300)


# 2. Pathways ----------------
library(purrr)

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

path1 <- path1 %>% left_join(df[,c(1,9)], by ="Species")


library(dplyr)
library(networkD3)
install.packages("networkD3", dependencies = TRUE)
names(path1)

path1 <- path1[!is.na(path1$Main.Path), ]
path1 <- path1[!path1$Main.Path =="NA", ]

nodes <- data.frame(name = unique(c(path1$Main.Path, path1$Sub.Path))) %>%
  mutate(group = ifelse(name %in% path1$Main.Path, "Main", "Sub")) %>%
  arrange(name == "Other", name)


path1 <- path1 %>%
  mutate(
    source = match(Main.Path, nodes$name) - 1,
    target = match(Sub.Path, nodes$name) - 1)

links <- path1 %>%
  group_by(Main.Path, source, target) %>%
  summarise(value = dplyr::n(), .groups = "drop") %>%
  mutate(group = Main.Path)

main_paths <- unique(links$group)
color_range <- RColorBrewer::brewer.pal(min(length(main_paths), 8), "Set2")

my_color <- paste0(
  'd3.scaleOrdinal().domain(["',
  paste(main_paths, collapse = '","'),
  '"]).range(["',
  paste(color_range[1:length(main_paths)], collapse = '","'),
  '"])'
)

p <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  LinkGroup = "group",
  colourScale = my_color,
  sinksRight = FALSE,
  fontSize = 15,           # Larger for better legibility
  nodeWidth = 40,          # Wider nodes for label space
  nodePadding = 18         # More padding between nodes
)

p

path1<- path1 %>% left_join(df[,c(1,9)], by ="Species")
str(path1)




# 3. Spatial distribution ----------------
list.files(pattern = ".zip")
grid_res <- 0.08333333
grid_res <- 0.08333333 * 2
r <- rast(nrows = 180 / grid_res, ncols = 360 / grid_res,
          xmin = -180, xmax = 180, ymin = -90, ymax = 90)
values(r) <- 0  

species_cell_tracker <- data.table(species = character(), cell_id = integer())

target_file <- "occurrence.txt"
f <- "0014973-250426092105405.zip"
unzipped_files <- unzip(f, list = TRUE)

unzip(f, files = target_file, overwrite = TRUE)
occurrence_data <- fread(target_file, select = c("species", "occurrenceStatus", "basisOfRecord",
                                                 "countryCode", "county", "acceptedTaxonKey",
                                                 "decimalLatitude", "decimalLongitude", "year"))

occurrence_data <- occurrence_data %>%
  filter(!basisOfRecord %in% c("FOSSIL_SPECIMEN", "PRESERVED_SPECIMEN"),
         occurrenceStatus != "ABSENT",
         !is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(species))

occurrence_data <- occurrence_data[occurrence_data$species %in% df$Species,]


spn <- unique(occurrence_data)
sp <-"Dreissena polymorpha"
summary <- data.frame()
for(sp in spn){
      occ1 <- occurrence_data[occurrence_data$species == sp, ]
      
      valid_coords <- !is.na(occ1$decimalLongitude) & !is.na(occ1$decimalLatitude) &
        occ1$decimalLongitude != 0 & occ1$decimalLatitude != 0
      
      occ1 <- occ1[valid_coords, ]
      
      flags <- tryCatch({
        CoordinateCleaner::clean_coordinates(x = occ1,
                            lon = "decimalLongitude",
                            lat = "decimalLatitude",
                         countries = "countryCode",
                         species = "species",
             tests = c("zeros", "centroids","outliers", "rounded","duplicates","institutions","gbif"))

      }, error = function(e) {
        cat("Error in coordinate cleaning for species:", sp, "\n") 
        return(NULL)  })
      
      if(is.null(flags)) {
        occ2 <- occ1 } else { occ2 <- occ1[flags$.summary, ]  }
     summary <- rbind(summary, occ2)
      
    }
occurrence_data <- summary

a <- occurrence_data[occurrence_data$countryCode=="GB", ]
unique(a$species)

denis <- read_xlsx("10530_2022_2908_MOESM2_ESM.xlsx")
colnames(denis)[c(1, 3, 4)] <- c("species", "decimalLatitude", "decimalLongitude")

occurrence_data <- rbind(
  occurrence_data[, c("species", "decimalLatitude", "decimalLongitude")],
  denis[, c("species", "decimalLatitude", "decimalLongitude")])

occurrence_data$cell_x <- floor(occurrence_data$decimalLongitude / grid_res) * grid_res
occurrence_data$cell_y <- floor(occurrence_data$decimalLatitude / grid_res) * grid_res

coords <- data.frame(
  x = occurrence_data$cell_x + grid_res / 2,
  y = occurrence_data$cell_y + grid_res / 2
)

cell_ids <- cellFromXY(raster(r), coords)
valid <- !is.na(cell_ids)
occurrence_data <- occurrence_data[valid, ]
cell_ids <- cell_ids[valid]
occurrence_data$cell_id <- cell_ids

occ_sf <- st_as_sf(occurrence_data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
occ_proj <- st_transform(occ_sf, crs = crs(r))
coords <- st_coordinates(occ_proj)
cell_ids <- cellFromXY(r, coords)

occurrence_data$cell_id <- cell_ids
occurrence_data <- occurrence_data[!is.na(cell_id)]
unique_species_cells <- unique(occurrence_data[, .(species, cell_id)])

species_counts <- unique_species_cells[, .N, by = cell_id]
r[species_counts$cell_id] <- r[species_counts$cell_id] + species_counts$N  # 0, 39  (min, max)

r[r[] == 0] <- NA

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs")
world <- world[!world$admin == "Antarctica", ]

r <- raster(r)  
r <- projectRaster(r, crs = "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs", method = "bilinear")

#r_df <- as.data.frame(rasterToPoints(r), xy = TRUE)
#colnames(r_df) <- c("x", "y", "richness")
#r <- rasterFromXYZ(r_df[, c("x", "y", "richness")], crs = "+proj=robin")

breaks <- c(1, 2, 3, 5, 10, 15)
labels <- c("1", "2", "3–4", "5–9", "10–15")
values <- getValues(r)
cut(values, breaks = breaks, labels = labels, include.lowest = TRUE)

#hist(values(r))
unique(occurrence_data$countryCode)
a<- occurrence_data[occurrence_data$countryCode=="AU",]
unique(a$species)
range(getValues(r), na.rm=T)
rcl_mat <- matrix(c(
  0.999, 2, 1,
  2, 3, 2,
  3, 5, 3,
  5, 10, 4,
  10, 15.001, 5
), ncol = 3, byrow = TRUE)

r_classified <- raster::reclassify(r, rcl = rcl_mat)


p1<- tm_shape(world) +
  tm_borders(col = "grey75") +
  tm_fill(col = "grey75") +
  tm_shape(r_classified) +  tm_raster(
    title = "Number of \nPonto-Caspian species",
    palette = "plasma",
    style = "cat",
    labels = labels,
    legend.show = TRUE
  ) +
  tm_layout(
    legend.position = c("right", "center"),
    legend.bg.color = "white",
    inner.margins = c(.04, .01, .1, .01),
    bg.color = "grey35",
    outer.bg.color = "white",
    earth.boundary = c(-180, 180, -70, 90),
    earth.boundary.color = "white",
    earth.boundary.lwd = 0.4,
    space.color = "white",
    attr.outside = TRUE,
    attr.color = "grey10",
    frame = FALSE,
    legend.title.size = 1.5,
    legend.text.size = 1)



# EU:
europe_bbox_wgs <- st_as_sfc(st_bbox(c(xmin = -15, xmax = 55, ymin = 34, ymax = 72), crs = 4326))
europe_bbox_robin <- st_transform(europe_bbox_wgs, crs = crs(r))

r_eu <- crop(r_classified, extent(st_bbox(europe_bbox_robin)))
world_eu <- st_crop(world, st_bbox(europe_bbox_robin))

p4<-tm_shape(world_eu) +
  tm_borders(col = "grey75") +
  tm_fill(col = "grey75") +
  tm_shape(r_eu) +
  tm_raster(
    title = "Ponto-Caspian Species",
    palette = "plasma",
    style = "cat",
    labels = labels,
    legend.show = F) +
  tm_layout(
    legend.position = c("right", "center"),
    legend.bg.color = "white",
    inner.margins = c(.04, .01, .1, .01),
    bg.color = "grey35",
    outer.bg.color = "white",
    earth.boundary = c(-180, 180, -70, 90),
    earth.boundary.color = "white",
    earth.boundary.lwd = 0.4,
    space.color = "white",
    attr.outside = TRUE,
    attr.color = "grey10",
    frame = FALSE,
    legend.title.size = 1.5,
    legend.text.size = 1)


# Great lakes
greatlakes_bbox_wgs <- st_as_sfc(st_bbox(c(
  xmin = -95, xmax = -70, 
  ymin = 40, ymax = 50), crs = 4326))

greatlakes_bbox_robin <- st_transform(greatlakes_bbox_wgs, crs = crs(r))

r_greatlakes <- crop(r_classified, extent(st_bbox(greatlakes_bbox_robin)))
world_greatlakes <- st_crop(world, st_bbox(greatlakes_bbox_robin))
library(remotes) # for install_github
install_github('ocean-tracking-network/glatos', build_vignettes = TRUE)
library(glatos)
data("great_lakes_polygon")
gl_polygon <- st_transform(great_lakes_polygon, crs = crs(r))

p5<- tm_shape(world_greatlakes) +
  tm_borders(col = "grey75") +
  tm_fill(col = "grey75") +
  tm_shape(r_greatlakes) +
    tm_raster(
      title = "Ponto‑Caspian Species",
      palette = "plasma",
      style = "cat",
      labels = labels,
      legend.show = FALSE
    ) +
  tm_layout(
    legend.position = c("right", "center"),
    legend.bg.color = "white",
    inner.margins = c(.04, .01, .1, .01),
    bg.color = "grey35",
    earth.boundary = c(-180, 180, -70, 90),
    earth.boundary.color = "white",
    earth.boundary.lwd = 0.4,
    space.color = "white",
    attr.outside = TRUE,
    attr.color = "grey10",
    frame = FALSE,
    legend.title.size = 1.5,
    legend.text.size = 1
  ) +  tm_shape(gl_polygon) +
    tm_polygons(col = "white", alpha = 0.5, border.col = NA) 

tmap_save(tmap_arrange(p4, p5, ncol = 1), filename = "sideplot.svg", width = 10, height = 6, dpi = 300)




# basin ------------------------    
a<- sf::st_read("C:/Users/IsmaSA/Downloads/hybas_af_lev03_v1c.shp")
b<- sf::st_read("C:/Users/IsmaSA/Downloads/hybas_as_lev03_v1c.shp")
c<- sf::st_read("C:/Users/IsmaSA/Downloads/hybas_eu_lev03_v1c.shp")
d<- sf::st_read("C:/Users/IsmaSA/Downloads/hybas_sa_lev03_v1c.shp")
e<- sf::st_read("C:/Users/IsmaSA/Downloads/hybas_na_lev03_v1c.shp")
f<- sf::st_read("C:/Users/IsmaSA/Downloads/hybas_si_lev03_v1c.shp")
g<- sf::st_read("C:/Users/IsmaSA/Downloads/hybas_au_lev03_v1c.shp")
h<- sf::st_read("C:/Users/IsmaSA/Downloads/hybas_gr_lev03_v1c.shp")
i<- sf::st_read("C:/Users/IsmaSA/Downloads/hybas_ar_lev03_v1c.shp")

hydro_combined <- bind_rows(a, b, c, d, e, f, g, h,i)
#hydro_simple <- st_simplify(hydro_combined, dTolerance = 1000)

occ_sf <- st_as_sf(occurrence_data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
hydro_combined <- st_sf(hydro_combined, sf_column_name = "geometry", crs = 4326)

hydro_combined <- st_make_valid(hydro_combined)
occ_proj <- st_transform(occ_sf, crs = st_crs(hydro_combined))
occ_with_basins <- st_join(occ_proj, hydro_combined["HYBAS_ID"], left = FALSE)
occ_dt <- as.data.table(occ_with_basins)
species_per_basin <- unique(occ_dt[, .(species, HYBAS_ID)])
basin_counts <- species_per_basin[, .N, by = HYBAS_ID]
setnames(basin_counts, "N", "species_count")



hydro_richness <- merge(hydro_combined, basin_counts, by = "HYBAS_ID", all.x = TRUE)
hydro_richness$species_count[is.na(hydro_richness$species_count)] <- 0

hydro_richness$species_count[hydro_richness$species_count==0] <- NA
range(hydro_richness$species_count, na.rm=T)
hydro_robin <- st_transform(hydro_richness, crs = crs(r))
breaks <- c(1, 2, 5, 10, 15, 20, 25, Inf)
labels <- c("1", "2–4", "5–9", "10–14", "15–19", "20–24", "25–30")
hydro_robin <- st_simplify(hydro_robin, dTolerance = 3000)
hydro_richness$richness_class <- cut(
  hydro_richness$species_count,
  breaks = breaks,
  labels = labels,
  include.lowest = TRUE,
  right = FALSE
)
hydro_richness <- st_transform(hydro_richness, crs = "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs")
p2<- tm_shape(hydro_richness) +
  tm_polygons(
    col = "species_count",
    palette = "plasma",
    title = "Ponto-Caspian Species",
    style = "fixed",
    breaks = breaks,
    labels = labels,  textNA = "No data"
  ) +
  tm_layout(
    legend.position = c("right", "center"),
    legend.bg.color = "white",
    inner.margins = c(.04, .01, .1, .01),
    bg.color = "grey35",
    outer.bg.color = "white",
    earth.boundary = c(-180, 180, -70, 90),
    earth.boundary.color = "white",
    earth.boundary.lwd = 0.4,
    space.color = "white",
    attr.outside = TRUE,
    attr.color = "grey10",
    frame = FALSE,
    legend.title.size = 0.8,
    legend.text.size = 0.6)
p2

# Map of temporal records --------------- 

occurrence_data <- summary
occurrence_data <- as.data.table(occurrence_data)
occurrence_data[, decade := floor(year / 10) * 10]
occurrence_data<- occurrence_data[!is.na(occurrence_data$decade), ]
occurrence_data<- occurrence_data[occurrence_data$decade >1940, ]

occurrence_sf <- occurrence_data %>%
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(decade)) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)


decades <- sort(unique(occurrence_sf$decade))
palette_colors <- viridis(length(decades), option = "turbo")

table(occurrence_data$decade)
tm_shape(world) +
  tm_borders(col = "grey75") +
  tm_fill(col = "grey75") + tm_shape(occurrence_sf) +
 tm_dots(col = "year", palette = "viridis", style = "cont", size = 0.1, alpha = 0.6,
          title = "Decade", legend.format = list(big.mark = "")) +  tm_layout(
    legend.outside = TRUE,
    frame = FALSE
  ) +
  tm_layout(
    legend.position = c("right", "center"),
    legend.bg.color = "white",
    inner.margins = c(.04, .01, .1, .01),
    bg.color = "grey35",
    outer.bg.color = "white",
    earth.boundary = c(-180, 180, -70, 90),
    earth.boundary.color = "white",
    earth.boundary.lwd = 0.4,
    space.color = "white",
    attr.outside = TRUE,
    attr.color = "grey10",
    frame = FALSE,
    legend.title.size = 0.8,
    legend.text.size = 0.6)


occurrence_sf <- occurrence_data %>%
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(decade)) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

min_dec <- min(occurrence_sf$decade)
max_dec <- max(occurrence_sf$decade)
breaks <- quantile(unique(occurrence_sf$decade), probs = seq(0, 1, length.out = 5))
labels <- paste0("Period ", 1:4)

occurrence_sf$period <- cut(occurrence_sf$decade, breaks = breaks, include.lowest = TRUE, labels = labels)
table(occurrence_sf$year, occurrence_sf$period)
world_simplified <- st_simplify(world, dTolerance = 9000, preserveTopology = TRUE)

p3<- tm_shape(world_simplified) +
  tm_borders(col = "grey75") +
  tm_fill(col = "grey75") +
tm_shape(occurrence_sf) +
  tm_dots(size = 0.1, col = "firebrick3", alpha = 0.2) +
  tm_facets(by = "period", ncol = 2, free.coords = FALSE) +
tm_layout(
    legend.position = c("right", "center"),
    legend.bg.color = "white",
    inner.margins = c(.04, .01, .1, .01),
    bg.color = "grey35",
    outer.bg.color = "white",
    earth.boundary = c(-180, 180, -70, 90),
    earth.boundary.color = "white",
    earth.boundary.lwd = 0.4,
    space.color = "white",
    attr.outside = TRUE,
    attr.color = "grey10",
    frame = FALSE,
    legend.title.size = 0.8,
    legend.text.size = 0.6)

tmap_arrange(p1, p2, p3, ncol = 1)
tmap_save(tmap_arrange(p3, ncol = 1), filename = "spatial3.svg", width = 10, height = 6, dpi = 300)



# 4. First Records ----------------
list.files()
df<- read_xlsx("First.records.xlsx")
df1 <- read_xlsx("./List.versions/9.6.2025_Taxonomy.xlsx")
df1 <- df1 %>% mutate(Species = str_extract(Species, "^\\S+\\s\\S+"))

df_clean <- df %>% filter(usageKey %in% df1$GBIF.key) %>% 
  filter(!is.na(year)) %>%
  arrange(year)

cumulative <- df_clean %>% as.data.frame() %>%
  group_by(year) %>%
  summarise(n = dplyr::n()) %>%
  arrange(year) %>%
  mutate(cumulative_n = cumsum(n))  %>%
  mutate(median_10 = zoo::rollmean(n, k = 10, fill = NA, align = "center"))

cumulative <- cumulative[cumulative$year > 1499, ]



p1 <- ggplot() +
  geom_vline(xintercept = 1992, linetype = "dashed", color = "grey30", size = 0.4) +
  geom_vline(xintercept = 1952, linetype = "dashed", color = "grey30", size = 0.4) +
  geom_vline(xintercept = 1784, linetype = "dashed", color = "grey30", size = 0.4) +
  geom_vline(xintercept = 1959, linetype = "dashed", color = "grey30", size = 0.4) +
  annotate("text", x = 1992, y = 25, label = "Rhine-Main\nDanube Canal\n(1992)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") +
  annotate("text", x = 1952, y = 25, label = "Volga-Don Canal\n(1952)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") + 
  annotate("text", x = 1959, y = 200, label = "Opening of \n(St. Lawrence Seaway\n(1959)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") +
  geom_jitter(data = cumulative, aes(x = year, y = n),
              width = 0.3, height = 0.2, alpha = 0.4, color = "#1f78b4", size = 1.4) +
  geom_line(data = cumulative, aes(x = year, y = median_10),
            color = "#08619c", size = 1.4) +
  geom_smooth(data = cumulative, aes(x = year, y = n),
              method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 0.8) +
  
  scale_y_continuous(name = "Number of Ponto-Caspian\n first records",
                     limits = c(0, 12),breaks = c(seq(0, 12, 2)))  +
  scale_x_continuous(name = "Year", breaks = c(seq(1500, 2000, 100), 2023))+
  
  # Theme and grid tweaks
  theme_classic(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85",  size = 0.3),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(10, 15, 10, 15),
    legend.position = "none")
p1


p2 <- ggplot() +
  geom_vline(xintercept = 1992, linetype = "dashed", color = "grey30", size = 0.4) +
  geom_vline(xintercept = 1952, linetype = "dashed", color = "grey30", size = 0.4) +
  geom_vline(xintercept = 1784, linetype = "dashed", color = "grey30", size = 0.4) +
  annotate("text", x = 1992, y = 600, label = "Rhine-Main\nDanube Canal\n(1992)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") +
             annotate("text", x = 1970, y = 600, label = "Royal Canal", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") +
  annotate("text", x = 1952, y = 400, label = "Volga-Don Canal\n(1952)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") + 
  annotate("text", x = 1952, y = 200, label = "Ahinski Canal\n(1784)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") + 
  geom_line(data = cumulative, aes(x = year, y = cumulative_n),
            color = "#08619c", size = 1.4) +
  scale_y_continuous(name = "Cumulative number of \nPonto-Caspian first records",
                     limits = c(0, 750), expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(name = "Year", breaks = c(seq(1500, 2000, 100), 2023))+
  
  # Theme and grid tweaks
  theme_classic(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85",  size = 0.3),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(10, 15, 10, 15),
    legend.position = "none")
p2
p1 + p2
ggsave("bin.svg", p2)



library(countrycode)

df_clean <- df_clean %>%
  mutate(
    country = countrycode(ISO3, origin = "iso3c", destination = "country.name"),
    continent = countrycode(ISO3, origin = "iso3c", destination = "continent")
  )
unique(df_clean$continent)


df_clean_eu <- df_clean[df_clean$continent =="Europe", ]  %>% 
  filter(!is.na(year)) %>%
  arrange(year)

cumulative_eu <- df_clean_eu %>% as.data.frame() %>%
  group_by(year) %>%
  summarise(n = dplyr::n()) %>%
  arrange(year) %>%
  mutate(cumulative_n = cumsum(n))  %>%
  mutate(median_10 = zoo::rollmean(n, k = 10, fill = NA, align = "center"))

cumulative_eu <- cumulative_eu[cumulative_eu$year > 1499, ]


df_clean_na <- df_clean[df_clean$continent =="Americas" &  df_clean$country %in% c("United States", "Canada", "Mexico"), ]  %>% 
  filter(!is.na(year)) %>%
  arrange(year)
unique(df_clean_na$country)

cumulative_na <- df_clean_na %>% as.data.frame() %>%
  group_by(year) %>%
  summarise(n = dplyr::n()) %>%
  arrange(year) %>%
  mutate(cumulative_n = cumsum(n))  %>%
  mutate(median_10 = zoo::rollmean(n, k = 10, fill = NA, align = "center"))

cumulative_na <- cumulative_na[cumulative_na$year > 1499, ]





p3 <- ggplot() +
  geom_vline(xintercept = 1992, linetype = "dashed", color = "grey30", size = 0.4) +
  geom_vline(xintercept = 1952, linetype = "dashed", color = "grey30", size = 0.4) +
  geom_vline(xintercept = 1784, linetype = "dashed", color = "grey30", size = 0.4) +
  annotate("text", x = 1992, y = 25, label = "Rhine-Main\nDanube Canal\n(1992)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") +
  annotate("text", x = 1952, y = 25, label = "Volga-Don Canal\n(1952)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") + 
  annotate("text", x = 1952, y = 200, label = "Ahinski Canal\n(1784)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") +
  geom_jitter(data = cumulative_eu, aes(x = year, y = n),
              width = 0.3, height = 0.2, alpha = 0.4, color = "#1f78b4", size = 1.4) +
  geom_line(data = cumulative_eu, aes(x = year, y = median_10),
            color = "#08619c", size = 1.4) +
  geom_smooth(data = cumulative_eu, aes(x = year, y = n),
              method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 0.8) +
  
  scale_y_continuous(name = "Number of Ponto-Caspian\n first records",
                     limits = c(0, 10),breaks = c(seq(0, 10, 2)))  +
  scale_x_continuous(name = "Year", breaks = c(seq(1500, 2000, 100), 2023))+
  
  # Theme and grid tweaks
  theme_classic(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85",  size = 0.3),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(10, 15, 10, 15),
    legend.position = "none")
p3


p4 <- ggplot() +
  geom_vline(xintercept = 1992, linetype = "dashed", color = "grey30", size = 0.4) +
  geom_vline(xintercept = 1952, linetype = "dashed", color = "grey30", size = 0.4) +
  geom_vline(xintercept = 1784, linetype = "dashed", color = "grey30", size = 0.4) +
  annotate("text", x = 1992, y = 600, label = "Rhine-Main\nDanube Canal\n(1992)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") +
  annotate("text", x = 1952, y = 400, label = "Volga-Don Canal\n(1952)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") + 
  annotate("text", x = 1952, y = 200, label = "Ahinski Canal\n(1784)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") + 
  geom_line(data = cumulative_eu, aes(x = year, y = cumulative_n),
            color = "#08619c", size = 1.4) +
  scale_y_continuous(name = "Cumulative number of \nPonto-Caspian first records",
                     limits = c(0, 400), expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(name = "Year", breaks = c(seq(1500, 2000, 100), 2023))+
  
  # Theme and grid tweaks
  theme_classic(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85",  size = 0.3),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(10, 15, 10, 15),
    legend.position = "none")
p4



p5 <- ggplot() +
  geom_vline(xintercept = 1992, linetype = "dashed", color = "grey30", size = 0.4) +
  geom_vline(xintercept = 1952, linetype = "dashed", color = "grey30", size = 0.4) +
  geom_vline(xintercept = 1784, linetype = "dashed", color = "grey30", size = 0.4) +
  annotate("text", x = 1992, y = 25, label = "Rhine-Main\nDanube Canal\n(1992)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") +
  annotate("text", x = 1952, y = 25, label = "Volga-Don Canal\n(1952)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") + 
  annotate("text", x = 1952, y = 200, label = "Ahinski Canal\n(1784)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") +
  geom_jitter(data = cumulative_na, aes(x = year, y = n),
              width = 0.3, height = 0.2, alpha = 0.4, color = "#1f78b4", size = 1.4) +
  geom_line(data = cumulative_na, aes(x = year, y = median_10),
            color = "#08619c", size = 1.4) +
  geom_smooth(data = cumulative_na, aes(x = year, y = n),
              method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 0.8) +
  
  scale_y_continuous(name = "Number of Ponto-Caspian\n first records",
                     limits = c(0, 4),breaks = c(seq(0, 4, 1)))  +
  scale_x_continuous(name = "Year", breaks = c(seq(1500, 2000, 100), 2023))+
  
  # Theme and grid tweaks
  theme_classic(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85",  size = 0.3),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(10, 15, 10, 15),
    legend.position = "none")
p5


p6 <- ggplot() +
  geom_vline(xintercept = 1992, linetype = "dashed", color = "grey30", size = 0.4) +
  geom_vline(xintercept = 1952, linetype = "dashed", color = "grey30", size = 0.4) +
  geom_vline(xintercept = 1784, linetype = "dashed", color = "grey30", size = 0.4) +
  annotate("text", x = 1992, y = 600, label = "Rhine-Main\nDanube Canal\n(1992)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") +
  annotate("text", x = 1952, y = 400, label = "Volga-Don Canal\n(1952)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") + 
  annotate("text", x = 1952, y = 200, label = "Ahinski Canal\n(1784)", 
           angle = 90, vjust = -0.5, hjust = 1, size = 3.5, color = "grey30") + 
  geom_line(data = cumulative_na, aes(x = year, y = cumulative_n),
            color = "#08619c", size = 1.4) +
  scale_y_continuous(name = "Cumulative number of \nPonto-Caspian first records",
                     limits = c(0, 50), expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(name = "Year", breaks = c(seq(1500, 2000, 100), 2023))+
  
  theme_classic(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85",  size = 0.3),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(10, 15, 10, 15),
    legend.position = "none")
p6

combined_plot <- (p1 + p2) / (p3 + p4) / (p5 + p6)

ggsave("Temporal.svg", plot = combined_plot, width = 14, height = 12, dpi = 300, units = "in")
