
#### Figure Ponto-Caspia  definition 
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")
rivers <- ne_download(scale = 10, type = "rivers_lake_centerlines",
                      category = "physical", returnclass = "sf")

bbox <- st_bbox(c(xmin = 20, ymin = 30, xmax = 65, ymax = 60), crs = st_crs(world))
rivers1 <- st_crop(rivers, bbox)
world1  <- st_crop(world, bbox)


ggplot() +
  geom_sf(data = world1, fill = "#ede9dd", color = "#444444", size = 0.3) +
  geom_sf(data = rivers1, color = "#2b83ba", size = 0.6, alpha = 0.8) +
  coord_sf(xlim = c(20, 65), ylim = c(30, 60), expand = FALSE,
           crs = st_crs(4326), datum = NA) +

  theme_minimal(base_family = "Helvetica", base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#e6f2ff", color = NA),
    panel.background = element_rect(fill = "#d4ecf9", color = NA),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "#333333"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#555555"),
    plot.caption = element_text(size = 10, color = "#777777", hjust = 1),
    axis.text = element_text(color = "#666666", size = 10),
    panel.grid.major = element_line(color = "#b3d1ff", size = 0.2)
  )


eu_riv <- st_read("C:/Users/IsmaSA/Downloads/HydroRIVERS_v10.shp")
#eu_riv <- load_rivers()

get_river_width <- function() {
  eu_riv_width <- eu_riv |>
    dplyr::mutate(
      width = as.numeric(ORD_FLOW),
      width = dplyr::case_when(
        width == 3 ~ 1,
        width == 4 ~ 0.8,
        width == 5 ~ 0.6,
        width == 6 ~ 0.4,
        width == 7 ~ 0.2,
        width == 8 ~ 0.2,
        width == 9 ~ 0.1,
        width == 10 ~ 0.1,
        TRUE ~ 0
      )
    ) |>
    sf::st_as_sf()
  
  return(eu_riv_width)
}

eu_riv_width <- get_river_width()

# 3. MAKE BOUNDING BOX
#---------

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

get_bounding_box <- function(bbox, new_prj, bb) {
  bbox <- st_sfc(
    st_polygon(list(cbind(
      c(-10.5, 44.5, 44.5, -10.5, -10.5), #xmin xmax
      c(35.000, 35.000, 69.5, 69.5, 35.000) #ymin y max
    ))),
    crs = crsLONGLAT
  )
  
  new_prj <- sf::st_transform(bbox, crs = 4087)
  bb <- sf::st_bbox(new_prj)
  
  return(bb)
}

bbox <- get_bounding_box()


eu_riv_width1 <- eu_riv_width[eu_riv_width$width > 0.4, ]
europe_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::filter(continent %in% c( "Europe",'Asia') )

bbox <- st_bbox(c(xmin = -15, xmax = 65, ymin = 35, ymax = 65), crs = 4326)
eu_riv_width1 <- st_simplify(eu_riv_width1, preserveTopology = TRUE, dTolerance = 8000)
ggplot() +
    geom_sf(data = europe_map, fill = "gray99", color = "gray30") +
    geom_sf(
      data = eu_riv_width1,
      aes(
        color = factor(ORD_FLOW), size = width,
        alpha = factor(ORD_FLOW)
      )
    ) +    
coord_sf(
  crs = 4326,
  xlim = c(bbox["xmin"], bbox["xmax"]),
  ylim = c(bbox["ymin"], bbox["ymax"]),
  expand = FALSE
)  +
    labs(
      y = "", subtitle = "",
      x = "",
      title = "",
      caption = ""
    ) +
    scale_color_manual(
      name = "",
      values = c(
        "#08306b", "#08519c", "#2171b5",
        "#4292c6", "#6baed6", "#9ecae1",
        "#c6dbef", "#deebf7"
      )
    ) +
    scale_size(range = c(0, .3)) +
    scale_alpha_manual(values = c(
      "3" = 1, "4" = 1, "5" = .7, "6" = .6,
      "7" = .4, "8" = .3, "9" = .2, "10" = .1
    )) +
    theme_minimal() +
    theme(
      panel.background = element_blank(),
      legend.background = element_blank(),
      legend.position = "none",
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.title = element_text(
        size = 40, color = "#2171b5", hjust = 0.5, vjust = 0
      ),
      plot.subtitle = element_text(
        size = 14, color = "#ac63a0", hjust = 0.5, vjust = 0
      ),
      plot.caption = element_text(
        size = 10, color = "grey60", hjust = 0.5, vjust = 10
      ),
      axis.title.x = element_text(
        size = 10, color = "grey20", hjust = 0.5, vjust = -6
      ),
      legend.text = element_text(
        size = 9, color = "grey20"
      ),
      legend.title = element_text(size = 10, color = "grey20"),
      strip.text = element_text(size = 12),
      plot.margin = unit(c(t = 1, r = -2, b = -1, l = -2), "lines"),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
ggsave("Ponto-Caspian.svg", width = 10, height = 8, dpi = 300)
#444,948