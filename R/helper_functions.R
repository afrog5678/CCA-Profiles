setwd()

load_data <- function(){
 demographics <<- read.csv("data/demographics.csv")
 lifeexp <<- read.csv("data/lifeexpectancyovertime.csv")
 lcod <<- read.csv("data/LCOD.csv")
 LCOD_ot <<- read.csv("data/LCODovertime.csv")
 narratives <<- read.csv("data/cca_text.csv")
 pl <<- read.csv("spatial/pointlayers.csv")
}
render_cca <- function(cca){
 rmarkdown::render("CCA_Profile.Rmd",params=list(cca=cca),output_file=file.path("reports",paste0(gsub(" ","_",cca),".html")))
}

load_spatial <- function(){
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  cca_sf <<- sf::st_read(
    here::here(
      "spatial",
      "chicago_cca.shp"
    ),
    quiet=T
)
}

plot_chicago_map <- function(cca){
  library(sf)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(patchwork)
  cca_sf <- st_read(
    here::here(
      "spatial",
      "Chicago_CCA.shp"
    ),
    quiet = TRUE
  )
  cca_sf <- cca_sf %>%
    rename("CCA" = "community")
  
  le2024 <- lifeexp %>%
    filter(Year == 2024) %>%
    select(CCA, le)
  
  cca_sf <- cca_sf %>%
    dplyr::mutate(
      CCA = str_to_title(toupper(CCA))
    )
  
  map_data <- cca_sf %>%
    left_join(le2024, by = "CCA")
  
  map_data <- map_data %>%
    mutate(
      LE_Group = cut(
        le,
        breaks = c(65, 70, 75, 80, 85, 90),
        include.lowest = TRUE,
        labels = c(
          "65 to 70 Years",
          "70 to 75 Years",
          "75 to 80 Years",
          '80 to 85 Years',
          "85 to 90 Years"
        )
      )
    )

  map_data <- subset(map_data, CCA != "Ohare")
  cca_sf <- subset(cca_sf, CCA!="Ohare")
  
  focus <- map_data %>%
    filter(CCA == cca)
  
  ggplot() +
    
    geom_sf(
      data = map_data,
      aes(fill = LE_Group),
      color = "white",
      linewidth = 0.2
    ) +
    
    geom_sf(
      data = cca_sf,
      fill = NA,
      color = "black",
      linewidth = 0.25
    ) +
    
    geom_sf(
      data = focus,
      fill = NA,
      color = "#D50032",
      linewidth = 1.2
    ) +
    
    scale_fill_brewer(
      palette = "YlGnBu",
      name = "2024 Life Expectancy",
      na.translate=FALSE
    ) +
    
    theme_void() +
    
    theme(
      legend.position = "right",
      legend.box.spacing = unit(-0.5, "cm"),
      legend.spacing.y = unit(0.2, "cm"),
      legend.title = element_text(
        face = "bold",
        size = 12
      ),
      legend.text = element_text(
        size = 12
      ),
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 16
      ),
      legend.key.size = unit(0.5,"cm")
    )
}

plot_cca_zoom <- function(cca){
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(patchwork)
  library(maptiles)
  library(ggspatial)
  library(terra)
  
  cca_sf <- st_read(
    here::here(
      "spatial",
      "Chicago_CCA.shp"
    ),
    quiet = TRUE
  )
  cca_sf <- cca_sf %>%
    rename("CCA" = "community")
  
  cca_sf <- cca_sf %>%
    dplyr::mutate(
      CCA = str_to_title(toupper(CCA))
    )

  focus <- cca_sf %>%
    filter(CCA == cca)
  
  pl <- st_as_sf(pl, coords = c("Longitude", "Latitude"), crs = 4326)
  pl <- st_transform(pl, crs = st_crs(focus))
  find <- st_intersects(pl, focus, sparse = FALSE)
  pointsinside <- apply(find, 1, any)
  pl <- pl[pointsinside, ]
  
  fqhc <- pl %>%
    filter(Name == "FQHC")
  
  hosp <- pl %>%
    filter(Name == "Hospitals")
  
  grocery <- pl %>%
    filter(Name == "Grocery Stores")
  
  bbox <- st_bbox(focus)
  
  bbox_poly <- st_as_sfc(st_bbox(focus))
  outside <- st_difference(
    bbox_poly, st_union(focus)
  )
  
tiles <- get_tiles(
  focus,
  provider = "OpenStreetMap.HOT",
  zoom = 14
)

tiles_masked <- mask(
  crop(tiles, vect(focus)), vect(focus)
)

  ggplot() +
    layer_spatial(tiles_masked) +
    
    geom_sf(
      data = focus,
      fill = NA,
      alpha = 0.2,
      color = "black",
      linewidth = 1
    ) +

    theme_void() +
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      )
    )  +
    geom_sf(data = fqhc, aes(color = "FQHCs"), size = 3,
            key_glyph = draw_key_point) +
    geom_sf(data=hosp, aes(color = "Hospitals"), size = 3,
            key_glyph = draw_key_point) +
    geom_sf(data=grocery, aes(color = "Grocery Stores"), size = 3,
            key_glyph = draw_key_point) +
    labs(color = "Services") +
    scale_color_manual(values = c("FQHCs" = "blue", "Hospitals" = "forestgreen", "Grocery Stores" = "red"), 
                       limits = c("FQHCs", "Hospitals", "Grocery Stores"), drop=FALSE) +
    
    theme(
      legend.position = "right",
      legend.box.spacing = unit(0.5, "cm"),
      legend.title = element_text(
        face = "bold",
        size = 15
      ),
      legend.text = element_text(
        size = 15
      ),
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 18
      ),
      legend.key.size = unit(0.5,"cm")
    )
}
