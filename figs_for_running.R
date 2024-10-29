library(magrittr)
library(ggplot2)

shp <- sf::read_sf("https://data.climate.umt.edu/mca/fgb/mt.fgb")

tas <- terra::readRDS("./assets/cmip_tas.rds") %>%
  dplyr::mutate(diff = list(terra::rast(diff))) %>%
  dplyr::filter(scenario %in% c("ssp245", "ssp370"), period == "end_century") %>%
  dplyr::group_by(scenario, period) %>%
  dplyr::summarise(
    diff = list(terra::app(terra::rast(diff), fun="mean") %>%
                  raster::raster() %>%
                  spex::qm_rasterToPolygons(rm.na = T) %>%
                  sf::`st_crs<-`(4326) %>%
                  sf::st_intersection(shp))
  ) %>%
  tidyr::unnest(diff) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(
    period = ifelse(
      period == "end_century",
      "Projected Change in End of Century\n(2071 - 2100) Average Temperature ",
      period
    ),
    scenario = dplyr::case_match(
      scenario,
      "ssp245" ~ "SSP2-4.5",
      "ssp370" ~ "SSP3-7.0"
    )
  ) %>%
  ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill=mean), color=NA) +
    ggplot2::geom_sf(data = shp, mapping = ggplot2::aes(), fill = NA, color="black") +
    ggplot2::facet_grid(rows = dplyr::vars(scenario), cols = dplyr::vars(period)) +
    scale_fill_distiller(palette = "Reds", direction = -1, trans = "reverse") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.key.height = ggplot2::unit(0.10, "npc"),
      strip.text = ggplot2::element_text(size=12, face="bold"),
      legend.title = ggplot2::element_text(hjust=0.5)
    ) +
    ggplot2::labs(y="", x = "", fill = "Temperature\nChange [degF]")


ggsave("~/data/figs_for_steve/avg_air_proj.png", tas, width=7, height = 7, bg = "white")

ppt <- terra::readRDS("./assets/cmip_pr.rds") %>%
  dplyr::mutate(diff = list(terra::rast(diff))) %>%
  dplyr::filter(scenario %in% c("ssp245", "ssp370"), period == "end_century") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    diff = list(terra::app(terra::unwrap(diff), fun=sum))
  ) %>%
  dplyr::group_by(scenario, period) %>%
  dplyr::summarise(
    diff = list(terra::app(terra::rast(diff), fun="mean") %>%
                  raster::raster() %>%
                  spex::qm_rasterToPolygons(rm.na = T) %>%
                  sf::`st_crs<-`(4326) %>%
                  sf::st_intersection(shp))
  ) %>%
  tidyr::unnest(diff) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(
    period = ifelse(
      period == "end_century",
      "Projected Change in End of Century\n(2071 - 2100) Annual Precipitation",
      period
    ),
    scenario = dplyr::case_match(
      scenario,
      "ssp245" ~ "SSP2-4.5",
      "ssp370" ~ "SSP3-7.0"
    )
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=mean), color=NA) +
  ggplot2::geom_sf(data = shp, mapping = ggplot2::aes(), fill = NA, color="black") +
  ggplot2::facet_grid(rows = dplyr::vars(scenario), cols = dplyr::vars(period)) +
  viridis::scale_fill_viridis(option = "viridis", direction = 1, trans = "reverse") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
    plot.title = ggplot2::element_text(hjust = 0.5),
    legend.key.height = ggplot2::unit(0.10, "npc"),
    strip.text = ggplot2::element_text(size=12, face="bold"),
    legend.title = ggplot2::element_text(hjust=0.5)
  ) +
  ggplot2::labs(y="", x = "", fill = "Precipitation\nChange [in]")


ggsave("~/data/figs_for_steve/avg_ppt_proj.png", ppt, width=7, height = 7, bg = "white")


ff <- terra::readRDS("./assets/cmip_freeze-free.rds") %>%
  dplyr::mutate(diff = list(terra::rast(diff))) %>%
  dplyr::filter(scenario %in% c("ssp245", "ssp370"), period == "end_century") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    diff = list(terra::app(terra::unwrap(diff), fun=sum))
  ) %>%
  dplyr::group_by(scenario, period) %>%
  dplyr::summarise(
    diff = list(terra::app(terra::rast(diff), fun="mean") %>%
                  raster::raster() %>%
                  spex::qm_rasterToPolygons(rm.na = T) %>%
                  sf::`st_crs<-`(4326) %>%
                  sf::st_intersection(shp))
  ) %>%
  tidyr::unnest(diff) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(
    period = ifelse(
      period == "end_century",
      "Projected Change in End of Century\n(2071 - 2100) Freeze-Free Days",
      period
    ),
    scenario = dplyr::case_match(
      scenario,
      "ssp245" ~ "SSP2-4.5",
      "ssp370" ~ "SSP3-7.0"
    )
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=mean), color=NA) +
  ggplot2::geom_sf(data = shp, mapping = ggplot2::aes(), fill = NA, color="black") +
  ggplot2::facet_grid(rows = dplyr::vars(scenario), cols = dplyr::vars(period)) +
  viridis::scale_fill_viridis(option = "magma", direction = -1, trans = "reverse") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
    plot.title = ggplot2::element_text(hjust = 0.5),
    legend.key.height = ggplot2::unit(0.10, "npc"),
    strip.text = ggplot2::element_text(size=12, face="bold"),
    legend.title = ggplot2::element_text(hjust=0.5)
  ) +
  ggplot2::labs(y="", x = "", fill = "Number\nof Days")


ggsave("~/data/figs_for_steve/avg_ff_proj.png", ff, width=7, height = 7, bg = "white")


abv90 <- terra::readRDS("./assets/cmip_above90.rds") %>%
  dplyr::mutate(diff = list(terra::rast(diff))) %>%
  dplyr::filter(scenario %in% c("ssp245", "ssp370"), period == "end_century") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    diff = list(terra::app(terra::unwrap(diff), fun=sum))
  ) %>%
  dplyr::group_by(scenario, period) %>%
  dplyr::summarise(
    diff = list(terra::app(terra::rast(diff), fun="mean") %>%
                  raster::raster() %>%
                  spex::qm_rasterToPolygons(rm.na = T) %>%
                  sf::`st_crs<-`(4326) %>%
                  sf::st_intersection(shp))
  ) %>%
  tidyr::unnest(diff) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(
    period = ifelse(
      period == "end_century",
      "Projected Change in End of Century\n(2071 - 2100) Days Above 90°F",
      period
    ),
    scenario = dplyr::case_match(
      scenario,
      "ssp245" ~ "SSP2-4.5",
      "ssp370" ~ "SSP3-7.0"
    )
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=mean), color=NA) +
  ggplot2::geom_sf(data = shp, mapping = ggplot2::aes(), fill = NA, color="black") +
  ggplot2::facet_grid(rows = dplyr::vars(scenario), cols = dplyr::vars(period)) +
  viridis::scale_fill_viridis(option = "magma", direction = -1, trans = "reverse") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
    plot.title = ggplot2::element_text(hjust = 0.5),
    legend.key.height = ggplot2::unit(0.10, "npc"),
    strip.text = ggplot2::element_text(size=12, face="bold"),
    legend.title = ggplot2::element_text(hjust=0.5)
  ) +
  ggplot2::labs(y="", x = "", fill = "Number\nof Days")


ggsave("~/data/figs_for_steve/avg_abv90_proj.png", abv90, width=7, height = 7, bg = "white")
