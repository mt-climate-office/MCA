library(magrittr)

make_map_plot <- function(dat, shp, title_txt, hot=TRUE) {

  diverging <- any(dat$mean < 0)
  midpoint <- ifelse(diverging, 0, mean(dat$mean))

  if (hot && diverging) {
    pal <-  c('#fc8d59','#ffffbf','#91bfdb')
    pal <- ggplot2::scale_fill_gradient2(
      low = pal[1], mid = pal[2], high =  pal[3],
      midpoint = midpoint
    )
  } else if (hot && !diverging) {
    pal <- viridis::scale_fill_viridis(option = "magma")
  } else if (!hot && diverging) {
    pal <- c('#d8b365','#f5f5f5','#5ab4ac')
    pal <- ggplot2::scale_fill_gradient2(
      low = pal[1], mid = pal[2], high =  pal[3],
      midpoint = midpoint
    )
  } else {
    pal <- viridis::scale_fill_viridis(option = "viridis")
  }

  ggplot2::ggplot(dat) +
    ggplot2::geom_raster(ggplot2::aes(x=x, y=y, fill=mean), color="white") +
    ggplot2::geom_sf(data = shp, mapping = ggplot2::aes(), fill = NA, color="black") +
    ggplot2::facet_grid(rows = dplyr::vars(scenario), cols = dplyr::vars(period)) +
    pal +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.key.height = ggplot2::unit(0.10, "npc")
    ) +
    ggplot2::labs(y="", x = "", fill = "", title = title_txt)

}

shp <- sf::read_sf("https://data.climate.umt.edu/mca/fgb/mt.fgb")

terra::readRDS("./assets/cmip_tas.rds") %>%
  dplyr::mutate(diff = list(terra::rast(diff))) %>%
  dplyr::filter(scenario %in% c("ssp245", "ssp370"), period == "end_century") %>%
  dplyr::group_by(scenario, period) %>%
  dplyr::summarise(
    diff = list(terra::app(terra::rast(diff), fun="mean") %>%
                  terra::as.data.frame(xy=TRUE) %>%
                  sf::st_as_sf(coords = c("x", "y"), crs=4326) %>%
                  sf::st_crop(shp))
  ) %>%
  tidyr::unnest(diff) %>%
  make_map_plot(shp=shp, title_txt = "test", hot = TRUE)
