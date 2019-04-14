
# Common libraries----------------------------------------------------------------------

library(tidyverse)
library(plotly)
library(httr) # For http access

# Common functions -------------------------------------------------------------

ggplot_ptc <- function(plot, tip) {
  ggplotly(plot, session = "knitr", tooltip = tip,
           height = 500, width = 1000) %>%
    plotly::config(displayModeBar = FALSE)
}

theme_ptc <- function(base_size = 10, base_family = "sans") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      legend.key = element_blank(),
      strip.background = element_blank(),
      plot.title = element_text(size = 12, colour = "#1380A1", face = "bold"),
      axis.title = element_text(face = "italic")
    )
}

lay_out <- function(...) {
  dev.off()
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(
      layout.pos.row = x[[i]][[2]],
      layout.pos.col = x[[i]][[3]]
    ))
  }
}

pca_lookup <- function(postcode) {
  r <- content(GET(paste0("https://api.postcodes.io/postcodes/", URLencode(postcode), "/autocomplete")))
  LSOA <- NULL
  for (i in 1:length(r[[2]])) {
    LSOA[i] <- content(
      GET(
        paste0(
          "https://api.postcodes.io/postcodes?q=", URLencode(
            as.character(
              r[[2]][[i]]
            )
          )
        )
      )
    )[[2]][[1]]["lsoa"]
  }
  return(tibble(postcode, LSOA))
}

pc_lookup <- function(postcode, feature) {
  r <- content(GET(paste0("https://api.postcodes.io/postcodes/", postcode)))
  if (r[[1]] == 404) {
    return("Missing")
  }
  else if (feature == "lsoa") {
    return(r[[2]][["lsoa"]])
  }
  else if (feature == "long") {
    return(r[[2]][["longitude"]])
  }
  else if (feature == "lat") {
    return(r[[2]][["latitude"]])
  }
  else if (feature == "district") {
    return(r[[2]][["admin_district"]])
  }
  else if (feature == "county") {
    return(r[[2]][["admin_county"]])
  }
  else {
    return("Error")
  }
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if (missing(cex.cor)) cex.cor <- 0.8 / strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r * 2.5)
}
