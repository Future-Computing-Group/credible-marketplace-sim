suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
  library(patchwork)
})

## ── Shared plot utilities ─────────────────────────────────────────
## Common theme, palettes, and helper functions for all experiment plots.

## IEEE-ready theme (matches TSC paper style)
theme_ieee <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      text             = element_text(family = "serif"),
      plot.title       = element_text(face = "bold", size = base_size + 1),
      axis.title       = element_text(size = base_size),
      axis.text        = element_text(size = base_size - 1),
      legend.title     = element_text(size = base_size, face = "bold"),
      legend.text      = element_text(size = base_size - 1),
      legend.position  = "bottom",
      legend.key.size  = unit(1.2, "lines"),
      strip.text       = element_text(face = "bold", size = base_size),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
      plot.margin      = margin(5, 8, 5, 5)
    )
}


## ── Colour palettes ──────────────────────────────────────────────

# Operator strategies (extended with ghost_bidder, keyed by raw and label)
palette_operator <- c(
  truthful      = "#2C7BB6",
  misreporter   = "#D7191C",
  inflator      = "#FDAE61",
  discriminator = "#ABD9E9",
  ghost_bidder  = "#7B3294",
  Truthful              = "#2C7BB6",
  `Capacity Misreporter` = "#D7191C",
  `Price Inflator`       = "#FDAE61",
  Discriminator          = "#ABD9E9",
  `Ghost Bidder`         = "#7B3294"
)

# Credibility mechanisms (keyed by raw and label values)
palette_credibility <- c(
  none       = "#D7191C",
  broadcast  = "#2C7BB6",
  blockchain = "#1A9641",
  exchange   = "#F4A460",
  regulatory = "#ABD9E9",
  None                = "#D7191C",
  Broadcast           = "#2C7BB6",
  Blockchain          = "#1A9641",
  `Neutral Exchange`  = "#F4A460",
  `Regulatory Audit`  = "#ABD9E9"
)

# DAG topologies (keyed by raw and label values)
palette_topology <- c(
  tree      = "#2C7BB6",
  sp        = "#FDAE61",
  entangled = "#D7191C",
  Tree              = "#2C7BB6",
  `Series-Parallel` = "#FDAE61",
  Entangled         = "#D7191C"
)

# Integrator count
palette_k <- c(
  "1" = "#D7191C",
  "2" = "#FDAE61",
  "3" = "#ABD9E9",
  "5" = "#2C7BB6"
)

# Fee modes (Exp F)
palette_fee <- c(
  stake     = "#D7191C",
  separated = "#1A9641"
)


## ── Label formatters ─────────────────────────────────────────────

label_topology <- c(
  tree      = "Tree",
  sp        = "Series-Parallel",
  entangled = "Entangled"
)

label_operator <- c(
  truthful      = "Truthful",
  misreporter   = "Capacity Misreporter",
  inflator      = "Price Inflator",
  discriminator = "Discriminator",
  ghost_bidder  = "Ghost Bidder"
)

label_credibility <- c(
  none       = "None",
  broadcast  = "Broadcast",
  blockchain = "Blockchain",
  exchange   = "Neutral Exchange",
  regulatory = "Regulatory Audit"
)

label_strategy <- c(
  competitive = "Competitive",
  collusive   = "Collusive"
)


## ── Figure saving helper ─────────────────────────────────────────

save_fig <- function(p, filename, width = 3.5, height = 2.8, dpi = 300) {
  dir.create("fig", showWarnings = FALSE, recursive = TRUE)
  ggsave(
    filename = file.path("fig", filename),
    plot     = p,
    width    = width,
    height   = height,
    dpi      = dpi,
    device   = "pdf"
  )
  p
}
