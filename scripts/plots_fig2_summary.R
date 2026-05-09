## ── Fig. 2 (manuscript): CoNC summary — closure + knife-edge ───────────
##
## Two-panel cross-column figure for IEEE TSC manuscript.
##   (a) Ghost-bid net surplus per round across credibility mechanisms
##       (None / Broadcast / Blockchain / Domain Separation)
##       — closure narrative; data: expE_summary, n_agents = 50
##   (b) Domain-separation knife-edge: surplus vs ownership-stake φ
##       on log axis; vertical at φ = 0.01 marks "1% breaks guarantee"
##       — data: expL_summary, ghost_bidder operator
##
## Visual language (verified against TSC Vol 19 No 2 exemplars):
##   - sans-serif in-figure text
##   - closed-box spines (no Tufte minimal)
##   - light grey gridlines
##   - patchwork tag_levels = "a" → "(a)" "(b)" top-left
##   - single-accent palette (vermillion None vs bluish-green mechanisms)
##   - figure dimensions 7.16 in × 2.5 in (IEEE TSC text width)

suppressPackageStartupMessages({
  library(targets)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

SIM_DIR <- "/Users/lloven/Dropbox/Research/workspace/2 Papers and Manuscripts/2026 Trustworthy Marketplace Architecture for Agents/src/simulation"
TARGETS_STORE <- file.path(SIM_DIR, "_targets")
OUT_PDF <- file.path(SIM_DIR, "figs", "fig2_conc_summary.pdf")

## ─── Palette (Okabe–Ito subset) ─────────────────────────────────────────
COL_VERMILLION <- "#D55E00"   # baseline / "the gap"
COL_GREEN      <- "#009E73"   # closure mechanisms
COL_BLUE       <- "#0072B2"   # knife-edge curve
COL_GREY       <- "#999999"   # reference lines

## ─── Load data ───────────────────────────────────────────────────────────
tar_load(expE_summary, store = TARGETS_STORE)
tar_load(expL_summary, store = TARGETS_STORE)

## ─── Panel (a): closure across mechanisms ───────────────────────────────
mech_levels <- c("none", "broadcast", "blockchain", "exchange")
mech_labels <- c("None", "Broadcast", "Blockchain", "Domain Sep.")

panel_a_df <- expE_summary %>%
  filter(n_agents_cond == max(n_agents_cond)) %>%      # n = 50
  group_by(credibility) %>%
  summarise(
    surplus_mean = mean(ghost_surplus_mean),
    surplus_se   = sd(ghost_surplus_mean) / sqrt(n()),  # SE across topologies
    .groups = "drop"
  ) %>%
  mutate(
    surplus_lo  = surplus_mean - 1.96 * surplus_se,
    surplus_hi  = surplus_mean + 1.96 * surplus_se,
    credibility = factor(credibility, levels = mech_levels, labels = mech_labels),
    is_baseline = credibility == "None"
  )

p_a <- ggplot(panel_a_df,
              aes(x = credibility, y = surplus_mean, fill = is_baseline)) +
  geom_col(width = 0.65, colour = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = surplus_lo, ymax = surplus_hi),
                width = 0.20, linewidth = 0.4) +
  geom_hline(yintercept = 0, colour = COL_GREY,
             linetype = "dashed", linewidth = 0.4) +
  scale_fill_manual(values = c(`TRUE` = COL_VERMILLION,
                               `FALSE` = COL_GREEN),
                    guide = "none") +
  labs(x = NULL,
       y = "Ghost-bid net surplus / round") +
  theme_bw(base_size = 8, base_family = "Helvetica") +
  theme(
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y  = element_line(colour = "grey90", linewidth = 0.3),
    panel.border        = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    axis.text.x         = element_text(size = 8, colour = "black"),
    axis.text.y         = element_text(size = 7, colour = "black"),
    axis.title          = element_text(size = 8),
    plot.margin         = margin(2, 4, 2, 2)
  )

## ─── Panel (b): knife-edge (ghost_bidder, log-x) ────────────────────────
panel_b_df <- expL_summary %>%
  filter(operator_type == "ghost_bidder", stake_fraction > 0) %>%
  group_by(stake_fraction) %>%
  summarise(
    s_mean = mean(surplus_mean),
    s_se   = if (n() > 1) sd(surplus_mean) / sqrt(n()) else 0,
    .groups = "drop"
  ) %>%
  mutate(
    surplus_mean = s_mean,
    surplus_lo   = s_mean - 1.96 * s_se,
    surplus_hi   = s_mean + 1.96 * s_se
  )

p_b <- ggplot(panel_b_df,
              aes(x = stake_fraction, y = surplus_mean)) +
  geom_ribbon(aes(ymin = surplus_lo, ymax = surplus_hi),
              fill = COL_BLUE, alpha = 0.20) +
  geom_line(colour = COL_BLUE, linewidth = 0.7) +
  geom_point(colour = COL_BLUE, size = 1.3) +
  geom_hline(yintercept = 0, colour = COL_GREY,
             linetype = "dashed", linewidth = 0.4) +
  geom_vline(xintercept = 0.01, colour = COL_VERMILLION,
             linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = 0.011, y = max(panel_b_df$surplus_hi, na.rm = TRUE) * 0.95,
           label = "1% knife-edge", colour = COL_VERMILLION,
           hjust = 0, vjust = 1, size = 2.4, family = "Helvetica",
           fontface = "italic") +
  scale_x_log10(breaks   = c(0.01, 0.05, 0.1, 0.25, 0.5, 1.0),
                labels   = c("0.01", "0.05", "0.1", "0.25", "0.5", "1.0"),
                expand   = expansion(mult = c(0.02, 0.02))) +
  labs(x = expression("Ownership stake " * phi * " (log scale)"),
       y = "Ghost-bid surplus / round") +
  theme_bw(base_size = 8, base_family = "Helvetica") +
  theme(
    panel.grid.minor    = element_blank(),
    panel.grid.major    = element_line(colour = "grey90", linewidth = 0.3),
    panel.border        = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    axis.text           = element_text(size = 7, colour = "black"),
    axis.title          = element_text(size = 8),
    plot.margin         = margin(2, 4, 2, 2)
  )

## ─── Combine and save ────────────────────────────────────────────────────
fig2 <- (p_a | p_b) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag          = element_text(size = 9, face = "bold",
                                         family = "Helvetica"),
        plot.tag.position = c(0.02, 0.97))

dir.create(dirname(OUT_PDF), showWarnings = FALSE, recursive = TRUE)
ggsave(OUT_PDF, fig2,
       width = 7.16, height = 2.0, units = "in")

cat("Wrote", OUT_PDF, "\n")
cat("Panel (a) data:\n"); print(panel_a_df)
cat("\nPanel (b) data:\n"); print(panel_b_df)
