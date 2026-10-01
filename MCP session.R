# CRAN release
library(tidyverse)
library(mcptools)
mcp_session()

lijst <- c(1,2,3,4,5)

# ---- Scatterplot: hoogte vs. bosbedekking (crossbill-dataset) ----
library(unmarked)
library(ggplot2)

data(crossbill)

p <- ggplot(crossbill, aes(x = ele, y = forest)) +
  geom_point(color = "#2a78d6", size = 2.4, alpha = 0.85) +
  labs(
    title = "Hoogte vs. bosbedekking per telpunt",
    subtitle = "Crossbill-dataset (unmarked), 267 telpunten",
    x = "Hoogte (m)",
    y = "Bosbedekking (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(color = "#0b0b0b", face = "bold"),
    plot.subtitle = element_text(color = "#52514e"),
    axis.title = element_text(color = "#52514e"),
    axis.text = element_text(color = "#898781"),
    panel.grid.major = element_line(color = "#e1e0d9", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#c3c2b7"),
    plot.background = element_rect(fill = "#fcfcfb", color = NA),
    panel.background = element_rect(fill = "#fcfcfb", color = NA)
  )

print(p)

ggsave("crossbill_hoogte_vs_bos.png", plot = p, width = 7.5, height = 5.5, dpi = 150, bg = "#fcfcfb")

cat("Correlatie (Pearson) hoogte-bos:", round(cor(crossbill$ele, crossbill$forest, use = "complete.obs"), 3), "\n")