# -----------------------------------------------------------------------
# Project: NicheCompleteness
# File name: fig-2.R
# Last updated: 2023-02-23
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/NicheCompleteness
# Load libraries --------------------------------------------------------
library(ggplot2)
# Set-up ----------------------------------------------------------------
# Colours for plotting
cols <- RColorBrewer::brewer.pal(n = 5, name = "Blues")
# Generate figure -------------------------------------------------------
# Define matrix
ggplot() +
  geom_rect(aes(xmin = c(0, 1), xmax = c(1, 0), ymin = c(0, 0), ymax = c(1, 1)), colour = "black", fill = cols[4]) +
  geom_rect(aes(xmin = c(0, 1), xmax = c(1, 0), ymin = c(1, 1), ymax = c(2, 2)), colour = "black", fill = cols[2]) +
  geom_rect(aes(xmin = c(1, 2), xmax = c(2, 1), ymin = c(0, 0), ymax = c(1, 1)), colour = "black", fill = cols[2]) +
  geom_rect(aes(xmin = c(1, 2), xmax = c(2, 1), ymin = c(1, 1), ymax = c(2, 2)), colour = "black", fill = cols[4]) +
  geom_text(aes(x = 0.5, y = 1.5, label = "True Postive (TP)"), fontface = "bold", size = 5) +
  geom_text(aes(x = 1.5, y = 1.5, label = "False Postive (FP)"), fontface = "bold", colour = "white", size = 5) +
  geom_text(aes(x = 0.5, y = 0.5, label = "False Negative (FN)"), fontface = "bold", colour = "white", size = 5) +
  geom_text(aes(x = 1.5, y = 0.5, label = "True Negative (TN)"), fontface = "bold", size = 5) +
  geom_text(aes(x = 1.5, y = 1.4, label = "Type I Error"), colour = "white") +
  geom_text(aes(x = 0.5, y = 0.4, label = "Type II Error"), colour = "white") +
  annotate("text", x = -0.1, y = 1.5, label = "Negative (-)", angle = 90, fontface = "bold", size = 5) +
  annotate("text", x = -0.1, y = 0.5, label = "Positive (+)", angle = 90, fontface = "bold", size = 5) +
  annotate("text", x = 1.5, y = 2.1, label = "Negative (-)", angle = 0, fontface = "bold", size = 5) +
  annotate("text", x = 0.5, y = 2.1, label = "Positive (+)", angle = 0, fontface = "bold", size = 5) +
  annotate("text", x = 1, y = 2.2, label = "Actual", angle = 0, fontface = "bold", size = 5) +
  annotate("text", x = -0.2, y = 1, label = "Predicted", angle = 90, fontface = "bold", size = 5) +
  theme_void() +
  theme(plot.background = element_rect(colour = NA, fill = "white"),
        panel.background = element_blank())

# Save
ggsave("./figures/fig-2.png", height = 100, width = 100, units = "mm", dpi = 600, scale = 1.5)
