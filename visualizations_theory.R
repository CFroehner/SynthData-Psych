# SynthData-Psych
# Cosima Fröhner

# Create folder for plots and tables in thesis
if (!dir.exists("Plots_Tables")) {
  dir.create("Plots_Tables", recursive = TRUE)
}

# Stepwise KL Divergence --------------------------------------------------

# Load # human base and synthetic data
dat_full <- read_csv("Data/HumanBaseSynthetic.csv")

# Define Item Sets
original_colnames_HH_W <- grep("^HH_W_", names(dat_full), value = TRUE)
llm_colnames_HH_W <- gsub("^HH_W_", "LLM_HH_W_", original_colnames_HH_W)

original_cols_HH_W <- dat_full[original_colnames_HH_W]
llm_cols_HH_W <- dat_full[llm_colnames_HH_W]

# Compute mean trait scores for Honesty-Humility at Work
dat_full$person_score_HH_W <- rowMeans(original_cols_HH_W)
dat_full$llm_score_HH_W <- rowMeans(llm_cols_HH_W)

# Stepwise KL Divergence --------------------------------------------------

# Define score columns for KL divergence
# Example Honesty-Humility at Work (Trait Score)
person_col <- "person_score_HH_W"
llm_col    <- "llm_score_HH_W"
epsilon <- 1e-10

# Extract and rename relevant columns
scores <- dat_full %>%
  dplyr::select(all_of(c(person_col, llm_col))) %>%
  rename(Person = person_col, LLM = llm_col)

# Compute density estimates over common support
dens_p <- density(scores$Person, from = 1, to = 5.5)
dens_q <- density(scores$LLM, from = 1, to = 5.5)

# Interpolate LLM density to match grid of Human density
density_df <- data.frame(
  y = dens_p$x,
  p_y = dens_p$y,
  q_y = approx(dens_q$x, dens_q$y, xout = dens_p$x, rule = 2)$y
) %>%
  dplyr::mutate(log_ratio = log((p_y + epsilon) / (q_y + epsilon)),
                kl_contrib = p_y * log_ratio)

# Prepare long-format data for Plot 1 (density lines)
density_long <- density_df %>%
  dplyr::select(y, p_y, q_y) %>%
  rename(Human = p_y, LLM = q_y) %>%
  pivot_longer(cols = c("Human", "LLM"), names_to = "Source", 
               values_to = "Density")

# Compute plot-specific y-axis ranges
y_range_p1 <- range(density_long$Density, na.rm = TRUE)
y_range_p2 <- range(density_df$log_ratio, na.rm = TRUE)
y_range_p3 <- range(density_df$kl_contrib, na.rm = TRUE)
y_range_p2 <- c(min(y_range_p2[1], -0.5), max(y_range_p2[2], 0.5))
y_range_p3 <- c(min(y_range_p3[1], -0.05), max(y_range_p3[2], 0.2))

# Plot 1: Density comparison
p1 <- ggplot(density_long, aes(x = y, y = Density, color = Source)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey30") +
  scale_color_manual(values = c("Human" = "purple", "LLM" = "orange")) +
  scale_linetype_manual(values = c("Human" = "solid", "LLM" = "dashed")) +
  labs(y = "Density", x = "") +
  ylim(c(0, max(y_range_p1))) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9, 0.83),
    legend.background = element_blank(),
    legend.key.size = unit(1.7, "lines"),
    legend.text = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

# Plot 2: Log-ratio log(p(y)/q(y))
p2 <- ggplot(density_df, aes(x = y, y = log_ratio)) +
  geom_line(color = "black", size = 1) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey30") +
  labs(y = "log(p(y)/q(y))", x = "") +
  ylim(y_range_p2) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

# Plot 3: KL divergence
p3 <- ggplot(density_df, aes(x = y)) +
  geom_area(aes(y = kl_contrib, fill = "KL Divergence\nContribution"), 
            alpha = 0.4, show.legend = TRUE) +
  geom_line(aes(y = kl_contrib), color = "black", size = 1) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey30") +
  scale_fill_manual(values = c("KL Divergence\nContribution" = "grey60")) +
  labs(
    y = "p(y)*log(p(y)/q(y))",
    x = "Trait Score",
    fill = NULL
  ) +
  ylim(y_range_p3) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = c(0.9, 0.83),
    axis.title.x = element_text(margin = ggplot2::margin(t = 5)),
    axis.title.y = element_text(margin = ggplot2::margin(t = 5)),
    legend.text = element_text(size = 14)
  )

# Combine plots
final_plot <- p1 / p2 / p3 +
  plot_layout(heights = c(1.2, 1, 1)) &
  theme(plot.margin = unit(c(10, 10, 10, 10), "pt"))

# View and save
print(final_plot)
ggsave("Plots_Tables/KL-Demo-HH-W-aligned.png",
       width = 8, height = 6, dpi = 300)

## Wasserstein vs KL-Divergence --------------------------------------------

# Example distributions
dist1 <- c(0.6, 0.1, 0.1, 0.1, 0.1) # reference distribution
dist2 <- c(0.1, 0.1, 0.1, 0.1, 0.6)
dist3 <- c(0.1, 0.6, 0.1, 0.1, 0.1)

# Comparison: dist1 vs dist2
kl_1 <- KL(rbind(dist1, dist2), unit = "log2")
wass_1 <- wasserstein1d(1:5, 1:5, dist1, dist2, p = 1)

df1 <- data.frame(
  Response_Score = factor(rep(1:5, 2)),
  Probability = c(dist1, dist2),
  Group = rep(c("A", "B"), each = 5)
)

p1 <- ggplot(df1, aes(x = Response_Score, y = Probability, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("A" = "#4D4D4D", "B" = "#B3B3B3")) +
  labs(
    title = paste("KL Divergence:", round(kl_1, 2), 
                  "| Wasserstein:", round(wass_1, 2)),
    x = "Response Score",
    y = "Probability"
  ) +
  ylim(0, 0.6) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14))

# Comparison: dist1 vs dist3
kl_2 <- KL(rbind(dist1, dist3), unit = "log2")
wass_2 <- wasserstein1d(1:5, 1:5, dist1, dist3, p = 1)

df2 <- data.frame(
  Response_Score = factor(rep(1:5, 2)),
  Probability = c(dist1, dist3),
  Group = rep(c("A", "B"), each = 5)
)

p2 <- ggplot(df2, aes(x = Response_Score, y = Probability, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("A" = "#4D4D4D", "B" = "#B3B3B3")) +
  labs(
    title = paste("KL Divergence:", round(kl_2, 2), 
                  "| Wasserstein:", round(wass_2, 2)),
    x = "Response Score",
    y = "Probability"
  ) +
  ylim(0, 0.6) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14))

# Combine plots
combined_plot <- grid.arrange(p1, p2, ncol = 1)

# Save
ggsave(
  filename = "Plots_Tables/Comparison-KL-WS.png",
  plot = combined_plot,
  width = 8,
  height = 6,
  dpi = 300
)

