# SynthData-Psych
# Cosima Fröhner

# Load Packages & Functions -----------------------------------------------
packages <- c(
  "dplyr", "tidyverse", "corrplot", "psych", 
  "scipub", "ggplot2", "tibble", "transport", "randomForest", "mirt",
  "lavaan", "paran", "stargazer", "philentropy", "patchwork", "gridExtra", 
  "xtable", "dad"
)
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}
invisible(lapply(packages, library, character.only = TRUE))

tmp_env <- new.env()
set.seed(42)

# Load functions
source("functions.R")

# Create folder for plots and tables in thesis
if (!dir.exists("Plots_Tables")) {
  dir.create("Plots_Tables", recursive = TRUE)
}

# Load & Preprocess Data --------------------------------------------------
# human base data and synthetic data columns
dat_full <- read_csv("Data/HumanBaseSynthetic.csv") # human base & synth. data
original_cols_new <- read_csv("Data/ComparisonData.csv") # comparison data

## Define Item Sets --------------------------------------------------------

original_colnames_C_W <- grep("^C_W_", names(dat_full), value = TRUE)
llm_colnames_C_W <- gsub("^C_W_", "LLM_C_W_", original_colnames_C_W)

original_colnames_HH_W <- grep("^HH_W_", names(dat_full), value = TRUE)
llm_colnames_HH_W <- gsub("^HH_W_", "LLM_HH_W_", original_colnames_HH_W)

original_colnames <- c(original_colnames_C_W, original_colnames_HH_W)
llm_colnames <- c(llm_colnames_C_W, llm_colnames_HH_W)

original_cols_C_W <- dat_full[original_colnames_C_W]
llm_cols_C_W <- dat_full[llm_colnames_C_W]

original_cols_HH_W <- dat_full[original_colnames_HH_W]
llm_cols_HH_W <- dat_full[llm_colnames_HH_W]

original_cols <- dat_full[original_colnames]
llm_cols <- dat_full[llm_colnames]

# Trait Score
dat_full$person_score_C_W <- rowMeans(original_cols_C_W)
dat_full$llm_score_C_W <- rowMeans(llm_cols_C_W)

dat_full$person_score_HH_W <- rowMeans(original_cols_HH_W)
dat_full$llm_score_HH_W <- rowMeans(llm_cols_HH_W)

dat_extended <- cbind(
  dat_full, original_cols_new) # human base, human comparison, synthetic data

dat_extended$person_score_new_C_W <- rowMeans(
  original_cols_new[grep("^C_W_", names(original_cols_new), value = TRUE)])
dat_extended$person_score_new_HH_W <- rowMeans(
  original_cols_new[grep("^HH_W_", names(original_cols_new), value = TRUE)])

# Datasets Random Forest --------------------------------------------------
# Combined dataset for random forest training (human and synthetic)
original_cols_source <- original_cols %>%
  mutate(source = rep(0, nrow(original_cols))) # label human base data 0

llm_cols_source <- llm_cols %>%
  rename_with(~ sub("^LLM_", "", .x), starts_with("LLM_")) %>%
  mutate(source = rep(1, nrow(llm_cols))) # label synth. data: 1

dat_mixed <- rbind(original_cols_source, llm_cols_source) %>%
  mutate(source = as.factor(source))

traits <- c("C_W", "HH_W")

# Combined dataset for random forest training (human base and human comparison)
original_cols_new_source <- original_cols_new %>%
  rename_with( ~ sub("new_", "", .x), contains("new"))%>%
  dplyr::mutate(source = rep(2, nrow(llm_cols))) # label human comp. data: 2
  
original_colnames_new <- original_cols_new_source %>%
  dplyr::select(!contains("score")) %>%
  dplyr::select(!contains("source")) %>%
  names(.)

dat_mixed2 <- rbind(original_cols_source, original_cols_new_source)
dat_mixed2$source <- factor(as.integer(dat_mixed2$source), levels = c(0, 2))

# Initial Check -----------------------------------------------------------

## Mean and SD per Item -------------------------------------------------

# Basic statistics for each item
item_means <- colMeans(dat_extended, na.rm = TRUE)
item_medians <- apply(dat_extended, 2, median, na.rm = TRUE)
item_sds <- apply(dat_extended, 2, sd, na.rm = TRUE)

# Build summary table
summary_table <- data.frame(
  Item = names(item_means),
  Mean = round(item_means, 2),
  Median = item_medians,
  SD = round(item_sds, 2)
) %>%
  dplyr::filter(!grepl("score", Item)) %>%
  dplyr::mutate(
    Item = case_when(
      Item == "person_score_HH_W" ~ "score_HH_W",
      Item == "person_score_C_W" ~ "score_C_W",
      Item == "person_score_new_HH_W" ~ "score_new_HH_W",
      Item == "person_score_new_C_W" ~ "score_new_C_W",
      TRUE ~ Item
    ),
    BaseItem = Item %>%
      sub("^LLM_", "", .) %>%
      sub("_new", "", .),
    Source = case_when(
      str_starts(Item, regex("LLM_", ignore_case = TRUE)) ~ "LLM",
      str_detect(Item, regex("new", ignore_case = TRUE)) ~ "Human2",
      TRUE ~ "Human"
    )
  )

# Reshape into wide format for comparison
comparison_table <- summary_table %>%
  dplyr::select(BaseItem, Source, Mean, SD) %>%
  pivot_wider(
    names_from = Source,
    values_from = c(Mean, SD),
    names_sep = "_"
  ) %>%
  dplyr::filter(!is.na(Mean_Human) & !is.na(Mean_LLM)) %>%
  dplyr::mutate(
    SD_ratio_LLM_Human = SD_LLM / SD_Human,
    SD_ratio_Human_Human2 = SD_Human2 / SD_Human
  ) %>%
  dplyr::mutate(across(-BaseItem, ~ round(.x, 2))) 

# Rename columns for clarity
colnames(comparison_table) <- c(
  "BaseItem",
  "Mean (Human)",
  "Mean (LLM)",
  "Mean (Human Comp.)",
  "SD (Human)",
  "SD (LLM)",
  "SD (Human Comp.)",
  "SD Ratio (LLM vs Human)",
  "SD Ratio (Human Comp. vs Human)"
)

# Table of means and SDs for human base, human comparison, and synthetic data
comparison_table

### Final Comparison Table Human - LLM (latex) ------------------------------

escape_tex <- function(x) gsub("_", "\\\\_", x)

comparison_CW <- comparison_table %>%
  dplyr::filter(grepl("^C_W", BaseItem) | grepl("^score_C_W", BaseItem)) %>%
  dplyr::select(
    BaseItem,
    `Mean (LLM)`,
    `Mean (Human)`,
    `SD (LLM)`,
    `SD (Human)`,
    `SD Ratio (LLM vs Human)`
  )

comparison_HHW <- comparison_table %>%
  dplyr::filter(grepl("^HH_W", BaseItem) | grepl("^score_HH_W", BaseItem)) %>%
  dplyr::select(
    BaseItem,
    `Mean (LLM)`,
    `Mean (Human)`,
    `SD (LLM)`,
    `SD (Human)`,
    `SD Ratio (LLM vs Human)`
  )

# Output function
write_latex_table_LLM <- function(df, file_name, caption_text, label_name) {
  xtab <- xtable(df, digits = 2)
  tab_body <- capture.output(print(
    xtab,
    include.rownames = FALSE,
    include.colnames = FALSE,
    only.contents = TRUE,
    sanitize.text.function = identity,
    hline.after = NULL
  ))
  
  latex_code <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    sprintf("\\caption{%s}", caption_text),
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{lrrrrr}",
    "\\hline",
    "\\textbf{Item} & \\textbf{Mean} & \\textbf{Mean} & \\textbf{SD} & 
    \\textbf{SD} & \\textbf{SD Ratio} \\\\",
    "              & (LLM) & (Human) & (LLM) & (Human) & \\\\",
    "\\hline",
    tab_body,
    "\\hline",
    "\\end{tabular}%",
    "}",
    sprintf("\\label{%s}", label_name),
    "\\end{table}"
  )
  
  latex_code <- gsub("_", "\\\\_", latex_code)
  
  output_dir <- "Plots_Tables"
  writeLines(latex_code, file.path(output_dir, file_name))
}

# Save trait-specific comparison tables
write_latex_table_LLM(
  comparison_CW,
  "comparison_table_CW.tex",
  "Means and standard deviations (SD) of responses from LLM-generated and human 
  base data for items in the Conscientiousness at Work (C-W) item pool. The 
  final column reports the SD ratio (LLM / Human).",
  "tab:means-cw"
)

write_latex_table_LLM(
  comparison_HHW,
  "comparison_table_HHW.tex",
  "Means and standard deviations of responses from LLM-generated and human base
  data for items in the Honesty-Humility at Work (HH-W) item pool. The final 
  column reports the SD ratio (LLM / Human).",
  "tab:means-hhw"
)

### Final Comparison Table Human Base - Human Comparison (latex)  -----------

escape_tex <- function(x) gsub("_", "\\\\_", x)

comparison_CW_humancomp <- comparison_table %>%
  dplyr::filter(grepl("^C_W", BaseItem) | grepl("^score_C_W", BaseItem)) %>%
  dplyr::select(
    BaseItem,
    `Mean (Human Comp.)`,
    `Mean (Human)`,
    `SD (Human Comp.)`,
    `SD (Human)`,
    `SD Ratio (Human Comp. vs Human)`
  )

comparison_HHW_humancomp <- comparison_table %>%
  dplyr::filter(grepl("^HH_W", BaseItem) | grepl("^score_HH_W", BaseItem)) %>%
  dplyr::select(
    BaseItem,
    `Mean (Human Comp.)`,
    `Mean (Human)`,
    `SD (Human Comp.)`,
    `SD (Human)`,
    `SD Ratio (Human Comp. vs Human)`
  )

# Output function
write_latex_table_humancomp <- function(df, file_name, caption_text, label_name) {
  xtab <- xtable(df, digits = 2)
  tab_body <- capture.output(print(
    xtab,
    include.rownames = FALSE,
    include.colnames = FALSE,
    only.contents = TRUE,
    sanitize.text.function = identity,
    hline.after = NULL
  ))
  
  latex_code <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    sprintf("\\caption{%s}", caption_text),
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{lrrrrr}",
    "\\hline",
    "\\textbf{Item} & \\textbf{Mean} & \\textbf{Mean} & \\textbf{SD} & 
    \\textbf{SD} & \\textbf{SD Ratio} \\\\",
    "              & (Human & (Human) & (Human & (Human) & \\\\",
    "              & Comp.) &       & Comp.) &       & \\\\",
    "\\hline",
    tab_body,
    "\\hline",
    "\\end{tabular}%",
    "}",
    sprintf("\\label{%s}", label_name),
    "\\end{table}"
  )
  
  latex_code <- gsub("_", "\\\\_", latex_code)
  
  output_dir <- "Plots_Tables"
  writeLines(latex_code, file.path(output_dir, file_name))
}

# Save trait-specific comparison tables
write_latex_table_humancomp(
  comparison_CW_humancomp,
  "comparison_table_CW_human_humancomp.tex",
  "Means and standard deviations (SD) of responses from human comparison 
  (Human Comp.) and human base (Human) data for items in the Conscientiousness 
  at Work (C-W) item pool. The final column reports the SD ratio 
  (Human Comp. / Human).",
  "tab:means-cw-humancomp"
)

write_latex_table_humancomp(
  comparison_HHW_humancomp,
  "comparison_table_HHW_human_humancomp.tex",
  "Means and standard deviations (SD) of responses from human-comparison 
  (Human Comp.) and human base (Human) data for items in the Honesty-Humility 
  at Work (HH-W) item pool. The final column reports the SD ratio 
  (Human Comp. / Human).",
  "tab:means-hhw-humancomp"
)

## Trait Score Densities ---------------------------------------------------
density_plots <- list()
all_scores <- c()
all_densities <- c()

# Generate density plots for each trait
for (trait in traits) {
  
  # Construct column names
  person_col <- paste0("person_score_", trait)
  llm_col <- paste0("llm_score_", trait)
  person_col_new <- paste0("person_score_new_", trait)
  
  # Collect all scores
  scores <- dat_extended %>%
    dplyr::select(all_of(c(person_col, llm_col, person_col_new))) %>%
    unlist(use.names = FALSE)
  all_scores <- c(all_scores, scores)
  
  # Prepare long-form data for density estimation
  density_data <- dat_extended %>%
    dplyr::select(all_of(c(person_col, llm_col, person_col_new))) %>%
    pivot_longer(cols = everything(), 
                 names_to = "Source", values_to = "Score") %>%
    dplyr::mutate(Source = case_when(
      Source == person_col ~ "Human (Base)",
      Source == llm_col ~ "LLM",
      Source == person_col_new ~ "Human (Comparison)",
      TRUE ~ Source
    ))
  
  # Compute individual densities and track max y for consistent scaling
  for (src in unique(density_data$Source)) {
    d <- density(density_data$Score[density_data$Source == src], 
                 # bw = 0.15, 
                 na.rm = TRUE)
    all_densities <- c(all_densities, d$y)
  }
  
  # Set plot limits
  x_range <- range(all_scores, na.rm = TRUE)
  y_range <- c(0, max(all_densities, na.rm = TRUE))
  
  # Create density plot
  p <- ggplot(density_data, aes(x = Score, colour = Source)) +
    geom_line(stat = "density", bw = 0.15, size = 1) +
    scale_color_manual(values = c(
      "Human (Base)" = "purple",
      "Human (Comparison)" = "grey45", 
      "LLM" = "orange"))+
    labs(
      x = paste("Trait Score", gsub("_", "-", trait)),
      y = "Density"
    ) +
    xlim(1, 5.5) +
    ylim(0, 1.75) +
    theme_minimal(base_size = 14) +
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.key.size = unit(1.5, "lines")) +
    guides(colour = guide_legend(override.aes = list(size = 1.5)))
  
  density_plots[[trait]] <- p
}

# Combine and save plots
combined_plot <- density_plots[["C_W"]] + density_plots[["HH_W"]] +
plot_layout(guides = "collect") & 
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.margin = ggplot2::margin(t = 10)
  )

ggsave("Plots_Tables/trait_density.png",
       width = 8, height = 4, dpi = 300)

combined_plot

## Item Level --------------------------------------------------------------
# Note: The following warning message may appear:
# "Removed XX rows containing missing values or values outside the scale range 
# (`geom_col()`)"
# This refers to rows where response counts are zero (Count == 0), 
# which are not plotted as bars. The warning can be safely ignored.

for (trait in traits) {
  
  # Extract relevant columns for the trait
  llm_trait <- llm_cols %>%
    dplyr::select(starts_with(paste0("LLM_", trait)))
  
  human_trait <- original_cols %>%
    dplyr::select(starts_with(trait))
  
  # Align column names for merging
  colnames(llm_trait) <- sub("^LLM_", "", colnames(llm_trait))
  colnames(human_trait) <- colnames(llm_trait)
  
  # Convert to long format and label source
  llm_long <- llm_trait %>%
    pivot_longer(cols = everything(), names_to = "Item", values_to = "Value")%>%
    dplyr::mutate(Source = "LLM")
  
  human_long <- human_trait %>%
    pivot_longer(cols = everything(), names_to = "Item", values_to = "Value")%>%
    dplyr::mutate(Source = "Human")
  
  # Combine and summarize
  combined <- bind_rows(llm_long, human_long)
  
  combined_summary <- combined %>%
    group_by(Item, Value, Source) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(
      Value = as.numeric(as.character(Value)),
      x_pos = Value + ifelse(Source == "LLM", 0.15, -0.15),
      Item = gsub("^HH_W_|^C_W_", "", Item),
      Item = gsub("othersresponsibility", "others\nresponsibility", Item)
      )

  # Create item-level histogram plot
  p <- ggplot(combined_summary, aes(x = x_pos, y = Count, fill = Source)) +
    geom_col(width = 0.3, color = "black") +
    facet_wrap(~Item, scales = "fixed") +
    scale_fill_manual(values = c("Human" = "purple", "LLM" = "orange")) +
    scale_x_continuous(
      breaks = 1:5,
      labels = 1:5,
      limits = c(0.5, 5.5)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      axis.ticks.x = element_line(),
      axis.line.x = element_line(),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "center",
      axis.title = element_text(size = 14)
    ) +
    labs(
      x = "Response Score", 
      y = "Count", 
      fill = "Source"
    )
  
  # Print and save plot
  print(p)
  ggsave(filename = paste0("Plots_Tables/histogram_", trait, ".png"),
         plot = p,
         width = 8, height = 8, dpi = 300)
  
}

# Fidelity ----------------------------------------------------------------

## Bias and MSE ------------------------------------------------------------

### Item Level --------------------------------------------------------------

# Compute bias and MSE for each item
item_metrics <- mapply(function(llm_col, human_col) {
  diff      <- dat_extended[[llm_col]] - dat_extended[[human_col]]
  bias      <- mean(diff, na.rm = TRUE)
  # variance  <- var(diff, na.rm = TRUE)
  mse       <- mean(diff^2, na.rm = TRUE)
  c(Bias = bias, 
    MSE = mse)
}, llm_colnames, original_colnames)

# Create a data frame and format results
item_metrics_df <- as.data.frame(t(item_metrics))
item_metrics_df$Item <- original_colnames
item_metrics_df <- item_metrics_df %>%
  dplyr::select(Item, 
                Bias, 
                MSE)%>%
  dplyr::arrange(desc(MSE))


#### Final Bias, MSE Table (latex) -------------------------------------------
xtab <- xtable(
  item_metrics_df,
  digits = 3,
  caption = "Bias and mean squared error (MSE) for LLM 
  predictions compared to human ground truth, per item from the 
  Conscientiousness at Work (C-W) and Honesty-Humility at Work (HH-W) item pools. 
  Items are ordered in descending MSE.",
  label = "tab:bias"
)

tab_body <- capture.output(print(
  xtab,
  include.rownames = FALSE,
  include.colnames = FALSE,
  only.contents = TRUE,
  hline.after = NULL,
  sanitize.text.function = identity
))

latex_code <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Bias and mean squared error (MSE) for LLM 
  predictions compared to human ground truth, per item from the 
  Conscientiousness at Work (C-W) and Honesty-Humility at Work (HH-W) item pools. 
  Items are ordered in descending MSE.}",
  "\\begin{tabular}{lrr}",
  "  \\hline",
  "\\textbf{Item} & \\textbf{Bias} & \\textbf{MSE} \\\\",
  "  \\hline",
  tab_body,
  "  \\hline",
  "\\end{tabular}",
  "\\label{tab:bias}",
  "\\end{table}"
)

latex_code <- gsub("_", "\\\\_", latex_code)

writeLines(latex_code, "Plots_Tables/bias_variance_mse_table.tex")

#### Final MSE Visualisation -------------------------------------------------

item_metrics_df <- item_metrics_df %>%
  mutate(
    Trait = ifelse(grepl("^C_W", Item), "C-W (Conscientiousness at Work)", 
                   "HH-W (Honesty-Humility at Work)"),
    ItemShort = gsub("^(C_W_|HH_W_)", "", Item)
  ) %>%
  arrange(desc(MSE)) %>%
  mutate(ItemID = factor(row_number())) 

ggplot(item_metrics_df, aes(x = ItemID, y = MSE, fill = Trait)) +
  geom_col() +
  scale_x_discrete(labels = item_metrics_df$ItemShort) +
  scale_fill_manual(values = c("C-W (Conscientiousness at Work)" = "#117733", 
  "HH-W (Honesty-Humility at Work)" = "#3399E6")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Item",
    y = "MSE",
    fill = "Trait"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.margin = unit(c(1, 1, 3, 1), "lines"),
    legend.position = "none"
  ) 
  
ggsave("Plots_Tables/mse-item.png", width = 8, height = 4,
       dpi = 300)

#### Final Bias Visualisation ------------------------------------------

# Bias
bias_max <- max(abs(item_metrics_df$Bias), na.rm = TRUE)

# Resort by absolute bias:
item_metrics_df <- item_metrics_df %>%
  arrange(desc(abs(Bias))) %>%
  mutate(ItemID = factor(row_number()))

ggplot(item_metrics_df, aes(x = ItemID, y = Bias, fill = Trait)) +
  geom_col() +
  scale_x_discrete(labels = item_metrics_df$ItemShort) +
  scale_fill_manual(values = c("C-W (Conscientiousness at Work)" = "#117733", 
                               "HH-W (Honesty-Humility at Work)" = "#3399E6")) +
  theme_minimal(base_size = 14) +
  ylim(-bias_max, bias_max) +
  labs(
    x = "Item",
    y = "Bias",
    fill = "Trait"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.margin = unit(c(1, 1, 3, 1), "lines"),
    legend.title = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.margin = ggplot2::margin(2,2,2,2,unit = "pt")
  )

ggsave("Plots_Tables/bias-item.png", width = 8, height = 4, 
       dpi = 300)

## KL-Divergence -----------------------------------------------------------

### Trait Level -------------------------------------------------------------

# Initialize result table
kl_results_trait <- data.frame(
  Trait = traits,
  KL_Divergence_LLM_Human = NA_real_,
  KL_Divergence_Human_Human2 = NA_real_
)

# Compute KL divergence between LLM and human trait scores
kl_results_trait$KL_Divergence_LLM_Human[1] <- compute_kl_from_density(
  dat_full$person_score_C_W, 
  dat_full$llm_score_C_W
)

kl_results_trait$KL_Divergence_LLM_Human[2] <- compute_kl_from_density(
  dat_full$person_score_HH_W, 
  dat_full$llm_score_HH_W
)

# Compute KL divergence between human base and human comparison scores
kl_results_trait$KL_Divergence_Human_Human2[1] <- compute_kl_from_density(
  dat_extended$person_score_C_W,
  dat_extended$person_score_new_C_W
)

kl_results_trait$KL_Divergence_Human_Human2[2] <- compute_kl_from_density(
  dat_extended$person_score_HH_W, 
  dat_extended$person_score_new_HH_W
)

# Clear naming of table columns
colnames(kl_results_trait) <- c("Trait", "LLM vs Human", "Human vs Human Comp.")

# View results
kl_results_trait

### Item Level --------------------------------------------------------------

# Compute KL divergence for each item (LLM vs Human)
kl_values <- sapply(seq_along(llm_colnames), function(i) {
    compute_kl_divergence(
      original_cols[[i]], 
      llm_cols_source[[i]]
    )
  })

# Create results data frame
kl_item_df <- data.frame(
  column = original_colnames,
  kl_divergence = as.numeric(kl_values)
)

# Order by descending KL divergence
kl_item_df <- kl_item_df %>% arrange(desc(kl_divergence))

# Add IDs for unique identification without prefix
kl_item_df <- kl_item_df %>%
  mutate(
    Trait = ifelse(
      grepl("^C_W", column), "C-W (Conscientiousness at Work)", 
      "HH-W (Honesty-Humility at Work)"),
    ItemShort = gsub("^(C_W_|HH_W_)", "", column)
  ) %>%
  arrange(desc(kl_divergence)) %>%
  mutate(ItemID = factor(row_number())) 

# Coloured plot
ggplot(kl_item_df, aes(x = ItemID, y = kl_divergence, fill = Trait)) +
  geom_col() +
  scale_x_discrete(labels = kl_item_df$ItemShort) +
  scale_fill_manual(values = c("C-W (Conscientiousness at Work)" = "#117733", 
                               "HH-W (Honesty-Humility at Work)" = "#3399E6")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Item",
    y = "KL Divergence",
    fill = "Trait"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.margin = unit(c(1, 1, 3, 1), "lines"),
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.title = element_blank()
  )

ggsave("Plots_Tables/kl-div-item.png",
       # width = 8, height = 4.5, dpi = 300)
       width = 8, height = 4, dpi = 300)

## JS-Divergence -----------------------------------------------------------

### Trait Level -------------------------------------------------------------

# Initialize result table
js_results_trait <- data.frame(
  Trait = traits,
  JS_Divergence_LLM_Human = NA_real_,
  JS_Divergence_Human_Human2 = NA_real_
)


# Compute JS divergence: LLM vs Human (Base)
js_results_trait$JS_Divergence_LLM_Human[1] <- compute_js_from_density(
  dat_full$person_score_C_W, 
  dat_full$llm_score_C_W
)

js_results_trait$JS_Divergence_LLM_Human[2] <- compute_js_from_density(
  dat_full$person_score_HH_W, 
  dat_full$llm_score_HH_W
)

# Compute JS divergence: Human Comparison vs Human Base
js_results_trait$JS_Divergence_Human_Human2[1] <- compute_js_from_density(
  dat_extended$person_score_C_W, 
  dat_extended$person_score_new_C_W
)

js_results_trait$JS_Divergence_Human_Human2[2]  <- compute_js_from_density(
  dat_extended$person_score_HH_W,
  dat_extended$person_score_new_HH_W
)

# Clear naming of table columns
colnames(js_results_trait) <- c("Trait", "LLM vs Human", "Human vs Human Comp.")
js_results_trait

### Item Level --------------------------------------------------------------

js_results <- sapply(seq_along(llm_colnames), function(i) {
  x <- original_cols[[i]]
  y <- llm_cols[[i]]
  
  #Check for length mismatch
  if (length(x) != length(y)) {
    stop(
      sprintf(
        "Length mismatch in column %s: Human = %d, LLM = %d",
        original_colnames[i], length(x), length(y)
      )
    )
  }

  # Compute JS divergence
  ddjensen(x, y)
})

# Assign names and build final df
names(js_results) <- original_colnames

js_df <- data.frame(
  column = names(js_results),
  js_divergence = as.numeric(js_results)
) %>%
  arrange(desc(js_divergence))

# Create unique identifier to distinguish between items without prefix
js_df <- js_df %>%
  mutate(
    Trait = ifelse(grepl("^C_W", column), "C-W", "HH-W"),
    ItemShort = gsub("^(C_W_|HH_W_)", "", column)
  ) %>%
  arrange(desc(js_divergence)) %>%
  mutate(ItemID = factor(row_number()))

# Coloured plot JS divergences per item
ggplot(js_df, aes(x = ItemID, y = js_divergence, fill = Trait)) +
  geom_col() +
  scale_fill_manual(values = c("C-W" = "#117733", 
                               "HH-W" = "#3399E6")) +
  scale_x_discrete(labels = js_df$ItemShort) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Item",
    y = "JS Divergence",
    fill = "Trait"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.margin = unit(c(1, 1, 3, 1), "lines"),
    legend.position = "none"
  )

# Save and view
ggsave("Plots_Tables/js-item.png",
       width = 8, height = 4, dpi = 300)

## Wasserstein -------------------------------------------------------------

### Trait Level -------------------------------------------------------------

# Initialize results list
wasserstein_results <- list()

# Compute Wasserstein distance for each trait
for (trait in traits) {
  
  # Define column names
  person_col <- paste0("person_score_", trait)
  llm_col <- paste0("llm_score_", trait)
  person_col2 <- paste0("person_score_new_", trait)

  # Compute Wasserstein distances (p = 1)
  dist_llm <- wasserstein1d(dat_extended[[person_col]], 
                            dat_extended[[llm_col]], p = 1)
  dist_person2 <- wasserstein1d(dat_extended[[person_col]], 
                                dat_extended[[person_col2]], p = 1)
  
  # Store results
  wasserstein_results[[trait]] <- list(
    `LLM vs Human` = dist_llm,
    `Human vs Human Comp.` = dist_person2
  )
}

# Convert to df and view
wasserstein_table <- tibble(
  Trait                = names(wasserstein_results),
  `LLM vs Human`       = sapply(
    wasserstein_results, function(x) x[["LLM vs Human"]]),
  `Human vs Human Comp.` = sapply(
    wasserstein_results, function(x) x[["Human vs Human Comp."]])
)
wasserstein_table

### Item Level --------------------------------------------------------------

# Clean LLM column names for alignment check
llm_names_clean <- sub("^LLM_", "", colnames(llm_cols))
original_names <- colnames(original_cols)

# Check alignment in item names
if (!all(llm_names_clean == original_names)) {
  mismatches <- which(llm_names_clean != original_names)
  stop("Column name mismatch at positions: ", 
       paste(mismatches, collapse = ", "),
       "\nLLM columns: ",
       paste(llm_names_clean[mismatches], collapse = ", "),
       "\nOriginal columns: ", 
       paste(original_names[mismatches], collapse = ", "))
}

# Compute Wasserstein distances for each item (LLM vs Human)
wasserstein_results <- sapply(seq_along(llm_colnames), function(i) {
    x <- original_cols[[i]]
    y <- llm_cols[[i]]
    
    # Check for length mismatch
    if (length(x) != length(y)) {
      stop(
        sprintf(
          "Length mismatch in column %s: Human = %d, LLM = %d",
          original_colnames[i], length(x), length(y)
        )
      )
    }
    
    # Compute Wasserstein distance
    wasserstein1d(x, y)
  })

# Assign names and build final df
names(wasserstein_results) <- original_colnames

wasserstein_df <- data.frame(
  column = names(wasserstein_results),
  wasserstein_distance = as.numeric(wasserstein_results)
) %>%
  arrange(desc(wasserstein_distance))

# Create unique identifier to distinguish between items without prefix
wasserstein_df <- wasserstein_df %>%
  mutate(
    Trait = ifelse(grepl("^C_W", column), "C-W", "HH-W"),
    ItemShort = gsub("^(C_W_|HH_W_)", "", column)
  ) %>%
  arrange(desc(wasserstein_distance)) %>%
  mutate(ItemID = factor(row_number()))

# Coloured plot Wasserstein distances per item
ggplot(wasserstein_df, aes(x = ItemID, y = wasserstein_distance, fill = Trait))+
  geom_col() +
  scale_fill_manual(values = c("C-W" = "#117733", 
                               "HH-W" = "#3399E6")) +
  scale_x_discrete(labels = wasserstein_df$ItemShort) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Item",
    y = "WS Distance",
    fill = "Trait"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.margin = unit(c(1, 1, 3, 1), "lines"),
    legend.position = "none"
  )

# Save and view
ggsave("Plots_Tables/wasserstein-item.png",
       width = 8, height = 4, dpi = 300)

## Random Forest Based Fidelity Metrics ------------------------------------

### pMSE (per Trait) --------------------------------------------------------

# LLM and Human combined dataset
llm_human <- compute_pMSE(dat_mixed, class_label = 1)

# Human Base and Human Comparison combined dataset
human_human2 <- compute_pMSE(dat_mixed2, class_label = 2)

# Combine results
pMSE_results <- data.frame(
  Trait = traits,
  `pMSE (LLM vs Human)` = llm_human$pMSE,
  `pMSE (Human vs Human Comp.)` = human_human2$pMSE
)

### Feature Importance per Item (using full data) -----------------------------

# Define target and predictors
target <- "source"
predictors <- setdiff(names(dat_mixed), target)

# Create formula (predictors are all items from Conscientiousness and Honesty-
# Humility at Work)
rf_formula_fullcols <- as.formula(paste(
  target, "~", paste(predictors, collapse = " + ")))

# Fit Random Forest model with settings as for previous pMSE calculations
rf_model_fullcols_llm_human <- randomForest(
  formula = rf_formula_fullcols,
  data = dat_mixed,
  importance = TRUE,
  ntree = 500
)

# Extract feature importance
importance_df <- as.data.frame(importance(rf_model_fullcols_llm_human))
importance_df$Feature <- rownames(importance_df)

# Add domain and short base name
importance_df <- importance_df %>%
  mutate(
    Trait = ifelse(grepl("^C_W", Feature), "C-W", "HH-W"),
    BaseName = gsub("^(C_W_|HH_W_)", "", Feature)
  )

# Ensure unique y-axis position by assigning a unique row ID
# and preserving sort order by importance
importance_df <- importance_df %>%
  arrange(MeanDecreaseGini) %>% 
  mutate(ItemID = factor(row_number()))  # unique position for each item

# Plot
plot <- ggplot(importance_df, aes(x = MeanDecreaseGini,
                          y = ItemID,
                          color = Trait)) +
  geom_segment(aes(
    x = 0, 
    xend = MeanDecreaseGini, 
    y = ItemID, 
    yend = ItemID, 
    color = Trait),
    size = 2)+
  geom_point(size = 3) +
  scale_y_discrete(labels = importance_df$BaseName) +
  scale_colour_manual(values = c("C-W" = "#117733", 
                               "HH-W" = "#3399E6")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Mean Decrease in Gini",
    y = "Item",
    color = "Trait"
  ) +
  theme(
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )

# Save and view
ggsave(
  "Plots_Tables/feature_importance_item.png",
  plot = plot,
  dpi = 300,
  width = 10,
  height = 12
)

plot

##  Comparison of Item Rankings (WS-KL-Ft.Importance) ----------------------

# Compute Pearson correlations on standardized values
pearson_kl_ws <- cor(
  kl_item_df$kl_divergence, wasserstein_df$wasserstein_distance, 
  method = "pearson")
pearson_kl_rf <- cor(
  kl_item_df$kl_divergence, rev(importance_df$MeanDecreaseGini), 
  method = "pearson")
pearson_ws_rf <- cor(
  wasserstein_df$wasserstein_distance, rev(importance_df$MeanDecreaseGini), 
  method = "pearson")
pearson_js_kl <- cor(
  js_df$js_divergence, kl_item_df$kl_divergence,
  method = "pearson"
)
pearson_js_rf <- cor(
  js_df$js_divergence, rev(importance_df$MeanDecreaseGini),
  method = "pearson"
)
pearson_js_ws <- cor(
  wasserstein_df$wasserstein_distance, js_df$js_divergence,
  method = "pearson"
)

# Create a named vector of the correlations
pearson_values <- c(
  "KL vs. Wasserstein" = pearson_kl_ws,
  "KL vs. RF Gini"     = pearson_kl_rf,
  "Wasserstein vs. RF" = pearson_ws_rf,
  "JS vs. KL" = pearson_js_kl,
  "JS vs. RF Gini" = pearson_js_rf,
  "JS vs. WS" = pearson_js_ws
)

# Sort by absolute value (descending)
sorted_pearson <- pearson_values[order(abs(pearson_values), decreasing = TRUE)]

# Print sorted values
cat("Pearson Correlations (sorted by |value|):\n")
for (name in names(sorted_pearson)) {
  cat(sprintf("%-25s %.3f\n", name, sorted_pearson[name]))
}

## Summary Table -----------------------------------------------------------

# Prepare input tables
kl_df <- as.data.frame(kl_results_trait)
wasserstein_df <- as.data.frame(wasserstein_table)
pMSE_df <- as.data.frame(pMSE_results)

colnames(kl_df) <- c("Trait", "KL_LLM_Human", "KL_Human_Comp")
colnames(wasserstein_df) <- c("Trait", "Wass_LLM_Human", "Wass_Human_Comp")
colnames(pMSE_df) <- c("Trait", "pMSE_LLM_Human", "pMSE_Human_Comp")

# Merge by trait
merged_df <- Reduce(function(x, y) merge(x, y, by = "Trait"), 
                    list(kl_df, wasserstein_df, pMSE_df))

# Save in latex
xtab <- xtable(merged_df, digits = 2)

tab_body <- capture.output(print(xtab,
                                 include.rownames = FALSE,
                                 include.colnames = FALSE,
                                 only.contents = TRUE,
                                 sanitize.text.function = identity,
                                 hline.after = NULL))

latex_code <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Kullback-Leibler (KL) divergence, Wasserstein distance, and 
  propensity mean squared error (pMSE) between the human base (Hum.) and 
  LLM-generated dataset, and between the human base and human comparison (Comp.) 
  datasets, for the traits Conscientiousness at Work (C-W) and Honesty-Humility 
  at Work (HH-W).}",
  "\\label{tab:trait-divergences}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccc}",
  "\\hline",
  "Trait & \\multicolumn{2}{c}{KL Divergence} & 
  \\multicolumn{2}{c}{Wasserstein Distance} & \\multicolumn{2}{c}{pMSE} \\\\",
  "\\cline{2-3} \\cline{4-5} \\cline{6-7}",
  "& Hum./LLM & Hum./Comp. & Hum./LLM & Hum./Comp. & Hum./LLM 
  & Hum./Comp. \\\\",
  "\\hline",
  tab_body,
  "\\hline",
  "\\end{tabular}%",
  "}",
  "\\end{table}"
)

latex_code <- gsub("C_W", "C-W", latex_code)
latex_code <- gsub("HH_W", "HH-W", latex_code)

writeLines(latex_code, "Plots_Tables/trait_divergence_table.tex")

# Utility -----------------------------------------------------------------

## Settings ----------------------------------------------------------------

# Rotation method
rotation_method <- "oblimin"
# rotation_method <- "varimax" # for robustness check

# Set according to selection algorithm (i.e., incl. top-loading or not)
top_loading = 0 # top-loading selection yes = 1 / no (include all non-excluded 
# items) = 0
k = 4 # if top_loading = 1, set number of top-loading items to be selected

## LLM Data ----------------------------------------------------------------

## Dimensionality Assessment -----------------------------------------------

# Horn's parallel analysis to extract suggested number of factors
paran(iterations = 100,
      centile = 0,
      graph = TRUE,
      status = TRUE,
      all = TRUE,
      quietly = FALSE,
      mat = cor(llm_cols),
      n = nrow(llm_cols)
      )

## Exploratory Factor Analysis ---------------------------------------------

# Perform factor analysis on LLM-generated data
llm_fa_result <- fa(
  llm_cols,
  nfactors = 2,
  fm = "ols",
  rotate = rotation_method
)

# Extract factor loadings matrix
llm_loadings_matrix <- as.matrix(llm_fa_result$loadings[, 1:2])
rownames(llm_loadings_matrix) <- original_colnames

## Initial Loadings Table (latex) --------------------------------------------

# Bold loadings > |0.4|)
format_loading <- function(x) {
  if (abs(x) > 0.4) {
    sprintf("\\textbf{%.2f}", x)
  } else {
    sprintf("%.2f", x)
  }
}
formatted_loadings <- apply(llm_loadings_matrix, c(1, 2), format_loading)

# Swap columns of Factor 1 and Factor 2 for presentation purposes only
# (no impact on interpretation because of full swap)
df_formatted <- data.frame(
  Item = rownames(llm_loadings_matrix),
  `Factor 1` = formatted_loadings[, 2],  # was Factor 2
  `Factor 2` = formatted_loadings[, 1],  # was Factor 1
  check.names = FALSE
)

# Compose latex table
xtab <- xtable(df_formatted, digits = 3)

tab_body <- capture.output(print(
  xtab,
  include.rownames = FALSE,
  include.colnames = FALSE,
  only.contents = TRUE,
  sanitize.text.function = identity,
  hline.after = NULL
))

latex_code <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Exploratory factor loadings (oblimin rotation) based on 
  LLM-generated data for the full set of Conscientiousness (C) and 
  Honesty-Humility (HH) at Work items. 
  Loadings \\textgreater{} 0.40 are in bold.}",
  "\\label{tab:pre-EFA-loadings-llm}",
  "\\begin{tabular}{lrr}",
  "\\hline",
  "Item & Factor 1 & Factor 2 \\\\",
  "\\hline",
  tab_body,
  "\\hline",
  "\\end{tabular}",
  "\\end{table}"
)

latex_code <- gsub("_", "\\\\_", latex_code)

# Save latex table
writeLines(latex_code, "Plots_Tables/efa_loadings_LLM.tex")

## Item Selection ----------------------------------------------------------

##### STEP 1: Iterative EFA Filtering -------------------------------------------------

# Iteration 1
# Extract initial factor loadings
llm_loadings <- llm_fa_result$loadings
print(llm_loadings)

# Exclude items with low loadings on both factors or cross-loadings > .30
excl1 <- names(llm_loadings[which((abs(llm_loadings[, 1]) < .40 &
                                     abs(llm_loadings[, 2]) < .40) |
                                    (abs(llm_loadings[, 1]) > .30 &
                                       abs(llm_loadings[, 2]) > .30)), 1])

length(excl1) # 2 items ("LLM_C_W_workhours" "LLM_C_W_changes")

# Remove excluded items
llm_Select1 <- llm_cols[, -which(names(llm_cols) %in% excl1)]
dim(llm_Select1) # 150  37

# Iteration 2
llm_loadings <- fa(
  llm_Select1,
  nfactors = 2,
  fm = "ols",
  rotate = rotation_method
  )$loadings
print(llm_loadings)

# Check if any new items meet exclusion criteria
excl2 <- names(llm_loadings[which((abs(llm_loadings[,1]) < .40 &
                                     abs(llm_loadings[,2]) < .40) |
                                    (abs(llm_loadings[,1]) > .30 &
                                       abs(llm_loadings[,2]) > .30)),1])
length(excl2) # 0 items
dim(llm_Select1)
colnames(llm_Select1)


#### STEP 2: Communality Thresholding --------------------------------------

communalities <- fa(
  llm_Select1, nfactors = 2, fm = "ols", rotate = rotation_method
  )$communality

# Exclude items with low communalities
excl_comm <- names(which(communalities < .30))
length(excl_comm) # 0 items

#### Item Filter -----------------------------------------------------------

llm_cols <- llm_Select1

llm_colnames_C_W <- colnames(llm_Select1[
  colnames(llm_Select1) %in% llm_colnames_C_W])

llm_colnames_HH_W <- colnames(llm_Select1[
  colnames(llm_Select1) %in% llm_colnames_HH_W])

llm_cols_C_W <- llm_cols[llm_colnames_C_W]
llm_cols_HH_W <- llm_cols[llm_colnames_HH_W]


#### STEP 3: Internal Consistency Check -------------------------------------

alpha_result <- psych::alpha(llm_cols, check.keys=TRUE)$total

# Raw Alpha Increase > .001
alpha_raw_diff <- alpha_result$alpha.drop$raw_alpha - 
  alpha_result$total$raw_alpha
names(alpha_raw_diff) <- rownames(alpha_result$alpha.drop)
improving_raw_items <- alpha_raw_diff[alpha_raw_diff > 0.001]

# Standardised Alpha Increase > .001
alpha_std_diff <- alpha_result$alpha.drop$std.alpha - 
  alpha_result$total$std.alpha
names(alpha_std_diff) <- rownames(alpha_result$alpha.drop)
improving_std_items <- alpha_std_diff[alpha_std_diff > 0.001]

# Potentially removable items
improving_raw_items # 0
improving_std_items # 0

#### Final EFA with Selected Items -------------------------------------------

llm_fa_result <- fa(
  llm_cols,
  nfactors = 2,
  fm = "ols",
  rotate = rotation_method
)

# Clean row names for readability
llm_loadings <- llm_fa_result$loadings
rownames(llm_loadings) <- sub("^LLM_", "", rownames(llm_loadings))

#### Final Loadings Summary (latex) ------------------------------------------

loadings_df <- as.data.frame(unclass(llm_loadings))
loadings_df$Item <- rownames(loadings_df)

loadings_df$Trait <- ifelse(grepl("C_W", loadings_df$Item), "C_W", "HH_W")
loadings_df$MaxLoading <- apply(
  loadings_df[, c("V1", "V2")], 1, function(x) max(abs(x), na.rm = TRUE))

loadings_sorted <- loadings_df %>%
  arrange(Trait, desc(MaxLoading))

formatted_loadings <- loadings_sorted %>%
  dplyr::mutate(
    `Factor 1` = sapply(V2, format_loading),
    `Factor 2` = sapply(V1, format_loading)
  ) %>%
  dplyr::select(Item, `Factor 1`, `Factor 2`)

# Compose latex table
xtab <- xtable(formatted_loadings, digits = 3)

tab_body <- capture.output(print(
  xtab,
  include.rownames = FALSE,
  include.colnames = FALSE,
  only.contents = TRUE,
  sanitize.text.function = identity,
  hline.after = NULL
))

latex_code <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Exploratory factor loadings (oblimin rotation) after item 
  selection based on LLM-generated data for Conscientiousness (C) and 
  Honesty-Humility (HH) at Work items. 
  Loadings \\textgreater{} 0.40 are in bold.}",
  "\\label{tab:post-EFA-loadings-llm}",
  "\\begin{tabular}{lrr}",
  "\\hline",
  "Item & Factor 1 & Factor 2 \\\\",
  "\\hline",
  tab_body,
  "\\hline",
  "\\end{tabular}",
  "\\end{table}"
)

latex_code <- gsub("_", "\\\\_", latex_code)

# Save
writeLines(latex_code, "Plots_Tables/post_efa_loadings_llm.tex")

#### STEP 4: Top-Loading Selection ------------------------------------------

# Number of items to be selected
# (as determined by "Settings" at beginning of Utility Evaluations)
k <- if (top_loading == 0) {
  nrow(loadings_sorted)
} else {
  k
}

# Select top-k items per trait based on highest loadings (MaxLoading)
highest_per_trait <- loadings_sorted %>%
  dplyr::group_by(Trait) %>%
  arrange(MaxLoading, .by_group = TRUE) %>% # ascending order = lowest first
  slice_tail(n = k) %>%
  ungroup()

# Review selected items
print(highest_per_trait)

## Final Selection ---------------------------------------------------------

# Extract clean item names (remove "LLM_" prefix)
llm_selection <- sub("LLM_", "", highest_per_trait$Item)

# Separate selections by trait
llm_select_C_W <- llm_selection[grepl("C_W", llm_selection)]
llm_select_HH_W <- llm_selection[grepl("HH_W", llm_selection)]

## Human Data --------------------------------------------------------------

## Dimensionality Assessment -----------------------------------------------

# Horn's parallel analysis to extract suggested number of factors
paran(iterations = 100,
      centile = 0,
      graph = TRUE,
      status = TRUE,
      all = TRUE,
      quietly = FALSE,
      mat = cor(original_cols),
      n = nrow(original_cols)
) # varimax: 2 components

## Exploratory Factor Analysis ---------------------------------------------

# Perform factor analysis on LLM-generated data
human_fa_result <- fa(original_cols,
                      nfactors = 2,
                      fm = "ols",
                      rotate = rotation_method
                      )

# Extract factor loadings matrix
human_loadings_matrix <- as.matrix(human_fa_result$loadings[, 1:2])
stargazer(human_loadings_matrix)

## Initial Loadings Table (latex) ---------------------------------------------

# Bold loadings > |0.4|
formatted_loadings <- apply(human_loadings_matrix, c(1, 2), format_loading)

# Swap columns of Factor 1 and Factor 2 for presentation purposes only
# (no impact on interpretation because of full swap)
df_formatted <- data.frame(
  Item = rownames(human_loadings_matrix),
  `Factor 1` = formatted_loadings[, 2],
  `Factor 2` = formatted_loadings[, 1],
  check.names = FALSE
)

# Compose latex table
xtab <- xtable(df_formatted, digits = 3)

tab_body <- capture.output(print(
  xtab,
  include.rownames = FALSE,
  include.colnames = FALSE,
  only.contents = TRUE,
  sanitize.text.function = identity,
  hline.after = NULL
))

latex_code <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Exploratory factor loadings (oblimin rotation) based on human 
  data for the full set of Conscientiousness (C) and Honesty-Humility (HH) 
  at Work items. Loadings \\textgreater{} 0.40 are in bold.}",
  "\\begin{tabular}{lrr}",
  "\\hline",
  "Item & Factor 1 & Factor 2 \\\\",
  "\\hline",
  tab_body,
  "\\hline",
  "\\end{tabular}",
  "\\label{tab:pre-EFA-loadings-human}",
  "\\end{table}"
)

latex_code <- gsub("_", "\\\\_", latex_code)

writeLines(latex_code, "Plots_Tables/efa_loadings_human.tex")

## Item Selection ----------------------------------------------------------

#### STEP 1: Iterative EFA Filtering -----------------------------------------

# Iteration 1
human_loadings <- human_fa_result$loadings
print(human_loadings)

# Exclude items with low loadings on both factors or cross-loadings > .30
excl1 <- names(human_loadings[which((abs(human_loadings[, 1]) < .40 &
                                       abs(human_loadings[, 2]) < .40) |
                                      (abs(human_loadings[, 1]) > .30 &
                                         abs(human_loadings[, 2]) > .30)), 1])

length(excl1) # 12 items; varimax: 16 items

# Remove excluded items
human_Select1 <- original_cols[, -which(names(original_cols) %in% excl1)]
dim(human_Select1) # 150  27 / varimax: 150  23

# Iteration 2
human_loadings <- fa(
  human_Select1,
  nfactors = 2,
  fm="ols",
  rotate = rotation_method
  )$loadings
print(human_loadings)

# Check if any new items meet exclusion criteria
excl2 <- names(human_loadings[which((abs(human_loadings[,1]) < .40 &
                                       abs(human_loadings[,2]) < .40) |
                                      (abs(human_loadings[,1]) > .30 &
                                         abs(human_loadings[,2]) > .30)),1])
length(excl2) # oblimin and varimax: 1 item
dim(human_Select1)

# Iteration 3
human_Select2 <- human_Select1[, -which(names(human_Select1) %in% excl2)]
human_loadings <- fa(
  human_Select2,
  nfactors = 2, 
  fm="ols",
  rotate = rotation_method
  )$loadings
print(human_loadings)

# Check if any new items meet exclusion criteria
excl3 <- names(human_loadings[which((abs(human_loadings[,1]) < .40 &
                                       abs(human_loadings[,2]) < .40) |
                                      (abs(human_loadings[,1]) > .30 &
                                         abs(human_loadings[,2]) > .30)),1])
length(excl3) # oblimin: 0 items
dim(human_Select2) # 150  26 / varimax: 150 22

#### STEP 2: Communality Thresholding ----------------------------------------

communalities <- fa(
  human_Select2, nfactors = 2, fm = "ols", rotate = rotation_method
  )$communality

# Exclude items with low communalities
excl_comm <- names(which(communalities < .30))
length(excl_comm) # oblimin and varimax: 11 items
human_Select3<- human_Select2[, -which(names(human_Select2) %in% excl_comm)]
dim(human_Select3) # 150  15, varimax: 150 11

#### Item Filter -----------------------------------------------------------

original_cols <- human_Select3

original_colnames_C_W <- colnames(human_Select3[
  colnames(human_Select3) %in% original_colnames_C_W])

original_colnames_HH_W <- colnames(human_Select3[
  colnames(human_Select3) %in% original_colnames_HH_W])

original_cols_C_W <- original_cols[original_colnames_C_W]
original_cols_HH_W <- original_cols[original_colnames_HH_W]


#### STEP 3: Internal Consistency Check --------------------------------------

alpha_result <- psych::alpha(original_cols, check.keys=TRUE)$total

# Raw Alpha Increase > .001
alpha_raw_diff <- alpha_result$alpha.drop$raw_alpha - 
  alpha_result$total$raw_alpha
names(alpha_raw_diff) <- rownames(alpha_result$alpha.drop)
improving_raw_items <- alpha_raw_diff[alpha_raw_diff > 0.001]

# Standardised Alpha Increase > .001
alpha_std_diff <- alpha_result$alpha.drop$std.alpha - 
  alpha_result$total$std.alpha
names(alpha_std_diff) <- rownames(alpha_result$alpha.drop)
improving_std_items <- alpha_std_diff[alpha_std_diff > 0.001]

# Potentially removable items
improving_raw_items # 0
improving_std_items # 0


#### Final EFA with Selected Items -------------------------------------------

human_fa_result <- fa(
  original_cols,
  nfactors = 2,
  fm = "ols",
  rotate = rotation_method
)

# custom exclusion of items
original_cols$HH_W_integrity <- NULL # high loadings on C-W instead of HH-W
original_cols$HH_W_doublecheck <- NULL # high loadings on C-W instead of HH-W
original_cols$HH_W_assistance <- NULL # failed initial factor loading inclusion

human_fa_result <- fa(
  original_cols,
  nfactors = 2,
  fm = "ols",
  rotate = rotation_method
)

human_loadings <- human_fa_result$loadings

#### Final Loadings Summary (latex) ------------------------------------------

loadings_df <- as.data.frame(unclass(human_loadings))
loadings_df$Item <- rownames(loadings_df)

loadings_df$Trait <- ifelse(grepl("C_W", loadings_df$Item), "C_W", "HH_W")
loadings_df$MaxLoading <- apply(loadings_df[, c("V1", "V2")], 1, function(x)
  max(abs(x), na.rm = TRUE))

loadings_sorted <- loadings_df %>%
  dplyr::arrange(Trait, desc(MaxLoading))

formatted_postefa <- loadings_sorted %>%
  dplyr::mutate(
    `Factor 1` = sapply(V1, format_loading),
    `Factor 2` = sapply(V2, format_loading)
  ) %>%
  dplyr::select(Item, `Factor 1`, `Factor 2`)

# Compose latex table
xtab_postefa <- xtable(formatted_postefa, digits = 3)

tab_body <- capture.output(print(
  xtab_postefa,
  include.rownames = FALSE,
  include.colnames = FALSE,
  only.contents = TRUE,
  sanitize.text.function = identity,
  hline.after = NULL
))

latex_code <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Exploratory factor loadings (oblimin rotation) after item 
  selection based on human data for Conscientiousness (C) and Honesty-Humility 
  (HH) at Work items. Loadings \\textgreater{} 0.40 are in bold.}",
  "\\begin{tabular}{lrr}",
  "\\hline",
  "Item & Factor 1 & Factor 2 \\\\",
  "\\hline",
  tab_body,
  "\\hline",
  "\\end{tabular}",
  "\\label{tab:post-EFA-loadings-human}",
  "\\end{table}"
)

latex_code <- gsub("_", "\\\\_", latex_code)

# Write to file
writeLines(latex_code, "Plots_Tables/post_efa_loadings_human.tex")

#### STEP 4: Top-Loading Selection -------------------------------------------

# Number of items to be selected
# (as determined by "Settings" at beginning of Utility Evaluations)
k <- if (top_loading == 0) {
  nrow(loadings_sorted)
} else {
  k
}

# Select top-k items per trait based on highest loadings (MaxLoading)
highest_per_trait <- loadings_sorted %>%
  dplyr::group_by(Trait) %>%
  arrange(MaxLoading, .by_group = TRUE) %>%  # ascending order = lowest first
  dplyr::slice_tail(n = k) %>%
  ungroup()

# Review selected items
print(highest_per_trait)

#### FINAL SELECTION ---------------------------------------------------------

human_selection <- highest_per_trait$Item
human_select_C_W <- human_selection[grepl("C_W", human_selection)]
human_select_HH_W <- human_selection[grepl("HH_W", human_selection)]

# Validation --------------------------------------------------------------

## Test Set ----------------------------------------------------------------

# Use previous human comparison data as test set
human_data_test <- original_cols_new
colnames(human_data_test) <- sub("new_", "", colnames(human_data_test))

## Internal Consistency ----------------------------------------------------

# Inter-Trait Correlation
cor.test(
  rowMeans(human_data_test[llm_select_C_W], na.rm = TRUE),
  rowMeans(human_data_test[llm_select_HH_W], na.rm = TRUE)
)

cor.test(
  rowMeans(human_data_test[human_select_C_W], na.rm = TRUE),
  rowMeans(human_data_test[human_select_HH_W], na.rm = TRUE)
)

# Alpha Coefficient
psych::alpha(human_data_test[,llm_select_C_W], check.keys=TRUE)$total
psych::alpha(human_data_test[,human_select_C_W], check.keys=TRUE)$total

psych::alpha(human_data_test[,llm_select_HH_W], check.keys=TRUE)$total
psych::alpha(human_data_test[,human_select_HH_W], check.keys=TRUE)$total

# Omega Total
psych::omega(human_data_test[,llm_select_C_W], nfactors = 1,
                              rotate = "promax")$omega.tot
psych::omega(human_data_test[,human_select_C_W], nfactors = 1,
             rotate = "promax")$omega.tot

psych::omega(human_data_test[,llm_select_HH_W], nfactors = 1,
             rotate = "promax")$omega.tot
psych::omega(human_data_test[,human_select_HH_W], nfactors = 1,
             rotate = "promax")$omega.tot

# Inter-Item Correlation
(icorr <- cor(human_data_test[,llm_select_C_W], use = "complete.obs"))
icorr[icorr == 1] <- NA
colMeans(icorr[], na.rm=T)
min(colMeans(icorr[], na.rm=T))
max(colMeans(icorr[], na.rm=T))
mean(colMeans(icorr[], na.rm=T))

(icorr <- cor(human_data_test[,llm_select_HH_W], use = "complete.obs"))
icorr[icorr == 1] <- NA
colMeans(icorr[], na.rm=T)
min(colMeans(icorr[], na.rm=T))
max(colMeans(icorr[], na.rm=T))
mean(colMeans(icorr[], na.rm=T))

(icorr <- cor(human_data_test[,human_select_C_W], use = "complete.obs"))
icorr[icorr == 1] <- NA
colMeans(icorr[], na.rm=T)
min(colMeans(icorr[], na.rm=T))
max(colMeans(icorr[], na.rm=T))
mean(colMeans(icorr[], na.rm=T))

(icorr <- cor(human_data_test[,human_select_HH_W], use = "complete.obs"))
icorr[icorr == 1] <- NA
icorr[upper.tri(icorr)] <- NA
stargazer(icorr)
colMeans(icorr[], na.rm=T)
min(colMeans(icorr[], na.rm=T))
max(colMeans(icorr[], na.rm=T))
mean(colMeans(icorr[], na.rm=T))

# Item-Total Correlation
itotal <- human_data_test[
  which(complete.cases(human_data_test[, llm_select_C_W])), llm_select_C_W]
itotal$score <- rowMeans(itotal)
cor(itotal)[,ncol(itotal)][-ncol(itotal)]
min(cor(itotal)[,ncol(itotal)][-ncol(itotal)])
max(cor(itotal)[,ncol(itotal)][-ncol(itotal)])
mean(cor(itotal)[,ncol(itotal)][-ncol(itotal)])

itotal <- human_data_test[
  which(complete.cases(human_data_test[, llm_select_HH_W])), llm_select_HH_W]
itotal$score <- rowMeans(itotal)
cor(itotal)[, ncol(itotal)][-ncol(itotal)]
min(cor(itotal)[, ncol(itotal)][-ncol(itotal)])
max(cor(itotal)[, ncol(itotal)][-ncol(itotal)])
mean(cor(itotal)[, ncol(itotal)][-ncol(itotal)])

itotal <- human_data_test[
  which(complete.cases(human_data_test[,human_select_C_W])),human_select_C_W]
itotal$score <- rowMeans(itotal)
cor(itotal)[,ncol(itotal)][-ncol(itotal)]
min(cor(itotal)[,ncol(itotal)][-ncol(itotal)])
max(cor(itotal)[,ncol(itotal)][-ncol(itotal)])
mean(cor(itotal)[,ncol(itotal)][-ncol(itotal)])

itotal <- human_data_test[
  which(complete.cases(human_data_test[,human_select_HH_W])),human_select_HH_W]
itotal$score <- rowMeans(itotal)
cor(itotal)[,ncol(itotal)][-ncol(itotal)]
min(cor(itotal)[,ncol(itotal)][-ncol(itotal)])
max(cor(itotal)[,ncol(itotal)][-ncol(itotal)])
mean(cor(itotal)[,ncol(itotal)][-ncol(itotal)])


## Confirmatory Factor Analysis --------------------------------------------

# LLM-based model
model_string_llm <- paste0(
  "F1 =~ ", paste(llm_select_C_W, collapse = " + "), "\n",
  "F2 =~ ", paste(llm_select_HH_W, collapse = " + "), "\n"
)

fit_llm <- cfa(model_string_llm, data = human_data_test, std.lv = TRUE)
summary(fit_llm, fit.measures = TRUE, standardized = TRUE)

# Human-based model
model_string_human <- paste0(
  "F1 =~ ", paste(human_select_C_W, collapse = " + "), "\n",
  "F2 =~ ", paste(human_select_HH_W, collapse = " + "), "\n"
)

fit_human <- cfa(model_string_human, data = human_data_test, std.lv = TRUE)
summary(fit_human, fit.measures = TRUE, standardized = TRUE)

# Extract selected fit measures
fit_metrics <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
                 "rmsea.ci.lower", "rmsea.ci.upper", "srmr")


fit1 <- round(fitMeasures(fit_llm, fit_metrics), 3)
fit2 <- round(fitMeasures(fit_human, fit_metrics), 3)

# Create comparison table
fit_table <- data.frame(
  Measure = names(fit1),
  fit_llm = unname(fit1),
  fit_human = unname(fit2)
)
fit_table

## Final Fit Indices Table (latex) -----------------------------------------

format_val <- function(x) sprintf("%.2f", x)

fit_table_custom <- tibble::tibble(
  Measure = c("CFI", "TLI", "RMSEA", "\\quad 90\\% CI", "SRMR"),
  fit_llm = c(
    format_val(fit_table$fit_llm[fit_table$Measure == "cfi"]),
    format_val(fit_table$fit_llm[fit_table$Measure == "tli"]),
    format_val(fit_table$fit_llm[fit_table$Measure == "rmsea"]),
    sprintf("[%s, %s]",
            format_val(
              fit_table$fit_llm[fit_table$Measure == "rmsea.ci.lower"]),
            format_val(
              fit_table$fit_llm[fit_table$Measure == "rmsea.ci.upper"])),
    format_val(fit_table$fit_llm[fit_table$Measure == "srmr"])
  ),
  fit_human = c(
    format_val(fit_table$fit_human[fit_table$Measure == "cfi"]),
    format_val(fit_table$fit_human[fit_table$Measure == "tli"]),
    format_val(fit_table$fit_human[fit_table$Measure == "rmsea"]),
    sprintf("[%s, %s]",
            format_val(
              fit_table$fit_human[fit_table$Measure == "rmsea.ci.lower"]),
            format_val(
              fit_table$fit_human[fit_table$Measure == "rmsea.ci.upper"])),
    format_val(fit_table$fit_human[fit_table$Measure == "srmr"])
  )
)

xtab_fit <- xtable(fit_table_custom)

tab_body <- capture.output(print(
  xtab_fit,
  include.rownames = FALSE,
  include.colnames = FALSE,
  only.contents = TRUE,
  sanitize.text.function = identity,
  hline.after = NULL
))

if (top_loading == 0) {
  caption_text <- sprintf(
    "Fit indices from confirmatory factor analysis for models estimated from 
    LLM-generated data and human base data: comparative fit index (CFI), 
    Tucker–Lewis index (TLI), root mean squared error of approximation (RMSEA) 
    with confidence interval (CI), and standardized root mean square residual 
    (SRMR). Indices were calculated on the unseen human comparison dataset. 
    Note that the LLM model includes %d items, and the human model %d items.",
    (length(llm_select_C_W) + length(llm_select_HH_W)),
    (length(human_select_C_W) + length(human_select_HH_W))
  )
  label <- "\\label{tab:fit-comparison}"
} else {
  caption_text <- sprintf(
    "Fit indices from confirmatory factor analysis for models estimated from 
    LLM-generated data and human base data using top-loading item selection 
    ($k = %d$): comparative fit index (CFI), Tucker–Lewis index (TLI), root mean 
    squared error of approximation (RMSEA) with confidence interval (CI), and 
    standardized root mean square residual (SRMR). Indices were calculated on 
    the unseen human comparison dataset.",
    k
  )
  label <- "\\label{tab:fit-comparison-top}"
  }

latex_code <- c(
  "\\begin{table}[H]",
  "\\centering",
  sprintf("\\caption{%s}", caption_text),
  "\\begin{tabular}{lcc}",
  "\\hline",
  "\\textbf{Fit Measure} & \\textbf{LLM Model} & \\textbf{Human Model} \\\\",
  "\\hline",
  tab_body,
  "\\hline",
  "\\end{tabular}",
  label,
  "\\end{table}"
)


if (top_loading == 0) {
  writeLines(latex_code, "Plots_Tables/model_fit_comparison_table.tex")
} else {
  writeLines(
    latex_code, 
    sprintf("Plots_Tables/model_fit_comparison_table_top%d.tex", k))
}