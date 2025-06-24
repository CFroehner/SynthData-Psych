# SynthData-Psych
# Cosima Fröhner


# Help Functions ----------------------------------------------------------

# Reverses values on a 5-point Likert scale in reverse-coded items.
# Input: A vector of numeric or character values from a 5-point Likert scale.
# Returns: A vector of the same length with values reversed (1 to 5, 2 to 4, ...).
recode_values_5p <- function(x) {
  dplyr::recode(
    x,
    `1` = 5,
    `2` = 4,
    `3` = 3,
    `4` = 2,
    `5` = 1
  )
}

# Main Functions ----------------------------------------------------------

# Computes the KL divergence between two discrete distributions.
#
# Input:
#       p_vec: A vector of categorical values representing the empirical 
#               distribution P.
#       q_vec: -"- empirical distribution Q.
#
# Returns: A single numeric value: the KL divergence KL(P, Q).
compute_kl_divergence <- function(p_vec, q_vec) {
  support_vals <- sort(unique(c(p_vec, q_vec)))
  
  p_table <- table(factor(p_vec, levels = support_vals))
  q_table <- table(factor(q_vec, levels = support_vals))
  
  p_probs <- p_table / sum(p_table)
  q_probs <- q_table / sum(q_table)
  
  epsilon <- 1e-10
  p_probs <- pmax(p_probs, epsilon)
  q_probs <- pmax(q_probs, epsilon)
  
  sum(p_probs * log(p_probs / q_probs))
}

# Computes the KL divergence between two continuous distributions using
# kernel density estimation.
#
# Input:
#       p_vec: A numeric vector representing samples from distribution P.
#       q_vec: -"- from distribution Q.
#
# Returns: A single numeric value: the KL divergence KL(P, Q), approximated 
#           using the estimated densities and the Riemann sum.
compute_kl_from_density <- function(p_vec, q_vec, from = 1, to = 5.5) {
  dens_p <- density(p_vec, from = from, to = to)
  dens_q <- density(q_vec, from = from, to = to)
  
  # linear interpolation
  q_interp <- approx(dens_q$x, dens_q$y, xout = dens_p$x, rule = 2)$y
  
  epsilon <- 1e-10
  p_y <- pmax(dens_p$y, epsilon)
  q_y <- pmax(q_interp, epsilon)
  
  kl_div <- sum(p_y * log(p_y / q_y)) * (dens_p$x[2] - dens_p$x[1])
  return(kl_div)
}

# Computes the JS divergence between two discrete distributions.
#
# Inputs:
#         p_vec: A vector of categorical values representing the empirical distribution P.
#         q_vec: -"- empirical distribution Q.
#
# Returns: A single numeric value: the JS divergence between P and Q.
compute_js_divergence <- function(p_vec, q_vec) {
  support_vals <- sort(unique(c(p_vec, q_vec)))
  
  p_table <- table(factor(p_vec, levels = support_vals))
  q_table <- table(factor(q_vec, levels = support_vals))
  
  p_probs <- p_table / sum(p_table)
  q_probs <- q_table / sum(q_table)
  
  epsilon <- 1e-10
  p_probs <- pmax(p_probs, epsilon)
  q_probs <- pmax(q_probs, epsilon)
  
  m_probs <- 0.5 * (p_probs + q_probs) # mixture distribution
  
  kl_pm <- sum(p_probs * log(p_probs / m_probs))
  kl_qm <- sum(q_probs * log(q_probs / m_probs))
  
  js_divergence <- 0.5 * (kl_pm + kl_qm)
  return(js_divergence)
}

# Computes the JS divergence between two continuous distributions
# using kernel density estimation.
#
# Inputs:
#         p_vec: A numeric vector representing samples from distribution P.
#         q_vec: -"- from distribution Q.
#
# Returns: A single numeric value: the JS divergence between P and Q.
compute_js_from_density <- function(p_vec, q_vec, from = 1, to = 5.5) {
  
  dens_p <- density(p_vec, from = from, to = to)
  dens_q <- density(q_vec, from = from, to = to)
  
  q_interp <- approx(dens_q$x, dens_q$y, xout = dens_p$x, rule = 2)$y
  
  epsilon = 1e-10
  p_y <- pmax(dens_p$y, epsilon)
  q_y <- pmax(q_interp, epsilon)
  
  m_y <- 0.5 * (p_y + q_y) # mixture distribution
  
  kl_pm <- sum(p_y * log(p_y / m_y)) * (dens_p$x[2] - dens_p$x[1])
  kl_qm <- sum(q_y * log(q_y / m_y)) * (dens_p$x[2] - dens_p$x[1])
  
  js_div <- 0.5 * (kl_pm + kl_qm)
  return(js_div)
}

# Function to compute pMSE for a combined dataset at the trait level
#
# Inputs:
# data: A data frame containing observations from different sources.
#       Must include a "source" column indicating class membership 
#       (e.g., Human, LLM).
# class_label: The numeric label of the class for which predicted probability
#               is compared to the true class proportion 
#               (e.g., 1 for LLM, 2 for Human Comparison).
#
# Returns: A numeric vector of pMSE values, one for each trait.
compute_pMSE <- function(data, class_label = 1) {
  c_val <- mean(data$source == class_label)  # class probability
  
  pMSE_values <- numeric(length(traits))
  importance_list <- vector("list", length(traits))
  names(importance_list) <- traits
  
  for (i in seq_along(traits)) {
    trait <- traits[i]
    
    # Select trait-specific predictors and source label
    trait_data <- data %>%
      dplyr::select(starts_with(paste0(trait, "_")), source)
    
    target     <- "source"
    predictors <- setdiff(names(trait_data), target)
    
    rf_formula <- as.formula(paste(
      target, "~", paste(predictors, collapse = " + ")))
    
    rf_model <- randomForest(
      formula   = rf_formula,
      data      = trait_data,
      importance = TRUE,
      ntree     = 500
    )
    
    # pMSE calculation
    rf_probs <- predict(rf_model, type = "prob")[, as.character(class_label)]
    pMSE_values[i] <- mean((rf_probs - c_val)^2)
    
    # Feature importance
    importance_df <- as.data.frame(importance(rf_model))
    importance_df$Feature <- rownames(importance_df)
    importance_df$Trait <- trait
    importance_list[[i]] <- importance_df
    
    # Order features by importance
    importance_sorted <- importance_df %>%
      arrange(desc(MeanDecreaseGini))
  }
  
  return(list(pMSE = pMSE_values, importance = importance_list))
  
  # return(pMSE_values)
}

