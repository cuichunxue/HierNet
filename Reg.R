# ============================================================================
# hierNet 階層的LASSO回帰分析アプリケーション
# Hierarchical LASSO Regression with Interaction Terms
# ============================================================================
# Optimized & Refactored Version
# - Modular architecture with extracted utility functions
# - Vectorized operations for performance
# - Memoization for expensive computations
# - Enhanced error handling and validation
# - Statistical computation improvements
# ============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(hierNet)
  library(glmnet)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(DT)
  library(plotly)
  library(scales)
  library(bslib)
})

# ============================================================================
# CONSTANTS & CONFIGURATION
# ============================================================================

# Numeric thresholds
COEF_ZERO_THRESHOLD <- 1e-8
DISPLAY_THRESHOLD <- 1e-6
MIN_EXPLANATORY_VARS <- 2
MAX_RECOMMENDED_VARS <- 15
DEFAULT_SAMPLE_SIZE <- 200
RANDOM_SEED <- 42
MIN_SAMPLE_SIZE <- 10  # Minimum observations for reliable analysis

# Null coalescing operator (for R < 4.1 compatibility)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Color palette (GitHub-inspired dark theme)
COLORS <- list(
  bg_primary   = "#0d1117",
  bg_secondary = "#161b22",
  bg_tertiary  = "#21262d",
  border       = "#30363d",
  text_primary = "#c9d1d9",
  text_muted   = "#8b949e",
  accent_blue  = "#58a6ff",
  accent_green = "#3fb950",
  accent_red   = "#f85149",
  accent_orange = "#d29922",
  accent_purple = "#a371f7",
  info_blue    = "#388bfd",
  link_blue    = "#79c0ff"
)

# R² thresholds for color coding
R2_EXCELLENT <- 0.8
R2_GOOD <- 0.6

# ============================================================================
# THEME & STYLING
# ============================================================================

custom_theme <- bs_theme(

  version = 5,
  bg = COLORS$bg_primary,
  fg = COLORS$text_primary,
  primary = COLORS$accent_blue,
  secondary = COLORS$bg_tertiary,
  success = COLORS$accent_green,
  warning = COLORS$accent_orange,

  danger = COLORS$accent_red,
  base_font = font_google("IBM Plex Sans"),
  heading_font = font_google("IBM Plex Mono"),
  code_font = font_google("IBM Plex Mono")
)

custom_css <- sprintf("
/* Base theme */
body {
  background: linear-gradient(135deg, %s 0%%, %s 50%%, %s 100%%);
  min-height: 100vh;
  font-family: 'IBM Plex Sans', sans-serif;
}

/* Header */
.main-header {
  background: linear-gradient(90deg, %s 0%%, %s 100%%);
  border-bottom: 1px solid %s;
  padding: 1.5rem 2rem;
  margin-bottom: 2rem;
}

.app-title {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 1.8rem;
  font-weight: 700;
  color: %s;
  letter-spacing: -0.5px;
  margin: 0;
}

.app-subtitle {
  font-size: 0.9rem;
  color: %s;
  margin-top: 0.25rem;
}

/* Cards */
.analysis-card {
  background: rgba(22, 27, 34, 0.95);
  border: 1px solid %s;
  border-radius: 12px;
  padding: 1.5rem;
  margin-bottom: 1.5rem;
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.3);
  backdrop-filter: blur(10px);
  transition: all 0.3s ease;
}

.analysis-card:hover {
  border-color: %s;
  box-shadow: 0 8px 32px rgba(88, 166, 255, 0.15);
}

.card-header {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.85rem;
  font-weight: 600;
  color: %s;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  margin-bottom: 1rem;
  padding-bottom: 0.75rem;
  border-bottom: 1px solid %s;
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.card-header::before {
  content: '▸';
  color: %s;
}

/* Form controls */
.form-control, .selectize-input {
  background: %s !important;
  border: 1px solid %s !important;
  border-radius: 8px !important;
  color: %s !important;
  font-family: 'IBM Plex Sans', sans-serif !important;
  transition: all 0.2s ease !important;
}

.form-control:focus, .selectize-input.focus {
  border-color: %s !important;
  box-shadow: 0 0 0 3px rgba(88, 166, 255, 0.15) !important;
}

.selectize-dropdown {
  background: %s !important;
  border: 1px solid %s !important;
  border-radius: 8px !important;
}

.selectize-dropdown-content .option {
  color: %s !important;
  padding: 10px 12px !important;
}

.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .option.active {
  background: %s !important;
  color: %s !important;
}

/* Button */
.btn-analysis {
  background: linear-gradient(135deg, #238636 0%%, #2ea043 100%%);
  border: none;
  border-radius: 8px;
  color: #ffffff;
  font-family: 'IBM Plex Mono', monospace;
  font-weight: 600;
  font-size: 0.9rem;
  padding: 12px 24px;
  letter-spacing: 0.5px;
  transition: all 0.3s ease;
  width: 100%%;
  margin-top: 1rem;
}

.btn-analysis:hover {
  background: linear-gradient(135deg, #2ea043 0%%, #3fb950 100%%);
  transform: translateY(-2px);
  box-shadow: 0 4px 20px rgba(46, 160, 67, 0.4);
}

.btn-analysis:active { transform: translateY(0); }

/* Inline radio buttons for scale toggle */
.card-header .shiny-input-radiogroup {
  margin: 0;
}

.card-header .shiny-input-radiogroup .shiny-options-group {
  display: flex;
  gap: 0.5rem;
}

.card-header .shiny-input-radiogroup label.radio-inline {
  padding: 0.25rem 0.75rem;
  margin: 0;
  font-size: 0.75rem;
  font-weight: 500;
  background: %s;
  border: 1px solid %s;
  border-radius: 4px;
  cursor: pointer;
  transition: all 0.2s ease;
}

.card-header .shiny-input-radiogroup label.radio-inline:hover {
  border-color: %s;
}

.card-header .shiny-input-radiogroup input[type='radio']:checked + span {
  color: %s;
}

.card-header .shiny-input-radiogroup label.radio-inline:has(input:checked) {
  background: %s;
  border-color: %s;
}

/* Result section */
.result-section {
  background: linear-gradient(135deg, %s 0%%, %s 100%%);
  border: 1px solid %s;
  border-radius: 12px;
  padding: 1.5rem;
  margin-bottom: 1.5rem;
}

.equation-display {
  background: %s;
  border: 1px solid %s;
  border-radius: 8px;
  padding: 1.25rem;
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.95rem;
  color: %s;
  overflow-x: auto;
  white-space: pre-wrap;
  word-break: break-all;
  line-height: 1.8;
}

/* Metrics grid */
.metric-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
  gap: 1rem;
  margin-top: 1rem;
}

.metric-box {
  background: linear-gradient(135deg, %s 0%%, %s 100%%);
  border: 1px solid %s;
  border-radius: 10px;
  padding: 1.25rem;
  text-align: center;
  transition: all 0.3s ease;
}

.metric-box:hover {
  border-color: %s;
  transform: translateY(-3px);
}

.metric-label {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.7rem;
  color: %s;
  text-transform: uppercase;
  letter-spacing: 1px;
  margin-bottom: 0.5rem;
}

.metric-value {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 1.5rem;
  font-weight: 700;
  color: %s;
}

.metric-value.success { color: %s; }
.metric-value.warning { color: %s; }
.metric-value.danger { color: %s; }

/* Tables */
.dataTables_wrapper { font-family: 'IBM Plex Sans', sans-serif; }

table.dataTable {
  background: transparent !important;
  border-collapse: separate !important;
  border-spacing: 0 4px !important;
}

table.dataTable thead th {
  background: %s !important;
  color: %s !important;
  font-family: 'IBM Plex Mono', monospace !important;
  font-size: 0.8rem !important;
  font-weight: 600 !important;
  text-transform: uppercase !important;
  letter-spacing: 1px !important;
  border: none !important;
  padding: 12px 16px !important;
}

table.dataTable tbody td {
  background: %s !important;
  color: %s !important;
  border: none !important;
  padding: 12px 16px !important;
}

table.dataTable tbody tr:hover td { background: %s !important; }

/* Notifications */
.shiny-notification {
  background: %s !important;
  border: 1px solid %s !important;
  border-radius: 8px !important;
  color: %s !important;
}

/* Scrollbar */
::-webkit-scrollbar { width: 8px; height: 8px; }
::-webkit-scrollbar-track { background: %s; }
::-webkit-scrollbar-thumb { background: %s; border-radius: 4px; }
::-webkit-scrollbar-thumb:hover { background: #484f58; }

/* Plot container */
.plot-container {
  background: %s;
  border: 1px solid %s;
  border-radius: 8px;
  padding: 1rem;
}

/* Tabs */
.nav-tabs { border-bottom: 1px solid %s; }

.nav-tabs .nav-link {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.85rem;
  color: %s;
  border: none;
  padding: 0.75rem 1.25rem;
  transition: all 0.2s ease;
}

.nav-tabs .nav-link:hover {
  color: %s;
  border-bottom: 2px solid #484f58;
}

.nav-tabs .nav-link.active {
  background: transparent;
  color: %s;
  border-bottom: 2px solid %s;
}

/* Labels */
.control-label {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.8rem;
  color: %s;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  margin-bottom: 0.5rem;
}

/* Responsive */
@media (max-width: 768px) {
  .metric-grid { grid-template-columns: repeat(2, 1fr); }
  .app-title { font-size: 1.4rem; }
}
",
  # Template arguments in order of appearance
  COLORS$bg_primary, COLORS$bg_secondary, COLORS$bg_primary,
  COLORS$bg_secondary, COLORS$bg_tertiary, COLORS$border,
  COLORS$accent_blue, COLORS$text_muted, COLORS$border, COLORS$accent_blue,
  COLORS$accent_blue, COLORS$border, COLORS$accent_green,
  COLORS$bg_primary, COLORS$border, COLORS$text_primary, COLORS$accent_blue,
  COLORS$bg_secondary, COLORS$border, COLORS$text_primary,
  COLORS$bg_tertiary, COLORS$accent_blue,
  # Radio button scale toggle
  COLORS$bg_tertiary, COLORS$border, COLORS$accent_blue,
  COLORS$accent_blue, COLORS$accent_blue, COLORS$accent_blue,
  COLORS$bg_secondary, COLORS$bg_primary, COLORS$border,
  COLORS$bg_primary, COLORS$border, COLORS$link_blue,
  COLORS$bg_tertiary, COLORS$bg_secondary, COLORS$border, COLORS$accent_blue,
  COLORS$text_muted, COLORS$accent_blue,
  COLORS$accent_green, COLORS$accent_orange, COLORS$accent_red,
  COLORS$bg_tertiary, COLORS$text_muted,
  COLORS$bg_secondary, COLORS$text_primary, COLORS$bg_tertiary,
  COLORS$bg_tertiary, COLORS$border, COLORS$text_primary,
  COLORS$bg_primary, COLORS$border,
  COLORS$bg_primary, COLORS$border, COLORS$border,
  COLORS$text_muted, COLORS$text_primary, COLORS$accent_blue, COLORS$accent_blue,
  COLORS$text_muted
)

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Validate numeric data for hierNet analysis
#' @param X Matrix of predictors
#' @param y Response vector
#' @return List with validated data and diagnostics
validate_data <- function(X, y) {
  issues <- character(0)

  # Check for complete cases
  complete_idx <- complete.cases(X, y)
  n_missing <- sum(!complete_idx)

  if (n_missing > 0) {
    issues <- c(issues, sprintf("%d行の欠損値を除外", n_missing))
    X <- X[complete_idx, , drop = FALSE]
    y <- y[complete_idx]
  }

  # Check for constant columns (handle NA from single observation)
  const_cols <- apply(X, 2, function(col) {
    v <- var(col, na.rm = TRUE)
    is.na(v) || v < .Machine$double.eps
  })
  if (any(const_cols, na.rm = TRUE)) {
    const_names <- colnames(X)[const_cols]
    issues <- c(issues, sprintf("定数列を検出: %s", paste(const_names, collapse = ", ")))
    X <- X[, !const_cols, drop = FALSE]
  }

  # Check minimum sample size
  if (nrow(X) < MIN_SAMPLE_SIZE) {
    issues <- c(issues, sprintf("サンプルサイズが小さすぎます（n=%d, 推奨: n≥%d）", nrow(X), MIN_SAMPLE_SIZE))
  }

  # Check for highly correlated columns
  if (ncol(X) > 1 && nrow(X) > 1) {
    cor_mat <- tryCatch(cor(X), error = function(e) NULL)
    if (!is.null(cor_mat)) {
      diag(cor_mat) <- 0
      # Handle NaN from singular columns
      if (any(!is.finite(cor_mat))) {
        issues <- c(issues, "完全共線性の変数ペアを検出")
      } else {
        high_cor <- which(abs(cor_mat) > 0.99, arr.ind = TRUE)
        if (nrow(high_cor) > 0) {
          issues <- c(issues, "高相関変数ペアを検出（|r| > 0.99）")
        }
      }
    }
  }

  list(
    X = X,
    y = y,
    n = nrow(X),
    p = ncol(X),
    issues = issues,
    valid = nrow(X) > 0 && ncol(X) >= MIN_EXPLANATORY_VARS
  )
}

#' Extract interaction coefficients as a tidy data frame (vectorized)
#' @param int_mat Interaction matrix (display values, e.g. OLS-refit-derived)
#' @param var_names Variable names
#' @param threshold Minimum absolute value to include (used only when active_mask is NULL)
#' @param active_mask Optional logical matrix (same shape as int_mat): which
#'   cells hierNet actually selected. Preferred over thresholding int_mat
#'   directly, since int_mat's display values may carry small refit noise
#'   on terms hierNet zeroed exactly.
#' @return Data frame of interactions (always has var1, var2, coefficient columns)
extract_interactions <- function(int_mat, var_names, threshold = DISPLAY_THRESHOLD,
                                 active_mask = NULL) {
  # Return properly structured empty data.frame

  empty_result <- data.frame(
    var1 = character(0),
    var2 = character(0),
    coefficient = numeric(0),
    stringsAsFactors = FALSE
  )

  n <- nrow(int_mat)
  if (is.null(n) || n < 1) return(empty_result)

  # Get upper triangle indices INCLUDING diagonal (for quadratic terms)
  idx <- which(upper.tri(int_mat, diag = TRUE), arr.ind = TRUE)
  coefficients <- int_mat[upper.tri(int_mat, diag = TRUE)]

  keep <- if (!is.null(active_mask)) {
    active_mask[upper.tri(active_mask, diag = TRUE)]
  } else {
    abs(coefficients) > threshold
  }

  if (!any(keep)) return(empty_result)

  data.frame(
    var1 = var_names[idx[keep, 1]],
    var2 = var_names[idx[keep, 2]],
    coefficient = coefficients[keep],
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(desc(abs(coefficient)))
}

#' Build regression equation string
#' @param intercept Intercept value
#' @param main_effects Named vector of main effects (display values)
#' @param int_mat Interaction matrix (display values)
#' @param var_names Variable names
#' @param target_name Target variable name
#' @param main_active Optional logical vector: which main effects hierNet
#'   selected. Preferred over thresholding main_effects directly.
#' @param int_active Optional logical matrix: which interaction/quadratic
#'   terms hierNet selected. Preferred over thresholding int_mat directly.
#' @return Character string of equation
build_equation <- function(intercept, main_effects, int_mat, var_names, target_name,
                          scale = "original", mx = NULL,
                          main_active = NULL, int_active = NULL) {
  # For standardized scale, intercept is 0 (Y is centered), so don't display it
  # For original scale, always show intercept
  if (abs(intercept) < DISPLAY_THRESHOLD) {
    terms <- ""
    first_term <- TRUE
  } else {
    terms <- sprintf("%.4f", intercept)
    first_term <- FALSE
  }

  # Main effects (NOT centered for original scale). Prefer hierNet's own
  # selection mask over thresholding the (OLS-refit) display value, since a
  # term hierNet zeroed exactly can carry small refit noise.
  active_main <- if (!is.null(main_active)) main_active else abs(main_effects) > DISPLAY_THRESHOLD
  for (i in which(active_main)) {
    coef <- main_effects[i]
    var_name <- names(main_effects)[i]
    # Main effects: always just β × X (no centering)
    if (first_term) {
      # First term: no leading sign for positive, "-" for negative
      if (coef < 0) {
        terms <- paste0("-", sprintf("%.4f", abs(coef)), " × ", var_name)
      } else {
        terms <- paste0(sprintf("%.4f", coef), " × ", var_name)
      }
      first_term <- FALSE
    } else {
      sign <- if (coef > 0) " + " else " - "
      terms <- paste0(terms, sign, sprintf("%.4f", abs(coef)), " × ", var_name)
    }
  }

  # Interactions and quadratic terms (centered for original scale only)
  interactions <- extract_interactions(int_mat, var_names, DISPLAY_THRESHOLD,
                                       active_mask = int_active)
  for (i in seq_len(nrow(interactions))) {
    coef <- interactions$coefficient[i]
    v1 <- interactions$var1[i]
    v2 <- interactions$var2[i]

    is_quadratic <- (v1 == v2)

    if (scale == "original" && !is.null(mx)) {
      m1 <- as.numeric(mx[v1])
      if (is_quadratic) {
        # Quadratic term: θ × (X - X̄)²
        if (length(m1) > 0 && !is.na(m1)) {
          term <- sprintf("(%s - %.2f)²", v1, m1)
        } else {
          term <- paste0(v1, "²")
        }
      } else {
        # Interaction term: θ × (Xi - X̄i)(Xj - X̄j)
        m2 <- as.numeric(mx[v2])
        if (length(m1) > 0 && !is.na(m1) && length(m2) > 0 && !is.na(m2)) {
          term <- sprintf("(%s - %.2f)(%s - %.2f)", v1, m1, v2, m2)
        } else {
          term <- paste0(v1, " × ", v2)
        }
      }
    } else {
      # Standardized
      if (is_quadratic) {
        term <- paste0(v1, "²")
      } else {
        term <- paste0(v1, " × ", v2)
      }
    }

    if (first_term) {
      if (coef < 0) {
        terms <- paste0("-", sprintf("%.4f", abs(coef)), " × ", term)
      } else {
        terms <- paste0(sprintf("%.4f", coef), " × ", term)
      }
      first_term <- FALSE
    } else {
      sign <- if (coef > 0) " + " else " - "
      terms <- paste0(terms, sign, sprintf("%.4f", abs(coef)), " × ", term)
    }
  }

  # Handle edge case: no terms selected
  if (terms == "") {
    terms <- "0"
  }

  paste0(target_name, " = ", terms)
}

#' Compute model metrics with robust edge case handling
#' @param y Actual values
#' @param predictions Predicted values
#' @param n_params Number of parameters (excluding intercept)
#' @return List of metrics
compute_metrics <- function(y, predictions, n_params) {
  n <- length(y)
  residuals <- y - predictions

  ss_res <- sum(residuals^2)
  ss_tot <- sum((y - mean(y))^2)

  # Handle zero variance in response
  if (ss_tot < .Machine$double.eps) {
    warning("目的変数の分散がゼロです")
    r_squared <- NA_real_
  } else {
    # Deliberately NOT clamped: a negative R² (model worse than the mean)
    # is a red flag the user must see, not a value to hide
    r_squared <- 1 - ss_res / ss_tot
  }

  # Adjusted R² with proper degrees of freedom
  df_res <- n - n_params - 1
  adj_r_squared <- if (df_res > 0 && !is.na(r_squared)) {
    1 - (1 - r_squared) * (n - 1) / df_res
  } else {
    NA_real_
  }

  # MAPE: handle individual zeros gracefully
  non_zero_idx <- y != 0
  mape <- if (sum(non_zero_idx) > 0) {
    mean(abs(residuals[non_zero_idx] / y[non_zero_idx])) * 100
  } else {
    NA_real_
  }

  list(
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    rmse = sqrt(mean(residuals^2)),
    mae = mean(abs(residuals)),
    mape = mape,
    max_error = max(abs(residuals)),
    residuals = residuals,
    n = n,
    df_residual = df_res
  )
}

#' Count active coefficients from hierNet's own active/support masks
#' @param main_active Logical vector: which main effects hierNet selected
#' @param int_active Logical matrix: which interaction/quadratic terms hierNet selected
#' @return List with counts
count_active_coefficients <- function(main_active, int_active) {
  list(
    n_main = sum(main_active),
    n_interaction = sum(int_active[upper.tri(int_active)]),
    n_quadratic = sum(diag(int_active))
  )
}

#' Get R² color class
#' @param r2 R-squared value
#' @return CSS class name
get_r2_class <- function(r2) {
  if (is.na(r2)) "danger"
  else if (r2 >= R2_EXCELLENT) "success"
  else if (r2 >= R2_GOOD) "warning"
  else "danger"
}

#' Create ggplot theme for hierNet
#' @return ggplot theme object
theme_hiernet <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_line(color = COLORS$bg_tertiary),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = COLORS$text_muted),
      axis.title = element_text(color = COLORS$text_primary),
      legend.background = element_rect(fill = COLORS$bg_secondary, color = COLORS$border),
      legend.text = element_text(color = COLORS$text_primary),
      legend.title = element_text(color = COLORS$text_primary)
    )
}

#' Configure plotly layout for hierNet
#' @param p plotly object
#' @return Configured plotly object
layout_hiernet <- function(p) {
  p |>
    layout(
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent",
      font = list(color = COLORS$text_primary)
    ) |>
    config(displayModeBar = FALSE)
}

# ============================================================================
# DATA GENERATION
# ============================================================================

#' Generate sample data with specified pattern
#' @param pattern One of: main_linear, strong_interact, weak_interact, quadratic, low_signal_noise
#' @param n Number of observations
#' @param seed Random seed
#' @return Data frame
generate_sample_data <- function(
    pattern = c("main_linear", "strong_interact", "weak_interact", "quadratic", "low_signal_noise"),
    n = DEFAULT_SAMPLE_SIZE,
    seed = RANDOM_SEED
) {
  pattern <- match.arg(pattern)
  set.seed(seed)

  # Generate predictors
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- c("X1", "X2", "X3", "X4", "X5")
  eps <- rnorm(n, 0, 1)

  # Define coefficients based on pattern
  coeffs <- switch(
    pattern,
    main_linear = list(
      intercept = 5,
      main = c(2.0, 1.5, 0.8, 0.3, 0.0),
      int12 = 0, int23 = 0, quad1 = 0, quad2 = 0,
      noise_sd = 1
    ),
    strong_interact = list(
      intercept = 5,
      main = c(1.5, 1.5, 0.5, 0.0, 0.0),
      int12 = 1.8, int23 = 0.8, quad1 = 0, quad2 = 0,
      noise_sd = 1
    ),
    weak_interact = list(
      intercept = 5,
      main = c(0.0, 1.5, 0.0, 0.0, 0.0),
      int12 = 2.0, int23 = 0, quad1 = 0, quad2 = 0,
      noise_sd = 1
    ),
    quadratic = list(
      intercept = 10,
      main = c(0.5, -0.5, 0.0, 0.0, 0.0),
      int12 = 1.0, int23 = 0, quad1 = 2.0, quad2 = -1.5,
      noise_sd = 1
    ),
    low_signal_noise = list(
      intercept = 50,
      main = c(0.1, -0.1, 0.05, 0.0, 0.0),
      int12 = 0, int23 = 0, quad1 = 0, quad2 = 0,
      noise_sd = 5
    )
  )

  # Compute response (explicit vectorization for matrix multiplication)
  Y <- coeffs$intercept +
    as.vector(X %*% coeffs$main) +
    coeffs$int12 * X[, 1] * X[, 2] +
    coeffs$int23 * X[, 2] * X[, 3] +
    coeffs$quad1 * X[, 1]^2 +
    coeffs$quad2 * X[, 2]^2 +
    rnorm(n, 0, coeffs$noise_sd)

  data.frame(
    パターン = pattern,
    品質スコア = round(as.vector(Y), 2),
    温度 = round(X[, 1], 3),
    圧力 = round(X[, 2], 3),
    速度 = round(X[, 3], 3),
    材料硬度 = round(X[, 4], 3),
    湿度 = round(X[, 5], 3)
  )
}

# ============================================================================
# HIERNET ANALYSIS WRAPPER
# ============================================================================

#' Perform hierNet analysis with cross-validation
#' @param X Predictor matrix
#' @param y Response vector
#' @param strong Use strong hierarchy constraint
#' @param nlam Number of lambda values
#' @param nfolds Number of CV folds
#' @param model_type "interaction" (no quadratic) or "quadratic" (with quadratic)
#' @param seed Optional random seed for reproducible CV fold assignment
#' @return List with fit, cv_fit, and extracted results
#' @throws Error if analysis fails
run_hiernet_analysis <- function(
    X, y,
    strong = TRUE,
    nlam = 20,
    nfolds = 5,
    model_type = "interaction",
    seed = NULL
) {
  # Input validation
  if (!is.matrix(X)) X <- as.matrix(X)
  if (nrow(X) < nfolds) {
    stop(sprintf("サンプル数(%d)がCV fold数(%d)より少ないです", nrow(X), nfolds))
  }

  # Collected warnings, surfaced in the UI by the caller
  warn_msgs <- character(0)

  # Quadratic terms are controlled at FIT time via hierNet's diagonal argument.
  # This is the statistically correct approach: for the interaction-only model,
  # coefficients are optimized and lambda is cross-validated WITHOUT X² terms
  # (post-hoc zeroing of fitted quadratic terms would invalidate both).
  include_diagonal <- (model_type == "quadratic")

  # Fit lambda path (done once, reused for CV and final fit)
  path_fit <- tryCatch(
    hierNet.path(
      x = X,
      y = y,
      nlam = nlam,
      strong = strong,
      diagonal = include_diagonal
    ),
    error = function(e) {
      stop(sprintf("hierNet.path failed: %s", e$message))
    }
  )

  # Cross-validation (seeded so fold assignment and lambda selection are reproducible)
  seed_valid <- !is.null(seed) && is.finite(seed) &&
    seed >= 1 && seed <= .Machine$integer.max
  if (seed_valid) {
    set.seed(as.integer(seed))
  } else if (!is.null(seed)) {
    # seed was supplied (e.g. cleared to NA, or out of R's integer range) but
    # unusable — surface this rather than silently skipping reproducibility
    warn_msgs <- c(warn_msgs, "乱数シードが無効なため、再現性は保証されません（結果は実行のたびに変わる可能性があります）")
  }
  cv_fit <- tryCatch(
    hierNet.cv(
      fit = path_fit,
      x = X,
      y = y,
      nfolds = nfolds
    ),
    error = function(e) {
      stop(sprintf("Cross-validation failed: %s", e$message))
    }
  )

  # Validate CV result
  best_lambda <- cv_fit$lamhat
  if (is.null(best_lambda) || is.na(best_lambda) || best_lambda <= 0) {
    stop("交差検証で最適なλを見つけられませんでした")
  }

  # CV error at the selected lambda: honest out-of-sample performance estimate
  # (in-sample R²/RMSE are optimistic). hierNet.cv returns the CV curve as
  # $lamlist/$cv (mean squared error), NOT $cv.err (that field doesn't exist).
  cv_rmse <- tryCatch({
    if (is.null(cv_fit$lamlist) || is.null(cv_fit$cv) ||
        length(cv_fit$lamlist) == 0 || length(cv_fit$lamlist) != length(cv_fit$cv)) {
      NA_real_
    } else {
      idx <- which.min(abs(cv_fit$lamlist - best_lambda))
      sqrt(cv_fit$cv[idx])
    }
  }, error = function(e) NA_real_)

  # Final model with optimal lambda
  final_fit <- tryCatch(
    hierNet(
      x = X,
      y = y,
      lam = best_lambda,
      strong = strong,
      diagonal = include_diagonal
    ),
    error = function(e) {
      stop(sprintf("Final model fitting failed: %s", e$message))
    }
  )

  # Validate fit result
  if (is.null(final_fit$bp) || is.null(final_fit$bn)) {
    stop("モデルフィッティングの結果が不正です")
  }

  # Extract scaling info from hierNet fit
  mx <- final_fit$mx  # column means
  sx <- final_fit$sx  # column standard deviations

  # If sx is NULL or missing, compute from data
  if (is.null(sx) || length(sx) == 0) {
    sx <- apply(X, 2, sd, na.rm = TRUE)
    sx[sx < .Machine$double.eps] <- 1  # Avoid division by zero
  }
  if (is.null(mx) || length(mx) == 0) {
    mx <- colMeans(X, na.rm = TRUE)
  }

  p <- ncol(X)
  mean_y <- mean(y, na.rm = TRUE)

  # Raw hierNet coefficients. These are what actually went through LASSO-style
  # sparsity (exactly 0 for terms hierNet did not select), so they — not the
  # displayed OLS-refit values below — are the correct basis for "is this
  # term active" decisions. The OLS refit reproduces predict() only up to the
  # solver's own numerical tolerance, so a term hierNet zeroed can come back
  # from the refit as O(1e-4)-O(1e-3) noise rather than exact 0; thresholding
  # the refit's *displayed* magnitude against COEF_ZERO_THRESHOLD (1e-8) would
  # misclassify that noise as "selected."
  main_effects_std_raw <- final_fit$bp - final_fit$bn
  if (any(!is.finite(main_effects_std_raw))) {
    warn_msgs <- c(warn_msgs, "標準化係数にNaN/Infが含まれていました")
  }
  interaction_matrix_th_raw <- final_fit$th
  if (is.null(interaction_matrix_th_raw)) {
    interaction_matrix_th_raw <- matrix(0, nrow = p, ncol = p)
  }
  if (any(!is.finite(interaction_matrix_th_raw))) {
    warn_msgs <- c(warn_msgs, "交互作用係数にNaN/Infが含まれていました")
  }

  main_effects_active <- abs(main_effects_std_raw) > COEF_ZERO_THRESHOLD
  main_effects_active[!is.finite(main_effects_active)] <- FALSE
  names(main_effects_active) <- colnames(X)
  interaction_matrix_active <- abs(interaction_matrix_th_raw) > COEF_ZERO_THRESHOLD
  interaction_matrix_active[!is.finite(interaction_matrix_active)] <- FALSE
  if (!include_diagonal) diag(interaction_matrix_active) <- FALSE
  dimnames(interaction_matrix_active) <- list(colnames(X), colnames(X))

  # Predictions come from hierNet itself: the authoritative model output.
  # Metrics (R², RMSE, ...) are computed against these, so they always
  # reflect the actually fitted model.
  predictions <- tryCatch(
    as.vector(predict(final_fit, newx = X)),
    error = function(e) {
      warn_msgs <<- c(warn_msgs, "予測計算に失敗したためフィット値を使用します")
      as.vector(final_fit$yhat)
    }
  )
  n_bad_predictions <- sum(!is.finite(predictions))
  if (n_bad_predictions > 0) {
    warn_msgs <- c(warn_msgs, sprintf(
      "予測値のうち%d件にNaN/Infが含まれていたため平均値で補完しました。モデルが不安定な可能性があります",
      n_bad_predictions
    ))
    predictions[!is.finite(predictions)] <- mean_y
  }

  # --------------------------------------------------------------------------
  # Equation derivation: exact OLS refit against predict()
  # --------------------------------------------------------------------------
  # Displayed equation (original scale):
  #   ŷ = intercept + Σ β_orig×X + Σ_{i<j} θ_ij×(Xi-X̄i)(Xj-X̄j) + Σ θ_ii×(Xi-X̄i)²
  #
  # Rather than assuming a specific hierNet internal convention (exact
  # standardization/centering of interaction and quadratic features, and
  # whether the fitted model applies a 1/2 factor to θ_ii) — which could not
  # be confirmed against a live install of hierNet because CRAN and GitHub
  # are both unreachable from this sandbox — the displayed coefficients are
  # instead recovered by an exact ordinary-least-squares refit of hierNet's
  # own predict() output against the basis we display:
  #   {X_1, ..., X_p} ∪ {(Xi-X̄i)(Xj-X̄j) : i<j} ∪ {(Xi-X̄i)² : i, if included}
  # hierNet's fitted model is, by construction, some linear+pairwise-bilinear
  # function of x; any such function has a UNIQUE representation in this
  # basis (e.g. Xi², z_i², or (Xi-X̄i)²/sx_i² all differ only by a linear
  # change of variables absorbed into the basis's own coefficients). So this
  # refit reproduces predict() exactly (residual ~0) regardless of hierNet's
  # internal parameterization — it needs no assumption about that
  # parameterization at all, only that n is large enough for the design
  # matrix to have full column rank (true whenever validate_data's n>=10p
  # guidance is followed).

  Xc <- sweep(X, 2, mx)
  tri_idx <- which(upper.tri(matrix(0, p, p), diag = include_diagonal), arr.ind = TRUE)
  n_terms <- nrow(tri_idx)

  extra <- if (n_terms > 0) {
    Xc[, tri_idx[, 1], drop = FALSE] * Xc[, tri_idx[, 2], drop = FALSE]
  } else {
    matrix(0, nrow(X), 0)
  }

  design <- cbind(Intercept = 1, X, extra)

  # lm.fit does NOT error on a rank-deficient design (e.g. a <3-level variable
  # under model_type="quadratic" makes its (Xi-X̄i)² exactly collinear with
  # Xi, or two explanatory variables correlated above validate_data's 0.99
  # warning-only threshold) — it silently returns NA for the aliased
  # column(s). Dropping just those columns and refitting the reduced design
  # keeps every well-identified coefficient on the accurate OLS-refit
  # footing, instead of discarding the whole equation to the fallback
  # formula over a single collinear term.
  keep_cols <- seq_len(ncol(design))
  refit <- tryCatch(lm.fit(x = design, y = predictions), error = function(e) NULL)
  n_dropped <- 0
  while (!is.null(refit) && any(!is.finite(refit$coefficients)) && length(keep_cols) > 1) {
    aliased <- !is.finite(refit$coefficients)
    keep_cols <- keep_cols[!aliased]
    n_dropped <- n_dropped + sum(aliased)
    refit <- tryCatch(lm.fit(x = design[, keep_cols, drop = FALSE], y = predictions),
                      error = function(e) NULL)
  }
  refit_succeeded <- !is.null(refit) && all(is.finite(refit$coefficients))
  if (refit_succeeded && n_dropped > 0) {
    warn_msgs <- c(warn_msgs, sprintf(
      "多重共線性のため%d項を除外して式を再フィットしました（該当項は0として扱われます）",
      n_dropped
    ))
  }
  # Aliased/dropped columns are reported as 0 (their effect is already fully
  # absorbed by the correlated column that was kept), not NA — so a single
  # collinear term never propagates missing values into the display.
  refit_coefs <- rep(0, ncol(design))
  if (refit_succeeded) refit_coefs[keep_cols] <- refit$coefficients

  # hierNet's fit comes from an iterative solver (ADMM/coordinate descent)
  # converged to a numerical TOLERANCE (default tol=1e-5), not an exact
  # closed-form solution — so predict() is never an exact quadratic function
  # of x to machine precision, and the OLS refit's residual reflects that
  # inherent solver slop, not necessarily an error in our basis/formula. R²
  # (variance explained) is used rather than an absolute tolerance since it
  # is scale-invariant: a refit is considered a good match once it explains
  # essentially all of predict()'s variance.
  refit_r2 <- if (refit_succeeded) {
    ss_res <- sum(refit$residuals^2)
    ss_tot <- sum((predictions - mean(predictions))^2)
    if (ss_tot > .Machine$double.eps) 1 - ss_res / ss_tot else 1
  } else {
    NA_real_
  }
  refit_resid_sd <- if (refit_succeeded) sd(refit$residuals) else NA_real_

  if (refit_succeeded) {
    # The OLS refit is, by construction, the best possible representation of
    # predict() in our display basis — always adopt it rather than falling
    # back to a hand-derived conversion whenever it succeeds numerically.
    intercept_orig <- refit_coefs[1]
    main_effects_orig <- refit_coefs[2:(p + 1)]
    names(main_effects_orig) <- colnames(X)
    interaction_matrix_orig <- matrix(0, p, p, dimnames = list(colnames(X), colnames(X)))
    if (n_terms > 0) {
      theta_vals <- refit_coefs[(p + 2):(p + 1 + n_terms)]
      interaction_matrix_orig[tri_idx] <- theta_vals
      interaction_matrix_orig[tri_idx[, 2:1, drop = FALSE]] <- theta_vals
    }
  } else {
    # Fallback: naive conversion, only when the refit itself fails outright
    # (e.g. a rank-deficient design from an unusual/degenerate dataset)
    warn_msgs <- c(warn_msgs, "回帰式セルフチェック: predict()との整合を計算できませんでした。式は近似値として扱ってください")
    main_effects_orig <- main_effects_std_raw / sx
    main_effects_orig[!is.finite(main_effects_orig)] <- 0
    names(main_effects_orig) <- colnames(X)
    interaction_matrix_orig <- interaction_matrix_th_raw / outer(sx, sx)
    interaction_matrix_orig[!is.finite(interaction_matrix_orig)] <- 0
    if (!include_diagonal) diag(interaction_matrix_orig) <- 0
    rownames(interaction_matrix_orig) <- colnames(X)
    colnames(interaction_matrix_orig) <- colnames(X)
    intercept_orig <- mean_y - sum(main_effects_orig * mx)
    if (!is.finite(intercept_orig)) intercept_orig <- 0
  }

  # A refit against a degenerate (near-constant, post NaN/Inf repair)
  # `predictions` vector can trivially reach R²≈1 (ss_tot≈0) even though the
  # underlying model fit catastrophically failed — never report success in
  # that case, regardless of the refit's own R².
  equation_ok <- refit_succeeded && n_bad_predictions == 0 &&
    is.finite(refit_r2) && refit_r2 >= 0.999
  if (n_bad_predictions > 0) {
    warn_msgs <- c(warn_msgs, "回帰式セルフチェック: 予測値が不正だったため式の正確性を保証できません")
  } else if (refit_succeeded && !equation_ok) {
    warn_msgs <- c(warn_msgs, sprintf(
      "回帰式セルフチェック: 表示式がpredict()の分散の%.2f%%しか説明できていません。式は近似値として扱ってください",
      100 * max(refit_r2, 0)
    ))
  }

  # Standardized-scale coefficients, derived from the (now exact) original-
  # scale ones so both displays describe the same underlying model
  main_effects_std <- main_effects_orig * sx
  names(main_effects_std) <- colnames(X)
  interaction_matrix_std <- interaction_matrix_orig * outer(sx, sx)
  rownames(interaction_matrix_std) <- colnames(X)
  colnames(interaction_matrix_std) <- colnames(X)

  # Standardized equation is displayed in centered-Y form:
  #   (Y - Ȳ) = intercept_std + Σ β_std×X* + Σ θ_std×Xi*×Xj*
  # intercept_std is exactly 0 when no interactions are active; with active
  # interactions it equals -Σθ_std×mean(Xi*Xj*) (correlation correction)
  intercept_std <- intercept_orig - mean_y + sum(main_effects_orig * mx)
  if (!is.finite(intercept_std)) intercept_std <- 0

  list(
    fit = final_fit,
    cv_fit = cv_fit,
    path_fit = path_fit,
    best_lambda = best_lambda,
    cv_rmse = cv_rmse,
    mean_y = mean_y,
    # Standardized coefficients
    main_effects_std = main_effects_std,
    interaction_matrix_std = interaction_matrix_std,
    intercept_std = intercept_std,
    # Original scale coefficients
    main_effects_orig = main_effects_orig,
    interaction_matrix_orig = interaction_matrix_orig,
    intercept_orig = intercept_orig,
    # Scaling info
    mx = mx,
    sx = sx,
    # Which terms hierNet actually selected (LASSO-exact sparsity, used for
    # "選択" indicators and active-term counts — NOT for display magnitudes)
    main_effects_active = main_effects_active,
    interaction_matrix_active = interaction_matrix_active,
    # Equation self-check result: whether the OLS refit against predict()
    # explains essentially all of its variance (small residuals are expected
    # from hierNet's iterative solver tolerance, not necessarily a bug)
    equation_check = list(ok = equation_ok, deviation_sd = refit_resid_sd,
                          r_squared = refit_r2),
    # Warnings to surface in the UI
    warnings = warn_msgs,
    predictions = predictions
  )
}

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- fluidPage(
  theme = custom_theme,
  tags$head(
    tags$style(HTML(custom_css)),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600;700&family=IBM+Plex+Sans:wght@400;500;600&display=swap",
      rel = "stylesheet"
    )
  ),

  # Header
  div(
    class = "main-header",
    div(
      class = "container-fluid",
      h1(
        class = "app-title",
        tags$span(style = sprintf("color: %s;", COLORS$accent_purple), "◆"),
        " hierNet 階層的LASSO回帰"
      ),
      p(
        class = "app-subtitle",
        "Hierarchical LASSO with Strong/Weak Hierarchy | 主効果・交互作用の階層制約付き回帰分析"
      )
    )
  ),

  # Main content
  div(
    class = "container-fluid",
    fluidRow(
      # Left panel - Controls (with tabs)
      column(
        4,
        tabsetPanel(
          type = "tabs",
          id = "settings_tabs",

          # Data settings tab
          tabPanel(
            title = "データ設定",
            value = "data_settings",
            div(
              style = "margin-top: 1rem;",

              # Data input
              div(
                class = "analysis-card",
                div(class = "card-header", "データ入力"),
                tabsetPanel(
                  id = "data_input_mode",
                  type = "pills",
                  tabPanel(
                    title = "CSVファイル",
                    value = "csv",
                    br(),
                    fileInput(
                      "data_file",
                      label = "CSVファイルをアップロード",
                      accept = c(".csv", ".CSV"),
                      buttonLabel = "選択",
                      placeholder = "ファイル未選択"
                    ),
                    selectInput(
                      "encoding", "文字エンコーディング",
                      choices = c("UTF-8" = "UTF-8", "Shift-JIS" = "CP932", "EUC-JP" = "EUC-JP"),
                      selected = "UTF-8"
                    )
                  ),
                  tabPanel(
                    title = "表データ貼り付け",
                    value = "paste",
                    br(),
                    textAreaInput(
                      "data_paste",
                      label = "表データを貼り付け（CSV または タブ区切り）",
                      placeholder = "例：\n品質スコア,温度,圧力,速度\n80,0.12,-0.55,1.02",
                      rows = 6
                    )
                  )
                ),
                hr(style = sprintf("border-color: %s;", COLORS$border)),
                checkboxInput("header", "1行目をヘッダー行として扱う", value = TRUE)
              ),

              # Variable selection
              div(
                class = "analysis-card",
                div(class = "card-header", "変数設定"),
                selectInput("target_var", label = "目的変数 (Y)", choices = NULL),
                selectInput("explanatory_vars", label = "説明変数 (X)", choices = NULL, multiple = TRUE),
                p(
                  style = sprintf("color: %s; font-size: 0.8rem;", COLORS$accent_orange),
                  "変数は10個以下を推奨"
                )
              ),

              # Sample data pattern
              div(
                class = "analysis-card",
                div(class = "card-header", "サンプルデータ"),
                selectInput(
                  "sample_pattern",
                  "パターン選択（データ未指定時に使用）",
                  choices = c(
                    "主効果のみ" = "main_linear",
                    "強い交互作用" = "strong_interact",
                    "弱い階層の交互作用" = "weak_interact",
                    "二次モデル" = "quadratic",
                    "低寄与・ほぼノイズ" = "low_signal_noise"
                  ),
                  selected = "strong_interact"
                )
              )
            )
          ),

          # Model settings tab
          tabPanel(
            title = "モデル設定",
            value = "model_settings",
            div(
              style = "margin-top: 1rem;",
              div(
                class = "analysis-card",
                div(class = "card-header", "hierNetパラメータ"),
                selectInput(
                  "model_type",
                  "モデルタイプ",
                  choices = c(
                    "交互作用モデル" = "interaction",
                    "二次モデル" = "quadratic"
                  ),
                  selected = "interaction"
                ),
                hr(style = sprintf("border-color: %s;", COLORS$border)),
                selectInput(
                  "hierarchy_type",
                  "階層制約タイプ",
                  choices = c("Strong hierarchy" = "strong", "Weak hierarchy" = "weak"),
                  selected = "strong"
                ),
                hr(style = sprintf("border-color: %s;", COLORS$border)),
                sliderInput("nlam", "Lambda候補数", min = 10, max = 50, value = 20, step = 5),
                sliderInput("nfolds", "交差検証フォールド数", min = 3, max = 10, value = 5, step = 1),
                numericInput("cv_seed", "乱数シード（再現性の確保）",
                             value = 42, min = 1, max = 2147483647, step = 1),
                actionButton("run_analysis", "分析実行", class = "btn-analysis", icon = icon("play"))
              )
            )
          )
        )
      ),

      # Right panel - Results
      column(
        8,
        tabsetPanel(
          type = "tabs",
          id = "result_tabs",

          # Data preview tab
          tabPanel(
            title = "データプレビュー",
            value = "data_tab",
            div(
              class = "result-section", style = "margin-top: 1rem;",
              div(class = "card-header", "アップロードデータ / サンプルデータ"),
              DTOutput("data_preview")
            )
          ),

          # Results tab
          tabPanel(
            title = "回帰結果",
            value = "result_tab",
            div(
              style = "margin-top: 1rem;",
              div(
                class = "result-section",
                div(class = "card-header", "モデル評価指標"),
                div(class = "metric-grid", uiOutput("metrics_display"))
              ),
              div(
                class = "result-section",
                div(
                  class = "card-header",
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  span("回帰式・係数"),
                  radioButtons(
                    "coef_scale",
                    label = NULL,
                    choices = c("元単位" = "original", "標準化" = "standardized"),
                    selected = "original",
                    inline = TRUE
                  )
                ),
                div(class = "equation-display", uiOutput("equation_display")),
                uiOutput("equation_check_badge"),
                p(
                  style = sprintf("color: %s; font-size: 0.75rem; margin: 0.5rem 0;", COLORS$text_muted),
                  uiOutput("scale_description")
                )
              ),
              div(
                class = "result-section",
                div(class = "card-header", "係数一覧 (主効果・交互作用)"),
                DTOutput("coef_table")
              )
            )
          ),

          # Plots tab
          tabPanel(
            title = "予測判定グラフ",
            value = "plot_tab",
            div(
              style = "margin-top: 1rem;",
              div(
                class = "result-section",
                div(class = "card-header", "実測値 vs 予測値"),
                div(class = "plot-container", plotlyOutput("prediction_plot", height = "500px"))
              ),
              fluidRow(
                column(
                  6,
                  div(
                    class = "result-section",
                    div(class = "card-header", "残差分布"),
                    div(class = "plot-container", plotlyOutput("residual_plot", height = "350px"))
                  )
                ),
                column(
                  6,
                  div(
                    class = "result-section",
                    div(class = "card-header", "Lambda選択 (CV)"),
                    div(class = "plot-container", plotOutput("cv_plot", height = "350px"))
                  )
                )
              )
            )
          ),

          # Coefficient structure tab
          tabPanel(
            title = "係数構造",
            value = "structure_tab",
            div(
              style = "margin-top: 1rem;",
              fluidRow(
                column(
                  6,
                  div(
                    class = "result-section",
                    div(class = "card-header", "主効果係数"),
                    div(class = "plot-container", plotlyOutput("main_effect_plot", height = "400px"))
                  )
                ),
                column(
                  6,
                  div(
                    class = "result-section",
                    div(class = "card-header", "交互作用ヒートマップ"),
                    div(class = "plot-container", plotlyOutput("interaction_heatmap", height = "400px"))
                  )
                )
              ),
              div(
                class = "result-section",
                div(class = "card-header", "選択された交互作用"),
                DTOutput("interaction_table")
              )
            )
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVER DEFINITION
# ============================================================================

server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(
    data = NULL,
    analysis = NULL  # Combined fit, cv_fit, and results
  )

  # -------------------------------------------------------------------------
  # Data loading
  # -------------------------------------------------------------------------

  parsed_data <- reactive({
    mode <- input$data_input_mode %||% "csv"
    pattern <- input$sample_pattern %||% "strong_interact"

    # CSV mode
    if (mode == "csv" && !is.null(input$data_file)) {
      tryCatch({
        df <- read.csv(
          input$data_file$datapath,
          header = isTRUE(input$header),
          fileEncoding = input$encoding %||% "UTF-8",
          stringsAsFactors = FALSE
        )
        showNotification("CSVファイルを読み込みました", type = "message", duration = 3)
        df
      }, error = function(e) {
        showNotification(
          sprintf("CSV読み込みエラー: %s（サンプルデータを使用します）", e$message),
          type = "error", duration = 8
        )
        generate_sample_data(pattern = pattern)
      })
    }
    # Paste mode
    else if (mode == "paste") {
      txt <- input$data_paste
      if (!is.null(txt) && nzchar(trimws(txt))) {
        sep <- if (grepl("\t", txt)) "\t" else ","
        tryCatch({
          df <- as.data.frame(read.table(
            text = txt,
            sep = sep,
            header = isTRUE(input$header),
            stringsAsFactors = FALSE,
            check.names = FALSE
          ))
          showNotification("貼り付けデータを読み込みました", type = "message", duration = 3)
          df
        }, error = function(e) {
          showNotification(
            sprintf("貼り付けデータの読み込みエラー: %s（サンプルデータを使用します）", e$message),
            type = "error", duration = 8
          )
          generate_sample_data(pattern = pattern)
        })
      } else {
        generate_sample_data(pattern = pattern)
      }
    }
    # Default: sample data
    else {
      generate_sample_data(pattern = pattern)
    }
  })

  # Update variable selectors when data changes
  observe({
    df <- parsed_data()
    rv$data <- df

    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]

    if (length(numeric_cols) == 0) {
      updateSelectInput(session, "target_var", choices = character(0))
      updateSelectInput(session, "explanatory_vars", choices = character(0))
      showNotification("数値列が見つかりませんでした", type = "error")
      return()
    }

    # Check minimum columns for hierNet (target + at least 2 explanatory)
    if (length(numeric_cols) < 3) {
      showNotification(
        sprintf("hierNetには最低3つの数値列が必要です（現在: %d列）", length(numeric_cols)),
        type = "warning"
      )
    }

    # Check sample size vs number of variables
    n_obs <- nrow(df)
    n_vars <- length(numeric_cols) - 1  # excluding target
    if (n_obs < 10 * n_vars) {
      showNotification(
        sprintf("サンプルサイズ(%d)が変数数(%d)に対して小さい可能性があります（推奨: n≥10p）",
                n_obs, n_vars),
        type = "warning"
      )
    }

    updateSelectInput(session, "target_var", choices = numeric_cols, selected = numeric_cols[1])

    explanatory_choices <- if (length(numeric_cols) > 1) numeric_cols[-1] else numeric_cols
    updateSelectInput(
      session, "explanatory_vars",
      choices = explanatory_choices,
      selected = explanatory_choices
    )
  })

  # Update explanatory variables when target changes (exclude target from choices)
  observeEvent(input$target_var, {
    req(rv$data)
    numeric_cols <- names(rv$data)[vapply(rv$data, is.numeric, logical(1))]
    explanatory_choices <- setdiff(numeric_cols, input$target_var)

    # Keep current selections that are still valid
    current_selected <- input$explanatory_vars
    valid_selected <- intersect(current_selected, explanatory_choices)

    # If no valid selections, select all available
    if (length(valid_selected) == 0) {
      valid_selected <- explanatory_choices
    }

    updateSelectInput(
      session, "explanatory_vars",
      choices = explanatory_choices,
      selected = valid_selected
    )
  })

  # -------------------------------------------------------------------------
  # Data preview
  # -------------------------------------------------------------------------

  output$data_preview <- renderDT({
    req(rv$data)
    datatable(
      rv$data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "frtip",
        language = list(
          search = "検索:",
          lengthMenu = "表示: _MENU_ 件",
          info = "_TOTAL_ 件中 _START_ - _END_ 件表示",
          paginate = list(previous = "前", `next` = "次")
        )
      ),
      class = "stripe hover",
      rownames = FALSE
    )
  })

  # -------------------------------------------------------------------------
  # Analysis execution
  # -------------------------------------------------------------------------

  observeEvent(input$run_analysis, {
    req(rv$data, input$target_var, input$explanatory_vars)

    # Validation: target must not be in explanatory variables
    if (input$target_var %in% input$explanatory_vars) {
      showNotification(
        "目的変数と説明変数に同じ変数が選択されています",
        type = "error"
      )
      return()
    }

    # Validation: minimum explanatory variables
    if (length(input$explanatory_vars) < MIN_EXPLANATORY_VARS) {
      showNotification(
        sprintf("hierNetには%d個以上の説明変数が必要です", MIN_EXPLANATORY_VARS),
        type = "error"
      )
      return()
    }

    if (length(input$explanatory_vars) > MAX_RECOMMENDED_VARS) {
      showNotification(
        sprintf("変数が多すぎます。%d個以下を推奨します", MAX_RECOMMENDED_VARS),
        type = "warning"
      )
    }

    withProgress(message = "hierNet分析実行中...", value = 0, {
      tryCatch({
        incProgress(0.05, detail = "データ準備中")

        y <- rv$data[[input$target_var]]
        X <- as.matrix(rv$data[, input$explanatory_vars, drop = FALSE])

        # Validate data
        validated <- validate_data(X, y)

        if (length(validated$issues) > 0) {
          for (issue in validated$issues) {
            showNotification(issue, type = "warning")
          }
        }

        if (!validated$valid) {
          showNotification("有効なデータがありません", type = "error")
          return()
        }

        X <- validated$X
        y <- validated$y

        # Quadratic model requires >= 3 distinct levels: for a 2-level
        # variable X² is perfectly collinear with X and cannot be estimated
        if (input$model_type == "quadratic") {
          n_unique <- apply(X, 2, function(col) length(unique(col)))
          low_level_vars <- colnames(X)[n_unique < 3]
          if (length(low_level_vars) > 0) {
            showNotification(
              sprintf("2水準以下の変数はX²がXと完全共線のため2次項を推定できません: %s",
                      paste(low_level_vars, collapse = ", ")),
              type = "warning", duration = 10
            )
          }
        }

        incProgress(0.25, detail = "交差検証実行中（時間がかかります）")

        # Run analysis
        analysis_result <- run_hiernet_analysis(
          X = X,
          y = y,
          strong = (input$hierarchy_type == "strong"),
          nlam = input$nlam,
          nfolds = input$nfolds,
          model_type = input$model_type,
          seed = input$cv_seed
        )

        # Surface analysis warnings (NaN/Inf replacement, equation self-check, ...)
        for (w in analysis_result$warnings) {
          showNotification(w, type = "warning", duration = 10)
        }

        incProgress(0.40, detail = "評価指標計算中")

        # Compute metrics (active-term counts use hierNet's own selection,
        # not the OLS-refit display values — see main_effects_active/
        # interaction_matrix_active in run_hiernet_analysis)
        active_counts <- count_active_coefficients(
          analysis_result$main_effects_active,
          analysis_result$interaction_matrix_active
        )

        n_params <- active_counts$n_main + active_counts$n_interaction +
          active_counts$n_quadratic
        metrics <- compute_metrics(y, analysis_result$predictions, n_params)

        if (is.finite(metrics$r_squared %||% NA_real_) && metrics$r_squared < 0) {
          showNotification(
            "R²が負です: モデルの予測性能が平均値予測を下回っています。結果の使用は推奨されません",
            type = "error", duration = 10
          )
        }

        incProgress(0.20, detail = "結果整理中")

        # Store results
        rv$analysis <- list(
          y = y,
          X = X,
          var_names = colnames(X),
          target_var = input$target_var,
          mean_y = analysis_result$mean_y,
          cv_rmse = analysis_result$cv_rmse,
          equation_check = analysis_result$equation_check,
          predictions = analysis_result$predictions,
          # Original scale coefficients
          intercept_orig = analysis_result$intercept_orig,
          main_effects_orig = analysis_result$main_effects_orig,
          interaction_matrix_orig = analysis_result$interaction_matrix_orig,
          # Standardized coefficients
          intercept_std = analysis_result$intercept_std,
          main_effects_std = analysis_result$main_effects_std,
          interaction_matrix_std = analysis_result$interaction_matrix_std,
          # Scaling info
          mx = analysis_result$mx,
          sx = analysis_result$sx,
          # Which terms hierNet actually selected (for "選択" indicators)
          main_effects_active = analysis_result$main_effects_active,
          interaction_matrix_active = analysis_result$interaction_matrix_active,
          # Model info
          fit = analysis_result$fit,
          cv_fit = analysis_result$cv_fit,
          best_lambda = analysis_result$best_lambda,
          metrics = metrics,
          n_main = active_counts$n_main,
          n_interaction = active_counts$n_interaction,
          n_quadratic = active_counts$n_quadratic,
          hierarchy_type = input$hierarchy_type,
          model_type = input$model_type
        )

        incProgress(0.10, detail = "完了")

        updateTabsetPanel(session, "result_tabs", selected = "result_tab")
        showNotification(
          sprintf("hierNet分析が完了しました（R²=%.3f, 主効果%d, 交互作用%d, 2次項%d）",
                  metrics$r_squared %||% 0, active_counts$n_main,
                  active_counts$n_interaction, active_counts$n_quadratic),
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        # User-friendly error messages
        err_msg <- e$message
        user_msg <- if (grepl("fold", err_msg, ignore.case = TRUE)) {
          "サンプル数が少なすぎます。CV fold数を減らしてください"
        } else if (grepl("singular|collinear", err_msg, ignore.case = TRUE)) {
          "変数間に完全共線性があります。変数を見直してください"
        } else if (grepl("memory|allocate", err_msg, ignore.case = TRUE)) {
          "メモリ不足です。変数数を減らしてください"
        } else {
          sprintf("分析エラー: %s", err_msg)
        }
        showNotification(user_msg, type = "error", duration = 10)
        message("hierNet Error: ", err_msg)
      })
    })
  })

  # -------------------------------------------------------------------------
  # Metrics display
  # -------------------------------------------------------------------------

  output$metrics_display <- renderUI({
    req(rv$analysis)
    res <- rv$analysis
    m <- res$metrics

    r2_class <- get_r2_class(m$r_squared)

    # Helper for formatting potentially NA values
    fmt <- function(x, digits = 4) {
      if (is.null(x) || is.na(x)) "N/A" else sprintf("%.*f", digits, x)
    }

    tagList(
      div(class = "metric-box",
          div(class = "metric-label", "決定係数 R²"),
          div(class = paste("metric-value", r2_class), fmt(m$r_squared))),
      div(class = "metric-box",
          div(class = "metric-label", "調整済み R²"),
          div(class = "metric-value", fmt(m$adj_r_squared))),
      div(class = "metric-box",
          div(class = "metric-label", "RMSE"),
          div(class = "metric-value", fmt(m$rmse))),
      div(class = "metric-box",
          div(class = "metric-label", "CV-RMSE (交差検証)"),
          div(class = "metric-value", fmt(res$cv_rmse))),
      div(class = "metric-box",
          div(class = "metric-label", "MAE"),
          div(class = "metric-value", fmt(m$mae))),
      div(class = "metric-box",
          div(class = "metric-label", "選択主効果数"),
          div(class = "metric-value", res$n_main)),
      div(class = "metric-box",
          div(class = "metric-label", "選択交互作用数"),
          div(class = "metric-value", style = sprintf("color: %s;", COLORS$accent_purple), res$n_interaction)),
      div(class = "metric-box",
          div(class = "metric-label", "選択2次項数"),
          div(class = "metric-value", style = sprintf("color: %s;", COLORS$accent_orange),
              res$n_quadratic %||% 0)),
      div(class = "metric-box",
          div(class = "metric-label", "最適 λ"),
          div(class = "metric-value", fmt(res$best_lambda))),
      div(class = "metric-box",
          div(class = "metric-label", "階層タイプ"),
          div(class = "metric-value", style = "font-size: 1rem;",
              if (res$hierarchy_type == "strong") "Strong" else "Weak")),
      div(class = "metric-box",
          div(class = "metric-label", "モデルタイプ"),
          div(class = "metric-value", style = "font-size: 0.9rem;",
              if (res$model_type == "interaction") "交互作用モデル" else "二次モデル"))
    )
  })

  # -------------------------------------------------------------------------
  # Scale description
  # -------------------------------------------------------------------------

  output$scale_description <- renderUI({
    scale <- input$coef_scale %||% "original"
    if (scale == "original") {
      "元単位: 主効果β×X、交互作用θ×(Xi-X̄i)(Xj-X̄j)。予測式として使用可能。"
    } else {
      "標準化: 変数間の相対的重要度を比較可能。1SD変化あたりの効果を表す。左辺は中心化した目的変数 (Y - Ȳ)。"
    }
  })

  # -------------------------------------------------------------------------
  # Equation display
  # -------------------------------------------------------------------------

  output$equation_display <- renderUI({
    req(rv$analysis)
    res <- rv$analysis
    scale <- input$coef_scale %||% "original"

    # Select coefficients based on scale (main_effects_orig/std and
    # interaction_matrix_orig/std are always populated by
    # run_hiernet_analysis, so no cross-scale fallback is needed here)
    if (scale == "original") {
      intercept <- res$intercept_orig %||% 0
      main_effects <- res$main_effects_orig
      interaction_matrix <- res$interaction_matrix_orig
    } else {
      intercept <- res$intercept_std %||% 0
      main_effects <- res$main_effects_std
      interaction_matrix <- res$interaction_matrix_std
    }

    # Ensure intercept is numeric
    intercept <- as.numeric(intercept)
    if (is.na(intercept)) intercept <- 0

    # Use the target name captured at analysis time (input$target_var may have
    # changed since). Standardized equation predicts centered Y: label as (Y - Ȳ)
    target_name <- res$target_var %||% input$target_var
    if (scale != "original" && !is.null(res$mean_y)) {
      target_name <- sprintf("(%s - %.4g)", target_name, res$mean_y)
    }

    equation <- build_equation(
      intercept,
      main_effects,
      interaction_matrix,
      res$var_names,
      target_name,
      scale = scale,
      mx = res$mx,
      main_active = res$main_effects_active,
      int_active = res$interaction_matrix_active
    )
    HTML(equation)
  })

  # -------------------------------------------------------------------------
  # Equation self-check badge (persistent — survives tab switches, unlike the
  # transient showNotification shown once at analysis time)
  # -------------------------------------------------------------------------

  output$equation_check_badge <- renderUI({
    req(rv$analysis)
    check <- rv$analysis$equation_check
    if (is.null(check)) return(NULL)

    if (isTRUE(check$ok)) {
      div(
        style = sprintf("color: %s; font-size: 0.75rem; margin-top: 0.25rem;", COLORS$accent_green),
        sprintf("✓ 検証済み: この式はモデルの予測を再現します（説明分散 %.3f%%）",
                100 * (check$r_squared %||% 1))
      )
    } else {
      div(
        style = sprintf("color: %s; font-size: 0.75rem; margin-top: 0.25rem; font-weight: 600;",
                        COLORS$accent_orange),
        sprintf("⚠ 近似値: この式はモデルの予測と完全には一致していません（説明分散 %.3f%%）。参考値として扱ってください",
                100 * max(check$r_squared %||% 0, 0))
      )
    }
  })

  # -------------------------------------------------------------------------
  # Coefficient table
  # -------------------------------------------------------------------------

  output$coef_table <- renderDT({
    req(rv$analysis)
    res <- rv$analysis
    scale <- input$coef_scale %||% "original"

    # main_effects_orig/std and interaction_matrix_orig/std are always
    # populated by run_hiernet_analysis, so no cross-scale fallback is needed
    if (scale == "original") {
      main_effects <- res$main_effects_orig
      interaction_matrix <- res$interaction_matrix_orig
    } else {
      main_effects <- res$main_effects_std
      interaction_matrix <- res$interaction_matrix_std
    }

    # Ensure numeric
    main_effects <- as.numeric(main_effects)
    names(main_effects) <- res$var_names
    main_active <- res$main_effects_active

    # Main effects. "選択" reflects hierNet's own selection (main_active),
    # not a threshold on the displayed (OLS-refit) value, since a term
    # hierNet zeroed exactly can carry small refit noise.
    main_df <- data.frame(
      変数 = names(main_effects),
      係数 = main_effects,
      タイプ = "主効果",
      選択 = ifelse(main_active[names(main_effects)], "✓", ""),
      stringsAsFactors = FALSE
    )

    # Interactions (using utility function). Row inclusion is still
    # magnitude-based (DISPLAY_THRESHOLD) so the table doesn't fill with
    # negligible refit noise on terms hierNet zeroed; "選択" is mask-based.
    interactions <- extract_interactions(interaction_matrix, res$var_names, DISPLAY_THRESHOLD)

    if (nrow(interactions) > 0) {
      # Distinguish between quadratic (var1 == var2) and interaction (var1 != var2)
      is_quadratic <- interactions$var1 == interactions$var2
      int_active <- res$interaction_matrix_active
      selected <- mapply(function(v1, v2) isTRUE(int_active[v1, v2]),
                        interactions$var1, interactions$var2)
      int_df <- data.frame(
        変数 = ifelse(is_quadratic,
                     paste0(interactions$var1, "²"),
                     paste0(interactions$var1, " × ", interactions$var2)),
        係数 = interactions$coefficient,
        タイプ = ifelse(is_quadratic, "2次項", "交互作用"),
        選択 = ifelse(selected, "✓", ""),
        stringsAsFactors = FALSE
      )
      coef_df <- rbind(main_df, int_df)
    } else {
      coef_df <- main_df
    }

    # Sort by absolute coefficient
    coef_df <- coef_df[order(-abs(coef_df$係数)), ]
    rownames(coef_df) <- NULL

    datatable(
      coef_df,
      options = list(pageLength = 15, dom = "frtip", ordering = TRUE),
      rownames = FALSE,
      selection = "none"
    ) |>
      formatRound(columns = "係数", digits = 4) |>
      formatStyle(columns = "選択", color = COLORS$accent_green, fontWeight = "bold") |>
      formatStyle(
        columns = "タイプ",
        color = styleEqual(c("主効果", "交互作用", "2次項"),
                          c(COLORS$accent_blue, COLORS$accent_purple, COLORS$accent_orange))
      )
  })

  # -------------------------------------------------------------------------
  # Prediction plot
  # -------------------------------------------------------------------------

  output$prediction_plot <- renderPlotly({
    req(rv$analysis)
    req(rv$analysis$metrics)
    res <- rv$analysis

    # Prepare data with pre-computed tooltip text
    df <- data.frame(
      actual = res$y,
      predicted = res$predictions,
      residual = res$metrics$residuals,
      stringsAsFactors = FALSE
    )
    df$tooltip <- sprintf("実測: %.2f<br>予測: %.2f<br>残差: %.2f",
                          df$actual, df$predicted, df$residual)

    # Handle edge case: single observation
    if (nrow(df) < 2) {
      p <- ggplot(df, aes(x = actual, y = predicted)) +
        geom_point(aes(text = tooltip), color = COLORS$accent_purple, size = 5) +
        labs(x = "実測値", y = "予測値", title = "（データ点が少なすぎます）") +
        theme_hiernet()
      return(ggplotly(p, tooltip = "text") |> layout_hiernet())
    }

    range_min <- min(c(df$actual, df$predicted)) * 0.95
    range_max <- max(c(df$actual, df$predicted)) * 1.05

    # Handle identical values (zero range)
    if (abs(range_max - range_min) < .Machine$double.eps) {
      range_min <- range_min - 1
      range_max <- range_max + 1
    }

    # Safe regression line calculation
    reg_coef <- tryCatch({
      fit <- lm(predicted ~ actual, data = df)
      coef(fit)
    }, error = function(e) c(0, 1))

    r2_display <- if (is.na(res$metrics$r_squared)) "NA" else sprintf("%.3f", res$metrics$r_squared)
    ann_text <- sprintf("R² = %s\nRMSE = %.3f", r2_display, res$metrics$rmse)

    p <- ggplot(df, aes(x = actual, y = predicted)) +
      geom_abline(intercept = 0, slope = 1, color = COLORS$border, linetype = "dashed", linewidth = 1) +
      geom_ribbon(
        data = data.frame(x = seq(range_min, range_max, length.out = 100)),
        aes(x = x, ymin = x * 0.9, ymax = x * 1.1),
        inherit.aes = FALSE,
        fill = COLORS$accent_purple, alpha = 0.08
      )

    # Only add regression line if coefficients are valid
    if (!any(is.na(reg_coef))) {
      p <- p + geom_abline(intercept = reg_coef[1], slope = reg_coef[2],
                           color = COLORS$accent_green, linewidth = 1.0)
    }

    p <- p +
      geom_point(aes(text = tooltip), color = COLORS$accent_purple, alpha = 0.7, size = 3) +
      annotate(
        "text",
        x = range_min + 0.03 * (range_max - range_min),
        y = range_max - 0.03 * (range_max - range_min),
        label = ann_text, hjust = 0, vjust = 1, size = 3.5, color = COLORS$text_primary
      ) +
      labs(x = "実測値", y = "予測値") +
      theme_hiernet() +
      coord_fixed(ratio = 1, xlim = c(range_min, range_max), ylim = c(range_min, range_max))

    ggplotly(p, tooltip = "text") |>
      layout_hiernet() |>
      layout(legend = list(orientation = "h"))
  })

  # -------------------------------------------------------------------------
  # Residual plot
  # -------------------------------------------------------------------------

  output$residual_plot <- renderPlotly({
    req(rv$analysis)

    df <- data.frame(residual = rv$analysis$metrics$residuals)

    p <- ggplot(df, aes(x = residual)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = COLORS$accent_purple, alpha = 0.6, color = COLORS$bg_primary) +
      geom_density(color = COLORS$accent_green, linewidth = 1) +
      geom_vline(xintercept = 0, color = COLORS$accent_red, linetype = "dashed") +
      labs(x = "残差", y = "密度") +
      theme_hiernet()

    ggplotly(p) |> layout_hiernet()
  })

  # -------------------------------------------------------------------------
  # CV plot
  # -------------------------------------------------------------------------

  output$cv_plot <- renderPlot({
    req(rv$analysis)
    req(rv$analysis$cv_fit)
    cv_fit <- rv$analysis$cv_fit

    # Validate CV fit data
    if (!all(c("lamlist", "cv", "cv.se", "lamhat") %in% names(cv_fit))) {
      plot.new()
      text(0.5, 0.5, "CV データが不完全です", col = COLORS$text_muted)
      return(NULL)
    }
    if (length(cv_fit$lamlist) != length(cv_fit$cv)) {
      plot.new()
      text(0.5, 0.5, "CV データ長が不一致です", col = COLORS$text_muted)
      return(NULL)
    }

    par(
      bg = "transparent",
      fg = COLORS$text_primary,
      col.axis = COLORS$text_muted,
      col.lab = COLORS$text_primary,
      col.main = COLORS$text_primary,
      mar = c(5, 4, 2, 2)
    )

    plot(
      cv_fit$lamlist, cv_fit$cv,
      type = "b", pch = 19, col = COLORS$accent_purple,
      xlab = "Lambda", ylab = "Cross-Validation Error", main = ""
    )

    # Only draw error bars if cv.se is valid
    if (!is.null(cv_fit$cv.se) && length(cv_fit$cv.se) == length(cv_fit$cv)) {
      arrows(
        cv_fit$lamlist, cv_fit$cv - cv_fit$cv.se,
        cv_fit$lamlist, cv_fit$cv + cv_fit$cv.se,
        length = 0.02, angle = 90, code = 3, col = COLORS$accent_blue
      )
    }

    abline(v = cv_fit$lamhat, col = COLORS$accent_green, lty = 2, lwd = 2)

    legend(
      "topright",
      legend = paste("最適λ =", round(cv_fit$lamhat, 4)),
      col = COLORS$accent_green, lty = 2, lwd = 2,
      text.col = COLORS$text_primary,
      bg = COLORS$bg_secondary,
      box.col = COLORS$border
    )
  }, bg = "transparent")

  # -------------------------------------------------------------------------
  # Main effect plot
  # -------------------------------------------------------------------------

  output$main_effect_plot <- renderPlotly({
    req(rv$analysis)
    res <- rv$analysis
    scale <- input$coef_scale %||% "original"

    # main_effects_orig/std always populated by run_hiernet_analysis
    if (scale == "original") {
      main_effects <- res$main_effects_orig
    } else {
      main_effects <- res$main_effects_std
    }
    x_label <- if (scale == "original") "係数 (元単位)" else "係数 (標準化)"

    # Ensure numeric and named
    coef_values <- as.numeric(main_effects)
    var_names <- names(main_effects) %||% res$var_names

    df <- data.frame(
      variable = var_names,
      coefficient = coef_values,
      stringsAsFactors = FALSE
    ) |>
      dplyr::mutate(abs_coef = abs(coefficient)) |>
      dplyr::arrange(abs_coef) |>
      dplyr::mutate(variable = factor(variable, levels = variable))

    p <- ggplot(df, aes(x = coefficient, y = variable)) +
      geom_col(aes(fill = coefficient > 0), alpha = 0.8, width = 0.7) +
      geom_vline(xintercept = 0, color = COLORS$border, linewidth = 0.5) +
      scale_fill_manual(values = c("TRUE" = COLORS$accent_green, "FALSE" = COLORS$accent_red), guide = "none") +
      labs(x = x_label, y = "") +
      theme_hiernet() +
      theme(panel.grid.major.y = element_blank(), axis.text = element_text(color = COLORS$text_primary, size = 11))

    ggplotly(p) |> layout_hiernet()
  })

  # -------------------------------------------------------------------------
  # Interaction heatmap
  # -------------------------------------------------------------------------

  output$interaction_heatmap <- renderPlotly({
    req(rv$analysis)
    res <- rv$analysis
    scale <- input$coef_scale %||% "original"

    # interaction_matrix_orig/std always populated by run_hiernet_analysis
    if (scale == "original") {
      int_mat <- res$interaction_matrix_orig
    } else {
      int_mat <- res$interaction_matrix_std
    }
    var_names <- res$var_names
    legend_title <- if (scale == "original") "係数\n(元単位)" else "係数\n(標準化)"

    # Validate matrix
    if (is.null(int_mat) || !is.matrix(int_mat)) {
      p <- ncol(res$X) %||% length(var_names)
      int_mat <- matrix(0, nrow = p, ncol = p)
    }

    df <- expand.grid(var1 = var_names, var2 = var_names, stringsAsFactors = FALSE)
    df$coef_value <- as.numeric(as.vector(int_mat))

    p <- ggplot(df, aes(x = var1, y = var2, fill = coef_value)) +
      geom_tile(color = COLORS$bg_tertiary, linewidth = 0.5) +
      geom_text(
        aes(label = ifelse(abs(coef_value) > DISPLAY_THRESHOLD, sprintf("%.2f", coef_value), "")),
        color = COLORS$text_primary, size = 3
      ) +
      scale_fill_gradient2(
        low = COLORS$accent_red, mid = COLORS$bg_primary, high = COLORS$accent_purple,
        midpoint = 0, name = legend_title
      ) +
      labs(x = "", y = "") +
      theme_hiernet() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_text(color = COLORS$text_primary, size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      coord_fixed()

    ggplotly(p) |> layout_hiernet()
  })

  # -------------------------------------------------------------------------
  # Interaction table
  # -------------------------------------------------------------------------

  output$interaction_table <- renderDT({
    req(rv$analysis)
    res <- rv$analysis
    scale <- input$coef_scale %||% "original"

    # interaction_matrix_orig/std always populated by run_hiernet_analysis
    if (scale == "original") {
      interaction_matrix <- res$interaction_matrix_orig
    } else {
      interaction_matrix <- res$interaction_matrix_std
    }

    int_df <- extract_interactions(interaction_matrix, res$var_names, DISPLAY_THRESHOLD)

    if (nrow(int_df) == 0) {
      return(datatable(
        data.frame(メッセージ = "選択された交互作用・2次項はありません"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }

    # Add type column (quadratic vs interaction)
    is_quadratic <- int_df$var1 == int_df$var2
    int_df$タイプ <- ifelse(is_quadratic, "2次項", "交互作用")

    # Format variable display
    int_df$項 <- ifelse(is_quadratic,
                       paste0(int_df$var1, "²"),
                       paste0(int_df$var1, " × ", int_df$var2))

    col_name <- if (scale == "original") "係数(元単位)" else "係数(標準化)"
    result_df <- data.frame(
      項 = int_df$項,
      タイプ = int_df$タイプ,
      係数 = int_df$coefficient,
      stringsAsFactors = FALSE
    )
    names(result_df)[3] <- col_name

    datatable(
      result_df,
      options = list(pageLength = 10, dom = "t", ordering = TRUE),
      rownames = FALSE
    ) |>
      formatRound(columns = col_name, digits = 4) |>
      formatStyle(
        columns = "タイプ",
        color = styleEqual(c("交互作用", "2次項"), c(COLORS$accent_purple, COLORS$accent_orange))
      )
  })
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)
