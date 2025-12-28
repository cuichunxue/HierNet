# ============================================================================
# hierNet アプリケーション検証スクリプト
# 全5パターンのサンプルデータで機能をテスト
# ============================================================================

# ソースファイルから関数を抽出（Shiny部分を除く）
source_functions_only <- function() {
  # 定数
  COEF_ZERO_THRESHOLD <<- 1e-8
  DISPLAY_THRESHOLD <<- 1e-6
  MIN_EXPLANATORY_VARS <<- 2
  MAX_RECOMMENDED_VARS <<- 15
  DEFAULT_SAMPLE_SIZE <<- 200
  RANDOM_SEED <<- 42
  MIN_SAMPLE_SIZE <<- 10

  `%||%` <<- function(x, y) if (is.null(x)) y else x
}

source_functions_only()

# ライブラリ読み込み
suppressPackageStartupMessages({
  library(hierNet)
  library(dplyr)
})

# ============================================================================
# テスト用関数定義（Reg.Rから抽出）
# ============================================================================

validate_data <- function(X, y) {
  issues <- character(0)
  complete_idx <- complete.cases(X, y)
  n_missing <- sum(!complete_idx)

  if (n_missing > 0) {
    issues <- c(issues, sprintf("%d行の欠損値を除外", n_missing))
    X <- X[complete_idx, , drop = FALSE]
    y <- y[complete_idx]
  }

  const_cols <- apply(X, 2, function(col) {
    v <- var(col, na.rm = TRUE)
    is.na(v) || v < .Machine$double.eps
  })
  if (any(const_cols, na.rm = TRUE)) {
    const_names <- colnames(X)[const_cols]
    issues <- c(issues, sprintf("定数列を検出: %s", paste(const_names, collapse = ", ")))
    X <- X[, !const_cols, drop = FALSE]
  }

  if (nrow(X) < MIN_SAMPLE_SIZE) {
    issues <- c(issues, sprintf("サンプルサイズが小さすぎます（n=%d）", nrow(X)))
  }

  if (ncol(X) > 1 && nrow(X) > 1) {
    cor_mat <- tryCatch(cor(X), error = function(e) NULL)
    if (!is.null(cor_mat)) {
      diag(cor_mat) <- 0
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

extract_interactions <- function(int_mat, var_names, threshold = DISPLAY_THRESHOLD) {
  empty_result <- data.frame(
    var1 = character(0),
    var2 = character(0),
    coefficient = numeric(0),
    stringsAsFactors = FALSE
  )

  n <- nrow(int_mat)
  if (is.null(n) || n < 2) return(empty_result)

  idx <- which(upper.tri(int_mat), arr.ind = TRUE)
  coefficients <- int_mat[upper.tri(int_mat)]
  keep <- abs(coefficients) > threshold

  if (!any(keep)) return(empty_result)

  data.frame(
    var1 = var_names[idx[keep, 1]],
    var2 = var_names[idx[keep, 2]],
    coefficient = coefficients[keep],
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(desc(abs(coefficient)))
}

compute_metrics <- function(y, predictions, n_params) {
  n <- length(y)
  residuals <- y - predictions

  ss_res <- sum(residuals^2)
  ss_tot <- sum((y - mean(y))^2)

  if (ss_tot < .Machine$double.eps) {
    warning("目的変数の分散がゼロです")
    r_squared <- NA_real_
  } else {
    r_squared <- 1 - ss_res / ss_tot
    r_squared <- max(0, min(1, r_squared))
  }

  df_res <- n - n_params - 1
  adj_r_squared <- if (df_res > 0 && !is.na(r_squared)) {
    1 - (1 - r_squared) * (n - 1) / df_res
  } else {
    NA_real_
  }

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

count_active_coefficients <- function(main_effects, int_mat) {
  list(
    n_main = sum(abs(main_effects) > COEF_ZERO_THRESHOLD),
    n_interaction = sum(abs(int_mat[upper.tri(int_mat)]) > COEF_ZERO_THRESHOLD)
  )
}

generate_sample_data <- function(
    pattern = c("main_linear", "strong_interact", "weak_interact", "quadratic", "low_signal_noise"),
    n = DEFAULT_SAMPLE_SIZE,
    seed = RANDOM_SEED
) {
  pattern <- match.arg(pattern)
  set.seed(seed)

  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- c("X1", "X2", "X3", "X4", "X5")

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

  Y <- coeffs$intercept +
    as.vector(X %*% coeffs$main) +
    coeffs$int12 * X[, 1] * X[, 2] +
    coeffs$int23 * X[, 2] * X[, 3] +
    coeffs$quad1 * X[, 1]^2 +
    coeffs$quad2 * X[, 2]^2 +
    rnorm(n, 0, coeffs$noise_sd)

  data.frame(
    pattern = pattern,
    Y = round(as.vector(Y), 2),
    X1 = round(X[, 1], 3),
    X2 = round(X[, 2], 3),
    X3 = round(X[, 3], 3),
    X4 = round(X[, 4], 3),
    X5 = round(X[, 5], 3)
  )
}

run_hiernet_analysis <- function(
    X, y,
    strong = TRUE,
    nlam = 20,
    nfolds = 5,
    standardize = TRUE,
    center = TRUE
) {
  if (!is.matrix(X)) X <- as.matrix(X)
  if (nrow(X) < nfolds) {
    stop(sprintf("サンプル数(%d)がCV fold数(%d)より少ないです", nrow(X), nfolds))
  }

  path_fit <- tryCatch(
    hierNet.path(x = X, y = y, nlam = nlam, strong = strong,
                 standardize = standardize, center = center),
    error = function(e) stop(sprintf("hierNet.path failed: %s", e$message))
  )

  cv_fit <- tryCatch(
    hierNet.cv(fit = path_fit, x = X, y = y, nfolds = nfolds),
    error = function(e) stop(sprintf("Cross-validation failed: %s", e$message))
  )

  best_lambda <- cv_fit$lamhat
  if (is.null(best_lambda) || is.na(best_lambda) || best_lambda <= 0) {
    stop("交差検証で最適なλを見つけられませんでした")
  }

  final_fit <- tryCatch(
    hierNet(x = X, y = y, lam = best_lambda, strong = strong,
            standardize = standardize, center = center),
    error = function(e) stop(sprintf("Final model fitting failed: %s", e$message))
  )

  if (is.null(final_fit$bp) || is.null(final_fit$bn)) {
    stop("モデルフィッティングの結果が不正です")
  }

  main_effects <- final_fit$bp - final_fit$bn
  names(main_effects) <- colnames(X)

  interaction_matrix <- final_fit$th
  if (!is.null(interaction_matrix)) {
    rownames(interaction_matrix) <- colnames(X)
    colnames(interaction_matrix) <- colnames(X)
  } else {
    p <- ncol(X)
    interaction_matrix <- matrix(0, nrow = p, ncol = p)
    rownames(interaction_matrix) <- colnames(X)
    colnames(interaction_matrix) <- colnames(X)
  }

  predictions <- tryCatch(
    as.vector(predict(final_fit, newx = X)),
    error = function(e) {
      warning("予測計算に失敗しました。フィット値を使用します")
      as.vector(final_fit$yhat)
    }
  )

  col_means <- colMeans(X, na.rm = TRUE)
  intercept <- mean(y, na.rm = TRUE) - sum(col_means * main_effects)
  if (is.na(intercept)) intercept <- 0

  list(
    fit = final_fit,
    cv_fit = cv_fit,
    best_lambda = best_lambda,
    main_effects = main_effects,
    interaction_matrix = interaction_matrix,
    predictions = predictions,
    intercept = intercept
  )
}

# ============================================================================
# テスト実行
# ============================================================================

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("hierNet パターン別検証テスト\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

patterns <- c("main_linear", "strong_interact", "weak_interact", "quadratic", "low_signal_noise")

results_summary <- list()

for (pattern in patterns) {
  cat("-" |> rep(60) |> paste(collapse = ""), "\n")
  cat(sprintf("パターン: %s\n", pattern))
  cat("-" |> rep(60) |> paste(collapse = ""), "\n")

  tryCatch({
    # 1. データ生成
    df <- generate_sample_data(pattern = pattern)
    cat(sprintf("  データ生成: OK (n=%d, p=%d)\n", nrow(df), ncol(df) - 2))

    # 2. データ検証
    y <- df$Y
    X <- as.matrix(df[, c("X1", "X2", "X3", "X4", "X5")])
    validated <- validate_data(X, y)
    cat(sprintf("  データ検証: %s\n", if (validated$valid) "OK" else "NG"))
    if (length(validated$issues) > 0) {
      for (issue in validated$issues) {
        cat(sprintf("    - %s\n", issue))
      }
    }

    # 3. Strong hierarchy分析
    cat("  Strong hierarchy分析...\n")
    result_strong <- run_hiernet_analysis(X, y, strong = TRUE, nlam = 15, nfolds = 5)
    active_strong <- count_active_coefficients(result_strong$main_effects, result_strong$interaction_matrix)
    metrics_strong <- compute_metrics(y, result_strong$predictions,
                                       active_strong$n_main + active_strong$n_interaction)
    cat(sprintf("    R² = %.4f, RMSE = %.4f\n", metrics_strong$r_squared, metrics_strong$rmse))
    cat(sprintf("    選択: 主効果=%d, 交互作用=%d, λ=%.4f\n",
                active_strong$n_main, active_strong$n_interaction, result_strong$best_lambda))

    # 4. Weak hierarchy分析
    cat("  Weak hierarchy分析...\n")
    result_weak <- run_hiernet_analysis(X, y, strong = FALSE, nlam = 15, nfolds = 5)
    active_weak <- count_active_coefficients(result_weak$main_effects, result_weak$interaction_matrix)
    metrics_weak <- compute_metrics(y, result_weak$predictions,
                                     active_weak$n_main + active_weak$n_interaction)
    cat(sprintf("    R² = %.4f, RMSE = %.4f\n", metrics_weak$r_squared, metrics_weak$rmse))
    cat(sprintf("    選択: 主効果=%d, 交互作用=%d, λ=%.4f\n",
                active_weak$n_main, active_weak$n_interaction, result_weak$best_lambda))

    # 5. 交互作用抽出テスト
    interactions <- extract_interactions(result_strong$interaction_matrix, colnames(X))
    cat(sprintf("  交互作用抽出: %d件\n", nrow(interactions)))
    if (nrow(interactions) > 0) {
      top3 <- head(interactions, 3)
      for (i in seq_len(nrow(top3))) {
        cat(sprintf("    %s × %s: %.4f\n", top3$var1[i], top3$var2[i], top3$coefficient[i]))
      }
    }

    # 結果保存
    results_summary[[pattern]] <- list(
      status = "OK",
      strong = list(r2 = metrics_strong$r_squared, n_main = active_strong$n_main,
                    n_int = active_strong$n_interaction),
      weak = list(r2 = metrics_weak$r_squared, n_main = active_weak$n_main,
                  n_int = active_weak$n_interaction)
    )

    cat("  ステータス: OK\n\n")

  }, error = function(e) {
    cat(sprintf("  エラー: %s\n\n", e$message))
    results_summary[[pattern]] <<- list(status = "ERROR", message = e$message)
  })
}

# ============================================================================
# エッジケーステスト
# ============================================================================

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("エッジケーステスト\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

# Test 1: 小サンプル
cat("1. 小サンプル (n=20)...\n")
tryCatch({
  df_small <- generate_sample_data("strong_interact", n = 20)
  y <- df_small$Y
  X <- as.matrix(df_small[, c("X1", "X2", "X3", "X4", "X5")])
  validated <- validate_data(X, y)
  cat(sprintf("   検証: %s\n", if (validated$valid) "OK" else "NG"))
  if (length(validated$issues) > 0) {
    cat(sprintf("   警告: %s\n", paste(validated$issues, collapse = "; ")))
  }
  result <- run_hiernet_analysis(X, y, strong = TRUE, nlam = 10, nfolds = 3)
  cat(sprintf("   分析: OK (λ=%.4f)\n", result$best_lambda))
}, error = function(e) {
  cat(sprintf("   エラー: %s\n", e$message))
})

# Test 2: 欠損値を含むデータ
cat("\n2. 欠損値を含むデータ...\n")
tryCatch({
  df_na <- generate_sample_data("main_linear", n = 100)
  df_na$X1[1:5] <- NA
  df_na$Y[6:8] <- NA
  y <- df_na$Y
  X <- as.matrix(df_na[, c("X1", "X2", "X3", "X4", "X5")])
  validated <- validate_data(X, y)
  cat(sprintf("   検証: %s (除外後 n=%d)\n", if (validated$valid) "OK" else "NG", validated$n))
  if (length(validated$issues) > 0) {
    cat(sprintf("   警告: %s\n", paste(validated$issues, collapse = "; ")))
  }
}, error = function(e) {
  cat(sprintf("   エラー: %s\n", e$message))
})

# Test 3: 定数列を含むデータ
cat("\n3. 定数列を含むデータ...\n")
tryCatch({
  df_const <- generate_sample_data("main_linear", n = 100)
  df_const$X5 <- 0  # 定数列
  y <- df_const$Y
  X <- as.matrix(df_const[, c("X1", "X2", "X3", "X4", "X5")])
  validated <- validate_data(X, y)
  cat(sprintf("   検証: %s (除外後 p=%d)\n", if (validated$valid) "OK" else "NG", validated$p))
  if (length(validated$issues) > 0) {
    cat(sprintf("   警告: %s\n", paste(validated$issues, collapse = "; ")))
  }
}, error = function(e) {
  cat(sprintf("   エラー: %s\n", e$message))
})

# Test 4: 高相関変数
cat("\n4. 高相関変数...\n")
tryCatch({
  df_cor <- generate_sample_data("main_linear", n = 100)
  df_cor$X5 <- df_cor$X1 + rnorm(100, 0, 0.001)  # X1とほぼ同一
  y <- df_cor$Y
  X <- as.matrix(df_cor[, c("X1", "X2", "X3", "X4", "X5")])
  validated <- validate_data(X, y)
  cat(sprintf("   検証: %s\n", if (validated$valid) "OK" else "NG"))
  if (length(validated$issues) > 0) {
    cat(sprintf("   警告: %s\n", paste(validated$issues, collapse = "; ")))
  }
}, error = function(e) {
  cat(sprintf("   エラー: %s\n", e$message))
})

# Test 5: ゼロを含む目的変数
cat("\n5. ゼロを含む目的変数（MAPE計算）...\n")
tryCatch({
  df_zero <- generate_sample_data("main_linear", n = 100)
  df_zero$Y[1:10] <- 0
  y <- df_zero$Y
  X <- as.matrix(df_zero[, c("X1", "X2", "X3", "X4", "X5")])
  result <- run_hiernet_analysis(X, y, strong = TRUE, nlam = 10, nfolds = 5)
  active <- count_active_coefficients(result$main_effects, result$interaction_matrix)
  metrics <- compute_metrics(y, result$predictions, active$n_main + active$n_interaction)
  cat(sprintf("   MAPE: %s\n", if (is.na(metrics$mape)) "NA (正常)" else sprintf("%.2f%%", metrics$mape)))
  cat(sprintf("   R²: %.4f\n", metrics$r_squared))
}, error = function(e) {
  cat(sprintf("   エラー: %s\n", e$message))
})

# ============================================================================
# サマリー
# ============================================================================

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("テスト結果サマリー\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

cat(sprintf("%-20s %-8s %-12s %-12s\n", "パターン", "状態", "Strong R²", "Weak R²"))
cat("-" |> rep(55) |> paste(collapse = ""), "\n")

for (pattern in patterns) {
  res <- results_summary[[pattern]]
  if (!is.null(res) && res$status == "OK") {
    cat(sprintf("%-20s %-8s %-12.4f %-12.4f\n",
                pattern, "OK",
                res$strong$r2, res$weak$r2))
  } else {
    cat(sprintf("%-20s %-8s\n", pattern, "ERROR"))
  }
}

cat("\n全テスト完了\n")
