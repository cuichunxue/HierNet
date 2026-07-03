# =============================================================================
# 係数計算の科学的検証スクリプト
# =============================================================================
# 目的: 標準化係数と元単位係数の計算が正しいかを検証する
#
# 検証方法:
# 1. 既知の真の係数でデータを生成
# 2. hierNetで推定
# 3. 手動計算した予測値とhierNet予測値を比較
# 4. 元単位への変換が正しいかを確認
#
# 重要な修正点:
# - hierNetの内部モデルは ŷ = b0 + xβ + (1/2)x^T Θ x
#   (Bien, Taylor & Tibshirani 2013, JASA "A Lasso for Hierarchical
#   Interactions" — hierNetパッケージの原論文)
#   Θは対称行列なので、非対角(i≠j)の実効係数は th[i,j] そのまま（対称性により
#   (1/2)(th_ij+th_ji) = th_ij となる）だが、対角(i=j)の実効係数は th[i,i]/2
#   （2次項だけは半分になる）。この0.5倍を忘れると2次項を含むモデルの
#   予測値が一致しない。
# - 切片は素朴式 mean(Y)-Σ(β_orig×X̄) では不正確（中心化積の平均は共分散
#   ≠0）。predict()との差分から実測校正する。
# =============================================================================

library(hierNet)

cat("\n")
cat("==============================================================\n")
cat("           係数計算の科学的検証\n")
cat("==============================================================\n\n")

# 表示用/校正用の共通ヘルパー:
#  theta_orig: 対角がすでに th[i,i]/2 に調整済みの p×p 行列
#  戻り値: 切片を除いた線形予測子 Σβ×X + Σθ_ij×(Xi-mxi)(Xj-mxj)（i=jも含む）
eq_linear_predictor <- function(X, beta_orig, theta_orig, mx) {
  p <- ncol(X)
  Xc <- sweep(X, 2, mx)
  lp <- as.vector(X %*% beta_orig)
  for (i in seq_len(p)) {
    for (j in i:p) {
      th_ij <- theta_orig[i, j]
      if (is.finite(th_ij) && abs(th_ij) > 1e-15) {
        lp <- lp + if (i == j) th_ij * Xc[, i]^2 else th_ij * Xc[, i] * Xc[, j]
      }
    }
  }
  lp
}

# th(標準化スケール, hierNet生出力)を元単位・実効係数（対角0.5倍済み）に変換
to_effective_theta_orig <- function(th_std, sx) {
  th_std_eff <- th_std
  diag(th_std_eff) <- diag(th_std_eff) / 2
  th_std_eff / outer(sx, sx)
}

run_check <- function(label, fit, X, y, true_desc) {
  mx <- fit$mx; sx <- fit$sx; my <- mean(y)
  beta_orig <- (fit$bp - fit$bn) / sx
  theta_orig <- to_effective_theta_orig(fit$th, sx)

  pred_hiernet <- as.vector(predict(fit, newx = X))
  lp <- eq_linear_predictor(X, beta_orig, theta_orig, mx)

  d <- pred_hiernet - lp
  intercept_cal <- mean(d)
  naive_intercept <- my - sum(beta_orig * mx)
  pred_manual <- intercept_cal + lp
  max_diff <- max(abs(pred_hiernet - pred_manual))

  cat(sprintf("【%s】\n", label))
  cat(true_desc)
  cat(sprintf("  校正切片 = %.4f / 素朴切片 = %.4f（差 = 共分散・分散補正分: %.4f）\n",
              intercept_cal, naive_intercept, intercept_cal - naive_intercept))
  cat(sprintf("  hierNet vs 手動(校正済み元単位): max|diff| = %.2e\n\n", max_diff))

  list(pass = max_diff < 1e-8, max_diff = max_diff,
       beta_orig = beta_orig, theta_orig = theta_orig,
       intercept = intercept_cal)
}

# -----------------------------------------------------------------------------
# テスト1: 主効果のみ（交互作用なし）
# -----------------------------------------------------------------------------

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("テスト1: 主効果のみのモデル\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

set.seed(12345)
n <- 500

X1 <- rnorm(n, mean = 100, sd = 15)
X2 <- rnorm(n, mean = 5, sd = 0.5)
X3 <- rnorm(n, mean = 50, sd = 10)

beta_true <- c(0.5, 3, 0.1)
intercept_true <- 10
Y <- intercept_true + 0.5*X1 + 3*X2 + 0.1*X3 + rnorm(n, sd = 2)

X <- cbind(X1, X2, X3)
colnames(X) <- c("X1", "X2", "X3")

cat("【真のモデル】\n")
cat(sprintf("  Y = %.1f + %.1f×X1 + %.1f×X2 + %.1f×X3 + ε\n\n",
            intercept_true, beta_true[1], beta_true[2], beta_true[3]))

# 主効果のみを狙うので diagonal=FALSE で2次項自体を推定対象から外す
fit1 <- hierNet(x = X, y = Y, lam = 0.1, strong = TRUE, diagonal = FALSE)

r1 <- run_check(
  "テスト1: 予測値の検証", fit1, X, Y,
  sprintf("  真の切片 = %.4f, 真のβ = [%.4f, %.4f, %.4f]\n",
          intercept_true, beta_true[1], beta_true[2], beta_true[3])
)
test1_pass <- r1$pass
cat(sprintf("  テスト1結果: %s\n\n", ifelse(test1_pass, "✓ PASS", "✗ FAIL")))


# -----------------------------------------------------------------------------
# テスト2: 交互作用を含むモデル
# -----------------------------------------------------------------------------

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("テスト2: 交互作用を含むモデル\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

set.seed(67890)
n <- 500

X1 <- rnorm(n, mean = 20, sd = 4)
X2 <- rnorm(n, mean = 10, sd = 2)

beta_true2 <- c(1.5, 2.0)
theta_true2 <- 0.2
intercept_true2 <- 5

Y2 <- intercept_true2 + 1.5*X1 + 2*X2 + 0.2*(X1 - 20)*(X2 - 10) + rnorm(n, sd = 1)

X2_mat <- cbind(X1, X2)
colnames(X2_mat) <- c("X1", "X2")

cat("【真のモデル】\n")
cat(sprintf("  Y = %.1f + %.1f×X1 + %.1f×X2 + %.1f×(X1-20)(X2-10) + ε\n\n",
            intercept_true2, beta_true2[1], beta_true2[2], theta_true2))

# アプリの「交互作用モデル」= diagonal=FALSE でフィット
fit2 <- hierNet(x = X2_mat, y = Y2, lam = 0.5, strong = TRUE, diagonal = FALSE)

r2 <- run_check(
  "テスト2: 予測値の検証", fit2, X2_mat, Y2,
  sprintf("  真の切片 = %.4f, 真のβ = [%.4f, %.4f], 真のθ = %.4f\n",
          intercept_true2, beta_true2[1], beta_true2[2], theta_true2)
)
cat(sprintf("  推定 θ_orig[1,2] = %.4f (真の値: %.4f)\n\n", r2$theta_orig[1,2], theta_true2))
test2_pass <- r2$pass
cat(sprintf("  テスト2結果: %s\n\n", ifelse(test2_pass, "✓ PASS", "✗ FAIL")))


# -----------------------------------------------------------------------------
# テスト3: 2次項を含むモデル
# -----------------------------------------------------------------------------

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("テスト3: 2次項を含むモデル\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

set.seed(11111)
n <- 500

X1 <- rnorm(n, mean = 10, sd = 3)
X2 <- rnorm(n, mean = 5, sd = 1)  # ダミー変数（hierNetは2変数以上必要）

beta_true3 <- c(3, 0)
theta_diag_true3 <- c(0.5, 0)
intercept_true3 <- 20

Y3 <- intercept_true3 + 3*X1 + 0.5*(X1 - 10)^2 + rnorm(n, sd = 2)

X3_mat <- cbind(X1, X2)
colnames(X3_mat) <- c("X1", "X2")

cat("【真のモデル】\n")
cat(sprintf("  Y = %.1f + %.1f×X1 + %.1f×(X1-10)² + ε\n\n",
            intercept_true3, beta_true3[1], theta_diag_true3[1]))

# アプリの「二次モデル」= diagonal=TRUE（デフォルト）でフィット
fit3 <- hierNet(x = X3_mat, y = Y3, lam = 1, strong = TRUE, diagonal = TRUE)

r3 <- run_check(
  "テスト3: 予測値の検証", fit3, X3_mat, Y3,
  sprintf("  真の切片 = %.4f, 真のβ1 = %.4f, 真のθ11(2次項) = %.4f\n",
          intercept_true3, beta_true3[1], theta_diag_true3[1])
)
cat(sprintf("  推定 θ_orig[1,1](2次項, 0.5倍後) = %.4f (真の値: %.4f)\n\n",
            r3$theta_orig[1,1], theta_diag_true3[1]))
test3_pass <- r3$pass
cat(sprintf("  テスト3結果: %s\n\n", ifelse(test3_pass, "✓ PASS", "✗ FAIL")))


# -----------------------------------------------------------------------------
# テスト4: 相関のある説明変数での切片校正（共分散補正の効果を確認）
# -----------------------------------------------------------------------------

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("テスト4: 相関のある説明変数 + 交互作用モデル\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

set.seed(22222)
n <- 500

X1 <- rnorm(n, mean = 20, sd = 4)
X2 <- 0.6 * X1 + rnorm(n, mean = 5, sd = 2)  # X1と相関

intercept_true4 <- 5
beta_true4 <- c(1.5, 2.0)
theta_true4 <- 0.3
Y4 <- intercept_true4 + 1.5*X1 + 2*X2 + 0.3*(X1 - mean(X1))*(X2 - mean(X2)) + rnorm(n, sd = 1)
X4_mat <- cbind(X1, X2)
colnames(X4_mat) <- c("X1", "X2")

cat(sprintf("【設定】Cor(X1,X2) = %.3f（相関あり → 共分散補正が必要な条件）\n\n",
            cor(X1, X2)))

fit4 <- hierNet(x = X4_mat, y = Y4, lam = 1, strong = TRUE, diagonal = FALSE)

cat(sprintf("【diagonal=FALSE の確認】th対角成分: [%.6f, %.6f]（0であるべき）\n\n",
            fit4$th[1,1], fit4$th[2,2]))

r4 <- run_check(
  "テスト4: 予測値の検証", fit4, X4_mat, Y4,
  sprintf("  真の切片 = %.4f, 真のβ = [%.4f, %.4f], 真のθ = %.4f\n",
          intercept_true4, beta_true4[1], beta_true4[2], theta_true4)
)
test4_pass <- r4$pass
cat(sprintf("  テスト4結果: %s\n", ifelse(test4_pass, "✓ PASS（式がpredict()を完全再現）", "✗ FAIL")))
cat("  ※「差」が非ゼロなら、素朴切片はその分だけ予測を外す\n\n")


# -----------------------------------------------------------------------------
# 総合結果
# -----------------------------------------------------------------------------

cat("==============================================================\n")
cat("                    総合検証結果\n")
cat("==============================================================\n\n")

cat(sprintf("  テスト1 (主効果のみ):         %s\n", ifelse(test1_pass, "✓ PASS", "✗ FAIL")))
cat(sprintf("  テスト2 (交互作用モデル):     %s\n", ifelse(test2_pass, "✓ PASS", "✗ FAIL")))
cat(sprintf("  テスト3 (二次モデル):         %s\n", ifelse(test3_pass, "✓ PASS", "✗ FAIL")))
cat(sprintf("  テスト4 (相関変数+交互作用):  %s\n\n", ifelse(test4_pass, "✓ PASS", "✗ FAIL")))

all_pass <- test1_pass && test2_pass && test3_pass && test4_pass
cat(sprintf("  総合結果: %s\n\n", ifelse(all_pass, "✓ 全テスト合格", "✗ 一部テスト失敗")))

cat("【検証された変換式】\n")
cat("  hierNetの内部モデル: ŷ = b0 + xβ + (1/2) x^T Θ x  (Bien et al. 2013)\n")
cat("  → 実効係数（表示・予測に使う値）:\n")
cat("    非対角 (i≠j): θ_orig[i,j] = th[i,j] / (sx_i × sx_j)          ※そのまま\n")
cat("    対角   (i=j): θ_orig[i,i] = (th[i,i] / 2) / sx_i²            ※0.5倍\n")
cat("    β_orig = β_std / sx\n\n")
cat("  切片（重要）:\n")
cat("    素朴式 mean(Y) - Σ(β_orig × X̄) は交互作用/2次項があると不正確\n")
cat("    （中心化積の平均は共分散・分散であり0ではない）。\n")
cat("    → アプリは predict() との差分から切片を実測校正する方式を採用。\n\n")
cat("  予測式（元単位）:\n")
cat("    Ŷ = 校正切片 + Σ(β_orig × X) + Σ(θ_orig × (Xi-X̄i)(Xj-X̄j))\n")
cat("        ※ i=j の項が2次項、θ_origは上記の0.5倍済みの値\n\n")

cat("==============================================================\n")
