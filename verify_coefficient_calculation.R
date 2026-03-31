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
# 重要な修正点 (2024):
# - 交互作用モデル: 対角要素（2次項）を除外して予測を計算
# - 2次モデル: 全項を含めて予測を計算
# - 係数と予測の一貫性を保証
# =============================================================================

library(hierNet)

cat("\n")
cat("==============================================================\n")
cat("           係数計算の科学的検証\n")
cat("==============================================================\n\n")

# -----------------------------------------------------------------------------
# テスト1: 主効果のみ（交互作用なし）
# -----------------------------------------------------------------------------

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("テスト1: 主効果のみのモデル\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

set.seed(12345)
n <- 500

# 異なるスケールの変数を生成
X1 <- rnorm(n, mean = 100, sd = 15)   # 大きいスケール
X2 <- rnorm(n, mean = 5, sd = 0.5)    # 小さいスケール
X3 <- rnorm(n, mean = 50, sd = 10)    # 中程度

# 真のモデル: Y = 10 + 0.5*X1 + 3*X2 + 0.1*X3 + noise
beta_true <- c(0.5, 3, 0.1)
intercept_true <- 10
Y <- intercept_true + 0.5*X1 + 3*X2 + 0.1*X3 + rnorm(n, sd = 2)

X <- cbind(X1, X2, X3)
colnames(X) <- c("X1", "X2", "X3")

cat("【真のモデル】\n")
cat(sprintf("  Y = %.1f + %.1f×X1 + %.1f×X2 + %.1f×X3 + ε\n\n",
            intercept_true, beta_true[1], beta_true[2], beta_true[3]))

# hierNet実行（弱い正則化で真の値に近づける）
fit1 <- hierNet(x = X, y = Y, lam = 0.1, strong = TRUE)

# hierNetの内部パラメータ
mx <- fit1$mx  # Xの平均
sx <- fit1$sx  # Xの標準偏差
my <- mean(Y)  # Yの平均

cat("【データの統計量】\n")
cat(sprintf("  X1: mean=%.2f, sd=%.2f\n", mx[1], sx[1]))
cat(sprintf("  X2: mean=%.2f, sd=%.2f\n", mx[2], sx[2]))
cat(sprintf("  X3: mean=%.2f, sd=%.2f\n", mx[3], sx[3]))
cat(sprintf("  Y:  mean=%.2f\n\n", my))

# 標準化係数 (hierNetが返す係数)
beta_std <- fit1$bp - fit1$bn

cat("【標準化係数】(hierNet出力: bp - bn)\n")
cat(sprintf("  β_std = [%.4f, %.4f, %.4f]\n\n", beta_std[1], beta_std[2], beta_std[3]))

# 元単位係数への変換: β_orig = β_std / sx
beta_orig <- beta_std / sx

cat("【元単位係数】(β_std / sx)\n")
cat(sprintf("  β_orig = [%.4f, %.4f, %.4f]\n", beta_orig[1], beta_orig[2], beta_orig[3]))
cat(sprintf("  真の値 = [%.4f, %.4f, %.4f]\n\n", beta_true[1], beta_true[2], beta_true[3]))

# 切片の計算
# 元単位: intercept = mean(Y) - Σ(β_orig × mean(X))
intercept_orig <- my - sum(beta_orig * mx)

cat("【切片】\n")
cat(sprintf("  元単位切片 = mean(Y) - Σ(β_orig × mean(X))\n"))
cat(sprintf("            = %.2f - (%.4f×%.2f + %.4f×%.2f + %.4f×%.2f)\n",
            my, beta_orig[1], mx[1], beta_orig[2], mx[2], beta_orig[3], mx[3]))
cat(sprintf("            = %.4f\n", intercept_orig))
cat(sprintf("  真の切片  = %.4f\n\n", intercept_true))

# -----------------------------------------------------------------------------
# 予測値の検証
# -----------------------------------------------------------------------------

cat("【予測値の検証】\n")

# hierNetの予測
pred_hiernet <- predict(fit1, newx = X)

# 手動計算（元単位）: Y = intercept + β×X
pred_manual_orig <- intercept_orig + X %*% beta_orig

# 手動計算（標準化形式から）:
# Y_centered = β_std × X_std → Y = mean(Y) + β_std × X_std
X_std <- scale(X, center = mx, scale = sx)
pred_manual_std <- my + X_std %*% beta_std

cat("  サンプル予測値（最初の5件）:\n")
cat("  -----------------------------------------------------\n")
cat(sprintf("  %-8s  %-12s  %-12s  %-12s\n", "実測値", "hierNet", "手動(元単位)", "手動(標準化)"))
cat("  -----------------------------------------------------\n")
for (i in 1:5) {
  cat(sprintf("  %-8.2f  %-12.4f  %-12.4f  %-12.4f\n",
              Y[i], pred_hiernet[i], pred_manual_orig[i], pred_manual_std[i]))
}

cat("\n  予測値の差分:\n")
cat(sprintf("    hierNet vs 手動(元単位): max|diff| = %.2e\n",
            max(abs(pred_hiernet - pred_manual_orig))))
cat(sprintf("    hierNet vs 手動(標準化): max|diff| = %.2e\n",
            max(abs(pred_hiernet - pred_manual_std))))
cat(sprintf("    元単位 vs 標準化:        max|diff| = %.2e\n\n",
            max(abs(pred_manual_orig - pred_manual_std))))

# 判定
test1_pass <- max(abs(pred_hiernet - pred_manual_orig)) < 1e-10 &&
              max(abs(pred_hiernet - pred_manual_std)) < 1e-10

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

# 真のモデル（中心化形式の交互作用）:
# Y = 5 + 1.5*X1 + 2*X2 + 0.2*(X1-20)*(X2-10) + noise
beta_true2 <- c(1.5, 2.0)
theta_true <- 0.2
intercept_true2 <- 5

Y2 <- intercept_true2 + 1.5*X1 + 2*X2 + 0.2*(X1 - 20)*(X2 - 10) + rnorm(n, sd = 1)

X2_mat <- cbind(X1, X2)
colnames(X2_mat) <- c("X1", "X2")

cat("【真のモデル】\n")
cat(sprintf("  Y = %.1f + %.1f×X1 + %.1f×X2 + %.1f×(X1-20)(X2-10) + ε\n\n",
            intercept_true2, beta_true2[1], beta_true2[2], theta_true))

fit2 <- hierNet(x = X2_mat, y = Y2, lam = 0.5, strong = TRUE)

mx2 <- fit2$mx
sx2 <- fit2$sx
my2 <- mean(Y2)

cat("【データの統計量】\n")
cat(sprintf("  X1: mean=%.2f, sd=%.2f\n", mx2[1], sx2[1]))
cat(sprintf("  X2: mean=%.2f, sd=%.2f\n", mx2[2], sx2[2]))
cat(sprintf("  Y:  mean=%.2f\n\n", my2))

# 標準化係数
beta_std2 <- fit2$bp - fit2$bn
theta_std2 <- fit2$th  # 交互作用行列

cat("【標準化係数】\n")
cat(sprintf("  主効果 β_std = [%.4f, %.4f]\n", beta_std2[1], beta_std2[2]))
cat(sprintf("  交互作用 θ_std[1,2] = %.4f\n\n", theta_std2[1,2]))

# 元単位への変換
beta_orig2 <- beta_std2 / sx2
theta_orig2 <- theta_std2 / outer(sx2, sx2)

cat("【元単位係数】\n")
cat(sprintf("  主効果 β_orig = [%.4f, %.4f]\n", beta_orig2[1], beta_orig2[2]))
cat(sprintf("  真の値       = [%.4f, %.4f]\n", beta_true2[1], beta_true2[2]))
cat(sprintf("  交互作用 θ_orig[1,2] = %.4f\n", theta_orig2[1,2]))
cat(sprintf("  真の値               = %.4f\n\n", theta_true))

# 切片
intercept_orig2 <- my2 - sum(beta_orig2 * mx2)
cat("【切片】\n")
cat(sprintf("  元単位切片 = %.4f (真の値: %.4f)\n\n", intercept_orig2, intercept_true2))

# 予測値の検証
cat("【予測値の検証】\n")

pred_hiernet2 <- predict(fit2, newx = X2_mat)

# 手動計算（元単位）: Y = intercept + β×X + θ×(X1-mx1)(X2-mx2)
X1_c <- X2_mat[,1] - mx2[1]
X2_c <- X2_mat[,2] - mx2[2]
interaction_term <- theta_orig2[1,2] * X1_c * X2_c
pred_manual_orig2 <- intercept_orig2 + X2_mat %*% beta_orig2 + interaction_term

# 手動計算（標準化形式から）
X2_std <- scale(X2_mat, center = mx2, scale = sx2)
interaction_std <- theta_std2[1,2] * X2_std[,1] * X2_std[,2]
pred_manual_std2 <- my2 + X2_std %*% beta_std2 + interaction_std

cat("  サンプル予測値（最初の5件）:\n")
cat("  -----------------------------------------------------\n")
cat(sprintf("  %-8s  %-12s  %-12s  %-12s\n", "実測値", "hierNet", "手動(元単位)", "手動(標準化)"))
cat("  -----------------------------------------------------\n")
for (i in 1:5) {
  cat(sprintf("  %-8.2f  %-12.4f  %-12.4f  %-12.4f\n",
              Y2[i], pred_hiernet2[i], pred_manual_orig2[i], pred_manual_std2[i]))
}

cat("\n  予測値の差分:\n")
cat(sprintf("    hierNet vs 手動(元単位): max|diff| = %.2e\n",
            max(abs(pred_hiernet2 - pred_manual_orig2))))
cat(sprintf("    hierNet vs 手動(標準化): max|diff| = %.2e\n",
            max(abs(pred_hiernet2 - pred_manual_std2))))

test2_pass <- max(abs(pred_hiernet2 - pred_manual_orig2)) < 1e-10 &&
              max(abs(pred_hiernet2 - pred_manual_std2)) < 1e-10

cat(sprintf("\n  テスト2結果: %s\n\n", ifelse(test2_pass, "✓ PASS", "✗ FAIL")))


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

# 真のモデル: Y = 20 + 3*X1 + 0.5*(X1-10)^2 + noise
beta_true3 <- c(3, 0)
theta_diag_true <- c(0.5, 0)
intercept_true3 <- 20

Y3 <- intercept_true3 + 3*X1 + 0.5*(X1 - 10)^2 + rnorm(n, sd = 2)

X3_mat <- cbind(X1, X2)
colnames(X3_mat) <- c("X1", "X2")

cat("【真のモデル】\n")
cat(sprintf("  Y = %.1f + %.1f×X1 + %.1f×(X1-10)² + ε\n\n",
            intercept_true3, beta_true3[1], theta_diag_true[1]))

fit3 <- hierNet(x = X3_mat, y = Y3, lam = 1, strong = TRUE)

mx3 <- fit3$mx
sx3 <- fit3$sx
my3 <- mean(Y3)

cat("【データの統計量】\n")
cat(sprintf("  X1: mean=%.2f, sd=%.2f\n", mx3[1], sx3[1]))
cat(sprintf("  X2: mean=%.2f, sd=%.2f\n", mx3[2], sx3[2]))
cat(sprintf("  Y:  mean=%.2f\n\n", my3))

beta_std3 <- fit3$bp - fit3$bn
theta_std3 <- fit3$th

cat("【標準化係数】\n")
cat(sprintf("  主効果 β_std = [%.4f, %.4f]\n", beta_std3[1], beta_std3[2]))
cat(sprintf("  2次項 θ_std[1,1] = %.4f, θ_std[2,2] = %.4f\n\n",
            theta_std3[1,1], theta_std3[2,2]))

beta_orig3 <- beta_std3 / sx3
theta_orig3 <- theta_std3 / outer(sx3, sx3)

cat("【元単位係数】\n")
cat(sprintf("  主効果 β_orig = [%.4f, %.4f]\n", beta_orig3[1], beta_orig3[2]))
cat(sprintf("  真の値       = [%.4f, %.4f]\n", beta_true3[1], beta_true3[2]))
cat(sprintf("  2次項 θ_orig[1,1] = %.4f (真の値: %.4f)\n", theta_orig3[1,1], theta_diag_true[1]))
cat(sprintf("  2次項 θ_orig[2,2] = %.4f (真の値: %.4f)\n\n", theta_orig3[2,2], theta_diag_true[2]))

intercept_orig3 <- my3 - sum(beta_orig3 * mx3)
cat("【切片】\n")
cat(sprintf("  元単位切片 = %.4f (真の値: %.4f)\n\n", intercept_orig3, intercept_true3))

# 予測値の検証
cat("【予測値の検証】\n")

pred_hiernet3 <- predict(fit3, newx = X3_mat)

# 手動計算（元単位）
X1_c3 <- X3_mat[,1] - mx3[1]
X2_c3 <- X3_mat[,2] - mx3[2]
quadratic_term <- theta_orig3[1,1] * X1_c3^2 + theta_orig3[2,2] * X2_c3^2
interaction_term3 <- theta_orig3[1,2] * X1_c3 * X2_c3
pred_manual_orig3 <- intercept_orig3 + X3_mat %*% beta_orig3 + quadratic_term + interaction_term3

# 手動計算（標準化形式から）
X3_std <- scale(X3_mat, center = mx3, scale = sx3)
quadratic_std <- theta_std3[1,1] * X3_std[,1]^2 + theta_std3[2,2] * X3_std[,2]^2
interaction_std3 <- theta_std3[1,2] * X3_std[,1] * X3_std[,2]
pred_manual_std3 <- my3 + X3_std %*% beta_std3 + quadratic_std + interaction_std3

cat("  サンプル予測値（最初の5件）:\n")
cat("  -----------------------------------------------------\n")
cat(sprintf("  %-8s  %-12s  %-12s  %-12s\n", "実測値", "hierNet", "手動(元単位)", "手動(標準化)"))
cat("  -----------------------------------------------------\n")
for (i in 1:5) {
  cat(sprintf("  %-8.2f  %-12.4f  %-12.4f  %-12.4f\n",
              Y3[i], pred_hiernet3[i], pred_manual_orig3[i], pred_manual_std3[i]))
}

cat("\n  予測値の差分:\n")
cat(sprintf("    hierNet vs 手動(元単位): max|diff| = %.2e\n",
            max(abs(pred_hiernet3 - pred_manual_orig3))))
cat(sprintf("    hierNet vs 手動(標準化): max|diff| = %.2e\n",
            max(abs(pred_hiernet3 - pred_manual_std3))))

test3_pass <- max(abs(pred_hiernet3 - pred_manual_orig3)) < 1e-10 &&
              max(abs(pred_hiernet3 - pred_manual_std3)) < 1e-10

cat(sprintf("\n  テスト3結果: %s\n\n", ifelse(test3_pass, "✓ PASS", "✗ FAIL")))


# -----------------------------------------------------------------------------
# 総合結果
# -----------------------------------------------------------------------------

cat("==============================================================\n")
cat("                    総合検証結果\n")
cat("==============================================================\n\n")

cat(sprintf("  テスト1 (主効果のみ):     %s\n", ifelse(test1_pass, "✓ PASS", "✗ FAIL")))
cat(sprintf("  テスト2 (交互作用あり):   %s\n", ifelse(test2_pass, "✓ PASS", "✗ FAIL")))
cat(sprintf("  テスト3 (2次項あり):      %s\n\n", ifelse(test3_pass, "✓ PASS", "✗ FAIL")))

all_pass <- test1_pass && test2_pass && test3_pass
cat(sprintf("  総合結果: %s\n\n", ifelse(all_pass, "✓ 全テスト合格", "✗ 一部テスト失敗")))

cat("【検証された変換式】\n")
cat("  元単位係数:\n")
cat("    β_orig = β_std / SD(X)\n")
cat("    θ_orig = θ_std / (SD(Xi) × SD(Xj))\n")
cat("    切片 = mean(Y) - Σ(β_orig × mean(X))\n\n")
cat("  予測式（元単位）:\n")
cat("    Ŷ = 切片 + Σ(β_orig × X) + Σ(θ_orig × (Xi-X̄i)(Xj-X̄j))\n\n")
cat("  予測式（標準化）:\n")
cat("    Ŷ = mean(Y) + Σ(β_std × X_std) + Σ(θ_std × Xi_std × Xj_std)\n")
cat("    ※ 標準化モデルの切片は0（Yが中心化されているため）\n\n")

cat("==============================================================\n")
