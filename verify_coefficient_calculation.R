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
pred_hiernet <- as.vector(predict(fit1, newx = X))

# 弱い正則化では小さな交互作用係数が残ることがある
n_active_th <- sum(abs(fit1$th) > 1e-10)
cat(sprintf("  （参考）非ゼロth要素数: %d\n", n_active_th))

# 手動計算（元単位）: Y = intercept + β×X + 残存th項（中心化積）
Xc1 <- sweep(X, 2, mx)
th_orig1 <- fit1$th / outer(sx, sx)
lp1 <- as.vector(X %*% beta_orig)
for (i in 1:ncol(X)) {
  for (j in i:ncol(X)) {
    th_ij <- if (i == j) th_orig1[i, i] else th_orig1[i, j] + th_orig1[j, i]
    if (abs(th_ij) > 1e-15) {
      lp1 <- lp1 + if (i == j) th_ij * Xc1[, i]^2 else th_ij * Xc1[, i] * Xc1[, j]
    }
  }
}
# upper規約でも計算し、良い方を採用
lp1u <- as.vector(X %*% beta_orig)
for (i in 1:ncol(X)) {
  for (j in i:ncol(X)) {
    th_ij <- th_orig1[i, j]
    if (abs(th_ij) > 1e-15) {
      lp1u <- lp1u + if (i == j) th_ij * Xc1[, i]^2 else th_ij * Xc1[, i] * Xc1[, j]
    }
  }
}
lp1_best <- if (sd(pred_hiernet - lp1u) <= sd(pred_hiernet - lp1)) lp1u else lp1
intercept_cal1 <- mean(pred_hiernet - lp1_best)
pred_manual_orig <- intercept_cal1 + lp1_best

cat(sprintf("  校正切片 = %.4f / 素朴切片 = %.4f\n", intercept_cal1, intercept_orig))

cat("\n  予測値の差分:\n")
cat(sprintf("    hierNet vs 手動(元単位): max|diff| = %.2e\n",
            max(abs(pred_hiernet - pred_manual_orig))))

# 判定
test1_pass <- max(abs(pred_hiernet - pred_manual_orig)) < 1e-8

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

# 予測値の検証（アプリと同じ校正方式）
# 注: 中心化した積の平均は共分散(≠0)なので、素朴切片 mean(Y)-Σβ×X̄ は
#     交互作用があると厳密には正しくない。切片は実測校正する。
cat("【予測値の検証】\n")

pred_hiernet2 <- as.vector(predict(fit2, newx = X2_mat))

# 非切片部分（元単位）: β×X + θ×(X1-mx1)(X2-mx2)
X1_c <- X2_mat[,1] - mx2[1]
X2_c <- X2_mat[,2] - mx2[2]

# th規約2種（upper / sum）で試し、predict()との差が定数になる方を採用
lp_upper <- as.vector(X2_mat %*% beta_orig2) + theta_orig2[1,2] * X1_c * X2_c
theta_sum2 <- theta_orig2[1,2] + theta_orig2[2,1]
lp_sum <- as.vector(X2_mat %*% beta_orig2) + theta_sum2 * X1_c * X2_c

sd_upper <- sd(pred_hiernet2 - lp_upper)
sd_sum <- sd(pred_hiernet2 - lp_sum)
lp2 <- if (sd_upper <= sd_sum) lp_upper else lp_sum
conv2 <- if (sd_upper <= sd_sum) "upper" else "sum"

# 切片の実測校正
intercept_orig2 <- mean(pred_hiernet2 - lp2)
naive_intercept2 <- my2 - sum(beta_orig2 * mx2)
pred_manual_orig2 <- intercept_orig2 + lp2

cat(sprintf("  採用th規約: %s（偏差SD: upper=%.2e, sum=%.2e）\n", conv2, sd_upper, sd_sum))
cat(sprintf("  校正切片 = %.4f / 素朴切片 = %.4f（差=共分散補正分: %.4f）\n",
            intercept_orig2, naive_intercept2, intercept_orig2 - naive_intercept2))
cat(sprintf("  真の切片 = %.4f\n\n", intercept_true2))

cat("  予測値の差分:\n")
cat(sprintf("    hierNet vs 手動(校正済み元単位): max|diff| = %.2e\n",
            max(abs(pred_hiernet2 - pred_manual_orig2))))

test2_pass <- max(abs(pred_hiernet2 - pred_manual_orig2)) < 1e-8

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

# 予測値の検証（アプリと同じ校正方式: 2次項の平均は分散≠0なので切片を校正）
cat("【予測値の検証】\n")

pred_hiernet3 <- as.vector(predict(fit3, newx = X3_mat))

X1_c3 <- X3_mat[,1] - mx3[1]
X2_c3 <- X3_mat[,2] - mx3[2]

# th規約2種で試す
mk_lp3 <- function(th_pair) {
  as.vector(X3_mat %*% beta_orig3) +
    theta_orig3[1,1] * X1_c3^2 + theta_orig3[2,2] * X2_c3^2 +
    th_pair * X1_c3 * X2_c3
}
lp3_upper <- mk_lp3(theta_orig3[1,2])
lp3_sum <- mk_lp3(theta_orig3[1,2] + theta_orig3[2,1])

sd3_upper <- sd(pred_hiernet3 - lp3_upper)
sd3_sum <- sd(pred_hiernet3 - lp3_sum)
lp3 <- if (sd3_upper <= sd3_sum) lp3_upper else lp3_sum
conv3 <- if (sd3_upper <= sd3_sum) "upper" else "sum"

intercept_orig3 <- mean(pred_hiernet3 - lp3)
naive_intercept3 <- my3 - sum(beta_orig3 * mx3)
pred_manual_orig3 <- intercept_orig3 + lp3

cat(sprintf("  採用th規約: %s（偏差SD: upper=%.2e, sum=%.2e）\n", conv3, sd3_upper, sd3_sum))
cat(sprintf("  校正切片 = %.4f / 素朴切片 = %.4f（差=分散補正分: %.4f）\n",
            intercept_orig3, naive_intercept3, intercept_orig3 - naive_intercept3))
cat(sprintf("  真の切片 = %.4f\n\n", intercept_true3))

cat("  予測値の差分:\n")
cat(sprintf("    hierNet vs 手動(校正済み元単位): max|diff| = %.2e\n",
            max(abs(pred_hiernet3 - pred_manual_orig3))))

test3_pass <- max(abs(pred_hiernet3 - pred_manual_orig3)) < 1e-8

cat(sprintf("\n  テスト3結果: %s\n\n", ifelse(test3_pass, "✓ PASS", "✗ FAIL")))


# -----------------------------------------------------------------------------
# テスト4: diagonal=FALSE（交互作用モデル）と切片校正の検証
# -----------------------------------------------------------------------------

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("テスト4: diagonal=FALSE と切片校正（アプリの実装方式）\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

set.seed(22222)
n <- 500

# 相関のあるX（共分散補正の効果を確認するため）
X1 <- rnorm(n, mean = 20, sd = 4)
X2 <- 0.6 * X1 + rnorm(n, mean = 5, sd = 2)  # X1と相関

Y4 <- 5 + 1.5*X1 + 2*X2 + 0.3*(X1 - mean(X1))*(X2 - mean(X2)) + rnorm(n, sd = 1)
X4_mat <- cbind(X1, X2)
colnames(X4_mat) <- c("X1", "X2")

cat(sprintf("【設定】Cor(X1,X2) = %.3f（相関あり → 共分散補正が必要な条件）\n\n",
            cor(X1, X2)))

# 交互作用モデル: diagonal=FALSE でフィット
fit4 <- hierNet(x = X4_mat, y = Y4, lam = 1, strong = TRUE, diagonal = FALSE)

cat("【diagonal=FALSE の確認】\n")
cat(sprintf("  th対角成分: [%.6f, %.6f]（0であるべき）\n\n",
            fit4$th[1,1], fit4$th[2,2]))

mx4 <- fit4$mx; sx4 <- fit4$sx; my4 <- mean(Y4)
beta_orig4 <- (fit4$bp - fit4$bn) / sx4
pred_hn4 <- as.vector(predict(fit4, newx = X4_mat))

# 規約チェック: pair効果 = th[i,j] (upper) か th[i,j]+th[j,i] (sum) か
Xc <- sweep(X4_mat, 2, mx4)
th_orig_upper <- fit4$th / outer(sx4, sx4)
th_orig_sum <- th_orig_upper + t(th_orig_upper); diag(th_orig_sum) <- diag(th_orig_upper)

check_conv <- function(theta) {
  lp <- as.vector(X4_mat %*% beta_orig4) + theta[1,2] * Xc[,1] * Xc[,2]
  d <- pred_hn4 - lp
  c(sd = sd(d), mean = mean(d))
}
res_upper <- check_conv(th_orig_upper)
res_sum <- check_conv(th_orig_sum)

cat("【th規約チェック】(predict() − 式) が定数になる規約が正解\n")
cat(sprintf("  upper規約: 偏差SD = %.3e\n", res_upper["sd"]))
cat(sprintf("  sum規約:   偏差SD = %.3e\n\n", res_sum["sd"]))

winner <- if (res_upper["sd"] <= res_sum["sd"]) "upper" else "sum"
res_best <- if (winner == "upper") res_upper else res_sum

cat(sprintf("  採用規約: %s\n", winner))
cat(sprintf("  校正切片 = %.4f\n", res_best["mean"]))

# 素朴な切片（共分散補正なし）との比較
naive_intercept <- my4 - sum(beta_orig4 * mx4)
cat(sprintf("  素朴切片 = mean(Y) - Σβ×X̄ = %.4f\n", naive_intercept))
cat(sprintf("  差（共分散補正分） = %.4f\n\n", res_best["mean"] - naive_intercept))

test4_pass <- res_best["sd"] < 1e-8
cat(sprintf("  テスト4結果: %s\n", ifelse(test4_pass, "✓ PASS（式がpredict()を完全再現）", "✗ FAIL")))
cat("  ※ 「差」が非ゼロなら、旧実装（素朴切片）はその分だけ予測を外していた\n\n")


# -----------------------------------------------------------------------------
# 総合結果
# -----------------------------------------------------------------------------

cat("==============================================================\n")
cat("                    総合検証結果\n")
cat("==============================================================\n\n")

cat(sprintf("  テスト1 (主効果のみ):     %s\n", ifelse(test1_pass, "✓ PASS", "✗ FAIL")))
cat(sprintf("  テスト2 (交互作用あり):   %s\n", ifelse(test2_pass, "✓ PASS", "✗ FAIL")))
cat(sprintf("  テスト3 (2次項あり):      %s\n", ifelse(test3_pass, "✓ PASS", "✗ FAIL")))
cat(sprintf("  テスト4 (diagonal/切片校正): %s\n\n", ifelse(test4_pass, "✓ PASS", "✗ FAIL")))

all_pass <- test1_pass && test2_pass && test3_pass && test4_pass
cat(sprintf("  総合結果: %s\n\n", ifelse(all_pass, "✓ 全テスト合格", "✗ 一部テスト失敗")))

cat("【検証された変換式】\n")
cat("  元単位係数:\n")
cat("    β_orig = β_std / SD(X)\n")
cat("    θ_orig = θ_std / (SD(Xi) × SD(Xj))\n\n")
cat("  切片（重要）:\n")
cat("    素朴式 mean(Y) - Σ(β_orig × X̄) は交互作用があると不正確。\n")
cat("    中心化積 (Xi-X̄i)(Xj-X̄j) のデータ平均は共分散(≠0)であり、\n")
cat("    正確な切片には -Σθ×Cov 補正が必要。\n")
cat("    → アプリは predict() との差分から切片を実測校正する方式を採用。\n\n")
cat("  予測式（元単位）:\n")
cat("    Ŷ = 校正切片 + Σ(β_orig × X) + Σ(θ_orig × (Xi-X̄i)(Xj-X̄j))\n\n")
cat("  標準化式（中心化Y形式）:\n")
cat("    (Y - Ȳ) = 切片_std + Σ(β_std × X*) + Σ(θ_std × Xi* × Xj*)\n")
cat("    ※ 交互作用が無ければ切片_std = 0、有れば相関補正分だけ非ゼロ\n\n")

cat("==============================================================\n")
