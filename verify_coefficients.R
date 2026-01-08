# =============================================================================
# 係数計算の検証スクリプト
# =============================================================================

library(hierNet)

set.seed(123)

# -----------------------------------------------------------------------------
# テスト1: 既知の係数でデータ生成し、推定結果を検証
# -----------------------------------------------------------------------------

cat("\n========================================\n")
cat("テスト1: 単純な線形モデル（交互作用なし）\n")
cat("========================================\n")

n <- 200
X1 <- rnorm(n, mean = 10, sd = 2)
X2 <- rnorm(n, mean = 20, sd = 5)
X3 <- rnorm(n, mean = 5, sd = 1)

# 真のモデル: y = 5 + 2*X1 + 0.5*X2 + 0*X3 + noise
beta_true <- c(2, 0.5, 0)
intercept_true <- 5
y <- intercept_true + 2*X1 + 0.5*X2 + rnorm(n, sd = 1)

X <- cbind(X1, X2, X3)
colnames(X) <- c("X1", "X2", "X3")

# hierNet実行
fit <- hierNet(x = X, y = y, lam = 5, strong = TRUE)

cat("\n--- hierNet出力 ---\n")
cat("bp:", fit$bp, "\n")
cat("bn:", fit$bn, "\n")
cat("mx (means):", fit$mx, "\n")
cat("sx (sds):", fit$sx, "\n")

# 標準化係数
beta_std <- fit$bp - fit$bn
cat("\n標準化係数 (bp - bn):", beta_std, "\n")

# 元単位係数への変換
sx <- fit$sx
mx <- fit$mx
beta_orig <- beta_std / sx
cat("元単位係数 (beta_std / sx):", beta_orig, "\n")
cat("真の係数:", beta_true, "\n")

# 切片の計算
mean_y <- mean(y)
intercept_orig <- mean_y - sum(beta_orig * mx)
cat("\n計算した切片:", intercept_orig, "\n")
cat("真の切片:", intercept_true, "\n")

# 予測値の検証
pred_hiernet <- predict(fit, newx = X)
pred_manual <- intercept_orig + X %*% beta_orig

cat("\n予測値の比較 (最初の5個):\n")
cat("hierNet predict:", head(pred_hiernet, 5), "\n")
cat("手動計算:", head(pred_manual, 5), "\n")
cat("差分の最大値:", max(abs(pred_hiernet - pred_manual)), "\n")

# -----------------------------------------------------------------------------
# テスト2: 交互作用を含むモデル
# -----------------------------------------------------------------------------

cat("\n\n========================================\n")
cat("テスト2: 交互作用を含むモデル\n")
cat("========================================\n")

set.seed(456)
n <- 300
X1 <- rnorm(n, mean = 10, sd = 2)
X2 <- rnorm(n, mean = 20, sd = 5)

# 真のモデル: y = 3 + 1.5*X1 + 0.8*X2 + 0.1*(X1-10)*(X2-20) + noise
# 交互作用は中心化形式
y <- 3 + 1.5*X1 + 0.8*X2 + 0.1*(X1 - 10)*(X2 - 20) + rnorm(n, sd = 2)

X <- cbind(X1, X2)
colnames(X) <- c("X1", "X2")

fit2 <- hierNet(x = X, y = y, lam = 3, strong = TRUE)

cat("\n--- hierNet出力 ---\n")
cat("bp:", fit2$bp, "\n")
cat("bn:", fit2$bn, "\n")
cat("th (interaction matrix):\n")
print(fit2$th)

beta_std2 <- fit2$bp - fit2$bn
sx2 <- fit2$sx
mx2 <- fit2$mx

cat("\n標準化係数:", beta_std2, "\n")

beta_orig2 <- beta_std2 / sx2
cat("元単位係数:", beta_orig2, "\n")
cat("真の係数: 1.5, 0.8\n")

# 交互作用係数の変換
theta_std <- fit2$th
theta_orig <- theta_std / outer(sx2, sx2)
cat("\n標準化交互作用係数:\n")
print(theta_std)
cat("\n元単位交互作用係数:\n")
print(theta_orig)
cat("真の交互作用係数: 0.1\n")

# 切片
mean_y2 <- mean(y)
intercept_orig2 <- mean_y2 - sum(beta_orig2 * mx2)
cat("\n計算した切片:", intercept_orig2, "\n")
cat("真の切片: 3\n")

# 予測値の検証（中心化形式）
pred_hiernet2 <- predict(fit2, newx = X)

# 手動計算: y = intercept + beta*X + theta*(X1-mx1)*(X2-mx2)
X1_centered <- X[,1] - mx2[1]
X2_centered <- X[,2] - mx2[2]
interaction_term <- theta_orig[1,2] * X1_centered * X2_centered
pred_manual2 <- intercept_orig2 + X %*% beta_orig2 + interaction_term

cat("\n予測値の比較 (最初の5個):\n")
cat("hierNet predict:", head(pred_hiernet2, 5), "\n")
cat("手動計算:", head(pred_manual2, 5), "\n")
cat("差分の最大値:", max(abs(pred_hiernet2 - pred_manual2)), "\n")

# -----------------------------------------------------------------------------
# テスト3: 2次項を含むモデル
# -----------------------------------------------------------------------------

cat("\n\n========================================\n")
cat("テスト3: 2次項を含むモデル\n")
cat("========================================\n")

set.seed(789)
n <- 300
X1 <- rnorm(n, mean = 5, sd = 2)

# 真のモデル: y = 10 + 2*X1 + 0.3*(X1-5)^2 + noise
y <- 10 + 2*X1 + 0.3*(X1 - 5)^2 + rnorm(n, sd = 1)

X <- cbind(X1)
colnames(X) <- c("X1")

# hierNetは最低2変数必要なのでダミー変数追加
X_dummy <- rnorm(n)
X_mat <- cbind(X1, X_dummy)
colnames(X_mat) <- c("X1", "dummy")

fit3 <- hierNet(x = X_mat, y = y, lam = 2, strong = TRUE)

cat("\n--- hierNet出力 ---\n")
cat("bp:", fit3$bp, "\n")
cat("bn:", fit3$bn, "\n")
cat("th (対角成分 = 2次項):\n")
print(diag(fit3$th))

beta_std3 <- fit3$bp - fit3$bn
sx3 <- fit3$sx
mx3 <- fit3$mx

cat("\n標準化主効果係数:", beta_std3, "\n")
beta_orig3 <- beta_std3 / sx3
cat("元単位主効果係数:", beta_orig3, "\n")
cat("真の主効果係数: 2, 0\n")

# 2次項係数
theta_diag_std <- diag(fit3$th)
theta_diag_orig <- theta_diag_std / (sx3^2)
cat("\n標準化2次項係数:", theta_diag_std, "\n")
cat("元単位2次項係数:", theta_diag_orig, "\n")
cat("真の2次項係数: 0.3, 0\n")

cat("\n========================================\n")
cat("検証完了\n")
cat("========================================\n")
