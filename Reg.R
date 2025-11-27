# ============================================================================

# hierNet 階層的LASSO回帰分析アプリケーション

# Hierarchical LASSO Regression with Interaction Terms

# 主効果・交互作用の階層制約付きLASSO回帰

# ============================================================================

# — パッケージ読み込み —

suppressPackageStartupMessages({
library(shiny)
library(hierNet)
library(glmnet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(plotly)
library(bslib)
})

# — カスタムテーマ設定 —

custom_theme <- bs_theme(
  version = 5,
  bg = "#0d1117",
  fg = "#c9d1d9",
  primary = "#58a6ff",
  secondary = "#21262d",
  success = "#3fb950",
  warning = "#d29922",
  danger = "#f85149",
  base_font = font_google("IBM Plex Sans"),
  heading_font = font_google("IBM Plex Mono"),
  code_font = font_google("IBM Plex Mono")
)

# — カスタムCSS —

custom_css <- "
/* ベーステーマ */
body {
background: linear-gradient(135deg, #0d1117 0%, #161b22 50%, #0d1117 100%);
min-height: 100vh;
font-family: 'IBM Plex Sans', sans-serif;
}

/* ヘッダーデザイン */
.main-header {
background: linear-gradient(90deg, #161b22 0%, #21262d 100%);
border-bottom: 1px solid #30363d;
padding: 1.5rem 2rem;
margin-bottom: 2rem;
}

.app-title {
font-family: 'IBM Plex Mono', monospace;
font-size: 1.8rem;
font-weight: 700;
color: #58a6ff;
letter-spacing: -0.5px;
margin: 0;
}

.app-subtitle {
font-size: 0.9rem;
color: #8b949e;
margin-top: 0.25rem;
}

/* カード・パネルデザイン */
.analysis-card {
background: rgba(22, 27, 34, 0.95);
border: 1px solid #30363d;
border-radius: 12px;
padding: 1.5rem;
margin-bottom: 1.5rem;
box-shadow: 0 8px 32px rgba(0, 0, 0, 0.3);
backdrop-filter: blur(10px);
transition: all 0.3s ease;
}

.analysis-card:hover {
border-color: #58a6ff;
box-shadow: 0 8px 32px rgba(88, 166, 255, 0.15);
}

.card-header {
font-family: 'IBM Plex Mono', monospace;
font-size: 0.85rem;
font-weight: 600;
color: #58a6ff;
text-transform: uppercase;
letter-spacing: 1.5px;
margin-bottom: 1rem;
padding-bottom: 0.75rem;
border-bottom: 1px solid #30363d;
display: flex;
align-items: center;
gap: 0.5rem;
}

.card-header::before {
content: '▸';
color: #3fb950;
}

/* 入力コントロール */
.form-control, .selectize-input {
background: #0d1117 !important;
border: 1px solid #30363d !important;
border-radius: 8px !important;
color: #c9d1d9 !important;
font-family: 'IBM Plex Sans', sans-serif !important;
transition: all 0.2s ease !important;
}

.form-control:focus, .selectize-input.focus {
border-color: #58a6ff !important;
box-shadow: 0 0 0 3px rgba(88, 166, 255, 0.15) !important;
}

.selectize-dropdown {
background: #161b22 !important;
border: 1px solid #30363d !important;
border-radius: 8px !important;
}

.selectize-dropdown-content .option {
color: #c9d1d9 !important;
padding: 10px 12px !important;
}

.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .option.active {
background: #21262d !important;
color: #58a6ff !important;
}

/* ボタンスタイル */
.btn-analysis {
background: linear-gradient(135deg, #238636 0%, #2ea043 100%);
border: none;
border-radius: 8px;
color: #ffffff;
font-family: 'IBM Plex Mono', monospace;
font-weight: 600;
font-size: 0.9rem;
padding: 12px 24px;
letter-spacing: 0.5px;
transition: all 0.3s ease;
width: 100%;
margin-top: 1rem;
}

.btn-analysis:hover {
background: linear-gradient(135deg, #2ea043 0%, #3fb950 100%);
transform: translateY(-2px);
box-shadow: 0 4px 20px rgba(46, 160, 67, 0.4);
}

.btn-analysis:active {
transform: translateY(0);
}

/* 結果表示エリア */
.result-section {
background: linear-gradient(135deg, #161b22 0%, #0d1117 100%);
border: 1px solid #30363d;
border-radius: 12px;
padding: 1.5rem;
margin-bottom: 1.5rem;
}

.equation-display {
background: #0d1117;
border: 1px solid #30363d;
border-radius: 8px;
padding: 1.25rem;
font-family: 'IBM Plex Mono', monospace;
font-size: 0.95rem;
color: #79c0ff;
overflow-x: auto;
white-space: pre-wrap;
word-break: break-all;
line-height: 1.8;
}

/* メトリクス表示 */
.metric-grid {
display: grid;
grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
gap: 1rem;
margin-top: 1rem;
}

.metric-box {
background: linear-gradient(135deg, #21262d 0%, #161b22 100%);
border: 1px solid #30363d;
border-radius: 10px;
padding: 1.25rem;
text-align: center;
transition: all 0.3s ease;
}

.metric-box:hover {
border-color: #58a6ff;
transform: translateY(-3px);
}

.metric-label {
font-family: 'IBM Plex Mono', monospace;
font-size: 0.7rem;
color: #8b949e;
text-transform: uppercase;
letter-spacing: 1px;
margin-bottom: 0.5rem;
}

.metric-value {
font-family: 'IBM Plex Mono', monospace;
font-size: 1.5rem;
font-weight: 700;
color: #58a6ff;
}

.metric-value.success { color: #3fb950; }
.metric-value.warning { color: #d29922; }
.metric-value.danger { color: #f85149; }

/* テーブルスタイル */
.dataTables_wrapper {
font-family: 'IBM Plex Sans', sans-serif;
}

table.dataTable {
background: transparent !important;
border-collapse: separate !important;
border-spacing: 0 4px !important;
}

table.dataTable thead th {
background: #21262d !important;
color: #8b949e !important;
font-family: 'IBM Plex Mono', monospace !important;
font-size: 0.8rem !important;
font-weight: 600 !important;
text-transform: uppercase !important;
letter-spacing: 1px !important;
border: none !important;
padding: 12px 16px !important;
}

table.dataTable tbody td {
background: #161b22 !important;
color: #c9d1d9 !important;
border: none !important;
padding: 12px 16px !important;
}

table.dataTable tbody tr:hover td {
background: #21262d !important;
}

/* プログレスインジケータ */
.shiny-notification {
background: #21262d !important;
border: 1px solid #30363d !important;
border-radius: 8px !important;
color: #c9d1d9 !important;
}

/* スクロールバー */
::-webkit-scrollbar {
width: 8px;
height: 8px;
}

::-webkit-scrollbar-track {
background: #0d1117;
}

::-webkit-scrollbar-thumb {
background: #30363d;
border-radius: 4px;
}

::-webkit-scrollbar-thumb:hover {
background: #484f58;
}

/* プロットエリア */
.plot-container {
background: #0d1117;
border: 1px solid #30363d;
border-radius: 8px;
padding: 1rem;
}

/* タブスタイル */
.nav-tabs {
border-bottom: 1px solid #30363d;
}

.nav-tabs .nav-link {
font-family: 'IBM Plex Mono', monospace;
font-size: 0.85rem;
color: #8b949e;
border: none;
padding: 0.75rem 1.25rem;
transition: all 0.2s ease;
}

.nav-tabs .nav-link:hover {
color: #c9d1d9;
border-bottom: 2px solid #484f58;
}

.nav-tabs .nav-link.active {
background: transparent;
color: #58a6ff;
border-bottom: 2px solid #58a6ff;
}

/* ラベルスタイル */
.control-label {
font-family: 'IBM Plex Mono', monospace;
font-size: 0.8rem;
color: #8b949e;
text-transform: uppercase;
letter-spacing: 0.5px;
margin-bottom: 0.5rem;
}

/* 効果タイプバッジ */
.effect-badge {
display: inline-block;
padding: 4px 10px;
border-radius: 20px;
font-family: 'IBM Plex Mono', monospace;
font-size: 0.75rem;
font-weight: 600;
}

.effect-badge.main { background: #388bfd20; color: #58a6ff; border: 1px solid #388bfd; }
.effect-badge.interaction { background: #a371f720; color: #a371f7; border: 1px solid #a371f7; }

/* hierNet説明 */
.hiernet-info {
background: rgba(56, 139, 253, 0.1);
border: 1px solid #388bfd;
border-radius: 8px;
padding: 1rem;
margin-bottom: 1rem;
font-size: 0.85rem;
color: #79c0ff;
}

.hiernet-info strong {
color: #58a6ff;
}

/* レスポンシブ調整 */
@media (max-width: 768px) {
.metric-grid {
grid-template-columns: repeat(2, 1fr);
}

.app-title {
font-size: 1.4rem;
}
}
"

# ============================================================================

# UI定義

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

# — ヘッダー —

div(class = "main-header",
div(class = "container-fluid",
h1(class = "app-title",
tags$span(style = "color: #a371f7;", "◆"),
" hierNet 階層的LASSO回帰"),
p(class = "app-subtitle",
"Hierarchical LASSO with Strong/Weak Hierarchy | 主効果・交互作用の階層制約付き回帰分析")
)
),

# — メインコンテンツ —

div(class = "container-fluid",
fluidRow(
# === 左パネル: データ入力・設定 ===
column(4,
# hierNet説明
div(class = "hiernet-info",
tags$strong("hierNetとは？"),
tags$br(),
"交互作用項 X₁×X₂ がモデルに選択されるには、",
"主効果 X₁ と X₂ が先に選択されている必要があるという",
tags$strong("階層制約（hierarchy constraint）"),
"を課したLASSO回帰です。",
tags$br(),
tags$br(),
tags$strong("Strong hierarchy:"), " 両方の主効果が必要",
tags$br(),
tags$strong("Weak hierarchy:"), " 少なくとも一方の主効果が必要"
),

    # データアップロード
    div(class = "analysis-card",
      div(class = "card-header", "データ入力"),
      fileInput("data_file", 
                label = "CSVファイルをアップロード",
                accept = c(".csv", ".CSV"),
                buttonLabel = "選択",
                placeholder = "ファイル未選択"),
      checkboxInput("header", "ヘッダー行あり", value = TRUE),
      selectInput("encoding", "文字エンコーディング",
                  choices = c("UTF-8" = "UTF-8",
                              "Shift-JIS" = "CP932",
                              "EUC-JP" = "EUC-JP"),
                  selected = "UTF-8"),
      hr(style = "border-color: #30363d;"),
      p(style = "color: #8b949e; font-size: 0.85rem;",
        "💡 サンプルデータを使用する場合は、ファイルを選択せずに進んでください。")
    ),
    
    # 変数選択
    div(class = "analysis-card",
      div(class = "card-header", "変数設定"),
      selectInput("target_var", 
                  label = "目的変数 (Y)",
                  choices = NULL),
      selectInput("explanatory_vars",
                  label = "説明変数 (X)",
                  choices = NULL,
                  multiple = TRUE),
      p(style = "color: #d29922; font-size: 0.8rem; margin-top: 0.5rem;",
        "⚠️ hierNetは変数の全ペア交互作用を計算するため、変数は10個以下を推奨")
    ),
    
    # モデルパラメータ
    div(class = "analysis-card",
      div(class = "card-header", "hierNetパラメータ"),
      selectInput("hierarchy_type",
                  "階層制約タイプ",
                  choices = c("Strong hierarchy" = "strong",
                              "Weak hierarchy" = "weak"),
                  selected = "strong"),
      p(style = "color: #8b949e; font-size: 0.8rem; margin-top: -0.5rem;",
        "Strong: 交互作用には両主効果が必要"),
      sliderInput("nlam",
                  "Lambda候補数",
                  min = 10, max = 50, value = 20, step = 5),
      checkboxInput("standardize", "変数を標準化", value = TRUE),
      checkboxInput("center", "変数を中心化", value = TRUE),
      hr(style = "border-color: #30363d;"),
      sliderInput("nfolds",
                  "交差検証フォールド数",
                  min = 3, max = 10, value = 5, step = 1),
      actionButton("run_analysis", 
                   "分析実行",
                   class = "btn-analysis",
                   icon = icon("play"))
    )
  ),
  
  # === 右パネル: 結果表示 ===
  column(8,
    # タブパネル
    tabsetPanel(
      type = "tabs",
      id = "result_tabs",
      
      # --- データプレビュー タブ ---
      tabPanel(
        title = "データプレビュー",
        value = "data_tab",
        div(class = "result-section", style = "margin-top: 1rem;",
          div(class = "card-header", "アップロードデータ"),
          DTOutput("data_preview")
        )
      ),
      
      # --- 回帰結果 タブ ---
      tabPanel(
        title = "回帰結果",
        value = "result_tab",
        div(style = "margin-top: 1rem;",
          # メトリクス
          div(class = "result-section",
            div(class = "card-header", "モデル評価指標"),
            div(class = "metric-grid",
              uiOutput("metrics_display")
            )
          ),
          
          # 回帰式
          div(class = "result-section",
            div(class = "card-header", "回帰式"),
            div(class = "equation-display",
              uiOutput("equation_display")
            )
          ),
          
          # 係数テーブル
          div(class = "result-section",
            div(class = "card-header", "係数一覧 (主効果・交互作用)"),
            DTOutput("coef_table")
          )
        )
      ),
      
      # --- 予測判定グラフ タブ ---
      tabPanel(
        title = "予測判定グラフ",
        value = "plot_tab",
        div(style = "margin-top: 1rem;",
          div(class = "result-section",
            div(class = "card-header", "実測値 vs 予測値"),
            div(class = "plot-container",
              plotlyOutput("prediction_plot", height = "500px")
            )
          ),
          
          fluidRow(
            column(6,
              div(class = "result-section",
                div(class = "card-header", "残差分布"),
                div(class = "plot-container",
                  plotlyOutput("residual_plot", height = "350px")
                )
              )
            ),
            column(6,
              div(class = "result-section",
                div(class = "card-header", "Lambda選択 (CV)"),
                div(class = "plot-container",
                  plotOutput("cv_plot", height = "350px")
                )
              )
            )
          )
        )
      ),
      
      # --- 係数構造 タブ ---
      tabPanel(
        title = "係数構造",
        value = "structure_tab",
        div(style = "margin-top: 1rem;",
          fluidRow(
            column(6,
              div(class = "result-section",
                div(class = "card-header", "主効果係数"),
                div(class = "plot-container",
                  plotlyOutput("main_effect_plot", height = "400px")
                )
              )
            ),
            column(6,
              div(class = "result-section",
                div(class = "card-header", "交互作用ヒートマップ"),
                div(class = "plot-container",
                  plotlyOutput("interaction_heatmap", height = "400px")
                )
              )
            )
          ),
          div(class = "result-section",
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

# Server定義

# ============================================================================

server <- function(input, output, session) {

# — リアクティブ値 —

rv <- reactiveValues(
data = NULL,
fit = NULL,
cv_fit = NULL,
results = NULL
)

# — サンプルデータ生成 —

generate_sample_data <- function() {
  set.seed(42)
  n <- 200

  # 説明変数
  X1 <- rnorm(n, 0, 1)  # 温度（標準化）
  X2 <- rnorm(n, 0, 1)  # 圧力
  X3 <- rnorm(n, 0, 1)  # 速度
  X4 <- rnorm(n, 0, 1)  # 材料硬度
  X5 <- rnorm(n, 0, 1)  # 湿度

  # 目的変数: 主効果 + 交互作用
  Y <- 5 +
    2.0 * X1 + 1.5 * X2 + 0.8 * X3 + 0.3 * X4 + 0 * X5 +  # 主効果（X5は影響なし）
    1.2 * X1 * X2 +   # 温度×圧力の交互作用
    0.6 * X2 * X3 +   # 圧力×速度の交互作用
    rnorm(n, 0, 1)     # 誤差

  data.frame(
    品質スコア = round(Y, 2),
    温度 = round(X1, 3),
    圧力 = round(X2, 3),
    速度 = round(X3, 3),
    材料硬度 = round(X4, 3),
    湿度 = round(X5, 3)
  )

}

# — データ読み込み —

observeEvent(input$data_file, {
  req(input$data_file)
  tryCatch({
    rv$data <- read.csv(input$data_file$datapath,
                     header = input$header,
                     fileEncoding = input$encoding,
                     stringsAsFactors = FALSE)

    # 数値列のみ抽出
    numeric_cols <- names(rv$data)[sapply(rv$data, is.numeric)]

    # 数値列の存在チェック
    if (length(numeric_cols) < 2) {
      showNotification("エラー: 数値列が2列以上必要です", type = "error")
      rv$data <- NULL
      return()
    }

    updateSelectInput(session, "target_var",
                      choices = numeric_cols,
                      selected = numeric_cols[1])
    updateSelectInput(session, "explanatory_vars",
                      choices = numeric_cols,
                      selected = if(length(numeric_cols) > 1) numeric_cols[-1] else NULL)

    showNotification("データを読み込みました", type = "message")
  }, error = function(e) {
    showNotification(paste("エラー:", e$message), type = "error")
  })

})

# サンプルデータを初期表示

observe({
  if (is.null(input$data_file)) {
    rv$data <- generate_sample_data()
    numeric_cols <- names(rv$data)

    updateSelectInput(session, "target_var",
                      choices = numeric_cols,
                      selected = numeric_cols[1])
    updateSelectInput(session, "explanatory_vars",
                      choices = numeric_cols[-1],
                      selected = numeric_cols[-1])
  }
})

# — データプレビュー —

output$data_preview <- renderDT({
  req(rv$data)
  datatable(
    rv$data,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = 'frtip',
      language = list(
        search = "検索:",
        lengthMenu = "表示: *MENU* 件",
        info = "*TOTAL* 件中 *START* - *END* 件表示",
        paginate = list(previous = "前", `next` = "次")
      )
    ),
    class = 'stripe hover',
    rownames = FALSE
  )
})

# — 分析実行 —

observeEvent(input$run_analysis, {
  req(rv$data, input$target_var, input$explanatory_vars)

  # 変数数チェック
  if (length(input$explanatory_vars) < 2) {
    showNotification("hierNetには2つ以上の説明変数が必要です", type = "error")
    return()
  }

  if (length(input$explanatory_vars) > 15) {
    showNotification("変数が多すぎます。15個以下を推奨します", type = "warning")
  }

  withProgress(message = 'hierNet分析実行中...', value = 0, {
  tryCatch({
    # データ準備
    incProgress(0.1, detail = "データ準備中")
    
    y <- rv$data[[input$target_var]]
    X <- as.matrix(rv$data[, input$explanatory_vars, drop = FALSE])
    
    # 欠損値チェック
    complete_cases <- complete.cases(X, y)
    if (sum(!complete_cases) > 0) {
      showNotification(paste(sum(!complete_cases), "行の欠損値を除外しました"), type = "warning")
      X <- X[complete_cases, ]
      y <- y[complete_cases]
    }

    # データ数の妥当性チェック
    n_obs <- nrow(X)
    n_vars <- ncol(X)
    if (n_obs < 10) {
      showNotification(paste0("エラー: 有効なデータが少なすぎます (", n_obs, "行)。最低10行必要です"), type = "error")
      return()
    }
    if (n_obs < n_vars * 2) {
      showNotification(paste0("警告: サンプル数(", n_obs, ")が変数数(", n_vars, ")の2倍未満です。結果が不安定になる可能性があります"), type = "warning")
    }

    incProgress(0.2, detail = "交差検証実行中（時間がかかります）")
    
    # hierNet 交差検証
    cv_fit <- hierNet.cv(
      fit = hierNet.path(
        x = X, 
        y = y,
        nlam = input$nlam,
        strong = (input$hierarchy_type == "strong"),
        standardize = input$standardize,
        center = input$center
      ),
      x = X,
      y = y,
      nfolds = input$nfolds
    )
    
    rv$cv_fit <- cv_fit
    
    incProgress(0.3, detail = "最適モデル適合中")
    
    # 最適lambdaでのモデル
    best_lambda <- cv_fit$lamhat
    
    fit <- hierNet(
      x = X,
      y = y,
      lam = best_lambda,
      strong = (input$hierarchy_type == "strong"),
      standardize = input$standardize,
      center = input$center
    )
    
    rv$fit <- fit
    
    incProgress(0.2, detail = "予測・評価中")
    
    # 予測値
    predictions <- predict(fit, newx = X)
    
    # 係数抽出
    # 主効果
    main_effects <- fit$bp - fit$bn  # 正と負の係数の差
    names(main_effects) <- colnames(X)
    
    # 交互作用（対称行列）
    interaction_matrix <- fit$th
    rownames(interaction_matrix) <- colnames(X)
    colnames(interaction_matrix) <- colnames(X)

    # 評価指標計算
    ss_res <- sum((y - predictions)^2)
    ss_tot <- sum((y - mean(y))^2)
    r_squared <- 1 - ss_res / ss_tot
    
    n_main <- sum(main_effects != 0)
    n_interaction <- sum(interaction_matrix[upper.tri(interaction_matrix)] != 0)
    n_params <- n_main + n_interaction
    
    adj_r_squared <- 1 - (1 - r_squared) * (length(y) - 1) / 
                      (length(y) - n_params - 1)
    rmse <- sqrt(mean((y - predictions)^2))
    mae <- mean(abs(y - predictions))
    
    incProgress(0.2, detail = "結果整理中")
    
    # 結果保存
    rv$results <- list(
      y = y,
      X = X,
      var_names = colnames(X),
      predictions = predictions,
      intercept = mean(y) - sum(colMeans(X) * main_effects),
      main_effects = main_effects,
      interaction_matrix = interaction_matrix,
      r_squared = r_squared,
      adj_r_squared = adj_r_squared,
      rmse = rmse,
      mae = mae,
      best_lambda = best_lambda,
      n_main = n_main,
      n_interaction = n_interaction,
      hierarchy_type = input$hierarchy_type
    )
    
    # 結果タブへ移動
    updateTabsetPanel(session, "result_tabs", selected = "result_tab")
    
    showNotification("hierNet分析が完了しました", type = "message")
    
    }, error = function(e) {
      showNotification(paste("エラー:", e$message), type = "error")
      print(e)
    })
  })

})

# — メトリクス表示 —

output$metrics_display <- renderUI({
  req(rv$results)
  res <- rv$results

  # R²の色分け
  r2_class <- if (res$r_squared >= 0.8) "success"
               else if (res$r_squared >= 0.6) "warning"
               else "danger"

  tagList(
    div(class = "metric-box",
      div(class = "metric-label", "決定係数 R²"),
      div(class = paste("metric-value", r2_class),
          sprintf("%.4f", res$r_squared))
    ),
    div(class = "metric-box",
      div(class = "metric-label", "調整済み R²"),
      div(class = "metric-value", sprintf("%.4f", res$adj_r_squared))
    ),
    div(class = "metric-box",
      div(class = "metric-label", "RMSE"),
      div(class = "metric-value", sprintf("%.4f", res$rmse))
    ),
    div(class = "metric-box",
      div(class = "metric-label", "MAE"),
      div(class = "metric-value", sprintf("%.4f", res$mae))
    ),
    div(class = "metric-box",
      div(class = "metric-label", "選択主効果数"),
      div(class = "metric-value", res$n_main)
    ),
    div(class = "metric-box",
      div(class = "metric-label", "選択交互作用数"),
      div(class = "metric-value", style = "color: #a371f7;", res$n_interaction)
    ),
    div(class = "metric-box",
      div(class = "metric-label", "最適 λ"),
      div(class = "metric-value", sprintf("%.4f", res$best_lambda))
    ),
    div(class = "metric-box",
      div(class = "metric-label", "階層タイプ"),
      div(class = "metric-value", style = "font-size: 1rem;",
          ifelse(res$hierarchy_type == "strong", "Strong", "Weak"))
    )
  )

})

# — 回帰式表示 —

output$equation_display <- renderUI({
  req(rv$results)
  res <- rv$results

  # 切片
  terms <- sprintf("%.4f", res$intercept)

  # 主効果項
  for (i in seq_along(res$main_effects)) {
    coef <- res$main_effects[i]
    if (abs(coef) > 1e-6) {
      var_name <- names(res$main_effects)[i]
      sign <- if (coef > 0) " + " else " - "
      terms <- paste0(terms, sign, sprintf("%.4f", abs(coef)), " × ", var_name)
    }
  }

  # 交互作用項
  int_mat <- res$interaction_matrix
  var_names <- res$var_names
  for (i in 1:(nrow(int_mat) - 1)) {
    for (j in (i + 1):ncol(int_mat)) {
      coef <- int_mat[i, j]
      if (abs(coef) > 1e-6) {
        sign <- if (coef > 0) " + " else " - "
        interaction_term <- paste0(var_names[i], " × ", var_names[j])
        terms <- paste0(terms, sign, sprintf("%.4f", abs(coef)), " × (", interaction_term, ")")
      }
    }
  }

  target_name <- input$target_var
  equation <- paste0(target_name, " = ", terms)

  HTML(equation)

})

# — 係数テーブル —

output$coef_table <- renderDT({
  req(rv$results)
  res <- rv$results

  # 主効果
  main_df <- data.frame(
    変数 = names(res$main_effects),
    係数 = res$main_effects,
    タイプ = "主効果",
    選択 = ifelse(abs(res$main_effects) > 1e-6, "✓", ""),
    stringsAsFactors = FALSE
  )

  # 交互作用
  int_mat <- res$interaction_matrix
  var_names <- res$var_names
  int_list <- list()
  idx <- 1
  for (i in 1:(nrow(int_mat) - 1)) {
    for (j in (i + 1):ncol(int_mat)) {
      coef <- int_mat[i, j]
      int_list[[idx]] <- data.frame(
        変数 = paste0(var_names[i], " × ", var_names[j]),
        係数 = coef,
        タイプ = "交互作用",
        選択 = ifelse(abs(coef) > 1e-6, "✓", ""),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  int_df <- do.call(rbind, int_list)

  # 結合してソート
  coef_df <- rbind(main_df, int_df)
  coef_df$絶対値 <- abs(coef_df$係数)
  coef_df <- coef_df[order(-coef_df$絶対値), ]
  coef_df$絶対値 <- NULL
  rownames(coef_df) <- NULL

  datatable(
    coef_df,
    options = list(
      pageLength = 15,
      dom = 'frtip',
      ordering = FALSE
    ),
    rownames = FALSE,
    selection = 'none'
  ) %>%
    formatRound(columns = "係数", digits = 4) %>%
    formatStyle(
      columns = "選択",
      color = "#3fb950",
      fontWeight = "bold"
    ) %>%
    formatStyle(
      columns = "タイプ",
      color = styleEqual(
        c("主効果", "交互作用"),
        c("#58a6ff", "#a371f7")
      )
    )

})

# — 予測判定グラフ —

output$prediction_plot <- renderPlotly({
  req(rv$results)
  res <- rv$results

  df <- data.frame(
    actual = res$y,
    predicted = as.vector(res$predictions),
    residual = res$y - as.vector(res$predictions)
  )

  # 理想線の範囲
  range_min <- min(c(df$actual, df$predicted)) * 0.95
  range_max <- max(c(df$actual, df$predicted)) * 1.05

  p <- ggplot(df, aes(x = actual, y = predicted)) +
    # 理想線（y=x）
    geom_abline(intercept = 0, slope = 1,
                color = "#30363d", linetype = "dashed", size = 1) +
    # ±10%バンド
    geom_ribbon(
      data = data.frame(x = seq(range_min, range_max, length.out = 100)),
      aes(x = x, ymin = x * 0.9, ymax = x * 1.1),
      inherit.aes = FALSE,
      fill = "#a371f7", alpha = 0.1
    ) +
    # データポイント
    geom_point(aes(
      text = sprintf("実測: %.2f<br>予測: %.2f<br>残差: %.2f",
                     actual, predicted, residual)
    ), color = "#a371f7", alpha = 0.7, size = 3) +
    labs(x = "実測値", y = "予測値") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_line(color = "#21262d"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#8b949e"),
      axis.title = element_text(color = "#c9d1d9")
    ) +
    coord_fixed(ratio = 1, xlim = c(range_min, range_max),
                ylim = c(range_min, range_max))

  ggplotly(p, tooltip = "text") %>%
    layout(
      paper_bgcolor = 'transparent',
      plot_bgcolor = 'transparent',
      font = list(color = '#c9d1d9')
    ) %>%
    config(displayModeBar = FALSE)

})

# — 残差プロット —

output$residual_plot <- renderPlotly({
  req(rv$results)
  res <- rv$results

  df <- data.frame(
    residual = res$y - as.vector(res$predictions)
  )

  p <- ggplot(df, aes(x = residual)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30, fill = "#a371f7", alpha = 0.6, color = "#0d1117") +
    geom_density(color = "#3fb950", size = 1) +
    geom_vline(xintercept = 0, color = "#f85149", linetype = "dashed") +
    labs(x = "残差", y = "密度") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_line(color = "#21262d"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#8b949e"),
      axis.title = element_text(color = "#c9d1d9")
    )

  ggplotly(p) %>%
    layout(
      paper_bgcolor = 'transparent',
      plot_bgcolor = 'transparent',
      font = list(color = '#c9d1d9')
    ) %>%
    config(displayModeBar = FALSE)

})

# — CV プロット —

output$cv_plot <- renderPlot({
  req(rv$cv_fit)

  cv_fit <- rv$cv_fit

  par(bg = "transparent", fg = "#c9d1d9", col.axis = "#8b949e",
      col.lab = "#c9d1d9", col.main = "#c9d1d9", mar = c(5, 4, 2, 2))

  plot(cv_fit$lamlist, cv_fit$cv,
       type = "b", pch = 19, col = "#a371f7",
       xlab = "Lambda", ylab = "Cross-Validation Error",
       main = "")

  # エラーバー
  arrows(cv_fit$lamlist, cv_fit$cv - cv_fit$cv.se,
         cv_fit$lamlist, cv_fit$cv + cv_fit$cv.se,
         length = 0.02, angle = 90, code = 3, col = "#58a6ff")

  # 最適lambda
  abline(v = cv_fit$lamhat, col = "#3fb950", lty = 2, lwd = 2)

  legend("topright",
         legend = c(paste("最適λ =", round(cv_fit$lamhat, 4))),
         col = "#3fb950", lty = 2, lwd = 2,
         text.col = "#c9d1d9",
         bg = "#161b22",
         box.col = "#30363d")

}, bg = "transparent")

# — 主効果プロット —

output$main_effect_plot <- renderPlotly({
  req(rv$results)
  res <- rv$results

  df <- data.frame(
    variable = names(res$main_effects),
    coefficient = res$main_effects,
    abs_coef = abs(res$main_effects)
  )

  df <- df[order(df$abs_coef), ]
  df$variable <- factor(df$variable, levels = df$variable)

  p <- ggplot(df, aes(x = coefficient, y = variable)) +
    geom_col(aes(fill = coefficient > 0), alpha = 0.8, width = 0.7) +
    geom_vline(xintercept = 0, color = "#30363d", size = 0.5) +
    scale_fill_manual(values = c("TRUE" = "#3fb950", "FALSE" = "#f85149"), guide = "none") +
    labs(x = "係数", y = "") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#21262d"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#c9d1d9", size = 11),
      axis.title = element_text(color = "#c9d1d9")
    )

  ggplotly(p) %>%
    layout(
      paper_bgcolor = 'transparent',
      plot_bgcolor = 'transparent',
      font = list(color = '#c9d1d9')
    ) %>%
    config(displayModeBar = FALSE)

})

# — 交互作用ヒートマップ —

output$interaction_heatmap <- renderPlotly({
  req(rv$results)
  res <- rv$results

  int_mat <- res$interaction_matrix
  var_names <- res$var_names

  # ヒートマップ用データ
  df <- expand.grid(
    var1 = var_names,
    var2 = var_names,
    stringsAsFactors = FALSE
  )
  df$value <- as.vector(int_mat)

  p <- ggplot(df, aes(x = var1, y = var2, fill = value)) +
    geom_tile(color = "#21262d", size = 0.5) +
    geom_text(aes(label = ifelse(abs(value) > 1e-6, sprintf("%.2f", value), "")),
              color = "#c9d1d9", size = 3) +
    scale_fill_gradient2(
      low = "#f85149", mid = "#0d1117", high = "#a371f7",
      midpoint = 0, name = "係数"
    ) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid = element_blank(),
      axis.text = element_text(color = "#c9d1d9", size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.background = element_rect(fill = "#161b22", color = "#30363d"),
      legend.text = element_text(color = "#c9d1d9"),
      legend.title = element_text(color = "#c9d1d9")
    ) +
    coord_fixed()

  ggplotly(p) %>%
    layout(
      paper_bgcolor = 'transparent',
      plot_bgcolor = 'transparent',
      font = list(color = '#c9d1d9')
    ) %>%
    config(displayModeBar = FALSE)

})

# — 交互作用テーブル —

output$interaction_table <- renderDT({
  req(rv$results)
  res <- rv$results

  int_mat <- res$interaction_matrix
  var_names <- res$var_names

  # 選択された交互作用のみ抽出
  int_list <- list()
  idx <- 1
  for (i in 1:(nrow(int_mat) - 1)) {
    for (j in (i + 1):ncol(int_mat)) {
      coef <- int_mat[i, j]
      if (abs(coef) > 1e-6) {
        int_list[[idx]] <- data.frame(
          変数1 = var_names[i],
          変数2 = var_names[j],
          交互作用係数 = coef,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
    }
  }

  if (length(int_list) == 0) {
    return(datatable(
      data.frame(メッセージ = "選択された交互作用はありません"),
      options = list(dom = 't'),
      rownames = FALSE
    ))
  }

  int_df <- do.call(rbind, int_list)
  int_df <- int_df[order(-abs(int_df$交互作用係数)), ]
  rownames(int_df) <- NULL

  datatable(
    int_df,
    options = list(
      pageLength = 10,
      dom = 't',
      ordering = FALSE
    ),
    rownames = FALSE
  ) %>%
    formatRound(columns = "交互作用係数", digits = 4) %>%
    formatStyle(
      columns = "交互作用係数",
      color = styleInterval(0, c("#f85149", "#a371f7"))
    )

})
}

# ============================================================================

# アプリ実行

# ============================================================================

shinyApp(ui = ui, server = server)
