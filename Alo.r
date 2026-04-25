library(tidyverse)
library(lubridate)
library(xgboost)
library(rsample)
library(yardstick)
library(Matrix)
library(ggrepel)
library(SHAPforxgboost)
library(here)
library(scales)

# ==============================================================================
# PHASE 1: DATA PREPARATION & FEATURE ENGINEERING
# ==============================================================================

# 1.1. Đọc dữ liệu
# ------------------------------------------------------------------------------
players_final <- read_csv(here("players_final.csv"))
today_date <- as.Date("2025-05-10")
# 1.2. Hàm tiện ích xử lý số liệu
# ------------------------------------------------------------------------------

# Hàm safe_div: Hàm chia an toàn (Safe Division)
safe_div <- function(a, b) {
  ifelse(is.na(b) | b == 0, 0, a / b)
}

# Hàm cap_quantile: Hàm cắt đuôi giá trị dị thường (Winsorization / Outlier Capping)
cap_quantile <- function(x, low = 0.01, high = 0.99) {
  q_low  <- quantile(x, low, na.rm = TRUE)
  q_high <- quantile(x, high, na.rm = TRUE)
  pmin(pmax(x, q_low), q_high)
}

# Hàm scale01: Chuẩn hóa dữ liệu về khoảng [0, 1]
scale01 <- function(x) {
  if (all(is.na(x)) || max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    return(rep(0, length(x)))
  }
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# 1.3. Tiền xử lý dữ liệu (Clean data + Feature engineering)
# ------------------------------------------------------------------------------
df_clean <- players_final %>%
  mutate(
    # Lọc vị trí hợp lệ
    position = if_else(position %in% c("F", "M", "D", "G"), as.character(position), NA_character_),
    
    # Xử lý thời gian và tuổi tác
    dob                 = as_datetime(dateofbirth),
    contract_end        = if_else(contractuntil > 0, as_datetime(contractuntil), as_datetime(NA_real_)),
    age                 = as.numeric(interval(dob, today_date) / years(1)),
    contract_years_left = pmax(as.numeric(interval(today_date, contract_end) / years(1)), 0),
    
    # Xử lý missing values bằng giá trị mặc định hoặc median
    marketvalue        = replace_na(marketvalue, 0),
    minutesPlayedTotal = replace_na(minutesPlayedTotal, 0),
    height             = replace_na(height, median(height, na.rm = TRUE)),
    
    # Điền NA bằng 0 cho các chỉ số chuyên môn
    ratingPG                  = replace_na(ratingPG, 0),
    goalsP90                  = replace_na(goalsP90, 0),
    npExpectedGoalsP90        = replace_na(npExpectedGoalsP90, 0),
    onTargetScoringAttemptP90 = replace_na(onTargetScoringAttemptP90, 0),
    shotOffTargetP90          = replace_na(shotOffTargetP90, 0),
    expectedAssistsP90        = replace_na(expectedAssistsP90, 0),
    keyPassP90                = replace_na(keyPassP90, 0),
    bigChanceCreatedP90       = replace_na(bigChanceCreatedP90, 0),
    touchesP90                = replace_na(touchesP90, 0),
    accuratePassP90           = replace_na(accuratePassP90, 0),
    accurateLongBallsP90      = replace_na(accurateLongBallsP90, 0),
    totalTackleP90            = replace_na(totalTackleP90, 0),
    duelWonP90                = replace_na(duelWonP90, 0),
    yellowCardP90             = replace_na(yellowCardP90, 0),
    redCardP90                = replace_na(redCardP90, 0),
    savesP90                  = replace_na(savesP90, 0),
    interceptionWonP90        = replace_na(interceptionWonP90, 0),
    aerialWonP90              = replace_na(aerialWonP90, 0),
    dispossessedP90           = replace_na(dispossessedP90, 0),
    possessionLostCtrlP90     = replace_na(possessionLostCtrlP90, 0),
    wonContestP90             = replace_na(wonContestP90, 0),
    totalCrossP90             = replace_na(totalCrossP90, 0),
    accurateCrossP90          = replace_na(accurateCrossP90, 0),
    totalClearanceP90         = replace_na(totalClearanceP90, 0),
    wasFouledP90              = replace_na(wasFouledP90, 0),
    foulsP90                  = replace_na(foulsP90, 0),
    
    # Thông tin cơ bản
    team_name     = coalesce(team_name, teamName),
    preferredfoot = replace_na(preferredfoot, "Unknown"),
    
    # Xác định độ tuổi chín muồi (Peak Age) theo vị trí
    peak_age = case_when(
      position == "F" ~ 27,
      position == "M" ~ 26,
      position == "D" ~ 28,
      position == "G" ~ 29,
      TRUE ~ 27
    ),
    age_c  = age - peak_age,
    age_sq = age_c^2,
    
    # Log transform & Scaling thời gian thi đấu
    minutes_log   = log1p(minutesPlayedTotal),
    minutes_scale = pmin(minutesPlayedTotal / 1800, 1.5),
    
    # Log transform các chỉ số chuyên môn
    goalsP90_log              = log1p(goalsP90),
    npxgP90_log               = log1p(npExpectedGoalsP90),
    shots_on_target_log       = log1p(onTargetScoringAttemptP90),
    shot_off_target_log       = log1p(shotOffTargetP90),
    xA_P90_log                = log1p(expectedAssistsP90),
    keyPassP90_log            = log1p(keyPassP90),
    bigChanceCreatedP90_log   = log1p(bigChanceCreatedP90),
    touchesP90_log            = log1p(touchesP90),
    accuratePassP90_log       = log1p(accuratePassP90),
    accurateLongBallsP90_log  = log1p(accurateLongBallsP90),
    tackleP90_log             = log1p(totalTackleP90),
    duelWonP90_log            = log1p(duelWonP90),
    savesP90_log              = log1p(savesP90),
    aerialWonP90_log          = log1p(aerialWonP90),
    interceptionsP90_log      = log1p(interceptionWonP90),
    contestsP90_log           = log1p(wonContestP90),
    clearancesP90_log         = log1p(totalClearanceP90),
    crossesP90_log            = log1p(totalCrossP90),
    accurateCrossP90_log      = log1p(accurateCrossP90),
    
    # Tổ hợp các chỉ số chuyên môn (Composite metrics)
    finishing_efficiency = safe_div(goalsP90, npExpectedGoalsP90 + 0.05),
    shot_accuracy        = safe_div(onTargetScoringAttemptP90, onTargetScoringAttemptP90 + shotOffTargetP90 + 0.05),
    creativity_index     = expectedAssistsP90 + 0.5 * keyPassP90 + 0.7 * bigChanceCreatedP90,
    ball_security        = -(dispossessedP90 + possessionLostCtrlP90),
    passing_mix          = accuratePassP90 + 0.6 * accurateLongBallsP90,
    defensive_index      = totalTackleP90 + interceptionWonP90 + 0.5 * duelWonP90 + 0.5 * aerialWonP90,
    discipline_index     = -(yellowCardP90 + 1.5 * redCardP90),
    gk_distribution      = accurateLongBallsP90 + 0.3 * touchesP90,
    gk_activity          = savesP90 + 0.15 * touchesP90,
    crossing_quality     = safe_div(accurateCrossP90, totalCrossP90 + 0.05),
    
    # Dấu hiệu của một cầu thủ Elite
    elite_signal = case_when(
      ratingPG >= 7.5           ~ 1,
      goalsP90 >= 0.6           ~ 1,
      npExpectedGoalsP90 >= 0.5 ~ 1,
      expectedAssistsP90 >= 0.3 ~ 1,
      TRUE                      ~ 0
    ),
    
    # Chặn các giá trị ngoại lai (Capping outliers)
    finishing_efficiency = cap_quantile(finishing_efficiency),
    shot_accuracy        = cap_quantile(shot_accuracy),
    creativity_index     = cap_quantile(creativity_index),
    ball_security        = cap_quantile(ball_security),
    passing_mix          = cap_quantile(passing_mix),
    defensive_index      = cap_quantile(defensive_index),
    discipline_index     = cap_quantile(discipline_index),
    gk_distribution      = cap_quantile(gk_distribution),
    gk_activity          = cap_quantile(gk_activity),
    crossing_quality     = cap_quantile(crossing_quality),
    
    # Biến mục tiêu cho model XGBoost
    log_marketvalue = log1p(marketvalue)
  ) %>%
  # 1.4. Tính Team Effect (Giá trị trung bình đội bóng)
  group_by(team_name) %>%
  mutate(team_avg_value = mean(log_marketvalue, na.rm = TRUE)) %>%
  ungroup() %>%
  # Điều kiện lọc dữ liệu cho training model
  filter(
    !is.na(position),
    marketvalue > 0,
    minutesPlayedTotal >= 300,
    !is.na(age),
    between(age, 15, 45)
  )

# ==============================================================================
# PHASE 2: VALUATION MODEL (XGBOOST)
# ==============================================================================

# 2.1. Feature map: Chọn lọc feature theo từng vị trí
# ------------------------------------------------------------------------------
feature_map <- list(
  F = c( # Tiền đạo (Forwards) - Clean version
    "age", "age_sq", "contract_years_left",
    "minutes_scale", "height", "ratingPG",
    "npExpectedGoalsP90", "goalsP90",
    "expectedAssistsP90", "keyPassP90", "bigChanceCreatedP90",
    "touchesP90", "wasFouledP90",
    "finishing_efficiency", "shot_accuracy", "creativity_index", "ball_security",
    "elite_signal", "team_avg_value"
  ),
  M = c( # Tiền vệ (Midfielders)
    "age", "age_sq", "contract_years_left",
    "minutes_scale", "height", "ratingPG",
    "expectedAssistsP90", "keyPassP90", "bigChanceCreatedP90",
    "npExpectedGoalsP90", "goalsP90",
    "touchesP90", "accuratePassP90", "accurateLongBallsP90",
    "crossing_quality", "creativity_index", "passing_mix", "ball_security",
    "elite_signal", "team_avg_value"
  ),
  D = c( # Hậu vệ (Defenders)
    "age", "age_sq", "contract_years_left",
    "minutes_scale", "height", "ratingPG",
    "totalTackleP90", "duelWonP90", "aerialWonP90", "interceptionWonP90", "totalClearanceP90",
    "accuratePassP90", "accurateLongBallsP90", "touchesP90",
    "defensive_index", "passing_mix", "discipline_index", 
    "elite_signal", "team_avg_value"
  ),
  G = c( # Thủ môn (Goalkeepers)
    "age", "age_sq", "contract_years_left",
    "minutes_scale", "height", "ratingPG",
    "savesP90", "touchesP90", "accurateLongBallsP90",
    "gk_distribution", "gk_activity", "passing_mix", 
    "elite_signal", "team_avg_value"
  )
)

# 2.2. Xây dựng model matrix cho XGBoost
# ------------------------------------------------------------------------------
make_model_matrix <- function(df, features) {
  formula_text <- paste("~", paste(features, collapse = " + "), "-1")
  sparse.model.matrix(as.formula(formula_text), data = df)
}

# 2.3. Hàm Training XGBoost cho một vị trí cụ thể
# ------------------------------------------------------------------------------
train_xgb_position <- function(df, pos, seed = 123) {
  set.seed(seed)
  
  df_pos <- df %>%
    filter(position == pos) %>%
    select(
      player_id, name, team_name, position, marketvalue, log_marketvalue,
      all_of(feature_map[[pos]])
    ) %>%
    drop_na()
  
  # Chia tập train/test 80-20
  split_obj <- initial_split(df_pos, prop = 0.8)
  train_df  <- training(split_obj)
  test_df   <- testing(split_obj)
  
  x_train <- make_model_matrix(train_df, feature_map[[pos]])
  x_test  <- make_model_matrix(test_df, feature_map[[pos]])
  
  y_train <- train_df$log_marketvalue
  y_test  <- test_df$log_marketvalue
  
  dtrain <- xgb.DMatrix(data = x_train, label = y_train)
  dtest  <- xgb.DMatrix(data = x_test, label = y_test)
  
  # Tham số XGBoost (Tối ưu để tránh Overfit)
  params <- list(
    objective        = "reg:squarederror",
    eval_metric      = "rmse",
    eta              = 0.02,
    max_depth        = 3,
    min_child_weight = 10,
    subsample        = 0.7,
    colsample_bytree = 0.7,
    gamma            = 0.2,
    lambda           = 1.5,
    alpha            = 0.5
  )
  
  # Cross Validation để tìm số vòng lặp tối ưu
  cv_fit <- xgb.cv(
    params                = params,
    data                  = dtrain,
    nrounds               = 300,
    nfold                 = 5,
    early_stopping_rounds = 20,
    verbose               = 1
  )
  
  best_nrounds <- cv_fit$best_iteration
  if (is.null(best_nrounds) || is.na(best_nrounds) || best_nrounds <= 0) {
    best_nrounds <- 100
  }
  
  # Train model cuối cùng
  final_model <- xgb.train(
    params    = params,
    data      = dtrain,
    nrounds   = best_nrounds,
    watchlist = list(train = dtrain, test = dtest),
    verbose   = 0
  )
  
  # Hiệu chỉnh (Calibration) trên tập train
  train_pred_raw <- predict(final_model, newdata = dtrain)
  cal_model      <- lm(y_train ~ train_pred_raw)
  
  # Đánh giá trên tập test
  test_pred_raw <- predict(final_model, newdata = dtest)
  test_pred_cal <- predict(cal_model, newdata = tibble(train_pred_raw = test_pred_raw))
  
  test_result <- test_df %>%
    mutate(
      predicted_log_marketvalue_raw = test_pred_raw,
      predicted_log_marketvalue     = as.numeric(test_pred_cal),
      predicted_value               = pmax(exp(predicted_log_marketvalue) - 1, 0),
      value_ratio                   = predicted_value / marketvalue,
      undervaluation                = predicted_value - marketvalue,
      undervaluation_pct            = undervaluation / marketvalue,
      undervaluation_score          = log1p(predicted_value) - log1p(marketvalue),
      dataset                       = "test"
    )
  
  # Ghi nhận các chỉ số đánh giá model (Metrics)
  metric_tbl <- tibble(
    position    = pos,
    RMSE_log    = sqrt(mean((log1p(test_result$marketvalue) - log1p(test_result$predicted_value))^2, na.rm = TRUE)),
    MAE_log     = mean(abs(log1p(test_result$marketvalue) - log1p(test_result$predicted_value)), na.rm = TRUE),
    Correlation = cor(log1p(test_result$marketvalue), log1p(test_result$predicted_value), use = "complete.obs"),
    R2          = Correlation^2,
    n_train     = nrow(train_df),
    n_test      = nrow(test_df),
    best_nrounds = best_nrounds
  )
  
  list(
    pos       = pos,
    split     = split_obj,
    train_df  = train_df,
    test_df   = test_df,
    model     = final_model,
    cal_model = cal_model,
    features  = feature_map[[pos]],
    metrics   = metric_tbl,
    x_train   = x_train # Trả về x_train để dùng cho SHAP
  )
}

# 2.4. Hàm dự đoán giá trị cho toàn bộ dữ liệu
# ------------------------------------------------------------------------------
predict_xgb_position <- function(df, model_obj) {
  df_pos <- df %>%
    filter(position == model_obj$pos) %>%
    select(
      player_id, name, team_name, position, age, marketvalue, log_marketvalue,
      all_of(model_obj$features)
    ) %>%
    drop_na()
  
  x_all <- make_model_matrix(df_pos, model_obj$features)
  dall  <- xgb.DMatrix(data = x_all)
  
  raw_pred <- predict(model_obj$model, newdata = dall)
  cal_pred <- predict(model_obj$cal_model, newdata = tibble(train_pred_raw = raw_pred))
  
  df_pos %>%
    mutate(
      predicted_log_marketvalue_raw = raw_pred,
      predicted_log_marketvalue     = as.numeric(cal_pred),
      predicted_value               = pmax(exp(predicted_log_marketvalue) - 1, 0),
      value_ratio                   = predicted_value / marketvalue,
      undervaluation                = predicted_value - marketvalue,
      undervaluation_pct            = undervaluation / marketvalue,
      undervaluation_score          = log1p(predicted_value) - log1p(marketvalue)
    ) %>%
    arrange(desc(undervaluation_pct))
}

# 2.5. Khởi chạy quá trình Training & Prediction
# ------------------------------------------------------------------------------
model_F <- train_xgb_position(df_clean, "F")
model_M <- train_xgb_position(df_clean, "M")
model_D <- train_xgb_position(df_clean, "D")
model_G <- train_xgb_position(df_clean, "G")

models <- list(F = model_F, M = model_M, D = model_D, G = model_G)

result_F <- predict_xgb_position(df_clean, model_F)
result_M <- predict_xgb_position(df_clean, model_M)
result_D <- predict_xgb_position(df_clean, model_D)
result_G <- predict_xgb_position(df_clean, model_G)

all_results <- bind_rows(result_F, result_M, result_D, result_G)

# 2.6. Tổng hợp & Xuất kết quả Training
# ------------------------------------------------------------------------------
model_eval <- bind_rows(
  model_F$metrics,
  model_M$metrics,
  model_D$metrics,
  model_G$metrics
)

cat("\n=== ĐÁNH GIÁ MÔ HÌNH ===\n")
print(model_eval)

filter_undervalued <- function(df) {
  df %>%
    filter(
      marketvalue        >= 1000000,
      value_ratio        >= 1.10,
      value_ratio        <= 2.20,
      undervaluation_pct > 0.10
    ) %>%
    arrange(desc(undervaluation_pct))
}

undervalued_F <- filter_undervalued(result_F)
undervalued_M <- filter_undervalued(result_M)
undervalued_D <- filter_undervalued(result_D)
undervalued_G <- filter_undervalued(result_G)

# 2.7. Interpretability (SHAP Values)
# ------------------------------------------------------------------------------
cat("\n=== TÍNH TOÁN SHAP VALUES ===\n")

plot_shap_summary <- function(model_obj, title_text) {
  X_train_mat <- as.matrix(model_obj$x_train)
  
  shap_values <- shap.values(
    xgb_model = model_obj$model,
    X_train   = X_train_mat
  )
  
  shap_long <- shap.prep(
    shap_contrib = shap_values$shap_score,
    X_train      = X_train_mat
  )
  
  shap.plot.summary(shap_long) +
    labs(title = title_text)
}

get_shap_importance <- function(model_obj) {
  X_train_mat <- as.matrix(model_obj$x_train)
  
  shap_values <- shap.values(
    xgb_model = model_obj$model,
    X_train   = X_train_mat
  )
  
  tibble(
    Feature   = names(shap_values$mean_shap_score),
    Mean_SHAP = as.numeric(shap_values$mean_shap_score)
  ) %>%
    filter(Feature != "BIAS") %>%
    arrange(desc(Mean_SHAP))
}

# Vẽ SHAP summary
print(plot_shap_summary(model_F, "SHAP Summary: Forwards"))
print(plot_shap_summary(model_M, "SHAP Summary: Midfielders"))
print(plot_shap_summary(model_D, "SHAP Summary: Defenders"))
print(plot_shap_summary(model_G, "SHAP Summary: Goalkeepers"))

# Lưu SHAP importance
importance_F <- get_shap_importance(model_F)
importance_M <- get_shap_importance(model_M)
importance_D <- get_shap_importance(model_D)
importance_G <- get_shap_importance(model_G)

# 2.8. Demo Kết Quả: Các cầu thủ bị đánh giá thấp
# ------------------------------------------------------------------------------
cat("\n=== TOP 10 UNDERVALUED FORWARDS ===\n")
print(head(undervalued_F, 10), n = 10)

cat("\n=== TOP 10 UNDERVALUED MIDFIELDERS ===\n")
print(head(undervalued_M, 10), n = 10)

cat("\n=== TOP 10 UNDERVALUED DEFENDERS ===\n")
print(head(undervalued_D, 10), n = 10)

cat("\n=== TOP 10 UNDERVALUED GOALKEEPERS ===\n")
print(head(undervalued_G, 10), n = 10)

cat("\n=== TOP 30 HIGHEST MODEL-IMPLIED VALUES ===\n")
all_results %>%
  filter(value_ratio <= 2.2) %>%
  arrange(desc(predicted_value)) %>%
  head(30) %>%
  print(n = 30)

# 2.9. Trực quan hóa dữ liệu (Plots)
# ------------------------------------------------------------------------------
plot_pred_vs_market <- function(df_result, title_text, point_color = "steelblue") {
  r2 <- cor(log1p(df_result$marketvalue),
            log1p(df_result$predicted_value),
            use = "complete.obs")^2
  
  ggplot(df_result, aes(x = log1p(marketvalue), y = log1p(predicted_value))) +
    geom_point(alpha = 0.45, color = point_color) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    annotate(
      "text",
      x     = Inf, y = -Inf,
      label = paste0("R² = ", round(r2, 3)),
      hjust = 1.1, vjust = -1,
      size  = 5
    ) +
    labs(
      title = title_text,
      x     = "log(1 + Market Value)",
      y     = "log(1 + Predicted Value)"
    ) +
    theme_minimal()
}

plot_top_undervalued <- function(df_result, title_text) {
  df_result %>%
    head(10) %>%
    ggplot(aes(x = reorder(name, undervaluation_pct), y = undervaluation_pct)) +
    geom_col() +
    coord_flip() +
    labs(
      title = title_text,
      x     = "Player",
      y     = "Undervaluation %"
    ) +
    theme_minimal()
}


# 2.10. Xuất kết quả ra file CSV
# ------------------------------------------------------------------------------
write_csv(all_results, "all_position_predictions_xgboost.csv")
write_csv(undervalued_F, "undervalued_forwards_xgboost.csv")
write_csv(undervalued_M, "undervalued_midfielders_xgboost.csv")
write_csv(undervalued_D, "undervalued_defenders_xgboost.csv")
write_csv(undervalued_G, "undervalued_goalkeepers_xgboost.csv")
write_csv(model_eval,    "model_evaluation_xgboost.csv")

# ==============================================================================
# PHASE 3: SCOUTING LAYER & RECOMMENDATIONS
# ==============================================================================

# 3.1. Tổng hợp dữ liệu Scout Base
# ------------------------------------------------------------------------------
scout_base <- all_results %>%
  left_join(
    df_clean %>%
      select(
        player_id, minutesPlayedTotal, ratingPG, goalsP90, npExpectedGoalsP90,
        expectedAssistsP90, keyPassP90, totalTackleP90, duelWonP90, savesP90,
        interceptionWonP90, aerialWonP90, accuratePassP90, accurateLongBallsP90, touchesP90
      ) %>%
      rename(
        minutesPlayedTotal_joined = minutesPlayedTotal,
        ratingPG_joined = ratingPG,
        goalsP90_joined = goalsP90,
        npExpectedGoalsP90_joined = npExpectedGoalsP90,
        expectedAssistsP90_joined = expectedAssistsP90,
        keyPassP90_joined = keyPassP90,
        totalTackleP90_joined = totalTackleP90,
        duelWonP90_joined = duelWonP90,
        savesP90_joined = savesP90,
        interceptionWonP90_joined = interceptionWonP90,
        aerialWonP90_joined = aerialWonP90,
        accuratePassP90_joined = accuratePassP90,
        accurateLongBallsP90_joined = accurateLongBallsP90,
        touchesP90_joined = touchesP90
      ),
    by = "player_id"
  ) %>%
  mutate(
    # Điểm hiệu suất theo từng vị trí
    performance_score = case_when(
      position == "F" ~ (0.35 * ratingPG_joined + 1.2 * goalsP90_joined + 1.0 * npExpectedGoalsP90_joined + 0.5 * expectedAssistsP90_joined),
      position == "M" ~ (0.40 * ratingPG_joined + 0.8 * expectedAssistsP90_joined + 0.6 * keyPassP90_joined),
      position == "D" ~ (0.30 * ratingPG_joined + 0.4 * totalTackleP90_joined + 0.35 * duelWonP90_joined + 0.4 * interceptionWonP90_joined),
      position == "G" ~ (0.55 * ratingPG_joined + 0.8 * savesP90_joined),
      TRUE            ~ 0
    )
  ) %>%
  group_by(position) %>%
  mutate(
    perf_norm = scale01(performance_score),
    min_norm  = scale01(minutesPlayedTotal_joined),
    age_norm  = 1 - scale01(age)
  ) %>%
  ungroup() %>%
  mutate(
    player_quality_score = 0.5 * perf_norm + 0.3 * min_norm + 0.2 * age_norm,
    value_score          = scale01(value_ratio),
    scout_score          = 0.8 * player_quality_score + 0.2 * value_score
  )

# 3.2. Phân loại Cầu thủ theo Tiềm Năng & Giá Trị
# ------------------------------------------------------------------------------

# Nhóm 1: Cầu thủ bị định giá thấp (Undervalued)
undervalued_players <- scout_base %>%
  filter(
    marketvalue               >= 1000000,
    value_ratio               >= 1.10,
    value_ratio               <= 2.20,
    minutesPlayedTotal_joined >= 700
  ) %>%
  arrange(desc(scout_score))

# Nhóm 2: Thần đồng (Wonderkids: U20)
wonderkids <- scout_base %>%
  filter(
    age                       <= 20,
    minutesPlayedTotal_joined >= 500,
    marketvalue               >= 500000,
    value_ratio               >= 1.05
  ) %>%
  arrange(desc(scout_score))

# Nhóm 3: Tài năng trẻ (Young Talents: 21-23 tuổi)
young_players <- scout_base %>%
  filter(
    age                       > 20, age <= 23,
    minutesPlayedTotal_joined >= 500,
    marketvalue               >= 500000,
    value_ratio               >= 1.05
  ) %>%
  arrange(desc(scout_score))

# Nhóm 4: Đột phá (Breakout Stars: 24-27 tuổi)
breakout_players <- scout_base %>%
  filter(
    age                       > 23, age <= 27,
    minutesPlayedTotal_joined >= 700,
    marketvalue               >= 1000000,
    value_ratio               >= 1.10
  ) %>%
  arrange(desc(scout_score))

# 3.3. Top Players Overall
# ------------------------------------------------------------------------------
top_players <- all_results %>%
  left_join(
    df_clean %>%
      select(
        player_id, position, 
        minutesPlayedTotal, ratingPG, goalsP90, npExpectedGoalsP90,
        expectedAssistsP90, keyPassP90, totalTackleP90, duelWonP90,
        savesP90, interceptionWonP90, aerialWonP90,
        accuratePassP90, accurateLongBallsP90, touchesP90
      ) %>%
      rename(
        minutesPlayedTotal_joined = minutesPlayedTotal,
        ratingPG_joined = ratingPG,
        goalsP90_joined = goalsP90,
        npExpectedGoalsP90_joined = npExpectedGoalsP90,
        expectedAssistsP90_joined = expectedAssistsP90,
        keyPassP90_joined = keyPassP90,
        totalTackleP90_joined = totalTackleP90,
        duelWonP90_joined = duelWonP90,
        savesP90_joined = savesP90,
        interceptionWonP90_joined = interceptionWonP90,
        aerialWonP90_joined = aerialWonP90,
        accuratePassP90_joined = accuratePassP90,
        accurateLongBallsP90_joined = accurateLongBallsP90,
        touchesP90_joined = touchesP90
      ),
    by = c("player_id", "position")
  ) %>%
  mutate(
    minutes_score = pmin(minutesPlayedTotal_joined / 3000, 1),
    
    performance_score = case_when(
      position == "F" ~ pmin(0.35 * ratingPG_joined + 1.80 * goalsP90_joined + 1.20 * npExpectedGoalsP90_joined + 0.80 * expectedAssistsP90_joined, 10) / 10,
      position == "M" ~ pmin(0.40 * ratingPG_joined + 0.90 * expectedAssistsP90_joined + 0.70 * keyPassP90_joined + 0.60 * goalsP90_joined, 10) / 10,
      position == "D" ~ pmin(0.30 * ratingPG_joined + 0.40 * totalTackleP90_joined + 0.35 * duelWonP90_joined + 0.40 * interceptionWonP90_joined + 0.25 * aerialWonP90_joined + 0.35 * accuratePassP90_joined + 0.25 * accurateLongBallsP90_joined + 0.25 * touchesP90_joined, 10) / 10,
      position == "G" ~ pmin(0.65 * ratingPG_joined + 0.50 * savesP90_joined + 0.30 * log1p(touchesP90_joined), 10) / 10,
      TRUE            ~ 0
    ),
    
    market_score = pmin(log1p(marketvalue) / 20, 1),
    
    age_score = case_when(
      age >= 24 & age <= 30 ~ 1.00,
      age >= 21 & age < 24  ~ 0.85,
      age > 30              ~ 0.75,
      TRUE                  ~ 0.70
    ),
    
    top_player_score = 0.50 * performance_score + 0.20 * minutes_score + 0.20 * market_score + 0.10 * age_score
  ) %>%
  filter(minutesPlayedTotal_joined >= 1000)

# Trích xuất danh sách Top Player theo vị trí
top_F <- top_players %>% filter(position == "F") %>% arrange(desc(top_player_score))
top_M <- top_players %>% filter(position == "M") %>% arrange(desc(top_player_score))
top_D <- top_players %>% filter(position == "D") %>% arrange(desc(top_player_score))
top_G <- top_players %>% filter(position == "G") %>% arrange(desc(top_player_score))

# ==============================================================================
# DEMO KẾT QUẢ SCOUTING (Console Output)
# ==============================================================================

cat("\n=== SCOUTING: UNDERVALUED PLAYERS ===\n")
undervalued_players %>%
  select(name, team_name, position, age, marketvalue, predicted_value, value_ratio, scout_score) %>%
  head(10) %>% print()

cat("\n=== SCOUTING: WONDERKIDS (U20) ===\n")
wonderkids %>%
  select(name, team_name, position, age, marketvalue, predicted_value, value_ratio, scout_score) %>%
  head(10) %>% print()

cat("\n=== SCOUTING: YOUNG TALENTS (21-23) ===\n")
young_players %>%
  select(name, team_name, position, age, marketvalue, predicted_value, value_ratio, scout_score) %>%
  head(10) %>% print()

cat("\n=== SCOUTING: BREAKOUT STARS (24-27) ===\n")
breakout_players %>%
  select(name, team_name, position, age, marketvalue, predicted_value, value_ratio, scout_score) %>%
  head(10) %>% print()

cat("\n=== TOP 10 OVERALL FORWARDS ===\n")
top_F %>% select(name, team_name, age, marketvalue, top_player_score) %>% head(10) %>% print()

cat("\n=== TOP 10 OVERALL MIDFIELDERS ===\n")
top_M %>% select(name, team_name, age, marketvalue, top_player_score) %>% head(10) %>% print()

cat("\n=== TOP 10 OVERALL DEFENDERS ===\n")
top_D %>% select(name, team_name, age, marketvalue, top_player_score) %>% head(10) %>% print()

cat("\n=== TOP 10 OVERALL GOALKEEPERS ===\n")
top_G %>% select(name, team_name, age, marketvalue, top_player_score) %>% head(10) %>% print()

# Check cụ thể một cầu thủ tiềm năng (Ví dụ: Id 581310)
cat("\n=== CHI TIẾT CẦU THỦ TIỀM NĂNG (ID: 581310) ===\n")
top_players %>%
  filter(player_id == 581310) %>%
  select(player_id, name, team_name, position, age, marketvalue, top_player_score) %>%
  print()

# 3.4. Trực quan hóa kết quả Scouting
# ------------------------------------------------------------------------------

# Biểu đồ Nghịch lý Chuyển nhượng (Slide 2)
plot_problem_statement <- function(data) {
  sample_data <- data %>%
    filter(!is.na(player_quality_score), marketvalue >= 1000000) %>%
    mutate(
      category = case_when(
        value_ratio > 1.5 & player_quality_score > quantile(player_quality_score, 0.75, na.rm=TRUE) ~ "Món hời (Hidden Gem)",
        value_ratio < 0.5 & player_quality_score < quantile(player_quality_score, 0.50, na.rm=TRUE) ~ "Bị làm giá (Overpriced)",
        TRUE                                                                                       ~ "Bình thường"
      )
    )
  
  label_data <- sample_data %>% 
    filter(category != "Bình thường") %>% 
    group_by(category) %>% 
    top_n(5, wt = abs(value_ratio - 1)) %>% 
    ungroup()
  
  ggplot(sample_data, aes(x = marketvalue / 1e6, y = player_quality_score)) +
    geom_point(aes(color = category), alpha = 0.5, size = 2) +
    geom_text_repel(data = label_data, aes(label = name), size = 3.5, fontface = "bold") +
    scale_x_log10(labels = scales::comma) +
    scale_color_manual(values = c("Món hời (Hidden Gem)" = "#00B894", 
                                  "Bị làm giá (Overpriced)" = "#D63031", 
                                  "Bình thường"            = "#DFE6E9")) +
    labs(
      title    = "Nghịch lý Chuyển nhượng: Giá trị thị trường vs Năng lực thực tế",
      subtitle = "Nhiều cầu thủ đắt giá nhưng hiệu suất kém, trong khi các 'món hời' bị bỏ quên",
      x        = "Giá thị trường (Triệu Euro - Log Scale)",
      y        = "Điểm chất lượng (Player Quality Score)",
      color    = "Phân loại"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 14))
}

# Biểu đồ Peak Age Logic (Slide 6)
plot_peak_age <- function(df) {
  df_age <- df %>%
    filter(marketvalue > 0, age >= 16, age <= 40, !is.na(position)) %>%
    mutate(position = factor(position, levels = c("F", "M", "D", "G"),
                             labels = c("Tiền đạo (F - 27t)", "Tiền vệ (M - 26t)", 
                                        "Hậu vệ (D - 28t)", "Thủ môn (G - 29t)")))
  
  peak_lines <- data.frame(
    position = c("Tiền đạo (F - 27t)", "Tiền vệ (M - 26t)", "Hậu vệ (D - 28t)", "Thủ môn (G - 29t)"),
    peak_age = c(27, 26, 28, 29)
  )
  
  ggplot(df_age, aes(x = age, y = marketvalue / 1e6)) +
    geom_smooth(method = "loess", se = TRUE, color = "#0984E3", fill = "#74B9FF", alpha = 0.2) +
    geom_vline(data = peak_lines, aes(xintercept = peak_age), color = "#D63031", linetype = "dashed", linewidth = 1) +
    facet_wrap(~ position, scales = "free_y") +
    labs(
      title    = "Đường cong Giá trị theo Độ tuổi (Peak Age Logic)",
      subtitle = "Mỗi vị trí có một thời kỳ 'chín muồi' khác nhau (Đường đứt nét đỏ)",
      x        = "Độ tuổi",
      y        = "Giá trị thị trường trung bình (Triệu Euro)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14), strip.text = element_text(face = "bold", size = 11))
}

# Biểu đồ SHAP Importance (Slide 13)
plot_shap_importance <- function(importance_matrix, pos_title, bar_color) {
  
  importance_matrix %>%
    filter(Feature != "BIAS") %>%
    arrange(desc(Mean_SHAP)) %>%
    slice_head(n = 10) %>%
    mutate(
      Feature = forcats::fct_reorder(as.factor(Feature), Mean_SHAP)
    ) %>%
    
    ggplot(aes(x = Feature, y = Mean_SHAP)) +
    
    geom_col(fill = bar_color, alpha = 0.85) +
    
    coord_flip() +
    
    geom_text(
      aes(label = round(Mean_SHAP, 3)),
      hjust = -0.1,
      size = 3.5
    ) +
    
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.2))
    ) +
    
    labs(
      title    = paste("Top 10 SHAP Importance -", pos_title),
      subtitle = "Mean |SHAP value| trong mô hình XGBoost",
      x        = "Feature",
      y        = "Mean |SHAP|"
    ) +
    
    theme_minimal()
}

# 3.4. Trực quan hóa Bản đồ Tuyển trạch (Scout Map Plot)
# ------------------------------------------------------------------------------
create_scout_viewer <- function(data, title_text) {
  
  scout_map_data <- data %>%
    filter(
      !is.na(scout_score),
      !is.na(value_ratio),
      minutesPlayedTotal_joined >= 500,
      value_ratio <= 2.5
    ) %>%
    mutate(
      tooltip = paste0(
        "<b>", name, "</b>",
        "<br>Team: ", team_name,
        "<br>Position: ", position,
        "<br>Age: ", round(age, 1),
        "<br>Minutes: ", round(minutesPlayedTotal_joined, 0),
        "<br>Market Value: €", round(marketvalue / 1e6, 2), "M",
        "<br>Predicted Value: €", round(predicted_value / 1e6, 2), "M",
        "<br>Value Ratio: ", round(value_ratio, 2),
        "<br>Scout Score: ", round(scout_score, 3),
        "<br>Quality Score: ", round(player_quality_score, 3)
      )
    )
  
  if (nrow(scout_map_data) == 0) return(NULL)
  
  x_cut <- 1
  y_cut <- quantile(scout_map_data$scout_score, 0.65, na.rm = TRUE)
  
  scout_map_data <- scout_map_data %>%
    mutate(
      region = case_when(
        value_ratio >= x_cut & scout_score >= y_cut ~ "Top Target",
        value_ratio <  x_cut & scout_score >= y_cut ~ "Overpriced Star",
        value_ratio >= x_cut & scout_score <  y_cut ~ "Hidden Gem",
        TRUE ~ "Low Priority"
      )
    )
  
  p <- ggplot(
    scout_map_data,
    aes(
      x = value_ratio,
      y = scout_score,
      color = position,
      size = minutesPlayedTotal_joined,
      text = tooltip
    )
  ) +
    geom_point(alpha = 0.65) +
    geom_vline(xintercept = x_cut, linetype = "dashed") +
    geom_hline(yintercept = y_cut, linetype = "dashed") +
    labs(
      title = title_text,
      subtitle = "Click / hover từng node để xem thông tin cầu thủ",
      x = "Value Ratio (Predicted / Market)",
      y = "Scout Score",
      color = "Vị trí",
      size = "Phút thi đấu"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  plotly::ggplotly(p, tooltip = "text") %>%
    plotly::layout(
      hoverlabel = list(align = "left"),
      legend = list(orientation = "h")
    )
}


print(plot_pred_vs_market(result_F, "Forwards: XGBoost Predicted vs Market", "dodgerblue3"))
print(plot_pred_vs_market(result_M, "Midfielders: XGBoost Predicted vs Market", "darkgreen"))
print(plot_pred_vs_market(result_D, "Defenders: XGBoost Predicted vs Market", "purple"))
print(plot_pred_vs_market(result_G, "Goalkeepers: XGBoost Predicted vs Market", "orange"))


# In các biểu đồ phân tích cơ bản
print(plot_problem_statement(scout_base))
print(plot_peak_age(df_clean))
print(plot_shap_importance(importance_F, "Tiền đạo (Forwards)", "#0984E3"))
print(plot_shap_importance(importance_M, "Tiền vệ (Midfielders)", "#00B894"))
print(plot_shap_importance(importance_D, "Hậu vệ (Defenders)", "#6C5CE7"))
print(plot_shap_importance(importance_G, "Thủ môn (Goalkeepers)", "#E17055"))

# In các bản đồ tuyển trạch chuyên sâu
create_scout_viewer(breakout_players, "Interactive Scout Viewer: Breakout Stars")
create_scout_viewer(wonderkids, "Interactive Scout Viewer: Wonderkids")
create_scout_viewer(young_players, "Interactive Scout Viewer: Young Talents")
create_scout_viewer(undervalued_players, "Interactive Scout Viewer: Undervalued Players")

top10_buy_players <- undervalued_players %>%
  filter(
    !is.na(scout_score),
    !is.na(value_ratio),
    !is.na(undervaluation),
    marketvalue >= 1000000,
    minutesPlayedTotal_joined >= 700,
    value_ratio >= 1.10,
    value_ratio <= 2.20
  ) %>%
  arrange(desc(scout_score)) %>%
  slice_head(n = 10) %>%
  mutate(
    name_label = paste0(name, " (", position, ")"),
    market_m = marketvalue / 1e6,
    predicted_m = predicted_value / 1e6,
    undervaluation_m = undervaluation / 1e6
  )

# In bảng Top 10
cat("\n=== TOP 10 CẦU THỦ ĐÁNG MUA ===\n")
top10_buy_players %>%
  select(
    name, team_name, position, age,
    minutesPlayedTotal_joined,
    marketvalue, predicted_value,
    value_ratio, scout_score
  ) %>%
  mutate(
    age = round(age, 1),
    marketvalue = round(marketvalue / 1e6, 2),
    predicted_value = round(predicted_value / 1e6, 2),
    value_ratio = round(value_ratio, 2),
    scout_score = round(scout_score, 3)
  ) %>%
  rename(
    Team = team_name,
    Position = position,
    Age = age,
    Minutes = minutesPlayedTotal_joined,
    `Market Value (€M)` = marketvalue,
    `Predicted Value (€M)` = predicted_value,
    `Value Ratio` = value_ratio,
    `Scout Score` = scout_score
  ) %>%
  print(n = 10)

# Biểu đồ Top 10
ggplot(
  top10_buy_players,
  aes(
    x = reorder(name_label, scout_score),
    y = scout_score,
    fill = position
  )
) +
  geom_col(width = 0.7, alpha = 0.9) +
  coord_flip() +
  geom_text(
    aes(
      label = paste0(
        "VR: ", round(value_ratio, 2),
        " | €", round(market_m, 1), "M → €", round(predicted_m, 1), "M"
      )
    ),
    hjust = -0.05,
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, max(top10_buy_players$scout_score, na.rm = TRUE) * 1.25)
  ) +
  labs(
    title = "Top 10 Cầu thủ đáng mua",
    subtitle = "Xếp hạng theo Scout Score, lọc các cầu thủ đang bị định giá thấp",
    x = "",
    y = "Scout Score",
    fill = "Position"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )
