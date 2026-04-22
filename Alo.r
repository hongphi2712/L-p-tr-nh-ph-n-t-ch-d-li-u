library(tidyverse)
library(lubridate)
library(xgboost)
library(rsample)
library(yardstick)
library(Matrix)
library(ggrepel)

# ==============================================================================
# PHASE 1: DATA PREPARATION & FEATURE ENGINEERING
# ==============================================================================

# ---------------------------------------------------------
# 1.1. Đọc dữ liệu
# ---------------------------------------------------------
players_final <- read_csv("C:/Users/gtvbe/OneDrive/Desktop/players_final.csv")
today_date <- as.Date("2026-04-22")

# ---------------------------------------------------------
# 1.2. Hàm tiện ích xử lý số liệu
# ---------------------------------------------------------
# Hàm safe_div: Hàm chia an toàn (Safe Division)
# Mục đích: Trong bóng đá, đôi khi cầu thủ không thi đấu hoặc không có dữ liệu (mẫu số = 0 hoặc NA).
# Nếu ta lấy tử số (a) chia mẫu số (b) bình thường trong R sẽ sinh ra lỗi Inf (vô cực) hoặc NaN.
# Hàm này bẫy lỗi đó: Nếu mẫu số (b) bị NA hoặc bằng 0, hàm tự động trả về 0 thay vì báo lỗi.
# Ứng dụng: Cực kỳ quan trọng khi tính các tỷ lệ (Ratios), ví dụ Hiệu suất dứt điểm (Goals / xG).
safe_div <- function(a, b) {
  ifelse(is.na(b) | b == 0, 0, a / b)
}

# Hàm cap_quantile: Hàm cắt đuôi giá trị dị thường (Winsorization / Outlier Capping)
# Mục đích: Xử lý các giá trị ngoại lai (outliers) có thể làm hỏng mô hình Machine Learning.
# Ví dụ thực tế: Một cầu thủ trẻ chỉ đá đúng 10 phút cả mùa nhưng vô tình ghi 1 bàn. Nếu quy đổi 
# ra tỷ lệ "Bàn thắng mỗi 90 phút" (Goals per 90), con số này sẽ cao cao đột biến và phi lý (9 bàn/trận).
# Cơ chế: Cắt bỏ hai đầu cực đoan. Tính phân vị 1% (low) và 99% (high) của tập dữ liệu.
# Hàm pmin(pmax()) sẽ ép các giá trị bé hơn 1% bằng đúng mốc 1%, và lớn hơn 99% bằng đúng mốc 99%.
# Lợi ích: Giúp phân phối dữ liệu bớt lệch (skewed), thuật toán XGBoost không bị nhiễu do các "dị nhân".
cap_quantile <- function(x, low = 0.01, high = 0.99) {
  q_low <- quantile(x, low, na.rm = TRUE)
  q_high <- quantile(x, high, na.rm = TRUE)
  pmin(pmax(x, q_low), q_high)
}

# ---------------------------------------------------------
# 1.3. Tiền xử lý dữ liệu (Clean data + Feature engineering)
# ---------------------------------------------------------
df_clean <- players_final %>%
  mutate(
    # Lọc vị trí hợp lệ
    position = if_else(position %in% c("F", "M", "D", "G"), as.character(position), NA_character_),
    
    # Xử lý thời gian và tuổi tác
    dob = as_datetime(dateofbirth),
    contract_end = if_else(contractuntil > 0, as_datetime(contractuntil), as_datetime(NA_real_)),
    age = as.numeric(interval(dob, today_date) / years(1)),
    contract_years_left = pmax(as.numeric(interval(today_date, contract_end) / years(1)), 0),
    
    # Xử lý missing values bằng giá trị mặc định hoặc median
    marketvalue = replace_na(marketvalue, 0),
    minutesPlayedTotal = replace_na(minutesPlayedTotal, 0),
    height = replace_na(height, median(height, na.rm = TRUE)),
    
    # Điền NA bằng 0 cho các chỉ số chuyên môn
    ratingPG = replace_na(ratingPG, 0),
    goalsP90 = replace_na(goalsP90, 0),
    npExpectedGoalsP90 = replace_na(npExpectedGoalsP90, 0),
    onTargetScoringAttemptP90 = replace_na(onTargetScoringAttemptP90, 0),
    shotOffTargetP90 = replace_na(shotOffTargetP90, 0),
    expectedAssistsP90 = replace_na(expectedAssistsP90, 0),
    keyPassP90 = replace_na(keyPassP90, 0),
    bigChanceCreatedP90 = replace_na(bigChanceCreatedP90, 0),
    touchesP90 = replace_na(touchesP90, 0),
    accuratePassP90 = replace_na(accuratePassP90, 0),
    accurateLongBallsP90 = replace_na(accurateLongBallsP90, 0),
    totalTackleP90 = replace_na(totalTackleP90, 0),
    duelWonP90 = replace_na(duelWonP90, 0),
    yellowCardP90 = replace_na(yellowCardP90, 0),
    redCardP90 = replace_na(redCardP90, 0),
    savesP90 = replace_na(savesP90, 0),
    interceptionWonP90 = replace_na(interceptionWonP90, 0),
    aerialWonP90 = replace_na(aerialWonP90, 0),
    dispossessedP90 = replace_na(dispossessedP90, 0),
    possessionLostCtrlP90 = replace_na(possessionLostCtrlP90, 0),
    wonContestP90 = replace_na(wonContestP90, 0),
    totalCrossP90 = replace_na(totalCrossP90, 0),
    accurateCrossP90 = replace_na(accurateCrossP90, 0),
    totalClearanceP90 = replace_na(totalClearanceP90, 0),
    wasFouledP90 = replace_na(wasFouledP90, 0),
    foulsP90 = replace_na(foulsP90, 0),
    
    # Thông tin cơ bản
    team_name = coalesce(team_name, teamName),
    preferredfoot = replace_na(preferredfoot, "Unknown"),
    
    # Xác định độ tuổi chín muồi (Peak Age) theo vị trí
    peak_age = case_when(
      position == "F" ~ 27,
      position == "M" ~ 26,
      position == "D" ~ 28,
      position == "G" ~ 29,
      TRUE ~ 27
    ),
    age_c = age - peak_age,
    age_sq = age_c^2,
    
    # Log transform & Scaling thời gian thi đấu
    minutes_log = log1p(minutesPlayedTotal),
    minutes_scale = pmin(minutesPlayedTotal / 1800, 1.5),
    
    # Log transform các chỉ số chuyên môn để giảm bớt độ lệch
    goalsP90_log = log1p(goalsP90),
    npxgP90_log = log1p(npExpectedGoalsP90),
    shots_on_target_log = log1p(onTargetScoringAttemptP90),
    shot_off_target_log = log1p(shotOffTargetP90),
    xA_P90_log = log1p(expectedAssistsP90),
    keyPassP90_log = log1p(keyPassP90),
    bigChanceCreatedP90_log = log1p(bigChanceCreatedP90),
    touchesP90_log = log1p(touchesP90),
    accuratePassP90_log = log1p(accuratePassP90),
    accurateLongBallsP90_log = log1p(accurateLongBallsP90),
    tackleP90_log = log1p(totalTackleP90),
    duelWonP90_log = log1p(duelWonP90),
    savesP90_log = log1p(savesP90),
    aerialWonP90_log = log1p(aerialWonP90),
    interceptionsP90_log = log1p(interceptionWonP90),
    contestsP90_log = log1p(wonContestP90),
    clearancesP90_log = log1p(totalClearanceP90),
    crossesP90_log = log1p(totalCrossP90),
    accurateCrossP90_log = log1p(accurateCrossP90),
    
    # Tổ hợp các chỉ số chuyên môn (Composite metrics)
    finishing_efficiency = safe_div(goalsP90, npExpectedGoalsP90 + 0.05),
    shot_accuracy = safe_div(onTargetScoringAttemptP90, onTargetScoringAttemptP90 + shotOffTargetP90 + 0.05),
    creativity_index = expectedAssistsP90 + 0.5 * keyPassP90 + 0.7 * bigChanceCreatedP90,
    ball_security = -(dispossessedP90 + possessionLostCtrlP90),
    passing_mix = accuratePassP90 + 0.6 * accurateLongBallsP90,
    defensive_index = totalTackleP90 + interceptionWonP90 + 0.5 * duelWonP90 + 0.5 * aerialWonP90,
    discipline_index = -(yellowCardP90 + 1.5 * redCardP90),
    gk_distribution = accurateLongBallsP90 + 0.3 * touchesP90,
    gk_activity = savesP90 + 0.15 * touchesP90,
    crossing_quality = safe_div(accurateCrossP90, totalCrossP90 + 0.05),
    
    # Dấu hiệu của một cầu thủ Elite
    elite_signal = case_when(
      ratingPG >= 7.5 ~ 1,
      goalsP90 >= 0.6 ~ 1,
      npExpectedGoalsP90 >= 0.5 ~ 1,
      expectedAssistsP90 >= 0.3 ~ 1,
      TRUE ~ 0
    ),
    
    # Chặn các giá trị ngoại lai (Capping outliers)
    finishing_efficiency = cap_quantile(finishing_efficiency),
    shot_accuracy = cap_quantile(shot_accuracy),
    creativity_index = cap_quantile(creativity_index),
    ball_security = cap_quantile(ball_security),
    passing_mix = cap_quantile(passing_mix),
    defensive_index = cap_quantile(defensive_index),
    discipline_index = cap_quantile(discipline_index),
    gk_distribution = cap_quantile(gk_distribution),
    gk_activity = cap_quantile(gk_activity),
    crossing_quality = cap_quantile(crossing_quality),
    
    # Biến mục tiêu cho model XGBoost
    log_marketvalue = log1p(marketvalue)
  ) %>%
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

# ---------------------------------------------------------
# 2.1. Feature map: Chọn lọc feature theo từng vị trí
# ---------------------------------------------------------
feature_map <- list(
  F = c( # Tiền đạo (Forwards)
    "age", "age_c", "age_sq", "contract_years_left",
    "minutesPlayedTotal", "minutes_log", "minutes_scale",
    "height", "ratingPG",
    "goalsP90", "npExpectedGoalsP90", "onTargetScoringAttemptP90", "shotOffTargetP90",
    "goalsP90_log", "npxgP90_log", "shots_on_target_log", "shot_off_target_log",
    "expectedAssistsP90", "keyPassP90", "bigChanceCreatedP90",
    "xA_P90_log", "keyPassP90_log", "bigChanceCreatedP90_log",
    "touchesP90", "touchesP90_log", "wasFouledP90",
    "finishing_efficiency", "shot_accuracy", "creativity_index",
    "ball_security", "elite_signal"
  ),
  M = c( # Tiền vệ (Midfielders)
    "age", "age_c", "age_sq", "contract_years_left",
    "minutesPlayedTotal", "minutes_log", "minutes_scale",
    "height", "ratingPG",
    "expectedAssistsP90", "keyPassP90", "bigChanceCreatedP90",
    "xA_P90_log", "keyPassP90_log", "bigChanceCreatedP90_log",
    "npExpectedGoalsP90", "goalsP90", "npxgP90_log", "goalsP90_log",
    "touchesP90", "touchesP90_log",
    "accuratePassP90", "accurateLongBallsP90",
    "accuratePassP90_log", "accurateLongBallsP90_log",
    "crossing_quality", "creativity_index", "passing_mix",
    "ball_security", "elite_signal"
  ),
  D = c( # Hậu vệ (Defenders)
    "age", "age_c", "age_sq", "contract_years_left",
    "minutesPlayedTotal", "minutes_log", "minutes_scale",
    "height", "ratingPG",
    "totalTackleP90", "duelWonP90", "aerialWonP90", "interceptionWonP90", "totalClearanceP90",
    "tackleP90_log", "duelWonP90_log", "aerialWonP90_log", "interceptionsP90_log", "clearancesP90_log",
    "accuratePassP90", "accurateLongBallsP90",
    "accuratePassP90_log", "accurateLongBallsP90_log",
    "touchesP90", "touchesP90_log",
    "yellowCardP90", "redCardP90",
    "defensive_index", "passing_mix", "discipline_index", "elite_signal"
  ),
  G = c( # Thủ môn (Goalkeepers)
    "age", "age_c", "age_sq", "contract_years_left",
    "minutesPlayedTotal", "minutes_log", "minutes_scale",
    "height", "ratingPG",
    "savesP90", "savesP90_log",
    "touchesP90", "touchesP90_log",
    "accurateLongBallsP90", "accurateLongBallsP90_log",
    "gk_distribution", "gk_activity", "passing_mix", "elite_signal"
  )
)

# ---------------------------------------------------------
# 2.2. Xây dựng model matrix cho XGBoost
# ---------------------------------------------------------
make_model_matrix <- function(df, features) {
  formula_text <- paste("~", paste(features, collapse = " + "), "-1")
  sparse.model.matrix(as.formula(formula_text), data = df)
}

# ---------------------------------------------------------
# 2.3. Hàm Training XGBoost cho một vị trí cụ thể
# ---------------------------------------------------------
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
  train_df <- training(split_obj)
  test_df  <- testing(split_obj)
  
  x_train <- make_model_matrix(train_df, feature_map[[pos]])
  x_test  <- make_model_matrix(test_df, feature_map[[pos]])
  
  y_train <- train_df$log_marketvalue
  y_test  <- test_df$log_marketvalue
  
  dtrain <- xgb.DMatrix(data = x_train, label = y_train)
  dtest  <- xgb.DMatrix(data = x_test, label = y_test)
  
  # Tham số XGBoost
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.03,
    max_depth = 4,
    min_child_weight = 8,
    subsample = 0.8,
    colsample_bytree = 0.8,
    gamma = 0.1,
    lambda = 1.0,
    alpha = 0.2
  )
  
  # Cross Validation để tìm số vòng lặp tối ưu
  cv_fit <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 300,
    nfold = 5,
    early_stopping_rounds = 20,
    verbose = 1
  )
  
  best_nrounds <- cv_fit$best_iteration
  if (is.null(best_nrounds) || is.na(best_nrounds) || best_nrounds <= 0) {
    best_nrounds <- 100
  }
  
  # Train model cuối cùng
  final_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    watchlist = list(train = dtrain, test = dtest),
    verbose = 0
  )
  
  # Hiệu chỉnh (Calibration) trên tập train
  train_pred_raw <- predict(final_model, newdata = dtrain)
  cal_model <- lm(y_train ~ train_pred_raw)
  
  # Đánh giá trên tập test
  test_pred_raw <- predict(final_model, newdata = dtest)
  test_pred_cal <- predict(cal_model, newdata = tibble(train_pred_raw = test_pred_raw))
  
  test_result <- test_df %>%
    mutate(
      predicted_log_marketvalue_raw = test_pred_raw,
      predicted_log_marketvalue = as.numeric(test_pred_cal),
      predicted_value = pmax(exp(predicted_log_marketvalue) - 1, 0),
      value_ratio = predicted_value / marketvalue,
      undervaluation = predicted_value - marketvalue,
      undervaluation_pct = undervaluation / marketvalue,
      undervaluation_score = log1p(predicted_value) - log1p(marketvalue),
      dataset = "test"
    )
  
  # Ghi nhận các chỉ số đánh giá model (Metrics)
  metric_tbl <- tibble(
    position = pos,
    RMSE_log = sqrt(mean((log1p(test_result$marketvalue) - log1p(test_result$predicted_value))^2, na.rm = TRUE)),
    MAE_log = mean(abs(log1p(test_result$marketvalue) - log1p(test_result$predicted_value)), na.rm = TRUE),
    Correlation = cor(log1p(test_result$marketvalue), log1p(test_result$predicted_value), use = "complete.obs"),
    R2 = Correlation^2,
    n_train = nrow(train_df),
    n_test = nrow(test_df),
    best_nrounds = best_nrounds
  )
  
  list(
    pos = pos,
    split = split_obj,
    train_df = train_df,
    test_df = test_df,
    model = final_model,
    cal_model = cal_model,
    features = feature_map[[pos]],
    metrics = metric_tbl
  )
}

# ---------------------------------------------------------
# 2.4. Hàm dự đoán giá trị cho toàn bộ dữ liệu
# ---------------------------------------------------------
predict_xgb_position <- function(df, model_obj) {
  df_pos <- df %>%
    filter(position == model_obj$pos) %>%
    select(
      player_id, name, team_name, position, age, marketvalue, log_marketvalue,
      all_of(model_obj$features)
    ) %>%
    drop_na()
  
  x_all <- make_model_matrix(df_pos, model_obj$features)
  dall <- xgb.DMatrix(data = x_all)
  
  raw_pred <- predict(model_obj$model, newdata = dall)
  cal_pred <- predict(model_obj$cal_model, newdata = tibble(train_pred_raw = raw_pred))
  
  df_pos %>%
    mutate(
      predicted_log_marketvalue_raw = raw_pred,
      predicted_log_marketvalue = as.numeric(cal_pred),
      predicted_value = pmax(exp(predicted_log_marketvalue) - 1, 0),
      value_ratio = predicted_value / marketvalue,
      undervaluation = predicted_value - marketvalue,
      undervaluation_pct = undervaluation / marketvalue,
      undervaluation_score = log1p(predicted_value) - log1p(marketvalue)
    ) %>%
    arrange(desc(undervaluation_pct))
}

# ---------------------------------------------------------
# 2.5. Khởi chạy quá trình Training & Prediction
# ---------------------------------------------------------
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

# ---------------------------------------------------------
# 2.6. Tổng hợp & Xuất kết quả Training
# ---------------------------------------------------------
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
      marketvalue >= 1000000,
      value_ratio >= 1.10,
      value_ratio <= 2.20,
      undervaluation_pct > 0.10
    ) %>%
    arrange(desc(undervaluation_pct))
}

undervalued_F <- filter_undervalued(result_F)
undervalued_M <- filter_undervalued(result_M)
undervalued_D <- filter_undervalued(result_D)
undervalued_G <- filter_undervalued(result_G)

importance_F <- xgb.importance(model = model_F$model)
importance_M <- xgb.importance(model = model_M$model)
importance_D <- xgb.importance(model = model_D$model)
importance_G <- xgb.importance(model = model_G$model)

cat("\n=== TOP FEATURES: FORWARDS ===\n")
print(head(importance_F, 15))

cat("\n=== TOP FEATURES: MIDFIELDERS ===\n")
print(head(importance_M, 15))

cat("\n=== TOP FEATURES: DEFENDERS ===\n")
print(head(importance_D, 15))

cat("\n=== TOP FEATURES: GOALKEEPERS ===\n")
print(head(importance_G, 15))

# ---------------------------------------------------------
# 2.7. Demo Kết Quả: Các cầu thủ bị đánh giá thấp
# ---------------------------------------------------------
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

# ---------------------------------------------------------
# 2.8. Trực quan hóa dữ liệu (Plots)
# ---------------------------------------------------------
# Sử dụng phiên bản plot_pred_vs_market có tính toán R2
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
      x = Inf, y = -Inf,
      label = paste0("R² = ", round(r2, 3)),
      hjust = 1.1, vjust = -1,
      size = 5
    ) +
    labs(
      title = title_text,
      x = "log(1 + Market Value)",
      y = "log(1 + Predicted Value)"
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
      x = "Player",
      y = "Undervaluation %"
    ) +
    theme_minimal()
}

print(plot_pred_vs_market(result_F, "Forwards: XGBoost Predicted vs Market", "dodgerblue3"))
print(plot_pred_vs_market(result_M, "Midfielders: XGBoost Predicted vs Market", "darkgreen"))
print(plot_pred_vs_market(result_D, "Defenders: XGBoost Predicted vs Market", "purple"))
print(plot_pred_vs_market(result_G, "Goalkeepers: XGBoost Predicted vs Market", "orange"))

print(plot_top_undervalued(undervalued_F, "Top 10 Undervalued Forwards"))
print(plot_top_undervalued(undervalued_M, "Top 10 Undervalued Midfielders"))
print(plot_top_undervalued(undervalued_D, "Top 10 Undervalued Defenders"))
print(plot_top_undervalued(undervalued_G, "Top 10 Undervalued Goalkeepers"))

# ---------------------------------------------------------
# 2.9. Xuất kết quả ra file CSV
# ---------------------------------------------------------
write_csv(all_results, "all_position_predictions_xgboost.csv")
write_csv(undervalued_F, "undervalued_forwards_xgboost.csv")
write_csv(undervalued_M, "undervalued_midfielders_xgboost.csv")
write_csv(undervalued_D, "undervalued_defenders_xgboost.csv")
write_csv(undervalued_G, "undervalued_goalkeepers_xgboost.csv")
write_csv(model_eval, "model_evaluation_xgboost.csv")
write_csv(importance_F, "importance_forwards_xgboost.csv")
write_csv(importance_M, "importance_midfielders_xgboost.csv")
write_csv(importance_D, "importance_defenders_xgboost.csv")
write_csv(importance_G, "importance_goalkeepers_xgboost.csv")


# ==============================================================================
# PHASE 3: SCOUTING LAYER & RECOMMENDATIONS
# ==============================================================================

# ---------------------------------------------------------
# 3.1. Tổng hợp dữ liệu Scout Base
# ---------------------------------------------------------
scout_base <- all_results %>%
  left_join(
    df_clean %>%
      select(
        player_id, minutesPlayedTotal, ratingPG, goalsP90, npExpectedGoalsP90,
        expectedAssistsP90, keyPassP90, totalTackleP90, duelWonP90, savesP90,
        interceptionWonP90, aerialWonP90, accuratePassP90, accurateLongBallsP90, touchesP90
      ),
    by = "player_id",
    suffix = c("", "_joined")
  ) %>%
  mutate(
    # Điểm thi đấu (Tối đa 1 điểm)
    minutes_score = pmin(minutesPlayedTotal_joined / 2000, 1),
    
    # Điểm tiềm năng theo độ tuổi
    age_score = case_when(
      age <= 18 ~ 1.00,
      age <= 20 ~ 0.90,
      age <= 22 ~ 0.75,
      age <= 24 ~ 0.50,
      TRUE ~ 0.20
    ),
    
    # Điểm giá trị (dựa trên chênh lệch giá dự đoán và giá thị trường)
    value_score = pmin(pmax(value_ratio - 1, 0), 1),
    
    # Điểm hiệu suất theo từng vị trí chuyên biệt
    performance_score = case_when(
      position == "F" ~ pmin(
        0.35 * ratingPG_joined +
        1.20 * goalsP90_joined +
        1.00 * npExpectedGoalsP90_joined +
        0.50 * expectedAssistsP90_joined,
        10
      ) / 10,
      
      position == "M" ~ pmin(
        0.40 * ratingPG_joined +
        0.80 * expectedAssistsP90_joined +
        0.60 * keyPassP90_joined,
        10
      ) / 10,
      
      position == "D" ~ pmin(
        0.30 * ratingPG_joined +
        0.40 * totalTackleP90_joined +
        0.35 * duelWonP90_joined +
        0.40 * interceptionWonP90_joined +
        0.25 * aerialWonP90_joined +
        0.35 * accuratePassP90_joined +
        0.25 * accurateLongBallsP90_joined +
        0.25 * touchesP90_joined,
        10
      ) / 10,
      
      position == "G" ~ pmin(
        0.55 * ratingPG_joined +
        0.80 * savesP90_joined,
        10
      ) / 10,
      
      TRUE ~ 0
    ),
    
    # Tổng hợp Điểm Scout (Trọng số kết hợp)
    scout_score =
      0.25 * value_score +
      0.25 * performance_score +
      0.20 * minutes_score +
      0.15 * age_score +
      0.15 * pmin(contract_years_left / 5, 1)
  )

# ---------------------------------------------------------
# 3.2. Phân loại Cầu thủ theo Tiềm Năng & Giá Trị
# ---------------------------------------------------------

# Nhóm 1: Cầu thủ bị định giá thấp so với thực tế (Undervalued)
undervalued_players <- scout_base %>%
  filter(
    marketvalue >= 1000000,
    value_ratio >= 1.10,
    value_ratio <= 2.20,
    minutesPlayedTotal_joined >= 700
  ) %>%
  arrange(desc(scout_score))

# Nhóm 2: Thần đồng (Wonderkids: U20)
wonderkids <- scout_base %>%
  filter(
    age <= 20,
    minutesPlayedTotal_joined >= 500,
    marketvalue >= 500000,
    value_ratio >= 1.05
  ) %>%
  arrange(desc(scout_score))

# Nhóm 3: Tài năng trẻ (Young Talents: 21-23 tuổi)
young_players <- scout_base %>%
  filter(
    age > 20, age <= 23,
    minutesPlayedTotal_joined >= 500,
    marketvalue >= 500000,
    value_ratio >= 1.05
  ) %>%
  arrange(desc(scout_score))

# Nhóm 4: Đột phá (Breakout Stars: 24-27 tuổi)
breakout_players <- scout_base %>%
  filter(
    age > 23, age <= 27,
    minutesPlayedTotal_joined >= 700,
    marketvalue >= 1000000,
    value_ratio >= 1.10
  ) %>%
  arrange(desc(scout_score))

# ==============================================================================
# DEMO KẾT QUẢ SCOUTING
# ==============================================================================
cat("\n=== SCOUTING: UNDERVALUED PLAYERS ===\n")
undervalued_players %>%
  select(name, team_name, position, age, marketvalue, predicted_value, value_ratio, scout_score) %>%
  head(10) %>% print()

cat("\n=== SCOUTING: WONDERKIDS ===\n")
wonderkids %>%
  select(name, team_name, position, age, marketvalue, predicted_value, value_ratio, scout_score) %>%
  head(10) %>% print()

cat("\n=== SCOUTING: YOUNG TALENTS ===\n")
young_players %>%
  select(name, team_name, position, age, marketvalue, predicted_value, value_ratio, scout_score) %>%
  head(10) %>% print()

cat("\n=== SCOUTING: BREAKOUT STARS ===\n")
breakout_players %>%
  select(name, team_name, position, age, marketvalue, predicted_value, value_ratio, scout_score) %>%
  head(10) %>% print()


# ---------------------------------------------------------
# 3.3. Top Players (Những cầu thủ xuất sắc nhất mọi tiêu chí)
# ---------------------------------------------------------
top_players <- all_results %>%
  left_join(
    df_clean %>%
      select(
        player_id, position, minutesPlayedTotal, ratingPG, goalsP90, npExpectedGoalsP90,
        expectedAssistsP90, keyPassP90, totalTackleP90, duelWonP90, savesP90,
        interceptionWonP90, aerialWonP90, accuratePassP90, accurateLongBallsP90, touchesP90
      ),
    by = c("player_id", "position"),
    suffix = c("", "_joined")
  ) %>%
  mutate(
    minutes_score = pmin(minutesPlayedTotal_joined / 3000, 1),
    
    performance_score = case_when(
      position == "F" ~ pmin(
        0.35 * ratingPG_joined +
        1.80 * goalsP90_joined +
        1.20 * npExpectedGoalsP90_joined +
        0.80 * expectedAssistsP90_joined,
        10
      ) / 10,
      
      position == "M" ~ pmin(
        0.40 * ratingPG_joined +
        0.90 * expectedAssistsP90_joined +
        0.70 * keyPassP90_joined +
        0.60 * goalsP90_joined,
        10
      ) / 10,
      
      position == "D" ~ pmin(
        0.30 * ratingPG_joined +
        0.40 * totalTackleP90_joined +
        0.35 * duelWonP90_joined +
        0.40 * interceptionWonP90_joined +
        0.25 * aerialWonP90_joined +
        0.35 * accuratePassP90_joined +
        0.25 * accurateLongBallsP90_joined +
        0.25 * touchesP90_joined,
        10
      ) / 10,
      
      position == "G" ~ pmin(
        0.65 * ratingPG_joined +
        0.50 * savesP90_joined +
        0.30 * log1p(touchesP90_joined),
        10
      ) / 10,
      
      TRUE ~ 0
    ),
    
    market_score = pmin(log1p(marketvalue) / 20, 1),
    
    age_score = case_when(
      age >= 24 & age <= 30 ~ 1.00,
      age >= 21 & age < 24 ~ 0.85,
      age > 30 ~ 0.75,
      TRUE ~ 0.70
    ),
    
    # Điểm đánh giá Cầu thủ Top (Dựa trên Performance, Giá trị, Tuổi)
    top_player_score =
      0.50 * performance_score +
      0.20 * minutes_score +
      0.20 * market_score +
      0.10 * age_score
  ) %>%
  filter(minutesPlayedTotal_joined >= 1000)

# Trích xuất danh sách Top Player theo từng vị trí
top_F <- top_players %>% filter(position == "F") %>% arrange(desc(top_player_score))
top_M <- top_players %>% filter(position == "M") %>% arrange(desc(top_player_score))
top_D <- top_players %>% filter(position == "D") %>% arrange(desc(top_player_score))
top_G <- top_players %>% filter(position == "G") %>% arrange(desc(top_player_score))

cat("\n=== TOP 10 OVERALL FORWARDS ===\n")
top_F %>% select(name, team_name, age, marketvalue, top_player_score) %>% head(10) %>% print()

cat("\n=== TOP 10 OVERALL MIDFIELDERS ===\n")
top_M %>% select(name, team_name, age, marketvalue, top_player_score) %>% head(10) %>% print()

cat("\n=== TOP 10 OVERALL DEFENDERS ===\n")
top_D %>% select(name, team_name, age, marketvalue, top_player_score) %>% head(10) %>% print()

cat("\n=== TOP 10 OVERALL GOALKEEPERS ===\n")
top_G %>% select(name, team_name, age, marketvalue, top_player_score) %>% head(10) %>% print()

# ==============================================================================
# 3.4. Trực quan hóa Bản đồ Tuyển trạch (Scout Map Plot)
# ==============================================================================

scout_map_facet <- scout_base %>%
  filter(
    !is.na(scout_score),
    !is.na(value_ratio),
    minutesPlayedTotal_joined >= 500,
    value_ratio <= 2.5
  )

# 2) Chia trục
x_cut <- 1
y_cut <- quantile(scout_map_facet$scout_score, 0.65, na.rm = TRUE)

# 3) Gán vùng cho từng cầu thủ
scout_map_labeled <- scout_map_facet %>%
  mutate(
    region = case_when(
      value_ratio >= x_cut & scout_score >= y_cut ~ "Top Target",
      value_ratio <  x_cut & scout_score >= y_cut ~ "Overpriced Star",
      value_ratio >= x_cut & scout_score <  y_cut ~ "Hidden Gem",
      TRUE ~ "Low Priority"
    )
  )

# 4) Chỉ lấy 1 cầu thủ nổi bật nhất mỗi vùng trong mỗi vị trí để gắn nhãn
label_data <- scout_map_labeled %>%
  group_by(position, region) %>%
  arrange(desc(scout_score), desc(value_ratio), desc(minutesPlayedTotal_joined)) %>%
  slice_head(n = 1) %>%
  ungroup()

# 5) Tạo nhãn tên vùng theo từng vị trí để không bị lệch khi facet
region_labels <- scout_map_labeled %>%
  group_by(position, region) %>%
  summarise(
    x = median(value_ratio, na.rm = TRUE),
    y = median(scout_score, na.rm = TRUE),
    .groups = "drop"
  )

# 6) Tạo background nhẹ cho 4 vùng
bg_regions <- tibble(
  region = c("Top Target", "Overpriced Star", "Hidden Gem", "Low Priority"),
  xmin = c(x_cut, -Inf, x_cut, -Inf),
  xmax = c(Inf, x_cut, Inf, x_cut),
  ymin = c(y_cut, y_cut, -Inf, -Inf),
  ymax = c(Inf, Inf, y_cut, y_cut)
)

# 7) Vẽ biểu đồ
print(
  ggplot(scout_map_labeled, aes(x = value_ratio, y = scout_score)) +
    geom_rect(
      data = bg_regions,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = region),
      inherit.aes = FALSE,
      alpha = 0.05,
      color = NA
    ) +
    geom_point(
      aes(size = minutesPlayedTotal_joined, color = position),
      alpha = 0.45,
      stroke = 0
    ) +
    geom_vline(xintercept = x_cut, linetype = "dashed", linewidth = 0.5) +
    geom_hline(yintercept = y_cut, linetype = "dashed", linewidth = 0.5) +
    geom_text_repel(
      data = label_data,
      aes(label = name),
      size = 3.2,
      max.overlaps = 100,
      box.padding = 0.35,
      point.padding = 0.2,
      segment.alpha = 0.5,
      show.legend = FALSE
    ) +
    geom_text(
      data = region_labels,
      aes(x = x, y = y, label = region),
      inherit.aes = FALSE,
      size = 3.8,
      fontface = "bold",
      alpha = 0.8
    ) +
    facet_wrap(~ position, scales = "free_x") +
    scale_fill_manual(
      values = c(
        "Top Target" = "#00B894",
        "Overpriced Star" = "#E17055",
        "Hidden Gem" = "#0984E3",
        "Low Priority" = "#B2BEC3"
      ),
      guide = "none"
    ) +
    labs(
      title = "Scout Map by Position",
      subtitle = "Players are grouped by value and quality",
      x = "Value Ratio",
      y = "Scout Score",
      size = "Minutes Played",
      color = "Position"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
)