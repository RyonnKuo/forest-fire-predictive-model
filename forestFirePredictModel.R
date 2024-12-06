library(naniar)
library(readr)
library(jsonlite)
library(sf)
library(XML)
library(xml2)
library(readxl)
library(dplyr)
library(randomForest)
library(ggplot2)
library(car)
library(glmnet)
library(caret)
library(pROC)


path <- getwd();path
setwd(path)

#讀取山火紀錄
forest_fire_record <- read_excel("~/R_data/final_exam_data/forest_fire_data.xlsx")

# 讀取測站名單 
json_data <- fromJSON("~/R_data/final_exam_data/weather_station.json")
station_detail <- as.data.frame(json_data$records$Station)

# 9年內測站資訊xml位置
file_list <- list.files(path = "~/R_data/final_exam_data/weather_station_9y", pattern = "\\.xml$", full.names = TRUE)

# 清洗+整理 xml function
extract_location_xml_data <- function(file_path) {
  xml_data <- read_xml(file_path)
  
  # 定義namespace
  ns <- c(cwa = "urn:cwa:gov:tw:cwacommon:0.1")
  
  # 拆出所有需要的資料單元
  locations <- xml_find_all(xml_data, ".//cwa:location", ns)
  
  # 細項拆解+合併成data frame
  data <- tibble(
    StationID = xml_text(xml_find_all(locations, ".//cwa:StationID", ns)),
    StationName = xml_text(xml_find_all(locations, ".//cwa:StationName", ns)),
    YearMonth = sapply(xml_find_all(locations, ".//cwa:stationObsStatistics/cwa:YearMonth", ns), xml_text),
    MeanAirTemperature = sapply(xml_find_all(locations, ".//cwa:AirTemperature/cwa:monthly/cwa:Mean", ns), xml_text),
    MaximumAirTemperature = sapply(xml_find_all(locations, ".//cwa:AirTemperature/cwa:monthly/cwa:Maximum", ns), xml_text),
    MinimumAirTemperature = sapply(xml_find_all(locations, ".//cwa:AirTemperature/cwa:monthly/cwa:Minimum", ns), xml_text),
    RainAccumulation = sapply(xml_find_all(locations, ".//cwa:Precipitation/cwa:monthly/cwa:Accumulation", ns), xml_text),
    MaximumWindSpeed = sapply(xml_find_all(locations, ".//cwa:WindSpeed/cwa:monthly/cwa:Maximum", ns), xml_text),
    WindDirection = sapply(xml_find_all(locations, ".//cwa:WindDirection/cwa:monthly/cwa:Maximum", ns), xml_text),
    PeakGustSpeed = sapply(xml_find_all(locations, ".//cwa:PeakGustSpeed/cwa:monthly/cwa:Maximum", ns), xml_text),
    MeanRelativeHumidity = sapply(xml_find_all(locations, ".//cwa:RelativeHumidity/cwa:monthly/cwa:Mean", ns), xml_text),
    MinimumRelativeHumidity = sapply(xml_find_all(locations, ".//cwa:RelativeHumidity/cwa:monthly/cwa:Minimum", ns), xml_text),
    AirPressure = sapply(xml_find_all(locations, ".//cwa:AirPressure/cwa:monthly/cwa:Mean", ns), xml_text),
    SunshineDuration = sapply(xml_find_all(locations, ".//cwa:SunshineDuration/cwa:monthly/cwa:Total", ns), xml_text)
  )
  return(data)
}
# 清洗+整理 xml
all_station_data <- bind_rows(lapply(file_list, extract_location_xml_data))

# 減少負荷，測站資料縮減為前100筆
# all_station_data <- all_station_data[1:100, ]

# ================= 資料前處理 =========================
# Y = 月發生火災
# 
# 需要的測站名稱
stationNameList <- unique(all_station_data$StationName);
print(stationNameList)

# 新增測站海拔
# add_StationAltitude <- function(stationNameList) {
#   
# }
# Altitude <- sapply(stationNameList, add_StationAltitude)
# Station_Altitude <- data.frame(stationNameList, Altitude)


# 核對1.日期 2.站名
# 計算該月火災次數
# 定義一個函數來檢查"林火"的每條記錄是否應該被保留
filter_criteria <- function(縣市, 鄉鎮, 工作站) {
  any(sapply(stationNameList, function(loc) grepl(loc,縣市, fixed = TRUE)) |
      sapply(stationNameList, function(loc) grepl(loc,鄉鎮, fixed = TRUE)) |
      sapply(stationNameList, function(loc) grepl(loc,工作站, fixed = TRUE)))
}

# 測站有紀錄的林火
forest_fire_with_station <- forest_fire_record %>%
  rowwise() %>%
  filter(filter_criteria(縣市,鄉鎮,工作站))

#補缺值
na_counts <- colSums(is.na(forest_fire_with_station))
na_columns <- names(na_counts[na_counts > 0])

forest_fire_with_station$工作站 <- ifelse(is.na(forest_fire_with_station$工作站), 
                                       forest_fire_with_station$鄉鎮, 
                                       forest_fire_with_station$工作站)

forest_fire_with_station$發生時間 <- ifelse(is.na(forest_fire_with_station$發生時間), 
                                     forest_fire_with_station$發現時間, 
                                     forest_fire_with_station$發生時間)

forest_fire_with_station$發生時間 <- as.numeric(format(as.POSIXct(forest_fire_with_station$發生時間, origin = "2015-01-01", tz = "UTC"), "%H"))


#將所有林火資料 依照日期放入測站資料內
all_station_data <- all_station_data %>%
  mutate(
    觀測年度 = as.numeric(substr(YearMonth, 1, 4)),
    觀測月份 = as.numeric(substr(YearMonth, 6, 7))
  )

# 定義單次林火空白記錄
empty_row <- forest_fire_with_station[1,]
empty_row[,] <- 0

# 合併function
merge_function <- function(main_data, second_data) {
  matching_rows <- second_data %>%
    filter(
      年度 == main_data$觀測年度,
      月份 == main_data$觀測月份,
      grepl(main_data$StationName, 縣市) |
      grepl(main_data$StationName, 鄉鎮) |
      grepl(main_data$StationName, 工作站)
    )
  
  if (nrow(matching_rows) > 0) {
    happen_col <- data.frame(matrix(1,nrow(matching_rows),1))
    colnames(happen_col) <- c("happen")
    # combined_data <- cbind(happen_col, cbind(main_data, matching_rows))
    combined_data <- cbind(happen_col, main_data)
  } else {
    # 如果没有找到符合的，回傳空林火資料
    happen_col <- data.frame(matrix(0,1,1))
    colnames(happen_col) <- c("happen")
    # combined_data <- cbind(happen_col, cbind(main_data, empty_row))
    combined_data <- cbind(happen_col, main_data)
  }
  return(combined_data)
}

# 遍歷all_station_data -> 合併符合條件的數據
fire_station_combine <- do.call(rbind, lapply(1:nrow(all_station_data), function(i) {
  main_data <- all_station_data[i, ]
  merge_function(main_data, forest_fire_with_station)
}))



# ====================== 資料後處理 =======================
# 刪除無法解釋 或 重複的col 或 分類比較像預測目標
fire_station_combine <- fire_station_combine[, !(names(fire_station_combine) %in% c("StationID", "StationName", "YearMonth", "縣市", "鄉鎮", "工作站", "年度", "月份", "面積"))]

# 修正測站資料
# 降雨量T 代表 0 < 降雨量 < o.5 以0.25替代
fire_station_combine$RainAccumulation <- ifelse(fire_station_combine$RainAccumulation == "T", 0.25, fire_station_combine$RainAccumulation)
fire_station_combine <- data.frame(lapply(fire_station_combine, as.numeric))
# 處理NA
na_pos <- which(is.na(fire_station_combine), arr.ind = TRUE)
for (i in 1:nrow(na_pos)) {
  row <- na_pos[i, "row"]
  col <- na_pos[i, "col"]
  
  # 計算該列中的非 NA 值的平均數
  col_mean <- mean(fire_station_combine[, col], na.rm = TRUE)
  
  # 用計算出的平均數替換 NA 值
  fire_station_combine[row, col] <- col_mean
}

# 切割訓練集、測試集
sample <- sample(c(TRUE, FALSE), nrow(fire_station_combine), replace=TRUE, prob=c(0.7,0.3))
train_data <- fire_station_combine[sample, ]
test_data <- fire_station_combine[!sample, ]

# 隨機森林
prefix <- setdiff(colnames(train_data), "happen")
set.seed(1)
rf_ml <- randomForest(happen~., data=train_data,
                      ntree = 500,
                      importance = TRUE,
                      proximity = TRUE,
                      do.trace = 50)

# #作圖一張圖=一個畫面
# par(mfrow=c(1,1))
# #畫圖
# plot(rf_res)
# 找重要的自變量
importance(rf_ml)


# 警告訊息: The response has five or fewer unique values.  Are you sure you want to do regression?
# summary(lm)後 確定有過擬合的問題



# 嘗試使用Lasso解決
# Lasso (L2 Regularization)
y <- train_data$happen
x <- model.matrix(happen ~ ., data = train_data)[, -1]
x <- scale(x)

lasso_cv <- cv.glmnet(x, 
                      y, 
                      alpha = 1, 
                      family = "binomial", )
# 獲取最佳 lambda 值
best_lambda <- lasso_cv$lambda.min

# 使用最佳 lambda 值來訓練最終模型
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda, family = "binomial")

# 獲取選擇的特徵
selected_features <- rownames(coef(lasso_model))[which(coef(lasso_model) != 0)]
selected_features <- selected_features[-1]  # 移除Y = happen

formula <- as.formula(paste("happen ~", paste(selected_features, collapse = " + ")))
lm_model <- lm(formula, data = train_data)


# 檢查model
set.seed(1)
summary(lm_model)

# ======================== 預測模型 ======================

mountain_fire_glm <- glm(formula, family="binomial", data=train_data)

summary(mountain_fire_glm)

# Getting probabilities
predict(mountain_fire_glm, newdata = data.frame(MeanAirTemperature = 36, 
                                            MaximumAirTemperature = 50,
                                            MinimumAirTemperature = 30,
                                            RainAccumulation = 0,
                                            MaximumWindSpeed = 20,
                                            WindDirection = 20,
                                            PeakGustSpeed = 10,
                                            MeanRelativeHumidity = 15,
                                            MinimumRelativeHumidity = 10,
                                            AirPressure = 1100,
                                            SunshineDuration = 200,
                                            觀測年度 = 2024,
                                            觀測月份 = 5), type = "response")



# ========================= 模型比較 =======================

MAE <- function(value, prediction) {
  return(mean(abs(value - prediction)))
}
calculate_tpr <- function(pred, actuals, cutoff) {
  pred_classify <- ifelse(pred > cutoff, 1, 0)
  cm <- confusionMatrix(factor(pred_classify, levels = c(0, 1)), actuals)
  sensitivity <- cm$byClass["Sensitivity"]
  return(sensitivity)
}

# 一般線性模型
train_pred <- predict(lm_model, newdata = train_data, type = "response")
test_pred <- predict(lm_model, newdata = test_data, type = "response")

# 一般線性模型 MAE
print(MAE(train_pred, train_data$happen))
print(MAE(test_pred, test_data$happen))

# 一般線性模型 ROC
roc_train <- roc(train_data$happen, train_pred, levels = c(0, 1), auc=TRUE)
roc_test <- roc(test_data$happen, test_pred, levels = c(0, 1), auc=TRUE)

# 找最佳couoff 計算 lm 準確度
lm_youden_index <- roc_test$sensitivities + roc_test$specificities - 1
lm_optimal_cutoff <- roc_test$thresholds[which.max(lm_youden_index)]
print(mean(ifelse(test_pred > lm_optimal_cutoff, 1, 0) == test_data$happen))

# 一般線性模型 AUC
print(roc_train$auc)
print(roc_test$auc)

#==========================================
# 隨機森林
rf_train_pred <- predict(rf_ml, newdata = train_data, type = "response")
rf_test_pred <- predict(rf_ml, newdata = test_data, type = "response")

# 隨機森林 MAE
print(MAE(rf_train_pred, train_data$happen))
print(MAE(rf_test_pred, test_data$happen))

# 隨機森林模型 ROC
rf_roc_train <- roc(train_data$happen, rf_train_pred, levels = c(0, 1), auc=TRUE)
rf_roc_test <- roc(test_data$happen, rf_test_pred, levels = c(0, 1), auc=TRUE)

# 找最佳couoff 計算 rf 準確度
rf_youden_index <- rf_roc_test$sensitivities + rf_roc_test$specificities - 1
rf_optimal_cutoff <- rf_roc_test$thresholds[which.max(rf_youden_index)]
print(mean(ifelse(rf_test_pred > rf_optimal_cutoff, 1, 0) == test_data$happen))

# 隨機森林 AUC
print(rf_roc_train$auc)
print(rf_roc_test$auc)

# =========================================
# 羅吉斯回歸
logistic_train_pred <- predict(mountain_fire_glm, newdata = train_data, type = "response")
logistic_test_pred <- predict(mountain_fire_glm, newdata = test_data, type = "response")

# logistic MAE
print(MAE(logistic_train_pred, train_data$happen))
print(MAE(logistic_test_pred, test_data$happen))

# logistic ROC
logistic_roc_train <- roc(train_data$happen, logistic_train_pred, levels = c(0, 1), auc=TRUE)
logistic_roc_test <- roc(test_data$happen, logistic_test_pred, levels = c(0, 1), auc=TRUE)

# 找最佳couoff 計算 rf 準確度
logistic_youden_index <- logistic_roc_test$sensitivities + logistic_roc_test$specificities - 1
logistic_optimal_cutoff <- logistic_roc_test$thresholds[which.max(logistic_youden_index)]
print(mean(ifelse(logistic_test_pred > logistic_optimal_cutoff, 1, 0) == test_data$happen))

# logistic AUC
print(logistic_roc_train$auc)
print(logistic_roc_test$auc)





