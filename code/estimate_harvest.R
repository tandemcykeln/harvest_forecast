# This function tries to estimate the harvest
predict_harvest <- function(crop, po, year, modell, startyear) {
  # Make a tibble of the year we want to estimate
  est <- tribble(~ PO, ~ Year, ~ Crop,
                 po, year, crop)
  
  # Make a tibble with the data that we will use
  crop_df <- pd_harvest4 |>
    filter(!Year == year) |> 
    left_join(station_ids, by = "PO") |>
    select(PO, Year, Crop, Harvest_ha) |>
    arrange(PO, Year) |>
    filter(Crop == crop & PO == po & Year>=startyear) |>
    bind_rows(est) |>
    left_join(weather_week_wide2, by = c("Year", "PO")) |>
    select(-c(PO, Crop)) |>
    select(Harvest_ha, everything())
  
  # Create data for training the model
  train_data <- crop_df |>
    filter(!Year == year)
  
  # Prepare X and y for glmnet (needs to be matrix format)
  # The first column of train_data/test_data is 'y', so we select from second column onwards for X
  train_X <- as.matrix(train_data[, -1])
  train_y <- as.matrix(train_data$Harvest_ha)
  
  # Model Training (Lasso)
  # Use cv.glmnet for cross-validation to find the optimal lambda
  # alpha = 1 for Lasso regression
  # standardize = TRUE (default for alpha > 0) means predictors are standardized before fitting
  # family = "gaussian" for continuous response variable (regression)
  
  set.seed(19760501) # for reproducible cross-validation folds
  lasso_model <- cv.glmnet(
    x = train_X,
    y = train_y,
    alpha = modell,
    # Set alpha to 1 for Lasso
    family = "gaussian",
    # For regression
    type.measure = "mse",
    # Use mean squared error for cross-validation
    nfold = 10
  )
  
  #warnings()
  
  
  # Plot the cross-validation curve
  plot(lasso_model)
  # The plot shows MSE vs. log(lambda).
  # The left vertical dashed line indicates lambda.min (lambda that gives minimum MSE).
  # The right vertical dashed line indicates lambda.1se (largest lambda within 1 standard error of the minimum).
  # lambda.1se often results in a simpler model with comparable performance.
  
  # Get the optimal lambda values
  lambda_min <- lasso_model$lambda.min
  lambda_1se <- lasso_model$lambda.1se
  
  cat("\n === PO:", po, "-- Year:", year, "-- Crop:", crop, "=== \n")
  cat("\nOptimal lambda (min MSE):", lambda_min, "\n")
  cat("Optimal lambda (1-SE rule):", lambda_1se, "\n")
  
  # Coefficients at lambda.min
  coef_min <- coef(lasso_model, s = lambda_min)
  cat("\nCoefficients at lambda.min:\n")
  print(coef_min[coef_min[, 1] != 0, ]) # Print only non-zero coefficients
  
  # Coefficients at lambda.1se
  coef_1se <- coef(lasso_model, s = lambda_1se)
  cat("\nCoefficients at lambda.1se:\n")
  print(coef_1se[coef_1se[, 1] != 0, ]) # Print only non-zero coefficients
  
  # Lets see what the harvest in the year we have provided will be
  # Combine the two data frames
  harvest <- crop_df  |>
    filter(Year == year)  |>
    select(-Harvest_ha)
  
  # Lets make the predictions
  harvest2 <- predict(lasso_model, s = 'lambda.min', newx = harvest)
  harvest2 <- as.data.frame(harvest2)
  harvest2$Crop <- crop
  harvest2$Year <- year
  harvest2$PO <- po
  
  cat("\n")
  print(harvest2)
  return(harvest2)
  
}


