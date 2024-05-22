# In this file you can find all of the functions that were used during the course
# of this project on breakpoint analysis.

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

library(tidyverse)
library(readxl)

##################  PART 1: SIMULATED DATA GENERATION ################## 

generate_peak_data <- function(n = 50, 
                               breakpoint_range1 = c(20, 40), 
                               breakpoint_range2 = c(50, 70),
                               relative_max_end_height = 0.1,
                               min_breakpoint_distance = 15, 
                               random_noise_percentage = 10
){
  
  
  
  # Generate three breakpoints within the specified ranges with minimum distance
  breakpoint1 <- runif(1, min(breakpoint_range1), max(breakpoint_range1))
  breakpoint2 <- runif(1, max(breakpoint1 + min_breakpoint_distance, min(breakpoint_range2)), max(breakpoint_range2))
  
  
  # Generate equidistant x-coordinates for n points
  x_coords <- seq(0, 100, length.out = n)
  
  
  # Calculate intercepts depending on number of breakpoints
  
  intercept0 <- 0
  intercept1 <- runif(1, 0, 100)
  intercept2 <- runif(1, 1, intercept1*relative_max_end_height)
  intercept3 <- intercept2
  
  
  # calculate lines
  model1 <- lm(y ~ x, data = data.frame(x = c(0, breakpoint1), y = c(intercept0, intercept1)))
  model2 <- lm(y ~ x, data = data.frame(x = c(breakpoint1, 100), y = c(intercept1, intercept2)))
  model2 <- lm(y ~ x, data = data.frame(x = c(breakpoint1, breakpoint2), y = c(intercept1, intercept2)))
  model3 <- lm(y ~ x, data = data.frame(x = c(breakpoint2, 100), y = c(intercept2, intercept3)))
  
  
  # Generate y-coordinates based on the three lines
  y_coords <- numeric(n)
  
  
  for (i in 1:n) {
    if (x_coords[i] <= breakpoint1) {
      y_coords[i] <- predict(model1, newdata = data.frame(x = x_coords[i]))
    } else if (x_coords[i] <= breakpoint2) {
      y_coords[i] <- predict(model2, newdata = data.frame(x = x_coords[i]))
    } else{
      y_coords[i] <- predict(model3, newdata = data.frame(x = x_coords[i]))
    }
  }
  
  
  
  # Create a dataframe
  df <- data.frame(X = x_coords, Y = y_coords)
  df$Y <- df$Y + max(df$Y) * random_noise_percentage * sapply(df$Y, function(x) runif(1, min = -0.01, max = 0.01))
  df$Y <- pmax(df$Y, 0)
  
  # Add breakpoints and slopes to the dataframe
  df$Breakpoint1 <- breakpoint1
  df$Breakpoint2 <- breakpoint2
  
  
  
  # Extract coefficients from the linear models
  df$Line1_Intercept <- coef(model1)[1]
  df$Line1_Slope <- coef(model1)[2]
  
  df$Line2_Intercept <- coef(model2)[1]
  df$Line2_Slope <- coef(model2)[2]
  
  df$Line3_Intercept <- coef(model3)[1]
  df$Line3_Slope <- coef(model3)[2]
  
  
  
  # Change to Output a list of 2: the first is the dataframe with coordinates, second is a vector with extra info
  
  return(df)
}

# GENERATE PLATEAU DATA

generate_plateau_data <- function(n = 50, 
                                  breakpoints = 3,
                                  breakpoint_range1 = c(10, 40), 
                                  breakpoint_range2 = c(35, 65), 
                                  breakpoint_range3 = c(60, 90),
                                  min_breakpoint_distance = 15, 
                                  random_noise_percentage = 10,
                                  starts_at_0 = T,
                                  middle_plateau = T,
                                  up_plateau_down = T,
                                  ending_plateau = T){
  
  
  
  # Generate three breakpoints within the specified ranges with minimum distance
  breakpoint1 <- runif(1, min(breakpoint_range1), max(breakpoint_range1))
  breakpoint2 <- runif(1, max(breakpoint1 + min_breakpoint_distance, min(breakpoint_range2)), max(breakpoint_range2))
  breakpoint3 <- runif(1, max(breakpoint2 + min_breakpoint_distance, min(breakpoint_range3)), max(breakpoint_range3))
  
  # Generate equidistant x-coordinates for n points
  x_coords <- seq(0, 100, length.out = n)
  
  
  # Calculate intercepts depending on number of breakpoints
  
  if(starts_at_0){
    intercept0 <- 0
  }
  else{
    intercept0 <- runif(1, 0, 100)
  }
  intercept1 <- runif(1, 0, 100)
  intercept2 <- runif(1, 0, 100)
  if(breakpoints < 2){
    break
  }
  else{
    if(middle_plateau){
      intercept2 <- intercept1
    }
    if(up_plateau_down){
      intercept3 <- 0
    }
    else{
      intercept3 <- runif(1, 0, 100)
    }
    if(breakpoints < 3){
      
    }
    else{
      intercept4 <- runif(1, 0, 100)
      if(ending_plateau){
        intercept3 <- runif(1, 0, intercept2/10)
        intercept4 <- intercept3
      }
    }
  }
  
  # calculate lines
  model1 <- lm(y ~ x, data = data.frame(x = c(0, breakpoint1), y = c(intercept0, intercept1)))
  model2 <- lm(y ~ x, data = data.frame(x = c(breakpoint1, 100), y = c(intercept1, intercept2)))
  if(breakpoints > 1){
    model2 <- lm(y ~ x, data = data.frame(x = c(breakpoint1, breakpoint2), y = c(intercept1, intercept2)))
    model3 <- lm(y ~ x, data = data.frame(x = c(breakpoint2, 100), y = c(intercept2, intercept3)))
  }
  if(breakpoints > 2){
    model3 <- lm(y ~ x, data = data.frame(x = c(breakpoint2, breakpoint3), y = c(intercept2, intercept3)))
    model4 <- lm(y ~ x, data = data.frame(x = c(breakpoint3, 100), y = c(intercept3, intercept4)))
  }
  
  # Generate y-coordinates based on the three lines
  y_coords <- numeric(n)
  
  if(breakpoints == 3){
    for (i in 1:n) {
      if (x_coords[i] <= breakpoint1) {
        y_coords[i] <- predict(model1, newdata = data.frame(x = x_coords[i]))
      } else if (x_coords[i] <= breakpoint2) {
        y_coords[i] <- predict(model2, newdata = data.frame(x = x_coords[i]))
      } else if (x_coords[i] <= breakpoint3) {
        y_coords[i] <- predict(model3, newdata = data.frame(x = x_coords[i]))
      } else{
        y_coords[i] <- predict(model4, newdata = data.frame(x = x_coords[i]))
      }
    }
  }
  else{
    for (i in 1:n) {
      if (x_coords[i] <= breakpoint1) {
        y_coords[i] <- predict(model1, newdata = data.frame(x = x_coords[i]))
      } else if (x_coords[i] <= breakpoint2) {
        y_coords[i] <- predict(model2, newdata = data.frame(x = x_coords[i]))
      } else{
        y_coords[i] <- predict(model3, newdata = data.frame(x = x_coords[i]))
      }
    }
  }
  
  
  
  # Create a dataframe
  df <- data.frame(X = x_coords, Y = y_coords)
  df$Y <- df$Y + max(df$Y) * random_noise_percentage * sapply(df$Y, function(x) runif(1, min = -0.01, max = 0.01))
  df$Y <- pmax(df$Y, 0)
  
  # Add breakpoints and slopes to the dataframe
  df$Breakpoint1 <- breakpoint1
  df$Breakpoint2 <- breakpoint2
  if(breakpoints == 3){
    df$Breakpoint3 <- breakpoint3
  }
  
  
  # Extract coefficients from the linear models
  df$Line1_Intercept <- coef(model1)[1]
  df$Line1_Slope <- coef(model1)[2]
  
  df$Line2_Intercept <- coef(model2)[1]
  df$Line2_Slope <- coef(model2)[2]
  
  if (breakpoints > 1) {
    df$Line3_Intercept <- coef(model3)[1]
    df$Line3_Slope <- coef(model3)[2]
  }
  
  if (breakpoints > 2) {
    df$Line4_Intercept <- coef(model4)[1]
    df$Line4_Slope <- coef(model4)[2]
  }
  
  # Change to Output a list of 2: the first is the dataframe with coordinates, second is a vector with extra info
  
  return(df)
}

################## PART 2: BREAKPOINT EVALUATION ################## 

find_breakpoints_peak <- function(data,
                                  iterations = 20,
                                  threshold = 1e-10,
                                  backtrack_steps = 5,
                                  learning_step = 1,
                                  resetting = F,
                                  backtracking = F){
  
  data <- na.omit(data)
  
  min_x <- min(data$X)
  max_x <- max(data$X)
  
  current_psi_1 <- data$X[which.max(data$Y)]
  current_psi_2 <- (current_psi_1 + max_x)/2
  
  
  for(iter in 1:iterations){ # iter = 1
    
    # Printing the current psi
    
    cat(iter,": ",current_psi_1, " and ", current_psi_2, "\n", sep = "")
    
    data$U_s1 <- (data$X - current_psi_1)*(as.integer(data$X > current_psi_1))
    data$V_s1 <- -(as.integer(data$X > current_psi_1))
    
    data$U_s2 <- (data$X - current_psi_2)*(as.integer(data$X > current_psi_2))
    data$V_s2 <- -(as.integer(data$X > current_psi_2))
    
    
    # 2- Linear model with 2 covariates
    
    # lin_model <- lm(Y ~ X + U_s1 + V_s1 + U_s2 + V_s2, data = data)
    # 
    # alpha <- coef(lin_model)["X"]
    # 
    # beta_1 <- coef(lin_model)["U_s1"]
    # gamma_1 <- coef(lin_model)["V_s1"]
    # 
    # beta_2 <- coef(lin_model)["U_s2"]
    # gamma_2 <- coef(lin_model)["V_s2"]
    
    # WITH CONSTRAINTS: alpha + beta2 = -beta1
    
    lin_model <- lm(Y ~ I(-U_s1 - U_s2) + I(-U_s2 - X) + V_s1 + I(-U_s1 - X) + V_s2, data = data)
    
    alpha <- coef(lin_model)["I(-U_s1 - U_s2)"]
    
    beta_1 <- coef(lin_model)["I(-U_s2 - X)"]
    gamma_1 <- coef(lin_model)["V_s1"]
    
    beta_2 <- coef(lin_model)["I(-U_s1 - X)"]
    gamma_2 <- coef(lin_model)["V_s2"]
    
    
    
    
    
    
    
    # browser()
    if(abs(beta_1) < threshold | abs(beta_2) < threshold){
      warning("Beta too small")
    }
    else if(abs(gamma_1) > threshold | abs(gamma_2) > threshold){
      
      new_psi_1 <- (gamma_1 / beta_1) + current_psi_1
      new_psi_2 <- (gamma_2 / beta_2) + current_psi_2
      
      # RESETTING BAD VALUES
      
      if(resetting){
        if (is.na(new_psi_1) || new_psi_1 < min_x || new_psi_1 > max_x) {
          new_psi_1 <- (min_x + new_psi_2) / 2
        }
        
        if (is.na(new_psi_2) || new_psi_2 < min_x || new_psi_2 > max_x) {
          new_psi_2 <- (new_psi_1 + new_psi_3) / 2
        }
      }
      
      
      current_psi_1 <- new_psi_1
      current_psi_2 <- new_psi_2
    }
    
    else{
      break
    }
  }
  
  
  height <- alpha*current_psi_1
  
  return(unname(c(current_psi_1, current_psi_2, NA, alpha, beta_1+alpha, 0, height)))
}

find_breakpoints_plateau <- function(data,
                                     iterations = 20,
                                     start_psi_1 = 0.25,
                                     start_psi_2 = 0.50,
                                     start_psi_3 = 0.75,
                                     threshold = 1e-5,
                                     backtrack_steps = 5,
                                     alt_starts = T){
  
  # Find the starting psi values
  
  data[data == "NA"] <- NA
  data <- na.omit(data) # data <- na.omit(psylo_entries[2])
  min_x <- min(data$X)
  max_x <- max(data$X)
  
  current_psi_1 <- start_psi_1*max_x
  current_psi_2 <- start_psi_2*max_x
  current_psi_3 <- start_psi_3*max_x
  
  if(alt_starts){
    current_psi_1 <- data$X[which.max(data$Y)]/2
    current_psi_2 <- data$X[which.max(data$Y)]
    current_psi_3 <- (data$X[which.max(data$Y)] + max_x)/2
  }
  
  
  
  for(iter in 1:iterations){ # iter = 1
    
    
    
    # Printing the current psi
    
    cat(iter,": ",current_psi_1, " and ", current_psi_2, " and ", current_psi_3,"\n", sep = "")
    
    data$U_s1 <- (data$X - current_psi_1)*(as.integer(data$X > current_psi_1))
    data$V_s1 <- -(as.integer(data$X > current_psi_1))
    data$U_s2 <- (data$X - current_psi_2)*(as.integer(data$X > current_psi_2))
    data$V_s2 <- -(as.integer(data$X > current_psi_2))
    data$U_s3 <- (data$X - current_psi_3)*(as.integer(data$X > current_psi_3))
    data$V_s3 <- -(as.integer(data$X > current_psi_3))
    
    # TEST BEGIN #
    
    
    
    # TEST END #
    
    # 2- Linear model with 2 covariates
    
    lin_model <- lm(Y ~ I(X - U_s1) + I(U_s2 - U_s3) + V_s1 + V_s2 + V_s3, data = data)
    #
    #browser()
    
    # bonus
    # points(data$X, predict(lin_model, newdata = data), col = "red", type = "l")
    
    # Check if gamma is 0, if so then we have reached convergence
    
    alpha <- coef(lin_model)["I(X - U_s1)"]
    
    beta_1 <- -coef(lin_model)["I(X - U_s1)"]
    gamma_1 <- coef(lin_model)["V_s1"]
    
    beta_2 <- coef(lin_model)["I(U_s2 - U_s3)"]
    gamma_2 <- coef(lin_model)["V_s2"] 
    
    beta_3 <- -coef(lin_model)["I(U_s2 - U_s3)"]
    gamma_3 <- coef(lin_model)["V_s3"] 
    
    
    if(is.na(gamma_2)){
      gamma_2 <- gamma_1
    }
    if(is.na(gamma_3)){
      gamma_3 <- gamma_2
    }
    
    # browser()
    if(abs(beta_1) < threshold || abs(beta_2) < threshold || abs(beta_3) < threshold){
      warning("Beta too small")
    }
    else if(abs(gamma_1) > threshold | abs(gamma_2) > threshold | abs(gamma_3) > threshold){
      new_psi_1 <- (gamma_1 / beta_1) + current_psi_1
      new_psi_2 <- (gamma_2 / beta_2) + current_psi_2
      new_psi_3 <- (gamma_3 / beta_3) + current_psi_3
      
      backtracks <- 1
      
      while((is.na(new_psi_1) | new_psi_1 < min_x | new_psi_1 > max_x) & backtracks < backtrack_steps){
        backtracks <- backtracks + 1
        new_psi_1 <- (gamma_1 / beta_1) / (2*backtracks) + current_psi_1
      }
      
      backtracks <- 1
      
      while((is.na(new_psi_2) | new_psi_2 < min_x | new_psi_2 > max_x) & backtracks < backtrack_steps){
        backtracks <- backtracks + 1
        new_psi_2 <- (gamma_2 / beta_2) / (2*backtracks) + current_psi_2
      }
      
      backtracks <- 1
      
      while((is.na(new_psi_3) | new_psi_3 < min_x | new_psi_3 > max_x) & backtracks < backtrack_steps){
        backtracks <- backtracks + 1
        new_psi_3 <- (gamma_3 / beta_3) / (2*backtracks) + current_psi_3
      }
      
      
      
      current_psi_1 <- min(new_psi_1, new_psi_2, new_psi_3)
      current_psi_3 <- max(new_psi_1, new_psi_2, new_psi_3)
      current_psi_2 <- sum(new_psi_1, new_psi_2, new_psi_3) - current_psi_1 - current_psi_3
      
      
      
      if(current_psi_1 < min_x){
        current_psi_1 <- (current_psi_2)/2
        cat("Repositioning current psi 1 to ", current_psi_1, "\n")
      }
      if(current_psi_3 > max_x){
        current_psi_3 <- (current_psi_2 + max_x)/2
        cat("Repositioning current psi 1 to ", current_psi_3, "\n")
      }
      
      # current_psi_1 <- new_psi_1
      # current_psi_2 <- new_psi_2
      # current_psi_3 <- new_psi_3
      
      
    }
    
    else{
      break
    }
  }
  
  # Need it to also return the slope up angle, the plateau duration, and the
  # slope down angle. Maybe use alpha to find the angles?
  
  height <- alpha*current_psi_1
  # downslope worked at -beta_3
  return(unname(c(current_psi_1, current_psi_2, current_psi_3, alpha, -beta_3, current_psi_2-current_psi_1, height)))
  # Should there be a fourth breakpoint at the point in which we have the initial increase?
}


################## PART 3: PLOTTING FUNCTIONS #####################

plateau_with_lines_and_breakpoints <- function(df, breakpoints = TRUE, lines = TRUE) {
  # Plot the data points in black
  plot(df$X, df$Y, type = "p", col = "black", xlab = "X", ylab = "Y", main = "Simulated Plateau Data with Breakpoints and Lines")
  
  if (breakpoints) {
    # Add vertical lines for the breakpoints in red
    abline(v = c(df$Breakpoint1, df$Breakpoint2, df$Breakpoint3), col = "red")
  }
  
  if (lines) {
    
    # Add segments (lines) within specific x-axis intervals (dotted lines)
    segments(x0 = 0, x1 = df$Breakpoint1[1], y0 = df$Line1_Intercept + df$Line1_Slope * 0, y1 = df$Line1_Intercept[1] + df$Line1_Slope[1] * df$Breakpoint1[1], col = "lightblue", lwd = 2, lty = 1)
    segments(x0 = df$Breakpoint1[1], x1 = df$Breakpoint2[1], y0 = df$Line2_Intercept + df$Line2_Slope * df$Breakpoint1[1], y1 = df$Line2_Intercept + df$Line2_Slope * df$Breakpoint2[1], col = "lightblue", lwd = 2, lty = 1)
    segments(x0 = df$Breakpoint2[1], x1 = df$Breakpoint3[1], y0 = df$Line3_Intercept + df$Line3_Slope * df$Breakpoint2[1], y1 = df$Line3_Intercept + df$Line3_Slope * df$Breakpoint3[1], col = "lightblue", lwd = 2, lty = 1)
    segments(x0 = df$Breakpoint3[1], x1 = 100, y0 = df$Line4_Intercept + df$Line4_Slope * df$Breakpoint3[1], y1 = df$Line4_Intercept + df$Line4_Slope * 100, col = "lightblue", lwd = 2, lty = 1)
  }
  
}

peak_with_lines_and_breakpoints <- function(df, breakpoints = TRUE, lines = TRUE) {
  # Plot the data points in black
  plot(df$X, df$Y, type = "p", col = "black", xlab = "X", ylab = "Y", main = "Data Points with Breakpoints and Lines")
  
  if (breakpoints) {
    # Add vertical lines for the breakpoints in red
    abline(v = c(df$Breakpoint1, df$Breakpoint2), col = "red")
  }
  
  if (lines) {
    
    # Add segments (lines) within specific x-axis intervals (dotted lines)
    segments(x0 = 0, x1 = df$Breakpoint1[1], y0 = df$Line1_Intercept + df$Line1_Slope * 0, y1 = df$Line1_Intercept[1] + df$Line1_Slope[1] * df$Breakpoint1[1], col = "lightblue", lwd = 2, lty = 1)
    segments(x0 = df$Breakpoint1[1], x1 = df$Breakpoint2[1], y0 = df$Line2_Intercept + df$Line2_Slope * df$Breakpoint1[1], y1 = df$Line2_Intercept + df$Line2_Slope * df$Breakpoint2[1], col = "lightblue", lwd = 2, lty = 1)
    segments(x0 = df$Breakpoint2[1], x1 = 100, y0 = df$Line3_Intercept[1] + df$Line3_Slope[1] * df$Breakpoint2[1], y1 = df$Line3_Intercept[1] + df$Line3_Slope[1] * 100, col = "lightblue", lwd = 2, lty = 1)
  }
  
}

################ PART 4: ALGORITHM EVALUATION ON SIMULATION ################

save_100_results <- function(plateau = T, data_points = 15, BP_1 = 25, BP_2 = 50, BP_3 = 75){
  # Initialize an empty data frame with appropriate column names
  result_df <- data.frame(psi_1 = numeric(), 
                          psi_2 = numeric(), 
                          psi_3 = numeric(), 
                          slope_up = numeric(), 
                          slope_down = numeric(),
                          plateau_duration = numeric(),
                          plateau_height = numeric())
  if(plateau){
    for (x in 1:100) {
      tryCatch({
        cat("Current seed: ", x,"\n", sep = "")
        set.seed(x)
        data <- generate_plateau_data(n = data_points, 
                                      breakpoint_range1 = c(BP_1, BP_1),
                                      breakpoint_range2 = c(BP_2, BP_2),
                                      breakpoint_range3 = c(BP_3, BP_3))
        
        results <- find_breakpoints_plateau(data)
        
        # Transpose the results and add them as a new row to the data frame
        result_df <- rbind(result_df, t(results))
      }, error = function(e){
        cat("Run", x, "failed with error:", conditionMessage(e), "\n")
        result_df[x, ] <- rep(NA, ncol(result_df))
      })
    }
  } else {
    for (x in 1:100) {
      tryCatch({
        cat("Current seed: ", x,"\n", sep = "")
        set.seed(x)
        data <- generate_peak_data(n = data_points,
                                   breakpoint_range1 = c(BP_1, BP_1),
                                   breakpoint_range2 = c(BP_2, BP_2))
        
        results <- find_breakpoints_peak(data)
        
        # Transpose the results and add them as a new row to the data frame
        result_df <- rbind(result_df, t(results))
      }, error = function(e){
        cat("Run", x, "failed with error:", conditionMessage(e), "\n")
        result_df[x, ] <- rep(NA, ncol(result_df))
      })
    }
  }
  
  
  
  # Reset row names for better readability
  row.names(result_df) <- NULL
  names(result_df) <- c("BP_1", "BP_2", "BP_3", "Slope_Up", "Slope_Down", "Plateau_duration", "Plateau_Height")
  
  return(result_df)
}

boxplot_breakpoints <- function(result_df, plateau = TRUE) {
  n_rows <- nrow(result_df)
  percentage <- n_rows / 100
  
  if (plateau) {
    bp_data <- list(result_df$BP_1, result_df$BP_2, result_df$BP_3)
    names <- c("BP_1", "BP_2", "BP_3")
  } else {
    bp_data <- list(result_df$BP_1, result_df$BP_2)
    names <- c("BP_1", "BP_2")
  }
  
  bp <- boxplot(bp_data, names = names, col = c("red", "green", "blue"),
                main = "Breakpoint Positions",
                ylab = "Breakpoints",
                horizontal = TRUE,
                ylim = c(0, 100))
  
  # Add real breakpoint positions
  if (plateau) {
    abline(v = c(25, 50, 75), col = "black", lty = 2)
  } else {
    abline(v = c(50, 75), col = "black", lty = 2)
  }
  
  # Add percentage annotation
  text(x = 90, y = 1, labels = paste("No error:", round(percentage, 2) * 100, "%"))
  
  # Add legend
  legend("topleft", legend = "Real Breakpoint Positions", col = "black", lty = 2)
  
  # Label X coordinate
  mtext("Breakpoint Position", side = 1, line = 2, at = 50, cex = 1.2)
  
  # Calculate and print IQR for each group
  for (i in seq_along(bp_data)) {
    bp_stats <- boxplot.stats(bp_data[[i]])
    cat("IQR for", names[i], ":", bp_stats$stats[4] - bp_stats$stats[2], "\n")
  }
}

############# PART 5: ALGORITHM EVALUATION ON REAL DATA ##############

plot_real_data <- function(data, character){
  
  # formats and plots real data
  
  data$character <- character
  
  data_long <- data %>%
    gather(key = "Time", value = "Experience", -PatientID, -character)
  
  
  
  # Remove rows with NA values
  data_long <- na.omit(data_long)
  
  data_long$Time <- as.numeric(as.character(data_long$Time))
  
  
  
  # Plotting using ggplot2 with facet_grid
  plot <- ggplot(data_long, aes(x = Time, y = Experience, color = as.factor(character))) +
    geom_line() +
    geom_point() +
    facet_wrap(~ PatientID, scales = "free_y", ncol = 4) +
    labs(x = "Time", y = "Experience", title = "data") +
    theme_minimal()
  
  print(plot)
  
  return(data_long)
}

estimate_bp_real_data <- function(data, character){
  
  # format
  
  data$character <- character
  
  data_long <- data %>%
    gather(key = "Time", value = "Experience", -PatientID, -character)
  
  
  
  # Remove rows with NA values
  data_long <- na.omit(data_long)
  
  data_long$Time <- as.numeric(as.character(data_long$Time))
  
  # This retrieves the required format of data for the breakpoint function
  
  selected_columns <- data[, 2:20]
  
  data_entries <- lapply(1:nrow(selected_columns), function(i) {
    data.frame(Y = t(selected_columns[i, ]), X = as.integer(names(selected_columns)))
  })
  
  result_df <- data.frame(do.call(rbind, lapply(1:length(data_entries), function(i) {
    print(i)
    if(data$character[i]==0){
      find_breakpoints_plateau(data_entries[[i]])
    }
    else if(data$character[i]==1){
      find_breakpoints_peak(data_entries[[i]])
    }
    else{
      return(c(NA, NA, NA, NA, NA, NA, NA))
    }
  })))
  
  names(result_df) <- c("BP_1", "BP_2", "BP_3", "slope_up", "slope_down", "plateau_duration", "plateau_height")
  
  labelled_results <- data.frame(data[1], result_df)
  
  merged_data <- data_long %>%
    left_join(labelled_results, by = "PatientID")
  
  # Remove rows with NA values
  return(merged_data)
}

full_plot_breakpoint <- function(data, title = "Fit according to estimated breakpoints"){
  # Calculate average difference for each graph
  average_difference <- data %>%
    group_by(PatientID, character) %>%
    summarize(average_diff = mean((Experience - ifelse(character == 0,
                                                       ifelse(Time < BP_1, slope_up * Time, ifelse(Time < BP_2, plateau_height, ifelse(Time < BP_3, slope_up * BP_1 + slope_down * (Time - BP_2), slope_down * (BP_3 - BP_2) + slope_up * BP_1))),
                                                       ifelse(Time < BP_1, slope_up * Time, ifelse(Time < BP_2, slope_up * BP_1 + slope_down * (Time - BP_1), slope_up * BP_1 + slope_down * (BP_2 - BP_1)))))^2))
  
  # Plot with score annotation
  gg <- ggplot(data, aes(x = Time, y = Experience, color = as.factor(character))) +
    geom_line() +
    geom_point() +
    geom_segment(data = data %>% filter(character == 0),
                 aes(x = 0, y = 0, xend = BP_1, yend = slope_up * BP_1),
                 color = "red", linetype = "dashed") +
    geom_segment(data = data %>% filter(character == 0),
                 aes(x = BP_1, y = slope_up * BP_1, xend = BP_2, yend = slope_up * BP_1),
                 color = "red", linetype = "dashed") +
    geom_segment(data = data %>% filter(character == 0),
                 aes(x = BP_2, y = slope_up * BP_1, xend = BP_3, yend = slope_down * (BP_3 - BP_2) + slope_up * BP_1),
                 color = "red", linetype = "dashed") +
    geom_segment(data = data %>% filter(character == 0),
                 aes(x = BP_3, y = slope_down * (BP_3 - BP_2) + slope_up * BP_1, xend = 450, yend = slope_down * (BP_3 - BP_2) + slope_up * BP_1),
                 color = "red", linetype = "dashed") +
    geom_segment(data = data %>% filter(character == 1),
                 aes(x = 0, y = 0, xend = BP_1, yend = slope_up * BP_1),
                 color = "darkgreen", linetype = "dashed") +
    geom_segment(data = data %>% filter(character == 1),
                 aes(x = BP_1, y = slope_up * BP_1, xend = BP_2, yend = slope_up * BP_1 + slope_down * (BP_2 - BP_1)),
                 color = "darkgreen", linetype = "dashed") +
    geom_segment(data = data %>% filter(character == 1),
                 aes(x = BP_2, y = slope_up * BP_1 + slope_down * (BP_2 - BP_1), xend = 450, yend = slope_up * BP_1 + slope_down * (BP_2 - BP_1)),
                 color = "darkgreen", linetype = "dashed") +
    scale_color_manual(values = c("red", "darkgreen", "blue"),
                       breaks = c(0, 1, 2),
                       labels = c("Plateau", "Peak", "Undefined")) +
    geom_vline(aes(xintercept = BP_1), color = "black", linetype = "dashed") +
    geom_vline(aes(xintercept = BP_2), color = "black", linetype = "dashed") +
    geom_vline(data = data %>% filter(character == 0),
               aes(xintercept = BP_3), color = "black", linetype = "dashed") +
    facet_wrap(~ paste0("Patient ", substr(PatientID, nchar(PatientID) - 2, nchar(PatientID))), scales = "free_y", ncol = 4) +
    labs(x = "Time", y = "Experience", title = "SPS") +
    labs(color = "Data Type") +  # Set legend title
    theme_minimal()
  
  
  # Annotate the average difference score on each graph
  gg +
    geom_text(data = average_difference,
              aes(label = paste("MSE:", round(average_diff, 2))),
              x = Inf, y = Inf, hjust = 1, vjust = 1, size = 3, color = "black")
}

simplified_plot_breakpoint <- function(data, title = "Fit according to estimated breakpoints"){
  ggplot(data, aes(x = Time, y = Experience, color = as.factor(character))) +
    geom_point() +
    geom_segment(data = data %>% filter(character == 0),
                 aes(x = 0, y = 0, xend = BP_1, yend = slope_up * BP_1),
                 color = "red") +
    geom_segment(data = data %>% filter(character == 0),
                 aes(x = BP_1, y = slope_up * BP_1, xend = BP_2, yend = slope_up * BP_1),
                 color = "red") +
    geom_segment(data = data %>% filter(character == 0),
                 aes(x = BP_2, y = slope_up * BP_1, xend = BP_3, yend = slope_down * (BP_3 - BP_2) + slope_up * BP_1),
                 color = "red") +
    geom_segment(data = data %>% filter(character == 0),
                 aes(x = BP_3, y = slope_down * (BP_3 - BP_2) + slope_up * BP_1, xend = 450, yend = slope_down * (BP_3 - BP_2) + slope_up * BP_1),
                 color = "red") +
    geom_segment(data = data %>% filter(character == 1),
                 aes(x = 0, y = 0, xend = BP_1, yend = slope_up * BP_1),
                 color = "darkgreen") +
    geom_segment(data = data %>% filter(character == 1),
                 aes(x = BP_1, y = slope_up * BP_1, xend = BP_2, yend = slope_up * BP_1 + slope_down * (BP_2 - BP_1)),
                 color = "darkgreen") +
    geom_segment(data = data %>% filter(character == 1),
                 aes(x = BP_2, y = slope_up * BP_1 + slope_down * (BP_2 - BP_1), xend = 450, yend = slope_up * BP_1 + slope_down * (BP_2 - BP_1)),
                 color = "darkgreen") +
    scale_color_manual(values = c("red", "darkgreen", "blue"),
                       breaks = c(0, 1, 2),
                       labels = c("Plateau", "Peak", "Undefined")) +
    facet_wrap(~ paste0("Patient ", substr(PatientID, nchar(PatientID)-2, nchar(PatientID))), scales = "free_y", ncol = 4) +
    labs(x = "Time", y = "Experience", title = title) +
    labs(color = "Data Type") +  # Set legend title
    theme_minimal() +
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
}
