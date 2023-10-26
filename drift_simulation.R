# Loading libraries
packages <- c("purrr", "dplyr", "DALEX", "ggplot2","randomForest","doremi")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) {
  install.packages(new_packages)
}

library(purrr)
library(dplyr)
library(DALEX)
library(ggplot2)
library(randomForest)

# Loading data functions and windowing function
scripts.list <- list.files(
  "data-functions",
  pattern = "*.R$",
  full.names = TRUE,
  recursive = T
)
sapply(scripts.list, source, .GlobalEnv)

# Loading rashomon functions (changed)
scripts.list.rashomon <-
  list.files(
    "rashomon-functions",
    pattern = "*.R$",
    full.names = TRUE,
    recursive = T
  )
sapply(scripts.list.rashomon, source, .GlobalEnv)
RANDOM_SEED <- 123
set.seed(RANDOM_SEED)


coef_values <- list(c(1,-1),NULL)
models <- c("LR","RF")
results_list <- list()

for(model in models){
  for (current_coef in coef_values) {
    set.seed(RANDOM_SEED)
    
    # Creating data objects
    data_sudden <- data_sudden_drift(COEF_VAR_1 = current_coef)
    data_gradual <- data_gradual_drift(COEF_VAR_1 = current_coef)
    data_incremental <- data_incremental_drift(COEF_VAR_1 = current_coef)
    data_recuring <- data_recuring_drift(COEF_VAR_1 = current_coef)
    
    calculate_distances_for_model <- function(data, window_len) {
      data$y <- as.factor(data$y)
      windowing_data <- windowing_f(data, window_len = window_len)
      if(model == "LR"){
        model_ <- glm(y ~ ., data = windowing_data[[1]], family = "binomial")
      }else if(model == "RF"){
        model_ <- randomForest(y ~ ., data = windowing_data[[1]], family = "binomial")
      }

      explainer_list <- map(windowing_data, function(df) {
        explainer_lr <- DALEX::explain(model_, data = df[, -3], y = df$y, verbose = FALSE)
        model_profile(explainer = explainer_lr, variables = c("X.1", "X.2"))
      })
      
      distances <- data.frame()
      for(k in 2:length(explainer_list)){
        if(length(explainer_list[[1]]$agr_profiles$`_vname_`) ==
           length(explainer_list[[k]]$agr_profiles$`_vname_`)){
          distances_k <- calculate_all_distances(list(explainer_list[[1]], explainer_list[[k]]),
                                                 pdi_method_numerical = derivative_fraction_sign_difference,
                                                 profiles_categorical = NULL)
          distances <- rbind(distances, distances_k)
        }
      } 
      
      return(distances)
    }
    
    drift_types <- c("sudden", "gradual", "incremental", "recuring")
    data_names <- paste0("data_", drift_types)
    window_lengths <- c(5, 10, 20)
    
    distance_list <- list()
    
    for(name in data_names) {
      for(wl in window_lengths) {
        list_name <- paste0(name, "_", wl)
        message(paste0("Calculation start - ",list_name))
        
        data_for_calculation <- get(name)
        distances <- calculate_distances_for_model(data_for_calculation, wl)
        
        distance_list[[list_name]] <- distances
      }
    }
    
    
    
    all_data <- list()
    
    for (drift in drift_types) {
      for (win_len in window_lengths) {
        data_name <- paste0("data_", drift, "_", win_len)
        temp_data <- distance_list[[data_name]] |> 
          filter(model_ind_1 == 1) |> 
          select(`X.1`) |> 
          mutate(Drift_Type = factor(drift, levels = drift_types),  
                 Window_Length = as.factor(win_len),
                 Batch_Number = 1:n())
        all_data[[paste0(drift, "_", win_len)]] <- temp_data
      }
    }
    
    all_data <- do.call(rbind, all_data)
    
    
    
    all_data$Drift_Type <- factor(all_data$Drift_Type,
                                  levels = drift_types)
    levels(all_data$Drift_Type) <- c("Sudden",
                                     "Gradual",
                                     "Incremental",
                                     "Recuring")
    
    results_list_plot_name <- ifelse(is.null(current_coef),
                                     "PDI Results ~ Change in Mean of X.1 Distribution",
                                     "PDI Results ~ Change in Coefficent")
    results_list_plot_name <- paste0(results_list_plot_name,
                                     " - ",
                                     ifelse(model == "RF","RF","LR"))
    
    results_list[[results_list_plot_name]] <- 
      ggplot(all_data,aes(x = Batch_Number, y = `X.1`)) +
      geom_line() +
      labs(y = "PDI", x = "Batch Number", title = "") +
      facet_grid(Drift_Type ~ Window_Length, scales = "free") +
      theme_minimal() +
      theme(
        panel.border = element_rect(fill = NA, color = "black"),
        strip.text.y = element_text(face = "bold", size = 10),
        strip.text.x = element_text(face = "bold", size = 10)
      ) +
      scale_x_continuous(labels = scales::comma)
    
  }
  
  
}

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

ggsave("output/concept_drift_coef_lr.png",
       plot= results_list$`PDI Results ~ Change in Coefficent - LR`,
      )
ggsave("output/concept_drift_mean_lr.png",
       plot= results_list$`PDI Results ~ Change in Mean of X.1 Distribution - LR`
      )
ggsave("output/concept_drift_coef_rf.png",
       plot= results_list$`PDI Results ~ Change in Coefficent - RF`,
)
ggsave("output/concept_drift_mean_rf.png",
       plot= results_list$`PDI Results ~ Change in Mean of X.1 Distribution - RF`
)



