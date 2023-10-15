# Library 
library(dplyr)
library(ggplot2)
library(FSA)
library(car)
library(knitr)
library(kableExtra)
library(ggpubr)
library(emmeans)
library(pROC)
library(caret)
library(ROCR)
library(grid)

# Load data #### 
load("ProcessedData/AUROC_merged_Estim.RDA")

data = merged_counties %>%
  ungroup()%>%
  mutate(
    expected_higher_rt = if_else(risk_level > target_risk, 1, 0),
    actual_higher_rt = if_else(Rt > target_rt, 1, 0),
    actual_higher_rt_NextWeek = if_else(Rt_NextWeek > target_rt_NextWeek, 1, 0),
    actual_higher_rt_1prevWeek = if_else(Rt_1prevWeek > target_rt_1prevWeek, 1, 0),
    actual_higher_rt_2prevWeek = if_else(Rt_2prevWeek > target_rt_2prevWeek, 1, 0),
    actual_higher_rt_3prevWeek = if_else(Rt_1prevWeek > target_rt_3prevWeek, 1, 0)
  ) 

 
plot = function(data, var, tRoc, tRocF, tCm, tCmF){
  data = data %>%
    dplyr::rename("targetVar" = var)%>%
    na.omit(targetVar)
  
  county1 = filter(data, UR_code == 1 & target_UR == 1)
  county2 = filter(data, UR_code == 2 & target_UR == 2)
  county3 = filter(data, UR_code == 3 & target_UR == 3)
  county4 = filter(data, UR_code == 4 & target_UR == 4)
  county5 = filter(data, UR_code == 5 & target_UR == 5)
  county6 = filter(data, UR_code == 6 & target_UR == 6)
  
  print("step02-1")
  # Create the prediction object
  pred_obj <- prediction(data$expected_higher_rt,
                         data$targetVar)
  print("step02-2")
  # Create the performance object
  perf_obj <- performance(pred_obj, measure = "tpr", x.measure = "fpr")
  
  print("step02-3")
  # Calculate the AUROC
  auroc <- performance(pred_obj, "auc")@y.values[[1]]
  
  print("step02-4")
  # Create the ROC curve data frame
  roc_data <- data.frame(fpr = unlist(perf_obj@x.values),
                         tpr = unlist(perf_obj@y.values))
  
  
  print("step3")
  # Create the ROC curve plot with AUROC
  gRoc <- ggplot(data = roc_data, aes(x = fpr, y = tpr)) +
    geom_path(color = "#377eb8", size = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
    labs(
      x = "False Positive Rate", y = "True Positive Rate",
      title = tRoc,
      subtitle = paste("AUROC:", round(auroc, 2))) +
    theme_bw() +
    annotate("text", x = 0.8,
             y = 0.2,
             label = paste("AUROC =", round(auroc, 2)), size = 4)
  
  # Print the ROC curve plot
  ggsave(paste0("Figures/gRoc_", var, ".jpg"),
         gRoc, 
         height=6,
         width=8,
         scale=1)
  
  print("step4")
  # AUC facet ####
  # Create the prediction object 
  pred_obj1 <- prediction(county1$expected_higher_rt,
                          county1$targetVar)
  
  pred_obj2 <- prediction(county2$expected_higher_rt,
                          county2$targetVar)
  
  pred_obj3 <- prediction(county3$expected_higher_rt,
                          county3$targetVar)
  
  pred_obj4 <- prediction(county4$expected_higher_rt,
                          county4$targetVar)
  
  pred_obj5 <- prediction(county5$expected_higher_rt,
                          county5$targetVar)
  
  pred_obj6 <- prediction(county6$expected_higher_rt,
                          county6$targetVar)
  
  
  
  # Create the performance object
  perf_obj1 <- performance(pred_obj1, "tpr", "fpr")
  perf_obj2 <- performance(pred_obj2, "tpr", "fpr")
  perf_obj3 <- performance(pred_obj3, "tpr", "fpr")
  perf_obj4 <- performance(pred_obj4, "tpr", "fpr")
  perf_obj5 <- performance(pred_obj5, "tpr", "fpr")
  perf_obj6 <- performance(pred_obj6, "tpr", "fpr")
  
  
  # Calculate the AUROC
  auroc1 <- performance(pred_obj1, "auc")@y.values[[1]]
  auroc2 <- performance(pred_obj2, "auc")@y.values[[1]]
  auroc3 <- performance(pred_obj3, "auc")@y.values[[1]]
  auroc4 <- performance(pred_obj4, "auc")@y.values[[1]]
  auroc5 <- performance(pred_obj5, "auc")@y.values[[1]]
  auroc6 <- performance(pred_obj6, "auc")@y.values[[1]]
  
  
  
  # Create the ROC curve data frame
  roc_data1 <- data.frame(fpr = unlist(perf_obj1@x.values),
                          tpr = unlist(perf_obj1@y.values))%>%
    mutate(
      UR_Category = "Large central metro",
      auroc = auroc1
    )
  
  roc_data2 <- data.frame(fpr = unlist(perf_obj2@x.values),
                          tpr = unlist(perf_obj2@y.values))%>%
    mutate(
      UR_Category = "Large fringe metro",
      auroc = auroc2
    )
  
  roc_data3 <- data.frame(fpr = unlist(perf_obj3@x.values),
                          tpr = unlist(perf_obj3@y.values))%>%
    mutate(
      UR_Category = "Medium metro",
      auroc = auroc3
    )
  
  roc_data4 <- data.frame(fpr = unlist(perf_obj4@x.values),
                          tpr = unlist(perf_obj4@y.values))%>%
    mutate(
      UR_Category = "Small metro",
      auroc = auroc4
    )
  
  roc_data5 <- data.frame(fpr = unlist(perf_obj5@x.values),
                          tpr = unlist(perf_obj5@y.values))%>%
    mutate(
      UR_Category = "Micropolitan",
      auroc = auroc5
    )
  
  roc_data6 <- data.frame(fpr = unlist(perf_obj6@x.values),
                          tpr = unlist(perf_obj6@y.values))%>%
    mutate(
      UR_Category = "Noncore",
      auroc = auroc6
    )
  roc_data = rbind(
    roc_data1,
    roc_data2,
    roc_data3,
    roc_data4,
    roc_data5,
    roc_data6
  )
  
  # Create the ROC curve plot with AUROC Facet wrap
  gRocF <- ggplot(data = roc_data, aes(x = fpr, y = tpr)) +
    geom_path(color = "#377eb8", size = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
    labs(
      x = "False Positive Rate", y = "True Positive Rate",
      title = tRocF) +
    theme_bw() +
    facet_wrap(. ~ UR_Category,
               ncol=2)+
    geom_text(aes(x = 0.8, y = 0.2, label = paste("AUROC =", round(auroc, 2))),
              size = 4, show.legend = FALSE, data = subset(roc_data, !is.na(auroc)))
  
  
  # Print the ROC curve plot
  ggsave(paste0("Figures/gRocF_", var, ".jpg"),
         gRocF, 
         height=6,
         width=4,
         scale=1.8)
  
  
  # Confusion Matrix ####
  
  names(data)
  cm = confusionMatrix(factor(data$expected_higher_rt),
                       factor(data$targetVar))
  
  cm
  
  # Create the confusion matrix
  confusion_matrix <- cm$table
  
  # Convert the confusion matrix to a data frame
  confusion_df <- as.data.frame(confusion_matrix)
  
  
  # Calculate percentage for labels
  confusion_df$Percent <- sprintf("%.1f%%", confusion_df$Freq / sum(confusion_df$Freq) * 100)
  
  
  # Plot the confusion matrix using ggplot2
  gCM = ggplot(confusion_df,
               aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile() +
    #geom_text(aes(label = Count), color = "black") +
    geom_text(aes(label = Percent), color = "black", size = 3) +
    scale_fill_gradient(low = "#fff7ec", high = "#ef6548") +
    scale_x_discrete(limits = rev(levels(confusion_df$Reference)),
                     position = "top") +  # Reverse x-axis
    
    labs(title = tCm,
         x = "Actual",
         y = "Predicted") +
    #theme_minimal()+
    theme_classic()+
    guides(fill = FALSE)
  
  ggsave(paste0("Figures/gCM_",var,".jpg"),
         gCM, 
         height=6,
         width=8,
         scale=1)
  
  
  names(county1)
  
  
  # Confusion Matrix facet ####
  cm1 = confusionMatrix(factor(county1$expected_higher_rt),
                        factor(county1$targetVar))
  
  cm2 = confusionMatrix(factor(county2$expected_higher_rt),
                        factor(county2$targetVar))
  
  cm3 = confusionMatrix(factor(county3$expected_higher_rt),
                        factor(county3$targetVar))
  
  cm4 = confusionMatrix(factor(county4$expected_higher_rt),
                        factor(county4$targetVar))
  
  cm5 = confusionMatrix(factor(county5$expected_higher_rt),
                        factor(county5$targetVar))
  
  cm6 = confusionMatrix(factor(county6$expected_higher_rt),
                        factor(county6$targetVar))
  
  
  
  # Create the confusion matrix
  confusion_df1 <- as.data.frame(cm1$table) %>%
    mutate(
      UR_Category = "Large central metro",
      Percent = sprintf("%.1f%%", Freq / sum(Freq) * 100),
      per_num = Freq / sum(Freq) * 100
    )
  
  confusion_df2 <- as.data.frame(cm2$table) %>%
    mutate(
      UR_Category = "Large fringe metro",
      Percent = sprintf("%.1f%%", Freq / sum(Freq) * 100),
      per_num = Freq / sum(Freq) * 100
    )
  
  confusion_df3 <- as.data.frame(cm3$table) %>%
    mutate(
      UR_Category = "Medium metro",
      Percent = sprintf("%.1f%%", Freq / sum(Freq) * 100),
      per_num = Freq / sum(Freq) * 100
    )
  
  confusion_df4 <- as.data.frame(cm4$table) %>%
    mutate(
      UR_Category = "Small metro",
      Percent = sprintf("%.1f%%", Freq / sum(Freq) * 100),
      per_num = Freq / sum(Freq) * 100
    )
  
  confusion_df5 <- as.data.frame(cm5$table) %>%
    mutate(
      UR_Category = "Micropolitan",
      Percent = sprintf("%.1f%%", Freq / sum(Freq) * 100),
      per_num = Freq / sum(Freq) * 100
    )
  
  confusion_df6 <- as.data.frame(cm6$table) %>%
    mutate(
      UR_Category = "Noncore",
      Percent = sprintf("%.1f%%", Freq / sum(Freq) * 100),
      per_num = Freq / sum(Freq) * 100
    )
  
  
  confusion_df = rbind(confusion_df1,
                       confusion_df2,
                       confusion_df3,
                       confusion_df4,
                       confusion_df5,
                       confusion_df6)
  
  
  
  # Plot the confusion matrix using ggplot2
  gCMF = ggplot(confusion_df,
                aes(x = Reference, y = Prediction, fill = per_num)) +
    geom_tile() +
    #geom_text(aes(label = Count), color = "black") +
    geom_text(aes(label = Percent), color = "black", size = 3) +
    scale_fill_gradient(low = "#fff7ec",
                        high = "#ef6548") +
    scale_x_discrete(limits = rev(levels(confusion_df$Reference)),
                     position = "top") +  # Reverse x-axis
    
    labs(
      title = tCmF ,
      x = "Actual",
      y = "Predicted") +
    #theme_minimal()+
    theme_classic()+
    guides(fill = FALSE)+
    facet_wrap(. ~ UR_Category,
               ncol=2)
  #labs(caption = "Figure 2: Confusion Matrix showing the three weeks later predictive performance of the CDC risk level 
  #    \nas indicator of Rt values in each NCHS Urban-Rural Classification.") +
  #theme(plot.caption = element_text(hjust = 0))
  
  
  ggsave(paste0("Figures/gCMF_",var,".jpg"),
         gCMF, 
         height=6,
         width=4,
         scale=1.8)
  
}

plot(data=data,
     var = "actual_higher_rt",
     tRoc = "ROC Curve for Evaluating CDC Transmission Risk Level (Same week)",
     tRocF = "ROC Curve for Evaluating CDC Transmission Risk Level (Same week)",
     tCm = "Confusion Matrix for all counties (Same week)",
     tCmF = "Confusion Matrix showing the prediction accuracy of Rt in \ndifferent UR categories (Same week)")


plot(data=data,
     var = "actual_higher_rt_NextWeek",
     tRoc = "ROC Curve for Evaluating CDC Transmission Risk Level (next week)",
     tRocF = "ROC Curve for Evaluating CDC Transmission Risk Level (next week)",
     tCm = "Confusion Matrix for all counties (next week)",
     tCmF = "Confusion Matrix showing the prediction accuracy of Rt in \ndifferent UR categories (next week)")


plot(data=data,
     var = "actual_higher_rt_1prevWeek",
     tRoc = "ROC Curve for Evaluating CDC Transmission Risk Level (one week before)",
     tRocF = "ROC Curve for Evaluating CDC Transmission Risk Level (one week before)",
     tCm = "Confusion Matrix for all counties (one week before)",
     tCmF = "Confusion Matrix showing the prediction accuracy of Rt in \ndifferent UR categories (one week before)")

plot(data=data,
     var = "actual_higher_rt_2prevWeek",
     tRoc = "ROC Curve for Evaluating CDC Transmission Risk Level (2 week before)",
     tRocF = "ROC Curve for Evaluating CDC Transmission Risk Level (2 week before)",
     tCm = "Confusion Matrix for all counties (2 week before)",
     tCmF = "Confusion Matrix showing the prediction accuracy of Rt in \ndifferent UR categories (2 week before)")

plot(data=data,
     var = "actual_higher_rt_3prevWeek",
     tRoc = "ROC Curve for Evaluating CDC Transmission Risk Level (3 week before)",
     tRocF = "ROC Curve for Evaluating CDC Transmission Risk Level (3 week before)",
     tCm = "Confusion Matrix for all counties (3 week before)",
     tCmF = "Confusion Matrix showing the prediction accuracy of Rt in \ndifferent UR categories (3 week before)")






