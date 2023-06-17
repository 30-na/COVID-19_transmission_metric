# Library # boxplot Anova   #### 
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
load("ProcessedData/county.NCHS.RDA")
load("ProcessedData/AUROC_merged.RDA")


names(merged_counties)

print("nrow sample: ")
nrow(compared_counties)
print("    ----------------        ")
# AUC ####
county_list = county.NCHS %>%
  select(state,
         fips_code,
         UR_code,
         UR_category) %>%
  distinct() %>%
  mutate(
    fips_state = paste(state, fips_code, sep=",")
  ) %>% 
  select(fips_state,
         UR_code,
         UR_category)

print("step00-1")
UR1 = filter(county_list, UR_code == 1)$fips_state
UR2 = filter(county_list, UR_code == 2)$fips_state
UR3 = filter(county_list, UR_code == 3)$fips_state
UR4 = filter(county_list, UR_code == 4)$fips_state
UR5 = filter(county_list, UR_code == 5)$fips_state
UR6 = filter(county_list, UR_code == 6)$fips_state


print("step1")

merged_counties = merged_counties %>%
  mutate(
    truePositive = ifelse(expected_higher_rt == 1 & actual_higher_rt == 1, 1, 0),
    falsePositive = ifelse(expected_higher_rt == 1 & actual_higher_rt == 0, 1, 0),
    trueNegative = ifelse(expected_higher_rt == 0 & actual_higher_rt == 0, 1, 0),
    falseNegative = ifelse(expected_higher_rt == 0 & actual_higher_rt == 1, 1, 0)
  )%>%
  na.omit(c(truePositive, falsePositive, trueNegative, falseNegative)) %>%
  mutate(
    target_UR = case_when(
      stateFips %in% UR1 ~ 1,
      stateFips %in% UR2 ~ 2,
      stateFips %in% UR3 ~ 3,
      stateFips %in% UR4 ~ 4,
      stateFips %in% UR5 ~ 5,
      stateFips %in% UR6 ~ 6
    )
  )%>%
  dplyr::select(
    UR_code,
    UR_category,
    mean_last_7_days,
    Rt3NextWeeks,
    risk_level,
    stateFips,
    target_county,
    target_risk,
    target_rt,expected_higher_rt,
    actual_higher_rt,
    target_UR,
    truePositive,
    falsePositive,
    trueNegative,
    falseNegative
  )

gc()

county1 = dplyr::filter(merged_counties, UR_code == 1 & target_UR == 1)
print("step2.1")
county2 = filter(merged_counties, UR_code == 2 & target_UR == 2)
print("step2.2")
county3 = filter(merged_counties, UR_code == 3 & target_UR == 3)
county4 = filter(merged_counties, UR_code == 4 & target_UR == 4)
county5 = filter(merged_counties, UR_code == 5 & target_UR == 5)
county6 = filter(merged_counties, UR_code == 6 & target_UR == 6)

print("step02-1")
# Create the prediction object
pred_obj <- prediction(merged_counties$expected_higher_rt,
                       merged_counties$actual_higher_rt)
print("step02-2")
# Create the performance object
perf_obj <- performance(pred_obj, "tpr", "fpr")

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
    title = "ROC Curve for Evaluating CDC Transmission Risk Level",
    subtitle = paste("AUROC:", round(auroc, 2))) +
  theme_bw() +
  annotate("text", x = 0.8,
           y = 0.2,
           label = paste("AUROC =", round(auroc, 2)), size = 4)

# Print the ROC curve plot
ggsave("Fig/gRoc.jpg",
       gRoc, 
       height=6,
       width=8,
       scale=1)

print("step4")
# AUC facet ####
# Create the prediction object 
pred_obj1 <- prediction(county1$expected_higher_rt,
                        county1$actual_higher_rt)

pred_obj2 <- prediction(county2$expected_higher_rt,
                        county2$actual_higher_rt)

pred_obj3 <- prediction(county3$expected_higher_rt,
                        county3$actual_higher_rt)

pred_obj4 <- prediction(county4$expected_higher_rt,
                        county4$actual_higher_rt)

pred_obj5 <- prediction(county5$expected_higher_rt,
                        county5$actual_higher_rt)

pred_obj6 <- prediction(county6$expected_higher_rt,
                        county6$actual_higher_rt)



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
    title = "ROC Curve for Evaluating CDC Transmission Risk Level") +
  theme_bw() +
  facet_wrap(. ~ UR_Category,
             ncol=2)+
  geom_text(aes(x = 0.8, y = 0.2, label = paste("AUROC =", round(auroc, 2))),
            size = 4, show.legend = FALSE, data = subset(roc_data, !is.na(auroc)))


# Print the ROC curve plot
ggsave("Fig/gRocF.jpg",
       gRocF, 
       height=6,
       width=4,
       scale=1.8)


# Confusion Matrix ####


cm = confusionMatrix(factor(merged_counties$expected_higher_rt),
                     factor(merged_counties$actual_higher_rt))

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
  
  labs(title = paste0("Confusion Matrix for all counties") ,
       x = "Actual",
       y = "Predicted") +
  #theme_minimal()+
  theme_classic()+
  guides(fill = FALSE)

ggsave("Fig/gCM.jpg",
       gCM, 
       height=6,
       width=8,
       scale=1)





# Confusion Matrix facet ####
cm1 = confusionMatrix(factor(county1$expected_higher_rt),
                      factor(county1$actual_higher_rt))

cm2 = confusionMatrix(factor(county2$expected_higher_rt),
                      factor(county2$actual_higher_rt))

cm3 = confusionMatrix(factor(county3$expected_higher_rt),
                      factor(county3$actual_higher_rt))

cm4 = confusionMatrix(factor(county4$expected_higher_rt),
                      factor(county4$actual_higher_rt))

cm5 = confusionMatrix(factor(county5$expected_higher_rt),
                      factor(county5$actual_higher_rt))

cm6 = confusionMatrix(factor(county6$expected_higher_rt),
                      factor(county6$actual_higher_rt))



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
    title = paste0("Confusion Matrix showing the prediction accuracy of Rt in \ndifferent UR categories") ,
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


ggsave("Fig/gCMF.jpg",
       gCMF, 
       height=6,
       width=4,
       scale=1.8)





