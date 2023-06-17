
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
library(gridExtra)
# Load data #### 
load("ProcessedData/CovidActNow.rda")
load("ProcessedData/county.NCHS.RDA")
load("ProcessedData/AUROC_merged.RDA")

unique(covid_data$date)
# Clean and Merged Data #### 
data = covid_data %>%
  dplyr::select(
    date,
    state,
    county,
    "fips_code" = fips,
    # R_t, or the estimated number of infections arising from a typical case
    "infection_rate" = metrics.infectionRate,
    #  Enum: 0 1 2 3 4 Community transmission level for region, calculated using the CDC definition.
    # Possible values:  0: Low - 1: Moderate - 2: Substantial - 3: High - 4: Unknown
    cdcTransmissionLevel
  ) %>%
  dplyr::mutate(
    cdcTransmissionLevel = case_when(
      cdcTransmissionLevel == 0 ~ "Low",
      cdcTransmissionLevel == 1 ~ "Moderate",
      cdcTransmissionLevel == 2 ~ "Substantial",
      cdcTransmissionLevel == 3 ~ "High",
      cdcTransmissionLevel == 4 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    cdcTransmissionLevel = factor(cdcTransmissionLevel,
                                  levels = c("Low", "Moderate", "Substantial", "High")), 
    date = as.Date(date, format = "%y-%m-%d"),
    cdcTransmissionLevel = factor(
      cdcTransmissionLevel,
      levels = c("Low", "Moderate", "Substantial", "High")
    )
  ) %>%
  left_join(county.NCHS,
            by = c("fips_code", "state")) %>%
  group_by(county, state) %>%
  mutate(mean_last_7_days = ifelse(is.na(infection_rate),
                                   NA,
                                   zoo::rollmean(infection_rate, k = 7, fill = NA, align = "right")),
         Rt3NextWeeks = lead(mean_last_7_days, 21),
         risk_level = case_when(cdcTransmissionLevel == "Low"  ~ 1,
                                cdcTransmissionLevel == "Moderate"  ~ 2,
                                cdcTransmissionLevel == "Substantial"  ~ 3,
                                cdcTransmissionLevel == "High"  ~ 4),
         stateFips = paste(state, fips_code, sep = ",")
  ) %>%
  filter(!is.na(Rt3NextWeeks),
         !is.na(risk_level),
         !is.na(UR_category)
  )


# boxplot Anova   #### 
myfit = aov(mean_last_7_days ~ cdcTransmissionLevel,
            data=data)
anova(myfit)


gAnova = ggplot(data, aes(x = cdcTransmissionLevel, y = mean_last_7_days, fill = cdcTransmissionLevel)) +
  geom_boxplot(alpha=.7) +
  scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
  labs(x = "CDC Transmission Level", y = "Rt (last 7 days Average)", 
       title = "Distribution of Rt by CDC risk level") +
  theme_bw()+
  stat_compare_means(method = "anova")+
  stat_compare_means(
    comparisons = combn(levels(data$cdcTransmissionLevel), 2, simplify = FALSE)[c(1, 4, 6)],
    method="t.test"
  )+
  ylim(c(0,3.8))

ggsave("Fig/gAnova.jpg",
       gAnova, 
       height=6,
       width=8,
       scale=1)


# boxplot Anova  3 weeks later #### 
myfit = aov(mean_last_7_days ~ cdcTransmissionLevel,
             data=data)





gAnova3 = ggplot(data, aes(x = cdcTransmissionLevel,
                           y = Rt3NextWeeks,
                           fill = cdcTransmissionLevel)) +
  geom_boxplot(alpha=.7) +
  scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
  labs(x = "CDC riks level", y = "Rt (last 7 days Average)", 
       title = "A) Distribution of Rt by CDC \nrisk level 3 weeks later") +
  theme_bw()+
  stat_compare_means(method = "anova")+
  stat_compare_means(
    comparisons = combn(levels(data$cdcTransmissionLevel), 2, simplify = FALSE)[c(1, 4, 6)],
    method="t.test"
  )+
  ylim(c(0,3.8))+
  guides(fill = guide_legend(title = "CDC Risk Level"))

ggsave("Fig/gAnova.jpg",
       gAnova3, 
       height=6,
       width=8,
       scale=1)



# Emmeans plot same day  #### 
cont <-
  emmeans::emmeans(
    myfit
    , specs = "cdcTransmissionLevel"
  )



gEmmeans1 = plot(cont, comparisons = F)+
  theme_bw()+
  labs(title = "Rt means for each CDC risk level")+
  labs( x = "Mean risk level")+
  labs(y = "CDC risk level")+
  labs(caption = "Distribution of Rt (last 7 days average) across different CDC risk levels.")+
  theme(plot.caption = element_text(hjust = 0))

ggsave("Fig/gEmmeans1.jpg",
       gEmmeans1, 
       height=6,
       width=8,
       scale=1)




# Emmeans plot 3 weeks later #### 

myfit3weeks = aov(Rt3NextWeeks ~ cdcTransmissionLevel,
                  data=data)

# Contrasts to perform pairwise comparisons
cont3weeks <-
  emmeans::emmeans(
    myfit3weeks
    , specs = "cdcTransmissionLevel"
  )

gEmmeans3 = plot(cont3weeks, comparisons = F)+
  theme_bw()+
  labs(title = "B) Rt means for each CDC \nrisk level 3 weeks later")+
  labs( x = "Mean risk level")+
  labs(y = "")

ggsave("Fig/gEmmeans3.jpg",
       gEmmeans3, 
       height=6,
       width=8,
       scale=1)


# merge two onova graph ####
# Combine the plots using grid.arrange
combined_plot = grid.arrange(
  gAnova3,
  gEmmeans3,
  nrow = 1,
  widths = c(6.2, 5)
  #bottom = textGrob("Figure 1: The distribution of infection rate numbers in different CDC risk levels based on ANOVA and pairwise t-tests. \nAdditionally, the marginal means are also presented.",
   #                 x = 0,
    #                y = 0.5,
     #               just = "left")
  )

# Save the combined plot
ggsave("Fig/combined_plot.jpg",
       combined_plot,
       height = 6.6,
       width = 11,
       scale = 1,
       dpi = 300)




# boxplot kruskal same day ####

gbox1 = ggplot(data, aes(x = cdcTransmissionLevel, y = mean_last_7_days, fill = cdcTransmissionLevel)) +
  geom_boxplot(alpha=.7, outlier.shape = NA) +
  scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
  labs(x = "CDC Transmission Level", y = "Rt (last 7 days Average)" 
       ,title = "Distribution of Rt by CDC transmission level"
  ) +
  theme_bw()+
  stat_compare_means()+
  ylim(c(0.5,1.5))


ggsave("Fig/gbox1.jpg",
       gbox1, 
       height=6,
       width=8,
       scale=1)




# boxplot kruskal 3 weeks later ####

gbox2 = ggplot(data, aes(x = cdcTransmissionLevel, y = Rt3NextWeeks, fill = cdcTransmissionLevel)) +
  geom_boxplot(alpha=.7) +
  scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
  labs(x = "CDC Transmission Level", y = "Rt (last 7 days Average)" 
       ,title = "Distribution of Rt by CDC transmission level at 21 days later"
  ) +
  theme_bw()+
  stat_compare_means()+
  ylim(c(0,3.3))

ggsave("Fig/gbox2.jpg",
       gbox2, 
       height=6,
       width=8,
       scale=1)




# Facet box same day #### 


gFacet = ggplot(data, aes(x = cdcTransmissionLevel, y = mean_last_7_days, fill = cdcTransmissionLevel)) +
  geom_boxplot(alpha=.7) +
  scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
  theme_bw()+
  stat_compare_means()+
  stat_compare_means(
    comparisons = combn(levels(data$cdcTransmissionLevel), 2, simplify = FALSE)[c(1, 4, 6)],
    method="wilcox.test",
    size = 2.5
  )+
  ylim(c(0,3.8))+
  labs(x = "CDC risk level", y = "Rt (last 7 days Average)", 
       title = "Distribution of Rt by CDC risk level") +
  guides(fill = guide_legend(title = "CDC Risk Level")) +
  labs(caption = "Distribution of infection rate numbers in different CDC risk levels and NCHS Urban-Rural Classification. \nStatistical comparisons based on Kruskal–Wallis and Wilcoxon signed-rank test.") +
  theme(plot.caption = element_text(hjust = 0))+
  facet_wrap(. ~ UR_category,
             ncol=2)


ggsave("Fig/gFacet.jpg",
       gFacet, 
       height=10,
       width=8,
       scale=1)



# facet box 3 weeks later  ####


gFacet3 = ggplot(data, aes(x = cdcTransmissionLevel, y = Rt3NextWeeks, fill = cdcTransmissionLevel)) +
  geom_boxplot(alpha=.7) +
  scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
  theme_bw()+
  stat_compare_means()+
  stat_compare_means(
    comparisons = combn(levels(data$cdcTransmissionLevel), 2, simplify = FALSE)[c(1, 4, 6)],
    method="wilcox.test",
    size = 2.5
  )+
  ylim(c(0,3.8))+
  labs(x = "CDC risk level", y = "Rt (last 7 days Average)", 
       title = "Distribution of Rt by CDC risk level at three weeks later") +
  guides(fill = guide_legend(title = "CDC Risk Level")) +
  labs(caption = "Distribution of infection rate numbers in different CDC risk levels and NCHS Urban-Rural Classification \nat three weeks later. Statistical comparisons based on Kruskal–Wallis and Wilcoxon signed-rank test.") +
  theme(plot.caption = element_text(hjust = 0))+
  facet_wrap(. ~ UR_category,
             ncol=2)


ggsave("Fig/gFacet3.jpg",
       gFacet3, 
       height=10,
       width=8,
       scale=1)




# table output ####
sampleSize = data %>%
  group_by(cdcTransmissionLevel,
           UR_category) %>%
  count() %>%
  rename("Counties" = n)

table_data <- data %>% 
  group_by(cdcTransmissionLevel,
           UR_category)%>%
  summarize(
    Rtmean = mean(mean_last_7_days, na.rm = T),
    Rtmedian = median(mean_last_7_days, na.rm = T),
    RtVariance= var(mean_last_7_days, na.rm = T),
    Rtmean3NextWeeks = mean(Rt3NextWeeks, na.rm = T),
    Rtmedian3NextWeeks = median(Rt3NextWeeks, na.rm = T),
    RtVariance3NextWeeks= var(Rt3NextWeeks, na.rm = T)
  )%>%
  left_join(sampleSize, by = c("cdcTransmissionLevel",
                               "UR_category"))%>%
  arrange(UR_category) %>%
  select(UR_category,
         "Risk Level" = cdcTransmissionLevel,
         "Observation" = Counties,
         "Mean Rt" = Rtmean,
         "Mean Rt (3 Weeks)" = Rtmean3NextWeeks,
         "Median Rt" = Rtmedian,
         "Median Rt (3 Weeks)" = Rtmedian3NextWeeks
  )

#kable(table_data,
#      caption = "Summary statistics by CDC transmission level",
#      align = "l")%>%
#  kableExtra::kable_styling(latex_options = "hold_position")


UR_category_list = unique(sampleSize$UR_category)



kbl_result <- kbl(table_data,
    caption = "Summary statistics by CDC transmission level",
    booktabs = T) %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>%
  pack_rows(UR_category_list[1], 1, 4) %>%
  pack_rows(UR_category_list[2], 5, 8) %>%
  pack_rows(UR_category_list[3], 9, 12) %>%
  pack_rows(UR_category_list[4], 13, 16) %>%
  pack_rows(UR_category_list[5], 17, 20) %>%
  pack_rows(UR_category_list[6], 21, 24)



# Create a temporary plot to save the table
output_file_jpg <- "Fig/table1.jpg"
kableExtra::save_kable(kbl_result,
                       file = output_file_jpg,
                       zoom = 4,
                       vwidth = 800)








# Facet ANOVA box same day #### 

gAnovaF = ggplot(data, aes(x = cdcTransmissionLevel, y = mean_last_7_days, fill = cdcTransmissionLevel)) +
  geom_boxplot(alpha=.7) +
  scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
  theme_bw()+
  stat_compare_means(method = "anova")+
  stat_compare_means(
    comparisons = combn(levels(data$cdcTransmissionLevel), 2, simplify = FALSE)[c(1, 4, 6)],
    method="t.test",
    size = 2.5
  )+
  ylim(c(0,3.8))+
  labs(x = "CDC risk level", y = "Rt (last 7 days Average)", 
       title = "Distribution of Rt by CDC risk level") +
  guides(fill = guide_legend(title = "CDC Risk Level")) +
  labs(caption = "Distribution of infection rate numbers in different CDC risk levels and NCHS Urban-Rural Classification.\nStatistical comparisons based on ANOVA and pairwise t-tests.") +
  theme(plot.caption = element_text(hjust = 0))+
  facet_wrap(. ~ UR_category,
             ncol=2)


ggsave("Fig/gAnovaF.jpg",
       gAnovaF, 
       height=10,
       width=8,
       scale=1)




# facet ANOVA box 3 weeks later  ####

gAnovaF3 = ggplot(data, aes(x = cdcTransmissionLevel, y = Rt3NextWeeks, fill = cdcTransmissionLevel)) +
  geom_boxplot(alpha=.7) +
  scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
  theme_bw()+
  stat_compare_means(method = "anova")+
  stat_compare_means(
    comparisons = combn(levels(data$cdcTransmissionLevel), 2, simplify = FALSE)[c(1, 4, 6)],
    method="t.test",
    size = 2.5
  )+
  ylim(c(0,3.8))+
  labs(x = "CDC risk level", y = "Rt (last 7 days Average)", 
       title = "Distribution of Rt by CDC risk level at three weeks later") +
  guides(fill = guide_legend(title = "CDC Risk Level")) +
  labs(caption = "Distribution of infection rate numbers in different CDC risk levels and NCHS Urban-Rural Classification \nat three weeks later. Statistical comparisons based on ANOVA and pairwise t-tests.") +
  theme(plot.caption = element_text(hjust = 0))+
  facet_wrap(. ~ UR_category,
             ncol=2)


ggsave("Fig/gAnovaF3.jpg",
       gAnovaF3, 
       height=10,
       width=8,
       scale=1)
