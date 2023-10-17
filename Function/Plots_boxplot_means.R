
# Library
library(dplyr)
library(ggplot2)
library(gridExtra)
library(data.table)
library(usdata)
library(ggpubr)
library(emmeans)


# Load CDC, covidestimates and NCHS Data
estimates_data = fread("RawData/estimates.csv")
load("ProcessedData/county.NCHS.RDA")
CDC_data = fread("RawData/United_States_COVID-19_County_Level_of_Community_Transmission_Historical_Changes_-_ARCHIVED.csv")





#clean estimate data
estimates = estimates_data %>%
  dplyr::select(
    date,
    fips,
    #Estimate of the effective reproductive number (Rt)
    "Rt" = r_t
  ) %>%
  mutate(
    date = as.Date(date, "%y/%m/%d")
  )




# Clean CDC data
CDC = CDC_data %>%
  dplyr::select(
    date,
    "fips" = fips_code,
    "county" = county_name,
    "state" = state_name,
    community_transmission_level
  ) %>%
  filter(community_transmission_level != "")%>%
  mutate(
    date = as.Date(date, "%m/%d/%Y"),
    community_transmission_level = factor(community_transmission_level,
                                          level=c("low", "moderate", "substantial", "high"))
  )



# clean NCHS Data
NCHS = county.NCHS %>%
  dplyr::select(
    "fips" = fips_code,
    state,
    UR_code,
    UR_category
  ) %>%
  dplyr::mutate(
    state = abbr2state(state)
  )


data = estimates %>%
  inner_join(CDC, by=c("date", "fips")) %>%
  left_join(NCHS, by=c("state", "fips")) %>%
  group_by(county, state) %>%
  mutate(
    Rt_sameWeek = Rt,
    Rt_NextWeek = lead(Rt, 1),
    Rt_1prevWeek = lag(Rt, 1),
    Rt_2prevWeek = lag(Rt, 2),
    Rt_3prevWeek = lag(Rt, 3),
    risk_level = case_when(community_transmission_level == "low"  ~ 1,
                           community_transmission_level == "moderate"  ~ 2,
                           community_transmission_level == "substantial"  ~ 3,
                           community_transmission_level == "high"  ~ 4),
    stateFips = paste0(state, fips),
    community_transmission_level = factor(community_transmission_level)
  ) 
  # na.omit(
  #   cols=c("Rt",
  #         "Rt_1prevWeek",
  #         "Rt_2prevWeek",
  #         "Rt_3prevWeek",)
  #   )



plot = function(t11, t12, t21, t22, t31, t32, t41, t42){
  xl1 = .9
  xl2 = 1.2
  
  aly = 1.75
  tly = c(-.4, -.3, -.2)
  
  ylim = c(.5,1.9)
  ## first ROW
  g11 =  ggplot(data,
                aes(x = community_transmission_level,
                          y = Rt,
                          fill = community_transmission_level)) +
    geom_boxplot(alpha=.7, outlier.shape = NA) +
    scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
    labs(x = "CDC Risks Level",
         y = "Rt", 
         title = t11) +
    theme_bw()+
    stat_compare_means(method = "anova",
                       label.y = aly)+
    stat_compare_means(
      comparisons = combn(levels(data$community_transmission_level), 2, simplify = FALSE)[c(1, 4, 6)],
      method="t.test",
      label.y = tly,
      tip.length = 0
    )+
    guides(fill = guide_legend(title = "CDC Risk Level"))+
    ylim(ylim)
  
  
  # Emmeans plot same week 
  myfit12 = lm(Rt ~ community_transmission_level,
                    data=data)
  
  # Contrasts to perform pairwise comparisons
  cont12 = emmeans::emmeans(
      myfit12,
      specs = "community_transmission_level",
      data=data
    )
  
  pairs(cont12)
  
  # Create a data frame from the comparison results
  comparison_df <- as.data.frame(cont12)
  
  # Create a custom plot using ggplot2
  g12 = ggplot(comparison_df,
         aes(y = community_transmission_level,
             x = emmean,
             xmin = emmean - SE,
             xmax = emmean + SE)) +
    geom_pointrange() +
    labs(y = "",
         x = "Mean CDC Risk Level",
         title = t12) +
    theme_bw()+
    xlim(c(xl1,xl2))
  
  
  ## Second ROW
  g21 =  ggplot(data=data %>% na.omit(cols="Rt_1prevWeek"),
                aes(x = community_transmission_level,
                    y = Rt_1prevWeek,
                    fill = community_transmission_level)) +
    geom_boxplot(alpha=.7, outlier.shape = NA) +
    scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
    labs(x = "CDC Risks Level",
         y = "Rt", 
         title = t21) +
    theme_bw()+
    stat_compare_means(method = "anova",
                       label.y = aly)+
    stat_compare_means(
      comparisons = combn(levels(data$community_transmission_level), 2, simplify = FALSE)[c(1, 4, 6)],
      method="t.test",
      label.y = tly,
      tip.length = 0
    )+
    guides(fill = guide_legend(title = "CDC Risk Level"))+
    ylim(ylim)
  
  
  # Emmeans plot same week 
  myfit12 = lm(Rt_1prevWeek ~ community_transmission_level,
               data=data%>%na.omit(cols="Rt_1prevWeek"))
  
  # Contrasts to perform pairwise comparisons
  cont12 = emmeans::emmeans(
    myfit12,
    specs = "community_transmission_level",
    data=data%>%na.omit(cols="Rt_1prevWeek")
  )
  
  # Create a data frame from the comparison results
  comparison_df <- as.data.frame(cont12)
  
  # Create a custom plot using ggplot2
  g22 = ggplot(comparison_df,
               aes(y = community_transmission_level,
                   x = emmean,
                   xmin = emmean - SE,
                   xmax = emmean + SE)) +
    geom_pointrange() +
    labs(y = "",
         x = "Mean CDC Risk Level",
         title = t22) +
    theme_bw()+
    xlim(c(xl1,xl2))
  
  
  ## Third ROW
  g31 =  ggplot(data=data%>%na.omit(cols="Rt_2prevWeek"),
                aes(x = community_transmission_level,
                    y = Rt_2prevWeek,
                    fill = community_transmission_level)) +
    geom_boxplot(alpha=.7, outlier.shape = NA) +
    scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
    labs(x = "CDC Risks Level",
         y = "Rt", 
         title = t31) +
    theme_bw()+
    stat_compare_means(method = "anova",
                       label.y = aly)+
    stat_compare_means(
      comparisons = combn(levels(data$community_transmission_level), 2, simplify = FALSE)[c(1, 4, 6)],
      method="t.test",
      label.y = tly,
      tip.length = 0
    )+
    guides(fill = guide_legend(title = "CDC Risk Level"))+
    ylim(ylim)
  
  
  # Emmeans plot same week 
  myfit12 = lm(Rt_2prevWeek ~ community_transmission_level,
               data=data%>%na.omit(cols="Rt_2prevWeek"))
  
  # Contrasts to perform pairwise comparisons
  cont12 = emmeans::emmeans(
    myfit12,
    specs = "community_transmission_level",
    data=data%>%na.omit(cols="Rt_2prevWeek")
  )
  
  # Create a data frame from the comparison results
  comparison_df <- as.data.frame(cont12)
  
  # Create a custom plot using ggplot2
  g32 = ggplot(comparison_df,
               aes(y = community_transmission_level,
                   x = emmean,
                   xmin = emmean - SE,
                   xmax = emmean + SE)) +
    geom_pointrange() +
    labs(y = "",
         x = "Mean CDC Risk Level",
         title = t32) +
    theme_bw()+
    xlim(c(xl1,xl2))
  
  
  ## Fourth ROW
  g41 =  ggplot(data=data%>%na.omit(cols="Rt_3prevWeek"),
                aes(x = community_transmission_level,
                    y = Rt_3prevWeek,
                    fill = community_transmission_level)) +
    geom_boxplot(alpha=.7,outlier.shape = NA) +
    scale_fill_manual(values = c("#4393c3", "#ffffbf", "#fdae61", "#d73027" )) +
    labs(x = "CDC Risks Level",
         y = "Rt", 
         title = t41) +
    theme_bw()+
    stat_compare_means(method = "anova",
                       label.y = aly)+
    stat_compare_means(
      comparisons = combn(levels(data$community_transmission_level), 2, simplify = FALSE)[c(1, 4, 6)],
      method="t.test",
      label.y = tly,
      tip.length = 0
    )+
    guides(fill = guide_legend(title = "CDC Risk Level"))+
    ylim(ylim)
  
  
  # Emmeans plot same week 
  myfit12 = lm(Rt_3prevWeek ~ community_transmission_level,
               data=data%>%na.omit(cols="Rt_3prevWeek"))
  
  # Contrasts to perform pairwise comparisons
  cont12 = emmeans::emmeans(
    myfit12,
    specs = "community_transmission_level",
    data=data%>%na.omit(cols="Rt_3prevWeek")
  )
  
  # Create a data frame from the comparison results
  comparison_df <- as.data.frame(cont12)
  
  # Create a custom plot using ggplot2
  g42 = ggplot(comparison_df,
               aes(y = community_transmission_level,
                   x = emmean,
                   xmin = emmean - SE,
                   xmax = emmean + SE)) +
    geom_pointrange() +
    labs(y = "",
         x = "Mean CDC Risk Level",
         title = t42) +
    theme_bw()+
    xlim(c(xl1,xl2))

  # Combine the plots using grid.arrange
  combined_plot = grid.arrange(
    g11,
    g12,
    g21,
    g22,
    g31,
    g32,
    g41,
    g42,
    nrow = 4,
    widths = c(6.2, 5)
  )
  
  # Save the combined plot
  ggsave("Figures/combined_plot.jpg",
         combined_plot,
         height = 14.4,
         width = 10.8,
         scale = 1,
         dpi = 300)
  
}


plot(t11 = "A) Distribution of Rt by CDC \nrisk level for the same week",
     t12 = "B) Rt means for each CDC \nrisk level for same weeks",
     t21 = "C) Distribution of Rt by CDC \nrisk level for one week prior",
     t22 = "D) Rt means for each CDC \nrisk level for one week prior",
     t31 = "E) Distribution of Rt by CDC \nrisk level for two week prior",
     t32 = "F) Rt means for each CDC \nrisk level for two week prior",
     t41 = "G) Distribution of Rt by CDC \nrisk level for three week prior",
     t42 = "H) Rt means for each CDC \nrisk level for three week prior"
     )

1?stat_compare_mean
