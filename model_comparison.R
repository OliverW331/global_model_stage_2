rm(list=ls());gc()
graphics.off()

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(countrycode)
library(dplyr)
library(tidyr)
library(sf)
library(scales)
library(pROC)
### Switch ###
data_preprocess = FALSE
run_glm_models = FALSE
draw_sum_p_t_graph = FALSE

if(data_preprocess){
  df = readRDS("result/all_sp_time_0.75/pvo_df.rds")
  gdp_pop = readRDS("model_data/regionalized_model_data/A-socioEcoDat-array_unnormalized.rds")
  gdp_norm = scale(gdp_pop[,,1])
  pop_norm = scale(gdp_pop[,,2])
  df[,2] = colnames(gdp_pop)[df[, 2]]
  df$gdp <- mapply(function(year, country) gdp_norm[year, country], df$year, df$country)
  df$pop <-mapply(function(year, country) pop_norm[year, country], df$year, df$country)
}

df = readRDS("result/all_sp_time_0.75/pvo_df.rds")

### Compare models ###
if (run_glm_models){
  glm_t = glm(obs ~ year, df, family = "binomial")
  glm_simp_t_gdp_pop = glm(obs ~ year + gdp + pop, df, family = "binomial")
  glm_t_gdp_pop = glm(obs ~ year * gdp * pop, df, family = "binomial")
  
  df$logit.pred_simp_t_gdp_pop = glm_simp_t_gdp_pop$fitted.values
  df$logit.pred_t_gdp_pop = glm_t_gdp_pop$fitted.values
  
  saveRDS(glm_t, "compare_models/glm_t.rds")
  saveRDS(glm_simp_t_gdp_pop, "compare_models/glm_simp_t_gdp_pop.rds")
  saveRDS(glm_t_gdp_pop, "compare_models/glm_t_gdp_pop.rds")
}
glm_t = readRDS("compare_models/glm_t.rds")
glm_simp_t_gdp_pop = readRDS("compare_models/glm_simp_t_gdp_pop.rds")
glm_t_gdp_pop = readRDS("compare_models/glm_t_gdp_pop.rds")
summary(glm_t)
summary(glm_simp_t_gdp_pop)
summary(glm_t_gdp_pop)

#saveRDS(df, "result/all_sp_time_0.75/pvo_df.rds")
# write.csv(df, "result/all_sp_time_0.75/pvo_df.csv")
(glm_simp_t_gdp_pop$null.deviance-glm_simp_t_gdp_pop$deviance)/glm_simp_t_gdp_pop$null.deviance
roc(df$obs ~ df$logit.pred_simp_t_gdp_pop)$auc

### MAKE A DATAFRAME TO COMPARE MODELS ###
#Calculate AIC for global_invasion_model

cal_aic = function(k, pred, obs){
  logLikelihood <- sum(obs * log(pred) + (1 - obs) * log(1 - pred))
  AIC <- -2 * logLikelihood + 2 * k
  return(AIC)
}
gim_aic = cal_aic(9,df$pred, df$obs)
df_models = data.frame(model = c("glm_t","glm_simp_t_gdp_pop","glm_t_gdp_pop", "global_invasion_model"))
df_models$AIC = c(glm_t$aic, glm_simp_t_gdp_pop$aic, glm_t_gdp_pop$aic, gim_aic)

saveRDS(df_models, "result/model_comparison/df_models.rds")

### Compare models###
df_models = readRDS("result/model_comparison/df_models.rds")




###regional change in probability over years###
region_map = readRDS("model_data/regionalized_model_data/ecoregion_map.rds")
region_name = readRDS("model_data/regionalized_model_data/ecoregions_with_names.rds")

# Group data by country and summarize the change in pred, logit.pred, and logit.pred_t_gdp_pop over time
df_trends <- df %>%
  group_by(country, year) %>%
  summarise(
    sum_pred = sum(pred),
    sum_logit_pred = sum(logit.pred),
    sum_logit_pred_t_gdp_pop = sum(logit.pred_t_gdp_pop),
    sum_logit_pred_simp_t_gdp_pop = sum(logit.pred_simp_t_gdp_pop),
    .groups = 'drop'
  )
saveRDS(df_trends, "model_data/regionalized_model_data/df_trends.rds")

df_trends = readRDS("model_data/regionalized_model_data/df_trends.rds")
# Plotting the trend of pred for each region over time
if (draw_sum_p_t_graph){
  ggplot(df_trends, aes(x = year)) +
    geom_line(aes(y = sum_pred, group = country, color = country), size = 0.5) +
    labs(title = "Sum Predicted Probability of Invasion (global invasion model,AUC=0.75, AIC=26512.34) Over Time by Region",
         x = "Year",
         y = "Sum pred") +
    theme_bw()
  ggsave("graph/probability_time_graph/pred_change_over_time.png", width = 12, height = 9)
  # Repeat plotting for logit.pred and logit.pred_t_gdp_pop
  ggplot(df_trends, aes(x = year)) +
    geom_line(aes(y = sum_logit_pred, group = country, color = country), size = 0.5) +
    labs(title = "Sum Predicted Probability of Invasion (GLM with time, AUC=0.65, AIC=26994.63) Over Time by Region",
         x = "Year",
         y = "Sum logit.pred") +
    theme_bw()
  ggsave("graph/probability_time_graph/logit_pred_change_over_time.png", width = 12, height = 9)
  
  ggplot(df_trends, aes(x = year)) +
    geom_line(aes(y = sum_logit_pred_simp_t_gdp_pop, group = country, color = country), size = 0.5) +
    labs(title = "Sum Predicted Probability of Invasion (simple GLM with time, GDP, Population, AUC=0.657, AIC=26952.15) Over Time by Region",
         x = "Year",
         y = "Sum logit.pred_simp_t_gdp_pop") +
    theme_bw()
  ggsave("graph/probability_time_graph/logit_pred_simp_t_gdp_pop_change_over_time.png", width = 12, height = 9)
  
  ggplot(df_trends, aes(x = year)) +
    geom_line(aes(y = sum_logit_pred_t_gdp_pop, group = country, color = country), size = 0.5) +
    labs(title = "Sum Predicted Probability of Invasion (interative GLM with time, GDP, Population, AUC=0.70, AIC=26607.13) Over Time by Region",
         x = "Year",
         y = "Sum logit.pred_t_gdp_pop") +
    theme_bw()
  ggsave("graph/probability_time_graph/logit_pred_t_gdp_pop_change_over_time.png", width = 12, height = 9)
}


### create a map the reflect the change in probability of each region ###
# use the slopes of fitted lines generated by the probabilities of each region over year
slopes <- df_trends %>%
  group_by(country) %>%
  do(trend = lm(sum_pred ~ year, data = .)) %>%
  summarise(country = country, slope = coef(trend)[["year"]])

p_final <- df_trends %>%
  filter(year == max(year))

region_map <- region_map %>%
  left_join(slopes, by = c("region" = "country")) %>%
  left_join(p_final, by = c("region" = "country"))


# Assuming region_map is already loaded into R and has the appropriate structure.

eco_map_slope <- ggplot(data = region_map) + 
  geom_polygon(aes(x = long, y = lat, fill = slope, group = group), color = "white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "purple", midpoint = mean(slopes$slope), 
                       name = "Trend Slope", 
                       breaks = pretty_breaks(n = 5), # This will create pretty breaks for your legend
                       labels = scales::number_format(accuracy = 0.001)) + # Formatting the labels
  coord_fixed(1.3) + 
  labs(title = "Regional Trends in Predicted Probability of Invasion",
       subtitle = "Slope of the fitted line over time",
       x = "", y = "") + # Hiding x and y axis labels as they are often not informative for maps
  theme_bw() +
  theme(legend.position = "right", # Positioning the legend on the right
        plot.title = element_text(hjust = 0.5), # Center the plot title
        plot.subtitle = element_text(hjust = 0.5)) # Center the plot subtitle

ggsave("graph/map/regional_trends_sum_pred_overtime.png", height = 12, width = 10)
print(eco_map_slope)

eco_map_22 <- ggplot(data = region_map) + 
  geom_polygon(aes(x = long, y = lat, fill = sum_pred, group = group), color = "white") + 
  scale_fill_gradient2(low = "green", high = "yellow", mid = "orange", midpoint = mean(p_final$sum_pred), 
                       name = "Probability", 
                       breaks = pretty_breaks(n = 5), # This will create pretty breaks for your legend
                       labels = scales::number_format(accuracy = 0.000001)) + # Formatting the labels
  coord_fixed(1.3) + 
  labs(title = "Regional Sum of Predicted Probability of Invasion in 2016",
       subtitle = "Sum of predicted probability across species for each region over time",
       x = "", y = "") + # Hiding x and y axis labels as they are often not informative for maps
  theme_bw() +
  theme(legend.position = "right", # Positioning the legend on the right
        plot.title = element_text(hjust = 0.5), # Center the plot title
        plot.subtitle = element_text(hjust = 0.5)) # Center the plot subtitle
ggsave("graph/map/regional_sum_pred_2016.png", height = 12, width = 10)
print(eco_map_22)

