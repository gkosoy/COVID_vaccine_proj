library(corrplot)
library(tidyverse)

#create wide df 
protein_list_LOD <- unique(LOD_corrected_df$protein_name)#can choose proteins of interest here   
draw_choice <- "s_05" #choose blood draw 
corr_plot_s_05_LOD <- LOD_corrected_df %>% 
  filter(time_draw == draw_choice) %>%
  filter(protein_name %in% protein_list_LOD) %>%
  select(c(protein_name, corrected_thickness_2)) %>%
  pivot_wider(names_from = protein_name,
              values_from = corrected_thickness_2,
              values_fn = list) %>% 
  unnest(., all_of(protein_list_LOD))   

#get df of all R^2 and p-values for binding between all protein combinations
get_correlation_list <- function(df, col_list){
  new_df <- data.frame() 
  for (protein in col_list){
    protein <- protein
    col_list <- col_list[col_list != protein]
    for (prot in col_list){
      model <- lm(get(protein)~get(prot), data=df)
      r_val <- summary(model)$adj.r.squared
      p_val <- summary(model)$coefficients[,4][2]
      new_df <- rbind(new_df, c(r_val, p_val, protein, prot))
    }
  }
  colnames(new_df) <- c("r_value","p_val", "protein_1", "protein_2")
  return(new_df)
}
s_05_corr_cols <- colnames(corr_plot_s_05) 
s_05_corr_df<- get_correlation_list(corr_plot_s_05, s_05_corr_cols)
#filter for significant correlations and save df
s_05_corr_df_filt <- s_05_corr_df %>% filter(r_value >= 0.7)
write.csv(s_05_corr_df_filt, "correlations_df_covid.csv")

#plotting antibody response over time (waning antibody)
####begin####
#time course geom point plot
ids <- c("02","03","04","12","27","28","29")
#protein <- "SARS-CoV-2 RBD"
proteins <- final_sums$protein_name
for (protein in proteins){
time_course_df <- final_sums %>% filter(protein_name == protein) %>% 
  filter(!is.na(time_draw)) %>% filter(!time_draw == 0)  %>% 
  filter(!day_draw > 250)  %>% filter(id %in% ids) 
plot <- ggplot(data = time_course_df)+
  geom_point(aes(x = day_draw, y = corrected_thickness, color = id), size = 10) + 
  guides(colour = guide_legend(override.aes = list(size=7))) +
  geom_vline(xintercept = 28, size = 2) +
  geom_vline(xintercept = 21, size = 2, linetype = "dashed") +
  geom_text(aes(8, 25, label = "2nd Pfizer"), size = 13) +
  geom_text(aes(44, 0, label = "2nd Moderna"), size = 13) +
  ggtitle("") +
  theme_bw() +
  ylim(0,40) +
  xlab("days") + ylab(expression(paste("thickness change (", ring(A), ")"))) +
  theme(text = element_text(size = 50)) +
  theme(legend.position="none")
tiff(paste(protein, "time_course.tiff"), units="in", width=25, height=12, res=300)
print(plot)
dev.off()
}
####end####