# Jung Mee Park
# 2023-03-28
# upset plot in R

#### another attempt with ComplexUpset ####
library(ComplexUpset)
#### make an UpSetR plot ####
upset_plot_df <- Cat_date_filter %>% 
  select(`App-instance ID`, Appt, Edit, Case, Goal) %>% 
  distinct() 
upset_plot_df <- upset_plot_df %>% 
  select(-`App-instance ID`) %>% 
  data.frame() %>%
  # t() %>% # transpose the result, ugh
  as_tibble()
upset_plot_df[is.na(upset_plot_df)] <- 0
upset_plot_df <- upset_plot_df %>% 
  filter(Appt== 1 | Edit ==1 |Case ==1 |Goal==1) %>%
  ungroup()
set_vars <- c("Appt", "Edit","Case", "Goal")
names(set_vars) <- set_vars
# Cat_date_filter %>% print(n = nrow(Cat_date_filter))
# 
# 
# symptom_mat <- map_dfc(subsets, str_detect, symptoms) %>%
#   data.frame() %>%
#   t() %>% # transpose the result, ugh
#   as_tibble()
# 
# colnames(symptom_mat) <- symptoms
upset_plot <- upset(data = upset_plot_df, intersect = set_vars, set_sizes = FALSE) +
  labs(title = "Usage of CatCloud Tools")
upset_plot
today <- Sys.Date()
image_path = "/Users/jungmeepark/Documents/My Tableau Repository/Shapes/Rplots/"

ggsave(filename = paste0(image_path,today,"upset_plot.png"),  
       width = 10, height = 4, dpi = 150, units = "in", device='png')

# 
# ggsave(filename = file.path(image_path, '.png', plot = upset_plot))

