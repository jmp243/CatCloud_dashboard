# Jung Mee Park
# data visualizations
# 2022-09-26

library(data.table)
library(lattice)
library(latticeExtra)      

#### get the top N majors ####
count_Majors <- table(cat_sf_full$Study1)  

print (count_Majors)

cat_sf_full$Study1 <- factor(cat_sf_full$Study1)
levels(cat_sf_full$Study1)

####
cat_sf_full %>%
  group_by(Study1) %>%
  top_n(2, Cumulative.GPA)

#### visualizations ####
# 
# plot <- ggplot() +
#   # Points for each car
#   geom_point(data = SF_UAccess, mapping = aes(y = Cumulative.GPA, x = Primary.Major.Plan)) +
#   # Vertical bars for the means
#   geom_point(data = SF_UAccess %>% 
#                # Group the data by brand then get means
#                group_by(Primary.Major.Plan) %>% 
#                mutate(mean_GPA = mean(Cumulative.GPA)), 
#              # Specify aesthetics
#              mapping = aes(y = Primary.Major.Plan, x = mean_GPA), 
#              size = 10, color = 'red', shape = '|') 
# plot <- ggplot(SF_UAccess, aes(Primary.Major.Plan, Cumulative.GPA)) +           # ggplot2 barplot with sum
#   geom_point(stat = "identity")
# 
# print(plot)

# top 20 undergraduate majors
d2 <- cat_sf_full %>%
  count(Study1) %>%
  top_n(20) %>%
  arrange(desc(n, Study1)) %>%
  mutate(Study1 = factor(Study1, levels = unique(Study1)))

GPA_top20_major <- cat_sf_full %>%
  filter(Study1 %in% d2$Study1) %>%
  filter(Career == "Undergraduate") %>% 
  mutate(Study1 = factor(Study1, levels = levels(d2$Study1))) %>%
  ggplot(aes(x = Study1, y = Cumulative.GPA, fill = Study1)) +
  geom_violin() +
  ylab("Cumulative GPA") +
  xlab("Majors") +
  stat_summary(fun = "mean", geom = "point", shape = 8, size = 3, color = "midnightblue") +
  theme(axis.text.x = element_text(angle = -70, hjust = 0, vjust = 0)) +
  theme(legend.position="none") 

GPA_top20_major  <- GPA_top20_major  +  labs(title = "Violin plot of Top 20 majors", 
                                     subtitle = "with mean GPA") 
print(GPA_top20_major)


#### all majors distribution ####
u_grads <- Cat_SF_Students %>% 
  filter(Career == "Undergraduate")
#### reorder undergrads ####
# unique(u_grads$Academic.Level...Beginning.of.Term)
# u_grads$Academic.Level...Beginning.of.Term <- factor(u_grads$Academic.Level...Beginning.of.Term, 
#                                                      levels=c("Freshman", "Sophomore", "Junior", "Senior"))

#### histogram for undergraduate GPA ####
hist_ugrad <- ggplot(u_grads, aes(Cumulative.GPA, fill = Academic.Level...Beginning.of.Term)) + 
  geom_histogram(color = "black", binwidth = 0.3) +
  xlab("Cumulative GPA") +
  ylab("Number of Students") +
  labs(fill='Academic Level')

hist_ugrad  <- hist_ugrad  +  labs(title = "Histogram of Undergraduate GPA") 
hist_ugrad

#### what do do about class app ####

g <- ggplot(cat_sf_full[which(cat_sf_full$Goals.Event.count>0),]) + 
  stat_summary(mapping = aes(x=Class_Standing_recode, y=Goals.Event.count, fill = Career), fun = "sum", geom = "bar", 
               na.rm = TRUE, inherit.aes = FALSE) +
  ylab("Academic Level") +
  xlab("Count of Goals Users") +
  labs(title = "Academic Levels of Goal Setters", fill = "Academic Career") +
  theme(legend.position = c(0.85, 0.75), axis.text.x = element_text(angle = -70, hjust = 0, vjust = 0)) 

g 

add_users <- ggplot(Cat_SF_Students[which(Cat_SF_Students$Add.Event.Count>0),]) + 
  stat_summary(mapping = aes(x=Academic.Level...Beginning.of.Term, y=Add.Event.Count, fill = Career), fun = "sum", geom = "bar", 
               na.rm = TRUE, inherit.aes = FALSE) +
  ylab("Academic Level") +
  xlab("Count of Add Users") +
  labs(title = "Academic Levels of add or edit", fill = "Academic Career") +
  theme(legend.position = c(0.15, 0.75), axis.text.x = element_text(angle = -70, hjust = 0, vjust = 0)) 

add_users


add_goal <- ggplot(Cat_SF_Students[which(Cat_SF_Students$Add.Event.Count>0 | Cat_SF_Students$Goals.Event.count>0),]) + 
  stat_summary(mapping = aes(x=Academic.Level...Beginning.of.Term, y=Add.Event.Count, fill = Career), fun = "sum", 
               geom = "bar",
               na.rm = TRUE, inherit.aes = FALSE) +
  stat_summary(mapping = aes(x=Academic.Level...Beginning.of.Term, y=Goals.Event.count), fun = "sum", 
               # geom = "line", 
               na.rm = TRUE, inherit.aes = FALSE) +
  ylab("Academic Level") +
  xlab("Count of Users") +
  labs(title = "Academic Levels of Event Counts", fill = "Academic Career") +
  theme(legend.position = c(0.15, 0.75), axis.text.x = element_text(angle = -70, hjust = 0, vjust = 0)) 

add_goal

add_goal <- add_goal +   scale_fill_manual(name = NULL,
                               guide = "legend",
                               values = c("Undergraduate" = "lightblue",
                                          "Graduate" = "red",
                                          "Law" = "darkblue",
                                          "Pharmacy" = "orange",
                                          "Goals.Event.count" = "black"
                                          ),
                               labels = c('Undergraduate',
                                          'Graduate',
                                          'Law',
                                          'Pharmacy',
                                          'Goals'))
add_goal


# list_to_matrix(upset_plot_df)

# text_scale_options1 <- c(1, 1, 1, 1, 0.75, 1)
# text_scale_options2 <- c(1.3, 1.3, 1, 1, 2, 0.75)
# text_scale_options3 <- c(1.5, 1.25, 1.25, 1, 2, 1.5)
# 
# main_bar_col <- c("violetred4")
# sets_bar_col <- c("turquoise4")
# matrix_col <- c("slateblue4")
# shade_col <- c("wheat4")
# 
# mb_ratio1 <- c(0.55,0.45)

set_vars <- c("Appt", "Edit", "Goal")

# upset(upset_plot_df, 
#       sets = set_vars,
#       empty.intersections=FALSE,
#       mb.ratio = mb_ratio1, 
#       order.by = "freq",
#       show.numbers = FALSE,
#       point.size = 2, 
#       line.size = 1,
#       text.scale=text_scale_options3,
#       main.bar.color = main_bar_col,
#       sets.bar.color = sets_bar_col,
#       matrix.color = matrix_col,
#       shade.color = shade_col)

# #### another attempt with ComplexUpset ####
library(ComplexUpset)
#### make an UpSetR plot ####
upset_plot_df <- Cat_date_filter %>% 
  select(`App-instance ID`, Appt, Edit, Goal) %>% 
  distinct() 
upset_plot_df <- upset_plot_df %>% 
  select(-`App-instance ID`) %>% 
  data.frame() %>%
  # t() %>% # transpose the result, ugh
  as_tibble()
upset_plot_df[is.na(upset_plot_df)] <- 0
upset_plot_df <- upset_plot_df %>% 
  filter(Appt== 1 | Edit ==1 |Goal==1) %>%
  ungroup()
set_vars <- c("Appt", "Edit", "Goal")
names(set_vars) <- set_vars
Cat_date_filter %>% print(n = nrow(Cat_date_filter))
# 
# 
# symptom_mat <- map_dfc(subsets, str_detect, symptoms) %>%
#   data.frame() %>%
#   t() %>% # transpose the result, ugh
#   as_tibble()
# 
# colnames(symptom_mat) <- symptoms
upset(data = upset_plot_df, intersect = set_vars, set_sizes = FALSE) +
  labs(title = "Usage of CatCloud Tools")

