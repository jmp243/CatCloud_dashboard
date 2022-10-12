# Jung Mee Park
# data visualizations
# 2022-09-26

library(data.table)
#### get the top N majors ####
count_Majors <- table(SF_UAccess$Primary.Major.Plan)  

print (count_Majors)

SF_UAccess$Primary.Major.Plan <- factor(SF_UAccess$Primary.Major.Plan)
levels(SF_UAccess$Primary.Major.Plan)

####
SF_UAccess %>%
  group_by(Primary.Major.Plan) %>%
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

d2 <- SF_UAccess %>%
  count(Primary.Major.Plan) %>%
  top_n(20) %>%
  arrange(desc(n, Primary.Major.Plan)) %>%
  mutate(Primary.Major.Plan = factor(Primary.Major.Plan, levels = unique(Primary.Major.Plan)))

GPA_top20_major <- SF_UAccess %>%
  filter(Primary.Major.Plan %in% d2$Primary.Major.Plan) %>%
  mutate(Primary.Major.Plan = factor(Primary.Major.Plan, levels = levels(d2$Primary.Major.Plan))) %>%
  ggplot(aes(x = Primary.Major.Plan, y = Cumulative.GPA, fill = Primary.Major.Plan)) +
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
ggplot(SF_UAccess, aes(Cumulative.GPA)) + 
  geom_histogram(binwidth = 0.1) 
