# Jung Mee Park
# data visualizations
# 2022-09-26

library(data.table)
library(lattice)
library(latticeExtra)      

#### get the top N majors ####
count_Majors <- table(Cat_SF_Students$Primary.Major.Plan)  

print (count_Majors)

Cat_SF_Students$Primary.Major.Plan <- factor(Cat_SF_Students$Primary.Major.Plan)
levels(Cat_SF_Students$Primary.Major.Plan)

####
Cat_SF_Students %>%
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

# top 20 undergraduate majors
d2 <- Cat_SF_Students %>%
  count(Primary.Major.Plan) %>%
  top_n(20) %>%
  arrange(desc(n, Primary.Major.Plan)) %>%
  mutate(Primary.Major.Plan = factor(Primary.Major.Plan, levels = unique(Primary.Major.Plan)))

GPA_top20_major <- Cat_SF_Students %>%
  filter(Primary.Major.Plan %in% d2$Primary.Major.Plan) %>%
  filter(Career == "Undergraduate") %>% 
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

g <- ggplot(Cat_SF_Students[which(Cat_SF_Students$Goals.Event.count>0),]) + 
  stat_summary(mapping = aes(x=Academic.Level...Beginning.of.Term, y=Goals.Event.count, fill = Career), fun = "sum", geom = "bar", 
               na.rm = TRUE, inherit.aes = FALSE) +
  ylab("Academic Level") +
  xlab("Count of Goals Users") +
  labs(title = "Academic Levels of Goal Setters", fill = "Academic Career") +
  theme(legend.position = c(0.15, 0.75), axis.text.x = element_text(angle = -70, hjust = 0, vjust = 0)) 

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
