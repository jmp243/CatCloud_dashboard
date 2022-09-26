# Jung Mee Park
# data visualizations
# 2022-09-26

#### visualizations ####
graph <- SF_UAccess %>%
  group_by(Cumulative.GPA) %>% 
  # mutate(Q4 = fct_infreq(Q4)) %>%
  drop_na(Cumulative.GPA) %>% 
  ggplot(aes(x = factor(Primary.Major.Plan)), y = ..count.., fill = EDS.Primary.Affiliation) +
  geom_bar(aes(fill = )) +
  # coord_flip() +
  labs(x = "", y = "responses")

Q4graph <- Q4graph + labs(title = "How satisfied were you with your initial training in Trellis?",
                          subtitle = "Question 4 - full data", fill = "Profile Name")

print(Q4graph)
