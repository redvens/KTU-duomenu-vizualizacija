#Duomenu skaitymas ir filtravimas
library(tidyverse)
data <- read_csv("lab_sodra.csv")
R <- data %>% filter(ecoActCode == 862300)

#1
R %>% ggplot(aes(x=avgWage)) +
  geom_histogram(binwidth = 100, colour = 'black', fill = 'grey')+
  labs(title="Histogram of Average Wage", subtitle = "code 862300")+
  theme_classic()

dev.copy(png,'Histogram.png', width = 780)
dev.off()

#2


Top5Imones <-  R %>% group_by(name) %>% slice_max(avgWage, n=1) %>% ungroup() %>% top_n(avgWage, n=5) 
Top5p <- Top5Imones %>% select(name)

duomenys_grafikui<- R %>% filter(name %in% Top5p$name)

duomenys_grafikui%>% ggplot(aes(x=month, y= avgWage, col=name)) +
  geom_line(size =1)+
  geom_point() +theme_classic()+
  labs(title = "Top 5 average wages through the year 2020")

dev.copy(png,'Line.png', width = 780)
dev.off()

#3
Top5Insured <- duomenys_grafikui %>% group_by(name) %>%slice_max(numInsured, n=1) %>%
  top_n(numInsured, 1) %>% distinct(name, numInsured) 


Top5Insured %>% ggplot(aes(x=reorder(name, -numInsured), y=numInsured, fill = name))+
  geom_col()+
  labs(x="Company", y="Insured", title="Number of insured employees")+
  theme_classic()+
  scale_x_discrete(guide = guide_axis(n.dodge = 5))

dev.copy(png,'col.png', width = 780)
dev.off()

