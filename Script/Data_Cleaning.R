#Some data cleaning
data <- read.csv("Point Frame/plot_data.csv") 
test <- select(data, c('plot', 'year', 'y', 'x'))

#Certain (~13) plots have <100 points of observation - 
test2 <- unique(test) %>%
  group_by(plot, year) %>%
  summarise(obs = n())

#Plot 8A and 8B plots NO DATA for select yrs
test3 <- test2 %>%
  group_by(plot) %>%
  summarise(obs = n())

#Sometimes hit order for single entries at x, y is not == '1' (the idea is that if there's only one 'hit' it should have a value of 1?)
hit <-  select(data, c('plot', 'year', 'y', 'x', 'hit_order'))
x <- hit %>%
  group_by(plot, year, y, x) %>%
  summarise(obs = n())
x2 <- filter(x, obs == 1)
x3 <- merge(data, x2, all.y = TRUE)



