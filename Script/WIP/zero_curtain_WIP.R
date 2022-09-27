#Zero curtain ---

dat <- filter(ground, plot == "NB01_CTL_Dry")
plot(dat$date, dat$t_ground, type = 'l')
plot(test$date, test$curtain, type = 'p')

ggplot(test)+
  geom_line(aes(x = date, y = t_ground))+
  geom_point(aes(x = date, y = curtain, color = 'red'))+
  geom_hline(yintercept = c(-0.75, 0.75), color = 'grey')

test <- dat %>%
  mutate(curtain = if_else(t_ground > -0.75 & t_ground < 0.75, 1, -5)) %>%
  mutate(rolling_curtain = zoo::rollsumr(curtain, k = 3, fill = NA, align = 'left'))
