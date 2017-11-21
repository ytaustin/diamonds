library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggsave("cut_price.jpg")
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggsave("color_price.jpg")
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()
ggsave("clarity_price.jpg")
ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)
ggsave("carat_price.jpg")
diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))
ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins = 50)
ggsave("lcarat_lprice.jpg")
mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)
grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, colour = "red", size = 1)
ggsave("compare_carat_price.jpg")
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) + 
  geom_hex(bins = 50)
ggsave("lcarat_lresid.jpg")
ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggsave("cut_lresid.jpg")
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggsave("color_lresid.jpg")
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()
ggsave("clarity_lresid.jpg")
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)
diamonds2 <- mutate(diamonds2,  pred_lprice=predict(mod_diamond2, diamonds2))
diamonds2 <- mutate(diamonds2, pred_price=2^pred_lprice)

ggplot(data = diamonds2)+
  geom_line(mapping = aes(x=carat, y=price)) +
  geom_line(mapping = aes(x=carat, y=pred_price, color = "red"))
ggsave("line_price_compare.jpg")

ggplot(data = diamonds2)+
  geom_smooth(mapping = aes(x=carat, y=price)) +
  geom_smooth(mapping = aes(x=carat, y=pred_price, color = "red"))
ggsave("smooth_price_compare.jpg")

ggplot(data = diamonds2)+
  geom_smooth(mapping = aes(x=lcarat, y=lprice)) +
  geom_smooth(mapping = aes(x=lcarat, y=pred_lprice, color = "red"))
ggsave("smooth_lprice_compare.jpg")

ggplot(data = diamonds2)+
  geom_line(mapping = aes(x=price, y=pred_price))+
ggsave("pred_true_compare.jpg")


save.image("diamond")

  
  
