library(tidyverse)

npix <- read_csv('data/npix.csv')

frac <- npix |>
  group_by(name_lwr) |>
  mutate(tot = sum(n_pix)) |>
  ungroup() |>
  mutate(fraction = n_pix/tot) |>
  group_by(name_lwr) |>
  summarise(mx = max(fraction)) |>
  ungroup() |>
  arrange(desc(mx)) 

over_75 <- read_csv('data/npix.csv') |>
  group_by(name_lwr) |>
  mutate(tot = sum(n_pix)) |>
  ungroup() |>
  mutate(fraction = n_pix/tot) |>
  group_by(name_lwr) |>
  filter(fraction == max(fraction)) |>
  ungroup() |>
  arrange(desc(fraction)) |> 
  filter(fraction > .75) |>
  dplyr::select(aoi = name_lwr)

frac|>
  print(n=190)

frac |> nrow()

# 43% of countries had only one KG type
(nrow(filter(frac, mx ==1)))/190

# 56% of countries 90% one KG type
(nrow(filter(frac, mx >.9)))/190

# 69% of countries had only one KG type
(nrow(filter(frac, mx >=0.8)))/190 #68%
(nrow(filter(frac, mx >=0.7)))/190 #76%
(nrow(filter(frac, mx >=0.6)))/190 #83%
#92%

df <- data.frame(x=NA, y=NA)
counter <- 1
for(i in c(0.3, 0.4, .5, .6, .7, .8, .9, 1)){

  df[counter, 1] <- i
  df[counter, 2] <- nrow(filter(frac, mx >= i))/190 
  counter <- counter +1
}
write_csv(df, 'data/npix_plotdf.csv')

ggplot(df, aes(y=x*100, x=y*100)) +
  geom_line() +
  scale_y_continuous(limits = c(0,100)) +
  xlab('% of Countries') +
  ylab('% of Area Occupied by\nLargest KG class') +
  theme_bw() +
  geom_hline(yintercept = 75, lty=3)+
  geom_vline(xintercept = 72.5, lty=3)



