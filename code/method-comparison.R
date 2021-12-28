library(tidyverse)
library(here)

compare <-read_csv(here("data","sensors", "method_comparison.csv"))

alg <- compare %>%
  filter(site=="Alegria")

bml <- compare %>%
  filter(site=="Bodega Bay")

lol <- compare %>%
  filter(site=="Lompoc Landing")

ggplot(compare, aes(x=date_time, y=p_h, group=type)) +
  geom_line(aes(color=type), size=1, alpha=0.5) +
  facet_wrap(~site)

ggsave(here("figures",  "method_comparison_sites.png"), height=20, width=40, units="cm")

geom_smooth(method="loess", span=0.2, aes(color=type)) #span 0.3 looks similar to method="gam"
