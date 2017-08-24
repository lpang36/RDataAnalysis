#load dataset
library(ggplot2)
data(diamonds)

#price histogram facet cut
ggplot(aes(x=price),data=diamonds)+facet_wrap(~color)+
  geom_histogram(aes(fill=diamonds$cut),binwidth=1000)

#price vs table scatter
ggplot(aes(y=price,x=table),data=diamonds)+
  geom_point(aes(color=diamonds$cut))

#price vs volume scatter
diamonds$volume <- diamonds$x*diamonds$y*diamonds$z
ggplot(aes(y=price,x=volume),data=diamonds)+
  geom_point(aes(color=diamonds$clarity))+
  scale_y_log10()+xlim(0,quantile(diamonds$volume,0.99))

#proportion of friendships initiated
pf <- read.delim('pseudo_facebook.tsv')
pf$prop_initiated <- pf$friendships_initiated/pf$friend_count
summary(pf$prop_initiated)

#prop initiated vs tenure smoothed
pf$year_joined <- 2014-pf$tenure/365
pf$year_joined.bucket <- cut(pf$year_joined,c(2004,2009,2011,2012,2014))
ggplot(aes(y=prop_initiated,x=tenure),data=subset(pf,is.finite(tenure)))+
  geom_smooth(aes(color=year_joined.bucket),stat='smooth',fun.y=median)
by(pf$prop_initiated[is.finite(pf$prop_initiated)],pf$year_joined.bucket[is.finite(pf$prop_initiated)],function(x) mean(x))

#price carat ratio facet color
ggplot(aes(y=price/carat,x=cut),data=diamonds)+facet_wrap(~clarity)+
  geom_jitter(aes(color=diamonds$color))
