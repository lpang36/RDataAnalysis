#load dataset
library(ggplot2)
data(diamonds)

#price vs x
ggplot(aes(x=x,y=price),data=diamonds)+geom_point()
cor.test(x=diamonds$x,y=diamonds$price,method="spearman")
cor.test(x=diamonds$y,y=diamonds$price,method="spearman")
cor.test(x=diamonds$z,y=diamonds$price,method="spearman")

#price vs depth
ggplot(aes(x=depth,y=price),data=diamonds)+geom_point(alpha=0.01)+
  scale_x_continuous(limits=c(56,68),breaks=seq(56,68,2))
cor.test(x=diamonds$depth,y=diamonds$price,method="spearman")

#price vs carat
ggplot(aes(x=carat,y=price),data=diamonds)+geom_point()+
  xlim(0,quantile(diamonds$carat,0.99))+
  ylim(0,quantile(diamonds$price,0.99))

#price vs volume
ggplot(aes(x=x*y*z,y=price),data=diamonds)+geom_point()
diamonds$volume = diamonds$x*diamonds$y*diamonds$z
volume_set <- subset(diamonds,volume!=0&volume<800)
cor.test(x=volume_set$volume,y=volume_set$price,method="spearman")
ggplot(aes(x=volume,y=price),data=volume_set)+geom_point(alpha=0.01)+
  geom_smooth()+xlim(0,quantile(volume_set$volume,0.99))+
  ylim(0,quantile(volume_set$price,0.99))

#price vs clarity
library(dplyr)
clarityGroups <- group_by(diamonds,clarity)
diamondsByClarity <- summarise(clarityGroups,
  mean_price = mean(price),
  median_price = median(price),
  min_price = min(price),
  max_price = max(price),
  n = n()
)
head(diamondsByClarity)

#price vs color
colorGroups <- group_by(diamonds,color)
diamondsByColor <- summarise(colorGroups,
  mean_price = mean(price),
  median_price = median(price),
  min_price = min(price),
  max_price = max(price),
  n = n()
)
head(diamondsByColor)

#bar charts
p1 <- ggplot(aes(x=clarity,y=mean_price),data=diamondsByClarity)+geom_bar(stat='identity')
p2 <- ggplot(aes(x=color,y=mean_price),data=diamondsByColor)+geom_bar(stat='identity')
grid.arrange(p1,p2,ncol=1)