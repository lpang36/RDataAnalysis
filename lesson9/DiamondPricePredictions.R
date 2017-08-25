#load dataset
library(ggplot2)
library(reshape2)
library(dplyr)
library(GGally)
library(scales)
library(memisc)
library(lattice)
library(MASS)
library(car)
library(ggplot2)
data(diamonds)

#price vs carat scatter
ggplot(aes(x=carat,y=price),data=diamonds)+geom_point()+
  xlim(0,quantile(diamonds$carat,0.99))+
  ylim(0,quantile(diamonds$price,0.99))

#ggpairs
set.seed(2222)
diamond_samp <- diamonds[sample(1:length(diamonds$price),10000),]
ggpairs(diamond_samp,
  lower = list(continuous = wrap("points", shape = I('.'))),
  upper = list(combo = wrap("box", outlier.shape = I('.'))))

#two price histograms
library(gridExtra)
plot1 <- qplot(x=price,data=diamonds) + 
  ggtitle('Price')
plot2 <- qplot(x=price,data=diamonds) + scale_x_log10() +
  ggtitle('Price (log10)')
grid.arrange(plot1,plot2,ncol=2)

#cuberoot transformation
cuberoot_trans <- function() trans_new('cuberoot',
                                       transform = function(x) x^(1/3),
                                       inverse = function(x) x^3
                                       )

#scatter transformation
ggplot(aes(carat, price), data = diamonds) + 
  geom_point(alpha=0.1,position='jitter',size=1) + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat')

#with carat, clarity, color, cut
ggplot(aes(x = carat, y = price, color=clarity), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Clarity', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Clarity')
ggplot(aes(x = carat, y = price, color=cut), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Cut', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Cut')
ggplot(aes(x = carat, y = price, color=color), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Color', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Color')

#linear model
m1 <- lm(I(log(price)) ~ I(carat^(1/3)), data=diamonds)
m2 <- update(m1, ~ . +carat)
m3 <- update(m1, ~ . +color)
m4 <- update(m1, ~ . +cut)
m5 <- update(m1, ~ . +clarity)
mtable(m1,m2,m3,m4,m5)

#predictions (lol i need a better version of r)