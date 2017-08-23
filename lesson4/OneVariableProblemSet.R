#load dataset
library(ggplot2)
data(diamonds)

#display basic info 
summary(diamonds)
names(diamonds)
?diamonds
NROW(diamonds)

#price histogram
summary(diamonds$price)
summary(diamonds$price<500)
summary(diamonds$price<250)
summary(diamonds$price>=15000)
price <- qplot(x = price, data = diamonds)
price + scale_x_continuous(limits=c(0,1000),breaks=seq(0,1000,50))

#price by cut
price + facet_wrap(~cut)
price + facet_wrap(~cut,scales='free_y')
by(diamonds$price,diamonds$cut,function(x) max(x))
by(diamonds$price,diamonds$cut,function(x) min(x))
by(diamonds$price,diamonds$cut,function(x) median(x))

#price per carat by cut
priceCarat <- qplot(x = price/carat, data = diamonds)
price + facet_wrap(~cut) + scale_x_log10()

#price by color box plot
price <- qplot(x = color, y = price, data = diamonds, geom = 'boxplot')
by(diamonds$price,diamonds$color=='D',function(x) summary(x))
by(diamonds$price,diamonds$color=='J',function(x) summary(x))
by(diamonds$price,diamonds$color=='D',function(x) IQR(x))
by(diamonds$price,diamonds$color=='J',function(x) IQR(x))

#price per carat by color box plot
price <- qplot(x = color, y = price/carat, data = diamonds, geom = 'boxplot')

#carat frequency polygon
price <- qplot(x = carat, data = diamonds, geom = 'freqpoly')
summary(diamonds$carat==0.1)
summary(diamonds$carat==0.3)
summary(diamonds$carat==0.8)
summary(diamonds$carat==1.01)
summary(diamonds$carat==1.6)
summary(diamonds$carat==2.0)
summary(diamonds$carat==3.0)
summary(diamonds$carat==5.0)
