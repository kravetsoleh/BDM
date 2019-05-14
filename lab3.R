.libPaths(c( "C:/Users/Oleh Kravets/Documents/R/win-library/3.5", "C:/Program Files/R/R-3.5.3/library"))
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

str(anscombe)
summary(anscombe)	

cor.test(anscombe$x1,anscombe$y1)

lm1 <- lm(data = anscombe, y1 ~ x1)
lm1$fitted.values
lm1$residuals

anscombe$residuals_lm1 <- lm1$residuals
ggplot(anscombe, aes(x = residuals_lm1)) + geom_dotplot(fill ="orange")

lm2 <- lm(data = anscombe, y2 ~ x2)
anscombe$residuals_lm2 <- lm2$residuals
ggplot(anscombe, aes(x = residuals_lm2)) + geom_dotplot(fill ="orange")

lm3 <- lm(data = anscombe, y3 ~ x3)
anscombe$residuals_lm3 <- lm3$residuals
ggplot(anscombe, aes(x = residuals_lm3)) + geom_dotplot(fill ="orange")

lm4 <- lm(data = anscombe, y4 ~ x4)
anscombe$residuals_lm4 <- lm2$residuals
ggplot(anscombe, aes(x = residuals_lm4)) + geom_dotplot(fill ="orange")

qqnorm(lm1$residuals, col="orange", pch=20)
qqline(lm1$residuals, col = "blue")

qqnorm(lm2$residuals, col="orange", pch=20)
qqline(lm2$residuals, col = "blue")

qqnorm(lm3$residuals, col="orange", pch=20)
qqline(lm3$residuals, col = "blue")

qqnorm(lm4$residuals, col="orange", pch=20)
qqline(lm4$residuals, col = "blue")

anscombe$fitted_lm1 <- lm1$fitted.values
ggplot(data=anscombe, aes(x=fitted_lm1, y=residuals_lm1)) + geom_point(col="orange")

anscombe$fitted_lm2 <- lm2$fitted.values
ggplot(data=anscombe, aes(x=fitted_lm2, y=residuals_lm2)) + geom_point(col="orange")

anscombe$fitted_lm3 <- lm3$fitted.values
ggplot(data=anscombe, aes(x=fitted_lm3, y=residuals_lm3)) + geom_point(col="orange")

anscombe$fitted_lm4 <- lm4$fitted.values
ggplot(data=anscombe, aes(x=fitted_lm4, y=residuals_lm4)) + geom_point(col="orange")


str(diamonds)

ggplot(data=diamonds, aes(x=carat, y=price)) +
  geom_point(col="lightblue")

ggplot(data=diamonds, aes(x=carat, y=price, col=cut)) +
  geom_point()

ggplot(data=diamonds, aes(x=carat, y=price)) + geom_point(col="lightblue") + facet_wrap(~cut)

ggplot(data=diamonds, aes(x=carat_ideal, y=price_ideal)) + geom_point(col="lightblue") + geom_smooth(method="lm", se=FALSE) + facet_wrap(~cut)

diamonds%>%
  filter(cut == 'Ideal')%>%
  ggplot(aes(x=carat, y=price)) + geom_point(col="lightblue") + ylab('carat.ideal') + geom_smooth(method="lm", se=FALSE) + facet_wrap(~cut)
  
diamonds%>%
  filter(cut == 'Fair')%>%
  ggplot(aes(x=carat, y=price)) + geom_point(col="lightblue") + ylab('carat.fair') + geom_smooth(method="lm", se=FALSE) + facet_wrap(~cut)

diamonds%>%
  filter(carat == 1)%>%
  ggplot(aes(x=price, y=depth)) + geom_point(col="orange")

