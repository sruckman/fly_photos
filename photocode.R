#Fly Photos

library(ggraptR)
library(ggplot2)
library(nlme)
library(emmeans)

pho <- read.csv("C:\\Users\\sarah\\Documents\\GitHub\\fly_photos\\Means.csv", header= T)
pho$Gen <- Gen <- as.character(pho$Gen)
for (i in 1:550){
  if (pho$Trident[i] <= 0){
    pho$Trident[i] = 0
  } 
}


fit1 <- aov(Trident ~ Gen, data = pho)
summary(fit1)

#             Df Sum Sq Mean Sq F value   Pr(>F)    
#Gen           2   3488  1743.8   9.735 0.000107 ***
#Residuals   147  26330   179.1                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Post Hoc
TukeyHSD(fit1)
#Tukey multiple comparisons of means
#95% family-wise confidence level
#Fit: aov(formula = Trident ~ Gen, data = pho)
#$Gen
#          diff       lwr        upr     p adj
#3-14 11.654294   5.316680 17.991908 0.0000737
#7-14  4.165399  -2.172215 10.503013 0.2680259
#7-3  -7.488895 -13.826509 -1.151281 0.0159967


#Graph
#ggraptR(pho)
ggplot(pho, aes(y=Trident, x=as.factor(as.numeric(Gen)))) + 
  geom_boxplot(stat="boxplot", position="dodge", alpha=0.5, width=0.2) + 
  theme_classic() + xlab("Generation") + ylab("Grey Scale Value")
#Export 5x5



fit <- lme(Trident ~ Gen, random = ~ 1|Person, data = pho)
summary(fit)

#Post Hoc Test
emmeans(fit, list(pairwise ~ Gen), adjust = "tukey")

#Graph
#ggraptR(pho)
ggplot(pho, aes(y=Trident, x=as.factor(as.numeric(Gen)))) + 
  geom_boxplot(stat="boxplot", position="dodge", alpha=0.5, width=0.2) + 
  theme_classic() + xlab("Generation") + ylab("Grey Scale Value")
