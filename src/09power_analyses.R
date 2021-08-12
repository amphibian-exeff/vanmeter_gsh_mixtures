library(WebPower)

## EXAMPLES
########## Chapter 6. two-way and three-way ANOVA  #############
## Main effect of two-way ANOVA
wp.kanova(n=120, ndf=2, f=0.2, alph=0.05, ng=6)

## Interaction effect of two-way ANOVA
wp.kanova(n=120, ndf=2, f=0.4, alph=0.05, ng=6)

## Interaction effect of three-way ANOVA
wp.kanova(n=360, ndf=4, f=0.3, alph=0.05, ng=18)




wp.kanova(n=24, ndf=2, f=0.2, alph=0.05, ng=4)
# n ndf ddf   f ng alpha     power
# 24   2  20 0.2  4  0.05 0.1175833