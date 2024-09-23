# linear mixed-effects models 

# clear everything in memory (of R)
remove(list=ls())

library(tidyverse)      # for piping, dplyr
library(lme4)          # for fitting mixed-effects models
library(lmerTest)     # for tests of significance of mixed-effects models
library(multcompView)
library(emmeans)

# Use this dataset
#browseURL("https://docs.google.com/spreadsheets/d/1pZzz337HcuQ28i7sIZu64JWAah9W77fI3hp0I9StZHA/edit?gid=0#gid=0")

dat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ3XEZfbq9FoQZ2r1d4XBzCd7Dbn4Hh4IKphhJhKqQtoRQ0cNwLxjx_U3ZOlOfpUvm_ADHgzRf6wFks/pub?gid=0&single=true&output=csv") 


# plot the results for species richness, ignoring the block effects
ggplot(data=dat,aes(x=graztreat,y=specrich,fill=graztreat)) +
  geom_boxplot()

# explore the distribution of the species richness data within each treatment
dat %>% ggplot(aes(specrich)) +
  geom_histogram(binwidth = 3) +
  facet_grid(graztreat~.) +
  ylab("frequency") +
  xlab("plant species richness per 2m2")

# test with  linear model assuming the residuals are normally distributed,
# if plant species richness is different between the treatments 
# and inspect the Q-Q plot of the residuals, and do a Shapiro test for normality 
# do a one-way anova ( 1 predictor with >2 categories)
m1 <- lm(specrich~graztreat,data=dat)
anova(m1)

#same thing
m1 <- aov(specrich~graztreat,data=dat)
anova(m1)

# use tukey test to see which groups are different (= a sort of posthoc test, is test between different groups after you have done a particular analysis)
tukey_result <- TukeyHSD(m1)
tukey_result

# I can add letters and for C - V - R they are a b b 
# means with the same letter are not significantly different
tukey_cld <- multcompView::multcompLetters(TukeyHSD(m1)$graztreat[,"p adj"])
tukey_cld

# explore the q-q plot of the residuals of the model to check normality (if its not normal, you have to transform the data)
plot(m1, which=2) 

# plot the histogram of the residuals with normal curve
g <- m1$residuals
m <- mean(g)
std <- (sd(g))
hist(g, prob = T)
curve(dnorm(x, mean=m, sd=std), col="blue", lwd=2, add=T, yaxt="n")

# just as a normal linear model 
m2 <- lm(specrich~graztreat,data=dat)
anova(m2)

# run the standard linear model as a generalized linear model, gaussian = normal distribution
m3 <- glm(specrich~graztreat, family=gaussian(link="identity"), data=dat)
anova(m3)


# test with generalized linear model assuming now a poisson distribution of residuals 
# if plant species richness is different between the treatments (log is linked with poisson)
m4 <- glm(specrich~graztreat, family=poisson(link="log"), data=dat)
anova(m4)
# is this better?
AIC(m4, m3) 
# difference is less than 1, so not really better - normal distribution is fine

# plot cover mean species richness accounting for block
ggplot(data=dat, aes(x=block, y=specrich, fill=graztreat)) +
  geom_boxplot()
# or 
ggplot(data=dat, aes(x=graztreat, y=specrich, fill=block)) +
  geom_boxplot()

# test with standard linear model (assuming normal error  distribution) 
# if plant species richness is different between the treatments 
# and account for block effects in the design, assuming it is a fixed effect
# first only a model with only block , "reference model"
m5 <- lm(specrich~graztreat + block + graztreat:block, data=dat)
m5 <- lm(specrich~graztreat*block, data=dat) #this is the same

anova(m5)

# test with  linear mixed model (assuming normal error  distribution) 
# if plant species richness is different between the treatments 
# and account for block effects in the design, assuming it is a random effect
# first fit a model with only block
m6 <- lmerTest::lmer(specrich~(1|block), data=dat)
coef(m6)
anova(m6)

# fixed slopes model, assuming the treatment effect is the same for every block
m7 <- lmerTest::lmer(specrich~graztreat + (1|block), data=dat)
coef(m7)
summary(m7)
anova(m7)

# random slopes model, assume the treatment effect is different per block
m8 <- lmerTest::lmer(specrich~graztreat + (graztreat|block), data=dat)
coef(m8)
summary(m8)
anova(m8)
AIC(m8, m7) #difference of 10, so it matters, overall treatment effect is significant, but the difference also depends on the block
anova(m8, m7) #significant the difference between the treatments depends on the block

# test with generalized linear mixed model (poisson distribution) 
# if plant species richness is different between the treatments 
# and account for block effects in the design, assuming it is a random effect
# (the correct assumption)

# first fit a model with only block
m9 <- lme4::glmer(specrich~(1|block), family = poisson(log), data=dat)

# add treatment to the model, assuming fixed slopes (i.e. treatment effect is the same for every block)
m10 <- lme4::glmer(specrich~graztreat+(1|block), family = poisson(log), data=dat)
anova(m10, m9) #significant, so grazing treatment does matter

# add treatment to the model, now  assuming random slopes and intercepts (i.e. treatment effect is the different for every block)
m11 <- lme4::glmer(specrich~graztreat+(graztreat|block), family = poisson(log), data=dat)
anova(m11,m10)

# Test if the model with grazing treatment is a better model, by comparing it to the model with block 
# only, calculation the AIC of each model and testing the difference with a Chi-square test

# -> final conclusion? what should I do? Go lmer with normal distribution is ok 

