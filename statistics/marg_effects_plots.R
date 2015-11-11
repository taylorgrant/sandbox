## Proper Marginal Effects Plots
## 
## approximate style of Berry, Golder, & Milton (2012).
## data from http://mattgolder.com/files/research/jop2.zip

library(foreign)
seev <- read.dta("alexseev.dta")
seev <- na.omit(seev)
names(seev)[2] <- "xenovote"
names(seev)[7] <- "slavicshare"
names(seev)[8] <- "changenonslav"
names(seev)[10] <- "slavicshare_changenonslav"

## estimate the model (interaction = slavicshare_changenonslav)
m1 <- lm(xenovote ~ slavicshare + changenonslav + slavicshare:changenonslav + inc9903 +  
           eduhi02 + unemp02 + apt9200 + vsall03 + brdcont, data = seev)

## the original model was fitted with clustered SEs
## use package(multiwayvcov) to get vcov matrix and 
## calculate clusetered SE based upon our cluster choice
library(multiwayvcov)
m1.vcov <-cluster.vcov(m1, seev$region)

# get estimates with new SEs
est <- coeftest(m1, m1.vcov)

## Pull out data for marginal effects plots
## Note: intercept is the first coefficient
# coefficients
b2 <- est[2,1]
b3 <- est[3,1]
b10 <- est[10,1] # interaction term
# variances
varb2 <- m1.vcov[2,2]
varb3 <- m1.vcov[3,3]
varb10 <- m1.vcov[10,10]
# covariances
covb2b10 <- m1.vcov[2,10]
covb3b10 <- m1.vcov[3,10]

list(b2, b3, b10, varb2, varb3, varb10, covb2b10, covb3b10)

## MVZ sequence should over the range of var of interest
## by length of data frame
library(dplyr)
seev <- seev %>% mutate(MVZ = seq(-2, 13, length.out= 72),
                        conbx = b2+b10*MVZ,
                        consx = sqrt(varb2 + varb10*(MVZ^2)+2*covb2b10*MVZ),
                        ax = 1.96*consx,
                        upperx = conbx + ax,
                        lowerx = conbx - ax)

## plotting (ggplot doesn't easily allow two y axes, so can't 
## plot the histogram underneath. rely on rug plot instead.)
library(ggplot)
p1 <- ggplot(data = seev) + 
  scale_y_continuous("Marginal Effect of Slavic Share") + 
  scale_x_continuous(limits = c(-2,12)) +
  geom_line(aes(x = MVZ, y = conbx)) + 
  geom_line(aes(x = MVZ, y = upperx), linetype = 'dashed') + 
  geom_line(aes(x = MVZ, y = lowerx), linetype = 'dashed') + 
  geom_hline(yint = 0) + 
  geom_rug(mapping=aes(x = changenonslav)) + 
  xlab(expression(paste(Delta, "-non Slavic Share", sep = " "))) +
  ggtitle("Marginal Effect of Slavic Share on Xenophobic Voting") + theme_bw()
p1


