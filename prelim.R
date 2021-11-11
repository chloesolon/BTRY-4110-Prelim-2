# packages
library(pROC)

# import data
fdata = read.csv("malaria.csv")
head(fdata)

# remove Nid column
data = fdata[,-1]
head(data)

# check for null values
sum(is.na(data))

# change categorical variables using factor
data$source = factor(data$source)
data$behavior = factor(data$behavior)
data$nettype = factor(data$nettype)
data$district = factor(data$district)
data$work = factor(data$work)
data$health = factor(data$health)

# characteristics of subjects
table(data$source)
table(data$behavior)
table(data$nettype)
table(data$district)
table(data$work)
table(data$health)
summary(data$insecticide)
table(data$malaria)
table(data$health)

# remove outlier
data = data[(data$insecticide < 424.01),]
data = data[(data$health != 37),]
data = data[(data$district != "9Moon"),]
data$district = droplevels(data$district, exclude = "9Moon")
data = data[(data$stress>0),]

# check for null values after removing outliers
sum(is.na(data))

# histograms
#par(mfrow = c(1, 3))
hist(data$stress)
hist(data$insecticide, breaks = seq(0,350, by=50))
hist(data$health)
par(mfrow = c(1, 1))

# plot of contingency table
par(mfrow = c(2, 3))
mosaicplot(table(data$source, data$malaria),
           color = c("Red", "Blue"),
           xlab="Source",
           ylab="Malaria",
           main="Malaria vs Source",
           cex.axis = 0.8)

mosaicplot(table(data$behavior, data$malaria),
           color = c("Red", "Blue"),
           xlab="Behavior",
           ylab="Malaria",
           main="Malaria vs Behavior",
           cex.axis = 0.8)

mosaicplot(table(data$nettype, data$malaria),
           color = c("Red", "Blue"),
           xlab="Net Type",
           ylab="Malaria",
           main = "Malaria vs Net Type",
           cex.axis = 0.8)

mosaicplot(table(data$district, data$malaria),
           color = c("Red", "Blue"),
           xlab="District",
           ylab="Malaria",
           main="Malaria vs District",
           cex.axis = 0.8)

mosaicplot(table(data$work, data$malaria),
           color = c("Red", "Blue"),
           xlab="Work",
           ylab="Malaria",
           main="Malaria vs Work",
           cex.axis = )

mosaicplot(table(data$health, data$malaria),
           color = c("Red", "Blue"),
           xlab="Health",
           ylab="Malaria",
           main="Malaria vs Health")
par(mfrow = c(1, 1))

#chisq testing for categorical variables
chisq.test(table(data$malaria, data$source))
chisq.test(table(data$malaria, data$behavior))
chisq.test(table(data$malaria, data$nettype))
chisq.test(table(data$malaria, data$district))
chisq.test(table(data$malaria, data$work))
chisq.test(table(data$malaria, data$health))

#Warning message:
#  In chisq.test(table(data$malaria, as.factor(data$health))) :
#  Chi-squared approximation may be incorrect
chisq.test(table(data$malaria, data$health), simulate.p.value = TRUE)

# LR test for continuous variable
glm.stress = glm(malaria ~ stress, data=data, family="binomial")
glm.insecticide = glm(malaria ~ insecticide, data=data, family="binomial")

pchisq(anova(glm.stress)[2,2], anova(glm.stress)[2,1], lower.tail = FALSE)
pchisq(anova(glm.insecticide)[2,2], anova(glm.insecticide)[2,1], lower.tail = FALSE)

# slicing-dicing”plot of empirical log-odds
# Stress
table(data$stress)
stress.frac = factor(cut(data$stress,breaks=seq(0,20, by=2)))
table(stress.frac)
stressdata <- data.frame(data, stress.frac)
e.probs= tapply(data$malaria, stress.frac, mean)
round(e.probs,2)
e.logits <- log(e.probs/(1-e.probs))
par(mfrow = c(1, 2))
plot(seq(1,19, by=2), e.probs, xlab = "Stress", col = "blue", pch = 16, main= "Empirical probability for stress")
plot(seq(1,19, by=2), e.logits, xlab="Stress", col = "red", pch = 16,main= "Logit for stress")

# insecticide
insecticide.frac = factor(cut(data$insecticide,breaks=seq(0,350, by=50)))
table(insecticide.frac)
insecticidedata <- data.frame(data, insecticide.frac)
e.probsins= tapply(data$malaria, insecticide.frac, mean)
e.probsins
round(e.probsins,2)
e.logitsins <- log(e.probsins/(1-e.probsins))
plot(seq(25, 325, by=50), e.probsins, xlab = "Insecticide", col = "blue", pch = 16,main= "Empirical probability for insecticide")
plot(seq(25, 325, by=50), e.logitsins, xlab="Insecticide", col = "red", pch = 16,main= "Logit for insecticide")

#######################
#### MODEL FITTING ####
#######################

# fit model with all parameters
glm.all = glm(malaria ~., data = data, family="binomial")
summary(glm.all)
pchisq(956.98-807.19, 13, lower.tail=FALSE)

# remove variables that are not statistically significant in output
# summary above
glm.reduce = glm(malaria ~. - source - health - work, data=data, family="binomial")
summary(glm.reduce)

# LR test for variables that were not statistically significant. p-value shows
# that source, health, work is not statistically significant
anova(glm.reduce, glm.all)
pchisq(3.754, 4, lower.tail=FALSE) # 0.4403207


# create intercept model for selection algorithm
glm.intercept = glm(malaria ~ 1, data=data, family="binomial")

# forward selection algorithm
glm.forward = step(glm.intercept, direction='forward', scope=formula(glm.all))
formula(glm.forward) # malaria ~ stress + district + nettype + insecticide

# backward selection algorithm
glm.backward = step(glm.all, direction='backward')
formula(glm.backward) # malaria ~ stress + insecticide + nettype + district

# both direction
glm.both = step(glm.intercept, direction='both', scope=formula(glm.all))
formula(glm.both) # malaria ~ stress + district + nettype + insecticide

# check to see if variables omitted from glm.both are not statistically
# significant. p-value shows that variables can be omitted
anova(glm.both, glm.all)
pchisq(1.8935, 4, lower.tail = FALSE) # 0.755339

# check for interaction
glm.interaction = glm(malaria ~ stress + district + nettype + insecticide + stress*district + stress*nettype + stress*insecticide + district*nettype + district*insecticide + nettype*insecticide, data = data, family = "binomial")
summary(glm.interaction)

# forward
glm.interact.forward = step(glm.both, direction='forward', scope=formula(glm.interaction))
formula(glm.interact.forward) # malaria ~ stress + district + nettype + insecticide + nettype:insecticide

# backward
glm.interact.backward = step(glm.interaction, direction='backward')
formula(glm.interact.backward) # malaria ~ stress + district + nettype + insecticide + nettype*insecticide

# both direction
glm.interact.both = step(glm.both, direction='both', scope=formula(glm.interaction))
formula(glm.interact.both) # malaria ~ stress + district + nettype + insecticide + nettype*insecticide

# LR to see if removed interaction terms are statistically significant
anova(glm.interact.both, glm.interaction)
pchisq(6.9708, 8, lower.tail = FALSE) # 0.5397864

# LR to see if interaction term nettype:insecticide cab be omitted
# seems that interaction term is not relevant
anova(glm.both, glm.interact.both)
pchisq(3.057, 1, lower.tail = FALSE) # 0.08038997

############################################
##### Anova tests for model variations #####
############################################

# all the associated variables
model.full = glm(malaria ~ nettype + district + work + stress + insecticide,
                 family= "binomial",
                 data = data)
summary(model.full)
BIC(model.full) # 868.0192
AIC(model.full) # 831.166

# remove net type
model.rem.nettype = glm(malaria ~ district + work + stress + insecticide,
                        family= "binomial",
                        data = data)
summary(model.rem.nettype)
BIC(model.rem.nettype) # 872.7548
AIC(model.rem.nettype) # 840.5082

#remove district
model.rem.district = glm(malaria ~ nettype + work + stress + insecticide,
                         family= "binomial",
                         data = data)
summary(model.rem.district)
BIC(model.rem.district) # 887.7656
AIC(model.rem.district) # 860.1257

# remove work

model.rem.work = glm(malaria ~ nettype + district + stress + insecticide,
                    family= "binomial",
                    data = data)
summary(model.rem.work)
BIC(model.rem.work) # 857.3503
AIC(model.rem.work) # 829.7104

#remove stress
model.rem.stress = glm(malaria ~ nettype + district + work + insecticide,
                     family= "binomial",
                     data = data)
summary(model.rem.stress)
BIC(model.rem.stress) # 933.7979
AIC(model.rem.stress) # 901.5514

#remove insecticide
model.rem.stress = glm(malaria ~ nettype + district + work + stress,
                       family= "binomial",
                       data = data)
summary(model.rem.stress)
BIC(model.rem.stress) # 866.3756
AIC(model.rem.stress) # 834.129

# From the outs above, the model with the best AIC is the
# model with netype, district, stress, and insecticide as variables.

# Adding interaction to best fit model

# nettype*insecticide
summary(glm(data$malaria~data$nettype+ data$district + data$stress + data$insecticide+ data$nettype*data$insecticide, family= "binomial"))
AIC(glm(data$malaria~data$nettype+ data$district + data$stress + data$insecticide+ data$nettype:data$insecticide, family= "binomial")) # 828.6534

# stress*work
summary(glm(data$malaria~data$nettype+ data$district + data$stress+ data$work + data$insecticide + data$stress:data$work, family= "binomial"))
AIC(glm(data$malaria~data$nettype+ data$district + data$stress+ data$insecticide + + data$work + data$stress:data$work, family= "binomial")) # 834.3218

# take away district from best AIC model
summary(glm(data$malaria~data$nettype + data$stress + data$insecticide+ data$nettype:data$insecticide, family= "binomial"))
AIC(glm(data$malaria~data$nettype + data$stress + data$insecticide+ data$nettype:data$insecticide, family= "binomial")) # 858.2966

# Parameter estimates for best AIC model
best.fit<-glm(malaria ~ nettype + district + stress + insecticide + nettype*insecticide,
              family= "binomial",
              data = data)
summary(best.fit)

#OR's
oddsratios<-exp(best.fit$coefficients); oddsratios

#confidence intervals for OR's
exp(confint(best.fit))

# classification table / confusion matrix
best.fitted = best.fit$fitted.values
best.predict = ifelse(best.fit$fitted.values >= 0.5, 1, 0)
table(data$malaria, best.predict)

# goodness of fit
anova(glm.intercept, best.fit)
pchisq(142.32, 6, lower.tail = FALSE) # 3.245272e-28

# roc curve
plot.roc(data$malaria, best.fit$fitted.values, print.auc=TRUE, quiet=TRUE,
         main = "ROC Curve for Fitted Model")
auc(data$malaria, best.fit$fitted.values) # 0.756


p.success = tapply(data$malaria, factor(cut(data$stress, seq(0,20, by=2))), mean)
new1 = data.frame(stress = 0:20, district="1North", nettype = "TypeA", insecticide = 140)
pred.dist1 = predict(best.fit, newdata = new1, type="response")
prob.dist1 = exp(pred.dist1)/(1+exp(pred.dist1))

new2 = data.frame(stress = 0:20, district="2East", nettype = "TypeA", insecticide = 140)
pred.dist2 = predict(best.fit, newdata = new2, type="response")
prob.dist2 = exp(pred.dist2)/(1+exp(pred.dist2))

new3 = data.frame(stress = 0:20, district="3South", nettype = "TypeA", insecticide = 140)
pred.dist3 = predict(best.fit, newdata = new3, type="response")
prob.dist3 = exp(pred.dist3)/(1+exp(pred.dist3))


plot(x=seq(1,19, by=2), y=p.success, type="n", xlim=c(0, 20),ylim=c(0.4,0.8),xlab="Stress", ylab="Probability of malaria", main="Predicted probability of malaria based on stress and district")
lines(0:20, prob.dist1, col="red", lwd=1.5)
lines(0:20, prob.dist2, col="blue", lwd=1.5)
lines(0:20, prob.dist3, col="green", lwd=1.5)
