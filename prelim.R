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

# characteristics of subjects
table(data$source)
table(data$behavior)
table(data$nettype)
table(data$district)
table(data$work)
summary(data$insecticide)
table(data$malaria)

# remove outlier
data = data[(data$insecticide < 424.01),]
data = data[(data$health != 37),]
data$district = droplevels(data$district, exclude = "9Moon")
data = data[(data$stress>0),]

# histograms
#par(mfrow = c(1, 3))
hist(data$stress)
hist(data$insecticide, breaks = seq(0,350, by=50))
hist(data$health)
par(mfrow = c(1, 1))

# plot of contingency table
mosaicplot(table(data$source, data$malaria),
           color = c("Red", "Blue"),
           xlab="Source",
           ylab="Malaria")

mosaicplot(table(data$behavior, data$malaria),
           color = c("Red", "Blue"),
           xlab="Behavior",
           ylab="Malaria")

mosaicplot(table(data$nettype, data$malaria),
           color = c("Red", "Blue"),
           xlab="Net Type",
           ylab="Malaria")

mosaicplot(table(data$district, data$malaria),
           color = c("Red", "Blue"),
           xlab="District",
           ylab="Malaria")

mosaicplot(table(data$work, data$malaria),
           color = c("Red", "Blue"),
           xlab="Work",
           ylab="Malaria")
           
mosaicplot(table(data$health, data$malaria),
           color = c("Red", "Blue"),
           xlab="Health",
           ylab="Malaria")

#chisq testing for categorical variables
chisq.test(table(data$malaria, data$source))
chisq.test(table(data$malaria, data$behavior))
chisq.test(table(data$malaria, data$nettype))
chisq.test(table(data$malaria, data$district))
chisq.test(table(data$malaria, data$work))

#Stress
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

#insecticide
insecticide.frac = factor(cut(data$insecticide,breaks=seq(0,350, by=50))) 
table(insecticide.frac)
insecticidedata <- data.frame(data, insecticide.frac)
e.probsins= tapply(data$malaria, insecticide.frac, mean) 
e.probsins
round(e.probsins,2)
e.logitsins <- log(e.probsins/(1-e.probsins))
plot(seq(25, 325, by=50), e.probsins, xlab = "Insecticide", col = "blue", pch = 16,main= "Empirical probability for insecticide")
plot(seq(25, 325, by=50), e.logitsins, xlab="Insecticide", col = "red", pch = 16,main= "Logit for insecticide")
