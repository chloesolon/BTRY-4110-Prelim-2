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

# remove outlier
data = data[(data$insecticide != 128765),]
data = data[(data$district != "9Moon"),]
data = data[(data$stress>0),]

# histograms
par(mfrow = c(1, 3))
hist(data$stress,
     breaks = seq(min(data$stress), max(data$stress), length.out = 8))
hist(data$insecticide,
     breaks = seq(min(data$insecticide), max(data$insecticide), length.out = 8))
hist(data$health,
     breaks = seq(min(data$health), max(data$health), length.out = 8))
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
