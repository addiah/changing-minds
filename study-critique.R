## Gay marriage study revisited
##
## The original study was retracted due to fraudulent data, allegedly
## using and falsifying data from a previous survey, rather than
## collecting original data for the study as the paper claimed

ccap.data <- read.csv(file.choose()) # the survey data they allegedly used
gay.reshaped <- read.csv(file.choose()) # a cross comparison with the CCAP data
View(ccap.data)
## The data from the gay study was reshaped to match criteria for the feeling
## thermometer which is on a scale of 0-100, that is what the CCAP data used

## Scholars thought the correlation between control group responses between
## each wave was suspiciously high - we check that here

cor(gay.reshaped$therm1[gay.reshaped$treatment == "No Contact" &
  gay.reshaped$study == 1],
gay.reshaped$therm2[gay.reshaped$treatment == "No Contact" &
  gay.reshaped$study == 1],
use = "complete.obs"
) # 0.9975817

## The data from wave 1 and wave 2 are almost perfectly correlated, implying
## that hardly anyone changed their minds on the topic if not directly contacted
## this, of course, does not make much sense given the study also showed a
## significant increase after a prolonged absence from contact, more so than
## was demonstrated with contact

gay.s2.reshaped <- subset(gay.reshaped, subset = (gay.reshaped$study == 2 &
  gay.reshaped$treatment == "No Contact"))
gay.s2therm <- matrix(gay.s2.reshaped$therm1, byrow = FALSE)

gay.s2therm <- cbind(
  gay.s2.reshaped$therm1, gay.s2.reshaped$therm2,
  gay.s2.reshaped$therm3, gay.s2.reshaped$therm4
)
colnames(gay.s2therm) <- c("therm1", "therm2", "therm3", "therm4")

cor(gay.s2therm, use = "pairwise.complete.obs")
##           therm1    therm2    therm3    therm4
## therm1 1.0000000 0.9734449 0.9594085 0.9709017
## therm2 0.9734449 1.0000000 0.9308287 0.9436621
## therm3 0.9594085 0.9308287 1.0000000 0.9343249
## therm4 0.9709017 0.9436621 0.9343249 1.0000000


gay.sd.reshaped <- cbind(
  sd(gay.s2therm.df$therm1, na.rm = TRUE),
  sd(gay.s2therm.df$therm2, na.rm = TRUE),
  sd(gay.s2therm.df$therm3, na.rm = TRUE),
  sd(gay.s2therm.df$therm4, na.rm = TRUE)
)
colnames(gay.sd.reshaped) <- c("therm1", "therm2", "therm3", "therm4")

## Checking outliers per wave
count <- 0

for (i in gay.s2therm[, "therm1"]) {
  if (i > 3 * gay.sd.reshaped[, "therm1"]) {
    count <- count + 1
  }
}
count # there were 273 outliers in wave 1

count <- 0

for (i in gay.s2therm[, "therm2"]) {
  if (is.na(i) == FALSE & i > 3 * gay.sd.reshaped[, "therm2"]) {
    count <- count + 1
  }
}
count # there were 234 outliers in wave 2

count <- 0

for (i in gay.s2therm[, "therm3"]) {
  if (is.na(i) == FALSE & i > 3 * gay.sd.reshaped[, "therm3"]) {
    count <- count + 1
  }
}
count # there were 239 outliers in wave 3

count <- 0

for (i in gay.s2therm[, "therm4"]) {
  if (is.na(i) == FALSE & i > 3 * gay.sd.reshaped[, "therm4"]) {
    count <- count + 1
  }
}
count # there were 230 outliers in wave 4

gay.s2therm.df <- data.frame(gay.s2therm)
View(gay.s2therm.df)
plot(gay.s2therm.df)
## The data appears almost identical across all waves, there is a clear trend
## and no obvious outliers from that trend when comparing by wave, each wave
## has between 230 and 273 outliers individually


## Comparing CCAP data visually
hist(ccap.data$gaytherm, breaks = 10)
hist(gay.s2therm.df$therm1)
## Visually, they appear to follow very similar trends, with the CCAP having
## several thousand more respondents in each bracket

qqplot(ccap.data$gaytherm, gay.s2.reshaped$therm1)
abline(0, 1)
## The plots are almost perfectly along the 45 degree line, suggesting that they
## came from populations with very similar or near identical distributions of
## support for gay marriage, which would suggest that the data was collected from
## a similar location on a similar scale, even though it reportedly is not
