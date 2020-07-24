## Gay Marriage Study
gay <- read.csv(file.choose())
summary(gay)
dim(gay)
View(gay)
## study - dataset contains 2 studies
## treatment - there are 5 treatment types
## wave - 1:7 for study 1 & 1, 2, 3, 7 for  study 2
## SSM - scale for support of gay marriage (5pt scale, 5 = strongly support)

## No contact study 1 waves 1 & 2
s1w1.no.contact <- subset(gay, subset = (gay$treatment == "No Contact" &
  gay$study == 1 & gay$wave == 1))
mean(s1w1.no.contact$ssm) # 3.042764

s1w2.no.contact <- subset(gay, subset = (gay$treatment == "No Contact" &
  gay$study == 1 & gay$wave == 2))
mean(s1w2.no.contact$ssm) # 3.039615

## Gay marriage script by gay canvasser 1 & 2
s1w1.gay.mscript <- subset(gay, subset = (gay$treatment == "Same-Sex Marriage Script by Gay Canvasser"
& gay$study == 1 & gay$wave == 1))
mean(s1w1.gay.mscript$ssm) # 3.025195

s1w2.gay.mscript <- subset(gay, subset = (gay$treatment == "Same-Sex Marriage Script by Gay Canvasser" &
  gay$study == 1 & gay$wave == 2))
mean(s1w2.gay.mscript$ssm) # 3.139489

## Gay marriage script by straight canvasser 1 & 2
s1w1.straight.mscript <- subset(gay, subset = (gay$treatment == "Same-Sex Marriage Script by Straight Canvasser" &
  gay$study == 1 & gay$wave == 1))
mean(s1w1.straight.mscript$ssm) # 3.09971

s1w2.straight.mscript <- subset(gay, subset = (gay$treatment == "Same-Sex Marriage Script by Straight Canvasser" &
  gay$study == 1 & gay$wave == 2))
mean(s1w2.straight.mscript$ssm) # 3.161863

## Gay recycle 1 & 2
s1w1.gay.recycle <- subset(gay, subset = (gay$treatment == "Recycling Script by Gay Canvasser" &
  gay$study == 1 & gay$wave == 1))
mean(s1w1.gay.recycle$ssm) # 3.130975

s1w2.gay.recycle <- subset(gay, subset = (gay$treatment == "Recycling Script by Gay Canvasser" &
  gay$study == 1 & gay$wave == 2))
mean(s1w2.gay.recycle$ssm) # 3.107447

## Straight recycle 1 & 2
s1w1.straight.recycle <- subset(gay, subset = (gay$treatment == "Recycling Script by Straight Canvasser" &
  gay$study == 1 & gay$wave == 1))

mean(s1w1.straight.recycle$ssm) # 3.013474

s1w2.straight.recycle <- subset(gay, subset = (gay$treatment == "Recycling Script by Straight Canvasser" &
  gay$study == 1 & gay$wave == 2))
mean(s1w2.straight.recycle$ssm) # 3.004278

## Five times as many houses received the no contact condition,
## mean SSM was very similar across all conditions at wave 1 (baseline).
##
## It appears to be randomized, but We do not have information
## on demographics of the households in this data. We cannot
## account for other factors such as education, household
## income, local politics, religion, change in ownership, etc.

## Checking whether contact alone influenced SSM


mean(s1w2.gay.recycle$ssm) - mean(s1w2.no.contact$ssm) # 0.06783225

mean(s1w2.straight.recycle$ssm) - mean(s1w2.no.contact$ssm) # -0.03533649
## It would appear that contact alone with a gay canvasser improved
## SSM scores in study 1 wave 2 using the recycling script


mean(s1w2.gay.mscript$ssm) - mean(s1w2.straight.mscript$ssm)
## So far mere exposure did not appear to work as intended with the marriage
## script; straight canvassers had a higher average increase in approval
## by .02237333 than gay canvassers in study 1 wave 2
##
## Overall, when the topic was gay marriage, straight canvassers
## appeared to have higher impact in wave 1 & 2, but when unrelated
## to gay marriage, contact with a gay canvasser worked as intended
##
## Since full supporters' SSM cannot go up, perhaps this lack of
## change is due to a sense among undecided/non-supporters that gay
## canvassers were self-serving, pushing the "Gay Agenda", and/or the
## tendency to take one's in-group more seriously than out-group
## (assuming all or most non-supporters were straight)

rm(
  s1w1.gay.mscript, s1w2.gay.mscript, s1w1.straight.mscript,
  s1w2.straight.mscript, s1w1.no.contact, s1w2.no.contact, s1w1.gay.recycle,
  s1w2.gay.recycle, s1w1.straight.recycle, s1w2.straight.recycle
)

## Setting up for further analysis
s1.gay.mscript <- subset(gay, subset = (gay$treatment == "Same-Sex Marriage Script by Gay Canvasser" &
  gay$study == 1))
mean(s1.gay.mscript$ssm) # 3.175576

s1.straight.mscript <- subset(gay, subset = (gay$treatment == "Same-Sex Marriage Script by Straight Canvasser" &
  gay$study == 1))
mean(s1.straight.mscript$ssm) # 3.165186

s1.no.contact <- subset(gay, subset = (gay$treatment == "No Contact" &
  gay$study == 1))
mean(s1.no.contact$ssm) # 3.097803

## ATE size for gay canvasser, overall
mean(s1.gay.mscript$ssm) - mean(s1.no.contact$ssm)
## 0.07777261

## Calculating SD assuming independence
sqrt(var(s1.gay.mscript$ssm) + var(s1.no.contact$ssm)) # 2.286781

## Briefly checking normality visually to see
## if confidence intervals would be valid
hist(s1.gay.mscript$ssm)
hist(s1.no.contact$ssm)
hist(s1.straight.mscript$ssm)
## Data appears approximately symmetric bimodal

## Study one, all waves
tapply(s1.gay.mscript$ssm, s1.gay.mscript$wave, mean) -
  tapply(s1.no.contact$ssm, s1.no.contact$wave, mean)
##      1           2           3           4
## -0.01756893  0.09987463  0.08136612  0.09262577
##     5           6           7
## 0.14789543  0.08643548  0.05936835

tapply(s1.straight.mscript$ssm, s1.straight.mscript$wave, mean) -
  tapply(s1.no.contact$ssm, s1.no.contact$wave, mean)
##      1           2           3           4
## 0.05694517  0.12224797  0.06040800  0.08719659
##     5           6           7
## 0.09855523  0.06351524 -0.04253721
##
## It would appear the initial gains of the straight
## canvasser diminishes overtime

## No contact study 2, waves 1 & 2
s2w1.no.contact <- subset(gay, subset = (gay$treatment == "No Contact" &
  gay$study == 2 & gay$wave == 1))
mean(s2w1.no.contact$ssm) # 2.970075

s2w2.no.contact <- subset(gay, subset = (gay$treatment == "No Contact" &
  gay$study == 2 & gay$wave == 2))
mean(s2w2.no.contact$ssm) # 2.9923

## Gay marriage script by gay canvasser study 2, wave 1 & 2
s2w1.gay.mscript <- subset(gay, subset = (gay$treatment == "Same-Sex Marriage Script by Gay Canvasser" &
  gay$study == 2 & gay$wave == 1))
mean(s2w1.gay.mscript$ssm) # 2.971729

s2w2.gay.mscript <- subset(gay, subset = (gay$treatment == "Same-Sex Marriage Script by Gay Canvasser" &
  gay$study == 2 & gay$wave == 2))
mean(s2w2.gay.mscript$ssm) # 3.116194
## Baseline randomization of SSM scores appears fine
## Study 2 did not have straight canvassers, only gay canvassers

mean(s2w2.gay.mscript$ssm) - mean(s2w2.no.contact$ssm) # 0.1238937
## Contact about a gay marriage script by a gay canvasser
## improved SSM score slightly more than study 1 wave 2,
## but both were positively impacted

rm(s2w1.gay.mscript, s2w1.no.contact, s2w2.gay.mscript, s2w2.no.contact)

## Setting up study 2
s2.gay.mscript <- subset(gay, subset = (gay$treatment == "Same-Sex Marriage Script by Gay Canvasser" &
  gay$study == 2))
mean(s2.gay.mscript$ssm) # 3.106866

s2.no.contact <- subset(gay, subset = (gay$treatment == "No Contact" &
  gay$study == 2))
mean(s2.no.contact$ssm) # 2.978031

tapply(s2.gay.mscript$ssm, s2.gay.mscript$wave, mean) -
  tapply(s2.no.contact$ssm, s2.no.contact$wave, mean)
##      1           2           3           4           7
## 0.001653782 0.123893673 0.150420627 0.125173822 0.307062649
## 5th & 6th waves did not exist
## Wave 7 was called such because it occurred at the same
## time from baseline as in study 1 (1 year)
##
## Despite having fewer contacts and more time between the last
## contact, wave 7 in study 2 shows a much larger difference than
## wave 7 in study one. Could there be overlooked influences?
## This study was published in 2014 - gay marriage was made
## legal federally in 2015, maybe there was more national dialogue
## which helped shape opinions? The study was conducted in LA county,
## gay marriage had been legal locally since 2008, so local politics
## may not have been as much of a factor
