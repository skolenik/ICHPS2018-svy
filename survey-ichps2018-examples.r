# Analysis of Complex Health Survey Data
# ICHPS 2018
setwd("C:/Users/kolenikovs/Conferences/ICHPS.2018")

# survey data analysis functionality 
library(survey)

# reading Stata files functionality
library(foreign)

# data source
nhanes2 <- read.dta("nhanes2.dta")
# fixing a miscoded variable
tapply(nhanes2$bpsystol,nhanes2$highbp,FUN=max)
nhanes2$highbp <- as.integer( (nhanes2$bpsystol>=140) | (nhanes2$bpdiast>=90) )

# declaring the survey design object
nhanes2.svy <- svydesign(
	~psu, 				# PSU/cluster variable = psu, 
	nest=TRUE,				# cluster IDs aren't unique but nested within strata
	strata  = ~strata, 		# stratification variable = strata
	weights = ~finalwgt,		# weight variable = finalwgt
	data    = nhanes2			# data source
)

# one-way tabulation of proportions
# 1st arg = formula for the categorical variable
# 2nd arg = design object
svymean(~highbp,design=nhanes2.svy)
svymean(~as.factor(highbp),design=nhanes2.svy)
svymean(~race,nhanes2.svy)
svymean(~race + region,nhanes2.svy)

# one-way tabulation of totals
svytotal(~race,nhanes2.svy)

# extracting components of the survey estimation object
svy.prop.race <- svymean(~race,nhanes2.svy)
coef(svy.prop.race)
SE(svy.prop.race)
confint(svy.prop.race)

# two-way tabulation
svytable(~highbp+race,nhanes2.svy)
svychisq(~highbp+race,nhanes2.svy)
svychisq(~highbp+race,nhanes2.svy)
for(st in c("F",  "Chisq","Wald","adjWald","saddlepoint")) {
  cat("Statistic option = ",st,"\n")
  print(svychisq(~highbp+race,nhanes2.svy,stat=st))
}

# continuous variables
svymean(~bpsystol+bpdiast,nhanes2.svy)
vcov(svymean(~bpsystol+bpdiast,nhanes2.svy))
svycontrast(svymean(~bpsystol+bpdiast,nhanes2.svy),contrast=c(1,-1))

# distribuion summaries
bmi.cdf <- svycdf(~bmi,nhanes2.svy)
plot(bmi.cdf)
svyquantile(~bmi,nhanes2.svy,c(0.25,0.5,0.75))
svyhist(~bmi,nhanes2.svy)
# export the graph -- not sure if works
# png(filename="bmi_hist_r.png",res=72)

# t-tests
svyttest(bmi~sex,nhanes2.svy)
regTermTest(svyglm(bmi~sex,nhanes2.svy),"sex")

# linear regression
summary(nhanes2$bmi)
svyglm(bmi~race+age+sex,nhanes2.svy)

# logistic regression
(logit.highbp <- svyglm(highbp ~ race+age+sex,nhanes2.svy,family=binomial()))
logit.highbp$coef[1]
regTermTest(logit.highbp,"race")
regTermTest(logit.highbp,"race",lrt.approximation = "satterthwaite")
