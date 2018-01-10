** Analysis of Complex Health Survey Data
** ICHPS 2018
** Author/instructor: Stas Kolenikov skolenik@gmail.com

* data source
use nhanes2.dta, clear

* fixing a miscoded variable
table highbp, c(min bpsystol max bpsystol min bpdiast max bpdiast )
replace highbp = (bpsystol >= 140) | (bpdiast >= 90)

* declaring the survey design:
* the data set comes with settings baked in...
svyset
* ... but we can reproduce them
svyset psu [pw=finalwgt], strata(strata)

* one-way tabulation of proportions
svy : tab highbp
svy : tab race
* prop is a formal estimation command while tab is technically a summary command
svy : prop race region

* one-way tabulation of totals
svy : tab race, count
svy : tab race, count se

* extracting components of the survey estimation results
svy : tab race
matrix list e(b)
matrix list e(V)

svy : prop race
matrix list e(b)
matrix list e(V)
ereturn list

* design effects of the proportions
estat effect

* two-way tabulation
svy : tab highbp race
* with column proportions
svy : tab highbp race, col se
* with row proportions
svy : tab highbp race, row se

* estimation in subpopulation/domains
svy, subpop(if race==2): prop highbp
svy : prop highbp, over(race)

* continuous variables
svy : mean bpsystol bpdiast
* contrasts
lincom bpsystol - bpdiast

* distribuion summaries
duplicates report bmi
* since there are ties in BMI, need to process groups rather than individual observations
egen _bmi_grp_sumw = sum( finalwgt ), by( bmi )
* tag the first observation per group of identical BMIs
bysort bmi (sampl) : gen _bmi_tag = (_n==1)
* sum of weights
sum finalwgt
* create a cdf = running sum / sum of weights
gen bmi_cdf = sum( _bmi_grp_sumw*_bmi_tag ) / r(sum)
* check this is between 0 and 1
assert inrange(bmi_cdf,0,1)
* look at some examples
list *bmi* in 1/10
list *bmi* in -10/l
* restore sort order, just in case
sort sampl

* plot the CDF
label variable bmi_cdf "CDF of BMI"
line bmi_cdf bmi, sort
graph export bmi_cdf_s.png, width(2000) replace

* quantiles: user-written command
epctile bmi, svy p(25 50 75)

* histogram
hist bmi [fw=finalwgt]
graph export bmi_hist_s.png, width(2000) replace

* t-test: no obvious command, have to use regression 
svy : reg bmi i.sex, coeflegend
* 2.sex is the notation for the indicator of category sex==2
test 2.sex
* design effects 
estat effect


* regression, proper
svy : regress bmi i.race age sex
* wald test for the entirety of race dummies
testparm i.race
* design effects of the coefficients
estat effect

* logistic regression
svy : logit highbp i.race age sex
testparm i.race
* Archer-Lemeshow goodness of fit: svy generalization of Hosmer-Lemeshow
estat gof

* that's it folks
exit
