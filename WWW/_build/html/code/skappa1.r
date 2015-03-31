#Script to do crosstabs, etc from 'kappadata1.csv'

library(irr)
sink("rkappa1.r")

d0 = read.csv("kappadata1.csv")
print(d0[1:3,])		#check data input ok

ctab12 = with(d0, table(psych1, psych2))	#psy1, psy2 crosstab
print(ctab12)

ctab34 = with(d0, table(psych4, psych3))	#psy3, psy4 crosstab
print(ctab34)

#To find kappa from raw data, use kappa2() if 2 raters, or kappam.light() if m raters
kap12 = kappam.light(d0[, c(1,2)])			#Select col1 & col2 of d0; find kappa for psy1 & psy2
print(kap12)
kap12a = kappa2(d0[, c(1,2)])				#Repeat calculation with kappa2()
print(kap12a)

#To get kappa from a crosstab (as opposed to raw data), use wkappa() from psych
library(psych)
kap12b = cohen.kappa(ctab12)						#Repeat with crosstab into wkappa() from 'psych'
print("kappa from crosstab, psych")
print(kap12b)

kap34 = kappam.light(d0[, c(3,4)])			#Select col3 & col4 of d0; find kappa for psy3 & psy4
print("Results for raters 3 & 4")
print(kap34)

print("Results for k = 4 raters")
kap1234 = kappam.light(d0[, c(1,2,3,4)])	#Select col1-col4 of d0; find kappa for psy1 thru psy4
print(kap1234)

print("Results for raters, 5, 6, 7, category A")
kapA567 = kappam.light(d0[, c(5,6,7)])		#Select A 
print(kapA567)

#Question 3
d1 = read.table('thought12.r')

ctab1 = table(d1$rater.1, d1$rater.2)
print(ctab1)
ckap1 = cohen.kappa(ctab1)				#Cohen's kappa based on 6 by 6 agreement matrix
print(ckap1)

sink(file=NULL, append=FALSE)