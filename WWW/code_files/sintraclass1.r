#Generates 'rating' data on 15 objects by a given set of 4 raters; calculates Intraclass correl, etc
library(lme4)      #Need to install the 'lme4' package
library('psych')
sink("rintraclass1.r")

#Generate data
e0 = matrix(rnorm(45), ncol=3)				#Measurement errors for 3 doctors/judges, 15 patients/items
p0 = matrix(2*rnorm(15), ncol=3, nrow=15)	#Patient random effects
d0 = round(data.frame(8 + p0 + e0), 1)		#Obs scores
rownames(d0) = paste('P',1:15,sep='')
colnames(d0) = paste('rep',1:3,sep='')
print(d0)
cat('\n')

#Rearrange data file into d1 for random effects modeling
d00 = as.matrix(d0)
d1 = data.frame(cbind(c(d00), rep(1:15,3), rep(1:3, each=15)))
colnames(d1) = c("rating","patient","replicate")
d1$patient = factor(d1$patient)
d1$replicate = factor(d1$replicate)			#Ignore 'replicate' as a factor
str(d1)

theme_set(theme_bw(base_size = 18)) 
# set up basic data, with x and y vars
p <- ggplot(d1, aes(patient, rating))
p + geom_boxplot(fill= 'mediumseagreen', color= 'darkgreen') + 
  labs(list(title = 'Patient Rating from 3 Raters', x = 'Patient', y='Rating')) +
  # plot the points
  geom_point(size=3,
             colour="black", 
             alpha = 0.5,
             position=position_jitter(width=0.025))
ggsave('~/Desktop/Calpha_smallerr.png', width = 10, height=6, dpi=300)
#print(d1)
#cat('\n')
summary(d1)

#Reverse order (D1,D2) pairs and find cor. Use ICC() to check.
x1 = c(d0[,1], d0[,2]); y1 = c(d0[,2], d0[,1])
icc0 = round(cor(x1, y1), 2)
cat('ICC12 as correl across 2n pairs = ', icc0)
cat('\n')

d01 = d0[,1:2]			#rep1, rep2 ratings
icc1 = ICC(d01)
cat('ICC12, judges fixed,  = ', icc1[[1]]$ICC[3])
#icc1[[1]]$ICC[3] should equal icc0!
cat('\n')

d01 = d0[,1:2]			#rep1, rep2 ratings
icc1 = ICC(d01)
cat('ICC12, judges random, = ', icc1[[1]]$ICC[2])
#icc1[[1]]$ICC[3] should equal icc1[[1]]$ICC[2]!
cat('\n')

d02 = d0[,c(1,3)]			#rep1, rep3 ratings
icc1a = ICC(d02)
cat('ICC13 from R function, ICC(), = ', icc1a[[1]]$ICC[3])
cat('\n')

d03 = d0[,c(2,3)]			#rep3, rep3 ratings
icc1b = ICC(d03)
cat('ICC23 from R function, ICC(), = ', icc1b[[1]]$ICC[3])
cat('\n')

icc1t = round((icc1[[1]]$ICC[3] + icc1a[[1]]$ICC[3] + icc1b[[1]]$ICC[3])/3, 2)
cat('Average ICC over all pairs of reps, = ', icc1t)
#icc1t should equal icc2, calculated immediately below
cat('\n')


icc2 = ICC(d0)			#Ratings from all 3 replicates
print(icc2)
cat("Whole sample ICC = ", icc2[[1]]$ICC[3])

cat('\n','\n')
cat('Compute ICC from fixed effects ANOVA table')
cat('ICC = (F-1)/(F+n-1)')
cat('\n')
rs0 = lm(rating ~ patient, data=d1)		#note that 'replicates' is not a factor
	#This fixed-effects model is not exactly right, but it gives the right F
print(anova(rs0))

(7.6309 - 1)/(7.6309+(3-1))

cat('\n','\n')
cat('Compute ICC from random effects ANOVA table')
cat('ICC = var(p)/[var(p)+var(resid)]')
cat('\n')
rs1 = lmer(rating ~ (1 | patient), data=d1)	
#This random-effects model is the right model. Note that 'replicates' is not a facto

print(summary(rs1))

2.34/(2.34+1.06)


sink(file=NULL,append=FALSE)
