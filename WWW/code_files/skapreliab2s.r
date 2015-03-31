# Steve S's problem: His script detects any reference in a medical record to 'Patient doesn't take her medicine punctually'
# Consider a threshold cognitive model with Steve's script as the gold standard (GS), and R2 == R
# So the 2 raters are not equivalent - indeed there is only 1 fallible rater! How useful is kappa in this case?
# p0 = prob of category 1 = P(GS responds '1'), 1-p0 = prob GS responds '2'
# prob(stim is suprathreshold) = w for R. If supra, stim is correctly categorised
# If sub-threshold, R guesses with P('1') = g. Params are p0, w and g.
# Data are 4 probs, p11 = P(GS = 1 = r), p12 = P(GS = 1, r = 2), p21 = P(GS = 2, r = 1), p22 = P(GS = 2 = r)

# Simulate data for differing g (.3,.5,.7), put w = .8, p0 = .3
# Use Bayesian model and nlminb() to estimate params.
# Assume prior for p and w is U(0,1), prior for g is Beta with shape params (a,a); U(0,1) is same as Beta(1,1)
# Vary a (e.g., a = .1, 1, 4, 10) to see effect

joint.dist1 = function(p, w, g) {  # generate joint freqs, given parameter values
	p11 = p*(w + g*(1-w))
	p12 = p*(1-g)*(1-w)
	p21 = (1-p)*(1-w)*g
	p22 = (1-p)*(w + (1-w)*(1-g))
	round(200*c(p11, p12, p21, p22))
}

d0 = joint.dist1(.3, .8, .3) + c(2, 2, -1, -3)	# Data = exp freqs plus noise
d1 = joint.dist1(.5, .8, .4) + c(2, 2, -1, -3)	# Data = exp freqs plus noise

kappa0 = function(dat) {	# compute kappa from dat = (n11,n12,n21,n22)
	pa1 = (dat[1] + dat[4])/sum(dat)
	pc1 = ((dat[1] + dat[2])/sum(dat)) * ((dat[1] + dat[3])/sum(dat)) + ((dat[4] + dat[2])/sum(dat)) * ((dat[4] + dat[3])/sum(dat))
	(pa1 - pc1)/(1 - pc1)
}

loglik3 = function(th, dat) {	# dat = (n11,n12,n21,n22); compute -log(likelihood) for minimisation in nlminb()
	p = th[1]; w = th[2]; g = th[3]
	p11 = p*(w + g*(1-w))
	p12 = p*(1-g)*(1-w)
	p21 = (1-p)*(1-w)*g
	p22 = (1-p)*(w + (1-w)*(1-g))
	-(dat[1]*log(p11) + dat[2]*log(p12) + dat[3]*log(p21) + dat[4]*log(p22) )	# Assumes Uniform priors
}

loglik.bayes3 = function(th, dat, a) {	# dat = (n11,n12,n21,n22); compute -log(likelihood) for minimisation in nlminb()
	p = th[1]; w = th[2]; g = th[3]
	lprior.g = (a-1)*(log(g)+log(1-g)) 	# log prior for g; prior is Beta(a,a) with mean = .5 and var = 1/(8*a+4)
	## In our prior for g, larger a means greater certainty that g is close to 0.5.
	
	p11 = p*(w + g*(1-w))
	p12 = p*(1-g)*(1-w)
	p21 = (1-p)*(1-w)*g
	p22 = (1-p)*(w + (1-w)*(1-g))
	-(dat[1]*log(p11) + dat[2]*log(p12) + dat[3]*log(p21) + dat[4]*log(p22) + lprior.g)
}

param.boot1 = function(data, a0) {	# data = (n11,n12,n21,n22); est kappa and ML estimate of theta
	rs0 = kappa0(data)
	rs00 = nlminb(start=c(0.5,0.5,0.5), loglik.bayes3, dat = data, a = a0, lower=c(0,0,0), upper=c(1,1,1))$par
	rs1 = c(rs00, rs0)
	names(rs1) = c("p","w","g","kappa")
	round(rs1, 4)
}

param.boot2 = function(data, a0) {	# data = (n11,n12,n21,n22); take random multinomial sample, get ML est of theta
	n0 = sum(data)
	dat1 = c(rmultinom(1, n0, data))	# random multinomial sample
	rs0 = kappa0(dat1)
	rs00 = nlminb(start=c(0.5,0.5,0.5), loglik.bayes3, dat = dat1, a = a0, lower=c(0,0,0), upper=c(1,1,1))$par
	rs1 = c(rs00, rs0)
	names(rs1) = c("p","w","g","kappa")
	round(rs1, 4)
}

boot.summary1 = function(vec) {	# calculate statistics for bootstrapped sample of each param
	rs1 = quantile(vec, c(.025, .5, .975), na.rm = T)
	rs2 = c(mean(vec, na.rm = T), sd(vec, na.rm = T))
	rs3 = c(rs2[1], rs1[2], rs2[2], rs1[c(1,3)])
	round(rs3, 4)
}

param.boot3 = function(data, R, a0) { # compare estimates from original and bootstrapped samples
	rs0 = array(dim = c(R, 4))
	rs2 = list(length = 2)
	rs2[[1]] = param.boot1(data, a0)
	
	for (i in 1:R) {
		rs0[i,] = param.boot2(data, a0)
		}
	rs3 = apply(rs0, 2, boot.summary1)
	rs3 = t(rs3)
	rownames(rs3) = c("p", "w", "g", "kappa")
	colnames(rs3) = c("mean", "median", "se", "q.025", "q.975")
	rs2[[2]] = rs3
	rs2
}

sink('rkapreliab2s.r')

cat("\n Joint freqs, sample 1 ", d0, "\n")
cat("\n Joint freqs, sample 2 ", d1, "\n")

cat("\n Parameter estimates for p = .3, w = .8, g = .3; a = .1 \n\n") 
print(param.boot3(d0, 1000, .1))

cat("\n Parameter estimates for p = .3, w = .8, g = .3; a = 1 \n\n") 
print(param.boot3(d0, 1000, 1))

cat("\n Parameter estimates for p = .3, w = .8, g = .3; a = 4 \n\n") 
print(param.boot3(d0, 1000, 4))

cat("\n Parameter estimates for p = .3, w = .8, g = .3; a = 10 \n\n") 
print(param.boot3(d0, 1000, 10))

# Girls' joint distrn is d0, Boys' joint distrn is d1. Discuss similarities & diffs

cat("\n Parameter estimates, sample 2; a = .10 \n\n") 
print(param.boot3(d1, 1000, .10))

cat("\n Parameter estimates, sample 2; a = 10 \n\n") 
print(param.boot3(d1, 1000, 10))



sink(file = NULL, append = F)

