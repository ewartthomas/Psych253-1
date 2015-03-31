# kappa Reliability as examined with a threshold cognitive model
# Model parameters are estimated by (i) Maximum Likelihood (ML) and the bootstrap, 
# and (ii) a Bayesian approach with Uniform priors, and using Markov Chain Monte Carlo (MCMC) estimation

# Use 2-category model with Rater1 and Rater2 independent, given the stimulus category
# p0 = prob of category 1, 1-p0 = prob of cat 2, on each trial
# prob(stim is suprathreshold) = w1 for R1, = w2 for R2. If supra, stim is correctly categorised
# If sub-threshold, R guesses with P(1) = .5. Params are p0, w1 and w2.
# Data are 4 probs, p11 = P(r1 = 1 = r2), p12 = P(r1 = 1, r2 = 2), p21 = P(r1 = 2, r2 = 1), p22 = P(r1 = 2 = r2)


kappa0 = function(dat) {  # compute Cohen's kappa from dat = (n11,n12,n21,n22)
  pa1 = (dat[1] + dat[4])/sum(dat)
  pc1 = ((dat[1] + dat[2])/sum(dat)) * ((dat[1] + dat[3])/sum(dat)) + ((dat[4] + dat[2])/sum(dat)) * ((dat[4] + dat[3])/sum(dat))
  round((pa1 - pc1)/(1 - pc1), 4)
}

loglik2 = function(th, dat) {	# dat = (n11,n12,n21,n22); compute -log(likelihood) for minimisation in nlminb()
  p0 = th[1]; w1 = th[2]; w2 = th[3]
  p11 = .25*((1 - w1)*(1 - w2)) + .5*p0*(w1 + w2)
  p12 = .25*((1 - w1)*(1 + w2)) + .5*p0*(w1 - w2)
  p21 = .25*((1 - w2)*(1 + w1)) + .5*p0*(w2 - w1)
  p22 = .25*((1 - w1)*(1 - w2)) + .5*(1 - p0)*(w1 + w2)
  -(dat[1]*log(p11) + dat[2]*log(p12) + dat[3]*log(p21) + dat[4]*log(p22) )
}

param.boot1 = function(data) {	# data = (n11,n12,n21,n22); est kappa and ML estimate of theta
  rs0 = kappa0(data)			# Calculate Cohen's kappa
  rs00 = nlminb(start=c(0.5,0.5,0.5), loglik2, dat = data, lower=c(0,0,0), upper=c(1,1,1))$par  # ML theta
  rs1 = c(rs00, rs0)
  names(rs1) = c("p","w1","w2","kappa")
  round(rs1, 4)
}

param.boot2 = function(data) {	# data = (n11,n12,n21,n22); take random multinomial sample, get ML est of theta
  n0 = sum(data)
  dat1 = c(rmultinom(1, n0, data))	# take a multinomial sample from data of same size
  rs0 = kappa0(dat1)
  rs00 = nlminb(start=c(0.5,0.5,0.5), loglik2, dat = dat1, lower=c(0,0,0), upper=c(1,1,1))$par
  rs1 = c(rs00, rs0)
  names(rs1) = c("p","w1","w2","kappa")
  round(rs1, 4)
}

boot.summary1 = function(vec) {	# calculate summary statistics for bootstrapped sample of each param
  rs1 = quantile(vec, c(.025, .5, .975), na.rm = T)
  rs2 = c(mean(vec, na.rm = T), sd(vec, na.rm = T))
  rs3 = c(rs2[1], rs1[2], rs2[2], rs1[c(1,3)])
  round(rs3, 4)
}

param.boot3 = function(data, R) { # get summary statistics for original and bootstrapped samples
  rs0 = array(dim = c(R, 4))
  rs2 = list(length = 2)
  rs2[[1]] = param.boot1(data)
  
  for (i in 1:R) {			# Use of a for() loop is inefficient; better to use {boot} package
    rs0[i,] = param.boot2(data)
  }
  rs3 = apply(rs0, 2, boot.summary1)
  rs3 = t(rs3)
  rownames(rs3) = c("p", "w1", "w2", "kappa")
  colnames(rs3) = c("mean", "median", "se", "q.025", "q.975")
  rs2[[2]] = rs3
  rs2
}

## The next function calculates the log (without the minus sign!) of the unnormalised posterior probability distribution
## of the parameters (p0, w1 and w2), given the data.  In this Bayesian approach, it is assumed that the prior
## distrn of the parameters is the Uniform distrn on (0, 1).  With this simplifying assn, the posterior likelihood
## of the params, given the data, is proportional to the likelihood of the data, given the params.
## Because of this proportionality, the posterior mode falls at the same parameter values that maximise the 
## likelihood function.  That is, the ML estimates are equal to the posterior mode.  If, as is expected, the mode
## is close to the mean and median of the posterior distrn, the ML estimates shd be close to
## the (Bayesian) posterior mean.  In sum, the ML and Bayesian approaches shd yield similar results.
## Bayesian estimation is done by MCMC sampling, using the Metropolis algorithm in the package, {mcmc}

library(mcmc)

loglikmc1 = function(logth, dat) {	#dat = (n11,n12,n21,n22); use logit(th) as parameters, for convenience
  th = exp(logth)/(1 + exp(logth))
  p0 = th[1]; w1 = th[2]; w2 = th[3]
  p11 = .25*((1 - w1)*(1 - w2)) + .5*p0*(w1 + w2)
  p12 = .25*((1 - w1)*(1 + w2)) + .5*p0*(w1 - w2)
  p21 = .25*((1 - w2)*(1 + w1)) + .5*p0*(w2 - w1)
  p22 = .25*((1 - w1)*(1 - w2)) + .5*(1 - p0)*(w1 + w2)
  (dat[1]*log(p11) + dat[2]*log(p12) + dat[3]*log(p21) + dat[4]*log(p22) )
}

param.mcmc1 = function(data, R) { # Bayesian estimates using MCMC
  rs1 = metrop(loglikmc1, rep(0, 3), nbatch = R, dat = data)		# MCMC sampling with Metropolis algorithm
  rs2 = apply(rs1$batch, 2, boot.summary1)						# Extract summary statistics from MCMC chains
  rs2 = t(rs2)
  rs3 = round(exp(rs2)/(1 + exp(rs2)), 4)							# Transform from logit to probability scale
  rownames(rs3) = c("p", "w1", "w2")
  colnames(rs3) = c("mean", "median", "se", "q.025", "q.975")
  rs3
}