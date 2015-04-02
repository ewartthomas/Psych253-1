

 Crosstab freqs 
96 2 2 0 
 Cohens kappa -0.0204 

 Parameter estimates from (i) original data, (ii) the bootstrap 

$length
      p      w1      w2   kappa 
 1.0000  0.9600  0.9600 -0.0204 

[[2]]
         mean  median     se   q.025 q.975
p      1.0000  1.0000 0.0000  1.0000     1
w1     0.9602  0.9600 0.0274  0.9000     1
w2     0.9602  0.9600 0.0269  0.9000     1
kappa -0.0156 -0.0152 0.0116 -0.0417     0


 Parameter estimates using Bayesian model and MCMC 

     mean median     se  q.025  q.975
p  1.0000 1.0000 0.9982 0.9664 1.0000
w1 0.9651 0.9624 0.7073 0.8755 0.9944
w2 0.9712 0.9676 0.7127 0.8733 0.9956


 Bootstrap Statistics obtained with boot and boot.ci from {boot} 


 Parameter is  p 
BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
Based on 1000 bootstrap replicates

CALL : 
boot.ci(boot.out = res1, conf = 0.95, type = "perc", index = j)

Intervals : 
Level     Percentile     
95%   ( 0.0351,  0.0953 )  
Calculations and Intervals on Original Scale


 Parameter is  w1 
BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
Based on 1000 bootstrap replicates

CALL : 
boot.ci(boot.out = res1, conf = 0.95, type = "perc", index = j)

Intervals : 
Level     Percentile     
95%   ( 0.9265,  0.9969 )  
Calculations and Intervals on Original Scale


 Parameter is  w2 
BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
Based on 1000 bootstrap replicates

CALL : 
boot.ci(boot.out = res1, conf = 0.95, type = "perc", index = j)

Intervals : 
Level     Percentile     
95%   ( 0.9101,  0.9920 )  
Calculations and Intervals on Original Scale


 Parameter is  kappa 
BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
Based on 1000 bootstrap replicates

CALL : 
boot.ci(boot.out = res1, conf = 0.95, type = "perc", index = j)

Intervals : 
Level     Percentile     
95%   ( 0.5325,  0.8853 )  
Calculations and Intervals on Original Scale


 Bootstrap Statistics 

ORDINARY NONPARAMETRIC BOOTSTRAP


Call:
boot(data = dl0, statistic = param.boot0, R = 1000)


Bootstrap Statistics :
    original     bias    std. error
t1*   0.0622  0.0000598     0.01564
t2*   0.9655  0.0006227     0.01873
t3*   0.9557  0.0005923     0.02042
t4*   0.7358 -0.0015069     0.08763
