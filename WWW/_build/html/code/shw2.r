#Reliability calculations for HW-1, #4
#One shd feel free to edit this script to explore one's own ideas for constructing a 'good' test.
#This script produces much output that shd be omitted from a write-up of HW solutions, so use your discretion!
#Caveat: As fas as I can see, the alpha values produced by score.items() below are correct.
#However, score.items() gives an error when reporting item-total correls for items that have been reverse-coded.
#The error is that an unnecessary minus sign is placed before the correls.  
#Thanks to the students and TAs of Psy 253 for pointing this out!

library("psych")
sink("rhw1.4.r")

cat('\n', '1. Initial checks of data', '\n','\n')

d1 = read.table('hw1.4.txt')
#print(head(d1))		#check that data are read correctly

#Check item means, sd, min, max to see if any coding errors, etc
res1 = describe(d1, na.rm=T)
#print(res1)

#Examine corr matrix to see if any items shd be reverse coded, or which items are uncorrel with rest
res3 = corr.test(d1)
print(res3)


#Next, combine items into various scales, possibly with overlapping items.
#Often our data file contains items from many different constructs, e.g., anxiety, extraversion, neuroticism.
#Use the make.keys() function to do this. First, create a list of vectors, 1 vector for each construct. 
#Concatenate, using c(), the item numbers that are relevant to the construct, putting a '-' before an item number
#if that item is reverse coded. 
#Of much value is the fact that score.items() gives the attenuated (or raw) correlations, 
#as well as the disattenuated correls among the various scales.

#Should we reverse code only item #7, or all of items #7, 8 & 9? Compare alpha in the 2 cases,
#using the score.items(keys, d1) function. 

cat('\n', '1. Compare alpha & item-total correls for (i) raw data, (ii) reverse code item 7, (iii) reverse code items 7, 8, 9', '\n','\n')

#Combine all 9 scores into 1 scale, without reverse coding any items.
key1.list = list(all=c(1:9))
keys1 = make.keys(9, key1.list)
res1 = score.items(keys1, d1)
print(res1$item.cor)			#raw or uncorrected item-total correls
print(res1$alpha)				#Cronbach's alpha

#Combine all 9 scores into 1 scale, reverse coding item #7.
key2.list = list(all=c(1:6, -7, 8,9))
keys2 = make.keys(9, key2.list)
res2 = score.items(keys2, d1)
print(res2$item.cor)
print(res2$alpha)

#Combine all 9 scores into 1 scale, reverse coding items #7, 8 & 9.
key3.list = list(all=c(1:6, -7,-8,-9))
keys3 = make.keys(9, key3.list)
res3 = score.items(keys3, d1)
print(res3$item.cor)
print(res3$alpha)

cat('\n', '2. Compute alpha for 3 different constructs', '\n','\n')

keys.list = list(construct0 = c(1:6,-7,-8,-9), first = 1:4, last=7:9)
keys4 = make.keys(9, keys.list, item.labels=colnames(d1))
res4 = score.items(keys4, d1)
print(res4$item.cor)
print(res4$alpha)

#Intraclass correl is sometimes used as reliability index. There are many versions, one of which,
#'Average_fixed raters, ICC3k' is equivalent to Cronbach's alpha. Alpha is good enough for most purposes.

cat('\n', '3. Compute ICC, including alpha, on RAW data', '\n','\n')
d2 = cbind(d1[,-7],11-d1[,7])	#Reverse code item 7 for use only in ICC() below

res5 = ICC(d2)		#NOTE: We use the data matrix, d2, with item 7 reverse coded
print(res5)			#ICC3k = Cronbach's alpha when only item 7 is reverse coded

sink(file=NULL, append=FALSE)