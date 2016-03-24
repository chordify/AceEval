setwd('/Users/hvkoops/repos/aceeval/results/posthoctesting')
source('/Users/hvkoops/repos/aceeval/results/posthoctesting/friedman.test.with.post.hoc.r', chdir = TRUE)
# mydata = read.csv("bb12/Billboard2012_MajMin.csv",header = TRUE,sep = "\t")
mydata = read.csv("bb13/Billboard2013_ROOT.csv",header = TRUE,sep = "\t")
mydata$ID <- NULL
mydata$SB8 <- NULL
mydata$CF2 <- NULL
mydata$NG2 <- NULL
mydata$KO2 <- NULL
mydata$NMSD2 <- NULL
mydata$CB3 <- NULL
mydata$PP3 <- NULL
mydata$KO1 <- NULL
mydata$NMSD1 <- NULL
mydata$PP4 <- NULL
mydata$CB4 <- NULL
mydata$NG1 <- NULL
mydata$CELIING <- NULL
# print(mydata)
dimensions<-dim(mydata)

rows<-dimensions[1]
columns<-dimensions[2]
# data=c(mydata[1:rows,1],mydata[1:rows,2],mydata[1:rows,3],mydata[1:rows,4],mydata[1:rows,5],mydata[1:rows,6],mydata[1:rows,7],mydata[1:rows,8],mydata[1:rows,9],mydata[1:rows,10],mydata[1:rows,11],mydata[1:rows,12],mydata[1:rows,13],mydata[1:rows,14],mydata[1:rows,15])

data=c(mydata[1:rows,1],mydata[1:rows,2],mydata[1:rows,3])

# print ("----------ANOVA test----------")
pain=c(data)
# drug=c(  rep("SB8",rows), rep("CF2",rows), rep("NG2",rows), rep("KO2",rows),rep("NMSD2",rows),rep("CB3",rows), rep("PP3",rows), rep("KO1",rows), rep("NMSD1",rows),rep("PP4",rows),rep("CB4",rows), rep("NG1",rows), rep("RANDOM",rows), rep("MVOTE",rows),rep("FUSION",rows))
drug=c(rep("RANDOM",rows), rep("MVOTE",rows),rep("FUSION",rows))
migraine=data.frame(pain,drug)
# plot(pain ~ drug, data=migraine)
# results = aov(pain ~ drug, data=migraine)
# print(summary(results))

# print("-------- Paired t-tests -------")
# p<-pairwise.t.test(pain, drug, p.adjust="bonferroni")
# print(p)
# results = aov(pain ~ drug, data=migraine)
# t<-TukeyHSD(results, conf.level = 0.95)
# print(t)

# #Friedman test
# print ("----------Friedman test----------")
WineTasting <- data.frame(
	  Taste = c(data),
	  Wine = factor(drug),
	  Taster = factor(rep(1:rows, columns)))

# with(WineTasting , boxplot( Taste  ~ Wine )) # boxploting
friedman.test.with.post.hoc(Taste ~ Wine | Taster ,WineTasting,  to.plot.parallel = F, to.plot.boxplot = F )	# the same with our function. With post hoc, and cool plots
