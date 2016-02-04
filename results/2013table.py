#!/usr/bin/python

import sys
# import matplotlib.pyplot as plt 
import seaborn as sns
import numpy as np
import pandas as pd
from scipy import stats

def stars(p):
   if p < 0.0001:
       return "****"
   elif (p < 0.001):
       return "***"
   elif (p < 0.01):
       return "**"
   elif (p < 0.05):
       return "*"
   else:
       return "-"

def cell(m,s):
	return(str('\\begin{tabular}[c]{@{}l@{}} \\textbf{') + str(m) + str('} \\\\ \emph{(') + str(s) + str(')}\end{tabular}') + str(' & ') ) 

def headercell(s):
	if s=='FUSION':
		return(str('\\textsc{') + str(s.lower()) + str('} & ')) 
	else:
		return(str('\\textsc{') + str(s.lower()[:-1]) + str('\\oldstylenums{') + str(s[-1]) + str('}} & ')) 

def indexcell(s):
	return(str('\\begin{tabular}[t]{@{}l@{}} ' + str(s) +  str('\\end{tabular} & ') )) 

pdR  = pd.read_csv("/Users/hvkoops/repos/aceeval/results/bb13/root/Billboard2013_root.csv" , sep='\t', header=0, dtype=np.float64)
pdMM = pd.read_csv("/Users/hvkoops/repos/aceeval/results/bb13/mm/Billboard2013_MM.csv"   , sep='\t', header=0, dtype=np.float64)
pdS = pd.read_csv("/Users/hvkoops/repos/aceeval/results/bb13/sevth/Billboard2013_Sevth.csv"   , sep='\t', header=0, dtype=np.float64)

Rmedians = [round(np.median(pdR.ix[:,i]),2) for i in np.arange(1,pdR.shape[1])]
Rstds    = [round(np.std(pdR.ix[:,i]),2) for i in np.arange(1,pdR.shape[1])]
Rmeans   = [np.mean(pdR.ix[:,i]) for i in np.arange(1,pdR.shape[1])]

MMmedians = [round(np.median(pdMM.ix[:,i]),2) for i in np.arange(1,pdMM.shape[1])]
MMstds    = [round(np.std(pdMM.ix[:,i]),2) for i in np.arange(1,pdMM.shape[1])]
MMmeans   = [np.mean(pdMM.ix[:,i]) for i in np.arange(1,pdMM.shape[1])]

Smedians = [round(np.median(pdS.ix[:,i]),2) for i in np.arange(1,pdS.shape[1])]
Sstds    = [round(np.std(pdS.ix[:,i]),2) for i in np.arange(1,pdS.shape[1])]
Smeans   = [np.mean(pdS.ix[:,i]) for i in np.arange(1,pdS.shape[1])]

print "\\begin{table*}"
print "\\resizebox{\\textwidth}{!}{%"
print "\centering"
print "\\begin{tabular}{l|llllllllllll|ll}"

header = pdR.columns.values[1:]
hlist = [headercell(a) for a in header]
print "~ & " + reduce(lambda a,b: a+b, hlist, "")[:-2] +  str('\\\\ \\hline')

rmEstd = zip(Rmedians, Rstds)
rmlist = [cell(a[0],a[1]) for a in rmEstd]
print indexcell("Root") + reduce(lambda a,b: a+b, rmlist, "")[:-2] +  str('\\\\')

mmEstd = zip(MMmedians, MMstds)
mmlist = [cell(a[0],a[1]) for a in mmEstd]
print "MajMin & " + reduce(lambda a,b: a+b, mmlist, "")[:-2]  + str('\\\\')

sEstd = zip(Smedians, Sstds)
slist = [cell(a[0],a[1]) for a in sEstd]
print "Sevth & " + reduce(lambda a,b: a+b, slist, "")[:-2]  + str('\\\\')

print str('\\end{tabular}')
print str('}')
print str('\\caption{Results of individual teams and data fusion on the \\bbIII dataset. Median accuracies in bold, variance in parentheses.}')
print str('\\label{tab:bb13}')
print str('\\end{table*}')

