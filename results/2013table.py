#!/usr/bin/python

import sys
import matplotlib.pyplot as plt 
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

pdR  = pd.read_csv("/Users/hvkoops/repos/aceeval/results/bb13/root/Billboard2013_root.csv" , sep='\t', header=0, dtype=np.float64)
pdMM = pd.read_csv("/Users/hvkoops/repos/aceeval/results/bb13/mm/Billboard2013_MM.csv"   , sep='\t', header=0, dtype=np.float64)

# Rpvals   = np.array([stats.wilcoxon(pdR.ix[:,i], pdR.ix[:,data.shape[1]])[1] for i in np.arange(1,pdR.shape[1])])
Rmedians = [str(np.median(pdR.ix[:,i])) for i in np.arange(1,pdR.shape[1])]
Rstds    = [str(np.std(pdR.ix[:,i])) for i in np.arange(1,pdR.shape[1])]
Rmeans   = [np.mean(pdR.ix[:,i]) for i in np.arange(1,pdR.shape[1])]

# MMpvals   = np.array([stats.wilcoxon(pd01.ix[:,i], pd01.ix[:,data.shape[1]])[1] for i in np.arange(1,data.shape[1])])
MMmedians = [np.median(pdMM.ix[:,i]) for i in np.arange(1,pdMM.shape[1])]
MMstds    = [np.std(pdMM.ix[:,i]) for i in np.arange(1,pdMM.shape[1])]
MMmeans   = [np.mean(pdMM.ix[:,i]) for i in np.arange(1,pdMM.shape[1])]

print "\\begin{table*}"
print "\\resizebox{\\textwidth}{!}{%"
print "\centering"
print "\\begin{tabular}{lllllllllllllll}"

rmEstd = zip(Rmedians, Rstds)
rmlist = [(a[0] + str(' \\\\ ') + str('\emph{(') + a[1] + '}) ' + str('& ')) for a in rmEstd]
print "Root & " + reduce(lambda a,b: a+b, rmlist, "")[:-2]

mmEstd = zip(MMmedians, MMstds)
mmlist = [(a[0] + str(' \\\\ ') + str('\emph{(') + a[1] + '}) ' + str('& ')) for a in rmEstd]
print "MajMin & " + reduce(lambda a,b: a+b, mmlist, "")[:-2]


