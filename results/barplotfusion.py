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

# fname = "/Users/hvkoops/repos/aceeval/results/bb13/root/01rootint.csv"

def box(fname, info=False):
	pd01 = pd.read_csv(fname, sep='\t', header=0, dtype=np.float64)
	
	labels 	= pd01.columns.values
	teams  	= labels[1:-4]
	medians = pd01[teams].median(axis=1)

	rdiff	= np.median(pd01['RANDOM'] - medians)
	mvdiff  = np.median(pd01['MVOTE'] - medians)
	fdiff 	= np.median(pd01['FUSION'] - medians)
	
	rdiffstd = np.var(pd01['RANDOM'] - medians)
	mvdiffstd = np.var(pd01['MVOTE'] - medians)
	fdiffstd = np.var(pd01['FUSION'] - medians)

	alldiff = np.array([rdiff, mvdiff, fdiff])
	allsts = np.array([rdiffstd, mvdiffstd, fdiffstd])

	fig, ax = plt.subplots()
	colors = [sns.xkcd_rgb["pale red"], sns.xkcd_rgb["medium green"], sns.xkcd_rgb["denim blue"]]		
	barp = ax.bar(np.arange(0,3), alldiff, color=colors, width=0.8, align='center')
	barlabels = ['rand', 'mvote', 'fusion']
	ax.set_xticklabels(barlabels)
	
	sns.set_style("whitegrid")
	plt.ylim([np.min(alldiff)-0.5,np.max(alldiff)+0.5])
	plt.xticks(np.arange(0,3,1))
	# plt.xticks(np.arange(0,len(labels)), labels, rotation='vertical')
	
	plt.title(fname + " differences")
	plt.show()

def main(fname, info):
	box(fname, info)

if __name__ == "__main__":
	if len(sys.argv) > 2:
   		main(sys.argv[1], sys.argv[2])
   	if len(sys.argv) == 2:
   		main(sys.argv[1], False)