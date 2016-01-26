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


def box(fname):
	pd01 = pd.read_csv(fname, sep='\t', header=0, dtype=np.float64)
	labels = pd01.columns.values[1:]

	data    = pd01.ix[:,1:]
	pvals   = np.array([stats.wilcoxon(pd01.ix[:,i], pd01.ix[:,data.shape[1]])[1] for i in np.arange(1,data.shape[1])])
	medians = [np.median(pd01.ix[:,i]) for i in np.arange(1,pd01.shape[1])]
	stds    = [np.std(pd01.ix[:,i]) for i in np.arange(1,pd01.shape[1])]
	means   = [np.mean(pd01.ix[:,i]) for i in np.arange(1,pd01.shape[1])]

	print labels
	print "medians: " + str(medians)
	print "means: " + str(means)
	print "stds: " + str(stds)
	sns.set_style("whitegrid")
	ax = sns.boxplot(data=data, palette="PuBuGn_d")
	# ax = sns.stripplot(data=data, size=4, jitter=True, edgecolor="gray", palette="PuBuGn_d")
	for p in np.arange(len(pvals)):
		ax.text(p, 101, stars(pvals[p]), horizontalalignment='center', verticalalignment='center')
	sns.despine(offset=10, trim=True)
	plt.ylim([0,100])
	plt.yticks(np.arange(0,100,10))
	plt.xticks(np.arange(0,len(labels)), labels, rotation='vertical')
	plt.title(fname, y=1.08)
	plt.show()

def main(fname):
	box(fname)

if __name__ == "__main__":
   main(sys.argv[1])