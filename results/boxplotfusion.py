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
	labels = pd01.columns.values[1:]

	data    = pd01.ix[:,1:]
	pvals   = np.array([stats.wilcoxon(pd01.ix[:,i], pd01.ix[:,data.shape[1]])[1] for i in np.arange(1,data.shape[1])])
	medians = [np.median(pd01.ix[:,i]) for i in np.arange(1,pd01.shape[1])]
	stds    = [np.std(pd01.ix[:,i]) for i in np.arange(1,pd01.shape[1])]
	means   = [np.mean(pd01.ix[:,i]) for i in np.arange(1,pd01.shape[1])]

	# sns.set_style("whitegrid")
	sns.set(style="ticks", palette='Set2')
	sns.set_context("poster", font_scale=1.1, rc={"lines.linewidth": 1.1})
	ax = sns.boxplot(data=data, color="white")
	# ax = sns.stripplot(data=data, size=1, jitter=True, edgecolor="gray", palette="PuBuGn_d")
	ax.plot(np.arange(len(means)), means, 'ok')
	if info:
		for p in np.arange(len(stds)):
			if p == len(pvals):
				txt = "med="+str(medians[p])+"\n"+"std="+str(stds[p])
				ax.text(p, 102, txt, horizontalalignment='center', verticalalignment='center')
			else:
				txt = "p="+str(pvals[p])+"\n"+stars(pvals[p])+"\n"+"med="+str(medians[p])+"\n"+"std="+str(stds[p])
				ax.text(p, 104, txt, horizontalalignment='center', verticalalignment='center')
	sns.despine(offset=10, trim=True)
	plt.ylim([0,100])
	plt.yticks(np.arange(0,110,10))
	plt.xticks(np.arange(0,len(labels)), labels, rotation='vertical')
	plt.title(fname, y=1.09)
	plt.show()

def main(fname, info):
	box(fname, info)

if __name__ == "__main__":
	if len(sys.argv) > 2:
   		main(sys.argv[1], sys.argv[2])
   	if len(sys.argv) == 2:
   		main(sys.argv[1], False)