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

path = "bb12/"

r12       = path + "Billboard2012_ROOT.csv"
rpd01     = pd.read_csv(r12, sep='\t', header=0, dtype=np.float64)
rlabels 	= rpd01.columns.values
rteams  	= rlabels[1:-4]
rmedians 	= rpd01[rteams].median(axis=1)
rrdiff	 	= np.median(rpd01['RANDOM'] - rmedians)
rmvdiff  	= np.median(rpd01['MVOTE']  - rmedians)
rfdiff 	 	= np.median(rpd01['FUSION'] - rmedians)
ralldiff 	= np.array([rrdiff, rmvdiff, rfdiff])

mm12      = path + "Billboard2012_MajMin.csv"
mmpd01    = pd.read_csv(mm12, sep='\t', header=0, dtype=np.float64)
mmlabels 	= mmpd01.columns.values
mmteams  	= mmlabels[1:-4]
mmmedians = mmpd01[mmteams].median(axis=1)
mmrdiff	 	= np.median(mmpd01['RANDOM'] - mmmedians)
mmmvdiff  = np.median(mmpd01['MVOTE']  - mmmedians)
mmfdiff 	= np.median(mmpd01['FUSION'] - mmmedians)
mmalldiff = np.array([mmrdiff, mmmvdiff, mmfdiff])

s12       = path + "Billboard2012_Sevth.csv"
spd01     = pd.read_csv(s12, sep='\t', header=0, dtype=np.float64)
slabels   = spd01.columns.values
steams    = slabels[1:-4]
smedians  = spd01[rteams].median(axis=1)
srdiff    = np.median(spd01['RANDOM'] - smedians)
smvdiff   = np.median(spd01['MVOTE']  - smedians)
sfdiff    = np.median(spd01['FUSION'] - smedians)
salldiff  = np.array([srdiff, smvdiff, sfdiff])

ball = ['Root', 'MajMin', 'MajMin7']
# rdiffstd 	= np.var(pd01['RANDOM'] - medians)
# mvdiffstd 	= np.var(pd01['MVOTE'] - medians)
# fdiffstd 	= np.var(pd01['FUSION'] - medians)
# rallsts = np.array([rdiffstd, mvdiffstd, fdiffstd])

sns.set_style("whitegrid")
sns.set_context("paper")
# sns.set_style("ticks", {"xtick.major.size": 16, "ytick.major.size": 16})
colors = sns.color_palette("GnBu_d", n_colors=4)[::-1]

fig, ax 	= plt.subplots(figsize=(5,5))
plt.rc('text', usetex=True)
font      = {'family' : 'serif', 'weight' : 'bold', 'size'   : 30}
plt.rc('font', **font)
offset 		= (1. / len(ball)) - 0.05
inx 		  = np.arange(0,len(ball))-offset
rbarp 		= ax.bar(inx, ralldiff, color=colors[0], width=offset, align='center')
mmbarp 		= ax.bar(inx+offset, mmalldiff, color=colors[2], width=offset, align='center')
sbarp 		= ax.bar(inx+offset+offset, salldiff+0.3, color=colors[3], width=offset, align='center')
barlabels = ['$\\textsc{rand}$', '$\\textsc{mv}$', '$\\textsc{fusion}$']
ax.set_xticklabels(barlabels, rotation='vertical')

allvals = np.concatenate([mmalldiff, ralldiff, salldiff])
# plt.yticks(np.arange(-6,-6),fontsize=16)
fontsize = 13
ax.tick_params(labelsize=fontsize)
plt.ylim([np.floor(np.min(allvals)),np.ceil(np.max(allvals))])
plt.ylabel("Percentage accuracy difference",fontsize=fontsize)
plt.xticks(np.arange(0,len(ball),1),fontsize=fontsize)
ax.legend((rbarp[0], mmbarp[0], sbarp[0]), (ball), loc=4,fontsize=fontsize, frameon=True)
ax.yaxis.grid(False)

sns.despine(left=True)
plt.tight_layout()
# plt.title(path + " differences")
plt.show()