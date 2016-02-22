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

year      = str(12)
path      = "bb" + year + "/"

r12       = path + "Billboard20" + year + "_ROOT.csv"
rpd01     = pd.read_csv(r12, sep='\t', header=0, dtype=np.float64)
rlabels 	= rpd01.columns.values
rteams  	= rlabels[1:-4]
rmaxmed   = rpd01[rteams].median().max()
rrdiff	 	= np.median(rpd01['RANDOM']) - rmaxmed
rmvdiff  	= np.median(rpd01['MVOTE'])  - rmaxmed
rfdiff 	 	= np.median(rpd01['FUSION']) - rmaxmed
ralldiff 	= np.array([rrdiff, rmvdiff, rfdiff])

mm12      = path + "Billboard20" + year + "_MajMin.csv"
mmpd01    = pd.read_csv(mm12, sep='\t', header=0, dtype=np.float64)
mmlabels 	= mmpd01.columns.values
mmteams  	= mmlabels[1:-4]
mmaxmed   = mmpd01[mmteams].median().max()
mmrdiff	 	= np.median(mmpd01['RANDOM']) - mmaxmed
mmmvdiff  = np.median(mmpd01['MVOTE'])  - mmaxmed
mmfdiff 	= np.median(mmpd01['FUSION']) - mmaxmed
mmalldiff = np.array([mmrdiff, mmmvdiff, mmfdiff])

s12       = path + "Billboard20" + year + "_Sevth.csv"
spd01     = pd.read_csv(s12, sep='\t', header=0, dtype=np.float64)
slabels   = spd01.columns.values
steams    = slabels[1:-4]
smaxmed   = spd01[mmteams].median().max()
srdiff    = np.median(spd01['RANDOM']) - smaxmed
svdiff    = np.median(spd01['MVOTE'])  - smaxmed
sfdiff    = np.median(spd01['FUSION']) - smaxmed
salldiff  = np.array([srdiff, svdiff, sfdiff])

# statistics

ball = ['Root', 'MajMin', 'MajMin7']

sns.set_style("whitegrid")
sns.set_context("paper")
# sns.set_style("ticks", {"xtick.major.size": 16, "ytick.major.size": 16})
colors = sns.color_palette("GnBu_d", n_colors=4)[::-1]

fig, ax 	= plt.subplots(figsize=(5,5))
plt.rc('text', usetex=True)
font = {'family' : 'serif', 'weight' : 'bold', 'size'   : 30}
plt.rc('font', **font)
offset 		= (1. / len(ball)) - 0.05
inx 		  = np.arange(0,len(ball))-offset
rbarp 		= ax.bar(inx, ralldiff, color=colors[0], width=offset, align='center')
mmbarp 		= ax.bar(inx+offset, mmalldiff, color=colors[2], width=offset, align='center')
sbarp 		= ax.bar(inx+offset+offset, salldiff, color=colors[3], width=offset, align='center')
barlabels = ['$\\textsc{rand}$', '$\\textsc{mv}$', '$\\textsc{fusion}$']
ax.set_xticklabels(barlabels, rotation='vertical')

starfont = {'family' : 'serif', 'weight' : 'bold', 'size'   : 10}
ax.text(rbarp[2].get_x() + rbarp[2].get_width()/2., 1.05*rbarp[2].get_height(), stars(stats.wilcoxon(rpd01['FUSION'],rpd01['MVOTE'])[1]), ha='center', va='bottom', **starfont)
ax.text(mmbarp[2].get_x() + mmbarp[2].get_width()/2., 1.05*mmbarp[2].get_height(), stars(stats.wilcoxon(mmpd01['FUSION'],mmpd01['MVOTE'])[1]), ha='center', va='bottom', **starfont)
ax.text(sbarp[2].get_x() + sbarp[2].get_width()/2., 1.05*sbarp[2].get_height(), stars(stats.wilcoxon(spd01['FUSION'],spd01['MVOTE'])[1]), ha='center', va='bottom', **starfont)

allvals = np.concatenate([mmalldiff, ralldiff, salldiff])
# plt.yticks(np.arange(-6,-6),fontsize=16)
fontsize = 13
ax.tick_params(labelsize=fontsize)
plt.ylim([np.floor(np.min(allvals)),np.ceil(np.max(allvals))])
plt.ylim([-15,8])
plt.yticks(np.arange(-13,8,1))

plt.ylabel("\% Chord Symbol Recall difference",fontsize=fontsize)
plt.xticks(np.arange(0,len(ball),1),fontsize=fontsize)
ax.legend((rbarp[0], mmbarp[0], sbarp[0]), (ball), loc=4,fontsize=fontsize, frameon=True)
# ax.yaxis.grid(False)

sns.despine(left=True)
plt.tight_layout()
# plt.title(path + " differences")
plt.show()