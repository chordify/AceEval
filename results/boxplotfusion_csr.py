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

# 2012 results
r12       = "bb12/Billboard2012_ROOT_wcsrs.csv"
rpd01     = pd.read_csv(r12, sep='\t', header=0, dtype=np.float64)
rlabels   = rpd01.columns.values
rteams    = rlabels[1:-4]
rmedians  = rpd01[rteams].median(axis=1)
rrdiff    = np.median(rpd01['RANDOM'] - rmedians)
rmvdiff   = np.median(rpd01['MVOTE']  - rmedians)
rfdiff    = np.median(rpd01['FUSION'] - rmedians)
ralldiff  = np.array([rrdiff, rmvdiff, rfdiff])

mm12       = "bb12/Billboard2012_MajMin_wcsrs.csv"
mmpd01    = pd.read_csv(mm12, sep='\t', header=0, dtype=np.float64)
mmlabels  = mmpd01.columns.values
mmteams   = mmlabels[1:-4]
mmmedians = mmpd01[mmteams].median(axis=1)
mmrdiff   = np.median(mmpd01['RANDOM'] - mmmedians)
mmmvdiff  = np.median(mmpd01['MVOTE']  - mmmedians)
mmfdiff   = np.median(mmpd01['FUSION'] - mmmedians)
mmalldiff = np.array([mmrdiff, mmmvdiff, mmfdiff])

s12       = "bb12/Billboard2012_Sevth_wcsrs.csv"
spd01     = pd.read_csv(s12, sep='\t', header=0, dtype=np.float64)
slabels   = spd01.columns.values
steams    = slabels[1:-4]
smedians  = spd01[rteams].median(axis=1)
srdiff    = np.median(spd01['RANDOM'] - smedians)
smvdiff   = np.median(spd01['MVOTE']  - smedians)
sfdiff    = np.median(spd01['FUSION'] - smedians)
salldiff  = np.array([srdiff, smvdiff, sfdiff])

# 2013 results
r13         = "bb13/Billboard2013_ROOT_wcsrs.csv"
rpd13       = np.genfromtxt(r13, delimiter=' ')
rpd0113     = rpd13[:,3]*100.
rmaxmed13   = np.max(rpd0113[0:12])
rrdiff13    = rpd0113[13] - rmaxmed13
rmvdiff13   = rpd0113[14] - rmaxmed13
rfdiff13    = rpd0113[15] - rmaxmed13
ralldiff13  = np.array([rrdiff13, rmvdiff13, rfdiff13])

mm13         = "bb13/Billboard2013_MajMin_wcsrs.csv"
mmpd13       = np.genfromtxt(mm13, delimiter=' ')
mmpd0113     = mmpd13[:,3]*100
mmmaxmed13   = np.max(mmpd0113[0:12])
mmrdiff13    = mmpd0113[13] - mmmaxmed13
mmmvdiff13   = mmpd0113[14] - mmmaxmed13
mmfdiff13    = mmpd0113[15] - mmmaxmed13
mmalldiff13  = np.array([mmrdiff13, mmmvdiff13, mmfdiff13])

s13         = "bb13/Billboard2013_Sevth_wcsrs.csv"
spd13       = np.genfromtxt(s13, delimiter=' ')
spd0113     = spd13[:,3]*100
smaxmed13   = np.max(spd0113[0:12])
srdiff13    = spd0113[13] - smaxmed13
svdiff13    = spd0113[14] - smaxmed13
sfdiff13    = spd0113[15] - smaxmed13
salldiff13  = np.array([srdiff13, svdiff13, sfdiff13])

# ball = ['Root', 'MajMin', 'MajMin7']
ball = ['R', 'MM', 'MM7']
sns.set_style("whitegrid")
sns.set_context("paper")
colors = sns.color_palette("GnBu_d", n_colors=4)[::-1]

f, (ax1, ax2) = plt.subplots(1, 2, sharey=True, sharex=True, figsize=(5,5))
plt.rc('text', usetex=True)
font = {'family' : 'serif', 'weight' : 'bold', 'size'   : 30}
plt.rc('font', **font)
offset 		= (1. / len(ball)) - 0.05
barlabels   = ['$\\textsc{rnd}$', '$\\textsc{mv}$', '$\\textsc{df}$']

inx 		    = np.arange(0,len(ball))-offset
rbarp 		  = ax1.bar(inx,               ralldiff,    color=colors[0], width=offset, align='center')
mmbarp 		  = ax1.bar(inx+offset,        mmalldiff,   color=colors[2], width=offset, align='center')
sbarp 		  = ax1.bar(inx+offset+offset, salldiff,    color=colors[3], width=offset, align='center')
ax1.set_xticklabels(barlabels, rotation='vertical')

rbarp13     = ax2.bar(inx,               ralldiff13,  color=colors[0], width=offset, align='center')
mmbarp13    = ax2.bar(inx+offset,        mmalldiff13, color=colors[2], width=offset, align='center')
sbarp13     = ax2.bar(inx+offset+offset, salldiff13,  color=colors[3], width=offset, align='center')
ax2.set_xticklabels(barlabels, rotation='vertical')

# starfont = {'family' : 'serif', 'weight' : 'bold', 'size'   : 10}
# ax.text(rbarp[2].get_x() + rbarp[2].get_width()/2., 1.05*rbarp[2].get_height(), stars(stats.wilcoxon(rpd01['FUSION'],rpd01['MVOTE'])[1]), ha='center', va='bottom', **starfont)
# ax.text(mmbarp[2].get_x() + mmbarp[2].get_width()/2., 1.05*mmbarp[2].get_height(), stars(stats.wilcoxon(mmpd01['FUSION'],mmpd01['MVOTE'])[1]), ha='center', va='bottom', **starfont)
# ax.text(sbarp[2].get_x() + sbarp[2].get_width()/2., 1.05*sbarp[2].get_height(), stars(stats.wilcoxon(spd01['FUSION'],spd01['MVOTE'])[1]), ha='center', va='bottom', **starfont)

# allvals = ralldiff
allvals = np.concatenate([ralldiff, mmalldiff, salldiff])
fontsize = 13
ax1.tick_params(labelsize=fontsize)
# plt.ylim([np.floor(np.min(allvals)),np.ceil(np.max(allvals))])
# plt.ylim([-12,6])
plt.yticks(np.arange(-12,6,1))

ax1.set_ylabel("\% \\textsc{wcsr} difference with best team",fontsize=fontsize)
plt.xticks(np.arange(0,len(ball),1),fontsize=fontsize)
ax2.legend((rbarp[0], mmbarp[0], sbarp[0]), (ball), loc=4,fontsize=10, frameon=True)
# ax.yaxis.grid(False)

sns.despine(left=True)
plt.tight_layout()
# plt.title(path + " differences")
plt.show()