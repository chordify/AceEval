#!/usr/bin/python

import sys
import matplotlib.pyplot as plt 
import matplotlib.gridspec as gridspec
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
rpd       = np.genfromtxt(r12, delimiter=' ')
rpd01     = rpd[:,3]*100.
rmaxmed   = np.max(rpd01[0:12])
rrdiff	 	= rpd01[13] - rmaxmed
rmvdiff  	= rpd01[14] - rmaxmed
rfdiff 	 	= rpd01[15] - rmaxmed
# ralldiff 	= np.array([rrdiff, rmvdiff, rfdiff])

mm12       = "bb12/Billboard2012_MajMin_wcsrs.csv"
mmpd       = np.genfromtxt(mm12, delimiter=' ')
mmpd01     = mmpd[:,3]*100.
mmmaxmed   = np.max(mmpd01[0:12])
mmrdiff    = mmpd01[13] - mmmaxmed
mmmvdiff   = mmpd01[14] - mmmaxmed
mmfdiff    = mmpd01[15] - mmmaxmed
# mmalldiff  = np.array([mmrdiff, mmmvdiff, mmfdiff])

s12       = "bb12/Billboard2012_Sevth_wcsrs.csv"
spd       = np.genfromtxt(s12, delimiter=' ')
spd01     = spd[:,3]*100.
smaxmed   = np.max(spd01[0:12])
srdiff    = spd01[13] - smaxmed
svdiff    = spd01[14] - smaxmed
sfdiff    = spd01[15] - smaxmed
# salldiff  = np.array([srdiff, svdiff, sfdiff])
ralldiff   = np.array([rrdiff,   mmrdiff, srdiff])
mmalldiff  = np.array([rmvdiff, mmmvdiff, svdiff])
salldiff  = np.array([rfdiff,  mmfdiff, sfdiff])

# 2013 results
r13         = "bb13/Billboard2013_ROOT_wcsrs.csv"
rpd13       = np.genfromtxt(r13, delimiter=' ')
rpd0113     = rpd13[:,3]*100.
rmaxmed13   = np.max(rpd0113[0:12])
rrdiff13    = rpd0113[13] - rmaxmed13
rmvdiff13   = rpd0113[14] - rmaxmed13
rfdiff13    = rpd0113[15] - rmaxmed13
# ralldiff13  = np.array([rrdiff13, rmvdiff13, rfdiff13])

mm13         = "bb13/Billboard2013_MajMin_wcsrs.csv"
mmpd13       = np.genfromtxt(mm13, delimiter=' ')
mmpd0113     = mmpd13[:,3]*100.
mmmaxmed13   = np.max(mmpd0113[0:12])
mmrdiff13    = mmpd0113[13] - mmmaxmed13
mmmvdiff13   = mmpd0113[14] - mmmaxmed13
mmfdiff13    = mmpd0113[15] - mmmaxmed13
# mmalldiff13  = np.array([mmrdiff13, mmmvdiff13, mmfdiff13])

s13         = "bb13/Billboard2013_Sevth_wcsrs.csv"
spd13       = np.genfromtxt(s13, delimiter=' ')
spd0113     = spd13[:,3]*100.
smaxmed13   = np.max(spd0113[0:12])
srdiff13    = spd0113[13] - smaxmed13
svdiff13    = spd0113[14] - smaxmed13
sfdiff13    = spd0113[15] - smaxmed13
# salldiff13  = np.array([srdiff13, svdiff13, sfdiff13])

ralldiff13   = np.array([rrdiff13,   mmrdiff13, srdiff13])
mmalldiff13  = np.array([rmvdiff13, mmmvdiff13, svdiff13])
salldiff13  = np.array([rfdiff13,  mmfdiff13, sfdiff13])

# ball = ['R', 'MM', 'MM7']
ball = ['$\\textsc{rnd}$','$\\textsc{mv}$','$\\textsc{df}$']
sns.set_style("whitegrid")
sns.set_context("paper")
plt.rc('text', usetex=True)
font = {'family' : 'serif', 'weight' : 'bold', 'size'   : 30}
plt.rc('font', **font)
# colors = sns.color_palette("GnBu_d", n_colors=4)[::-1]
colors = sns.color_palette("RdGy", 10)[::-1]

f = plt.figure(figsize=(8,4))
gs1 = gridspec.GridSpec(1,2)
ax1 = f.add_subplot(gs1[0])
ax2 = f.add_subplot(gs1[1])

offset 		  = (1. / len(ball)) - 0.05
# barlabels   = ['','$\\textsc{rnd}$','','$\\textsc{mv}$','','$\\textsc{df}$']
barlabels   = ['','$\\textsc{r}$','','$\\textsc{mm}$','','$\\textsc{mm\oldstylenums{7}}$']

inx         = np.arange(0,len(ball))-offset
print inx
rbarp       = ax1.bar(inx,               ralldiff,    color=colors[4], width=offset, align='center', hatch='.')
mmbarp      = ax1.bar(inx+offset,        mmalldiff,   color=colors[2], width=offset, align='center')
sbarp       = ax1.bar(inx+offset+offset, salldiff,    color=colors[0], width=offset, align='center')
ax1.set_xticklabels(barlabels, rotation='vertical')

rbarp13     = ax2.bar(inx,               ralldiff13,  color=colors[4], width=offset, align='center', hatch='.')
mmbarp13    = ax2.bar(inx+offset,        mmalldiff13, color=colors[2], width=offset, align='center')
sbarp13     = ax2.bar(inx+offset+offset, salldiff13,  color=colors[0], width=offset, align='center')
ax2.set_xticklabels(barlabels, rotation='vertical')

# starfont = {'family' : 'serif', 'weight' : 'bold', 'size'   : 10}
# ax.text(rbarp[2].get_x() + rbarp[2].get_width()/2., 1.05*rbarp[2].get_height(), stars(stats.wilcoxon(rpd01['FUSION'],rpd01['MVOTE'])[1]), ha='center', va='bottom', **starfont)
# ax.text(mmbarp[2].get_x() + mmbarp[2].get_width()/2., 1.05*mmbarp[2].get_height(), stars(stats.wilcoxon(mmpd01['FUSION'],mmpd01['MVOTE'])[1]), ha='center', va='bottom', **starfont)
# ax.text(sbarp[2].get_x() + sbarp[2].get_width()/2., 1.05*sbarp[2].get_height(), stars(stats.wilcoxon(spd01['FUSION'],spd01['MVOTE'])[1]), ha='center', va='bottom', **starfont)

allvals12 = np.concatenate([ralldiff, mmalldiff, salldiff])
print allvals12
allvals13 = np.concatenate([ralldiff13, mmalldiff13, salldiff13])
print allvals13

fontsize = 15
ax1.tick_params(labelsize=fontsize)
ax2.tick_params(labelsize=fontsize)
ax1.set_ylabel("\\textsc{wcsr} difference with best team",fontsize=fontsize)
ax1.set_title('\\textsc{bb12}',fontsize=fontsize)

ax1.set_ylim([-12,6])
ax1.set_yticks(np.arange(-12,7,1))
ax2.set_ylim([-12,6])
ax2.set_yticks(np.arange(-12,7,1))
ax2.set_title('\\textsc{bb13}',fontsize=fontsize)

ax2.legend((rbarp[0], mmbarp[0], sbarp[0]), (ball), loc=4,fontsize=fontsize, frameon=True, bbox_to_anchor=(1.22, 0))
ax2.yaxis.set_major_formatter(plt.NullFormatter())

# ax1.set_label('\\textcs{bb\oldstylenums{12}}')
# ax2.set_label('\\textcs{bb\oldstylenums{13}}')

sns.despine(left=True)
gs1.update(wspace=0.03) # set the spacing between axes. 
gs1.tight_layout(f)
# plt.show()
plt.savefig('/Users/hvkoops/repos/ismir2016/paper/figs/bb1213rev.pdf')
plt.close()