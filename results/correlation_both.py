
import sys
import matplotlib.pyplot as plt 
import matplotlib.gridspec as gridspec
import seaborn as sns
import numpy as np
import pandas as pd
from scipy import stats

sns.set_style("whitegrid")
sns.set_context("paper")
plt.rc('text', usetex=True)
font = {'family' : 'serif', 'weight' : 'bold', 'size'   : 30}
plt.rc('font', **font)
colors = sns.color_palette("RdGy", 20)[::-1]
dotsize = 1

f, (ax1, ax2) = plt.subplots(1, 2, sharey=True, sharex=True, figsize=(8,4))
gs1 = gridspec.GridSpec(1, 2)

year = str(12)
ax1 = plt.subplot(gs1[0])
ral 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT.csv', sep='\t')
ralc 	= ral.columns.values
rsa 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT_SA.csv', sep='\t')
rinx 	= rsa.columns.values[0:-2]
rtt		= stats.spearmanr(np.asarray(ral[ralc[1:-4]]).reshape(-1), np.asarray(rsa[rinx]).reshape(-1))
print "root ttest =" + str(rtt)
ax1.scatter(ral[ralc[1:-4]]/100, rsa[rinx], color=colors[2], s=dotsize)

mmal 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_MajMin.csv', sep='\t')
mmalc 	= mmal.columns.values
mmsa 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_MajMin_SA.csv', sep='\t')
mminx 	= mmsa.columns.values[0:-2]
mmtt	= stats.spearmanr(np.asarray(mmal[mmalc[1:-4]]).reshape(-1), np.asarray(mmsa[mminx]).reshape(-1))
print "mm ttest =" + str(mmtt)
ax1.scatter(mmal[mmalc[1:-4]]/100, mmsa[mminx], color=colors[1], s=dotsize)

sal 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_Sevth.csv', sep='\t')
salc 	= sal.columns.values
ssa 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_Sevth_SA.csv', sep='\t')
sinx 	= ssa.columns.values[0:-2]
stt		= stats.spearmanr(np.asarray(sal[salc[1:-4]]).reshape(-1), np.asarray(ssa[sinx]).reshape(-1))
print "mm ttest =" + str(stt)
ax1.scatter(sal[salc[1:-4]]/100, ssa[sinx], color=colors[0], s=dotsize)

year = str(13)
ax2 = plt.subplot(gs1[1])
ral 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT.csv', sep='\t')
ralc 	= ral.columns.values
rsa 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT_SA.csv', sep='\t')
rinx 	= rsa.columns.values[0:-2]
rtt		= stats.spearmanr(np.asarray(ral[ralc[1:-4]]).reshape(-1), np.asarray(rsa[rinx]).reshape(-1))
print "root ttest =" + str(rtt)
ax2.scatter(ral[ralc[1:-4]]/100, rsa[rinx], color=colors[2], s=dotsize)

mmal 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_MajMin.csv', sep='\t')
mmalc 	= mmal.columns.values
mmsa 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_MajMin_SA.csv', sep='\t')
mminx 	= mmsa.columns.values[0:-2]
mmtt	= stats.spearmanr(np.asarray(mmal[mmalc[1:-4]]).reshape(-1), np.asarray(mmsa[mminx]).reshape(-1))
print "mm ttest =" + str(mmtt)
ax2.scatter(mmal[mmalc[1:-4]]/100, mmsa[mminx], color=colors[1], s=dotsize)

sal 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_Sevth.csv', sep='\t')
salc 	= sal.columns.values
ssa 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_Sevth_SA.csv', sep='\t')
sinx 	= ssa.columns.values[0:-2]
stt		= stats.spearmanr(np.asarray(sal[salc[1:-4]]).reshape(-1), np.asarray(ssa[sinx]).reshape(-1))
print "mm ttest =" + str(stt)
ax2.scatter(sal[salc[1:-4]]/100, ssa[sinx], color=colors[0], s=dotsize)

fontsize = 16
ax1.tick_params(labelsize=fontsize)
ax2.tick_params(labelsize=fontsize)

ax1.set_xlim([0,1])
ax1.set_xticks(np.arange(0,1,0.2))
ax2.set_xlim([0,1])

ax1.set_ylim([0,1])
ax2.set_ylim([0,1])

# ax2.get_yaxis().set_ticks([])
ax2.yaxis.set_major_formatter(plt.NullFormatter())

ax1.set_xlabel('WCSR',fontsize=fontsize)
ax2.set_xlabel('WCSR',fontsize=fontsize)
ax1.set_ylabel('\\textsc{df} Source Accuracy',fontsize=fontsize)
ax1.set_title('\\textsc{bb12}',fontsize=fontsize)
ax2.set_title('\\textsc{bb13}',fontsize=fontsize)

gs1.tight_layout(f)
gs1.update(wspace=0.02, hspace=0) # set the spacing between axes. 
plt.show()