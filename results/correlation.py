
import sys
import matplotlib.pyplot as plt 
import seaborn as sns
import numpy as np
import pandas as pd
from scipy import stats

sns.set_style("whitegrid")
sns.set_context("paper")
plt.rc('text', usetex=True)
font = {'family' : 'serif', 'weight' : 'bold', 'size'   : 30}
plt.rc('font', **font)
fig, ax 	= plt.subplots(figsize=(5,5))
colors = sns.color_palette("RdGy", 10)[::-1]
dotsize = 1
year = str(13)

ral 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT.csv', sep='\t')
ralc 	= ral.columns.values
rsa 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT_SA.csv', sep='\t')
rinx 	= rsa.columns.values[0:-2]
rtt		= stats.ttest_ind(np.asarray(ral[ralc[1:-4]]).reshape(-1), np.asarray(rsa[rinx]).reshape(-1))
print "root ttest =" + str(rtt)
ax.scatter(ral[ralc[1:-4]], rsa[rinx]*100, color=colors[3], s=dotsize)

mmal 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_MajMin.csv', sep='\t')
mmalc 	= mmal.columns.values
mmsa 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_MajMin_SA.csv', sep='\t')
mminx 	= mmsa.columns.values[0:-2]
mmtt	= stats.ttest_ind(np.asarray(mmal[mmalc[1:-4]]).reshape(-1), np.asarray(mmsa[mminx]).reshape(-1))
print "mm ttest =" + str(mmtt)
ax.scatter(mmal[mmalc[1:-4]], mmsa[mminx]*100, color=colors[2], s=dotsize)

sal 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_Sevth.csv', sep='\t')
salc 	= sal.columns.values
ssa 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_Sevth_SA.csv', sep='\t')
sinx 	= ssa.columns.values[0:-2]
stt		= stats.ttest_ind(np.asarray(sal[salc[1:-4]]).reshape(-1), np.asarray(ssa[sinx]).reshape(-1))
print "mm ttest =" + str(stt)
ax.scatter(sal[salc[1:-4]], ssa[sinx]*100, color=colors[0], s=dotsize)

fontsize = 13
ax.tick_params(labelsize=fontsize)

plt.xlim([0,100])
plt.ylim([0,100])

plt.xlabel('WCSR',fontsize=fontsize)
plt.ylabel('Source accuracy',fontsize=fontsize)
plt.show()