
import sys
import matplotlib.pyplot as plt 
import seaborn as sns
import numpy as np
import pandas as pd
from scipy import stats


ral 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb12/Billboard2012_ROOT.csv', sep='\t')
ralc 	= ral.columns.values
rsa 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb12/Billboard2012_ROOT_SA.csv', sep='\t')
rinx 	= rsa.columns.values[0:-2]
rtt		= stats.ttest_ind(np.asarray(ral[ralc[1:-4]]).reshape(-1), np.asarray(rsa[rinx]).reshape(-1))
print "root ttest =" + str(rtt)
plt.scatter(ral[ralc[1:-4]], rsa[rinx])

mmal 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb12/Billboard2012_MajMin.csv', sep='\t')
mmalc 	= mmal.columns.values
mmsa 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb12/Billboard2012_MajMin_SA.csv', sep='\t')
mminx 	= mmsa.columns.values[0:-2]
mmtt	= stats.ttest_ind(np.asarray(mmal[mmalc[1:-4]]).reshape(-1), np.asarray(mmsa[mminx]).reshape(-1))
print "mm ttest =" + str(mmtt)
plt.scatter(mmal[mmalc[1:-4]], mmsa[mminx], color='r')


plt.xlabel('WCSR')
plt.ylabel('Source accuracy')
plt.show()