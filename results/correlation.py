
import sys
import matplotlib.pyplot as plt 
import seaborn as sns
import numpy as np
import pandas as pd
from scipy import stats



ral = pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb12/Billboard2012_ROOT.csv', sep='\t')
ralc = ral.columns.values

rsa = pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb12/Billboard2012_ROOT_SA.csv', sep='\t')
inx = rsa.columns.values[0:-2]

plt.scatter(ral[ralc[1:-4]], rsa[inx])
plt.xlabel('WCSR')
plt.ylabel('Source accuracy')
plt.show()