import json
import pandas as pd
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt

def readJsonFile(file):
        with open(file) as data_file:
                data = json.load(data_file)
        return data

bb12 = readJsonFile('/Users/hvkoops/repos/aceeval/scoring/vincent-2012-score-structure.json')
bb13 = readJsonFile('/Users/hvkoops/repos/aceeval/scoring/vincent-2013-score-structure.json')

# [u'bigramScoring', u'hmmScore', u'SdNR-linear', u'PID', u'tree nj']

year  		= str(12)
files12   	= bb12.keys()
methods12 	= bb12[files12[0]].keys()
# other methods
ral12  		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT.csv', sep='\t')
ralc12 		= ral12.columns.values
flines      = bb12[files12[0]][methods12[0]].keys()
# source accuracies
rsa12 		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT_SA.csv', sep='\t')
rinx12 		= rsa12.columns.values[0:-2]

allcorr12 	= []
allp12		= []
print "year : " + str(year)
for m in methods12:
	all12 = np.array([bb12[f][m][str(i)] for f in files12 for i in flines])
	gt12  = np.asarray(ral12[ralc12[1:-4]]).reshape(-1)
	rtt	  = stats.spearmanr(gt12, all12)
	print "method: " + m + " correlation: " + str(rtt)
	# slope, intercept, r_value, p_value, std_err = stats.linregress(gt12,all12)
	# print "method: " + m + " linear reg:  " + str(r_value**2)
	allcorr12 = allcorr12 + [rtt[0]]
	allp12 = allp12 + [rtt[1]]
satt12		= stats.spearmanr(gt12, np.asarray(rsa12[rinx12]).reshape(-1))
print "method: source accuracy correlation: " + str(satt12)
allcorr12 = allcorr12 + [satt12[0]]
allp12 = allp12 + [satt12[1]]

allcorr13 	= []
allp13		= []
year  		= str(13)
files13   	= bb13.keys()
methods13 	= bb13[files13[0]].keys()
# other methods
ral13  		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT.csv', sep='\t')
ralc13 		= ral13.columns.values
# source accuracies
rsa13 		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT_SA.csv', sep='\t')
rinx13 		= rsa13.columns.values[0:-2]

print "year : " + str(year)
for m in methods12:
	all13 = np.array([bb13[f][m][str(i)] for f in files13 for i in flines])
	gt13   = np.asarray(ral13[ralc13[1:-4]]).reshape(-1)
	rtt	  = stats.spearmanr(gt13, all13)	
	print "method: " + m + " correlation: " + str(rtt)	
	# slope, intercept, r_value, p_value, std_err = stats.linregress(gt13,all13)
	# print "method: " + m + " linear reg:  " + str(r_value**2)
	allcorr13 = allcorr13 + [rtt[0]]
	allp13 = allp13 + [rtt[1]]
satt13	= stats.spearmanr(gt13, np.asarray(rsa13[rinx13]).reshape(-1))
print "method: source accuracy correlation: " + str(satt13)
allcorr13 = allcorr13 + [satt13[0]]
allp13 = allp13 + [satt13[1]]
