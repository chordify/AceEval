import json
import pandas as pd
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt

def readJsonFile(file):
        with open(file) as data_file:
                data = json.load(data_file)
        return data


# [u'bigramScoring', u'hmmScore', u'SdNR-linear', u'PID', u'tree nj']

print "ROOTS"
bb12r = readJsonFile('/Users/hvkoops/repos/aceeval/scoring/vincent-2012-score-structure.json')
bb13r = readJsonFile('/Users/hvkoops/repos/aceeval/scoring/vincent-2013-score-structure.json')

year  		= str(12)
files12   	= bb12r.keys()
methods12 	= bb12r[files12[0]].keys()
# other methods
ral12  		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT.csv', sep='\t')
ralc12 		= ral12.columns.values
flines      = bb12r[files12[0]][methods12[0]].keys()
# source accuracies
rsa12 		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT_SA.csv', sep='\t')
rinx12 		= rsa12.columns.values[0:-2]

allcorr12 	= []
allp12		= []
print "year : " + str(year)
for m in methods12:
	all12 = np.array([bb12r[f][m][str(i)] for f in files12 for i in flines])
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
files13   	= bb13r.keys()
methods13 	= bb13r[files13[0]].keys()
# other methods
ral13  		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT.csv', sep='\t')
ralc13 		= ral13.columns.values
# source accuracies
rsa13 		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT_SA.csv', sep='\t')
rinx13 		= rsa13.columns.values[0:-2]

print "year : " + str(year)
for m in methods12:
	all13 = np.array([bb13r[f][m][str(i)] for f in files13 for i in flines])
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

# MAJMIN
print "MM"
bb12mm = readJsonFile('2012-mm-MA-score-structure.json')
bb13mm = readJsonFile('2013-mm-MA-score-structure.json')

year  		= str(12)
files12   	= bb12mm.keys()
methods12 	= bb12mm[files12[0]].keys()
# other methods
mmal12  	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_MajMin.csv', sep='\t')
mmalc12 	= mmal12.columns.values
flines      = bb12mm[files12[0]][methods12[0]].keys()
# source accuracies
msa12 		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_MajMin_SA.csv', sep='\t')
minx12 		= msa12.columns.values[0:-2]

allcorr12 	= []
allp12		= []
print "year : " + str(year)
for m in methods12:
	all12 = np.array([bb12mm[f][m][str(i)] for f in files12 for i in flines])
	gt12  = np.asarray(mmal12[mmalc12[1:-4]]).reshape(-1)
	rtt	  = stats.spearmanr(gt12, all12)
	print "method: " + m + " correlation: " + str(rtt)
	# slope, intercept, r_value, p_value, std_err = stats.linregress(gt12,all12)
	# print "method: " + m + " linear reg:  " + str(r_value**2)
	allcorr12 = allcorr12 + [rtt[0]]
	allp12 = allp12 + [rtt[1]]
satt12		= stats.spearmanr(gt12, np.asarray(msa12[minx12]).reshape(-1))
print "method: source accuracy correlation: " + str(satt12)
allcorr12 = allcorr12 + [satt12[0]]
allp12 = allp12 + [satt12[1]]

allcorr13 	= []
allp13		= []
year  		= str(13)
files13   	= bb13mm.keys()
methods13 	= bb13mm[files13[0]].keys()
# other methods
mmal13  		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_MajMin.csv', sep='\t')
mmalc13 		= mmal13.columns.values
# source accuracies
mmsa13 		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_MajMin_SA.csv', sep='\t')
mminx13 		= mmsa13.columns.values[0:-2]

print "year : " + str(year)
for m in methods12:
	all13 = np.array([bb13mm[f][m][str(i)] for f in files13 for i in flines])
	gt13   = np.asarray(mmal13[mmalc13[1:-4]]).reshape(-1)
	rtt	  = stats.spearmanr(gt13, all13)	
	print "method: " + m + " correlation: " + str(rtt)	
	# slope, intercept, r_value, p_value, std_err = stats.linregress(gt13,all13)
	# print "method: " + m + " linear reg:  " + str(r_value**2)
	allcorr13 = allcorr13 + [rtt[0]]
	allp13 = allp13 + [rtt[1]]
satt13	= stats.spearmanr(gt13, np.asarray(mmsa13[mminx13]).reshape(-1))
print "method: source accuracy correlation: " + str(satt13)
allcorr13 = allcorr13 + [satt13[0]]
allp13 = allp13 + [satt13[1]]


print "MM7"
bb12mms = readJsonFile('2012-mm7-MA-score-structure.json')
bb13mms = readJsonFile('2013-mm7-MA-score-structure.json')

year  		= str(12)
files12   	= bb12mms.keys()
methods12 	= bb12mms[files12[0]].keys()
# other methods
mmal12  	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_Sevth.csv', sep='\t')
mmalc12 	= mmal12.columns.values
flines      = bb12mms[files12[0]][methods12[0]].keys()
# source accuracies
msa12 		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_Sevth_SA.csv', sep='\t')
minx12 		= msa12.columns.values[0:-2]

allcorr12 	= []
allp12		= []
print "year : " + str(year)
for m in methods12:
	all12 = np.array([bb12mms[f][m][str(i)] for f in files12 for i in flines])
	gt12  = np.asarray(mmal12[mmalc12[1:-4]]).reshape(-1)
	rtt	  = stats.spearmanr(gt12, all12)
	print "method: " + m + " correlation: " + str(rtt)
	# slope, intercept, r_value, p_value, std_err = stats.linregress(gt12,all12)
	# print "method: " + m + " linear reg:  " + str(r_value**2)
	allcorr12 = allcorr12 + [rtt[0]]
	allp12 = allp12 + [rtt[1]]
satt12		= stats.spearmanr(gt12, np.asarray(msa12[minx12]).reshape(-1))
print "method: source accuracy correlation: " + str(satt12)
allcorr12 = allcorr12 + [satt12[0]]
allp12 = allp12 + [satt12[1]]

allcorr13 	= []
allp13		= []
year  		= str(13)
files13   	= bb13mms.keys()
methods13 	= bb13mms[files13[0]].keys()
# other methods
mmal13  		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_Sevth.csv', sep='\t')
mmalc13 		= mmal13.columns.values
# source accuracies
mmsa13 		= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_Sevth_SA.csv', sep='\t')
mminx13 		= mmsa13.columns.values[0:-2]

print "year : " + str(year)
for m in methods12:
	all13 = np.array([bb13mms[f][m][str(i)] for f in files13 for i in flines])
	gt13   = np.asarray(mmal13[mmalc13[1:-4]]).reshape(-1)
	rtt	  = stats.spearmanr(gt13, all13)	
	print "method: " + m + " correlation: " + str(rtt)	
	# slope, intercept, r_value, p_value, std_err = stats.linregress(gt13,all13)
	# print "method: " + m + " linear reg:  " + str(r_value**2)
	allcorr13 = allcorr13 + [rtt[0]]
	allp13 = allp13 + [rtt[1]]
satt13	= stats.spearmanr(gt13, np.asarray(mmsa13[mminx13]).reshape(-1))
print "method: source accuracy correlation: " + str(satt13)
allcorr13 = allcorr13 + [satt13[0]]
allp13 = allp13 + [satt13[1]]



# (1-((ral12[inx].std()/ral12[inx].mean())['FUSION']/(ral12[inx].std()/ral12[inx].mean())[ral12[inx[:-3]].median().idxmax()]))*100
# (1-((ral12[inx].std()/ral12[inx].mean())['MVOTE']/(ral12[inx].std()/ral12[inx].mean())[ral12[inx[:-3]].median().idxmax()]))*100
# (1-((ral12[inx].std()/ral12[inx].mean())['RANDOM']/(ral12[inx].std()/ral12[inx].mean())[ral12[inx[:-3]].median().idxmax()]))*100

# (1-((ds[inx].std()/ds[inx].mean())['FUSION']/(ds[inx].std()/ds[inx].mean())[ds[inx[:-3]].median().idxmax()]))*100
# (1-((ds[inx].std()/ds[inx].mean())['RANDOM']/(ds[inx].std()/ds[inx].mean())[ds[inx[:-3]].median().idxmax()]))*100
# (1-((ds[inx].std()/ds[inx].mean())['MVOTE']/(ds[inx].std()/ds[inx].mean())[ds[inx[:-3]].median().idxmax()]))*100
