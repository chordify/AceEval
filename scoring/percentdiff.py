
import json
import pandas as pd
import numpy as np
from scipy import stats

# (1-((ral12[inx].std()/ral12[inx].mean())['FUSION']/(ral12[inx].std()/ral12[inx].mean())[ral12[inx[:-3]].median().idxmax()]))*100
# (1-((ral12[inx].std()/ral12[inx].mean())['MVOTE']/(ral12[inx].std()/ral12[inx].mean())[ral12[inx[:-3]].median().idxmax()]))*100
# (1-((ral12[inx].std()/ral12[inx].mean())['RANDOM']/(ral12[inx].std()/ral12[inx].mean())[ral12[inx[:-3]].median().idxmax()]))*100
# print (1-((ds[inx].std()/ds[inx].mean())['FUSION']/(ds[inx].std()/ds[inx].mean())[ds[inx[:-3]].median().idxmax()]))*100
# print (1-((ds[inx].std()/ds[inx].mean())['RANDOM']/(ds[inx].std()/ds[inx].mean())[ds[inx[:-3]].median().idxmax()]))*100
# print (1-((ds[inx].std()/ds[inx].mean())['MVOTE']/(ds[inx].std()/ds[inx].mean())[ds[inx[:-3]].median().idxmax()]))*100

year = str(12)
print "2012"
ral12  	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT.csv', sep='\t')
inx = ral12.columns.values[[1,2,3,4,5,6,7,8,9,10,11,12,14,15,16]]
ds = ral12

print "ROOTS"
top3 = ds[inx[:-3]].median().sort_values(ascending=False)[0:2].keys()
top3 = ds[inx[:-3]].median().idxmax()
print ds[inx].std()['FUSION'] - ds[inx].std().mean()
print ds[inx].std()['RANDOM'] - ds[inx].std().mean()
print ds[inx].std()['MVOTE'] - ds[inx].std().mean()
r12res = [ds[inx].std()['FUSION'] - ds[inx].std().mean(), ds[inx].std()['RANDOM'] - ds[inx].std().mean(), ds[inx].std()['MVOTE'] - ds[inx].std().mean()]
r12res = np.round(r12res,2)
print "FUSION smallest std:" + str(ds[inx].std()['FUSION'] < ds[inx].std().mean())

mmal12 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_MajMin.csv', sep='\t')
ds = mmal12
print "MM"
top3 = ds[inx[:-3]].median().sort_values(ascending=False)[0:2].keys()
top3 = ds[inx[:-3]].median().idxmax()
print ds[inx].std()['FUSION'] - ds[inx].std().mean()
print ds[inx].std()['RANDOM'] - ds[inx].std().mean()
print ds[inx].std()['MVOTE'] - ds[inx].std().mean()
mm12res = [ds[inx].std()['FUSION'] - ds[inx].std().mean(), ds[inx].std()['RANDOM'] - ds[inx].std().mean(), ds[inx].std()['MVOTE'] - ds[inx].std().mean()]
mm12res = np.round(mm12res,2)
print "FUSION smallest std:" + str(ds[inx].std()['FUSION'] < ds[inx].std().mean())

sal12  	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_Sevth.csv', sep='\t')
ds = sal12
print "MM7"
top3 = ds[inx[:-3]].median().sort_values(ascending=False)[0:2].keys()
top3 = ds[inx[:-3]].median().idxmax()
print ds[inx].std()['FUSION'] - ds[inx].std().mean()
print ds[inx].std()['RANDOM'] - ds[inx].std().mean()
print ds[inx].std()['MVOTE'] - ds[inx].std().mean()
s12res = [ds[inx].std()['FUSION'] - ds[inx].std().mean(), ds[inx].std()['RANDOM'] - ds[inx].std().mean(), ds[inx].std()['MVOTE'] - ds[inx].std().mean()]
s12res = np.round(s12res,2)
print "FUSION smallest std:" + str(ds[inx].std()['FUSION'] < ds[inx].std().mean())

year = str(13)
print "2013"
ral13  	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_ROOT.csv', sep='\t')
ds = ral13
print "ROOTS"
top3 = ds[inx[:-3]].median().sort_values(ascending=False)[0:2].keys()
top3 = ds[inx[:-3]].median().idxmax()
print ds[inx].std()['FUSION'] - ds[inx].std().mean()
print ds[inx].std()['RANDOM'] - ds[inx].std().mean()
print ds[inx].std()['MVOTE'] - ds[inx].std().mean()
r13res = [ds[inx].std()['FUSION'] - ds[inx].std().mean(), ds[inx].std()['RANDOM'] - ds[inx].std().mean(), ds[inx].std()['MVOTE'] - ds[inx].std().mean()]
r13res = np.round(r13res,2)
print "FUSION smallest std:" + str(ds[inx].std()['FUSION'] < ds[inx].std().mean())


mmal13 	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_MajMin.csv', sep='\t')
ds = mmal13
print "MM"
top3 = ds[inx[:-3]].median().sort_values(ascending=False)[0:2].keys()
top3 = ds[inx[:-3]].median().idxmax()
print ds[inx].std()['FUSION'] - ds[inx].std().mean()
print ds[inx].std()['RANDOM'] - ds[inx].std().mean()
print ds[inx].std()['MVOTE'] - ds[inx].std().mean()
mm13res = [ds[inx].std()['FUSION'] - ds[inx].std().mean(), ds[inx].std()['RANDOM'] - ds[inx].std().mean(), ds[inx].std()['MVOTE'] - ds[inx].std().mean()]
mm13res = np.round(mm13res,2)
print "FUSION smallest std:" + str(ds[inx].std()['FUSION'] < ds[inx].std().mean())


sal13  	= pd.read_csv('/Users/hvkoops/repos/aceeval/results/bb' + year + '/Billboard20' + year + '_Sevth.csv', sep='\t')
ds = sal13
print "MM7"
top3 = ds[inx[:-3]].median().sort_values(ascending=False)[0:2].keys()
top3 = ds[inx[:-3]].median().idxmax()
print ds[inx].std()['FUSION'] - ds[inx].std().mean()
print ds[inx].std()['RANDOM'] - ds[inx].std().mean()
print ds[inx].std()['MVOTE'] - ds[inx].std().mean()
s13res = [ds[inx].std()['FUSION'] - ds[inx].std().mean(), ds[inx].std()['RANDOM'] - ds[inx].std().mean(), ds[inx].std()['MVOTE'] - ds[inx].std().mean()]
s13res = np.round(s13res,2)
print "FUSION smallest std:" + str(ds[inx].std()['FUSION'] < ds[inx].std().mean())


