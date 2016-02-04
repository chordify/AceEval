	ax.plot(np.arange(len(means)), means, 'ok')
	if info:
		for p in np.arange(len(stds)):
			if p == len(pvals):
				txt = "med="+str(round(medians[p],2))+"\n"+"std="+str(round(stds[p],3))
				ax.text(p, 102, txt, horizontalalignment='center', verticalalignment='center')
			else:
				txt = "p="+ '%.4E '% pvals[p] + "\n"+stars(pvals[p])+"\n"+"med="+str(round(medians[p],3))+"\n"+"std="+str(round(stds[p],3))
				ax.text(p, 104, txt, horizontalalignment='center', verticalalignment='center')
	sns.despine(offset=10, trim=True)