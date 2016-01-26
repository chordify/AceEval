from __future__ import division
import numpy as np
import matplotlib.pyplot as plt
import string
import pandas as pd
import sys


def plotAlignment(sequences):
    title = sequences[0:24]
    output = title + '.pdf'

    dfall = pd.read_csv(sequences, header=None)
    editids = dfall.iloc[:,0]

    df = dfall.iloc[:,1:]
    crange = np.arange(np.unique(df).shape[0])[::-1]
    dfn = df.replace(np.unique(df), crange+1)

    # fig,axes = plt.subplots(nrows=1, ncols=1, figsize=(75, 300))
    fig = plt.figure(figsize=(75, 300))
    axes = fig.add_subplot(1, 1, 1)

    img = axes.imshow(dfn, cmap=plt.cm.RdPu, interpolation='nearest')
    width = dfn.shape[1]
    height = dfn.shape[0]
    for x in np.arange(1,width):
        for y in np.arange(height):
            axes.annotate(df[x][y], xy=(x-1, y), 
                        horizontalalignment='center',
                        verticalalignment='center',size=2)

    axes.set_xticks(np.arange(width)+0.5)
    axes.set_xticklabels([])

    axes.set_yticks(np.arange(height)+0.5)
    axes.set_yticklabels(editids)
    axes.tick_params(axis='y', which='major', labelsize=7)
    axes.set_title(title, fontsize=14)
    axes.grid(color='black', linestyle='-', linewidth=0.3)

    plt.tight_layout()
    plt.savefig(output, bbox_inches='tight')
    plt.close()

def main():
    sequences = sys.argv[1]
    print "plotting..."
    plotAlignment(sequences)
    print "done!"

if __name__ == "__main__":
    main()
