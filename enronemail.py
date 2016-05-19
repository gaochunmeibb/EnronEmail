import os
import re
import networkx as nx
import matplotlib.pyplot as plt
from operator import itemgetter
# Enron email network analysis
# please change the direcoty in line 8 ans 20 to the folder with download data.
os.chdir("/Users/chunmeiGao/Documents/15fall/6907_bigdata_analysis/Homework1/python/data/")

# create networkx graph
G=nx.DiGraph()

def mylistdir(directory):
    """A specialized version of os.listdir() that ignores files that start with a leading period."""
    filelist = os.listdir(directory)
    return [x for x in filelist
            if not (x.startswith('.'))]

for filename in mylistdir("/Users/chunmeiGao/Documents/15fall/6907_bigdata_analysis/Homework1/python/data/"):
        f = open(filename, 'r')
        content=f.readlines()

        tolist=content[4]
        tolist=re.sub('To: ', '', tolist)
        tolist=re.sub('\r\n','',tolist)
        tolist=re.sub('@enron.com','',tolist)
        tolist=re.sub('.com','',tolist)
        recipientslist = tolist.split(',')

        fromlist=content[3]
        fromlist=re.sub('From: ', '', fromlist)
        fromlist=re.sub('\r\n','',fromlist)
        fromlist=re.sub('@enron.com','',fromlist)
        fromlist=re.sub('.com','',fromlist)

        if '<nodes>' not in fromlist:
            for recipients in recipientslist:
                if (recipients!=' '):
                    #print "Nodes of graph: ", fromlist
                    #print "Edges of graph: ", recipients
                    # add nodes
                    G.add_node(fromlist)
                    G.add_node(recipients)
                    # add edges
                    G.add_edge(fromlist, recipients)
# stat: centrality analysis
degreecentral=nx.degree_centrality(G)
maxPair = max(degreecentral.iteritems(), key=itemgetter(1))
print maxPair

# draw graph
nx.draw(G,node_size=40,with_labels=True)

# show graph
plt.show()
__author__ = 'chunmeiGao'
