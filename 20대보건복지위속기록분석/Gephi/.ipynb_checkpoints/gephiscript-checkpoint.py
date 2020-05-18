import os
os.chdir('/Users/foxey/GoogleDrive/Research/HCLee/Gephi')
for n in g.nodes:
    n.color = yellow
runLayout(OpenOrd)
stopLayout()
exportGraph("./figures/graph.pdf")