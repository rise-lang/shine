import matplotlib as mpl
mpl.use('pgf')
mpl.rcParams.update({
    "pgf.texsystem": "pdflatex",
    'font.family': 'serif',
    'font.size': 16,
    'text.usetex': True,
    'pgf.rcfonts': False,
})
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import matplotlib.transforms as transforms
import numpy as np
import pandas as pd
import math
from cycler import cycler
import scipy.optimize as opt
from functools import reduce
from itertools import accumulate

plots = [
  ("unguided-blocking", (4, 3)),
  ("unguided-parallel", (4, 3)),
  ("guided-blocking", (4, 3)),
  ("guided-parallel", (6, 3)),
]

plt.rc('axes', prop_cycle=
       (cycler('color', ['#1E88E5', '#FFC107', '#004D40']) +
        cycler('linestyle', ['-']) *
        cycler('marker', ['.', '+', 'x'])))
figsize = (10, 6)
fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=figsize, #tight_layout = {'pad': 0},
        sharex=True, sharey=False, constrained_layout=True)
axes = [ax1, ax2, ax3, ax4]

for i, ax in enumerate(axes):
    ax.set_title(plots[i][0].replace('-', ' '))

def plotOne(i, name, ax):
    data = pd.read_csv(name + '.csv', sep=' ')

    data['skit'] = list(zip(data.sketch, data.iteration))
    data['x'] = list(accumulate(zip(data.sketch, data.iteration),
        lambda acc, si: acc if si[1] == 0 else acc + 1,
        initial=0))[1:]
    print(data)
    # [i for (i, _) in enumerate([v for v in
    #    zip(data.sketch, data.iteration) if (v[0] == 0) or (v[1] != 0)])]

    # insert NaNs at discontinuities
    discontinuities = list(set(data.sketch.values))
    nans = pd.DataFrame({
        "sketch": [s for s in discontinuities],
        "iteration": [max(data[data["sketch"] == s].iteration.values) + 1 for s in discontinuities],
        "x": [max(data[data["sketch"] == s].x.values) for s in discontinuities],
        "nodes": [np.nan for s in discontinuities],
        "classes": [np.nan for s in discontinuities],
        "rules": [np.nan for s in discontinuities],
        })
    print(nans)
    nans['skit'] = list(zip(nans.sketch, nans.iteration))
    frame = pd.concat([data, nans]).sort_values(by=['x', 'skit'])
    print(frame)

    frame.plot("x", ["nodes", "classes", "rules"], ax=ax)

    maxColor='grey'
    maxY = max([max(data.nodes.values), max(data.classes.values), max(data.rules.values)])
    maxYC = math.ceil(maxY / 1000) * 1000
    (unitPrefixS, unitPrefixN) = ('M', 1000000) if (maxY > 1000000) or (name ==
            "unguided-parallel") else ('K', 1000) 
    prefixValue = lambda v: str(int(v / unitPrefixN)) + unitPrefixS
    print("max Y: ", maxY, " ~ ", maxYC)
    # plot max size
    if name != "unguided-parallel":
        ax.axhline(y=maxYC, color=maxColor, linestyle='--')
    if name == "guided-blocking":
        trans = transforms.blended_transform_factory(
            ax.get_yticklabels()[0].get_transform(), ax.transData)
        ax.text(0, maxYC, prefixValue(maxYC), color=maxColor, transform=trans, ha='right', va='center')

    # curve fitting
    def curveFun(x, a, b, c):
        return a * np.exp(-b * x) + c
    def plotCurve(xs, ys):
        optimizedParameters, pcov = opt.curve_fit(curveFun, xs, ys, maxfev=50000)
        xsbis = np.append(xs, [max(xs) + 1])
        ax.plot(xsbis, curveFun(xsbis, *optimizedParameters), linestyle=':')
    if name == "unguided-parallel":
        plotCurve(data['iteration'].values, data['nodes'].values)
        plotCurve(data['iteration'].values, data['classes'].values)
        plotCurve(data['iteration'].values, data['rules'].values)

    # plot sketch guides
    guideColor='#5500d4ff'
    periods = data[data.sketch.diff() != 0].x.values
    ax.vlines(periods[1:], ymax=maxYC, ymin=0, color=guideColor, linestyle='--')
    for (i, item) in enumerate(periods[1:]):
        ax.text(y=0.90*maxYC, x=item+0.5, s='sketch', color=guideColor)
        ax.text(y=0.80*maxYC, x=item+0.5, s='guide', color=guideColor)
        ax.text(y=0.70*maxYC, x=item+0.5, s='n°'+str(i+1), color=guideColor)

    ax.yaxis.set_major_formatter(FuncFormatter(lambda n, _: prefixValue(n)))
    if name == "unguided-parallel":
        ax.set_ylim((0, 4000000))

for (i, (name, figsize)), ax in zip(enumerate(plots), axes):
    plotOne(i, name, ax)

for ax in axes:
  ax.set_xlabel("iterations")
# plt.xticks(np.arange(data.skit.size), [i for (_, i) in data.skit.values])

ax1.get_legend().remove()
patches, _labels = ax1.get_legend_handles_labels()
patches.append(mpl.lines.Line2D([0], [0], color="black", linestyle=":"))
ax2.legend(patches, ["e-nodes", "e-classes", "rules", "estimate"])
ax3.get_legend().remove()
ax4.get_legend().remove()
plt.savefig('results.pgf')
plt.savefig('result.png')
