import numpy as np
import matplotlib.pyplot as plt
np.x1=np.array([1189.01,1185.0,1012.163,1873.221,194.2559])
np.y1=np.array([14.45688,979.85659999999996,639.15629999999,249.173599999,67.10436])
np.x2=np.array([1188.8416489474798,1184.843265982,1012.074063414,1873.077105230,194.30567379144])
np.y2=np.array([14.855950959,980.058587129,639.232680302,249.412057655,67.3004825209])
plt.figure()
ax = plt.gca()
ax.quiver(np.x1, np.y1, np.x2-np.x1, np.y2-np.y1, angles='xy', scale_units='xy', scale=10)
ax.set_xlim([0, 1472])
ax.set_ylim([0, 2184])
plt.draw()
plt.show()