import numpy as np
import pandas as pd
import seaborn as sns
import statsmodels
import matplotlib.pyplot as plt
import statsmodels.api as sm
from sklearn.linear_model import LinearRegression
from statsmodels.tsa.stattools import adfuller
from statsmodels.graphics.gofplots import qqplot
from scipy.stats import norm
from sklearn.mixture import GaussianMixture
from sklearn.neighbors import KernelDensity
from pykalman import KalmanFilter


def find_cointegrated_pairs(dataframe):
    n = dataframe.shape[1]
    pvalue_matrix = np.ones((n, n))
    keys = dataframe.keys()
    pairs = []
    for i in range(n):
        for j in range(i+1, n):
            stock1 = dataframe[keys[i]]
            stock2 = dataframe[keys[j]]
            result = sm.tsa.stattools.coint(stock1, stock2)
            pvalue = result[1]
            pvalue_matrix[i, j] = pvalue
            if pvalue < 0.05:
                pairs.append((keys[i], keys[j], pvalue))
    return pvalue_matrix, pairs

def trading_basic(z_score, s0, spread):
    profit = [0]*len(z_score)
    cum_profit = 0
    position = [0]*len(z_score)
    cur_pos = 0
    count = 0
    for i in range(1, len(z_score)):

        if z_score[i]<-s0 and cur_pos==0:
            # buy spread
            cum_profit += spread[i]
            count+=1
            cur_pos = 1
        elif z_score[i]>s0 and cur_pos==0:
            # short-sell spread
            cum_profit -= spread[i]
            count+=1
            cur_pos = -1
        elif z_score[i]*z_score[i-1]<0 and cur_pos==-1: # zero-crossing
            cum_profit += spread[i]
            cur_pos = 0
            count=0
        elif z_score[i]*z_score[i-1]<0 and cur_pos==1:
            cum_profit -= spread[i]
            cur_pos = 0


        profit[i] = cum_profit
        position[i] = cur_pos
    return position, profit, cum_profit

def trading(z_score, s0, spread):
    profit = [0]*len(z_score)
    cum_profit = 0
    position = [0]*len(z_score)
    cur_pos = 0
    count = 0
    for i in range(1, len(z_score)):

        if z_score[i]<-s0:
            # buy spread
            cum_profit += spread[i]
            count+=1
            cur_pos = 1
        elif z_score[i]>s0:
            # short-sell spread
            cum_profit -= spread[i]
            count+=1
            cur_pos = -1
        elif z_score[i]*z_score[i-1]<0 and cur_pos==-1: # zero-crossing
            cum_profit -= spread[i]*count
            cur_pos = 0
            count=0
        elif z_score[i]*z_score[i-1]<0 and cur_pos==1:
            cum_profit += spread[i]*count
            cur_pos = 0
            count=0

        profit[i] = cum_profit
        position[i] = cur_pos
    return position, profit, cum_profit

def plot_trading_result(z_score, position, profit, s0, val_num):
	plt.figure(figsize=(16,9))
	plt.subplot(3,1,1)
	plt.plot(z_score)
	plt.axhline(z_score.mean(), color="black")
	plt.axhline(s0, color="red", linestyle="--")
	plt.axhline(-s0, color="green", linestyle="--")
	plt.legend(["z-score", "mean", "{}".format(s0), "{}".format(-s0)])
	plt.ylabel("Spread")

	plt.subplot(3,1,2)
	plt.plot(position, "m")
	plt.ylabel("Position")

	plt.subplot(3,1,3)
	plt.plot(profit, "r")
	plt.ylabel("Cum P&L")
	plt.axvline(x=len(z_score)-val_num,color="darkgreen")
	plt.show()

def main():
	pass
if __name__ == '__main__':
    main()