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
    # 得到DataFrame长度
    n = dataframe.shape[1]
    # 初始化p值矩阵
    pvalue_matrix = np.ones((n, n))
    # 抽取列的名称
    keys = dataframe.keys()
    # 初始化强协整组
    pairs = []
    # 对于每一个i
    for i in range(n):
        # 对于大于i的j
        for j in range(i+1, n):
            # 获取相应的两只股票的价格Series
            stock1 = dataframe[keys[i]]
            stock2 = dataframe[keys[j]]
            # 分析它们的协整关系
            result = sm.tsa.stattools.coint(stock1, stock2)
            # 取出并记录p值
            pvalue = result[1]
            pvalue_matrix[i, j] = pvalue
            # 如果p值小于0.05
            if pvalue < 0.05:
                # 记录股票对和相应的p值
                pairs.append((keys[i], keys[j], pvalue))
    # 返回结果
    return pvalue_matrix, pairs

def trading(z_score, s0, spread):
    profit = [0]*len(z_score)
    cum_profit = 0
    position = [0]*len(z_score)
    cur_pos = 0
    for i in range(1, len(z_score)):

        if z_score[i]<-s0 and cur_pos==0:
            # buy spread
            cum_profit += spread[i]
            cur_pos = 1
        if z_score[i]>s0 and cur_pos==0:
            # short-sell spread
            cum_profit -= spread[i]
            cur_pos = -1
        if z_score[i]*z_score[i-1]<0 and cur_pos==-1: # zero-crossing
            cum_profit -= spread[i]
            cur_pos = 0
        if z_score[i]*z_score[i-1]<0 and cur_pos==1:
            cum_profit += spread[i]
            cur_pos = 0

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