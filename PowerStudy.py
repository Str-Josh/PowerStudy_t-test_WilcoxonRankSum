# -*- coding: utf-8 -*-
"""
Created on Fri Oct  4 13:06:23 2024

@author: jcarter11
"""

# Simulate t-test vs Wilcoxon Rank Sum Test
#from scipy.stats import ttest_ind
import numpy as np
from scipy.stats import norm, shapiro, ttest_ind, ranksums

alpha = 0.05


def ttest(x, y):
    p = ttest_ind(x, y).pvalue
    if p < alpha:
        return (1, p)
    else:
        return (0, p)

def rank_sum(x, y):
    p = ranksums(x, y).pvalue
    if p < alpha:
        return (1, p)
    else:
        return (0, p)

def simulation(n, s_coefficient):
    rejection_results = []
    u = s_coefficient  # the coef for s
    if s_coefficient is None:
        u = range(0, norm.rvs(size=20)[0])  # just some random range haha
    s = lambda k: k * 1/5
    for j in range(0, n):
        x = norm.rvs(loc=0, size=50)
        y = norm.rvs(loc=s(u), size=50)
        wrs = rank_sum(x, y)
        tts = ttest(x, y)
        WRS = wrs[0]
        TTS = tts[0]
        if WRS > TTS:
            #print("Rank Sum suggests to reject the null hypothesis but t-test doesn't")
            print("only Rank Sum -- reject H0")
            print(f"\t\tWRS: {wrs[1]}")
            print(f"\t\tt-test: {tts[1]}")
            rejection_results.append(0)
        elif WRS < TTS:
            #print("t-test suggests to reject the null hypothesis but Rank Sum doesn't")
            print("only t-test -- reject H0")
            print(f"\t\tt-test: {tts[1]}")
            print(f"\t\tWRS: {wrs[1]}")
            rejection_results.append(0)
        elif WRS == 1 and TTS == 1:
            #print("Both suggest to reject the null hypothesis")
            print("Both -- Reject H0")
            rejection_results.append(1)
        else:
            #print("Both suggest cannot reject the null hypothesis")
            print("Both -- don't reject")
            rejection_results.append(1)
    return rejection_results


if __name__ == "__main__":
    n = 40
    res = simulation(n, 3)
    avg_agree = sum(res)/len(res)
    print(avg_agree)
    num_differ = avg_agree*n
    pass
