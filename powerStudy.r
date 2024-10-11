alpha = 0.05

ttest = function(x, y) {
    p = t.test(x,y, alternative="less", mu=0, paired = FALSE, conf.level = 0.95)$p.value
    if (p < alpha) {
        return(1)
    }
    else {
        return(0)
    }
}

rank_sum = function(x, y) {
    p = wilcox.test(x, y, alternative="less", mu = 0, paired = FALSE, conf.level = 0.95)$p.value
    if (p < alpha) {
        return (1)
    }
    else {
        return (0)
    }
}

plot_power = function(power_normal, power_wilcoxon) {
	x = seq(-1, 1, 0.1)
    #plot(power_normal, power_wilcoxon)
}

simulation = function(n, k) {
    rejection_results_normal = vector(length = 0)
    rejection_results_wilcoxon = vector(length = 0)
    #k = 0
    s = k*(1/5)
    for (j in 1:n) {
        x = rnorm(50, 0, 1)
        y = rnorm(50, 0, 1)
        WRS = rank_sum(x, y)
        TTS = ttest(x, y)
        if (WRS > TTS) {
            #print("only Rank Sum -- reject H0")
	        rejection_results_wilcoxon = c(rejection_results_wilcoxon, 1)
	        rejection_results_normal = c(rejection_results_normal, 0)
        }
        else if (WRS < TTS) {
            #print("only t-test -- reject H0")
	        rejection_results_wilcoxon = c(rejection_results_wilcoxon, 0)
	        rejection_results_normal = c(rejection_results_normal, 1)
        }
        else if (WRS == 1 & TTS == 1) {
            #print("Both -- Reject H0")
	        rejection_results_wilcoxon = c(rejection_results_wilcoxon, 1)
	        rejection_results_normal = c(rejection_results_normal, 1)
        }
        else {
            #print("Both -- don't reject")
	        rejection_results_wilcoxon = c(rejection_results_wilcoxon, 0)
	        rejection_results_normal = c(rejection_results_normal, 0)
        }
	}
	num_rejections_normal   = sum(rejection_results_normal)
	num_rejections_wilcoxon = sum(rejection_results_wilcoxon)
	power_normal = num_rejections_normal / n
	power_wilcoxon = num_rejections_wilcoxon / n
	## plot_power(power_normal, power_wilcoxon)
	## return(rejection_results)
    
    # Returns the power estimated by # rejections / n
    return(c(power_normal, power_wilcoxon))
}


n = 50
#power_data_normal = vector(length=0)
#power_data_wilcoxon = vector(length=0)
#k_value = rep(0:10, each=2)
#plot(0:10, power_data)

simulation_results = sapply(0:10, function(k) simulation(500, k))
power_data_normal = simulation_results[1, ]
power_data_wilcoxon = simulation_results[2, ]
k = rep(0:10, each=2)
plot(0:10, power_data_normal, col="green", pch=19, ylim=c(0, 1), ylab="POWER", xlab="K Values")
points(0:10, power_data_wilcoxon, col="blue", pch=19)



#for (k in 0:5) {
    #res = simulation(n, k)
    #print(res)
    #power_data_normal = c(power_data_normal, )
    #power_data_wilcoxon = c(power_data_wilcoxon, )
#}
#avg_agree = sum(res)/length(res)
#print(avg_agree)
#num_differ = avg_agree*n

