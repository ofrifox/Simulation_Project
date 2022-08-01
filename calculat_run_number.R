
Accuracy <- function(t, n, v, avr){
  delta <- t*(v/(sqrt(n)))
  return(delta/avr)
}

#calculation of accuracy per indicator and alternative
n0 = 15
alfa <- 0.03
gamma <- 0.03
RelativeAccuracy <- (gamma)/(1+gamma)
t <- qt(0.985,n0-1)


#calculation of relative accuracy for n0=15
tableValues <- c(Accuracy(t,n0,(madad_1_STDEV)^2, madad_1_Mean),Accuracy(t,n0,(madad2_STDEV)^2, madad2_Mean),Accuracy(t,n0,(madad_3_stdev)^2, madad_3_mean),
                 Accuracy(t,n0,(madad_1_STDEV_a1)^2, madad_1_Mean_a1),Accuracy(t,n0,(madad2_STDEV_a1)^2, madad2_Mean_a1),Accuracy(t,n0,(madad_3_stdev_a1)^2, madad_3_mean_a1),
                 Accuracy(t,n0,(madad_1_STDEV_a2)^2, madad_1_Mean_a2),Accuracy(t,n0,(madad2_STDEV_a2)^2, madad2_Mean_a2),Accuracy(t,n0,(madad_3_stdev_a2)^2, madad_3_mean_a2))


checkAccuracy <- matrix(tableValues,ncol=3,byrow=TRUE)
colnames(checkAccuracy) <- c("Madad1", "madad2", "madad3")
rownames(checkAccuracy) <- c("Current state","Alternative1","Alternative2")
checkAccuracy <- as.table(checkAccuracy)
checkAccuracy



#calculate of how many runs we need to add - Indicator 2 A current state
nnew_madad2_a2 = n0 * (((Accuracy(t,n0,(madad2_STDEV_a2)^2, madad2_Mean_a2)*madad2_Mean_a2)/(madad2_Mean_a2*RelativeAccuracy))^2)
nnew_madad2_a2
# #calculate of how many runs we need to add - Indicator 2 Alternative 2 
nnew_madad2_currentstate = n0 * (((Accuracy(t,n0,(madad2_STDEV)^2, madad2_Mean)*madad2_Mean)/(madad2_Mean*RelativeAccuracy))^2)
nnew_madad2_currentstate

 
#calculation of relative accuracy for n1 = 26
n1 <- 26
t1 <- qt(0.985,(n1-1))


tableValues1 <- c(Accuracy(t1,n1,(madad_1_STDEV)^2, madad_1_Mean),Accuracy(t1,n1,(madad2_STDEV)^2, madad2_Mean),Accuracy(t1,n1,(madad_3_stdev)^2, madad_3_mean),
                 Accuracy(t1,n1,(madad_1_STDEV_a1)^2, madad_1_Mean_a1),Accuracy(t1,n1,(madad2_STDEV_a1)^2, madad2_Mean_a1),Accuracy(t1,n1,(madad_3_stdev_a1)^2, madad_3_mean_a1),
                 Accuracy(t1,n1,(madad_1_STDEV_a2)^2, madad_1_Mean_a2),Accuracy(t1,n1,(madad2_STDEV_a2)^2, madad2_Mean_a2),Accuracy(t1,n1,(madad_3_stdev_a2)^2, madad_3_mean_a2))


checkAccuracy1 <- matrix(tableValues1,ncol=3,byrow=TRUE)
colnames(checkAccuracy1)<- c("Madad1", "madad2", "madad3")
rownames(checkAccuracy1)  <- c("Current state","Alternative1","Alternative2")
checkAccuracy1 <- as.table(checkAccuracy1)
checkAccuracy1



#calculate of how many runs we need to add - Indicator 3 Alt 2 
# #calculate of how many runs we need to add - Indicator 2 Alternative 2 
nnew_madad2_a21 = n1 * (((Accuracy(t1,n1,(madad2_STDEV_a2)^2, madad2_Mean_a2)*madad2_Mean_a2)/(madad2_Mean_a2*RelativeAccuracy))^2)
nnew_madad2_a21
#calculate of how many runs we need to add - Indicator 2 A current state
nnew_madad2_currentstate1 = n1 * (((Accuracy(t1,n1,(madad2_STDEV)^2, madad2_Mean)*madad2_Mean)/(madad2_Mean*RelativeAccuracy))^2)
nnew_madad2_currentstate1
# #calculate of how many runs we need to add - Indicator 2 Alternative 1 
nnew_madad2_currentstate1 = n1 * (((Accuracy(t1,n1,(madad2_STDEV)^2, madad2_Mean)*madad2_Mean)/(madad2_Mean*RelativeAccuracy))^2)
nnew_madad2_currentstate1


#calculation of relative accuracy for n2 = 35 
n2 <- 35
t2 <- qt(0.985,(n2-1))

tableValues2 <-  c(Accuracy(t2,n2,(madad_1_STDEV)^2, madad_1_Mean),Accuracy(t2,n2,(madad2_STDEV)^2, madad2_Mean),Accuracy(t2,n2,(madad_3_stdev)^2, madad_3_mean),
                 Accuracy(t2,n2,(madad_1_STDEV_a1)^2, madad_1_Mean_a1),Accuracy(t2,n2,(madad2_STDEV_a1)^2, madad2_Mean_a1),Accuracy(t2,n2,(madad_3_stdev_a1)^2, madad_3_mean_a1),
                 Accuracy(t2,n2,(madad_1_STDEV_a2)^2, madad_1_Mean_a2),Accuracy(t2,n2,(madad2_STDEV_a2)^2, madad2_Mean_a2),Accuracy(t2,n2,(madad_3_stdev_a2)^2, madad_3_mean_a2))
checkAccuracy2 <- matrix(tableValues2,ncol=3,byrow=TRUE)
colnames(checkAccuracy2)<- c("Madad1", "madad2", "madad3")
rownames(checkAccuracy2) <- c("Current state","Alternative1","Alternative2")
checkAccuracy2 <- as.table(checkAccuracy2)
checkAccuracy2


# #calculate of how many runs we need to add - Indicator 2 Alternative 1 
nnew_madad2_currentstate2 = n2 * (((Accuracy(t2,n2,(madad2_STDEV)^2, madad2_Mean)*madad2_Mean)/(madad2_Mean*RelativeAccuracy))^2)
nnew_madad2_currentstate2



#calculation of relative accuracy for n3= 36
n3 <- 36
t3 <- qt(0.985,(n3-1))

tableValues3 <-  c(Accuracy(t3,n3,(madad_1_STDEV)^2, madad_1_Mean),Accuracy(t3,n3,(madad2_STDEV)^2, madad2_Mean),Accuracy(t3,n3,(madad_3_stdev)^2, madad_3_mean),
                   Accuracy(t3,n3,(madad_1_STDEV_a1)^2, madad_1_Mean_a1),Accuracy(t3,n3,(madad2_STDEV_a1)^2, madad2_Mean_a1),Accuracy(t3,n3,(madad_3_stdev_a1)^2, madad_3_mean_a1),
                   Accuracy(t3,n3,(madad_1_STDEV_a2)^2, madad_1_Mean_a2),Accuracy(t3,n3,(madad2_STDEV_a2)^2, madad2_Mean_a2),Accuracy(t3,n3,(madad_3_stdev_a2)^2, madad_3_mean_a2))
checkAccuracy3 <- matrix(tableValues3,ncol=3,byrow=TRUE)
colnames(checkAccuracy3)<- c("Madad1", "madad2", "madad3")
rownames(checkAccuracy3) <- c("Current state","Alternative1","Alternative2")
checkAccuracy3 <- as.table(checkAccuracy3)
checkAccuracy3

