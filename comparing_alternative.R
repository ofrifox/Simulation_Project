
############## madad 1 - mean_time ############################### 
#compare current state and alternative1 - madad  1 
CS_alt1_1_tTest <- t.test(x = madad1$mean_time , y = madad1_a1$mean_time, alternative = "two.sided", paired = TRUE, var.equal= TRUE, conf.level = 0.99)
print(CS_alt1_1_tTest) 


#compare current state and alternative2 - madad 1 
CS_alt2_1_tTest <- t.test(x = madad1$mean_time , y = madad1_a2$mean_time, alternative = "two.sided", paired = TRUE, var.equal= TRUE, conf.level = 0.99)
print(CS_alt2_1_tTest) 

#compare alternative1 and alternative2 - madad 1 
alt1_alt2_1_tTest <- t.test(x = madad1_a1$mean_time , y = madad1_a2$mean_time, alternative = "two.sided", paired = TRUE, var.equal= TRUE, conf.level = 0.99)
print(alt1_alt2_1_tTest)




t_test_madad1<-c(CS_alt1_1_tTest$conf.int[1], CS_alt1_1_tTest$conf.int[2], CS_alt2_1_tTest$conf.int[1], CS_alt2_1_tTest$conf.int[2], alt1_alt2_1_tTest$conf.int[1], alt1_alt2_1_tTest$conf.int[2])
madad1_t_test <- matrix(t_test_madad1,ncol=2,byrow=TRUE)
colnames(madad1_t_test)<- c("confidence interval bottom", "confidence interval upper")
rownames(madad1_t_test)  <- c("Current state vs Alternative 1","Current state vs Alternative 2","Alternative 1 vs Alternative 2")
madad1_t_test <- as.table(madad1_t_test)
madad1_t_test


############## madad 2 - AVG Simulation time ############################### 

#compare current state and alternative1 - madad 2
CS_alt1_2_tTest <- t.test(x = madad2$AVG_Simulation_time , y = madad2_a1$AVG_Simulation_time, alternative = "two.sided", paired = TRUE, var.equal= TRUE, conf.level = 0.99)
print(CS_alt1_2_tTest) 

#compare current state and alternative2 - madad 2 
CS_alt2_2_tTest <- t.test(x = madad2$AVG_Simulation_time , y = madad2_a2$AVG_Simulation_time, alternative = "two.sided", paired = TRUE, var.equal= TRUE, conf.level = 0.99)
print(CS_alt2_2_tTest) 

#compare alternative1 state and alternative2 - madad 2
alt1_alt2_2_tTest <- t.test(x = madad2_a1$AVG_Simulation_time , y = madad2_a2$AVG_Simulation_time, alternative = "two.sided", paired = TRUE, var.equal= TRUE, conf.level = 0.99)
print(alt1_alt2_2_tTest) 


t_test_madad2<-c(CS_alt1_2_tTest$conf.int[1], CS_alt1_2_tTest$conf.int[2], CS_alt2_2_tTest$conf.int[1], CS_alt2_2_tTest$conf.int[2], alt1_alt2_2_tTest$conf.int[1], alt1_alt2_2_tTest$conf.int[2])
madad2_t_test <- matrix(t_test_madad2,ncol=2,byrow=TRUE)
colnames(madad2_t_test)<- c("confidence interval bottom", "confidence interval upper")
rownames(madad2_t_test)  <- c("Current state vs Alternative 1","Current state vs Alternative 2","Alternative 1 vs Alternative 2")
madad2_t_test <- as.table(madad2_t_test)
madad2_t_test


############## madad 3 - bus Proportion ############################### 

#compare current state and alternative1 - madad 3 
CS_alt1_3_tTest <- t.test(x = madad3$bus_Proportion , y = madad3_a1_t$bus_Proportion, alternative = "two.sided", paired = TRUE, var.equal= TRUE, conf.level = 0.99)
print(CS_alt1_3_tTest)

#compare current state and alternative2 - madad 3 
CS_alt2_3_tTest <- t.test(x = madad3$bus_Proportion , y = madad3_a2$bus_Proportion, alternative = "two.sided", paired = TRUE, var.equal= TRUE, conf.level = 0.99)
print(CS_alt2_3_tTest)

#compare alternative1 state and alternative2 - madad 3
alt1_alt2_3_tTest <- t.test(x = madad3_a1_t$bus_Proportion , y = madad3_a2$bus_Proportion, alternative = "two.sided", paired = TRUE, var.equal= TRUE, conf.level = 0.99)
print(alt1_alt2_3_tTest)



t_test_madad3<-c(CS_alt1_3_tTest$conf.int[1], CS_alt1_3_tTest$conf.int[2], CS_alt2_3_tTest$conf.int[1], CS_alt2_3_tTest$conf.int[2], alt1_alt2_3_tTest$conf.int[1], alt1_alt2_3_tTest$conf.int[2])
madad3_t_test <- matrix(t_test_madad3,ncol=2,byrow=TRUE)
colnames(madad3_t_test)<- c("confidence interval bottom", "confidence interval upper")
rownames(madad3_t_test)  <- c("Current state vs Alternative 1","Current state vs Alternative 2","Alternative 1 vs Alternative 2")
madad3_t_test <- as.table(madad3_t_test)
madad3_t_test





# #
# library("xlsx")
# write.xlsx(x=madad1_t_test, file= "madad_t_test.xlsx", sheetName="madad1_t_test", append=TRUE)
# write.xlsx(x=madad2_t_test, file= "madad_t_test.xlsx", sheetName="madad2_t_test", append=TRUE)
# write.xlsx(x=madad3_t_test, file= "madad_t_test.xlsx", sheetName="madad3_t_test", append=TRUE)