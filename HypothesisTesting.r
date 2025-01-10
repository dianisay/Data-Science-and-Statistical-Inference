# Final Project
# Diana Paola Ayala Roldán (A01365809)
# José Rubén Villicaña Ibargüengoytia (A01654347)
# Carlos Cristóbal Rubio Hernández (A01745245)
# Clear all
graphics.off() #Clean Plots
rm(list = ls()) #Clean global environment
cat("\014") #Clean Console
set.seed(123)
# Obtaining needed libraries
library(ggplot2)
library(pwr)
library(boot)
library(boot.pval)
library(haven)
#########################################################################
#######
################################# Functions
####################################
#########################################################################
#######
# Function 1) Delete unnecesary data
erasedata<-function(variables,dataset){
filas<-c()
con<-1
borrar<-0
for(i in (1:nrow(dataset))){
for (j in variables){
if (dataset[i,j]==6 | dataset[i,j]==7 | is.na(data[i,j])){
borrar=1
}} if (
borrar==1){
filas[con]<-i
con=con+1
}
borrar=0
}
dataset<-dataset[-filas, ]
return (dataset)
}
# Function 2) Erase unessesary columns
select_columns <- function(variables, dataset) {
dataset_selected <- dataset[, variables, drop = FALSE]
return(dataset_selected)
}
# Function 2) Generate data based on the multinomial distribution of each group and
respecting the original sample sizes
generate_data<-function(data,mle){
gen_g1<-rmultinom(1,n1,c(mle[[1]],mle[[2]],mle[[3]],mle[[4]],mle[[5]]))
gen_g2<-rmultinom(1,n2,c(mle[[6]],mle[[7]],mle[[8]],mle[[9]],mle[[10]]))
gen=rbind(gen_g1,gen_g2)
return(gen)
}
# Function 3) Calculate the difference in location between the groups
location_boot<-function(data){
boot_g1<-c()
boot_g2<-c()
for (i in 1:5){
boot_g1<-append(boot_g1,rep(i,data[i][1]))
boot_g2<-append(boot_g2,rep(i,data[i+5][1]))
}
# Performing the parametric test again
test_boot=wilcox.test(boot_g1,boot_g2,conf.int=T)
return(test_boot$statistic)
}
# Function 4) Bootstrap function for Hypothesis 1
bootstrap_h1 <- function(data, variables, num_iterations = 10000) {
set.seed(123) # Set a seed for reproducibility
statistics <- numeric(num_iterations)
for (i in 1:num_iterations) {
# Sample with replacement
sampled_data <- data[sample(nrow(data), replace = TRUE), ]
# Create grouping variable for hostile environment
sampled_data$hostile <- apply(sampled_data[, c("C3A", "C3B", "C3C", "C3D",
"C3E")], 1, function(row) any(row %in% c(1, 2)))
# Create grouping variable for psychological impact
sampled_data$psy <- apply(sampled_data[, c("C44", "C45", "C46", "C47")], 1,
function(row) any(row %in% c(1)))
# Perform non-parametric test: Chi-square Test
result <- chisq.test(sampled_data$hostile, sampled_data$psy)
# Store the test statistic
statistics[i] <- result$statistic
}
return(statistics)
}
# Function 5) Bootstrap function for Hypothesis 2
bootstrap_h2 <- function(data, variable, group_variable, num_iterations = 1000) {
set.seed(123) # Set a seed for reproducibility
statistics <- numeric(num_iterations)
for (i in 1:num_iterations) {
# Sample with replacement
sampled_data <- data[sample(nrow(data), replace = TRUE), ]
# Perform U Mann-Whitney test on the sampled data
result <- wilcox.test(sampled_data[[variable]] ~ sampled_data[[group_variable]])
# Store the test statistic
statistics[i] <- result$statistic
}
return(statistics)
}
# Function 6) Bootstrap function for Hypothesis 3
bootstrap_h3<- function(data, variable, group_variable, num_iterations = 1000) {
set.seed(123) # Set a seed for reproducibility
statistics <- numeric(num_iterations)
for (i in 1:num_iterations) {
# Sample with replacement
sampled_data <- data[sample(nrow(data), replace = TRUE), ]
# Perform Wilcoxon rank-sum test on the sampled data
result <- wilcox.test(sampled_data[[variable]] ~ sampled_data[[group_variable]])
# Store the test statistic
statistics[i] <- result$statistic
}
return(statistics)
}
#########################################################################
#######
################################### Data
#######################################
#########################################################################
#######
data<-read_sas("C:/Users/jrube/Desktop/Final Proyect Data
Science/Data/nsapsn_puf.sas7bdat")
#########################################################################
#######
################################# Hypothesis 1
#################################
#########################################################################
#######
# Does exposure to a hostile environment before adoption have a significant
# psychological impact on adopted children?
## Variables:
# C3A, C3B, C3C, C3D, C3E - likelihood of hostile environment
# C44, C45, C46, C47 - presence of specific psychological disorders
# Grouping the vairables C3A, C3B, C3C, C3D and C3E, which indicate the
# likelihood of hostile environment, sinc eit asks for different kinds
# of abuse the children may have suffered in the past, before adopting
# them. If any of the variables' answer is "Likely" or "Very likely",
# it will belong to the group of hostile environment, and just if all
# the variables' answer were either "Child adopted at birth", "Unlikely"
# or "Very Unlikely" they will belong to the "No Hostile" group"
# & (C3C==0 | C3C==3 | C3C==4) & (C3D==0 | C3D==3 | C3D==4)
# & (C3E==0 | C3E==3 | C3E==4) & (C44!=6 & C44!=7)
# & (C45!=6 & C45!=7) & (C46!=6 & C46!=7)
# & (C47!=6 & C47!=7)
vars1<-c("C3A", "C3B", "C3C", "C3D", "C3E", "C44", "C45", "C46", "C47")
data1<-erasedata(vars1,data)
data1 <- select_columns(vars1, data1)
# For the non parametric chi-square test, two new variables must be added that
# represent the grouping, the same as before, if the child was exposed to a
# hostile environment and if the sample child have a significant
# psychological impact
host<-c()
for(i in (1:nrow(data1))){
if ((data1$C3A[i]==0 | data1$C3A[i]==3 | data1$C3A[i]==4) &
(data1$C3B[i]==0 | data1$C3B[i]==3 | data1$C3B[i]==4) &
(data1$C3C[i]==0 | data1$C3C[i]==3 | data1$C3C[i]==4) &
(data1$C3D[i]==0 | data1$C3D[i]==3 | data1$C3D[i]==4) &
(data1$C3E[i]==0 | data1$C3E[i]==3 | data1$C3E[i]==4)){
host[i]=0}
if (data1$C3A[i]==1 | data1$C3A[i]==2 | data1$C3B[i]==1 | data1$C3B[i]==2 |
data1$C3C[i]==1 | data1$C3C[i]==2 | data1$C3D[i]==1 | data1$C3D[i]==2 |
data1$C3E[i]==1 | data1$C3E[i]==2){
host[i]=1}
}
data1<-cbind(data1,host)
psy<-c()
for(i in (1:nrow(data1))){
if (data1$C44[i]==0 & data1$C45[i]==0 & data1$C46[i]==0 & data1$C47[i]==0){
psy[i]=0}
if (data1$C44[i]==1 | data1$C45[i]==1 | data1$C46[i]==1 | data1$C47[i]==1){
psy[i]=1}
}
data1<-cbind(data1,psy)
nohostile<-subset(data1, (C3A==0 | C3A==3 | C3A==4) & (C3B==0 | C3B==3 | C3B==4)
& (C3C==0 | C3C==3 | C3C==4) & (C3D==0 | C3D==3 | C3D==4)
& (C3E==0 | C3E==3 | C3E==4))
hostile<-subset(data1, (C3A==1 | C3A==2 | C3B==1 | C3B==2
| C3C==1 | C3C==2 | C3D==1 | C3D==2
| C3E==1 | C3E==2))
# The aim is to know if that past had an psychological impact on the children,
# another grouping will be made by adding a new variable (prob) which
# indicates if the Sample Child developed any kind of
# Disorder (PTSD, Attachment, ODD or Conduct), again, if any of the
# variables from C44 to C47 is "Yes" in the new variable, just if all the
# variables were marked as "No", the new variable prob will be marked as "No"
prob<-c()
for(i in (1:nrow(nohostile))){
if (nohostile$C44[i]==0 & nohostile$C45[i]==0
& nohostile$C46[i]==0 & nohostile$C47[i]==0){
prob[i]=0}
else
prob[i]=1
}
nohostile<-cbind(nohostile,prob)
prob<-c()
for(i in (1:nrow(hostile))){
if (hostile$C44[i]==0 & hostile$C45[i]==0
& hostile$C46[i]==0 & hostile$C47[i]==0){
prob[i]=0}
else
prob[i]=1
}
hostile<-cbind(hostile,prob)
# Analyzing the variables to be worked with. As they are Categorical Ordinal,
# bar plots are presented.
ta_ho<-table(hostile$prob)
ta_nh<-table(nohostile$prob)
barplot(ta_ho,main="Hostile Enviroment: \nDoes the child developed a phycological
disorder?",
col="lightslateblue",names.arg=c("No", "Yes"),ylab="Frequency")
barplot(ta_nh,main="No Hostile Enviroment: \nDoes the child developed a phycological
disorder?",
col="deeppink3",names.arg=c("No", "Yes"),ylab="Frequency")
# Performing the non-parametric test: Chi-square Test
res1<-chisq.test(data1$host,data1$psy)
pwr1=pwr.chisq.test(w = 0.1, N = nrow(data1),df = 1, sig.level = 0.05)
{if (res1$p.value>0.05){
cat(paste("With a P-Value of ",res1$p.value," H0 can not be rejected, there is no significant
correlation within the grouping. With a power of: ",pwr1$power))
cat("\n\nThe fact that the adopted child was exposed to a hostile environment prior
adoption, does not have a significant psychological impact on the S.C.")}
else{
cat(paste("With a P-Value of ",res1$p.value," H0 must be rejected, there is significant
correlation within the grouping. With a power of: ",pwr1$power))
cat("\n\nThe fact that the adopted child was exposed to a hostile environment prior
adoption, has a significant psychological impact on the S.C.")}}
# BOOTSTRAP
# Run bootstrap for Hypothesis 1
bootstrap_results_h1 <- bootstrap_h1(data1, c("C3A", "C3B", "C3C", "C3D", "C3E", "C44",
"C45", "C46", "C47"), num_iterations = 1000)
# Calculate the power of bootstrapping
power_h1 <- mean(bootstrap_results_h1 >= res1$statistic)
cat("\n\nPower of Bootstrapping for Hypothesis 1:", power_h1)
# Plot the bootstrap results
hist(bootstrap_results_h1, main = "Bootstrap Distribution of Chi-square Test Statistic",
xlab = "Chi-square Test Statistic", col = "skyblue", border = "black")
abline(v = res1$statistic, col = "red", lwd = 2, lty = 2)
#legend("topright", legend = c("Original Statistic", "Bootstrap Distribution"),
#col = c("red", "skyblue"), lty = c(2, 0),lwd=c(2,1))
#########################################################################
#######
################################# Hypothesis 2
#################################
#########################################################################
#######
# Does the fact that the adopted child ever lived with his/her birth family
# negatively affect the kid's psychological well-being, as perceived by
# the adoptive parents?
## Variables:
# Grouping: C1C ('Has [S.C.] ever lived with [his/her] birth family?): 0 and 1
# 'K7Q79 ('Please tell me if this statement was true for [S.C.] during the
# past month. [He/She] is unhappy, sad, or depressed.) Lower value means less unhappy
# Grouping variable C1C as 0 and 1 for its analyse. N/A and answers as
# "do not know", "don't want to answer", etc. had been omited for the
# sake of the study
vars2<-c("C1C","K7Q79")
data2<-erasedata(vars2,data)
data2 <- select_columns(vars2, data2)
# Creating subsets to separate both groups
everlived<-subset(data2, C1C==1)
neverlived<-subset(data2, C1C==0)
n1=nrow(everlived)
n2=nrow(neverlived)
# Analyzing the variables to be worked with. As they are Categorical Ordinal,
# bar plots are presented.
ta_el<-table(everlived$K7Q79)
ta_nl<-table(neverlived$K7Q79)
barplot(ta_el,main="Child lived with birth family: \n Is unhappy, sad, or depressed?",
names.arg=c("Never", "Rarely", "Sometimes", "Usually","Always"),
col="steelblue3",ylab="Frequency")
barplot(ta_nl,main="Child did not live with birth family: \n Is unhappy, sad, or depressed?",
names.arg=c("Never", "Rarely", "Sometimes", "Usually","Always"),
col="firebrick",ylab="Frequency")
# The U Mann-Whitney test is computed with the specified variable and the group
res2<-wilcox.test(data2$K7Q79~data2$C1C,conf.int = T)
# Computing Power
pwr2=pwr.t2n.test(d=0.2,n1,n2,sig.level=0.05)
pwr2$method<-"Mann-Whitney U Test power calculation"
{if (res2$p.value>0.05){
cat(paste("With a P-Value of ",res2$p.value," H0 can not be rejected, there is no significant
difference between groups. An a power of ",pwr2$power))
cat("\n\nThe fact that the adopted child ever or never lived with his/her birth family, does not
negatively affect the psychological well-being of the S.C.")}
else{
cat(paste("With a P-Value of ",res2$p.value," H0 must be rejected, there is significant
difference between groups. An a power of ",pwr2$power))
cat("\n\nThe fact that the adopted child ever lived with his/her birth family, negatively affects
the psychological well-being of the S.C.")}}
# Run bootstrap for Hypothesis 2
bootstrap_results_h2 <- bootstrap_h2(data2, "K7Q79", "C1C", num_iterations = 1000)
# Calculate the power of bootstrapping
power_h2 <- mean(bootstrap_results_h2 >= res2$statistic)
cat("\n\nPower of Bootstrapping for Hypothesis 2:", power_h2)
# Plot the bootstrap results
hist(bootstrap_results_h2, main = "Bootstrap Distribution of U Mann-Whitney Test Statistic",
xlab = "U Mann-Whitney Test Statistic", col = "skyblue", border = "black")
abline(v = res2$statistic, col = "red", lwd = 2, lty = 2)
#legend("topright", legend = c("Original Statistic", "Bootstrap Distribution"),
#col = c("red", "skyblue"), lty = c(2, 0),lwd=c(2,1))
## Parametric Bootstrap
# Probabilities
prob_h2_ever<-tabulate(everlived$K7Q79)/nrow(everlived)
prob_h2_never<-tabulate(neverlived$K7Q79)/nrow(neverlived)
orig_data2=append(everlived$K7Q79,neverlived$K7Q79)
# Adding the probabilities
mle=list()
for (i in (1:length(prob_h2_ever))){
mle=append(mle,prob_h2_ever[i])
} for (
i
in (
1:length(prob_h2_never))){
mle=append(mle,prob_h2_never[i])
}
boot_res2<-boot(orig_data2,location_boot,R=999,sim="parametric",ran.gen=generate_data,
mle=mle)
# Obtaining the confidence interval
interval_boot2=boot.ci(boot_res2,type="perc")
# Obtain the p-value
pval2<-boot.pval(boot_res2,type="perc",theta_null=0)
# Power of the parametric Bootstrap
power_boot2=mean(boot_res2$t>=boot_res2$t0)
cat("\n\nThe Parametric Bootstraping for the H2 has a P-Value of: ",pval2,"\nAnd a Power of:
",power_boot2,"\nThe Confidence of Interval is between ",interval_boot2$percent[4]," and
",interval_boot2$percent[5],"\n\n")
plot(boot_res2)
#########################################################################
#######
################################# Hypothesis 3
#################################
#########################################################################
#######
# Do the insubordinated attitudes of the child, such as: disobedience,
# stubbornness, sullennes or querulousness, make the adoptive parents
# regret their decision of having adopted the child?
## Variables:
# W15 (If you [and your spouse/partner] knew everything about [S.C.]
# before the adoption that you now know, how might that have affected
# your decision to accept [him/her] for adoption? Would you have…?):
# 1 | 2 & 3|4
# K7Q70 (argues too much), K7Q74 (disobedient) and K7Q75 (stubborn,
# sullen, or irritable): Lower values on each variable mean a refuse
# to the affirmation
# Grouping variable W15 as 1|2 and 3|4 for its analyse. N/A and answers as
# "do not know", "don't want to answer", etc. had been omited for the
# sake of the study
vars3<-c("W15","K7Q70","K7Q74","K7Q75")
data3<-erasedata(vars3,data)
data3 <- select_columns(vars3, data3)
# For the non parametric test, a new variable must be added that represents
# the grouping, the same as before, which shows if the parents regret adopting the child.
group3<-c()
borrar3<-c()
con3<-1
for(i in (1:nrow(data3))){
if (data3$W15[i]==1 | data3$W15[i]==2){
group3[i]=0}
if (data3$W15[i]==3 | data3$W15[i]==4){
group3[i]=1}
if (data3$W15[i]!=1 & data3$W15[i]!=2 & data3$W15[i]!=3 & data3$W15[i]!=4){
borrar3[con3]<-i
con3<-con3+1}
}
data3<-cbind(data3,group3)
# Creating subsets to separate both groups
noregrets<-subset(data3, (W15==1 | W15==2))
regrets<-subset(data3, (W15==3 | W15==4))
n1=nrow(noregrets)
n2=nrow(regrets)
# Analyzing the variables to be worked with. As they are Categorical Ordinal,
# bar plots are presented.
ta_no0<-table(noregrets$K7Q70)
ta_re0<-table(regrets$K7Q70)
ta_no4<-table(noregrets$K7Q74)
ta_re4<-table(regrets$K7Q74)
ta_no5<-table(noregrets$K7Q75)
ta_re5<-table(regrets$K7Q75)
barplot(ta_no0,main="Parents do not regret: \n Child argues too much?",
names.arg=c("Never", "Rarely", "Sometimes", "Usually","Always"),
col="royalblue"
,ylab="Frequency")
barplot(ta_re0,main="Parents regret: \n Child argues too much?",
names.arg=c("Never", "Rarely", "Sometimes", "Usually","Always"),
col="red2",ylab="Frequency")
barplot(ta_no4,main="Parents do not regret: \nIs the Sample Child disobedient?",
names.arg=c("Never", "Rarely", "Sometimes", "Usually","Always"),
col="royalblue"
,ylab="Frequency")
barplot(ta_re4,main="Parents regret: \n Is the Sample Child disobedient?",
names.arg=c("Never", "Rarely", "Sometimes", "Usually","Always"),
col="red2",ylab="Frequency")
barplot(ta_no5,main="Parents do not regret: \n Is the Sample Child stubborn, sullen, or
irritable?",
names.arg=c("Never", "Rarely", "Sometimes", "Usually","Always"),
col="royalblue"
,ylab="Frequency")
barplot(ta_re5,main="Parents regret: \nIs the Sample Child stubborn, sullen, or irritable?",
names.arg=c("Never", "Rarely", "Sometimes", "Usually","Always"),
col="red2",ylab="Frequency")
# The U Mann-Whitney test is computed for the specified variables and the grouping
res31<-wilcox.test(data3$K7Q70~data3$group3)
res32<-wilcox.test(data3$K7Q74~data3$group3)
res33<-wilcox.test(data3$K7Q75~data3$group3)
# Computing Power
pwr3=pwr.t2n.test(d=0.2,n1,n2,sig.level=0.05)
pwr3$method<-"Mann-Whitney U Test power calculation"
{if (res31$p.value>0.05){
cat(paste("\n\n\nWith a P-Value of ",res31$p.value," H0 can not be rejected, there is no
significant difference between groups. An a power of ",pwr3$power))
cat("\n\nIf the adopted child argues too much or not is not a significant reason that makes
the parents regret the adoption.")}
else{
cat(paste("\n\n\nWith a P-Value of ",res31$p.value," H0 must be rejected, there is
significant difference between groups. An a power of ",pwr3$power))
cat("\n\nThe fact that the adopted child argues too much is a significant reason that
makes the parents regret the adoption.")}}
{if (res32$p.value>0.05){
cat(paste("\n\n\nWith a P-Value of ",res32$p.value," H0 can not be rejected, there is no
significant difference between groups. An a power of ",pwr3$power))
cat("\n\nThe disobedience of the adopted child is not a significant reason that makes the
parents regret the adoption.")}
else{
cat(paste("\n\n\nWith a P-Value of ",res32$p.value," H0 must be rejected, there is
significant difference between groups. An a power of ",pwr3$power))
cat("\n\nThe disobedience of the adopted child is a significant reason that makes the
parents regret the adoption.")}}
{if (res33$p.value>0.05){
cat(paste("\n\n\nWith a P-Value of ",res33$p.value," H0 can not be rejected, there is no
significant difference between groups. An a power of ",pwr3$power))
cat("\n\nIf the adopted child is stubborn, sullen or irritable, is not a significant reason that
makes the parents regret the adoption.")}
else{
cat(paste("\n\n\nWith a P-Value of ",res33$p.value," H0 must be rejected, there is
significant difference between groups. An a power of ",pwr3$power))
cat("\n\nThe fact that the adopted child is stubborn, sullen or irritable, is a significant
reason that makes the parents regret the adoption.")}}
# Run bootstrap for Hypothesis 3 (Variable 1)
bootstrap_results_h3_var1 <- bootstrap_h3(data3, "K7Q70", "group3", num_iterations =
1000)
# Calculate the power of bootstrapping for Hypothesis 3 (Variable 1)
power_h3_var1 <- mean(bootstrap_results_h3_var1 >= res31$statistic)
cat("\n\nPower of Bootstrapping for Hypothesis 3 (Variable 1):", power_h3_var1)
# Plot the bootstrap results for Hypothesis 3 (Variable 1)
hist(bootstrap_results_h3_var1, main = "Bootstrap Distribution of Wilcoxon Test Statistic
(Variable 1)",
xlab = "Wilcoxon Test Statistic", col = "skyblue", border = "black")
abline(v = res31$statistic, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Original Statistic", "Bootstrap Distribution"),
col = c("red", "skyblue"), lty = c(2, 0), lwd=c(2,1))
# Run bootstrap for Hypothesis 3 (Variable 2)
bootstrap_results_h3_var2 <- bootstrap_h3(data3, "K7Q74", "group3", num_iterations =
1000)
# Calculate the power of bootstrapping for Hypothesis 3 (Variable 1)
power_h3_var2 <- mean(bootstrap_results_h3_var2 >= res32$statistic)
cat("\n\nPower of Bootstrapping for Hypothesis 3 (Variable 2):", power_h3_var2)
# Plot the bootstrap results for Hypothesis 3 (Variable 1)
hist(bootstrap_results_h3_var1, main = "Bootstrap Distribution of Wilcoxon Test Statistic
(Variable 2)",
xlab = "Wilcoxon Test Statistic", col = "skyblue", border = "black")
abline(v = res31$statistic, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Original Statistic", "Bootstrap Distribution"),
col = c("red", "skyblue"), lty = c(2, 0), lwd=c(2,1))
# Run bootstrap for Hypothesis 3 (Variable 3)
bootstrap_results_h3_var3 <- bootstrap_h3(data3, "K7Q75", "group3", num_iterations =
1000)
# Calculate the power of bootstrapping for Hypothesis 3 (Variable 1)
power_h3_var3 <- mean(bootstrap_results_h3_var3 >= res33$statistic)
cat("\n\nPower of Bootstrapping for Hypothesis 3 (Variable 3):", power_h3_var3)
# Plot the bootstrap results for Hypothesis 3 (Variable 1)
hist(bootstrap_results_h3_var1, main = "Bootstrap Distribution of Wilcoxon Test Statistic
(Variable 3)",
xlab = "Wilcoxon Test Statistic", col = "skyblue", border = "black")
abline(v = res33$statistic, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Original Statistic", "Bootstrap Distribution"),
col = c("red", "skyblue"), lty = c(2, 0), lwd=c(2,1))
## Parametric Bootstrap
# Probabilities
prob_h31_nore<-tabulate(noregrets$K7Q70)/nrow(noregrets)
prob_h31_re<-tabulate(regrets$K7Q70)/nrow(regrets)
prob_h32_nore<-tabulate(noregrets$K7Q74)/nrow(noregrets)
prob_h32_re<-tabulate(regrets$K7Q74)/nrow(regrets)
prob_h33_nore<-tabulate(noregrets$K7Q75)/nrow(noregrets)
prob_h33_re<-tabulate(regrets$K7Q75)/nrow(regrets)
orig_data31=append(noregrets$K7Q70,regrets$K7Q70)
orig_data32=append(noregrets$K7Q74,regrets$K7Q74)
orig_data33=append(noregrets$K7Q75,regrets$K7Q75)
# Adding the probabilities
mle=list()
for (i in (1:length(prob_h31_nore))){
mle=append(mle,prob_h31_nore[i])
} for (
i
in (
1:length(prob_h31_re))){
mle=append(mle,prob_h31_re[i])
}
mle2=list()
for (i in (1:length(prob_h32_nore))){
mle2=append(mle2,prob_h32_nore[i])
} for (
i
in (
1:length(prob_h32_re))){
mle2=append(mle2,prob_h32_re[i])
}
mle3=list()
for (i in (1:length(prob_h33_nore))){
mle3=append(mle3,prob_h33_nore[i])
} for (
i
in (
1:length(prob_h33_re))){
mle3=append(mle3,prob_h33_re[i])
}
#### First Variable
boot_res31<-boot(orig_data31,location_boot,R=999,sim="parametric",ran.gen=generate_dat
a,mle=mle)
# Obtaining the confidence interval
interval_boot31=boot.ci(boot_res31,type="perc")
# Obtain the p-value
pval31<-boot.pval(boot_res31,type="perc",theta_null=0)
# Power of the parametric Bootstrap
power_boot31=mean(boot_res31$t>=boot_res31$t0)
cat("\n\nThe Parametric Bootstraping for the for the first variable of H3 has a P-Value of:
",pval31,"\nAnd a Power of: ",power_boot31,"\nThe Confidence of Interval is between
",interval_boot31$percent[4]," and ",interval_boot31$percent[5],"\n\n")
plot(boot_res31)
### Second Variable
boot_res32<-boot(orig_data32,location_boot,R=999,sim="parametric",ran.gen=generate_dat
a,mle=mle2)
# Obtaining the confidence interval
interval_boot32=boot.ci(boot_res32,type="perc")
# Obtain the p-value
pval32<-boot.pval(boot_res32,type="perc",theta_null=0)
# Power of the parametric Bootstrap
power_boot32=mean(boot_res32$t>=boot_res32$t0)
cat("\n\nThe Parametric Bootstraping for the second variable of H3 has a P-Value of:
",pval32,"\nAnd a Power of: ",power_boot32,"\nThe Confidence of Interval is between
",interval_boot32$percent[4]," and ",interval_boot32$percent[5],"\n\n")
plot(boot_res32)
### Third Variable
boot_res33<-boot(orig_data33,location_boot,R=999,sim="parametric",ran.gen=generate_dat
a,mle=mle3)
# Obtaining the confidence interval
interval_boot33=boot.ci(boot_res33,type="perc")
# Obtain the p-value
pval33<-boot.pval(boot_res33,type="perc",theta_null=0)
# Power of the parametric Bootstrap
power_boot33=mean(boot_res33$t>=boot_res33$t0)
cat("\n\nThe Parametric Bootstraping for the third variable of H3 has a P-Value of:
",pval33,"\nAnd a Power of: ",power_boot33,"\nThe Confidence of Interval is between
",interval_boot33$percent[4]," and ",interval_boot33$percent[5],"\n\n")
plot(boot_res33)
