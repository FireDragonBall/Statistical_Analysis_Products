library(readxl)
M2 <- read_excel("C:/Users/Administrator/Desktop/Module 2 Project_Project Performance_v1(2).xlsx",sheet = "Applied Probability-Business",col_types = c("numeric", "numeric", "numeric", "numeric", "skip", "skip", "skip", "skip", "skip", "skip"), n_max = 51)
View(M2)
M2=c(M2)
names(M2)=c('PIP','Q','S','C')

# part 1

QC=table(M2$Q>500)["TRUE"]
SC=table(M2$S<13)["TRUE"]
CC=table(M2$C<234000)["TRUE"]
PQ=table(M2$Q>500)["TRUE"]/50
PS=table(M2$S<13)["TRUE"]/50
PC=table(M2$C<234000)["TRUE"]/50

# or

QC=sum(with(M2,S<13))
SC=sum(with(M2,Q>500))
CC=sum(with(M2,C<234000))
PQ=sum(with(M2,S<13))/50
PS=sum(with(M2,Q>500))/50
PC=sum(with(M2,C<234000))/50


# part 2

P7=sum(with(M2,S<13 & Q>500 & C<234000))
P4=sum(with(M2,S<13 & Q>500 & C>=234000))
P6=sum(with(M2,S<13 & Q<=500 & C<234000))
P5=sum(with(M2,S>=13 & Q>500 & C<234000))
P4=sum(with(M2,S<13 & Q>500 & C>=234000))
P3=sum(with(M2,S>=13 & Q<=500 & C<234000))
P2=sum(with(M2,S<13 & Q<=500 & C>=234000))
P1=sum(with(M2,S>=13 & Q>500 & C>=234000))
P0=sum(with(M2,S>=13 & Q<=500 & C>=234000))

P0p=sum(with(M2,S>=13 & Q<=500 & C>=234000))/50
P1p=sum(with(M2,S>=13 & Q>500 & C>=234000))/50
P2p=sum(with(M2,S<13 & Q<=500 & C>=234000))/50
P3p=sum(with(M2,S>=13 & Q<=500 & C<234000))/50
P4p=sum(with(M2,S<13 & Q>500 & C>=234000))/50
P5p=sum(with(M2,S>=13 & Q>500 & C<234000))/50
P6p=sum(with(M2,S<13 & Q<=500 & C<234000))/50
P7p=sum(with(M2,S<13 & Q>500 & C<234000))/50


# part 4

library(plyr)
Ss=c(which(M2$S<13))
Sf=c(which(M2$S>=13))
Qq=c(which(M2$Q>500))
Qf=c(which(M2$Q<=500))
Cc=c(which(M2$C<234000))
Cf=c(which(M2$C>=234000))

 ## a
PSC=nrow(count(intersect(Ss,Cc)))/50
PSC/PC
CC*PSC/PC

 ## b
PQC=nrow(count(intersect(Qq,Cc)))/50
PQC/PQ
QC*PQC/PQ

 ## c
PSQ=nrow(count(Reduce(intersect,list(Qq,Ss,Cf))))/50
PSQ/PQ
QC*PSQ/PQ

 ## d
PSC=nrow(count(Reduce(intersect,list(Ss,Cc,Qf))))/50
PSC/PC
CC*PSC/PC

 ## e
PCQ=nrow(count(Reduce(intersect,list(Sf,Cc,Qq))))/50
PSf=1-PS
PCQ/PSf
(50-SC)*PCQ/PSf

 ## f
six=P4p+P5p+P6p
50*six

 ## g
sev=(P1p+P2p+P3p)/1-P0p
(50-P0)*sev

 ## h
PCf=1-PC
eig=nrow(count(intersect(Cf,Ss)))/50/PCf
(50-CC)*eig