library("copula", lib.loc="~/.local/R/site-library") #stirling numbers
setwd("/home/tarasov/Dropbox/Rev-Bayes-test")
library(R.basic)

Q<-matrix(c(0,0.01,  0.2,0),2,2,byrow=TRUE)
rownames(Q)<-colnames(Q)<-c("0","1")
diag(Q)<-rowSums(Q)
Q
> Q
0    1
0 0.01 0.01
1 0.20 0.20

tree<-pbtree(n=300,scale=1)
tt<-sim.history(tree,Q)
plot(tt)
str(tt)
tt$states
write.tree(tt, file = "two-st-tree.tre")
write.table(cbind(tt$states), "two-st-chrs.csv", col.names = F, sep=" ", quote=F)
?write.csv
plot(tt, fsize=.5)
is.ultrametric(tt)
fastDist(tt, "t256", "t19")
hist(tt$edge.length)
sum(tt$edge.length)

(which(tt$states=="0") %>% length)/300

Stirling2(6, 4)

####putative formula for number of partitions with zero elements
Stirling2(8, 1)*1 +
  Stirling2(8, 2)*2 +
  Stirling2(8, 3)*3 +
  Stirling2(8, 4)*4 +
  Stirling2(8, 5)*5 +
  Stirling2(8, 6)*6 +
  Stirling2(8, 7)*7 +
  Stirling2(8, 8)*8

install_github("GuangchuangYu/ggtree")
install_github("revbayes/RevGadgets")




#########
setwd("/home/tarasov/Documents/RevBayes/hmm_dpp/test_runs/priorExp/output")
log = read.table("eqrates2_2-Temp_1-E3-prior.log", header=TRUE, sep="\t", stringsAsFactors=FALSE)
hist(exp(log$Prior))
exp(log$Prior) %>% mean
log$r1 %>% mean

hist(rexp(1000, 1/22))


dev.off()
#readignRev
setwd("/home/tarasov/Documents/RevBayes/hmm_dpp/output_dpp_hmm")
log = read.table("hidden_dpp.log", header=TRUE, sep="\t", stringsAsFactors=FALSE)
burin=100
total_samples=nrow(log)
r1=which(colnames(log)=="r1")
r8=which(colnames(log)=="r8")
##dt_rates as numeric
#dt_rates_num=t(log[,r1:r8])
dt_rates=t(log[,r1:r8])
#rownames(dt_rates_num)<-rownames(t(log[1:2,r1:r8]))
## dt_rates with levels
#dt_rates=apply(t(log[,r1:r8]), 2, as.character) #translate into character to apply factors
rownames(dt_rates)<-rownames(t(log[1:2,r1:r8]))
#dt_rates=data.frame(dt_rates)
str(dt_rates)
dt_rates[2]

##export to matrice
m_dim=4
q=c(1,2,3,4,5,6,7,8)

M1<-matrix(c(-1,1,  2,-2),2,2,byrow=TRUE)
rownames(M1)<-colnames(M1)<-c("0","1")

M2<-matrix(c(-3,3,  4,-4),2,2,byrow=TRUE)
rownames(M2)<-colnames(M2)<-c("a","b")

indepen2matrices(indepen2matrices(M1, M2, name.sep=":"), M1, name.sep=":")

indepen2matrices(M1, M2,  name.sep=":")

nrow(M1)
###############################
indepen2matrices<-function(M1,M2, name.sep="", diag.as=""){
M_kr=(M1%x%diag(nrow(M2)))+ (diag(nrow(M1))%x%M2)

#getting colnames

col=paste(colnames(kronecker(M1, diag(nrow(M2)), make.dimnames = T)),
      colnames(kronecker(diag(nrow(M1)), M2, make.dimnames = T)), sep="")
col=gsub("::", name.sep, col, fixed=T)

# merging two names
rownames(M_kr)<-colnames(M_kr)<-col
if (diag.as!="") diag(M_kr)<-diag.as
return(M_kr)
}


####################################
# populate all rows of coevolving matrix with different ratate
M_kr=indepen2matrices(M1, M2,  name.sep=":", diag.as=0)
#
rate_diff_joint<-function(M_kr){
N=which(M_kr>0)%>%length
M_kr[which(M_kr>0)]<-c(1:N)
M_kr=t(M_kr)
return(M_kr)
}

M_kr=rate_diff_joint(M_kr)
##################
# Map table rows to matrix rates
#
M_kr=M.temp
row.i=dt_rates[,7]
row.i=apply(dt_rates[,1:2], 2, function(x) x)
t=as.character(row.i)
names(t)<-rownames(row.i)
row.i=row.i[,1]
class(row.i)
row.i=dt_rates[2]
row.vec=c("r2","r1", "r5", "r6", "r3", "r4", "r8", "r7")
#
mode(row.i)
# row.i must be a character with names
rows2rate_matrix<-function(M_kr, row.vec, row.i){
rN=length(row.vec)
M_kr[match(c(1:rN), M_kr)]<-row.vec
M_kr[match(names(row.i), M_kr)]<-row.i
mode(M_kr)<-"numeric"
diag(M_kr)<-0
diag(M_kr)<-rowSums(-1*M_kr)
return(M_kr)
}

M_kr=rows2rate_matrix(M_kr, row.vec, dt_rates[4])
rows2rate_matrix(M.temp, row.vec, dt_rates[,7])
###################################
#
# check strong lumpability
#
part_scheme=list(c(1, 2), c(3), c(4))
part_scheme=list(c(1), c(2), c(3,4))
part_scheme=list(c(1,2), c(3,4))
is_strg_lumpable(M_kr, list(c(1,2), c(3,4)))

M_kr=rows2rate_matrix(M.temp, row.vec, dt_rates[,7])
###
is_strg_lumpable<-function(M_kr, part_scheme){

Nper.part=lapply(part_scheme, length)%>%unlist
stat2M=matrix(0, nrow=length(Nper.part), ncol=ncol(M_kr))
for (i in 1:nrow(stat2M))
  stat2M[i, part_scheme[[i]]]<-1

M_rows=M_kr %*% t(stat2M)
tru.vals=c()

for (i in 1:length(part_scheme)){
  for (j in 1:ncol(M_rows)){
    tru.vals=c(tru.vals, length(unique(M_rows[part_scheme[[i]],j]))<2)
    }
}
# if this is false then matrix is not lumpable
return(all(tru.vals))
}

##########
# chack weak lumpability: a special case involving division of rows sums by nrows


matrix_diff<-function(M_kr, part_scheme, t=1){

  Nper.part=lapply(part_scheme, length)%>%unlist
  stat2M=matrix(0, nrow=length(Nper.part), ncol=ncol(M_kr))
  for (i in 1:nrow(stat2M))
    stat2M[i, part_scheme[[i]]]<-1

 normalizer=diag(1/ Nper.part)
 diff=(normalizer %*% stat2M %*% expm::expm(M_kr*t) %*% t(stat2M))-
   (
     expm::expm(
       (normalizer %*% stat2M %*% M_kr %*% t(stat2M))*t
                 )
    )
  return(diff)
}

matrix_diff(M_kr, list(c(1, 2), c(3,4)), 10)
format(matrix_diff(M_kr, list(c(1, 2), c(3,4)), 20) , scientific=F)
max(abs(matrix_diff(M_kr, list(c(1, 2), c(3,4)))))
## max error between limpable matrix difference
lump_max_error<-function(...){
dif=matrix_diff(...)
max(abs(dif))
}
######
lump_max_error(M_kr, list(c(1, 2), c(3,4)))
#############################
#
#PIPILEINE
#for checking lumpability
#
#########

# creating two intial matrices to make coevolvingmatrix
M1<-matrix(c(-1,1,  2,-2),2,2,byrow=TRUE)
rownames(M1)<-colnames(M1)<-c("0","1")

M2<-matrix(c(-3,3,  4,-4),2,2,byrow=TRUE)
rownames(M2)<-colnames(M2)<-c("a","b")

# joining them
M_joint=indepen2matrices(M1, M2,  name.sep=":")

# making template matrix with all rates different
M.temp=rate_diff_joint(M_joint)

# populating template with rates from rows given row.vec as map
row.vec=c("r2","r1", "r5", "r6", "r3", "r4", "r8", "r7")
rows2rate_matrix(M.temp, row.vec, dt_rates[1])

# checking strong lumpability
is_strg_lumpable(rows2rate_matrix(M.temp, row.vec, dt_rates[1]), list(c(1,2), c(3,4)))

###################
# check strong lumpability right from matrix
part_scheme=list(c(1), c(2), c(3,4))

is_strg_lumpableM<-function(M.temp, row.vec, row.i, part_scheme){
    return(is_strg_lumpable(rows2rate_matrix(M.temp, row.vec, row.i), part_scheme))
}
is_strg_lumpableM(M.temp, row.vec, row.i, part_scheme)
#######################

apply(dt_rates, 2, function(x) is_strg_lumpableM(M.temp, row.vec, x, part_scheme))
apply(dt_rates[1:2], 2, function(x) as.character(x))
is_strg_lumpableM(M.temp, row.vec, apply(dt_rates[1], 2, function(x) x), part_scheme)

apply(dt_rates[,1:2], 2, function(x) rows2rate_matrix(M.temp, row.vec, x))
dt_rates[,1:7]
is_strg_lumpableM(M.temp, row.vec, dt_rates[,7], part_scheme)
rows2rate_matrix(M.temp, row.vec, dt_rates[,10])
#
which(apply(dt_rates, 2, function(x) is_strg_lumpableM(M.temp, row.vec, x, list(c(1), c(2), c(3,4)) ))==T) %>% length
which(apply(dt_rates, 2, function(x) is_strg_lumpableM(M.temp, row.vec, x, list(c(1, 2), c(3), c(4)) ))==T) %>% length
which(apply(dt_rates, 2, function(x) is_strg_lumpableM(M.temp, row.vec, x, list(c(1, 2), c(3,4)) ))==T) %>% length
#
is_strg_lumpableM(M.temp, row.vec, dt_rates[,7], list(c(1, 2), c(3), c(4)))

apply(dt_rates, 2, function(x) is_strg_lumpableM(M.temp, row.vec, x, part_scheme))
####################################

## number of zero elements
zeros=apply(dt_rates, 2, function(x) which(x==0)%>%length)
hist(zeros)
####
#zeros by rows
which(apply(dt_rates, 2, function(x) x[1]==0 & x[2]==0)) %>% length
dt_rates[apply(dt_rates, 2, function(x) x[1]==0 & x[2]==0)]
dt_rates[apply(dt_rates, 2, function(x) x[3]==0 & x[4]==0)]
dt_rates[apply(dt_rates, 2, function(x) x[5]==0 & x[6]==0)]
dt_rates[apply(dt_rates, 2, function(x) x[7]==0 & x[8]==0)]

#zeros by colmuns
dt_rates[apply(dt_rates, 2, function(x) x[3]==0 & x[5]==0)]
dt_rates[apply(dt_rates, 2, function(x) x[1]==0 & x[7]==0)]
dt_rates[apply(dt_rates, 2, function(x) x[2]==0 & x[8]==0)]
dt_rates[apply(dt_rates, 2, function(x) x[4]==0 & x[6]==0)]

which(apply(dt_rates, 2, function(x) x[3]==0 & x[5]==0))%>% length
      which(apply(dt_rates, 2, function(x) x[1]==0 & x[7]==0))%>% length
            which(apply(dt_rates, 2, function(x) x[2]==0 & x[8]==0))%>% length
                  which(apply(dt_rates, 2, function(x) x[4]==0 & x[6]==0))%>% length

## number of unique rate categories
rate_uniq=apply(dt_rates, 2, function(x) unique(x)%>%length)
hist(rate_uniq)
table(rate_uniq)

## number of unique rate categories ecluding )
rate_uniq=apply(dt_rates, 2, function(x) which(unique(x)!=0) %>%length)
table(rate_uniq)
##
lumpable
which(apply(dt_rates, 2, function(x) (x[2]==x[4]) & (x[5]==x[7]) )==T) %>% length
##
same_patterns(t(log[,r1:r8]))

dt_rates[,1:5]

factor(dt_rates[,2], levels(dt_rates[,2]))
levels(dt_rates[,3])
as.numeric(dt_rates[,2])

apply(t(log[1:5,r1:r8]), 2, as.character)
str(dt_rates)
####





out_file = "out.txt"



total_samples = length(log$rate_01)
m1_rate_samples = length(which(log$rate_01 == log$rate_10))
m01_irreversible = length(which(log$rate_01 == 0))
m10_irreversible = length(which(log$rate_10 == 0))
m2_rate_samples = total_samples - (m1_rate_samples + m01_irreversible + m10_irreversible)

m2_samples = which(log$rate_01 != log$rate_10)
mean(log[m2_samples,]$rate_01)
mean(log[m2_samples,]$rate_10)

write("Model posterior probabilities:", file=out_file)
write(paste("01 irreversible: ", round(m01_irreversible / total_samples, 2), sep=""), file=out_file, append=TRUE)
write(paste("10 irreversible: ", round(m10_irreversible / total_samples, 2), sep=""), file=out_file, append=TRUE)
write(paste("1 rate: ", round(m1_rate_samples / total_samples, 2), sep=""), file=out_file, append=TRUE)
write(paste("2 rate: ", round(m2_rate_samples / total_samples, 2), sep=""), file=out_file, append=TRUE)
write("", file=out_file, append=TRUE)

write(paste("BF 10_irr/1: ", round((m10_irreversible/total_samples)/(m1_rate_samples/total_samples), 2), sep=""), file=out_file, append=TRUE)
write(paste("BF 10_irr/2: ", round((m10_irreversible/total_samples)/(m2_rate_samples/total_samples), 2), sep=""), file=out_file, append=TRUE)
write(paste("BF 1/2: ", round((m1_rate_samples/total_samples)/(m2_rate_samples/total_samples), 2), sep=""), file=out_file, append=TRUE)
write(paste("BF 2/1: ", round((m2_rate_samples/total_samples)/(m1_rate_samples/total_samples), 2), sep=""), file=out_file, append=TRUE)






