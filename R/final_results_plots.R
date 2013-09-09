source("globals.R")
source("analysis_functions.R")

library(lmtest)

p <- visualize_activation_mechanism()
p1 <- visualize_annealing(conditions)


cut2 <- get_dat("results_full.csv")
cut2$ingroup_Total <- cut2$ingroup_Total +1
cut2$outgroup_Total <- cut2$outgroup_Total+1
#cut2 <- cut2[cut2$max_SCC > 1,] #& cut2$Annealing_Rate_Group != "5",]
cut2$odds <- with(cut2,log2(outgroup_Total/ingroup_Total))
cut2$PL_Ratio <- cut2$avg_pl_SCC/(log(cut2$max_SCC)/log(cut2$n_edges_max_SCC/cut2$max_SCC))
cut2$CC_Ratio <- cut2$unweighted_CC_SCC/(cut2$n_edges_max_SCC/(cut2$max_SCC^2))
cut2$SWQ <- cut2$CC_Ratio/cut2$PL_Ratio
cut2 <- cut2[cut2$Annealing_Rate_Group != "100",]
cut2$Annealing_Rate_Group <- factor(cut2$Annealing_Rate_Group)
cut2 <- ddply(cut2,c("Generation_Mechanism_Knowledge","Agent_Bias_Rate","Memory_Rate_Group","Annealing_Rate_Group","Memory_Rate_Individual"),function(l){l$Replication <- 1:nrow(l);return(l)})

k_labeller <- function(variable,value){
  k_names <- list('5'="GLP=5",'25'="GLP=25",'50'="GLP=50")
  return(k_names[value])
}

dat <- ddply(cut2, .(Agent_Bias_Rate,Generation_Mechanism_Knowledge), function(l){odd <- smean.cl.boot(l$odds)
names(odd) <- paste0(names(odd),"_Odds")
gr_mean <- smean.cl.boot(l$group_mean-1)
names(gr_mean) <- paste0(names(gr_mean),"_GM")
go_mean <- smean.cl.boot(l$GO_mean)
names(go_mean) <- paste0(names(go_mean),"_GO")
c(odd,gr_mean,go_mean)})
ggplot(dat, aes(y=Mean_Odds,ymin=Lower_Odds,ymax=Upper_Odds,color=Generation_Mechanism_Knowledge,x=Agent_Bias_Rate)) + geom_pointrange(size=.8) +geom_path()+ scale_colour_manual("Knowledge Condition",values=c('dark blue','dark green', 'red'))+ xlab("Initial Bias Parameter (IBP)") + ylab("Log-odds of an Outgroup Tie")  + scale_x_continuous(breaks=c(0,.25,.5,.75,1),labels=c("0",".25",".5",".75","1")) + scale_shape_discrete("GAT")

ggplot(dat, aes(y=Mean_GM,ymin=Lower_GM,ymax=Upper_GM,color=Generation_Mechanism_Knowledge,x=Agent_Bias_Rate)) + geom_pointrange(size=.8) +geom_path()+ scale_colour_manual("Knowledge Condition",values=c('dark blue','dark green', 'red'))+ xlab("Initial Bias Parameter (IBP)") + ylab("Mean Number of Group\nSchemas Per Agent")  + scale_x_continuous(breaks=c(0,.25,.5,.75,1),labels=c("0",".25",".5",".75","1")) + scale_shape_discrete("GAT")

ggplot(dat[dat$Generation_Mechanism_Knowledge !="all_same",], aes(y=Mean_GO,ymin=Lower_GO,ymax=Upper_GO,color=Generation_Mechanism_Knowledge,x=Agent_Bias_Rate)) + geom_pointrange(size=.8) +geom_path()+ scale_colour_manual("Knowledge Condition",values=c('dark green', 'red'))+ xlab("Initial Bias Parameter (IBP)") + ylab("Mean number of knowledge bits\nagents perceive generalized\nother to have")  + scale_x_continuous(breaks=c(0,.25,.5,.75,1),labels=c("0",".25",".5",".75","1")) + scale_shape_discrete("GAT")

###w/ annealing on Y
