library(plyr)
library(reshape)
library(ggplot2)
library(fitdistrplus)
library(doBy)
library(vcd)
library(snowfall)
library(igraph)
library(data.table)
library(reshape2)

min_edge_weight_for_network <- 2
data_dir <- "Y:/trans_mem/run/"
#list_of_dirs <- read.csv("Z:/trans_mem/finished_fils.txt",header=F)
#list_of_dirs$V1 <- paste0(data_dir,list_of_dirs$V1)
setwd("D:/kjoseph/")


#list_of_finished_dirs <- Sys.glob("success4/*")
#library(stringr)
#list_of_finished_dirs <- str_replace(list_of_finished_dirs, "success4/","")
#list_of_finished_dirs <- str_replace(list_of_finished_dirs, ".csv","")
#list_of_finished_dirs <- paste0(data_dir,list_of_finished_dirs)
#list_of_dirs <- list_of_dirs$V1[!(list_of_dirs$V1 %in% list_of_finished_dirs)]

 sfInit(parallel=TRUE,cpus=20)
# # # 
 sfLibrary(plyr)
 sfLibrary(reshape2)
 sfLibrary(doBy)
# # # 
  sfLibrary(igraph)
 sfLibrary(data.table)
 sfLibrary(snowfall)
 sfExport("controls")
 sfExport("n_turns_to_ignore")
 sfExport("min_edge_weight_for_network")
# 
print(length(list_of_dirs))
out <- parLapply(sfGetCluster(),list_of_dirs,function(output){
#out <- lapply(list_of_dirs$V1,function(output){
 tryCatch({
     # print(output)
    ##Load params
      params <- read.csv(paste0(output,"/params.csv"))
    ##Load groups
      groups <- read.csv(paste0(output,"/groups.csv"))
      if(nrow(groups)==0){
        ###Then we didn't have any groups...
        groups <- data.frame(agent=0:999, group=0,weight=1)
      }
      names(groups) <- c("agent","group","weight")
      agent_groups <- dlply(groups,.(agent), function(l){l$group})
    ##Load interactions
      interactions <- read.csv(paste0(output,"/interactions.csv"))
      #pull out self interactions and ignored turns
      interactions <- interactions[interactions$Time > n_turns_to_ignore & 
                                     interactions$agent != interactions$agent.1,]
      #get group overlap for each interaction
      interactions$group_over <- apply(interactions,1, function(f){
        length(intersect(agent_groups[[f[[1]]+1]],
                         agent_groups[[f[[2]]+1]]))})
    ##Generate group structure output
      
      if(nrow(interactions)==0){
        group_output <- data.frame(prop_mean=1,
                                   count_mean=NA)
      } else { 
        tmp_out <- ddply(interactions, .(Time),
                         function(l){
                           data.frame(
                             proportion=nrow(l[l$group_over>0,])/nrow(l),
                             mean_count=mean(l$group_over),
                             sd = sd(l$group_over)
                           )})
        #mod <- lm(proportion~Time,tmp_out)
        group_output <- data.frame(prop_mean=mean(tmp_out$proportion),
                                   count_mean=mean(tmp_out$mean_count))
        rownames(group_output) <- NULL
      }
    ##Generate graph output
      if(nrow(interactions)==0){
        graph_output <- data.frame(##Properties of the LSCC
                                  max_SCC=1,
                                  n_edges_max_SCC =0,
                                  avg_pl_SCC=NA,
                                  unweighted_CC=0,
                                  ##Properties of the total graph
                                  n_edges_total = 0,
                                  unweighted_CC=0,
                                  ##reciprocity measures
                                  reciprocity_SCC= 0,
                                  reciprocity_Total = 0,
                                  ##mean_reciprocal_diff_SCC = XXXX,
                                  ##mean_reciprocal_diff_Total = XXX,
                                  max_WCC=1,
                                  ##Edge Weight
                                  mean_edge_weight = 0,
                                  sd_edge_weight=0,
                                  mean_weighted_CC_SCC=0,
                                  mean_weighted_CC_Total=0,
                                  ingroup_Total =0,
                                  outgroup_Total=0)
      } else{ 
        edges <- data.table(interactions)
        edges$agent <- edges$agent +1
        edges$agent.1 <- edges$agent.1+1
        z <- edges[,list(V1=sum(agent.x.agent),ingroup=sum(group_over)>0),by=c("agent","agent.1")]
        z <- z[z$V1 >= min_edge_weight_for_network,]
        g <- graph.edgelist(as.matrix(z[,c("agent","agent.1"),with=FALSE]),directed=T);
        E(g)$weight <- z$V1
        cc_list <- clusters(g,mode="strong")
        slcc<-induced.subgraph(g, V(g)[which(cc_list$membership == which.max(cc_list$csize))])
        
        graph_output <- data.frame(##Properties of the LSCC
                                   max_SCC=max(cc_list$csize),
                                   n_edges_max_SCC = length(E(slcc)),
                                   avg_pl_SCC=average.path.length(slcc,directed=F),
                                   unweighted_CC_SCC=transitivity(slcc),
                                    
                                   ##Properties of the total graph
                                   n_edges_total = length(E(g)),
                                   unweighted_CC_Total=transitivity(g),
                                   ##reciprocity measures
                                   reciprocity_SCC= reciprocity(slcc),
                                   reciprocity_Total = reciprocity(g),
                                   ##mean_reciprocal_diff_SCC = XXXX,
                                   ##mean_reciprocal_diff_Total = XXX,
                                   max_WCC=max(clusters(g,mode="weak")$csize),
                                   ##Edge Weight
                                   mean_edge_weight = mean(z$V1),
                                   sd_edge_weight=sd(z$V1),
                                   mean_weighted_CC_SCC=mean(transitivity(slcc,"weighted")),
                                   mean_weighted_CC_Total=mean(transitivity(g,"weighted")),
                                   ingroup_Total=nrow(z[z$ingroup,]),
                                   outgroup_Total=nrow(z[!z$ingroup,])                             
                            )
      }
    ##Generate TM output
      tm_out <- read.csv(paste0(output,"/tm.csv"))
      tm_out <- tm_out[tm_out$TimePeriod > n_turns_to_ignore,]
      tm_output <- data.frame(agent_core_mean=mean(tm_out$CoreSize),
                              KB_mean = mean(tm_out$KnowledgeBitsHeld),
                              GO_mean = mean(tm_out$GeneralizedOtherKBHeld),
                              group_mean = mean(tm_out$GroupSize))
    #writeLines(c("SUCCESS"), fileConn)
    ##Bind everything together
    write.csv(cbind(dcast(params,1~parameter),
            tm_output,
            graph_output,
            group_output),
            paste0("D:/kjoseph/success6/",basename(output),".csv"))
   
  }, warning = function(war) {
  }, error = function(err) {
    return(err)
  }, finally = {    
  })
           
})
#write.csv(rbind.fill(out),"D:/kjoseph/Dropbox/results.csv")
sfStop()
d <- list.files("D:/kjoseph/success6/",full.names=T)
z <- lapply(d,function(l){read.csv(l,stringsAsFactors=F)})
library(data.table)
r <- rbindlist(z)
write.csv(r,"D:/kjoseph/Dropbox/current_papers/TransMem_Paper/new_ve/results_full.csv")

##Generate Knowledge output
#knowledge <- read.csv(paste0(output,"/knowledge.csv"),header=FALSE)
#knowledge$tp <- rep(c(1,50,100,150),each=1000)
#z <- ddply(knowledge, .(tp), function(l){
#  colSums(l)[1:500]
#})
##maybe work out what the poisson parameter would be
#plot(fitdist(as.numeric(z[2,-1]),'pois'))
#ggplot(melt(z,id="tp"), aes(value,color=factor(tp)))+ geom_density()+scale_x_log10()
#z <- ddply(knowledge, .(tp), function(l){
#  gofstat(fitdist(colSums(l)[1:500],"pois"))$chisq
#})
#ggplot(z, aes(V1,color=factor(tp)))+ geom_density()+scale_x_log10()
#gf <- goodfit(blah[blah$tp==20,"value"], type="poisson", method="MinChisq")  
# interactions_output <- tmp_out[tmp_out$Time %in% c(seq(1,150,by=15),149),]
