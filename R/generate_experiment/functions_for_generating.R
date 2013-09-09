require(VGAM)

standardize_intervals <- function(times,newrange){
  thisrange = range(times)
  stopifnot(thisrange[2]>thisrange[1])
  stopifnot(newrange[2]>newrange[1])
  (times-thisrange[1])/diff(thisrange)*diff(newrange) + newrange[1]
}

simulate_events <- function(nsim=100,type="bursty",shape=2,location=1,rate=1,...){
  return(switch(type,
          bursty = function(nsim){rpareto(nsim,shape=shape,location=location)},
          persistent = function(nsim){rexp(nsim,rate=rate)},
          uniform = function(nsim){runif(nsim)},
          normal = function(nsim){rnorm(nsim,mean=10,sd=1)})(nsim))
}

prepare_events <- function(type="bursty",censored_range=NULL,repetitions=1,nsim=500,...){
  times = c()
  max_t = 0
  min_t = 0
  for(rep in 1:repetitions){
    new_times = simulate_events(nsim=nsim/repetitions,type=type,...)+max_t
    times <- c(times, new_times)
    max_t = max(new_times)
  }
  times = standardize_intervals(times,c(0,1))
  times
}

write_induced_homophily_file <- function(filename, n_agents, gen_mechanism,clusters,scale){
  positions = prepare_events(type=as.character(gen_mechanism),
                             repetitions=as.numeric(clusters),
                             nsim=as.numeric(n_agents))
  distances = dist(positions)
  if(as.character(scale) == "exp"){
    distances <- exp(distances)
    distances <- distances/max(distances)
  }
  write.table(signif(1-as.matrix(distances),digits=3),filename,row.names=FALSE,col.names=FALSE, sep=",")
}



write_grouping_file <- function(filename, n_agents, n_groups, n_groups_per_agent, gen_mechanism){
  grouping <- c()
  for( g in 1:as.numeric(n_groups_per_agent)){
    positions <- prepare_events(type=as.character(gen_mechanism),
                                repetitions=1,
                                nsim=as.numeric(n_agents))
    grouping <- c(grouping,
                  cut(positions,breaks=as.numeric(n_groups),labels=FALSE,include.lowest=TRUE)-1)
  }
  output <- data.frame(agent=rep(0:(as.numeric(n_agents)-1),as.numeric(n_groups_per_agent)),
                       group=grouping,
                       weight=1)
  write.csv(output,filename,row.names=FALSE)
}

write_knowledge_file <- function(filename, 
                                 group_file_name, 
                                 agent_count,
                                 knowledge_count, 
                                 knowledge_density,
                                 gen_mechanism,
                                 percent_ingroup_knowledge=.8){
  
  agent_count <- as.numeric(agent_count)
  knowledge_count <- as.numeric(knowledge_count)
  knowledge_density <- as.numeric(knowledge_density)
  

  if(gen_mechanism == "uniform"){
    knowledge_net <- sample.int (2, agent_count*knowledge_count, TRUE,
                        prob=c(1.-knowledge_density,knowledge_density))-1L
    dim(knowledge_net) <- c(agent_count,knowledge_count)
  }
  else if(gen_mechanism == "all_same"){
    knowledge_net <- rep(sample.int(2, knowledge_count, TRUE,
                           prob=c(1.-knowledge_density,knowledge_density))-1L,each=agent_count)
    dim(knowledge_net) <- c(agent_count,knowledge_count)
    
  } else if(gen_mechanism == "group_based"){
    knowledge_net <- matrix(0,agent_count,knowledge_count)
    
    #get groups
    grouping <- read.csv(group_file_name)
    n_groups <- length(levels(factor(grouping$group)))  
    
    ##get number of bits per agent
    kbs_per_agent <- rbinom(agent_count,knowledge_count,knowledge_density)
    
    kbs <- 1:knowledge_count
    #split KBs into N groups
    group_kbs <- split(kbs,ceiling(seq(knowledge_count)/(knowledge_count/n_groups)))
  
    for(a in 0:(agent_count-1)){
      kb_agent <- kbs_per_agent[a+1]
      in_group_bits <- as.vector(unlist(group_kbs[grouping[grouping$agent == a, "group"]+1]))

      ##if ratio of kb agent has to set / total bits for groups agent is in is less than or equal to percent_ingroup_knowledge
      if(kb_agent/length(in_group_bits) <= percent_ingroup_knowledge){
        # then all bits go into groups
        knowledge_net[a+1, sample(in_group_bits, kb_agent)] <- 1
      } 
      #if ratio of kb agent has to set is / total bits for group agent is in is greater  than percent_ingroup,
      else {
        #set percent_ingroup_knowledge of bits
        samp <- sample(in_group_bits, ceiling(length(in_group_bits)*percent_ingroup_knowledge))
        knowledge_net[a+1,samp] <- 1
        # then spread the rest uniformly
        knowledge_net[a+1,sample(setdiff(kbs,in_group_bits), kb_agent - length(samp))] <- 1
      }
    }
  }

  write.table(knowledge_net,filename,row.names=FALSE,col.names=FALSE, sep=",")
}

