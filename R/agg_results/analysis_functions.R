
plot_interactions_across_groups_over_time <- function(output,n_turns_to_ignore,plot_type="proportion"){
  groups <- read.csv(paste0(output,"/groups.csv"))
  
  if(nrow(groups)==0){
    ###Then we didn't have any groups...
    stop("no groups in this output")
  }
  agent_groups <- dlply(groups,.(agent), function(l){l$group})
  interactions <- read.csv(paste0(output,"/interactions.csv"))
  interactions <- interactions[interactions$Time > n_turns_to_ignore &
                                 interactions$agent != interactions$agent.1,]
  interactions$group_over <- apply(interactions,1, function(f){length(intersect(agent_groups[[f[[1]]+1]],agent_groups[[f[[2]]+1]]))})
  return(switch(plot_type,
                proportion = ggplot(ddply(interactions, .(Time),function(l){nrow(l[l$group_over>0,])/nrow(l)}),aes(Time,V1))+geom_point()+geom_line(),
                count = ggplot(ddply(interactions, .(Time),function(l){mean(l$group_over)}),aes(Time,V1))+geom_point()+geom_line(),
                compare = ggplot(melt(ddply(interactions, .(Time),function(l){data.frame(a=nrow(l[l$group_over>0,]),b=nrow(l))}),id="Time"),aes(Time,value,color=variable))+geom_point()+geom_line()
  ))
}

vis_tm <- function(output){
  tm_out <- read.csv(paste0(output,"/tm.csv"))
  tm_out <- tm_out[tm_out$TimePeriod > n_turns_to_ignore,]
  blah <- summaryBy(d, FUN=mean)
  ggplot(melt(blah[blah$TimePeriod >20,-2],id="TimePeriod"), aes(TimePeriod,value)) + geom_point() + geom_line() + facet_wrap(~variable, scales="free_y")
  blah <- summaryBy(.~TimePeriod, d[,c("TimePeriod","CoreSize","GroupSize")], FUN=list(mean,sd,length))
  blah$GroupSE <- sqrt(blah$GroupSize.sd)/1000
  blah$AgentSE <- sqrt(blah$CoreSize.sd)/1000
  ggplot(blah[blah$TimePeriod >20,], aes(TimePeriod,GroupSize.mean,ymin=GroupSize.mean-1.96*GroupSE, ymax=GroupSize.mean+1.96*GroupSE)) + geom_point() + geom_line() + geom_errorbar(color='red')
  ggplot(blah[,], aes(TimePeriod,CoreSize.mean,ymin=CoreSize.mean-1.96*AgentSE, ymax=CoreSize.mean+1.96*AgentSE)) + geom_point() + geom_line() + geom_errorbar(color='red')
  plot(density(d[d$TimePeriod ==45,"GroupSize"]))
  
}

static_vis <- function(interactions, min_weight){
  edges <- interactions
  edges$agent <- edges$agent +1
  edges$agent.1 <- edges$agent.1+1
  z <-ddply(edges,.(agent,agent.1),nrow)
  g <- graph.edgelist(as.matrix(z[z$V1 > min_weight,c(1,2)]),directed=F);
  E(g)$weight <- z[z$V1 > min_weight,"V1"]
  V(g)$color <- color_pal[groups[,2]+1]
  layout <- layout.fruchterman.reingold(g,params=list(weights=E(g)$weight))
  return(plot(g,layout=layout,vertex.label="",
              vertex.frame.color=V(g)$color,
              vertex.size=1,edge.width=1.5,asp=9/16,margin=-0.15))
  g[from=z[z$V1 ==2,1],to=z[z$V1==2,2]] <- FALSE
}

dynamic_vis <- function(interactions){
  #http://markov.uc3m.es/2012/11/temporal-networks-with-igraph-and-r-with-20-lines-of-code/
  require(igraph)
  #load the edges with time stamp
  #there are three columns in edges: id1,id2,time
  edges <- interactions
  edges$agent <- edges$agent +1
  edges$agent.1 <- edges$agent.1+1
  #generate the full graph
  g <- graph.edgelist(as.matrix(edges[,c(1,2)]),directed=F)
  E(g)$time <- edges$Time
  
  #generate a cool palette for the graph
  YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
  YlOrBr.Lab <- colorRampPalette(YlOrBr, space = "Lab")
  #colors for the nodes are chosen from the very beginning
  vcolor <- rev(YlOrBr.Lab(vcount(g)))
  
  #time in the edges goes from 1 to 300. We kick off at time 3
  ti <- 3
  #weights of edges formed up to time ti is 1. Future edges are weighted 0
  E(g)$weight <- ifelse(E(g)$time < ti,1,0)
  #generate first layout using weights.
  layout.old <- layout.fruchterman.reingold(g,params=list(weights=E(g)$weight))
  
  
  #total time of the dynamics
  total_time <- max(E(g)$time)
  #This is the time interval for the animation. In this case is taken to be 1/10 
  #of the time (i.e. 10 snapshots) between adding two consecutive nodes 
  dt <- 0.1
  #Output for each frame will be a png with HD size 1600x900 :)
  png(file="example%03d.png", width=1600,height=900)
  nsteps <- max(E(g)$time)
  #Time loop starts
  for(ti in seq(3,total_time,dt)){
    #define weight for edges present up to time ti.
    E(g)$weight <- ifelse(E(g)$time < ti,1,0) 
    #Edges with non-zero weight are in gray. The rest are transparent
    E(g)$color <- ifelse(E(g)$time < ti,"gray",rgb(0,0,0,0))
    #Nodes with at least a non-zero weighted edge are in color. The rest are transparent
    V(g)$color <- ifelse(graph.strength(g)==0,rgb(0,0,0,0),vcolor)
    #given the new weights, we update the layout a little bit
    layout.new <- layout.fruchterman.reingold(g,params=list(niter=10,start=layout.old,weights=E(g)$weight,maxdelta=1))
    #plot the new graph
    plot(g,layout=layout.new,vertex.label="",vertex.size=1+2*log(graph.strength(g)),vertex.frame.color=V(g)$color,edge.width=1.5,asp=9/16,margin=-0.15)
    #use the new layout in the next round
    layout.old <- layout.new 
  }
  dev.off()
}

visualize_activation_mechanism <- function(){
  ###Describe the activation mechanism
  decayParameter = -0.5
  defaultActivation = .00001
  earlyTurnThreshold = 10
  immediacyDelay = .1
  salience =1 
  eventTurn = 0
  currTurn = 0:100
  ct <- (currTurn + immediacyDelay - eventTurn)
  val <- (ct)^decayParameter
  ##minimum value
  log(defaultActivation)
  ##maximum value
  log(sum(2/sqrt((1:40)+.1)))
  ##with a single activation
  out <- data.frame(curr=currTurn, o=log(defaultActivation+val))
  ggplot(out, aes(curr,o)) + geom_point() +geom_hline(y=-1,color='blue') + geom_hline(y=-5,color='red') + ylab("Activation Score") + xlab("Number of Turns")
}

visualize_annealing <- function(conditions){
  annealing_df <- expand.grid(1:60,c(5,25,50))
  annealing_df$val <- 1-(1/(1+exp(annealing_df$Var1*-1+annealing_df$Var2)))
  ggplot(annealing_df, aes(Var1,val, color=factor(Var2))) + geom_line()+ geom_point() + scale_colour_discrete("GLP") + ylab("Likelihood ofUpdating\nGroup Schema") + scale_y_continuous(labels=percent_format()) + xlab("Number of Turns")
  
}
get_dat <- function(file_name){
  dat <- read.csv(paste0("~/Dropbox/Kenny/current_papers/TransMem_Paper/new_ve/",file_name))
  dat$X <- NULL
  ##get list of output variables
  output_vars <- names(dat)[!(names(dat)%in%names(conditions))]
  #output_vars <- output_vars[output_vars != "Repetition"]
  #get input variables that have >1 level
  input_vars <- names(conditions)[names(conditions) %in% names(dat)]
  input_vars <- input_vars[!(apply(dat[,input_vars],2,function(l){length(levels(factor(l)))<=1}))]
  dat <- dat[,c(input_vars,output_vars)]
  dat[dat$Homophily == "off", "Homophily_Clusters"] <- 0
  
  ##create factors from input variables
  for(var in input_vars[input_vars != "Agent_Bias_Rate"]){
    dat[,var] <- factor(dat[,var])
  }
  dat
}

bar_plot <- function(variable,name,data=cut2){
  df <- data
  df$MCC <- cut(df$max_SCC, c(0,1,900,1000),dig.lab=4)
  blah <- ddply(df,c("GP","GK",variable,"MCC"), 
                function(l){
                  nrow(l)/nrow(df[df$GP==l$GP[1]&
                                  df$GK==l$GK[1]&
                                  df[,variable]==l[1,variable],])},.drop=FALSE)
  
  p <- ggplot(blah,aes_string(x="MCC",y="V1",fill=variable)) 
  p <- p + geom_bar(stat='identity',position='dodge') + facet_grid(GP~GK) 
  p <- p + theme(legend.position='top', axis.text.x=element_text(angle=45,hjust=1)) 
  p <- p + scale_y_continuous(labels=percent_format()) + ylab("Percent of Runs") 
  p <- p + xlab("Size of Largest Strongly Connected Component") + scale_x_discrete(breaks=levels(df$MCC),labels=str_replace(str_replace(str_replace(levels(df$MCC),"]",""),"\\(",""),",","-"))
  blues <- colorRampPalette(c('light blue', 'dark blue'))
  p + scale_fill_manual(name, values=blues(length(levels(df[,variable]))))
}

get_interactions <- function(g,n){
 ret<- unlist(apply(combn(1:ncol(g),n),2,function(l){ 
    run_call ="unique(paste("
    run_call = paste0(run_call, paste0("g[,", l,"]",collapse=","),",sep=\":\"))")
    eval(parse(text=run_call))
  }))
}