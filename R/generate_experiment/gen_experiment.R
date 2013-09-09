#source("globals.R")
#source("functions.R")

library(stringr)
library(snowfall)

sfInit(parallel=TRUE,cpus=2)
sfLibrary(stringr)
sfSource("C:/Users/kjoseph/Dropbox/Kenny/current_papers/TransMem_Paper/new_ve/R/generate_experiment/functions_for_generating.R")
sfExport("data_dir")
sfExport("nreps")
sfExport("induced_homophily_filename")
sfExport("group_filename")
sfExport("knowledge_filename")
l <- parApply(sfGetCluster(),all_combos,1,function(combo){
#apply(all_combos,1,function(combo){
  for(repetition in 1:(nreps)){
    ##CHANGE DIRECTORY...
    out_dir <- paste0(data_dir, paste(c(combo,repetition),collapse="_"))
    out_dir <- str_replace_all(out_dir," ","")
    dir.create(out_dir)
    ###WRITE PARAMETERS
    params_file<-file(paste0(out_dir,"/params.csv"))
    writeLines(c("parameter,value",
                 paste0("Homophily_File_Name,",induced_homophily_filename),
                 paste0("Group_File_Name,",group_filename),
                 paste0("Knowledge_File_Name,",knowledge_filename),
                 paste(names(combo),combo,sep=","),
                 paste0("Repetition,",repetition)
                 )
               , params_file)
    close(params_file)
    
    homophily_fn <- paste0(out_dir,"/",induced_homophily_filename)
    if(as.character(combo["Homophily"]=="off")){
      full_mat <- matrix(data=1,nrow=as.numeric(combo["Agent_Count"]),ncol=as.numeric(combo["Agent_Count"]))
      write.table(full_mat,homophily_fn,row.names=FALSE,col.names=FALSE, sep=",")
    } else {
      write_induced_homophily_file(homophily_fn,
                                 combo["Agent_Count"],
                                 combo["Homophily_Generation_Mechanism"],
                                 combo["Homophily_Clusters"],
                                 combo["Distance_Matrix_Scaling_Homophily"])
    }
    
    group_fn <- paste0(out_dir, "/",group_filename)
    if(as.numeric(combo["Group_Count"]) == 1){
      ##Construct automatically includes a single group (the generalized other)
      group_file<-file(group_fn)
      writeLines(c("source,target,weight"),con=group_fn)
      close(group_file)
    } else {
      write_grouping_file(group_fn,
                          combo["Agent_Count"],
                          combo["Group_Count"],
                          combo["Groups_Per_Agent"],
                          combo["Generation_Mechanism_Group"])
    }
    knowledge_fn <- paste0(out_dir, "/",knowledge_filename)
    write_knowledge_file(knowledge_fn,
                         group_fn,
                         combo["Agent_Count"],
                         combo["Knowledge_Count"],
                         combo["Mean_Knowledge"],
                         combo["Generation_Mechanism_Knowledge"])
  }
  return(out_dir)
})
#sfStop()