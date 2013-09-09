###directory structure
top_dir <- "C:/Users/kjoseph/Desktop/code/test_decks/"
data_dir <- paste0(top_dir,"test_dir/")
induced_homophily_filename = "homophily.csv"
group_filename = "groups.csv"
knowledge_filename = "knowledge.csv"
#list_of_dirs <- list.files(substr(data_dir,1,nchar(data_dir)-1),full.names=TRUE)
#ind <- lapply(list_of_dirs,function(output){file.exists(paste0(output,"/interactions.csv"))})
#l_o_d <- list_of_dirs[unlist(ind)]
###replications
nreps = 1

###analysis
n_turns_to_ignore <- 25
min_edge_weight_for_network <- 1

########CONSTRUCT DEPENDS ON THIS ORDER!!!!
conditions = list(
    ###Constants
    Number_Turns=c(150),
    Agent_Count=c(1000),
    Knowledge_Count=c(500),
    Interactions_per_Turn=c(2),
    Facts_per_Interaction=c(1),
    ###Parameter 1: Memory
    Memory_Rate_Individual=c(0),
    Memory_Rate_Group=c(-1,-5),
    Annealing_Rate_Individual=c(6),
    Annealing_Rate_Group= c(25),
    ##Confound 2: Knowledge Density
    Mean_Knowledge=c(0.4),
    ##Confound 1: Stereotype
    Group_Flip_To_Positive_Rate=c(0.0),
    Group_Flip_To_Negative_Rate=c(0.0),
    Agent_Bias_Rate=c(0.3),
    ##Agent_Flip_To_Negative_Rate=c(0.,0.5,1.0),
    ###Parameter 2: Group Structure
    ##How Many Groups?
    Group_Count=c(4),
    ##How Many Groups Per Agent?
    Groups_Per_Agent=c(1),#2,3
    ##How Is Group Memebership Structured?
    Generation_Mechanism_Group=c("uniform"),#,"normal","bursty"),
    ##Confound 3: Induced Homophily by Distance
    Homophily=c("off"),#"on",
    ##If Homophily is on:
    Homophily_Generation_Mechanism = c("normal"),
    Homophily_Clusters = c(1,5),
    Distance_Matrix_Scaling_Homophily = c("exp"),
    ##Confound 4: Knowledge structure
    Generation_Mechanism_Knowledge=c("group_based")#,"all_same","uniform")
)


all_combos = expand.grid(conditions,stringsAsFactors=FALSE)
all_combos <- all_combos[all_combos$Group_Count >= all_combos$Groups_Per_Agent,]
all_combos <- all_combos[!(all_combos$Homophily == "off" & 
  all_combos$Homophily_Clusters != 1 & 
  all_combos$Distance_Matrix_Scaling_Homophily != "none"
),]

controls <- c("Number_Turns","Agent_Count","Interactions_per_Turn",
              "Facts_per_Interaction","Group_Flip_To_Positive_Rate",
              "Group_Flip_To_Negative_Rate","Homophily_File_Name",
              "Group_File_Name","Knowledge_File_Name","Generation_Mechanism_Group" )
submit_file_text =   
  paste0('universe     = vanilla\n',
         'requirements = ((ARCH == \"INTEL\" || ARCH==\"X86_64\") && ((OPSYS == \"WINNT51\") ||',
         '(OPSYS == \"WINNT52\") || (OPSYS == \"WINNT61\") || (OPSYS == \"WINDOWS\"))',
         '&& target.machine =!= neptune.casos.cs.cmu.edu  && target.machine =!= sif.casos.cs.cmu.edu)\n',
         'should_transfer_files   = YES \n',
         'when_to_transfer_output = ON_EXIT \n',
         'executable 		= Construct.exe\n',
         'transfer_executable 	= true \n',
         'notification 		= Never\n',
         'output 			= out_setup_to_construct.txt\n',
         'error 			= err_setup_to_construct.txt\n',
         'arguments       = deck.xml\n',
         'transfer_input_files = params.csv,', group_filename,", ", induced_homophily_filename, ", ",knowledge_filename,
         ', ../../deck.xml\n',
         'log 			= ', top_dir, 'condor.log\n\n')

