#source("globals.R")
list_of_dirs <- list.files(substr(data_dir,1,nchar(data_dir)-1),full.names=TRUE)
n_per_segment <- 60
minutes_to_sleep <- 2.75
segments <- split(list_of_dirs, ceiling(seq_along(list_of_dirs)/n_per_segment))

submit_file_index <- 0
dir.create(paste0(top_dir,"/submit"))
for(segment in segments){
  print(segment)
  sf_name <-paste0(top_dir,"submit/submit",submit_file_index,".txt")
  submit_file <- file(sf_name,open="w")
  writeLines(submit_file_text,con=submit_file)
  writeLines(paste("initialdir = ", segment,"\nqueue\n"),con=submit_file)
  close(submit_file)
  system(paste("condor_submit",sf_name))
  submit_file_index <- submit_file_index + 1
  Sys.sleep(minutes_to_sleep * 60)

}
