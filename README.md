sscr_14
=======

This repository holds the code used in the following article:

Joseph, K., Morgan, G. P., Martin, M.K. and Carley, K.M. 
On the coevolution of stereotype, culture and social relationships. *to appear in Social Science Computer Review*

Original results from the paper are in results_full.csv

*Note:* The executable is Windows-based. I've put together a Unix version, but haven't yet replicated results on a unix system, so I'm holding off putting up the build in the repo. Lemme know if you want to try it on a unix machine, though.

The code is intended to provide a chance for replication analyses to be completed and as a stepping stone for future work.  

Please note, however, that I can't release code for the executable Construct.exe due to external restrictions.  If you're interested in how it works, though, I'd be more than happy to talk. You can also take a look at further documentation, available in the following article:

Carley, K. M., Filonuk, D. T., Joseph, K., Kowalchuck, M., Lanham, M. J., & Morgan, G. P. (2012). Construct User Guide.

The article is available at:
http://reports-archive.adm.cs.cmu.edu/anon/anon/usr/ftp/home/ftp/isr2012/CMU-ISR-12-112.pdf

To replicate, run gen_experiment.R to generate the directory structure for the experiments

I then used submit.R in conjunction with the CONDOR high throughput computing system to complete the runs- if you don't have a CONDOR cluster set up, you might want to do something similar.  If not, just run the sims locally, as they shouldn't really take that long. Just make sure the executable can find the deck - the easiest way to do this is to copy the executable and deck into each directory and then run from there (ie ./Construct.exe deck.xml)

I aggregated results with analysis.R and then produced plots with final_results_plots.R

