            
#' Displace plot 
#'
#' This function set the general variable useful for relative file path etc. 
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' setGeneralVariable(case_study="DanishFleet")  



setGeneralOverallVariable <- function (main_path_outputs =file.path("C:","DISPLACE_outputs"),
                                       case_study="DanishFleet",
                                       igraph=41,
                                       a.year="2015",
                                       a.country="DEN",
                                       nbpops=39,
                                       nbszgroup=14,
                                       namefolderinput="DanishFleet",
                                       the_scenarios1= c("svana_baseline", 
                                                       "svana_sub1mx20", 
                                                       "svana_sub4mx20", 
                                                       "svana_sub4mx5ns20bt", 
                                                       "svana_sub4mx20ns5bt", 
                                                       "svana_sub4mx5ns5bt" ),
                                       nbsimus=20
                                       ){

 # GENERAL SETTINGS
  general <<- list()

  general$case_study <- case_study

 if(.Platform$OS.type == "unix") {}
  general$main.path         <- file.path("~", main_path_outputs)                                                                                                   
  general$main.path.param   <- file.path("~","ibm_vessels", paste("DISPLACE_input_",general$case_study, sep=""))
  general$main.path.ibm     <- file.path("~","ibm_vessels", paste("DISPLACE_input_", general$case_study, sep='')) 
  # do not forget to install the R packages on the qrsh interactive node linux platform, i.e. R > install.packages("data.table"), etc.
  # (and possibly kill the current jobs on HPC with the command qselect -u $USER | xargs qdel)
  # submit the shell to HPC with the command qsub ./IBM_processoutput_plots_for_loglike.sh
  
 if(.Platform$OS.type == "windows") {
  general$main.path         <- main_path_outputs                                                                                                   
  general$main.path.param   <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_gis_",general$case_study, sep=""))
  general$main.path.ibm     <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_", general$case_study, sep='')) 
 }

 

    general$igraph            <- igraph
    general$a.year            <- a.year
    general$a.country         <- a.country
    general$nbpops            <- nbpops  
    general$nbszgroup         <- nbszgroup
    general$namefolderinput   <- case_study
  
    general$namefolderoutput   <- the_scenarios
    general$namesimu           <- list(
                                 svana_baseline=   paste("simu", c(1:nbsimus), sep=''),
                                 svana_sub1mx20=   paste("simu", c(1:nbsimus), sep=''),
                                 svana_sub4mx20=   paste("simu", c(1:nbsimus), sep=''),
                                 svana_sub4mx5ns20bt=   paste("simu", c(1:nbsimus), sep=''),
                                 svana_sub4mx20ns5bt=   paste("simu", c(1:nbsimus), sep=''),
                                 svana_sub4mx5ns5bt=   paste("simu", c(1:nbsimus), sep='')  
                                                                    
                                 ) 

 


   }

return(general)
} 



