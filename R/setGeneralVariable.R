            
#' Setting the general variable to use across other routines
#'
#' This function sets the general variable useful for relative file paths etc. 
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' general <- setGeneralOverallVariable()



setGeneralOverallVariable <- function(pathToRawInputs =file.path("C:", "Users", "fbas", 
                                                 "Documents", "GitHub", paste0("DISPLACE_input_gis_", 
                                                  general$case_study)),
                                       pathToDisplaceInputs = file.path("C:", "Users", "fbas", 
                                                 "Documents", "GitHub", paste0("DISPLACE_input_", general$case_study)),
                                       pathToOutputs =file.path("C:","DISPLACE_outputs"),
                                       caseStudy="DanishFleet",
                                       iGraph=41,
                                       iYear="2015",
                                       iCountry="DEN",
                                       nbPops=39,
                                       nbSzgroup=14,
                                       nameFolderInput="DanishFleet",
                                       theScenarios= c("svana_baseline",
                                                       "svana_sub1mx20",
                                                       "svana_sub4mx20",
                                                       "svana_sub4mx5ns20bt",
                                                       "svana_sub4mx20ns5bt",
                                                       "svana_sub4mx5ns5bt" ),
                                       nbSimus=20,
                                       useSQLite=FALSE    
                                       ){

 # GENERAL SETTINGS
  general <- list()

  general$case_study <- caseStudy

  general$main.path         <- pathToOutputs                                                                                                   
  general$main.path.param   <- pathToRawInputs
  general$main.path.ibm     <- pathToDisplaceInputs
 

    general$igraph            <- iGraph
    general$a.year            <- iYear
    general$a.country         <- iCountry
    general$nbpops            <- nbPops  
    general$nbszgroup         <- nbSzgroup
    general$namefolderinput   <- caseStudy
  
    general$namefolderoutput   <- theScenarios
    general$namesimu           <- list(
                                 svana_baseline=   paste("simu", c(1:nbSimus), sep=''),
                                 svana_sub1mx20=   paste("simu", c(1:nbSimus), sep=''),
                                 svana_sub4mx20=   paste("simu", c(1:nbSimus), sep=''),
                                 svana_sub4mx5ns20bt=   paste("simu", c(1:nbSimus), sep=''),
                                 svana_sub4mx20ns5bt=   paste("simu", c(1:nbSimus), sep=''),
                                 svana_sub4mx5ns5bt=   paste("simu", c(1:nbSimus), sep='')  
                                                                    
                                 ) 

 
    general$use_sqlite <- useSQLite

   

return(general)
} 


