

#' Re-load outcome files of getAggLoglikeFiles() in the global env.
#'
#' This function re-load the files produced by getAggLoglikeFiles()
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#'  \dontrun{
#' general <- setGeneralOverallVariable (pathToRawInputs =file.path("C:", "Users", "fbas", 
#'                                                 "Documents", "GitHub", paste0("DISPLACE_input_gis_", 
#'                                                  general$case_study)),
#'                                       pathToDisplaceInputs = file.path("C:", "Users", "fbas", 
#'                                                 "Documents", "GitHub", paste0("DISPLACE_input_", general$case_study)),
#'                                       pathToOutputs =file.path("C:","DISPLACE_outputs"),
#'                                       caseStudy="DanishFleet",
#'                                       iGraph=41,
#'                                       iYear="2015",
#'                                       iCountry="DEN",
#'                                       nbPops=39,
#'                                       nbSzgroup=14,
#'                                       nameFolderInput="DanishFleet",
#'                                       theScenarios= c("svana_baseline",
#'                                                       "svana_sub1mx20",
#'                                                       "svana_sub4mx20",
#'                                                       "svana_sub4mx5ns20bt",
#'                                                       "svana_sub4mx20ns5bt",
#'                                                       "svana_sub4mx5ns5bt" ),
#'                                       nbSimus=20,
#'                                       useSQLite=FALSE    
#'                                       )
#'
#'
#' loadLoglikeFiles(general=general, use_port_info=FALSE)
#' }

loadLoglikeFiles <- function (general=general, use_port_info=FALSE){

 # reload scenarios for what="weight"
 for (sce in general$namefolderoutput){
  if(sce %in% general$namefolderoutput) {
     load(file=file.path(general$main.path, general$namefolderinput,
                 sce, paste("lst_loglike_weight_agg_",sce,".RData", sep='')), envir = .GlobalEnv)
     if(!use_port_info) rm( list=paste("lst_loglike_agg_weight_vid_port_",sce, sep=''), envir = .GlobalEnv) ; gc(TRUE)  # remove bc too memory demanding
    }
               
  }



return()
}




