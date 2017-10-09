

#' Displace plot
#'
#' This function re-load the files produced by getAggLoglikeFiles()
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' loadLoglikeFiles(general=general, use_port_info=FALSE)

loadLoglikeFiles <- function (general, use_port_info=FALSE){

 # reload scenarios for what="weight"
 if(general$case_study=="DanishFleet"){

 for (sce in general$namefolderoutput){
  if(sce %in% general$namefolderoutput) load(file=file.path(general$main.path, general$namefolderinput,
                 sce, paste("lst_loglike_weight_agg_",sce,".RData", sep='')))
  if(!use_port_info) rm(paste("lst_loglike_weight_agg_weight_vid_port_",sce, sep='')) ; gc(TRUE)  # remove bc too memory demanding
  }

 }


return()
}




