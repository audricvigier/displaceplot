


#' Generate individual vessels time series on output variables from vessel aggregations
#'
#' This function is a quick way to help interpretation of results by a quick and dirty check on individual trajectories
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples                                                          
#' 
#' \dontrun{
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
#'
#'
#'  loadLoglikeFiles (general, use_port_info=FALSE)
#'
#'
#'
#'  compareSimSimIndividualVidCumTraj(general=general,
#'                                     vessel_indexes=200:210,
#'                                     a_var="gradva", 
#'                                     sce_baseline="svana_baseline", 
#'                                     sce_tested="svana_sub4mx20",
#'                                     nbiters=1,
#'                                     nby=5)
#'
#' }







 compareSimSimIndividualVidCumTraj <- function(general=general,
                     vessel_indexes=200:250, 
                     a_var="gradva",
                     sce_baseline="svana_baseline",
                     sce_tested="svana_sub4mx20",
                     nbiters=1,
                     nby=5){
   library(doBy)
   nbiters <-nbiters
   obj1 <- get(paste("lst_loglike_agg_weight_vid_", sce_baseline, sep=''), env=.GlobalEnv)
   obj2 <- get(paste("lst_loglike_agg_weight_vid_", sce_tested, sep=''), env=.GlobalEnv)

   vessel_names <- unique(as.character(obj1[[1]]$VE_REF))[vessel_indexes]

   xx      <- obj1[[1]][ obj1[[1]]$VE_REF %in% vessel_names, ]
   cumsums <- unlist(lapply(split(xx, f=xx$VE_REF), function(x) cumsum (x [, a_var])[nrow(x)]   )) 
   print(cumsums)
   

   
   plot(1:(nby*12), 1:(nby*12), ylim=c(quantile(obj1[[1]][ obj1[[1]]$VE_REF %in% vessel_names, a_var], 0.05), 
                           quantile(cumsums, 1)),
                            type="n", xlab="# Month", ylab="GVA")
   print( unique(as.character(obj1[[1]]$VE_REF))[vessels])
   
   for(i in 1:nbiters){
     obj1[[i]] <- obj1[[i]]  [obj1[[i]]$VE_REF %in% vessel_names,]
     obj2[[i]] <- obj2[[i]]  [obj2[[i]]$VE_REF %in% vessel_names,]

     year  <- as.numeric(unlist(lapply(strsplit(as.character(obj1[[i]]$year.month), split="\\."), function(x)x[1]))) 
     month <- as.numeric(unlist(lapply(strsplit(as.character(obj1[[i]]$year.month), split="\\."), function(x)x[2]))) 
     obj1[[i]]$month <- month + 12*(year-(min(year, na.rm=T))) 
     obj1[[i]] <- obj1[[i]] [!is.na(obj1[[i]]$month),]
 
     year  <- as.numeric(unlist(lapply(strsplit(as.character(obj2[[i]]$year.month), split="\\."), function(x)x[1]))) 
     month <- as.numeric(unlist(lapply(strsplit(as.character(obj2[[i]]$year.month), split="\\."), function(x)x[2]))) 
     obj2[[i]]$month <- month + 12*(year-(min(year, na.rm=T))) 
     obj2[[i]] <- obj2[[i]] [!is.na(obj2[[i]]$month),]
 
     lapply(split(obj1[[i]], f=obj1[[i]]$VE_REF), function(x, a_var=a_var){ 
                                                          x <- merge(x, data.frame(month=1:(nby*12), VE_REF=x$VE_REF[1]), all=TRUE)
                                                          x <- orderBy(~ month, data=x)
                                                          x[, a_var][is.na(x[, a_var])] <- 0
                                                          lines(x$month, cumsum(x[, a_var]), col=1)
                                                          points(x$month, cumsum(x[, a_var]), col=1)
                                                          }, a_var=a_var)
     lapply(split(obj2[[i]], f=obj2[[i]]$VE_REF), function(x, a_var=a_var){
                                                          x <- merge(x, data.frame(month=1:(nby*12), VE_REF=x$VE_REF[1]), all=TRUE)
                                                          x <- orderBy(~ month, data=x)
                                                          x[, a_var][is.na(x[, a_var])] <- 0
                                                          lines(x$month, cumsum(x[, a_var]), col=2)
                                                          points(x$month, cumsum(x[, a_var]), col=2)
                                                          }, a_var=a_var)

 }
return()
}  


