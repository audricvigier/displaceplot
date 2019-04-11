


#' Call compareSimSimPlot to generate a wealth of graphics (better to call callCompareSimSimPlot())
#'
#' This function generates numerous graphics in a hierarchy of folders
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
#' callCompareSimSimPlots (general,
#'                           the_baseline="svana_baseline",
#'                           explicit_pops=explicit_pops,
#'                           selected = "_selected_set1_",
#'                           selected_vessels=selected_set1,
#'                           years_span=2015:2019,
#'                             per_pop=FALSE,
#'                           per_vessel=TRUE)
#'  #=> produce files in output folders....
#'  }




callCompareSimSimPlots <- function(general,
                        the_baseline="svana_baseline",
                        explicit_pops=explicit_pops,
                        selected= "_selected_set1_",
                        selected_vessels=selected_set1,
                        years_span=2015:2019,
                        per_pop=FALSE,
                        per_vessel=TRUE){


  others_than_baseline  <- general$namefolderoutput[!general$namefolderoutput %in%  the_baseline]   ## CAUTION

 if(TRUE) write(c("id","sce","vid", "effort_base", "effort_sce", "gain_effort", "gain_seffort",
                     "baseline_totland_av",  "baseline_totland_explicit_av","gain_totland","gain_totland_explicit", "gain_totland_implicit",
                      "gain_av_vpuf","gain_av_vapuf", "gain_av_revenue", "gain_av_rev_av_prices", "gain_av_gav", "gain_av_gradva",
                      "gain_fcpue_all",  "gain_fcpue_explicit", "gain_fcpue_implicit", paste("gain_fcpue_pop", explicit_pops, sep=""),
                      "gain_av_trip_duration", "gain_av_traveled_dist", "gain_av_nbtrip"), ncol=24+length(explicit_pops),  ## CAUTION NCOL HERE ##
                   file=file.path(general$main.path, general$namefolderinput,
                       paste("vid_indicators_gain_in_totland_and_vpuf_",the_baseline,".txt", sep='')),
                     append = FALSE, sep = " ") # init


 if(TRUE) write(c("id","sce","vid", "simu", "effort_base", "effort_sce", "gain_effort", "gain_seffort",
                     "baseline_totland_av",  "baseline_totland_explicit_av","gain_totland","gain_totland_explicit", "gain_totland_implicit",
                      "gain_av_vpuf","gain_av_vapuf", "gain_av_revenue", "gain_av_rev_av_prices", "gain_av_gav", "gain_av_gradva",  "gain_fuelcost",
                      "gain_fcpue_all",  "gain_fcpue_explicit", "gain_fcpue_implicit", paste("gain_fcpue_pop", explicit_pops, sep=""),
                      "gain_av_trip_duration", "gain_av_traveled_dist", "gain_av_nbtrip"), ncol=26+length(explicit_pops),  ## CAUTION NCOL HERE ##
                   file=file.path(general$main.path, general$namefolderinput,
                     paste("vid_indicators_gain_in_totland_and_vpuf_",the_baseline,"_per_simu.txt", sep='')),
                     append = FALSE, sep = " ") # init






   ## OR PER COUNTRY---------------------
  for (sce in others_than_baseline){
     cat (paste(sce, "--------------------------------------------------------------\n"))




     what2 <- "weight"
     lst_loglike_w_agg_den_1 <- get(paste("lst_loglike_agg_",what2, selected, the_baseline, sep=''))
     lst_loglike_w_agg_vid_1 <- get(paste("lst_loglike_agg_",what2,"_vid_", the_baseline, sep=''))



     what2 <- "weight"
     lst_loglike_w_agg_den_2 <- get(paste("lst_loglike_agg_",what2, selected, sce, sep=''))
     lst_loglike_w_agg_vid_2 <- get(paste("lst_loglike_agg_",what2,"_vid_", sce, sep=''))

     sce1                  <- the_baseline
     sce2                  <- sce

     combined_name         <- paste(the_baseline,"_vs_", sce, sep='')


    ## PER COUNTRY --- WEIGHT
    compareSimSimPlots(
                      lst_loglike_agg_1=lst_loglike_w_agg_den_1,
                      lst_loglike_agg_2=lst_loglike_w_agg_den_2,
                      explicit_pops=explicit_pops,
                      years_span=years_span,
                      idx.sim=list(idx.sim.1= names(lst_loglike_w_agg_den_1)[-length(names(lst_loglike_w_agg_den_1))],
                                    idx.sim.2= names(lst_loglike_w_agg_den_2)),
                      combined_name=combined_name,
                      a.comment="den",
                      what="per_country",
                      what2="weight",
                      a.unit=1,
                      count=0,
                      plot_obs=FALSE,
                      general=general
                      )   #den


   }




   ## OR PER VESSELS---------------------
  for (sce in others_than_baseline){
     cat (paste(sce, "--------------------------------------------------------------\n"))

     what2 <- "weight"
     lst_loglike_w_agg_den_1 <- get(paste("lst_loglike_agg_",what2, selected, the_baseline, sep=''))
     lst_loglike_w_agg_vid_1 <- get(paste("lst_loglike_agg_",what2,"_vid_", the_baseline, sep=''))
     lst_loglike_w_agg_vid_1 <- lapply(lst_loglike_w_agg_vid_1, function(x) x[x$VE_REF %in% selected_vessels,])

     what2 <- "weight"
     lst_loglike_w_agg_den_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
     lst_loglike_w_agg_vid_2 <- get(paste("lst_loglike_agg_",what2,"_vid_", sce, sep=''))
     lst_loglike_w_agg_vid_2 <- lapply(lst_loglike_w_agg_vid_2, function(x) x[x$VE_REF %in% selected_vessels,])

    sce1                  <- the_baseline
    sce2                  <- sce

    combined_name         <- paste(the_baseline,"_vs_", sce, sep='')


    # calls per vessel  (set per_vessel at FALSE if you want to calls per pop instead...)
    par(mfrow=c(3,3))
    count <- 0
    for(vid in unique(lst_loglike_w_agg_vid_1[[1]]$VE_REF)){
        count <- count+1
        cat (paste(vid, "\n"))
          lst_loglike_w_agg_this_1 <- lapply(lst_loglike_w_agg_vid_1, function(x) x[x$VE_REF==vid,])
          lst_loglike_w_agg_this_1 <- lapply(lst_loglike_w_agg_this_1, function(x) merge(x, expand.grid(VE_REF=x[1,"VE_REF"], year.month=levels(x$year.month)), all=T) )  # all.combi to fill in the gap
          lst_loglike_w_agg_this_1 <- lapply(lst_loglike_w_agg_this_1, function(x) replace(x, is.na(x), 0) )

          lst_loglike_w_agg_this_2 <- lapply(lst_loglike_w_agg_vid_2, function(x) x[x$VE_REF==vid,])
          lst_loglike_w_agg_this_2 <- lapply(lst_loglike_w_agg_this_2, function(x) merge(x, expand.grid(VE_REF=x[1,"VE_REF"], year.month=levels(x$year.month)), all=T) )  # all.combi to fill in the gap
          lst_loglike_w_agg_this_2 <- lapply(lst_loglike_w_agg_this_2, function(x) replace(x, is.na(x), 0) )




          if(length(grep("DNK", vid))!=0 || length(grep("DEN", vid))!=0 || length(grep("ITA", vid))!=0 ){
            if(per_vessel) {  # PER VESSEL
              # weight
              compareSimSimPlots(
                      lst_loglike_w_agg_this_1,
                      lst_loglike_w_agg_this_2,
                      explicit_pops=explicit_pops,
                      years_span=years_span,
                     idx.sim=list(idx.sim.1= names(lst_loglike_w_agg_den_1)[-length(names(lst_loglike_w_agg_den_1))],
                                    idx.sim.2= names(lst_loglike_w_agg_den_2)),
                           combined_name=combined_name,
                      a.comment=vid,
                      what="per_vessel",
                      what2="weight",
                       plot_obs=TRUE,
                       general=general,
                       vid=vid
                      )
             cat (paste('...done', "\n"))

             graphics.off()
             }
             if(per_pop)  {        # PER POP
              compareSimSimPlots(
                      lst_loglike_w_agg_this_1,
                      lst_loglike_w_agg_this_2,
                      explicit_pops=explicit_pops,
                       years_span=years_span,
                    idx.sim=list(idx.sim.1= names(lst_loglike_w_agg_den_1)[-length(names(lst_loglike_w_agg_den_1))],
                                    idx.sim.2= names(lst_loglike_w_agg_den_2)),
                         combined_name=combined_name,
                      a.comment="pop.1",
                      what="per_pop",
                      what2="weight",
                      count=count
                      )
              }
            }









        }   # end for vid



  } # end for sce




  return()
  }

