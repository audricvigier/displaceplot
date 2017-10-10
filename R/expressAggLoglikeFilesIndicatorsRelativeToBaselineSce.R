

#' Produce a table relative to the baseline scenario
#'
#' This function produce a table of indicator from processing further the aggregate loglike data
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' \dontrun{
#' general <- setGeneralOverallVariable (main_path_outputs =file.path("C:","DISPLACE_outputs"),
#'                                       case_study="DanishFleet",
#'                                       igraph=41,
#'                                       a.year="2015",
#'                                       a.country="DEN",
#'                                       nbpops=39,
#'                                       nbszgroup=14,
#'                                       namefolderinput="DanishFleet",
#'                                       the_scenarios= c("svana_baseline",
#'                                                       "svana_sub1mx20",
#'                                                       "svana_sub4mx20",
#'                                                       "svana_sub4mx5ns20bt",
#'                                                       "svana_sub4mx20ns5bt",
#'                                                       "svana_sub4mx5ns5bt" ),
#'                                       nbsimus=20
#'                                       )
#'
#'
#' if(FALSE){
#'  aggregratLoglikeFiles(general=general, what="weight",
#'             explicit_pops=explicit_pops2,
#'             implicit_pops=c (4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 25, 27, 28, 29, 33, 34, 35, 36, 37, 38),
#'             selected_vessels_set1=selected_vessels_set_1,
#'             selected_vessels_set2=selected_vessels_set_2,
#'             selected_vessels_set3=selected_vessels_set_3)
#' } else{
#'   loadLoglikeFiles (general, use_port_info=FALSE)
#' }
#'
#'
#' expressAggLoglikeFilesIndicatorsRelativeToBaselineSce(general,
#'                              the_baseline = "svana_baseline",
#'                              sets=c("_selected_set1_", "_selected_set2_", "_selected_set3_"))
#'
#'
#' boxplotAggLoglikeFilesIndicators (general= general,
#'                                             the_baseline="svana_baseline",
#'                                             sets=c("_selected_set1_", "_selected_set2_", "_selected_set3_"))
#'
#'   }



expressAggLoglikeFilesIndicatorsRelativeToBaselineSce <- function(general,
                              the_baseline = "svana_baseline",
                              sets=c("_selected_set1_", "_selected_set2_", "_selected_set3_")) {


   selected <- "_selected_set1_"
   selected <- "_selected_set2_"
   selected <- "_selected_set3_"
   sets <- c("_selected_set1_", "_selected_set2_", "_selected_set3_")



   for (selected in sets){

   outcomes <- NULL



what2 <- "weight"
lst_loglike_w_agg_all_1     <- get(paste("lst_loglike_agg_",what2, selected, the_baseline, sep=''), env=.GlobalEnv)
    dd                      <- table(unlist(lapply(lst_loglike_w_agg_all_1, nrow)))
    expected_nb_rows        <- as.numeric(names(dd[dd==max(dd)]))[1] # most common number of rows
    idx                     <- unlist(lapply(lst_loglike_w_agg_all_1, function(x) nrow(x)==expected_nb_rows))
    namesimu1               <- names(unlist(lapply(lst_loglike_w_agg_all_1, function(x) nrow(x)==expected_nb_rows)))[idx]
    lst_loglike_w_agg_all_1 <- lst_loglike_w_agg_all_1[namesimu1]

lst_loglike_w_agg_vid_1 <- get(paste("lst_loglike_agg_",what2,"_vid_", the_baseline, sep=''), env=.GlobalEnv)


 #*****************************#
 others_than_baseline        <- general$namefolderoutput[!general$namefolderoutput %in% the_baseline]
 #*****************************#

  # quick check at the vessel id level
  plot(density(lst_loglike_w_agg_vid_1[[1]]$gradva))
  lst_loglike_w_agg_vid_2 <- get(paste("lst_loglike_agg_",what2,"_vid_", others_than_baseline[1], sep=''))
  for (ff in namesimu1){
  lines(density(lst_loglike_w_agg_vid_1[[ff]]$gradva),  col=1)
  lines(density(lst_loglike_w_agg_vid_2[[ff]]$gradva),  col=2)
  }





 # fishing effort
  feffort <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "------------------------feffort--------------------------------------\n"))
      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2, selected, sce, sep=''))
        dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(x[,'effort'])- sum(x[,'cumsteaming'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,'effort'])- sum(x[,'cumsteaming'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'effort']))- sum(as.numeric(x[,'cumsteaming'])))))
       ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'effort']))-sum(as.numeric(x[,'cumsteaming'])))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'effort']))-sum(as.numeric(x[,'cumsteaming']))))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<400]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      feffort[sce,"mean"] <- signif(a_mean,5)
     feffort[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="feffort"))
  }
  print(feffort)

  # note that
  #boxplot(dd1/dd2, dd1/rev(dd2)) almost identical so we do not care comparing sim with sim whatever the sim

 # steaming effort
  seffort <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------seffort------------------------------------\n"))
      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,'cumsteaming'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'cumsteaming'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,'cumsteaming']))))
       ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'cumsteaming']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,'cumsteaming'])))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<400]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      seffort[sce,"mean"] <- signif(a_mean,5)
     seffort[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="seffort"))
  }
  print(seffort)

 # percent fishing effort compared to total trip effort
  propfeffort <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "-------------------------propfeffort-------------------------------------\n"))
      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
        dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort']))))
       ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort'])))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<200]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      propfeffort[sce,"mean"] <- signif(a_mean,5)
     propfeffort[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="propfeffort"))
  }
  print(propfeffort)

  # nbtrip
  nbtrip <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "----------------------------nbtrip----------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,'nbtrip'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'nbtrip'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,'nbtrip']))))
       ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'nbtrip']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,'nbtrip'])))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<400]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      nbtrip[sce,"mean"] <- signif(a_mean,5)
     nbtrip[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="nbtrip"))
  }
  print(nbtrip)

 # av_bwtrip
  av_bwtrip <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------av_bwtrip------------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,'av_bwtrip']))))
       ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_bwtrip']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'av_bwtrip'])))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<200]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      av_bwtrip[sce,"mean"] <- signif(a_mean,5)
     av_bwtrip[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="av_bwtrip"))
  }
  print(av_bwtrip)

 #  trip_duration
  av_trip_duration <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------av_effort------------------------------------\n"))
      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'av_effort'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_effort'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,'av_effort']))))
       ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_effort']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'av_effort'])))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<200]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      av_trip_duration[sce,"mean"] <- signif(a_mean,5)
     av_trip_duration[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="av_trip_duration"))
  }
  print(av_trip_duration)



 # totland_implicit
if(any(lst_loglike_w_agg_all_1[[1]]$totland_implicit!=0)) {
  totland <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "------------------------------totland_implicit--------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'totland_implicit']))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_implicit']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_implicit']))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_implicit']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_implicit']))))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<600]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      totland[sce,"mean"] <- signif(a_mean,5)
     totland[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="totland"))
  }
  print(totland)
  }

 # totland_explicit
  totland_explicit <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------totland_explicit------------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'totland_explicit']))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_explicit']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_explicit']))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_explicit']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_explicit']))))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<200]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      totland_explicit[sce,"mean"] <- signif(a_mean,5)
     totland_explicit[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="totland_explicit"))
  }
  print(totland_explicit)


 # totland_
  for(sp in explicit_pops){
  print(sp)
  totland_thissp <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------totland_------------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='') ])))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='') ]))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='') ]))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='') ]))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='')]))))[namesimu1] *100) -100
       if(all(is.na(ratio_percent))) ratio_percent[] <- 0
       if(is.infinite(sum(ratio_percent,na.rm=TRUE))) ratio_percent[] <- 0
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<400]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      totland_thissp[sce,"mean"] <- signif(a_mean,5)
     totland_thissp[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable=paste('totland_', sp, sep='') ))
  }
  print(totland_thissp)
  }



 # revenue
  revenue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------revenue------------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'rev_from_av_prices'])))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'rev_from_av_prices']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'rev_from_av_prices']))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'rev_from_av_prices']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'rev_from_av_prices']))))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      revenue[sce,"mean"] <- signif(a_mean,5)
     revenue[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="revenue"))
  }
  print(revenue)



 # swept area
  sweptarea <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------sweptarea------------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'sweptr'])))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'sweptr']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'sweptr']))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'sweptr']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'sweptr']))))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      sweptarea[sce,"mean"] <- signif(a_mean,5)
     sweptarea[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="sweptarea"))
  }
  print(sweptarea)

 # revenue per swept area
  revenuepersweptarea <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------revpersweptarea------------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'revpersweptarea'])))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'revpersweptarea']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'revpersweptarea']))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'revpersweptarea']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'revpersweptarea']))))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      revenuepersweptarea[sce,"mean"] <- signif(a_mean,5)
     revenuepersweptarea[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="revenuepersweptarea"))
  }
  print(revenuepersweptarea)


  # fuel cost
  fuelcost <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "----------------------------fuelcost----------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'fuelcost'])))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'fuelcost']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'fuelcost']))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'fuelcost']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'fuelcost']))))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      fuelcost[sce,"mean"] <- signif(a_mean,4)
     fuelcost[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="fuelcost"))
  }
  print(fuelcost)


  #  gradva
  gav <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "-------------------------gradva-------------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'gradva']))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'gradva']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'gradva']))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'gradva']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'gradva']))))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      gav[sce,"mean"] <- signif(a_mean,4)
     gav[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="gav"))
  }
  print(gav)


   #  npv net present value NPV assuming 4% discount rate i.e. NPV= Ry1/(1+0.04)^1 + Ry2/(1+0.04)^2 +...
   nbyears         <- nrow(lst_loglike_w_agg_all_1[[1]])/12 # because monthly data
   discount_factor <- sapply(rep(paste("1/((1+0.04)^",1:nbyears,")"), each=12), function(x) eval(parse(text=x)))

  npv <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "----------------------------npv----------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'gradva']))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'gradva']) *discount_factor      )))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'gradva']) *discount_factor      )))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'gradva'])*discount_factor)))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'gradva'])*discount_factor)))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      npv[sce,"mean"] <- signif(a_mean,4)
     npv[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="npv"))
  }
  print(npv)


    # av_vapuf
  av_vpuf <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "------------------------------av_vapuf--------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  mean(x[,'vapuf'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,'vapuf'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'vapuf']))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'vapuf'])))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'vapuf'])))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      av_vpuf[sce,"mean"] <- signif(a_mean,4)
     av_vpuf[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="av_vpuf"))
  }
  print(av_vpuf)

      # av_vapuf_month
  av_vapuf_month <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "----------------------------av_vapuf_month----------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  mean(x[,'av_vapuf_month'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,'av_vapuf_month'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_vapuf_month']))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_vapuf_month'])))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'av_vapuf_month'])))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      av_vapuf_month[sce,"mean"] <- signif(a_mean,4)
     av_vapuf_month[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="av_vpuf_month"))
  }
  print(av_vapuf_month)

     # trip-based CPUEs (useless because somehow redundant with vpuf)
  cpue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "------------------------------trip_based_cpue--------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(as.numeric(x[,'totland']))/sum(x[,'effort'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(as.numeric(x[,'totland']))/sum(x[,'effort'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland']))/sum(x[,'effort']))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland']))/sum(x[,'effort'])))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland']))/sum(x[,'effort'])))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<200]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      cpue[sce,"mean"] <- signif(a_mean,4)
     cpue[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="trip_based_cpue"))
  }
  print(cpue)


       # fishing-based CPUEs
 if(any(lst_loglike_w_agg_all_1[[1]]$totland_implicit!=0)) {
  cpue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "------------------------------cpue_implicit--------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(as.numeric(x[,'totland_implicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(as.numeric(x[,'totland_implicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_implicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_implicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_implicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<400]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      cpue[sce,"mean"] <- signif(a_mean,4)
     cpue[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="fishing_based_cpue"))
  }
  print(cpue)
 }

       # fishing-based CPUEs (explicit only)
  cpue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "-----------------------------cpue_explicit---------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] /
                              unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<600]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      cpue[sce,"mean"] <- signif(a_mean,4)
     cpue[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="fishing_based_cpue_explicit"))
  }
  print(cpue)



        # fishing-based CPUEs
  for(sp in explicit_pops){
  print(sp)
 cpue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------------cpue sp------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(x[,paste('pop.',sp,sep='') ])/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,paste('pop.',sp,sep='')])/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,paste('pop.',sp,sep='')])/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,paste('pop.',sp,sep='')])/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,paste('pop.',sp,sep='')])/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] *100 )-100
       if(all(is.na(ratio_percent))) ratio_percent[] <- 0
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
        if(is.infinite(sum(ratio_percent,na.rm=TRUE))) ratio_percent[] <- 0
        ratio_percent <- ratio_percent[ratio_percent<400]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      cpue[sce,"mean"] <- signif(a_mean,4)
     cpue[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable=paste('fishing_based_cpue_',sp,sep='') ))
  }
  print(cpue)
  }



  # discard rate disc_rate_
  for(sp in explicit_pops){
  print(sp)
  disc_rate_thissp <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "----------------------------------disc_rate_----------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  mean(x[,paste('disc_rate_', sp, sep='') ], na.rm=TRUE)))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,paste('disc_rate_', sp, sep='')], na.rm=TRUE)))[namesimu1])
       if(all(is.na(dd1)))  dd1 [] <-0
       if(all(is.na(dd2)))  dd2 [] <-0
       # print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,paste('disc_rate_', sp, sep='')], na.rm=TRUE))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,paste('disc_rate_', sp, sep='')], na.rm=TRUE)))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,paste('disc_rate_', sp, sep='')], na.rm=TRUE)))[namesimu1] *100 )-100
       if(all(is.na(ratio_percent)))  ratio_percent [] <-0
           if(is.infinite(sum(ratio_percent,na.rm=TRUE))) ratio_percent[] <- 0
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      disc_rate_thissp[sce,"mean"] <- signif(a_mean,4)
     disc_rate_thissp[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable=paste('disc_rate_', sp, sep='')))
  }
 print(disc_rate_thissp)
 }


   # measure of equity, diversity or distributional profit
   # Shannon index is sum from i=1 to nbVessels of p_i*ln (p_i) with p_i is  the proportion of vessels belonging to the ith class of revenue in the dataset of interest.
   # nb of classes of revenue to decide upon?
   # but better to use the ginin index or 20:20 Ratio, ...
   #or the the Robin Hood index H (because bwteen 0 to 1 then can be useful for a ratio)
   # http://en.wikipedia.org/wiki/Income_inequality_metrics
  HooverIndex <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------------------------------------------\n"))
       lst_loglike_w_agg_vid_2 <- get(paste("lst_loglike_agg_",what2,"_vid_", sce, sep=''))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'gav']))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_vid_1, function (x) {
                              ei <- quantile(x[,'gradva'], prob=seq(0,1,length=21))
                              ei <- ei[ei!=0] # debug- possibly some revenue at 0 e.g. for early version of hpg_harbknowl_biolsce_Linfs08_M_mig_weight where displacement toward unrealistic fgrounds...
                              ai <- table(cut(x[,'gradva'], breaks=ei))
                              ei <- ei[-1]
                              hoover <- 0.5* sum(abs((ei/sum(ei))-(ai/sum(ai))))   #  where 0 indicates perfect equality and 1 (100%) indicates maximum inequality.
                              }
             ))[namesimu1])
      dd2 <- (unlist(lapply(lst_loglike_w_agg_vid_2, function (x) {
                              ei <- quantile(x[,'gradva'], prob=seq(0,1,length=21))
                              ei <- ei[ei!=0] # debug- possibly some revenue at 0 e.g. for early version of hpg_harbknowl_biolsce_Linfs08_M_mig_weight where displacement toward unrealistic fgrounds...
                              ai <- table(cut(x[,'gradva'], breaks=ei))
                              ei <- ei[-1]
                              hoover <- 0.5* sum(abs((ei/sum(ei))-(ai/sum(ai))))   #  where 0 indicates perfect equality and 1 (100%) indicates maximum inequality.

                              }
             ))[namesimu1])

       print(t.test(dd1,dd2))
       ratio_percent <- (dd2 /
                          dd1 *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<300]
       a_mean        <- mean(ratio_percent)
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      HooverIndex[sce,"mean"] <- signif(a_mean,4)
     HooverIndex[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="hoover"))
  }
  print(HooverIndex)





# export
write.table(outcomes,
                   file=file.path(general$main.path, general$namefolderinput,
                     paste("outcomes_all_simus_relative_to_baseline_sce_",selected, ".txt", sep='')),
              sep=";", quote=FALSE, row.names=FALSE)

} # end for-loop on sets


return()
}





