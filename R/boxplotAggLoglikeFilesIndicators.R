

#' Make a boxplot for fisheries indicators from previous aggregations
#'
#' This function makes a box-and-whiskers plot out of aggregation on indicator variables
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' 
#' \dontrun{
#' general <- setGeneralOverallVariable (pathToRawInputs =file.path("C:", "Users", "fbas", 
#'                                                 "Documents", "GitHub", paste0("DISPLACE_input_gis_", 
#'                                                  "DanishFleet")),
#'                                       pathToDisplaceInputs = file.path("C:", "Users", "fbas", 
#'                                                 "Documents", "GitHub", paste0("DISPLACE_input_", "DanishFleet")),
#'                                       pathToOutputs =file.path("C:","DISPLACE_outputs"),
#'                                       caseStudy="DanishFleet",
#'                                       iGraph=41,
#'                                       iYear="2015",
#'                                       iCountry="DEN",
#'                                       nbPops=39,
#'                                       nbSzgroup=14,
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
#'   selected_vessels_set_1 <- as.character(read.table(file.path(general$main.path, general$case_study,
#'           paste("selected_vessels_set_1.dat",sep='')), header=FALSE)[,1])
#'   selected_vessels_set_2 <-as.character(read.table(file.path(general$main.path, general$case_study,
#'           paste("selected_vessels_set_2.dat",sep='')), header=FALSE)[,1])
#'    selected_vessels_set_3 <- as.character(read.table(file.path(general$main.path, general$case_study,
#'           paste("selected_vessels_set_3.dat",sep='')), header=FALSE)[,1])
#'
#'   if(FALSE){
#'     getAggLoglikeFiles(general=general, what="weight",
#'             explicit_pops=c(0, 1, 2, 3, 11, 23, 24, 26, 30, 31, 32),
#'             implicit_pops=c (4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 25, 27, 28, 29, 33, 34, 35, 36, 37, 38),
#'             selected_vessels_set1=selected_vessels_set_1,
#'             selected_vessels_set2=selected_vessels_set_2,
#'             selected_vessels_set3=selected_vessels_set_3)
#'    } else{
#'     loadLoglikeFiles (general, use_port_info=FALSE)
#'    }
#'
#'
#'   expressAggLoglikeFilesIndicatorsRelativeToBaselineSce(general,
#'                              the_baseline = "svana_baseline",
#'                              sets=c("_selected_set1_", "_selected_set2_", "_selected_set3_"))
#'                   # => generate a file in output folders
#'
#'
#'   boxplotAggLoglikeFilesIndicators (general= general,
#'                                             the_baseline="svana_baseline",
#'                                             selected_scenarios =  general$namefolderoutput,
#'                                             sets=c("_selected_set1_", "_selected_set2_", "_selected_set3_"))
#'                  # => do the plot
#'   }




boxplotAggLoglikeFilesIndicators <- function(general= general,
                                             the_baseline="svana_baseline",
                                             selected_scenarios = general$namefolderoutput,
                                             sets=c("_selected_set1_", "_selected_set2_", "_selected_set3_")){


aggmean <- list() ; aggmedian <- list()
for (selected in sets){

 outcomes <- read.table(file.path(general$main.path, general$namefolderinput,
                    paste("outcomes_all_simus_relative_to_baseline_sce_",selected, ".txt", sep='')), header=TRUE, sep=";")

 ## CAUTION: (not the same levels when reading or when using directly the obj in the env)
 #levels (outcomes$scenario) <-  c("svana_sub1mx20",      "svana_sub4mx20",      "svana_sub4mx20ns5bt", "svana_sub4mx5ns20bt", "svana_sub4mx5ns5bt")

 # add baseline at 0,0,0, etc.
 baseline <- outcomes[outcomes$scenario == levels(outcomes$scenario)[1],]  # init
 baseline$ratio_percent <-0
 baseline$scenario <- the_baseline
 outcomes          <- rbind.data.frame(baseline, outcomes)
 outcomes$scenario <- factor(outcomes$scenario)

 # select because too much info
 select_some <- TRUE
 if(select_some){
 selected_variables <- c("feffort", "seffort", "nbtrip", "av_trip_duration", "fishing_based_cpue_explicit",
                                       "totland_explicit", "totland",  "sweptarea", "revenuepersweptarea", "npv", "av_vpuf_month", "hoover")
 outcomes           <- outcomes[outcomes$variable %in% selected_variables,]

 outcomes$variable <- factor(outcomes$variable)
 outcomes$variable <- factor(outcomes$variable, levels=selected_variables, labels= c( "F. effort", "S. effort", "Nb. of trips", "Trip duration",  "CPUE at fishing",
                                                                                       "Tot land. Assess. Stocks", "Tot land. OTHER",
                                                                                        "Swept Area", "Revenue Per Swept Area",
                                                                                        "NPV", "VPUF", "Income inequality"))



 outcomes <- outcomes[outcomes$scenario %in%selected_scenarios,]
 outcomes$scenario <- factor(outcomes$scenario)
 outcomes$scenario <- factor(outcomes$scenario, levels=selected_scenarios, labels=  selected_scenarios
                                )

 library(lattice)
 bwplot(ratio_percent~variable| scenario, data=outcomes)

 # a better plot
 namefile       <- paste(paste("indicators_boxplot_persce_",selected, sep=""))
 output.folder  <- file.path(general$main.path, general$namefolderinput)
 the_dim        <- c(2400, 1700)


 tiff(filename=file.path(output.folder, paste(namefile, ".tiff", sep="" )),
                                   width = the_dim[1], height = the_dim[2],
                                   units = "px", pointsize = 12,  res=300, compression=c("lzw"))

 library(ggplot2)
 outcomes[outcomes$ratio_percent< -25, "ratio_percent"]  <- -25
 outcomes[outcomes$ratio_percent>25, "ratio_percent"]    <- 25
 p <- ggplot(outcomes[outcomes$ratio_percent>=-25 & outcomes$ratio_percent<=25,], aes(factor(variable), ratio_percent))  + geom_boxplot(outlier.shape=1)  +
             labs(x = "Indicators", y = "% ratio over the baseline") # + ylim(-20, 20)
 print(
       p   + facet_wrap( ~ scenario, ncol=2, scales="free_y")    + theme_bw()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text.x =element_text(size =10),  panel.grid.major = element_line(colour = grey(0.4),linetype =3 ),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) +
        geom_abline(intercept=0, slope=0, color="grey", lty=2)  + geom_boxplot(outlier.shape=NA)
       )

 dev.off()

 } else{
 #keep all
 namefile       <- paste(paste("indicators_ALL_boxplot_persce_", sep=""))
 output.folder  <- file.path(general$main.path, general$namefolderinput)
 the_dim        <- c(3100, 1800)


 tiff(filename=file.path(output.folder, paste(namefile, ".tiff", sep="" )),
                                   width = the_dim[1], height = the_dim[2],
                                   units = "px", pointsize = 12,  res=300, compression="lzw")

 library(ggplot2)
 p <- ggplot(outcomes, aes(factor(variable), ratio_percent))   + geom_boxplot(outlier.shape=NA)  +
 p   + facet_wrap( ~ scenario, ncol=2, scales="free_y")   +
       coord_cartesian(ylim = range(boxplot(outcomes$ratio_percent, plot=FALSE)$stats)*c(.9, 1.1)) +
        theme(axis.text.x = element_text(angle = 90)) + geom_abline(intercept=0, slope=0, color="grey", lty=2)

 dev.off()
 }

  aggmean[[selected]] <- tapply(outcomes$ratio_percent, list(outcomes$scenario, outcomes$variable), mean)
  aggmedian[[selected]] <- tapply(outcomes$ratio_percent, list(outcomes$scenario, outcomes$variable), median)
 
 
 } # end FOR-loop over sets

 
 # useful to copy/paste into Excel!
 write.table(outcomes, "clipboard", sep="\t", row.names=TRUE)   # export to excel

 return(list(outcomes=outcomes,  aggmean=aggmean,  aggmedian=aggmedian, message="Look also for graphics in the DISPLACE output folders"))
 }



