

            
#' Produce an average spatial layer as first step to do a map 
#'
#' This function generates an average layer as a first step to generate maps from popnodes files 
#' Two different types of files are going to be written behind the scene i.e. one file storing the stack of all input files and keeping the sce dimension, the other doing the averaging per sce.
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
#' getAggNodeBenthosLayerFiles (general,  a_tstep="34321")
#'  #=> produce files in output folders....
#'  }








getAggNodeBenthosLayerFiles <- function(general,  a_tstep="34321"){


 for (sce in general$namefolderoutput){



 


    ## -------------------------------
    allcumbenthos <- NULL


     for (sim in general$namesimu[[sce]]){

         er <- try(   {
            cumbenthos <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                                                paste("benthosnodes_tot_biomasses_", sim, ".dat", sep='')))
            colnames (cumbenthos) <- c('funcgr','tstep', 'idx_node', 'long', 'lat', 'totN', 'totbio', 'meanweight', 
                                      "benthosbiomassoverK", "benthosnumberoverK", "benthosbiomassK")
            # fields in DISPLACE output file are: Func gr id/ tstep / node / long / lat / number this func group id /biomass this func group id/ 
            # mean weight this func group id / benthosbiomassoverK / benthosnumberoverK /benthos_tot_biomass_K this funcgr 

            cumbenthos    <- cumbenthos[c("idx_node",'lat','long','funcgr','tstep', "totbio", "totN", "meanweight", "benthosbiomassK")]
            cumbenthos    <- cbind(cumbenthos, simu=sim, sce=sce)
            allcumbenthos <- rbind(allcumbenthos, cumbenthos)
         }, silent=TRUE)

        if(class(er)=="try-error"){
           print(paste("no simu", sim))

        }

       }
     
     
     # 1. ts
     cumbenthos_totN_ts    <- tapply(as.numeric(as.character(allcumbenthos$totN)), 
                              list(paste(allcumbenthos$tstep, allcumbenthos$funcgr, allcumbenthos$sim, allcumbenthos$sce)), sum, na.rm=TRUE) # sum over nodes
     cumbenthos_totbio_ts     <- tapply(as.numeric(as.character(allcumbenthos$totbio)), 
                              list(paste(allcumbenthos$tstep, allcumbenthos$funcgr, allcumbenthos$sim, allcumbenthos$sce)), sum, na.rm=TRUE) # sum over nodes
     cumbenthos_meanweight_ts <- tapply(as.numeric(as.character(allcumbenthos$meanweight)),
                              list(paste(allcumbenthos$tstep, allcumbenthos$funcgr, allcumbenthos$sim, allcumbenthos$sce)), mean, na.rm=TRUE) # mean over nodes
     cumbenthos_biomassK_ts <- tapply(as.numeric(as.character(allcumbenthos$benthosbiomassK)),
                              list(paste(allcumbenthos$tstep, allcumbenthos$funcgr, allcumbenthos$sim, allcumbenthos$sce)), sum, na.rm=TRUE) # sum over nodes
     cumbenthos_tot_ts <- cbind.data.frame(tstep=names(cumbenthos_totN_ts),
                                       avcumbenthosN=cumbenthos_totN_ts, 
                                       avcumbenthosbio=cumbenthos_totbio_ts,
                                       avcumbenthosmeanweight=cumbenthos_meanweight_ts,
                                       avcumbenthosbiomassK=cumbenthos_biomassK_ts)
     colnames(cumbenthos_tot_ts) <- c("tstep funcgr sim sce", "totN", "tobio", "meanweight", "biomassK")
    
     write.table(cumbenthos_tot_ts, file=file.path(general$main.path, general$namefolderinput,  sce,
                              paste("allcumbenthos_ts.txt", sep='')), row.names=FALSE, quote=FALSE)

     
     # 2. spatial layer
     allcumbenthos    <- allcumbenthos[allcumbenthos$tstep=="34321",] # e.g. if "34321" then cumul at 1st of Dec 4th year
     allcumbenthos    <- allcumbenthos[, !colnames(allcumbenthos) %in% "tstep"]
     
     # CAUTION:
     # read graph coord and complete DISPLACE output files with all coords for image() to work properly
     coord <- read.table(file=file.path(paste(general$main.path.ibm,"_", general$case_study, sep=""), 
               "graphsspe", paste("coord", general$igraph, ".dat", sep=""))) # built in the c++ gui
     coord <- as.matrix(as.vector(coord))
     coord <- matrix(coord, ncol=3)
     colnames(coord) <- c('SI_LONG', 'SI_LATI', 'idx.port')
     # hereafter:
     allcumbenthos <- rbind(allcumbenthos, data.frame(idx_node=0, lat=coord[,2], long=coord[,1],  funcgr=NA, totbio=NA, totN=NA, meanweight=NA, benthosbiomassK=NA, simu="simu2", sce="fake")) 



    cumbenthos_totN <- tapply(as.numeric(as.character(allcumbenthos$totN)), 
                              list(paste(allcumbenthos$idx_node, allcumbenthos$lat, allcumbenthos$long)), mean, na.rm=TRUE) # average over simus
    cumbenthos_totbio <- tapply(as.numeric(as.character(allcumbenthos$totbio)), 
                              list(paste(allcumbenthos$idx_node, allcumbenthos$lat, allcumbenthos$long)), mean, na.rm=TRUE) # average over simus
    cumbenthos_meanweight <- tapply(as.numeric(as.character(allcumbenthos$meanweight)),
                              list(paste(allcumbenthos$idx_node, allcumbenthos$lat, allcumbenthos$long)), mean, na.rm=TRUE) # average over simus
   
     
    cumbenthos_tot <- cbind.data.frame(node=names(cumbenthos_totN),
                                       avcumbenthosN=cumbenthos_totN, 
                                       avcumbenthosbio=cumbenthos_totbio,
                                       avcumbenthosmeanweight=cumbenthos_meanweight)

    write.table(cumbenthos_tot, file=file.path(general$main.path, general$namefolderinput, sce,
                              paste("average_cumbenthos_layer.txt", sep='')), row.names=FALSE, quote=FALSE)

   
 
    }
     
  

return()
}


