

            
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
#' getAggNodeBenthosLayerFiles (general,  a_tstep="34321")
#'  #=> produce files in output folders....
#'  }








getAggNodeBenthosLayerFiles <- function(general,  a_tstep="34321"){


  for (sce in general$namefolderoutput){



 


    ## catches-------------------------------
    allcumbenthos <- NULL


     for (sim in general$namesimu[[sce]]){

         er <- try(   {
            cumbenthos <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                                                paste("benthosnodes_tot_biomasses_", sim, ".dat", sep='')))
            colnames (cumbenthos) <- c('funcgr','tstep', 'idx_node', 'long', 'lat', 'totN', 'totbio', 'meanweight', 
                                      "benthosbiomassoverK", "benthosnumberoverK", "benthosbiomassK")
            # fields in DISPLACE output file are: Func gr id/ tstep / node / long / lat / number this func group id /biomass this func group id/ 
            # mean weight this func group id / benthosbiomassoverK / benthosnumberoverK /benthos_tot_biomass_K this funcgr 

            cumbenthos    <- cumbenthos[c("idx_node",'lat','long','funcgr','tstep', "totbio", "totN", "meanweight")]
            cumbenthos    <- cbind(cumbenthos, simu=sim, sce=sce)
            allcumbenthos <- rbind(allcumbenthos, cumbenthos)
         }, silent=TRUE)

        if(class(er)=="try-error"){
           print(paste("no simu", sim))

        }

       }
     write.table(allcumbenthos, file=file.path(general$main.path, general$namefolderinput,
                              paste("allcumbenthos_layers.txt", sep='')), row.names=FALSE, quote=FALSE)

     
     allcumbenthos    <- allcumbenthos[allcumbenthos$tstep==a_tstep,] # e.g. if "34321" then cumul at 1st of Dec 4th year
     allcumbenthos    <- allcumbenthos[, !colnames(allcumbenthos) %in%  "tstep"]
     
     
     # CAUTION:
     # read graph coord and complete DISPLACE output files with all coords for image() to work properly
     coord <- read.table(file=file.path(paste(general$main.path.ibm,"_", general$case_study, sep=""), 
               "graphsspe", paste("coord", general$igraph, ".dat", sep=""))) # built in the c++ gui
     coord <- as.matrix(as.vector(coord))
     coord <- matrix(coord, ncol=3)
     colnames(coord) <- c('SI_LONG', 'SI_LATI', 'idx.port')
     # hereafter:
     allcumbenthos <- rbind(allcumbenthos, data.frame(idx_node=0, lat=coord[,2], long=coord[,1],  funcgr=NA, totbio=NA, totN=NA, meanweight=NA, simu="simu2", sce="fake")) 



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


