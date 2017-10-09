

            
#' Displace plot 
#'
#' This function generates an average layer as a first step to generate maps from popnodes files 
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' getAggNodeLayerFiles(general=general, type="cumcatches", a_tstep="34321")  






getAggNodeLayerFiles <- function(general, a_type="cumcatches", a_tstep="34321"){


  for (sce in general$namefolderoutput){



 

 
    ## catches-------------------------------
    alllayers <- NULL


     for (sim in general$namesimu[[sce]]){

         er <- try(   {
            obj <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                                                paste("popnodes_",a_type,"_", sim, ".dat", sep='')))
            colnames (obj) <- c('tstep', 'idx_node', 'long', 'lat', a_type)
            obj    <- obj[obj$tstep==a_tstep,] # e.g. if "34321" then cumul at 1st of Dec 4th year
            obj    <- obj[c("idx_node",'lat','long', a_type)]
            obj    <- cbind(obj, simu=sim)
            alllayers <- rbind(alllayers, obj)
         }, silent=TRUE)

        if(class(er)=="try-error"){
           print(paste("no simu", sim))

        }

       }
  
     # CAUTION:
     # read graph coord and complete DISPLACE output files with all coords for image() to work properly
     coord <- read.table(file=file.path(paste(general$main.path.ibm,"_", general$case_study, sep=""),
                          "graphsspe", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
     coord <- as.matrix(as.vector(coord))
     coord <- matrix(coord, ncol=3)
     colnames(coord) <- c('SI_LONG', 'SI_LATI', 'idx.port')
     # hereafter:
     alllayers <- rbind(alllayers, data.frame(idx_node=0, lat=coord[,2], long=coord[,1], a_type=NA, simu="simu2")) 
     colnames(alllayers) [colnames(alllayers)%in%  "a_type"] <- a_type
     
  

    alllayersav <- tapply(as.numeric(as.character(alllayers[,a_type])), list(paste(alllayers$idx_node, alllayers$lat, alllayers$long))
                                                              , mean, na.rm=TRUE) # average over simus
    alllayersav <- cbind.data.frame(node=names(alllayersav), avcum=alllayersav)

    write.table(alllayersav, file=file.path(general$main.path, general$namefolderinput, sce,
                              paste("average_",a_type,"_layer.txt", sep='')), row.names=FALSE, quote=FALSE)

   
 
    }


return()
}




