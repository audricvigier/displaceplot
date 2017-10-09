



getAggNodeLayerFiles <- function(general, type="cumcatches"){


  for (sce in general$namefolderoutput){



 

 
    ## catches-------------------------------
    allcumcatches <- NULL


     for (sim in general$namesimu[[sce]]){

         er <- try(   {
            cumcatches <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                                                paste("popnodes_cumcatches_", sim, ".dat", sep='')))
            colnames (cumcatches) <- c('tstep', 'idx_node', 'long', 'lat', 'catches')
            cumcatches    <- cumcatches[cumcatches$tstep=="34321",] # e.g. cumul at 1st of Dec 4th year
            cumcatches    <- cumcatches[c("idx_node",'lat','long',"catches")]
            cumcatches    <- cbind(cumcatches, simu=sim)
            allcumcatches <- rbind(allcumcatches, cumcatches)
         }, silent=TRUE)

        if(class(er)=="try-error"){
           print(paste("no simu", sim))

        }

       }
  
     # CAUTION:
     # read graph coord and complete DISPLACE output files with all coords for image() to work properly
     coord <- read.table(file=file.path(paste(general$main.path.ibm,"_", general$case_study, sep=""), "graphsspe", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
     coord <- as.matrix(as.vector(coord))
     coord <- matrix(coord, ncol=3)
     colnames(coord) <- c('SI_LONG', 'SI_LATI', 'idx.port')
     # hereafter:
     allcumcatches <- rbind(allcumcatches, data.frame(idx_node=0, lat=coord[,2], long=coord[,1], catches=NA, simu="simu2")) 
     
     
  

    cumcatches_tot <- tapply(as.numeric(as.character(allcumcatches$catches)), list(paste(allcumcatches$idx_node, allcumcatches$lat, allcumcatches$long))
                                                              , mean, na.rm=TRUE) # average over simus
    cumcatches_tot <- cbind.data.frame(node=names(cumcatches_tot), avcumcatches=cumcatches_tot)

    write.table(cumcatches_tot, file=file.path(general$main.path, general$namefolderinput, sce,
                              paste("average_cumcatches_layer.txt", sep='')), row.names=FALSE, quote=FALSE)

   
 
    }


return()
}




