



mapNodeAverageLayerFiles <- function(general, type="cumcatches"){



    legend.gradient2 <-
function (pnts, cols = heat.colors(100), limits = c(0, 1), title = "Legend", legend="",
    ...)
{
    pnts = try(as.matrix(pnts), silent = T)
    if (!is.matrix(pnts))
        stop("you must have a 4x2 matrix")
    if (dim(pnts)[1] != 4 || dim(pnts)[2] != 2)
        stop("Matrix must have dimensions of 4 rows and 2 columms")
    if (length(cols) < 2)
        stop("You must have 2 or more colors")
    yvals = seq(min(pnts[, 2]), max(pnts[, 2]), length = length(cols) +
        1)
    for (i in 1:length(cols)) {
        polygon(x = pnts[, 1], y = c(yvals[i], yvals[i], yvals[i +
            1], yvals[i + 1]), col = cols[i], border = F)
    }
    text(max(pnts[, 1]), min(pnts[, 2]), labels = limits[1],
        pos = 4, ...)
    text(max(pnts[, 1]), max(pnts[, 2]), labels = limits[2],
        pos = 4, ...)
    start_pos <- (min(pnts[, 2])+((max(pnts[, 2])-min(pnts[, 2]))/length(legend))/2)
    for (i in 1: length(legend)){
    text(max(pnts[, 1])-0, start_pos + ((i-1) * ((max(pnts[, 2])-min(pnts[, 2]))/length(legend)) ), labels = legend[i],
        pos = 4, ...)
    #browser()
    }
    text(min(pnts[, 1])-0.1, max(pnts[, 2])-0, labels = title, adj = c(0,
        -1), ...)
}



   library(maptools)

   sh_coastlines               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub','DISPLACE_input_myfish','graphsspe', 'shp', 'francois_EU'), proj4string=CRS("+proj=longlat +ellps=WGS84"))
   ices_areas                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub','DISPLACE_input_raw','ices_areas','ices_areas'), proj4string=CRS("+proj=longlat +ellps=WGS84"))

   NSsub1mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                     'DISPLACE_SVANAProject', 'Input for DISPLACE', 'NST2_sub1_mx20_wgs84'), proj4string=CRS("+proj=longlat +ellps=WGS84"))
   BSsub1mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                     'DISPLACE_SVANAProject', 'Input for DISPLACE', 'BHT2_Sub1_Mx_20_wgs84'), proj4string=CRS("+proj=longlat +ellps=WGS84"))
   NSsub4mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                       'DISPLACE_SVANAProject', 'Input for DISPLACE', 'NST2_sub4_mx_20_wgs84'), proj4string=CRS("+proj=longlat +ellps=WGS84"))
   BSsub4mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                         'DISPLACE_SVANAProject', 'Input for DISPLACE', 'BHT2_Sub4_Mx_20LongTailedD_wgs84'), proj4string=CRS("+proj=longlat +ellps=WGS84"))
   NSsub4mx5                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                          'DISPLACE_SVANAProject', 'Input for DISPLACE', 'NST2_sub4_mx05_wgs84'), proj4string=CRS("+proj=longlat +ellps=WGS84"))
   BSsub4mx5                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                           'DISPLACE_SVANAProject', 'Input for DISPLACE', 'BHT2_Sub4_Mx_5LgtailedD_wgs84'), proj4string=CRS("+proj=longlat +ellps=WGS84"))



  selected_scenarios_for_table <-  c("svana_baseline", "svana_sub1mx20", "svana_sub4mx20", "svana_sub4mx5ns20bt", "svana_sub4mx20ns5bt", "svana_sub4mx5ns5bt")

  selected_scenarios_for_plot <-  c("svana_baseline", "svana_sub1mx20", "svana_sub4mx20", "svana_sub4mx5ns20bt", "svana_sub4mx20ns5bt", "svana_sub4mx5ns5bt")


  #a_width <- 2400 ; a _height <- 4500
  #xlims <-  c(8.7, 18)
  #ylims <- c(54,58.5)
  a_width <- 3400 ; a_height <- 3500
  xlims <-  c(-1, 17)
  ylims <- c(53,60)

   selected_areas_for_table <-  c("22",    "23",    "24",    "25",    "IIIa",  "IVa",   "IVb",   "IVc")
   table_cumcatches <- matrix(0, nrow=length(selected_scenarios_for_table), ncol=length(selected_areas_for_table)+1)
   rownames(table_cumcatches) <- c(selected_scenarios_for_table)
   colnames(table_cumcatches) <- c(selected_areas_for_table, "Other")



    namefile <- file.path(general$main.path, general$namefolderinput, paste("map_averaged_cumcatches_selected_revised.tiff") )
    namefile2 <- file.path(general$main.path, general$namefolderinput, paste("table_cumcatches_revised.txt") )


    tiff(filename=namefile,   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=300, compression = c("lzw"))
 #par(mfrow=c(5,1))

 if(length(selected_scenarios_for_plot)==7) m <- rbind(c(1, 1), c(1, 1),c(2, 3), c(4, 5),  c(6, 7))
 if(length(selected_scenarios_for_plot)==2) m <- rbind(c(1, 2))
 if(length(selected_scenarios_for_plot)==6) m <- rbind(c(1, 2) ,c(3, 4), c(5, 6))
 layout(m)
 par(mar=c(2,2,3,1))
 par(oma=c(4,4,1,1))
 table_catches <- NULL


   for(sce in   selected_scenarios_for_table)
     {


     if(sce=="svana_baseline")                {  names_sce <-  c("svana_baseline")  ;  biolsce <- "biolsce1" }
     if(sce=="svana_sub1mx20")                {   names_sce <-  c("svana_sub1mx20")  ;  biolsce <- "biolsce1" }
     if(sce=="svana_sub4mx20")                {   names_sce <-  c("svana_sub4mx20")  ;  biolsce <- "biolsce1" }
     if(sce=="svana_sub4mx5ns20bt")                {   names_sce <-  c("svana_sub4mx5ns20bt")  ;  biolsce <- "biolsce1" }
     if(sce=="svana_sub4mx20ns5bt")                {   names_sce <-  c("svana_sub4mx20ns5bt")  ;  biolsce <- "biolsce1" }
     if(sce=="svana_sub4mx5ns5bt")                {   names_sce <-  c("svana_sub4mx5ns5bt")  ;  biolsce <- "biolsce1" }



     this <- read.table(file=file.path(general$main.path, general$namefolderinput, names_sce,
                              paste("average_cumcatches_layer.txt", sep='')), header=FALSE, skip = 1)
     colnames(this) <- c("node","lat",  "long", "cumcatches")


    this_for_gis <- this
    this_for_gis[,4] <- ceiling(this_for_gis[,4]) # because weird bug when importing XY data in GIS if not an integer!!!
    write.table(this_for_gis, file=file.path(general$main.path, general$namefolderinput, names_sce,
                              paste("average_cumcatches_layer_",names_sce,".txt", sep='')), col.names=TRUE, row.names=FALSE)


     # get an idea per area
     library(vmstools)
     this$SI_LATI <- this$lat
     this$SI_LONG <- this$long
     #ices <-  readShapePoly(file.path("C:","BENTHIS","ICES_areas", "ices_areas" ),
     #                 proj4string=CRS("+proj=longlat +ellps=WGS84"))
     data(ICESareas)
     this$area <- ICESarea(this, ICESareas, fast=TRUE)
     this$area <- factor(this$area)
     levels(this$area)[! levels(this$area) %in% selected_areas_for_table] <- "Other"
     table_cumcatches[sce, ] <-  tapply(this$cumcatches, this$area, sum, na.rm=TRUE)[colnames(table_cumcatches)]





     this$round_long <- round(this$long*12)  # 15
     this$round_lat  <- round(this$lat*17)   # 20

     this$cumcatches  <- round(this$cumcatches)  /(5.576564*6.540486)  # if 15 and 20 then divide by cell area 8.925*5.561km  check??

     this$cell_id <-  paste(this$round_long, this$round_lat, sep="_")
     if(sce == "svana_baseline") {
        the_baseline_layer <- this
        the_baseline_layer <- aggregate(the_baseline_layer$cumcatches,
                                list(the_baseline_layer$round_long, the_baseline_layer$round_lat, the_baseline_layer$cell_id), sum, na.rm=TRUE)
        colnames(the_baseline_layer) <- c("round_long", "round_lat", "cell_id", "cumcatches")


       Satellite.Palette.baseline <-colorRampPalette(c("cyan","aquamarine","orange","red"))
       #the_breaks_baseline <-   c(0.5, 1, round(exp(seq(0.5, 5, by=0.5))), 10000)
       the_breaks_baseline <-   c(0.5, 1, round(exp(seq(0.5, 14, by=1.1))), 1000000)

       the_points <- tapply(the_baseline_layer$cumcatches,
                  list(the_baseline_layer$round_lat, the_baseline_layer$round_long), sum, na.rm=TRUE)

       the_points <- replace (the_points, the_points>the_breaks_baseline[length(the_breaks_baseline)], the_breaks_baseline[length(the_breaks_baseline)])

       image(
        x=as.numeric(as.character(colnames(the_points)))/12,     # 8.925km  at 53 degree N
        y=as.numeric(as.character(rownames(the_points)))/17,     # 5.561km at 53 degree N
        z= t(the_points),  # convert in tons
        breaks=c(the_breaks_baseline),
        col =  Satellite.Palette.baseline(length(the_breaks_baseline[-1]))  ,
        useRaster=FALSE,
        xlab="",
        ylab="",
        axes=FALSE,
        #xlim=c(-2, 20 ), ylim=c(53,61)   # canadian
        #xlim=c(8.7, 18), ylim=c(54,56.5)    # balticonly
        xlim=xlims, ylim=ylims    # balticonly
        )
       #plot(sh1, add=TRUE, col=grey(0.7))
       library(maps)
       #map(add=TRUE, fill=TRUE,col=grey(0.7), border=grey(0.7))
       plot(sh_coastlines, add=TRUE, col=grey(0.8), border=FALSE)
       plot(ices_areas, add=TRUE,  border=grey(0.8))
       text(coordinates(ices_areas), labels=ices_areas$ICES_area, cex=1.4, col="black")

       #plot(ices, add =TRUE, border=grey(0.3))
       #text(ices, labels=ices$ICES_area, cex=0.8, col="black")

       box()
       mtext(side=3, sce, cex=1.2, line=0.5)
       axis(1, cex.axis=1.2)
       axis(2, las=2, cex.axis=1.2)

      #mtext(side=1, "Longitude", cex=1, adj=0.5, line=2)
      #mtext(side=2, "Latitude", cex=1, adj=0.5, line=3)

      #mtext(side=3, "(a)", cex=1.5, adj=0, line=1)

    x = c(xlims[1]+0.2, xlims[1]+0.4, xlims[1]+0.4, xlims[1]+0.2)
    y = c(ylims[1]+0.5, ylims[1]+4, ylims[1]+4, ylims[1]+0.5)
    the_breaks_leg <-NULL
      for(i in 1: length(the_breaks_baseline[-1])){ if(the_breaks_baseline[i]>1) {the_breaks_leg[i] <- round(the_breaks_baseline[i])} else{the_breaks_leg[i]<- the_breaks_baseline[i]}}
       legend.gradient2 (cbind(x = x , y = y ), cols=Satellite.Palette.baseline(length(the_breaks_baseline[-1])),
         limits="", title=expression(paste("Total catches kg per ", km^2)),
         legend= the_breaks_leg,
         cex=1, col="black")


        }   else{



      this <- aggregate(this$cumcatches, list(this$round_long, this$round_lat, this$cell_id), sum, na.rm=TRUE)
      colnames(this) <- c("round_long", "round_lat", "cell_id", "cumcatches")

      # Merge!
      this           <- merge(the_baseline_layer, this, by.x="cell_id", by.y="cell_id")
      this$cumcatches  <- (100* as.numeric(as.character(this$cumcatches.y)) / as.numeric(as.character(this$cumcatches.x)) )  -100

          # CAUTION!!!!: correct for area with low absolute value to avoid visual effect
      this$cumcatches [this$cumcatches.x <quantile(this$cumcatches.x[this$cumcatches.x!=0], prob=0.05)]  <- 0




   #  background <- expand.grid(node=0,
   #                           lat=0,
   #                           long=0,
   #                           round_long=unique(this$round_long),
   #                           round_lat=c(100,unique(this$round_lat)),
   #                           cumftime=0
   #                           )
   # this <- rbind(this[, c('node', 'lat', 'long', 'round_long', 'round_lat', 'cumftime')], background)

   #  background <- expand.grid(cell_id=0,
   #                           round_long.x=0,
   #                           round_lat.x=0,
   #                           round_long.y=unique(this$round_long.y),
   #                           round_lat.y=c(100,unique(this$round_lat.y)),
   #                           cumftime.y=0,
   #                           cumftime=0
   #                           )
   # this <- rbind(this[, c('cell_id', 'round_long.x', 'round_lat.x', 'round_long.y', 'round_lat.y', 'cumftime.y', 'cumftime')], background)

    in_relative <- TRUE
    if(in_relative){
        the_points <- tapply(this$cumcatches,
                  list(this$round_lat.y, this$round_long.y), sum)
        Satellite.Palette <-colorRampPalette(c("cyan","aquamarine","white","yellow","red"))
        # the_breaks <-  c(0, round(exp(seq(0, 6, by=0.5))))
        the_breaks <-  c(rev(-round(exp(seq(0, 7, by=1)))),  0, round(exp(seq(0, 7, by=1))))
        } else{
        the_points <- tapply(this$cumcatches.y,
                  list(this$round_lat.y, this$round_long.y), sum)
        the_breaks <-  the_breaks_baseline
        Satellite.Palette <- Satellite.Palette.baseline

        }


    the_points <- replace (the_points, the_points>the_breaks[length(the_breaks)], the_breaks[length(the_breaks)])


     # in ?
     sum(as.numeric(as.character(the_points)), na.rm=TRUE)



   # namefile <- file.path(general$main.path, general$namefolderinput, sce, paste("map_averaged_swept_area.tiff") )
   # tiff(filename=namefile,   width = 2000, height = 2000,
   #                                units = "px", pointsize = 12,  res=300, compression = c("lzw"))
   # par(mar=c(5,5,5,1))


  if(sce %in% selected_scenarios_for_plot){
    image(
     x=as.numeric(as.character(colnames(the_points)))/12,   #15
     y=as.numeric(as.character(rownames(the_points)))/17,   # 20
     z= t(the_points),  # convert in tons
     breaks=c(the_breaks),
     col = Satellite.Palette(length(the_breaks[-1])),
     useRaster=FALSE,
     xlab="",
     ylab="",
     axes=FALSE,
     #xlim=c(-2, 20 ), ylim=c(53,61)   # canadian
     #xlim=c(8.7, 18), ylim=c(54,56.5)    # balticonly
     xlim=xlims, ylim=ylims    # balticonly
     )
    #plot(sh1, add=TRUE, col=grey(0.7))
    library(maps)
    #map(add=TRUE, fill=TRUE,col=grey(0.7), border=grey(0.7))
    plot(sh_coastlines, add=TRUE, col=grey(0.8), border=FALSE)
    plot(ices_areas, add=TRUE,  border=grey(0.8))
    text(coordinates(ices_areas), labels=ices_areas$ICES_area, cex=1.4, col="black")

    #plot(ices, add =TRUE, border=grey(0.3))
    #text(ices, labels=ices$ICES_area, cex=0.8, col="black")

      box()
    mtext(side=3, sce, cex=1.2, line=0.5)
    axis(1, cex.axis=1.2)
    axis(2, las=2, cex.axis=1.2)

    #mtext(side=1, "Longitude", cex=1, adj=0.5, line=2)
    #mtext(side=2, "Latitude", cex=1, adj=0.5, line=3)

    #mtext(side=3, "(a)", cex=1.5, adj=0, line=1)

    x = c(xlims[1]+0.2, xlims[1]+0.4, xlims[1]+0.4, xlims[1]+0.2)
    y = c(ylims[1]+0.5, ylims[1]+4, ylims[1]+4, ylims[1]+0.5)
    the_breaks_leg <-NULL
    for(i in 1: length(the_breaks[-1])){ if(the_breaks[i]>1) {the_breaks_leg[i] <- round(the_breaks[i])} else{the_breaks_leg[i]<- the_breaks[i]}}
    legend.gradient2 (cbind(x = x , y = y ), cols=Satellite.Palette(length(the_breaks[-1])),
    limits="", title=expression(paste("% difference per cell")),
    legend= the_breaks_leg,
     cex=1.0, col="black")

    # closure polygons:
    if(sce=="svana_sub1mx20"){
        plot(NSsub1mx20, add=TRUE,  border=grey(0.2), col=NA)
        plot(BSsub1mx20, add=TRUE,  border=grey(0.2), col=NA)
       }
    if(sce=="svana_sub4mx20"){
        plot(NSsub4mx20, add=TRUE,  border=grey(0.2), col=NA)
        plot(BSsub4mx20, add=TRUE,  border=grey(0.2), col=NA)
       }
    if(sce=="svana_sub4mx5ns20bt"){
        plot(NSsub4mx5, add=TRUE,  border=grey(0.2), col=NA)
        plot(BSsub4mx20, add=TRUE,  border=grey(0.2), col=NA)
       }
   if(sce=="svana_sub4mx20ns5bt"){
        plot(NSsub4mx20, add=TRUE,  border=grey(0.2), col=NA)
        plot(BSsub4mx5, add=TRUE,  border=grey(0.2), col=NA)
       }
   if(sce=="svana_sub4mx5ns5bt"){
        plot(NSsub4mx5, add=TRUE,  border=grey(0.2), col=NA)
        plot(BSsub4mx5, add=TRUE,  border=grey(0.2), col=NA)
       }




     } # end selected sce for plot
#dev.off()
    } # end  Baseline
 } # end sce

  mtext("Latitude", 1, line=2, cex=1.5, outer=TRUE)
  mtext(side=2,"Longitude",line=2, cex=1.5, outer=TRUE)

dev.off()

table_cumcatches <- cbind(table_cumcatches, Total= apply(table_cumcatches, 1, sum, na.rm=TRUE) ) # marginal value

table_cumcatches_relative_to_baseline <- cbind(round(sweep(table_cumcatches, 2, table_cumcatches[1,], FUN="/")*100, 1)- 100)
table_cumcatches_relative_to_baseline[1,] <- table_cumcatches[1,]
write.table(table_cumcatches_relative_to_baseline,   file=namefile2, col.names=TRUE, row.names=TRUE, sep=";", quote=FALSE)
print(namefile2)

# useful to copy/paste into Excel!
write.table(table_cumcatches_relative_to_baseline, "clipboard", sep="\t", row.names=TRUE)   # export to excel

# check in absolute numbers:
 sum(table_cumcatches_relative_to_baseline["svana_sub1mx20",]/100*table_cumcatches_relative_to_baseline["svana_baseline",])



 return()
}








