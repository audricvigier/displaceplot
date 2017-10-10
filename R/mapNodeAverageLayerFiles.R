


#' Generate maps from averaging stochastic DISPLACE spatial layera
#'
#' This function generates maps from an average layer as a second step after call to getAggNodesLayerFiles()
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
#'   # caution: could take a  while...
#'   getAggNodeLayerFiles (general, a_type="cumcatches", a_tstep="34321")
#'
#'
#'   library(maptools)
#'   sh_coastlines               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub','DISPLACE_input_myfish','graphsspe', 'shp', 'francois_EU'), 
#'                                                       proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   ices_areas                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub','DISPLACE_input_raw','ices_areas','ices_areas'), 
#'                                                      proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'
#'   NSsub1mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                     'DISPLACE_SVANAProject', 'Input for DISPLACE', 'NST2_sub1_mx20_wgs84'),
#'                                                      proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   BSsub1mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                     'DISPLACE_SVANAProject', 'Input for DISPLACE', 'BHT2_Sub1_Mx_20_wgs84'), 
#'                                                        proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   NSsub4mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                       'DISPLACE_SVANAProject', 'Input for DISPLACE', 'NST2_sub4_mx_20_wgs84'), 
#'                                                        proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   BSsub4mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                         'DISPLACE_SVANAProject', 'Input for DISPLACE', 'BHT2_Sub4_Mx_20LongTailedD_wgs84'), 
#'                                                           proj4string=CRS("+proj=longlat #'+ellps=WGS84"))
#'   NSsub4mx5                   <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                          'DISPLACE_SVANAProject', 'Input for DISPLACE', 'NST2_sub4_mx05_wgs84'), 
#'                                                            proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   BSsub4mx5                   <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                           'DISPLACE_SVANAProject', 'Input for DISPLACE', 'BHT2_Sub4_Mx_5LgtailedD_wgs84'), 
#'                                                              proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'
#'
#'
#'   mapNodeAverageLayerFiles (general, a_type="cumcatches",   the_baseline= "svana_baseline",
#'                            selected_scenarios_for_plot=general$namefolderoutput,
#'                            selected_scenarios_for_table=general$namefolderoutput,
#'                            selected_areas_for_table=c("22",    "23",    "24",    "25",    "IIIa",  "IVa",   "IVb",   "IVc"),
#'                            the_breaks_baseline= c(0.5, 1, round(exp(seq(0.5, 14, by=1.1))), 1000000),
#'                            gis_shape=list(svana_baseline=   list(sh_coastlines), # ices_areas),
#'                                           svana_sub1mx20=   list(NSsub1mx20, BSsub1mx20),
#'                                           svana_sub4mx20=   list(NSsub4mx20, BSsub4mx20),
#'                                           svana_sub4mx5ns20bt=   list(NSsub4mx5, BSsub4mx20),
#'                                           svana_sub4mx20ns5bt=   list(NSsub4mx20, BSsub4mx5),
#'                                           svana_sub4mx5ns5bt=    list(NSsub4mx5, BSsub4mx5),
#'                                           a_width= 3400, a_height =3500, xlims =  c(-1, 17), ylims = c(53,60), xcell=12, ycell=17
#'                                           ))
#' }







mapNodeAverageLayerFiles <- function(general, a_type="cumcatches",  the_baseline= "svana_baseline",
                            selected_scenarios_for_plot=general$namefolderoutput,
                            selected_scenarios_for_table=general$namefolderoutput,
                            selected_areas_for_table=c("22",    "23",    "24",    "25",    "IIIa",  "IVa",   "IVb",   "IVc"),
                            the_breaks_baseline= c(0.5, 1, round(exp(seq(0.5, 14, by=1.2))), 1000000),
                            gis_shape=list(),
                            a_width= 3400, a_height =3500, xlims =  c(-1, 17), ylims = c(53,60), xcell=12, ycell=17
                            ){


 distance <- function (lon, lat, lonRef, latRef)  # vmstools::distance()
{
    pd <- pi/180
    a1 <- sin(((latRef - lat) * pd)/2)
    a2 <- cos(lat * pd)
    a3 <- cos(latRef * pd)
    a4 <- sin(((lonRef - lon) * pd)/2)
    a <- a1 * a1 + a2 * a3 * a4 * a4
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    return(6371 * c)
}

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



   table_obj <- matrix(0, nrow=length(selected_scenarios_for_table), ncol=length(selected_areas_for_table)+1)
   rownames(table_obj) <- c(selected_scenarios_for_table)
   colnames(table_obj) <- c(selected_areas_for_table, "Other")



    namefile  <- file.path(general$main.path, general$namefolderinput, paste0("map_averaged_",a_type,"_selected.tiff") )
    namefile2 <- file.path(general$main.path, general$namefolderinput, paste0("table_",a_type,".txt") )


    tiff(filename=namefile,   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=300, compression = c("lzw"))

    if(length(selected_scenarios_for_plot)==3) m <- rbind(c(1, 1), c(1, 1),c(2, 3))
    if(length(selected_scenarios_for_plot)==5) m <- rbind(c(1, 1), c(1, 1),c(2, 3), c(4, 5))
    if(length(selected_scenarios_for_plot)==7) m <- rbind(c(1, 1), c(1, 1),c(2, 3), c(4, 5),  c(6, 7))
    if(length(selected_scenarios_for_plot)==2) m <- rbind(c(1, 2))
    if(length(selected_scenarios_for_plot)==4) m <- rbind(c(1, 2), c(2,3))
    if(length(selected_scenarios_for_plot)==6) m <- rbind(c(1, 2) ,c(3, 4), c(5, 6))
    layout(m)
    par(mar=c(2,2,3,1))
    par(oma=c(4,4,1,1))
    #table_obj <- NULL


   for(sce in   selected_scenarios_for_table)
     {


    this <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                              paste("average_",a_type,"_layer.txt", sep='')), header=FALSE, skip = 1)
     colnames(this) <- c("node","lat",  "long", a_type)


    this_for_gis <- this
    this_for_gis[,4] <- ceiling(this_for_gis[,4]) # because weird bug when importing XY data in GIS if not an integer!!!
    write.table(this_for_gis, file=file.path(general$main.path, general$namefolderinput, sce,
                              paste("average_",a_type,"_layer_",sce,".txt", sep='')), col.names=TRUE, row.names=FALSE)


     # get an idea per area
     er <- require(vmstools)
     if(!er){
        this$SI_LATI <- this$lat
        this$SI_LONG <- this$long
        data(ICESareas)
        this$area <- ICESarea(this, ICESareas, fast=TRUE)
        this$area <- factor(this$area)
        levels(this$area)[! levels(this$area) %in% selected_areas_for_table] <- "Other"
     } else{
        if (is.null(this$area)) {
           this$area <- NA
           warning("No area code found here. Try to install vmstools if within ICES area and re-run, otherwise add an area field by hand to the input file", call. = FALSE)
        }
     }
     table_obj[sce, ] <-  tapply(this [, a_type], this$area, sum, na.rm=TRUE)[colnames(table_obj)]
     



     this$round_long <- round(this$long*xcell)  # 15
     this$round_lat  <- round(this$lat*ycell)   # 20

     # find out the grid res
     lo <- sort(this$round_long, decreasing=TRUE)
     la <- sort(this$round_lat, decreasing=TRUE)
     most_freq_in_long <- as.numeric(names(sort(table(diff(lo/xcell)), decreasing=TRUE)[2]))
     most_freq_in_lat  <- as.numeric(names(sort(table(diff(la/ycell)), decreasing=TRUE)[2]))
     xcellkm <- distance(this$round_long[1]/xcell, mean(this$round_lat)/ycell, (this$round_long[1]/xcell) + most_freq_in_long, mean(this$round_lat)/ycell)
     ycellkm <- distance(mean(this$round_long)/xcell, this$round_lat[2]/ycell , mean(this$round_long)/xcell, (this$round_lat[2]/ycell) + most_freq_in_lat)

     this[,a_type]  <- round(this[,a_type])  /(xcellkm * ycellkm) # (5.576564*6.540486)  # if 15 and 20 then divide by cell area 8.925*5.561km  check??

     this$cell_id <-  paste(this$round_long, this$round_lat, sep="_")
     if(sce == the_baseline) {
        the_baseline_layer <- this
        the_baseline_layer <- aggregate(the_baseline_layer[,a_type],
                                list(the_baseline_layer$round_long, the_baseline_layer$round_lat, the_baseline_layer$cell_id), sum, na.rm=TRUE)
        colnames(the_baseline_layer) <- c("round_long", "round_lat", "cell_id", a_type)


       Satellite.Palette.baseline <-colorRampPalette(c("cyan","aquamarine","orange","red"))
       #the_breaks_baseline <-   c(0.5, 1, round(exp(seq(0.5, 14, by=1.1))), 1000000)

       the_points <- tapply(the_baseline_layer[,a_type],
                  list(the_baseline_layer$round_lat, the_baseline_layer$round_long), sum, na.rm=TRUE)

       the_points <- replace (the_points, the_points>the_breaks_baseline[length(the_breaks_baseline)], the_breaks_baseline[length(the_breaks_baseline)])

       image(
        x=as.numeric(as.character(colnames(the_points)))/xcell,     # 8.925km  at 53 degree N
        y=as.numeric(as.character(rownames(the_points)))/ycell,     # 5.561km at 53 degree N
        z= t(the_points),  # convert in tons
        breaks=c(the_breaks_baseline),
        col =  Satellite.Palette.baseline(length(the_breaks_baseline[-1]))  ,
        useRaster=FALSE,
        xlab="",
        ylab="",
        axes=FALSE,
        xlim=xlims, ylim=ylims    # balticonly
        )
       library(maps)
       if (!is.null(gis_shape)) if(length(gis_shape[[sce]])>0) for (i in 1:length(gis_shape[[the_baseline]])) plot(gis_shape[[the_baseline]][[i]], add=TRUE, col=grey(0.8), border=FALSE)
       #text(coordinates(ices_areas), labels=ices_areas$ICES_area, cex=1.4, col="black")


       box()
       mtext(side=3, sce, cex=1.2, line=0.5)
       axis(1, cex.axis=1.2)
       axis(2, las=2, cex.axis=1.2)


    x = c(xlims[1]+0.2, xlims[1]+0.4, xlims[1]+0.4, xlims[1]+0.2)
    y = c(ylims[1]+0.5, ylims[1]+4, ylims[1]+4, ylims[1]+0.5)
    the_breaks_leg <-NULL
      for(i in 1: length(the_breaks_baseline[-1])){ if(the_breaks_baseline[i]>1) {the_breaks_leg[i] <- round(the_breaks_baseline[i])} else{the_breaks_leg[i]<- the_breaks_baseline[i]}}
       legend.gradient2 (cbind(x = x , y = y ), cols=Satellite.Palette.baseline(length(the_breaks_baseline[-1])),
         limits="", title=expression(paste("Total catches kg per ", km^2)),
         legend= the_breaks_leg,
         cex=1, col="black")


        }   else{



      this <- aggregate(this[,a_type], list(this$round_long, this$round_lat, this$cell_id), sum, na.rm=TRUE)
      colnames(this) <- c("round_long", "round_lat", "cell_id", a_type)

      # Merge!
      this           <- merge(the_baseline_layer, this, by.x="cell_id", by.y="cell_id")
      this[,a_type]  <- (100* as.numeric(as.character(this[,paste0(a_type,".y")])) / as.numeric(as.character(this[,paste0(a_type,".x")])) )  -100

          # CAUTION!!!!: correct for area with low absolute value to avoid visual effect
      this[,a_type] [ this[,paste0(a_type,".x")] <quantile(this[,paste0(a_type,".x")] [ this[,paste0(a_type,".x")] !=0], prob=0.05)]  <- 0




    in_relative <- TRUE
    if(in_relative){
        the_points <- tapply( this[,a_type],
                  list(this$round_lat.y, this$round_long.y), sum)
        Satellite.Palette <-colorRampPalette(c("cyan","aquamarine","white","yellow","red"))
        the_breaks <-  c(rev(-round(exp(seq(0, 7, by=1)))),  0, round(exp(seq(0, 7, by=1))))
        } else{
        the_points <- tapply(this[,paste0(a_type,".y")],
                  list(this$round_lat.y, this$round_long.y), sum)
        the_breaks <-  the_breaks_baseline
        Satellite.Palette <- Satellite.Palette.baseline

        }


    the_points <- replace (the_points, the_points>the_breaks[length(the_breaks)], the_breaks[length(the_breaks)])


     # in ?
     sum(as.numeric(as.character(the_points)), na.rm=TRUE)


  if(sce %in% selected_scenarios_for_plot){
    image(
     x=as.numeric(as.character(colnames(the_points)))/xcell,   #15
     y=as.numeric(as.character(rownames(the_points)))/ycell,   # 20
     z= t(the_points),  # convert in tons
     breaks=c(the_breaks),
     col = Satellite.Palette(length(the_breaks[-1])),
     useRaster=FALSE,
     xlab="",
     ylab="",
     axes=FALSE,
     xlim=xlims, ylim=ylims
     )
    library(maps)
     if (!is.null(gis_shape)) if(length(gis_shape[[sce]])>0) for (i in 1:length(gis_shape[[the_baseline]])) plot(gis_shape[[the_baseline]][[i]], add=TRUE, col=grey(0.8), border=FALSE)
    #text(coordinates(ices_areas), labels=ices_areas$ICES_area, cex=1.4, col="black")


    box()
    mtext(side=3, sce, cex=1.2, line=0.5)
    axis(1, cex.axis=1.2)
    axis(2, las=2, cex.axis=1.2)


    x = c(xlims[1]+0.2, xlims[1]+0.4, xlims[1]+0.4, xlims[1]+0.2)
    y = c(ylims[1]+0.5, ylims[1]+4, ylims[1]+4, ylims[1]+0.5)
    the_breaks_leg <-NULL
    for(i in 1: length(the_breaks[-1])){ if(the_breaks[i]>1) {the_breaks_leg[i] <- round(the_breaks[i])} else{the_breaks_leg[i]<- the_breaks[i]}}
    legend.gradient2 (cbind(x = x , y = y ), cols=Satellite.Palette(length(the_breaks[-1])),
    limits="", title=expression(paste("% difference per cell")),
    legend= the_breaks_leg,
     cex=1.0, col="black")

    # add closure polygons:
     if (!is.null(gis_shape)) if(length(gis_shape[[sce]])>0) for (i in 1:length(gis_shape[[sce]])) plot(gis_shape[[sce]][[i]], add=TRUE,  border=grey(0.2), col=NA)




     } # end selected sce for plot
    } # end  Baseline
 } # end sce

  mtext("Latitude", 1, line=2, cex=1.5, outer=TRUE)
  mtext(side=2,"Longitude",line=2, cex=1.5, outer=TRUE)

dev.off()


table_obj <- cbind(table_obj, Total= apply(table_obj, 1, sum, na.rm=TRUE) ) # marginal value

table_obj_relative_to_baseline <- cbind(round(sweep(table_obj, 2, table_obj[1,], FUN="/")*100, 1)- 100)
table_obj_relative_to_baseline[1,] <- table_obj[1,]
write.table(table_obj_relative_to_baseline,   file=namefile2, col.names=TRUE, row.names=TRUE, sep=";", quote=FALSE)
print(namefile2)

# useful to copy/paste into Excel!
write.table(table_obj_relative_to_baseline, "clipboard", sep="\t", row.names=TRUE)   # export to excel

# check in absolute numbers:
# sum(table_obj_relative_to_baseline["svana_sub1mx20",]/100*table_obj_relative_to_baseline["svana_baseline",])



 return(table_obj_relative_to_baseline)
}




