

#' Produce pie plots centered on ports with proportion affected vessels in classes of stress
#'
#' This function produces pieplots centered on ports of %  of vessel affected per bin for various indicators facing baseline sce
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' \dontrun{
#' general <- setGeneralOverallVariable(main_path_outputs =file.path("C:","DISPLACE_outputs"),
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
#'   library(maptools)
#'   NSsub1mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                     'DISPLACE_SVANAProject', 'Input for DISPLACE', 'NST2_sub1_mx20_wgs84'), proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   BSsub1mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                     'DISPLACE_SVANAProject', 'Input for DISPLACE', 'BHT2_Sub1_Mx_20_wgs84'), proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   NSsub4mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                       'DISPLACE_SVANAProject', 'Input for DISPLACE', 'NST2_sub4_mx_20_wgs84'), proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   BSsub4mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                         'DISPLACE_SVANAProject', 'Input for DISPLACE', 'BHT2_Sub4_Mx_20LongTailedD_wgs84'), proj4string=CRS("+proj=longlat #'+ellps=WGS84"))
#'
#'
#'  
#'  
#' loadLoglikeFiles (general, use_port_info=TRUE)
#'
#'
#' comparePieplotStressPerPort (general,
#'                                       nbsim=20,
#'                                       the_baseline="svana_baseline",
#'                                       a_polygon_including_interesting_ports= list(x=c(-2.850909,  16.955191,  18.233003, -12.434505),
#'                                                                                   y=c(46.47831, 52.22793, 63.08831, 62.44946)),
#'                                       selected_scenarios=  c("svana_sub1mx20","svana_sub4mx20"), 
#'                                       gis_shape=list(
#'                                           svana_sub1mx20=   list(NSsub1mx20, BSsub1mx20),
#'                                           svana_sub4mx20=   list(NSsub4mx20, BSsub4mx20)),
#'                                           a_width= 3200, a_height =2100, xlims =  c(7, 16), ylims = c(53.5,59)
#'                                           ) 
#'  }



 


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##




comparePieplotStressPerPort <- function(general,
                                       nbsim=20,
                                       the_baseline="svana_baseline",
                                       a_polygon_including_interesting_ports= list(x=c(-2.850909,  16.955191,  18.233003, -12.434505),
                                                                                   y=c(46.47831, 52.22793, 63.08831, 62.44946)),
                                       selected_scenarios=  c("svana_sub1mx20","svana_sub4mx20"), 
                                       gis_shape=list(
                                           svana_sub1mx20=   list(NSsub1mx20, BSsub1mx20),
                                           svana_sub4mx20=   list(NSsub4mx20, BSsub4mx20)),
                                           a_width= 3400, a_height =3500, xlims =  c(-1, 17), ylims = c(53,60)
                                                               
                                     ){

 
 
  #-------------------
  compare_per_port <- function(
          sim=1,
          a_polygon=list(x=c(13.27,9.08,11.25,15.15, 15.00,55.67),
                                  y=c(56.00,54.82,53.50,53.89,55.11,14.15)),
          sce1   =lst_loglike_agg_weight_vid_port_high_profit_grounds,
          sce2   =lst_loglike_agg_weight_vid_port_high_profit_grounds_biolsce_Linfs_M_mig_weight,
          stock  ="pop.10"
          )  {

   an <- function(x)as.numeric(as.character(x))
   library(sp)
   obj1 <- sce1[[sim]][ point.in.polygon( an(sce1[[sim]]$ld_port_x), an(sce1[[sim]]$ld_port_y), a_polygon$x, a_polygon$y )==1 ,]
   obj2 <- sce2[[sim]][ point.in.polygon( an(sce2[[sim]]$ld_port_x), an(sce2[[sim]]$ld_port_y), a_polygon$x, a_polygon$y )==1,]




   dd <-  tapply(apply(obj1[,stock], 1, sum, na.rm=TRUE),
        obj1$land_port, sum, na.rm=TRUE)
   dd2 <-  tapply(apply(obj2[,stock], 1, sum, na.rm=TRUE),
        obj2$land_port, sum, na.rm=TRUE)
   a_diff       <-  round(an(dd2[names(dd2)])/  an(dd[names(dd)]) *100  )    -100
  names(a_diff) <- names(dd)

  dd3 <-   tapply(obj1[,"rev_from_av_prices"],
        obj1$land_port, sum, na.rm=TRUE)
  

  nm_ports <- names(a_diff[!is.na(a_diff)])
  print(dd[nm_ports])
  print(dd2[nm_ports])

  return(cbind(a_diff[!is.na(a_diff) & !is.infinite(a_diff)], dd3[!is.na(a_diff) & !is.infinite(a_diff)]))
  }



  ##-----------
  ##-----------
  ##---utils---
  compare_per_port_vessel <- function(
          sim=1,
          a_polygon=list(x=c(13.27,9.08,11.25,15.15, 15.00,55.67),
                                  y=c(56.00,54.82,53.50,53.89,55.11,14.15)),
          sce1   =lst_loglike_agg_weight_vid_port_high_profit_grounds,
          sce2   =lst_loglike_agg_weight_vid_port_high_profit_grounds_biolsce_Linfs_M_mig_weight,
          stock  ="pop.10"
          )  {

   an <- function(x)as.numeric(as.character(x))
   library(sp)                  
   obj1 <- sce1[[sim]][ point.in.polygon( an(sce1[[sim]]$ld_port_x), an(sce1[[sim]]$ld_port_y), a_polygon$x, a_polygon$y )==1 ,]
   obj2 <- sce2[[sim]][ point.in.polygon( an(sce2[[sim]]$ld_port_x), an(sce2[[sim]]$ld_port_y), a_polygon$x, a_polygon$y )==1,]




    dd <-  aggregate(apply(obj1[,stock], 1, sum, na.rm=TRUE),
        list(obj1$land_port, obj1$VE_REF), sum, na.rm=TRUE)
    colnames(dd)[3] <- "ref" 
    dd2 <-  aggregate(apply(obj2[,stock], 1, sum, na.rm=TRUE),
         list(obj2$land_port, obj2$VE_REF), sum, na.rm=TRUE)
    colnames(dd2)[3] <- "sce" 

    dd3 <- merge(dd, dd2)
  
    dd3$a_diff       <-  round(an(dd3[,"sce"])/  an(dd3[,"ref"]) *100  )    -100
   
    dd4 <-   tapply(obj1[,"rev_from_av_prices"],
         list(obj1$land_port), sum, na.rm=TRUE)
  
    dd3$totrevenue <- dd4 [as.character(dd3[,1])]
  
    dd3[is.infinite(dd3$a_diff), "a_diff"]  <- - 100

 
    return(dd3[!is.na(dd3$a_diff)  & !is.infinite(dd3$a_diff),])
    }


 
  #-------------------------------- 
  these_ports <- NULL
  what <- "weight"
  loglike_baseline <- get( paste("lst_loglike_agg_",what,"_vid_port_", the_baseline , sep=''), env=.GlobalEnv)
  
  for(sce in general$namefolderoutput[!general$namefolderoutput %in% c(the_baseline)]) {


  for(sim in 1:nbsim) {

   
      

       loglike <- get( paste("lst_loglike_agg_",what,"_vid_port_", sce, sep=''), env=.GlobalEnv)
       res     <- compare_per_port_vessel(sim=sim,
                      a_polygon=a_polygon_including_interesting_ports,
                      sce1=loglike_baseline,
                      sce2=loglike,
                #      stock  =c("pop.0", "pop.1", "pop.2", "pop.3"))
                      stock  =c("rev_from_av_prices", "rev_from_av_prices"))

      these_ports <- rbind.data.frame(these_ports,
                                  cbind(sce, sim, res)
                                  )
   }
  } # end sce



  
  these_ports <- these_ports[these_ports$sce %in% selected_scenarios,]
  these_ports$sce <- factor(these_ports$sce)

  colnames(these_ports) <- c( "sce", "sim", "port", "vid", "ref", "sce", "percent_change", "totrevenue")



  ## get back the port name
  port_names <- read.table(file=file.path(general$main.path.ibm,
                                paste("harboursspe_",general$namefolderinput,sep=''),
                                  paste("names_harbours.dat", sep='')), sep=";", header=TRUE)
  port_names           <- cbind(port_names, port=rownames(port_names))
  coord <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
             paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  colnames(coord) <- c('x', 'y', 'harb')

  nodeports <- as.numeric(sapply(port_names$node.name, function(x) strsplit(as.character(x), " ")[[1]])[1,])
  these_ports$land_port    <- port_names[   as.numeric(as.character(these_ports$port))  , 'node.name']
  
  an <- function(x)as.numeric(as.character(x))
  these_ports$x    <- coord[  nodeports[an(these_ports$port)+1 ], "x"]  
  these_ports$y    <- coord[  nodeports[an(these_ports$port)+1 ], "y"]  
 

  sauv <- these_ports

                 
  library(maptools)                              
 
 # plot
 namefile       <- paste(paste("pie_chart_per_harbour", sep=""))
 output.folder  <- file.path(general$main.path, general$namefolderinput)
 tiff(filename=file.path(output.folder, paste(namefile, "2.tiff", sep="" )),
                                   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=300, compression=c("lzw"))
 par(mar=c(2.5,3,1,1))
 par(mfrow=c(1,2))
 par(oma=c(1,2,1,1))
 
 
 # 1
 library(mapdata)
 map("worldHires",   xlim=xlims, ylim=ylims, add=FALSE, col=grey(0.8), border=grey(0.8), fill=TRUE, asp= 2.587761)
 mtext(side=1, text="Longitude", line=1.9)
 mtext(side=2, text="Latitude", line=2.7)
 
  # add the pie chart
 library(mapplots)
 library(scales)
 box()
 
        if (!is.null(gis_shape)) if(length(gis_shape[[selected_scenarios[1]]])>0) for (i in 1:length(gis_shape[[selected_scenarios[1]]])) plot(gis_shape[[selected_scenarios[1]]][[i]], add=TRUE, fill=FALSE, border=grey(0.8))
 
 maxrev <- max(these_ports$totrevenue)  
 
 dd <-  these_ports[these_ports$sce==selected_scenarios[1],]  

 
 make.xyz<- function (x, y, z, group, FUN = sum, ...) 
{
    Z <- tapply(z, list(paste(x, y, sep = ", "), group), FUN, 
        ...)
    Z <- ifelse(is.na(Z), 0, Z)
    XY <- rownames(Z)
    tempfun <- function(XY, i) {
        as.numeric(unlist(lapply(strsplit(XY, ", "), function(x) x[i])))
    }
    X <- tempfun(XY, 1)
    Y <- tempfun(XY, 2)
    return(list(x = X, y = Y, z = Z))
}

 dd$stressclass <- cut (as.numeric(as.character(dd$percent_change)), breaks=c(-1000,-25,0, +25,+1000))
 dd$totrevenue2 <- dd$totrevenue / table(dd$port) [dd$port] # caution, important correction to account for the nb of time a port has been used => average revenue per trip over 5 years over 10 replicates
 xyz <- make.xyz(as.numeric(as.character(dd$x)), as.numeric(as.character(dd$y)), as.numeric(as.character(dd$totrevenue2)) , dd$stressclass)
 library(RColorBrewer)
 draw.pie (z=xyz$z, x=xyz$x, y=xyz$y, radius=0.5, col=alpha(rev(brewer.pal(4, "RdYlGn")), 0.8), labels="")
 legend.bubble(10,54, z=max(dd$totrevenue),round=0, maxradius=0.5, bty="n",txt.cex=0.6)
  axis(1)
  axis(2, las=2)
  box()
  mtext(side=3, adj=0, text=selected_scenarios[1], line=1)

 
  #head(dd[dd$land_port=="35084 Hundested",])

 
 # add the shapefile of closures
 library(maptools)
  
  
 #plot(excludeInBuffer6nm, add=TRUE, border="red")

  ports <- dd[!duplicated(dd$port), c('x', 'y', 'port')]
 # text (as.numeric(as.character(ports$x))+0.5, as.numeric(as.character(ports$y))+0.05,
 #       labels=ports$port, cex=0.5, col=1)
  legend("bottomright", legend=c('<-25%', '-25,0%', '0,+25%', '>25%'), fill=alpha(brewer.pal(4, "RdYlGn"), 0.8), bty="n")

  cc <- these_ports[!duplicated(data.frame(these_ports$port, these_ports$sce)),]
 
  # 2
 library(mapdata)
 map("worldHires",   xlim=xlims, ylim=ylims, add=FALSE, col=grey(0.8), border=grey(0.8), fill=TRUE, asp= 2.587761)
 mtext(side=1, text="Longitude", line=1.9)
 mtext(side=2, text="Latitude", line=2.7)


  if (!is.null(gis_shape)) if(length(gis_shape[[selected_scenarios[2]]])>0) for (i in 1:length(gis_shape[[selected_scenarios[2]]])) plot(gis_shape[[selected_scenarios[2]]][[i]], add=TRUE,fill=FALSE, border=grey(0.8))
 

 dd <-  these_ports[these_ports$sce==selected_scenarios[2],]  
 
 dd$stressclass <- cut (as.numeric(as.character(dd$percent_change)), breaks=c(-1000,-25,0, +25,+1000))
 dd$totrevenue  <- dd$totrevenue / table(dd$port) [dd$port] # caution, important correction to account for the nb of time a port has been used => average 5 year revenue per vessel in this port over 10 replicates
 xyz <- make.xyz(as.numeric(as.character(dd$x)), as.numeric(as.character(dd$y)), as.numeric(as.character(dd$totrevenue)) , dd$stressclass)
 library(RColorBrewer)
 draw.pie (z=xyz$z, x=xyz$x, y=xyz$y, radius=0.5, col=alpha(rev(brewer.pal(4, "RdYlGn")), 0.8), labels="")
 legend.bubble(10,54, z=max(dd$totrevenue),round=0,maxradius=0.5, bty="n",txt.cex=0.6)

 # add the shapefile of closures
 library(maptools)
  
  
 
  ports <- dd[!duplicated(dd$port), c('x', 'y', 'port')]

  legend("bottomright", legend=c('<-25%', '-25,0%', '0,+25%', '>25%'), fill=alpha(brewer.pal(4, "RdYlGn"), 0.8), bty="n")
  axis(1)
  axis(2, las=2)
  box()
  mtext(side=3, adj=0, text=selected_scenarios[2], line=1)
  
 


dev.off()



return()
}











