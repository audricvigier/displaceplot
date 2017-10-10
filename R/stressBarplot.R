

#' stress barplots in percentage of affected vessels
#'
#' This function produces barplots of percentage of vessels affected per bin for various indicators facing baseline sce
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' 
#'
#'  
#' \dontrun{
#'
#'  general <- setGeneralOverallVariable(main_path_outputs =file.path("C:","DISPLACE_outputs"),
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
#'   selected_vessels_set_1 <- as.character(read.table(file.path(general$main.path, general$case_study,
#'           paste("selected_vessels_set_1.dat",sep='')), header=FALSE)[,1])
#'
#'
#'  # CAUTION: this following process can take a while!
#'  compareSimSimPlots(general=general, lst_loglike_agg_1=lst_loglike_agg_1, lst_loglike_agg_2=lst_loglike_agg_2, years_span=2015:2019, ...,
#'                                   explicit_pops=explicit_pops, plot_obs=TRUE,
#'                                       idx.sim=list(sce1=c(1), sce2=c(1)), combined_name=c("baseline_vs_implicit"),
#'                                         a.comment="", what="per_vessel", what2="weight", count=0,
#'                                          a.xlab="", a.ylab="", a.unit=1, do_mtext=FALSE)
#'
#'
#'   require(plotrix)
#'   stressBarplot (general=general,
#'                          the_baseline="svana_baseline",
#'                          selected_vessels=selected_vessels_set_1,
#'                          by_class=NULL,
#'                          a_width=1500, a_height=3500)
#'
#'
#'  }  






##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

stressBarplot <- function(general=general,
                          the_baseline="svana_baseline",
                          selected_vessels=selected_vessels_set_1,
                          by_class=NULL,
                          a_width=3000, a_height=2000){



   #----------------
   do_stress_plot <- function (indic, a_xlab, a_ylab, general, a_min=-1.4, a_max=1.4, a_by=0.2, a_mfrow=c(3,4), the_dim=c(3000, 2000), sces=sces){

   namefile       <- paste(paste("stress_barplot_",indic, sep=""))
   output.folder  <- file.path(general$main.path, general$namefolderinput)

   tiff(filename=file.path(output.folder, paste(namefile, ".tiff", sep="" )),
                                   width = the_dim[1], height = the_dim[2],
                                   units = "px", pointsize = 12,  res=300,  compression = c("lzw") )

   par(mfrow=a_mfrow)
   par(mar=c(2,3,3,1))
   par(oma=c(4,4,1,1))
   for(sce in sces){
    if(sce!="Calib"){
    this <- ind[ind$sce==sce,]
    dat  <- this[, indic]
   if( indic != "gain_av_gradva") { dat <- replace(dat, is.infinite(dat)| dat==0, 1); dat <- log(this[, indic])  }
     dat[dat< a_min+0.1] <- a_min+0.1
    dat[dat> a_max-0.1] <-  a_max-0.1
   dat <- cut(dat, breaks=round(seq(a_min,a_max,by=a_by),2))
     percent <- table(dat)/length(dat)*100
    print(sum(percent))
    nbc <- floor(length( seq(a_min,a_max,by=a_by))/2)
    if(length( seq(a_min,a_max,by=a_by)) != nbc*2) middle <- "OliveDrab" else middle <- NULL
    percent <- ifelse(percent>45, max(45, percent-45), percent)
    bar <- barplot(percent, beside=FALSE, xlim=c(0,100-45),cex.names=0.8, axisnames = FALSE,
                        col=c(rep("IndianRed",nbc), middle, rep("OliveDrab",nbc)),
                                     xlab="",space = 0.5, axes=FALSE, plot=TRUE, border=FALSE, las=2, legend=FALSE, horiz=TRUE)

    xat <- pretty(1:(100-45))
    the_xlab <- ifelse(xat>45, xat+45, xat)
    dd <- rownames(percent)
    dd <- gsub('\\(', '', dd)
    dd <- gsub('\\]', '', dd)
    dd <- gsub('\\,', ' ', dd)
    axis(1,at=xat, labels=the_xlab)
    axis(2,at=bar, labels=dd, las=2, cex.axis=0.7)
    library(plotrix)
    box()
    axis.break(1,45,style="slash")
     mtext(side=3, sce) # title(sce)
     }
    }
   mtext("% vessels", 1, line=2, cex=1.5, outer=TRUE)
   mtext(side=2,a_ylab,line=2, cex=1.5, outer=TRUE)
   dev.off()

   return()
   }



   ## ---------------------------------------------------------------------------------##
   do_stress_plot_with_classes <- function (indic, by_class="gear", a_xlab, a_ylab, general, a_min=-1.4, a_max=1.4, a_by=0.2,
                                         a_mfrow=c(3,4), the_dim=c(3000, 2000), add_legend=TRUE, sces=sces){


    namefile       <- paste(paste("stress_barplot_per_",by_class,"_",indic, sep=""))
    output.folder  <- file.path(general$main.path, general$namefolderinput)

    tiff(filename=file.path(output.folder, paste(namefile, "_", by_class, ".tiff", sep="" )),
                                   width = 1200, height = 3500,
                                   units = "px", pointsize = 12,  res=300,  compression = c("lzw") )

    library(RColorBrewer)
    the_colors <- brewer.pal(8, "Paired")

    par(mfrow=a_mfrow)
    par(mar=c(2,3,3,1))
    par(oma=c(4,4,1,1))
    for(sce in sces){
       if(sce!="Calib"){
       dat <- ind[ind$sce==sce, indic]
       if( indic != "gain_av_gradva") dat <- log(ind[ind$sce==sce, indic])
       dat[dat< a_min] <- a_min  +0.1
      dat[dat> a_max] <-  a_max   -0.1
      dat    <- cut(dat, breaks=round(seq(a_min,a_max,by=a_by),2))
      the_classes  <- ind[ind$sce==sce, by_class]

      nbc <- floor(length( seq(a_min,a_max,by=a_by))/2)
      if(length( seq(a_min,a_max,by=a_by)) != nbc*2)  middle <-  rep(the_colors[1:length(by_class)], 1)  else middle <- NULL

      nbcl <- length(unique(ind[,by_class]))

      percent <- sweep(table(dat, the_classes), 2, sum(apply(table(dat, the_classes), 2, sum)), FUN="/")  *100
       percent[apply(percent,1, sum)>45, ] <- percent[apply(percent,1, sum)>45, ] * 45/apply(percent,1, sum)[apply(percent,1, sum)>45]
      bar <- barplot(t(percent), beside=FALSE, xlim=c(0,100-45),cex.names=0.8, axisnames = FALSE,
                        col= c(rep(the_colors[1:nbcl], nbc), middle,  rep(the_colors[(nbcl+1):(nbcl+nbcl)], nbc)), # vsize classes
                                     xlab="% vessels",space = 0.5, axes=FALSE, plot=TRUE, border=FALSE, las=2,
                                     legend=add_legend, horiz=TRUE, args.legend=list(x = "bottomright", cex=1, bty="n"))
      mtext(side=2,a_ylab,line=3.5, cex=1.0)
      xat <- pretty(1:(100))
      the_xlab <- xat

      xat <- pretty(1:(100-45))
      the_xlab <- ifelse(xat>45, xat+45, xat)
      dd <- rownames(percent)
      dd <- gsub('\\(', '', dd)
      dd <- gsub('\\]', '', dd)
      dd <- gsub('\\,', ' ', dd)
      axis(1,at=xat, labels=the_xlab)
      axis(2,at=bar, labels=dd, las=2, cex.axis=0.7)
      #library(plotrix)
      box()
      axis.break(1,45,style="slash")
       mtext(side=3, sce, cex=0.9) # title(sce)
       }
      }
    mtext("% vessels", 1, line=2, cex=1.2, outer=TRUE)

    dev.off()

    return()
    }


    #---------------------
   load( file=file.path(general$main.path, general$namefolderinput,
                     paste("selected_vessels.RData", sep='')) )
   ind <- read.table(file=file.path(general$main.path, general$namefolderinput,
                     paste("vid_indicators_gain_in_totland_and_vpuf_", the_baseline,".txt", sep="")),
                      header=TRUE, sep = " ")
                      # IF SMALL BUG WHEN READING?: just need to put headers on the same line first...   (do the correction in notepad++ in case processed by linux)
   ind <- ind[ind$vid %in% selected_vessels,]
   #levels(ind$sce) <-  c( "excludeIn6nmSoleSanctuaryAndPomoPit", "excludeInBuffer4nm" ,    "excludeInBuffer6nm",     "excludeInPomoPitBan",    "excludeInSoleSanctuary"
   #                      )
   ind$sce         <- factor(as.character(ind$sce)) # get the alphabetical order
   an <- function(x) as.numeric(as.character(x))
   the_dim        <- c(a_width, a_height)
   sces <- unique(ind$sce)
   #sces <- general$namefolderoutput


 ## CALLS-------
 indic    <- "gain_av_gradva"
 a_xlab   <- "log(GVA/baseline GVA)"
 a_ylab   <- "log(GVA/baseline GVA)"
 do_stress_plot (indic, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=the_dim, sces=sces)

 indic <- "gain_av_vapuf"
 a_ylab  <- "log(VPUF/baseline VPUF)"
 do_stress_plot (indic, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=the_dim, sces=sces)

 indic <- "gain_totland"
 a_ylab  <- "log(total landings/baseline landings)"
 do_stress_plot (indic, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=the_dim, sces=sces)

 indic <- "gain_av_trip_duration"
 a_ylab  <- "log(average trip duration/baseline duration)"
 do_stress_plot (indic, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=the_dim, sces=sces)

 indic <- "gain_av_traveled_dist"
 a_ylab  <- "log(average traveled distance/baseline distance)"
 do_stress_plot (indic, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=the_dim, sces=sces)

 indic <- "gain_av_nbtrip"
 a_ylab  <- "log(average number of trips/baseline number)"
 do_stress_plot (indic, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=the_dim, sces=sces)

 indic <- "gain_fcpue_explicit"
 a_ylab  <- "log(average gain in CPUEs/baseline number)"
 do_stress_plot (indic, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=the_dim, sces=sces)



 if(length(by_class)!=0){

  ## calls----------------
  ind$gear            <- NA
  ind[grep("NET", ind$vid), "gear"] <- "Net setters" # map
  ind[grep("OTB", ind$vid), "gear"] <- "Otter Trawl" # map
  ind[grep("RAP", ind$vid), "gear"] <- "Rapido Trawl" # map
  ind$gear <- factor(ind$gear)

  indic <- "gain_av_gradva"
  a_ylab  <- "log(GVA/baseline GVA)"
  do_stress_plot_with_classes (indic, by_class=by_class, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=c(1000, 3000), sces=sces)

  indic <- "gain_av_vapuf"
  a_ylab  <- "log(VPUF/baseline VPUF)"
  do_stress_plot_with_classes (indic, by_class=by_class, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=c(1000, 3000), sces=sces)

 indic <- "gain_totland"
 a_ylab  <- "log(total landings/baseline landings)"
 do_stress_plot_with_classes (indic, by_class=by_class, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=c(1000, 3000), sces=sces)

 indic <- "gain_av_trip_duration"
 a_ylab  <- "log(average trip duration/baseline duration)"
 do_stress_plot_with_classes (indic, by_class=by_class, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=c(1000, 3000), sces=sces)

 indic <- "gain_av_traveled_dist"
 a_ylab  <- "log(average traveled distance/baseline distance)"
 do_stress_plot_with_classes (indic, by_class=by_class, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=c(1000, 3000), sces=sces)

 indic <- "gain_av_nbtrip"
 a_ylab  <- "log(average number of trips/baseline number)"
 do_stress_plot_with_classes (indic, by_class=by_class, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=c(1000, 3000), sces=sces)

 indic <- "gain_fcpue_explicit"
 a_ylab  <- "log(average gain in CPUEs/baseline number)"
 do_stress_plot_with_classes (indic, by_class=by_class, a_xlab, a_ylab, general, a_min=-0.5, a_max=0.5, a_by=0.1, a_mfrow=c(5,1), the_dim=c(1000, 3000), sces=sces)

  } # end class

 return()
}






















