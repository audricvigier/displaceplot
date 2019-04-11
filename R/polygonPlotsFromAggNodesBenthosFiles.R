

            
#' Produce polygons time series plots for Benthos indicators and per functional groups
#'
#' This function generates polygon plots in absolute or relative to the baseline scenario.
#' Input data files are searched in the folder hierarchy.
#' Only the median is displayed if in relative terms because the uncertainty range cannot be handled in this case
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
#'
#'
#'   polygonPlotsFromAggNodeBenthosFiles (general, 
#'                                                is_relative= TRUE,
#'                                                the_baseline= "svana_baseline")
#'
#'
#'  }








polygonPlotsFromAggNodeBenthosFiles <- function(general, 
                                                is_relative= TRUE,
                                                the_baseline= "svana_baseline"){


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}





   
   allcumbenthos_ts <- NULL
   for(sce in general$namefolderoutput){ 
      this <- read.table(file=file.path(general$main.path, general$namefolderinput,  sce,
                              paste("allcumbenthos_ts.txt", sep='')), header=FALSE, skip = 1, sep=" ")
      colnames(this)   <- c("tstep","funcgr",  "sim", "sce", "totbio", "totN", "meanweight", "biomassK")
      allcumbenthos_ts <- rbind.data.frame(allcumbenthos_ts, this)
   }
   
   allcumbenthos_ts$biomassOverK <-  allcumbenthos_ts$totbio / allcumbenthos_ts$biomassK 
        
   library(doBy)
   this <- orderBy(~ tstep+funcgr+sce+sim, data=allcumbenthos_ts)
  
  
  
   for (a_variable in c("totbio", "totN", "meanweight", "biomassOverK")){
   
  
   a_width <- 3400 ; a_height <- 1200 
   
   if(is_relative) a_comment <- "_relative"  else a_comment <- ""
   namefile <- file.path(general$main.path, general$namefolderinput, paste("cumbenthos_ts_allfuncgr_",a_variable, a_comment,".tiff", sep='') )   

   
   tiff(filename=namefile,   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=300, compression = c("lzw"))
  # pdf(file=namefile,   width = 7, height = 7)
  
     if(length(unique(this$funcgr))==2){
        m <- rbind(c(1, 2))
       layout(m)
       }
     if(length(unique(this$funcgr))==4){
        m <- matrix(c(1,2,3,4), 2, 2, byrow = TRUE)
       layout(m)
       }
     par(mar=c(2,2,3,1))
     par(oma=c(4,4,1,1))
   
  
  ## GGPLOT 
  library(ggplot2)
  
  count<-0
  for(funcgr in unique(this$funcgr)){
  count <- count+1
  
     allsce <- NULL
     for(sce in unique(this$sce)){
        a_df   <- this[this$funcgr==funcgr & this$sce==sce,]
        quant  <- tapply(a_df[,a_variable], a_df$tstep, quantile, prob=c(0.05,0.5, 0.95))
        lowp   <- unlist(lapply(quant, function(x) x["5%"]))
        medp   <- unlist(lapply(quant, function(x) x["50%"]))
        higp   <- unlist(lapply(quant, function(x) x["95%"]))
  
        dat     <- data.frame(tstep= unlist(lapply(strsplit(names(medp), "\\."), function(x)x[1])), medp=medp, lowp=lowp, higp=higp)
        dat$sce <- sce
        allsce  <- rbind.data.frame(allsce, dat) 
     }
    
    
    # express numbers relative to baseline
    if(is_relative){
       baselinesce <-  allsce[allsce$sce==the_baseline,]
       dd <- merge(allsce, baselinesce, by.x="tstep", by.y="tstep")
       dd[,c ("medp.x", "lowp.x", "higp.x")] <- dd[,c ("medp.x", "lowp.x", "higp.x")]  / dd[,c ("medp.y", "lowp.y", "higp.y")] 
       colnames(dd) [colnames(dd) %in%  c ("medp.x", "lowp.x", "higp.x")] <-  c ("medp", "lowp", "higp")  # rename to make it generic easy
       colnames(dd) [colnames(dd) %in%  c ("sce.x")]   <- "sce"
       allsce <- dd
    }
    
    
    allsce$tstep  <- as.numeric(as.character(allsce$tstep))
    allsce$medp  <- as.numeric(as.character(allsce$medp))
    allsce$lowp  <- as.numeric(as.character(allsce$lowp))
    allsce$higp  <- as.numeric(as.character(allsce$higp))
  
   if(is_relative) assign(
      paste("plot_",count, sep=''), 
        ggplot(allsce, aes(tstep)) +  ylab(paste(a_variable, funcgr)) +
        geom_line(aes(y=medp, group=sce, color =sce)) 
     )
   
   if(!is_relative) assign(
      paste("plot_",count, sep=''), 
        ggplot(allsce, aes(tstep)) +  ylab(paste(a_variable, funcgr)) +
        geom_line(aes(y=medp, group=sce, color =sce))  +
        geom_ribbon(aes(ymin=lowp, ymax=higp, group=sce, fill =sce), alpha=1)
     )
 
  
 }
 
  multiplot(plot_1, plot_2, plot_3, plot_4, cols=2)
 
  dev.off()
  
  } # end a_variable 


return()
}



