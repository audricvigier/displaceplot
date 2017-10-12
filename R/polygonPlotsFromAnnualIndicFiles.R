
#' Produce polygon plots of time series for annual indicators
#'
#' This function produces all accumulated time series over month to compare scenario outcomes
#' All the plots are being stored in a polygons_plots folder that can further be found in the output folder.
#' (compare up to 5 scenarios simultaneously)
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
#' loadLoglikeFiles(general=general, use_port_info=FALSE)
#' 
#' 
#' 
#' polygonPlotsFromAnnualIndicFiles (general=general,
#'                                          a_variable="Fbar",  # could be "Fbar" or "totland" or "tac"
#'                                            the_baseline="svana_baseline",
#'                                            a_width=3500,
#'                                            a_height=1000,
#'                                            selected_scenarios=general$namefolderoutput[1:3],
#'                                            explicit_pops=c(0, 1, 2, 3, 11, 23, 24, 26, 30, 31, 32),
#'                                            is_individual_export=TRUE
#'                                            )
#'
#'   }



 
polygonPlotsFromAnnualIndicFiles <- function(general=general,
                                            a_variable="Fbar",
                                            the_baseline="svana_baseline",
                                            a_width=3500,
                                            a_height=1000,
                                            selected_scenarios=general$namefolderoutput[1:3],
                                            explicit_pops=c(0, 1, 2, 3, 11, 23, 24, 26, 30, 31, 32),
                                            is_individual_export=TRUE
                                            )
{


    do_polygon_plot_annual_indic <- function(
                  a_variable="Fbar",
                  a_stock=0,
                  a_stockname="0",
                  selected_scenarios=general$namefolderoutput[1:3],
                  the_scenario_names=general$namefolderoutput[1:3], 
                  name_set_of_sces= "setA",
                  selected=selected,
                  export=TRUE,
                  a_xlab="# Month",
                  a_ylab="F",               
                  add_legend=FALSE,
                  color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4),
                              rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
                  the_sims=1:20
                  ) {
 

  
    # look at annual indics such as the TACs...
    res <- NULL
    for(sce in selected_scenarios) {


         for(i in the_sims) {
         # merge all infos
               annual_indics              <-  read.table (file=file.path(general$main.path, general$namefolderinput, sce, paste('popdyn_annual_indic_simu',  i,".dat", sep=''))) [, 1:7]
               colnames(annual_indics)    <-  c("tstep", "stk", "multi", "multi2", "Fbar", "totland", "tac")

             res <- rbind (res, cbind(annual_indics, sce=sce, simu=paste("simu", i, sep="_")))
             }
      }
      res <- res[res$stk==a_stock,]
       


    sce1   <- selected_scenarios[1]
    sce2   <- selected_scenarios[2]
    sce3   <- selected_scenarios[3]
    if(length(selected_scenarios)>=4) sce4   <- selected_scenarios[4]
    if(length(selected_scenarios)>=5) sce5   <- selected_scenarios[5]
   
    obj1        <- res[res$sce==sce1,]
    obj2        <- res[res$sce==sce2,]
    obj3        <- res[res$sce==sce3,]
    if(length(selected_scenarios)>=4) obj4        <- res[res$sce==sce4,]
    if(length(selected_scenarios)>=5) obj5        <- res[res$sce==sce5,]
   
 
    simu_names <-  the_sims
     
    ## a nice quantile plot 
    mat_sce_base <- matrix(NA, nrow=max(the_sims), ncol=length( unique(obj1[,"tstep"]) )+1 ) # assuming max 11000 records
    rownames(mat_sce_base) <- 1:max(the_sims)
    mat_sce1 <- matrix(NA, nrow=max(the_sims),  ncol=length( unique(obj1[,"tstep"]) )+1)
    rownames(mat_sce1) <- 1:max(the_sims)
    mat_sce2 <- matrix(NA, nrow=max(the_sims),  ncol=length( unique(obj1[,"tstep"]) )+1)
    rownames(mat_sce2) <- 1:max(the_sims)
    if(length(selected_scenarios)>=4) mat_sce3 <- matrix(NA, nrow=max(the_sims),  ncol=length( unique(obj1[,"tstep"]) )+1)
    if(length(selected_scenarios)>=4) rownames(mat_sce3) <- 1:max(the_sims)
    if(length(selected_scenarios)>=5) mat_sce4 <- matrix(NA, nrow=max(the_sims),  ncol=length( unique(obj1[,"tstep"]) )+1)
    if(length(selected_scenarios)>=5) rownames(mat_sce4) <- 1:max(the_sims)
    for (sim in simu_names){

      dat <- as.numeric(obj1[obj1$simu==paste("simu_", sim, sep=''),a_variable])
      mat_sce_base[sim, ] <-       c(dat, dat[length(dat)]) 
                        
      dat <-  as.numeric(obj2[obj2$simu==paste("simu_", sim, sep=''),a_variable]) 
      mat_sce1[sim, ] <-      c(dat, dat[length(dat)]) 
                          
      dat <-  as.numeric(obj3[obj3$simu==paste("simu_", sim, sep=''),a_variable])
      mat_sce2[sim, ] <-        c(dat, dat[length(dat)]) 
                            
    
      if(length(selected_scenarios)>=4) {
      dat <-  as.numeric(obj4[obj4$simu==paste("simu_", sim, sep=''),a_variable]) 
      mat_sce3[sim, ] <-        c(dat, dat[length(dat)]) 
      }
                          
      if(length(selected_scenarios)>=5){
      dat <-   as.numeric(obj5[obj5$simu==paste("simu_", sim, sep=''),a_variable]) 
       mat_sce4[sim, ] <-       c(dat, dat[length(dat)]) 
      }
    }


    if(export) tiff(file=file.path(general$main.path, general$namefolderinput, "polygon_plots",
                    paste("", a_variable,"perScenario_",a_stockname, "_", name_set_of_sces, ".tiff", sep="")), compression="lzw")
   
    sim_ref <- names(which.max (apply(mat_sce_base, 1, function(x) sum(as.numeric(x), na.rm=TRUE))) )
   
   er <- try(   {
         
   plot(mat_sce1[sim_ref, ],  
           ylim=c(0,   max(as.numeric(obj1[obj1$simu==paste("simu_",sim_ref, sep=''),a_variable]))),    
        #   ylim=c(0,   1.2), 
       col=2, type="n", xlab="Year", ylab="", cex.lab=1.6, axes=FALSE)
      
   
      polygon(x=c((1:ncol(mat_sce_base))[1:(5)], rev((1:ncol(mat_sce_base))[1:(5)])),
       y=c(apply(mat_sce_base[,1:(5)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["5%",], rev(apply(mat_sce_base[,1:(5)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["95%",])),
         col= color_legend[1], border=NA)   # blue

       polygon(x=c((1:ncol(mat_sce1))[1:(5)], rev((1:ncol(mat_sce1))[1:(5)])),
       y=c(apply(mat_sce1[,1:(5)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["95%",], rev(apply(mat_sce1[,1:(5)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["5%",])),
         col=  color_legend[2], border=NA)  # green

       polygon(x=c((1:ncol(mat_sce2))[1:(5)], rev((1:ncol(mat_sce2))[1:(5)])),
       y=c(apply(mat_sce2[,1:(5)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["5%",], rev(apply(mat_sce2[,1:(5)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["95%",])),
         col=   color_legend[3], border=NA) # red
         
       if(length(selected_scenarios)>=4) polygon(x=c((1:ncol(mat_sce3))[1:(5)], rev((1:ncol(mat_sce3))[1:(5)])),
       y=c(apply(mat_sce3[,1:(5)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["5%",], rev(apply(mat_sce3[,1:(5)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["95%",])) ,
         col= color_legend[4], border=NA) # grey
 
       if(length(selected_scenarios)>=5) polygon(x=c((1:ncol(mat_sce4))[1:(5)], rev((1:ncol(mat_sce4))[1:(5)])),
       y=c(apply(mat_sce4[,1:(5)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["5%",], rev(apply(mat_sce4[,1:(5)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["95%",])) ,
         col=  color_legend[5], border=NA) # grey
    

         
    axis(2, las=2, cex.axis=1.5)
    axis(1, at=1:5, labels=c(12, 24,36,48,60), cex.axis=1.5)
    if(add_legend) legend("topleft", fill=color_legend, border =color_legend, legend=the_scenario_names, cex=1.3, bty="n")
    box()
  
    mtext(side = 2, text = substitute( paste(italic('F'), a_stockname)), line = 3.6, cex=1.2)
  
    abline(h=0, lty=2, col=grey(0.9))
 
   }, silent=TRUE)

   if(class(er)=="try-error"){
           print(paste("no data."))

   }
   
   if(export) dev.off()
  
 
   return()
   }

   


   #---------------------
   dir.create(file.path(general$main.path, general$namefolderinput,"polygon_plots"))
   
   
   graphics.off()
    tiff(file=file.path(general$main.path, general$namefolderinput, "polygon_plots", paste("accumulated_per_scenario_polygon_", a_variable, ".tiff", sep="")), 
                                  width = a_width, height = a_height,   compression="lzw",
                                   units = "px", pointsize = 12,  res=300)
   par(mfrow=c(1,1))
   par(mar=c(2,4.4,2,2))
   par(oma=c(4,4,1,1))

   for (a_stock in explicit_pops){
  
   do_polygon_plot_annual_indic (
                  a_variable=a_variable,
                  a_stock=a_stock,
                  a_stockname=a_stock,
                  selected_scenarios=general$namefolderoutput[1:3],
                  the_scenario_names=general$namefolderoutput[1:3], 
                  name_set_of_sces= "setA",
                  selected=selected,
                  export=is_individual_export,
                  a_xlab="# Month",
                  if(a_variable=="Fbar") a_ylab="F" else  a_ylab=a_variable,
                  add_legend=FALSE,
                  color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4),
                              rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
                  the_sims=1:20
                  ) 
   
 
  if(!is_individual_export) mtext("# months", 1, line=2, cex=1.5, outer=TRUE)
  if(!is_individual_export) mtext(side=2,"Indicators",line=2, cex=1.5, outer=TRUE)

  
  
  if(!is_individual_export) dev.off()

  } 
 
  
return()
}  
  
 
  