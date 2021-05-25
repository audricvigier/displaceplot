            
#' Processing the loglike.dat files (maybe better for you to use it on HPC) 
#'
#' This function process the loglike_simu_xxx.dat files 
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' 
#' \dontrun{
#' general <- setGeneralOverallVariable (pathToRawInputs =file.path("C:", "Users", "fbas", 
#'                                                 "Documents", "GitHub", paste0("DISPLACE_input_gis_", 
#'                                                  "DanishFleet")),
#'                                       pathToDisplaceInputs = file.path("C:", "Users", "fbas", 
#'                                                 "Documents", "GitHub", paste0("DISPLACE_input_", "DanishFleet")),
#'                                       pathToOutputs =file.path("C:","DISPLACE_outputs"),
#'                                       caseStudy="DanishFleet",
#'                                       iGraph=41,
#'                                       iYear="2015",
#'                                       iCountry="DEN",
#'                                       nbPops=39,
#'                                       nbSzgroup=14,
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
#'  if(FALSE){
#'  load(file.path(general$main.path.param, "FISHERIES",
#'           paste("logbooks_DNK_","2015",".RData",sep=''))) 
#'  eflalo                           <- cbind.data.frame (logbooks, totland=apply(logbooks[, grep("LE_KG", colnames(logbooks))],  1, sum, na.rm=TRUE )  )
#'  eflalo                           <- cbind(eflalo, ICESrectangle2LonLat(eflalo$LE_RECT))
#'  eflalo$area                      <- ICESarea2 (eflalo[, c("SI_LONG", "SI_LATI")])
#'  bottomfishingmets                <- as.character(unique(eflalo$LE_MET)[ unique(
#'                                             c(grep("OTB", unique(eflalo$LE_MET)),
#'                                               grep("PTB", unique(eflalo$LE_MET)),
#'                                               grep("SDN", unique(eflalo$LE_MET)),
#'                                               grep("PS", unique(eflalo$LE_MET)),
#'                                               grep("SSC", unique(eflalo$LE_MET)),
#'                                               grep("TBB", unique(eflalo$LE_MET)),
#'                                               grep("DRB", unique(eflalo$LE_MET))
#'                                                 )
#'                                            )])  
#'                                            
#'  
#'  vids_all                         <- unique(eflalo[, "VE_REF"])
#'  vids_bottomfishing_baltic        <- unique(eflalo[eflalo$area %in% c("2224", "2532") & eflalo$LE_MET %in% bottomfishingmets, "VE_REF"])
#'  vids_bottomfishing_nseakask      <- unique(eflalo[eflalo$area %in% c("nsea", "kask") & eflalo$LE_MET %in% bottomfishingmets, "VE_REF"])
#'  
#'  selected_vessels_set_1  <-  as.character(unique(vids_all[ vids_all %in%  loglike$VE_REF ]))  
#'  selected_vessels_set_2  <-  as.character(unique(vids_bottomfishing_baltic[ vids_bottomfishing_baltic %in%  vids_bottomfishing_baltic ]))  
#'  selected_vessels_set_3  <-  as.character(unique(vids_bottomfishing_nseakask[ vids_bottomfishing_nseakask %in%  vids_bottomfishing_nseakask ]))   
#'  selected_vessels_set_1  <- selected_vessels_set_1[!is.na(selected_vessels_set_1)]
#'  selected_vessels_set_2  <- selected_vessels_set_2[!is.na(selected_vessels_set_2)]
#' selected_vessels_set_3  <- selected_vessels_set_3[!is.na(selected_vessels_set_3)]
#' 
#'  write.table(selected_vessels_set_1, file.path(general$main.path, general$case_study,
#'           paste("selected_vessels_set_1.dat",sep='')), col.names=FALSE, row.names=FALSE)
#'  write.table(selected_vessels_set_2, file.path(general$main.path, general$case_study,
#'           paste("selected_vessels_set_2.dat",sep='')), col.names=FALSE, row.names=FALSE)
#'  write.table(selected_vessels_set_3, file.path(general$main.path, general$case_study,
#'           paste("selected_vessels_set_3.dat",sep='')), col.names=FALSE, row.names=FALSE)
#'  } else{
#'   selected_vessels_set_1 <- as.character(read.table(file.path(general$main.path, general$case_study,
#'           paste("selected_vessels_set_1.dat",sep='')), header=FALSE)[,1])
#'   selected_vessels_set_2 <-as.character(read.table(file.path(general$main.path, general$case_study,
#'           paste("selected_vessels_set_2.dat",sep='')), header=FALSE)[,1])
#'    selected_vessels_set_3 <- as.character(read.table(file.path(general$main.path, general$case_study,
#'           paste("selected_vessels_set_3.dat",sep='')), header=FALSE)[,1])
#'   
#'  
#'  }
#'  
#'  
#'  save(selected_vessels_set_1, selected_vessels_set_2, selected_vessels_set_3,
#'    file=file.path(general$main.path, general$namefolderinput, 
#'                     paste("selected_vessels.RData", sep='')) )
#'  
#'
#' 
#'
#'   getAggLoglikeFiles(general=general, what="weight",
#'             explicit_pops=c(0, 1, 2, 3, 11, 23, 24, 26, 30, 31, 32),
#'             implicit_pops=c (4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 25, 27, 28, 29, 33, 34, 35, 36, 37, 38),
#'             selected_vessels_set1=selected_vessels_set_1,
#'            selected_vessels_set2=selected_vessels_set_2,
#'             selected_vessels_set3=selected_vessels_set_3)  
#'   }

library(data.table)
library(doBy)
library(sp)
library(DBI)
library(RSQLite)

c.listquote <- function (...)
{
  args <- as.list(match.call()[-1])
  lstquote <- list(as.symbol("list"))
  for (i in args) {
    if (class(i) == "name" || (class(i) == "call" && i[[1]] !=
                               "list")) {
      i <- eval(substitute(i), sys.frame(sys.parent()))
    }
    if (class(i) == "call" && i[[1]] == "list") {
      lstquote <- c(lstquote, as.list(i)[-1])
    }
    else if (class(i) == "character") {
      for (chr in i) {
        lstquote <- c(lstquote, list(parse(text = chr)[[1]]))
      }
    }
    else stop(paste("[", deparse(substitute(i)), "] Unknown class [",
                    class(i), "] or is not a list()", sep = ""))
  }
  return(as.call(lstquote))
}

ICESarea2 <- function (tacsat, string = TRUE)
{
  ICES.area <- rep(NA, dim(tacsat)[1])
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
                                       10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
                                       11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
                             pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
                                       59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653,
                                       57.74247, 57.50441, 57.10708, 57.08073)) > 0] <- ifelse(string, 'kask', '0')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
                                       12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
                                       12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
                                       11.49492, 11.3094, 11.27652, 10.71374, 10.70218,
                                       10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247,
                                                                                          57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
                                                                                          56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
                                                                                          55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
                                                                                          55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
                                                                                          57.11107, 57.63566, 57.74247)) > 0] <-ifelse(string,'kask', '0')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(-4, 7, 8, 7, 7, 7.906163, -4, -4.6, -4.6, -4),
                             pol.y = c(62, 62, 61.5, 60, 58, 57.5, 57.5, 57.3, 58.2,
                                       58.4)) > 0] <- ifelse(string,'nsea', '1')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(7.906163, 8.6488368, 8.5, 10.3, 10.3, 9.2,
                                       9.2, 7.11, -1, -4, -1.78), pol.y = c(57.5, 57.08073,
                                                                            57, 57.3, 57, 56.2, 52.7, 53.5, 53.5, 56.1, 57.5)) >
              0] <- ifelse(string,'nsea', '1')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(0, 0.5, 7.5, 7.5), pol.y = c(53.5, 51, 51,
                                                                    53.5)) > 0] <- ifelse(string,'nsea', '1')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
                                       14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
                                                                            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
                                                                            55.5, 55.38, 55.33)) > 0] <- ifelse(string,'2224', '2')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(14.2, 14.78, 15, 15, 18, 18, 14.2), pol.y = c(55.4,
                                                                                     55.3, 55, 53, 53, 56.5, 56.5)) > 0] <- ifelse(string,'2532', '3')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(18, 18, 22, 22), pol.y = c(56.5, 53.5, 53.5,
                                                                  56.5)) > 0] <- ifelse(string,'2532', '3')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(18, 18, 18.32, 18.32, 19, 19, 16, 16), pol.y = c(56.5,
                                                                                        57, 57, 57.5, 57.925, 59.762, 59.762, 56.5)) > 0] <- ifelse(string,'2532', '3')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(19, 19, 18.45, 18.3, 18, 18, 21.5, 21.72, 21.98,
                                       22.17, 22.24, 21.93), pol.y = c(58.5, 57.9, 57.58,
                                                                       57, 57, 56.5, 56.5, 57.57, 57.97, 58.04, 58.15,
                                                                       58.5)) > 0] <- ifelse(string,'2532', '3')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(21.5, 21.72, 21.98, 22.17, 22.24, 22.24, 23,
                                       25, 25), pol.y = c(56.5, 57.57, 57.97, 58.04, 58.15,
                                                          58.35, 58.5, 58.5, 56.5)) > 0] <- ifelse(string,'2532', '3')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(19, 17.975, 21.6, 21.8, 23.325, 23.325, 23.191,
                                       23, 23, 23.5, 23.6, 24, 23.692, 22.5, 22.1, 21.92,
                                       19), pol.y = c(59.762, 60.5, 60.5, 60.7, 60.5, 59.965,
                                                      59.867, 59.827, 59, 59, 59.05, 58.75, 59.5, 59.5,
                                                      58.35, 58.5, 58.5)) > 0] <- ifelse(string,'2532', '3')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(16.5, 16.5, 19.7, 19.7, 22.6, 21.4), pol.y = c(60.5,
                                                                                      63.7, 63.7, 63.5, 63.5, 60.5)) > 0] <- ifelse(string,'2532', '3')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(19.7, 19.7, 25.7, 25.7, 19.7), pol.y = c(63.7,
                                                                                63.5, 63.5, 67, 67)) > 0] <-ifelse(string,'2532', '3')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(23.325, 23.325, 23.191, 23, 23, 30.5, 30.5),
                             pol.y = c(60.5, 59.965, 59.867, 59.827, 59, 59, 60.5)) >
              0] <- ifelse(string,'2532', '3')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
                                       12.5, 12.448), pol.y = c(56.13, 55.48, 55.31, 55.38,
                                                                55.41, 55.71, 56.29, 56.305)) > 0] <- ifelse(string,'2224', '2')
  ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                             pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
                                       11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
                                                                           55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,
                                                                           56.6)) > 0] <- ifelse(string,'2224', '2')
  return(ICES.area)
}

ICESrectangle <- function (dF) 
{
  rectChar1n2 <- as.integer(2 * (dF[, "SI_LATI"] - 35.5))
  rectChar3 <- ifelse(dF[, "SI_LONG"] <= -40, "A", ifelse(dF[, 
                                                             "SI_LONG"] <= -30, "B", ifelse(dF[, "SI_LONG"] <= -20, 
                                                                                            "C", ifelse(dF[, "SI_LONG"] <= -10, "D", ifelse(dF[, 
                                                                                                                                               "SI_LONG"] <= 0, "E", ifelse(dF[, "SI_LONG"] <= 10, 
                                                                                                                                                                            "F", ifelse(dF[, "SI_LONG"] <= 20, "G", ifelse(dF[, 
                                                                                                                                                                                                                              "SI_LONG"] <= 30, "H", "I"))))))))
  rectChar4 <- as.integer(dF[, "SI_LONG"]%%10)
  rectID <- paste(rectChar1n2, rectChar3, rectChar4, sep = "")
  return(rectID)
}

ICESrectangle2LonLat <- function (statsq, midpoint = F) {
  part1 <- substr(statsq, 1, 2)
  part2 <- substr(statsq, 3, 4)
  labels <- 0:90
  latlabels <- ifelse(labels < 10, paste("0", labels, sep = ""),
                      as.character(labels))
  latvalues <- seq(35.5, 80.5, 0.5) + 0.25
  lonlabels <- paste(rep(LETTERS[2:8], rep(10, 7)), rep(0:9,
                                                        7), sep = "")
  lonvalues <- (-40:29) + 0.5
  indx <- match(part1, latlabels)
  lat <- latvalues[indx]
  indx <- match(part2, lonlabels)
  lon <- lonvalues[indx]
  if (any(is.na(lat)) | any(is.na(lon)))
    warning("Some stat squares have not been recognised.")
  if (midpoint == F) {
    lat <- lat - 0.25
    lon <- lon - 0.5
  }
  return(data.frame(SI_LATI = lat, SI_LONG = lon))
}

loadGraph <- function(){
  # load the graph
  #load(file.path(general$main.path.igraph, paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
  coord <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
                                     paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  colnames(coord) <- c('x', 'y', 'harb')
  #plot(coord[,1], coord[,2])
  
  graph <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
                                     paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
  graph <- as.matrix(as.vector(graph))
  graph <- matrix(graph, ncol=3)
  segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R
  
  return(graph)
}

loadCoord <- function(){
  # load the graph
  #load(file.path(general$main.path.igraph, paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
  coord <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
                                     paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  colnames(coord) <- c('x', 'y', 'harb')
  #plot(coord[,1], coord[,2])
  
  graph <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
                                     paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
  graph <- as.matrix(as.vector(graph))
  graph <- matrix(graph, ncol=3)
  segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R
  
  return(coord)
}

##-------------
## agg utils ##
##-------------
aggregateLoglike <- function(loglike, agg_by=c("year.month"), what="weight",explicit_pops=explicit_pops){
  nm <- names(loglike)
  idx.col.c   <- grep('pop.', nm)
  idx.col.e   <- grep('effort', nm)
  idx.col.s   <- grep('cumsteaming', nm)
  idx.col.t   <- grep('nbtrip', nm)
  idx.col.b   <- grep('bwtrip', nm)
  idx.col.f   <- grep('fuelcons', nm)
  idx.col.r   <- grep('revenue', nm, fixed = TRUE)
  idx.col.sa   <- grep('sweptr', nm, fixed = TRUE)
  idx.col.rsa <- grep('revpersweptarea', nm, fixed = TRUE)
  idx.col.ra  <- grep('rev_from_av_prices', nm)
  idx.col.fc  <- grep('fuelcost', nm)
  idx.col.g   <- grep('gav', nm, fixed = TRUE)
  idx.col.g2  <- grep('gradva', nm)
  idx.col.l   <-  which(nm== 'totland')
  idx.col.l2  <- which(nm== 'totland_explicit')
  idx.col.l3  <- which(nm== 'totland_implicit')
  idx.col.pops  <- which(nm %in% paste('pop.', explicit_pops, sep=''))
  idx.col.g3  <- which(nm== 'rev_explicit_from_av_prices') 
  idx.col <- c(idx.col.c, idx.col.e, idx.col.s, idx.col.t, idx.col.b, idx.col.f, idx.col.r, 
               idx.col.ra,  idx.col.sa, idx.col.rsa, idx.col.fc, idx.col.g,  idx.col.g2, idx.col.l, idx.col.l2, idx.col.l3,
               idx.col.pops, idx.col.g3)
  colnames(loglike)[idx.col] # check             
  DT  <- data.table(loglike) # library data.table for fast grouping replacing aggregate()
  # AGGREGATE PER SPECIES -----> SUM (IF WEIGHT) OR MEAN (IF CPUE)
  a_mean <- function(x, na.rm) mean(x[x!=0], na.rm=na.rm) # modify the mean() so that 0 are first removed....
  if(what=="weight") eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
  if(what=="cpue") eq1  <- c.listquote( paste ("a_mean(",nm[idx.col],",na.rm=TRUE)",sep="") )
  a_by <- c.listquote(  agg_by ) 
  loglike.agg <- DT[,eval(eq1), by= eval(a_by)]
  loglike.agg <- data.frame( loglike.agg)
  colnames(loglike.agg) <- c(agg_by, colnames(loglike)[idx.col] )
  loglike.agg <- orderBy (as.formula(paste("~ ", paste(agg_by, collapse="+"))), data=loglike.agg) # order to ensure same order when collating....
  # AGGREGATE PER SPECIES -----> MEAN
  idx.col.e  <- grep('effort', nm)
  idx.col.b  <- grep('bwtrip', nm)
  idx.col.t  <- grep('traveled_dist', nm)
  idx.col.v  <- grep('vpuf', nm)
  idx.col.v2  <- grep('vapuf', nm)
  idx.col.disc_rate  <- which(nm %in% paste('disc_rate_', explicit_pops, sep=''))
  idx.col <- c(idx.col.e, idx.col.b, idx.col.t,  idx.col.v, idx.col.v2, idx.col.disc_rate)
  eq1  <- c.listquote( paste ("mean(",nm[idx.col],",na.rm=TRUE)",sep="") )
  a_by <- c.listquote(  agg_by ) 
  loglike.agg2 <- DT[,eval(eq1),by=eval(a_by)]
  loglike.agg2 <- data.frame( loglike.agg2)
  some_col_names           <- colnames(loglike)[idx.col]
  some_col_names_redundant <-  some_col_names [some_col_names %in%  colnames(loglike.agg[,-c(1:length(agg_by))])] 
  some_col_names [some_col_names %in% some_col_names_redundant] <- paste0("av_",some_col_names_redundant)  # a fix
  colnames(loglike.agg2)   <- c(agg_by, some_col_names )
  loglike.agg2 <- orderBy (as.formula(paste("~ ", paste(agg_by, collapse="+"))), data=loglike.agg2) # order to ensure same order when collating....
  # collate
  loglike.agg <- cbind(loglike.agg, loglike.agg2[,-c(1:length(agg_by))])
  
  # av_vpuf per trip can be biased by large outliers, so instead look at:
  loglike.agg$av_vapuf_month <- loglike.agg$rev_from_av_prices /  loglike.agg$fuelcons
  
  return(loglike.agg)
}
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!LOGLIKE.DAT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 getAggLoglikeFiles <- function(general=general, 
                                              what="weight", # CPUE or weight
                                              explicit_pops=0:26,
                                              implicit_pops=NULL,
                                              selected_vessels_set1=NULL,
                                              selected_vessels_set2=NULL,
                                              selected_vessels_set3=NULL
                                              ){
 
  #--------------------------------
  explicit_pops_params <- explicit_pops   # save
  implicit_pops_params <- implicit_pops   # save
  
  ## tstep
   t.seq <- seq(as.POSIXct(paste(general$a.year,"-01-01 00:00:00",sep='')),
        as.POSIXct(paste(general$a.yearEnd,"-12-31 00:00:00",sep='')), by="hours")
 
   for (sce in general$namefolderoutput){
     lst_loggetAggLoglilike_agg_all <- list()
     lst_loglike_agg_den <- list()
     lst_loglike_agg_selected_set1 <- list()
     lst_loglike_agg_selected_set2 <- list()
     lst_loglike_agg_selected_set3 <- list()
     lst_loglike_agg_deu <- list()
     lst_loglike_agg_swe <- list()
     lst_loglike_agg_ita <- list()
     lst_loglike_agg_vid <- list()
     lst_loglike_agg_vid_port <- list()
     lst_loglike_agg_met <- list()
     lst_loglike_agg_all <- list()
   
  
     explicit_pops <- explicit_pops_params
     implicit_pops <- implicit_pops_params
  
  
   
     for (sim in general$namesimu[[sce]]){


       if(!general$use_sqlite){
         ## robust read for the 'loglike' output
         er <- try(   {
           filename <- file.path(general$main.path, general$namefolderinput,  sce, 
                                 paste("loglike_", sim, ".dat", sep=''))
           
           #loglike <- read.table(file=filename)
           # because not same number of elements per line, replaced by:
           
           cnts<- count.fields(filename) 
           lf<-readLines(filename)
           lf<-cbind(lf,cnts)
           print(nrow(lf))
           most_freq_nb_of_field <- names(table(cnts)) [table(cnts)==max(table(cnts))]
           #!#!#!#!#!#!#!#!#!
           lf <- lf[lf[,2]==most_freq_nb_of_field,][,-2] # filter out if != most_freq_nb_of_field
           #!#!#!#!#!#!#!#!#!
           print(length(lf))
           # Write data out and read it back in (temporarily)
           filename2 <-  file.path(general$main.path, general$namefolderinput,  sce, 
                                   paste("loglike_", sim, "_corr", ".dat", sep=''))
           write(lf,file=paste(filename2))
           loglike <-read.table(filename2)
           gc(reset=T)
           
           
         }, silent=TRUE)
         
       } else{
         # Use sqlite
         con <- dbConnect(RSQLite::SQLite(), file.path(general$main.path, general$namefolderinput,  sce, 
                                                       paste(general$namefolderinput, "_", sim, "_out", ".db", sep='')))
         head(dbReadTable(con, "VesselLogLike"))
         head(dbReadTable(con, "VesselDef"))
         head(dbReadTable(con, "VesselLogLikeCatches"))
         
         res <- dbSendQuery(con, "SELECT TStep,SUM(Catches) FROM VesselLogLike JOIN VesselDef ON Id = VesselId JOIN VesselLogLikeCatches ON RowId = LoglikeId WHERE Nationality = 'DNK' GROUP BY TStep")
         dbFetch(res, n= -1)
         dbClearResult(res)
         
         res <- dbSendQuery(con, "SELECT TStep,AVG(vpuf) FROM VesselLogLike JOIN VesselDef ON Id = VesselId WHERE Nationality = 'DNK' GROUP BY TStep")
         dd <- dbFetch(res, n= -1)
         dbClearResult(res)
         
         res <- dbSendQuery(con, "SELECT TStep,SUM(gav) FROM VesselLogLike JOIN VesselDef ON Id = VesselId WHERE Nationality = 'DNK' GROUP BY TStep")
         dd <- dbFetch(res, n= -1)
         dbClearResult(res)
         sum(dd[,2])
         
         #plot(dd)
         dd$runningaverage <- cumsum(dd[,2])/(1:length(dd[,2]))
         #plot(dd[,1], dd$runningaverage)
         
         
         # todo....
       }

       
       if(class(er)!="try-error"){                                       
         
         
         colnames (loglike) <- c('tstep_dep', 'tstep_arr', 'reason_back','cumsteaming', 'idx_node',  'idx_vessel', 'VE_REF', 'timeatsea', 'fuelcons', 'traveled_dist',  paste('pop.', 0:(general$nbpops-1), sep=''), "freq_metiers", "revenue", "rev_from_av_prices", "rev_explicit_from_av_prices", "fuelcost", "vpuf", "gav", "gradva","sweptr", "revpersweptarea",  paste('disc_',  explicit_pops, sep=''), "GVA", "GVAPerRevenue", "LabourSurplus", "GrossProfit", "NetProfit",  "NetProfitMargin", "GVAPerFTE", "RoFTA", "BER", "CRBER", "NetPresentValue", "numTrips")   
         
         # discard rate (caution: bi-modal where a lots of 1 i.e. all discarded!) 
         for (a_st in explicit_pops){
           loglike[, paste("disc_rate_", a_st, sep="")]   <-  loglike   [, paste("disc_", a_st, sep="")]  / (loglike   [, paste("disc_", a_st, sep="")]  + loglike [, paste("pop.", a_st, sep="")]) 
         }
         
         # add an energy efficiency calculation
         loglike$vpuf <- as.numeric(as.character(loglike$revenue))/ 
           as.numeric(as.character(loglike$fuelcons)) #value per unit of fuel in euro per litre
         loglike$vapuf <- as.numeric(as.character(loglike$rev_from_av_prices))/ 
           as.numeric(as.character(loglike$fuelcons)) #value per unit of fuel in euro per litre
         
         ##...and get back the time info from this...
         loglike$time       <-   t.seq[as.numeric(as.character(loglike$tstep_arr))]
         loglike$month      <-   format(strptime(  loglike$time , tz='GMT',  "%Y-%m-%e %H:%M:%S"  ), "%m")
         loglike$year       <-   format(strptime(  loglike$time , tz='GMT',  "%Y-%m-%e %H:%M:%S"  ), "%Y")
         loglike$year.month <-   paste(loglike$year, '.', loglike$month, sep='')
         #!#!#!#!#!#
         ##=> caution: we are taking the time at the ARRIVAL date to assign a month...
         #(TO DO: better to assign repsective part to each month)
         # so possibly the physical limit (around 600 hours) can be overshoot 
         # if a given trip of the last month end up to the current month(it is just an artefact then...)
         #!#!#!#!#!#
         
         print("reading loglike----OK")
         
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
         
         #coord                <- cbind(coord, idx=0:(nrow(coord)-1))
         #head(coord[coord[,"harb"]==303,])
         loglike$land_port    <- port_names[   coord[loglike$idx_node +1 , "harb"]   , 'port']
         # => caution to this +1 because offset by 1 between loglike and coord row index!
         loglike$ld_port_x    <- coord[loglike$idx_node +1, "x"]  
         loglike$ld_port_y    <- coord[loglike$idx_node +1, "y"]  
         
         print("ports----OK")
         
         ## sort per vessel and compute effort
         loglike        <- orderBy(~VE_REF+tstep_dep, data=loglike)
         loglike$effort <- loglike$tstep_arr - loglike$tstep_dep # trip duration in hours because hourly time step...
         
         if(what=="cpue"){
           loglike$feffort <- (loglike$tstep_arr - loglike$tstep_dep) -loglike$cumsteaming
           loglike[,grep("pop.", colnames(loglike))] <-  loglike[,grep("pop.", colnames(loglike))] / loglike$feffort
           loglike <- loglike[,-grep("feffort", colnames(loglike))] # remove the col now it has been used.
         }
         
         ## add the total landings
         nm <- colnames(loglike)
         idx.col.c <- grep('pop.', nm)
         loglike$totland <- apply(loglike[,idx.col.c], 1, sum, na.rm=TRUE)
         
         ## add the total landings explicit
         nm <- colnames(loglike)
         #idx.col.c <- grep('pop.', nm)
         loglike$totland_explicit <- apply(loglike[, paste("pop.", explicit_pops, sep='')], 1, sum, na.rm=TRUE)
         if(!is.null(implicit_pops)) {
           loglike$totland_implicit <- apply(loglike[, paste("pop.", implicit_pops, sep='')], 1, sum, na.rm=TRUE)
         } else{
           loglike$totland_implicit <- 0
         }
         
         
         ##=> note that we have now the duration of each trip,
         ## and the duration between trips (i.e. stay on quayside) is given by time between two successive records
         ## and nb of trips per month can also be deduced here......
         loglike$nbtrip <- 1
         loglike$bwtrip <- 0
         loglike$bwtrip[1:(nrow(loglike)-1)] <- 
           loglike$tstep_dep[2:nrow(loglike)] - loglike$tstep_arr[1:(nrow(loglike)-1)] # trip duration in hours because hourly time step...
         loglike[diff(as.numeric(loglike$VE_REF))==1, "bwtrip"] <- 0 # correct when change of vid
         
         
         # get metiers
         loglike$metier = NULL
         if(general$case_study=="CelticSea") {
           loglike$metier = sapply(as.character(loglike$freq_metiers), function(x) strsplit(x, split="\\(")[[1]][2])
           loglike$metier = as.numeric(sapply(loglike$metier , function(x) strsplit(x, split="\\)")[[1]][1]))
         }
         
         ##-------------
         ## aggregate by year.month ALL METIERS
         loglike.agg <-  aggregateLoglike(loglike, agg_by=c("year.month"),explicit_pops=explicit_pops)
         
         ## aggregate by year.month PER METIER
         if(general$case_study=="CelticSea") loglike.agg.met <-  aggregateLoglike(loglike, agg_by=c("metier","year.month"),explicit_pops=explicit_pops)
         
         # split per country
         # loglike.den     <- loglike[grep('DNK',loglike$VE_REF),]  # caution 
         # loglike.agg.den <-  aggregateLoglike(loglike.den, agg_by=c("year.month"))
         
         
         # split per selected set of vessels
         if(!is.null(selected_vessels_set1)){
           loglike_selected_set1        <- loglike[loglike$VE_REF %in% selected_vessels_set1,]  # caution
           loglike_selected_set1$VE_REF <- factor(loglike_selected_set1$VE_REF)
           loglike.agg.selected.set1    <-  aggregateLoglike(loglike_selected_set1, agg_by=c("year.month"),explicit_pops=explicit_pops)
         }
         
         # split per selected set of vessels
         if(!is.null(selected_vessels_set2)){
           loglike_selected_set2        <- loglike[loglike$VE_REF %in% selected_vessels_set2,]  # caution
           loglike_selected_set2$VE_REF <- factor(loglike_selected_set2$VE_REF)
           loglike.agg.selected.set2    <-  aggregateLoglike(loglike_selected_set2, agg_by=c("year.month"),explicit_pops=explicit_pops)
         }
         
         # split per selected set of vessels
         if(!is.null(selected_vessels_set3)){
           loglike_selected_set3        <- loglike[loglike$VE_REF %in% selected_vessels_set3,]  # caution
           loglike_selected_set3$VE_REF <- factor(loglike_selected_set3$VE_REF)
           loglike.agg.selected.set3    <-  aggregateLoglike(loglike_selected_set3, agg_by=c("year.month"),explicit_pops=explicit_pops)
         }
         
         
         ## aggregate by year.month, PER VESSEL/metier
         if(general$case_study=="CelticSea"){
           loglike.agg.vid <-  aggregateLoglike(loglike, agg_by=c( "VE_REF","metier","year.month"),explicit_pops=explicit_pops)
         }else{
           loglike.agg.vid <-  aggregateLoglike(loglike, agg_by=c( "VE_REF","year.month"),explicit_pops=explicit_pops)
         }
         
         
         # CONTINGENCY TABLE -----> SPECIAL CASE FOR REASON_BACK     
         loglike$VE_REF     <- factor(loglike$VE_REF)
         loglike.agg3.vid   <- data.frame(xtabs(~year.month+VE_REF+reason_back, data=loglike))
         loglike.agg3.vid   <- orderBy (~year.month+VE_REF, data=loglike.agg3.vid)
         
         loglike.agg3.vid_1 <- loglike.agg3.vid[loglike.agg3.vid$reason_back=="1",]
         
         # print(sce)
         # print(head(loglike.agg3.vid))
         # print(dim(loglike.agg3.vid ))
         # print(dim(loglike.agg3.vid_1 ))
         # print(head(loglike.agg3.vid_1$year.month))
         # print(head(loglike.agg3.vid_1$VE_REF))
         # print(dim(paste (loglike.agg3.vid_1$year.month,".",loglike.agg3.vid_1$VE_REF, sep='')))
         
         loglike.agg3.vid_2 <- loglike.agg3.vid[loglike.agg3.vid$reason_back=="2",]
         loglike.agg3.vid_3 <- loglike.agg3.vid[loglike.agg3.vid$reason_back=="3",]
         if(nrow(loglike.agg3.vid_1)>0) rownames(loglike.agg3.vid_1) <- paste (loglike.agg3.vid_1$year.month,".",loglike.agg3.vid_1$VE_REF, sep='')
         if(nrow(loglike.agg3.vid_2)>0) rownames(loglike.agg3.vid_2) <- paste (loglike.agg3.vid_2$year.month,".",loglike.agg3.vid_2$VE_REF, sep='')
         if(nrow(loglike.agg3.vid_3)>0) rownames(loglike.agg3.vid_3) <- paste (loglike.agg3.vid_3$year.month,".",loglike.agg3.vid_3$VE_REF, sep='')
         if(nrow(loglike.agg3.vid)>0)rownames(loglike.agg.vid) <- paste (loglike.agg.vid$year.month,".",loglike.agg.vid$VE_REF, sep='')
         # collate
         if(nrow(loglike.agg3.vid)>0) loglike.agg.vid <- cbind(loglike.agg.vid, 
                                                               reason_1= loglike.agg3.vid_1[rownames(loglike.agg.vid),c(4)],
                                                               reason_2= loglike.agg3.vid_2[rownames(loglike.agg.vid),c(4)],
                                                               reason_3= loglike.agg3.vid_3[rownames(loglike.agg.vid),c(4)] )
         
         ## aggregate by year.month, PER VESSEL AND PORT
         loglike.agg.vid.port <-  aggregateLoglike(loglike, agg_by=c("VE_REF", "year.month", "land_port","ld_port_x", "ld_port_y"),explicit_pops=explicit_pops)
         
         
         #loglike.agg3.vid.port_0 <- loglike.agg3.vid.port[loglike.agg3.vid.port$reason_back=="0",]
         #loglike.agg3.vid.port_1 <- loglike.agg3.vid.port[loglike.agg3.vid.port$reason_back=="1",]
         #loglike.agg3.vid.port_2 <- loglike.agg3.vid.port[loglike.agg3.vid.port$reason_back=="2",]
         #loglike.agg3.vid.port_3 <- loglike.agg3.vid.port[loglike.agg3.vid.port$reason_back=="3",]
         #rownames(loglike.agg3.vid.port_0) <- paste (loglike.agg3.vid.port_0$year.month,".",loglike.agg3.vid.port_0$VE_REF,".",loglike.agg3.vid.port_0$land_port, sep='')
         #rownames(loglike.agg3.vid.port_1) <- paste (loglike.agg3.vid.port_1$year.month,".",loglike.agg3.vid.port_1$VE_REF,".",loglike.agg3.vid.port_1$land_port, sep='')
         #rownames(loglike.agg3.vid.port_2) <- paste (loglike.agg3.vid.port_2$year.month,".",loglike.agg3.vid.port_2$VE_REF,".",loglike.agg3.vid.port_2$land_port, sep='')
         #rownames(loglike.agg3.vid.port_3) <- paste (loglike.agg3.vid.port_3$year.month,".",loglike.agg3.vid.port_3$VE_REF,".",loglike.agg3.vid.port_3$land_port, sep='')
         #rownames(loglike.agg.vid.port) <- paste (loglike.agg.vid.port$year.month,".",loglike.agg.vid.port$VE_REF,".",loglike.agg.vid.port$land_port, sep='')
         # collate
         
         #loglike.agg.vid.port <- cbind(loglike.agg.vid.port, loglike.agg2.vid.port[,-(1:5)] #,  
         #                   reason_0= loglike.agg3.vid.port_0[rownames(loglike.agg.vid.port),c(4)],
         #                     reason_1= loglike.agg3.vid.port_1[rownames(loglike.agg.vid.port),c(4)],
         #                      reason_2= loglike.agg3.vid.port_2[rownames(loglike.agg.vid.port),c(4)],
         #                       reason_3= loglike.agg3.vid.port_3[rownames(loglike.agg.vid.port),c(4)] 
         #                            )
         
         
         
         
         
         
         
         print("Aggregations from loglike----OK")
         
         
         # if(.Platform$OS.type == "windows") {
         #   ## plot landings per species
         #   X11()
         #   matplot( loglike.agg[,paste('pop.', 0:(general$nbpops-1), sep='')]/1e3, ylim=c(1,1e4), type="b", main="DEN simulated landings per species",
         #            pch=as.character(1:general$nbpops), xlab="Month", ylab="Landings [tons]", lwd=2, lty=1)
         #   
         #   ## plot total effort
         #   X11()
         #   matplot( loglike.agg[,'effort'], ylim=c(1,max(loglike.agg[,'effort'])), type="b", main="simulated total effort DEN",
         #            xlab="Month", ylab="Effort [hours]", lwd=2, lty=1)
         #   
         #   graphics.off()
         # } # end windows
         
         lst_loglike_agg_all[[sim]] <- loglike.agg
         if(!is.null(selected_vessels_set1)){ lst_loglike_agg_selected_set1[[sim]] <- loglike.agg.selected.set1  }
         if(!is.null(selected_vessels_set2)){ lst_loglike_agg_selected_set2[[sim]] <- loglike.agg.selected.set2  }
         if(!is.null(selected_vessels_set3)){ lst_loglike_agg_selected_set3[[sim]] <- loglike.agg.selected.set3  }
         # if('GER' %in% general$a.country){ lst_loglike_agg_deu[[sim]] <- loglike.agg.deu }
         # if('SWN' %in% general$a.country){ lst_loglike_agg_swe[[sim]] <- loglike.agg.swe }
         # if('DEN' %in% general$a.country){ lst_loglike_agg_den[[sim]] <- loglike.agg.den }
         # if('ITA' %in% general$a.country){ lst_loglike_agg_ita[[sim]] <- loglike.agg.ita }
         lst_loglike_agg_vid[[sim]] <- loglike.agg.vid
         lst_loglike_agg_vid_port[[sim]] <- loglike.agg.vid.port
         if(general$case_study=="CelticSea")  lst_loglike_agg_met[[sim]] <- loglike.agg.met
         
         
         cat(paste(sce,"...OK\n"))
         cat(paste(sim,"...OK\n"))
       } else{
         cat(paste("error detected for this", sim, ":remove from the list of simus"))
         general$namesimu[[sce]] <- general$namesimu[[sce]][!general$namesimu[[sce]] %in% sim]
       }
       
       
                     
 } # end for sim
 
 assign(paste("lst_loglike_agg_",what,"_all_", sce, sep=''), lst_loglike_agg_all)
 if(!is.null(selected_vessels_set1)){ assign(paste("lst_loglike_agg_",what,"_selected_set1_", sce, sep=''), lst_loglike_agg_selected_set1)  }
 if(!is.null(selected_vessels_set2)){ assign(paste("lst_loglike_agg_",what,"_selected_set2_", sce, sep=''), lst_loglike_agg_selected_set2)  }
 if(!is.null(selected_vessels_set3)){ assign(paste("lst_loglike_agg_",what,"_selected_set3_", sce, sep=''), lst_loglike_agg_selected_set3)  }
 # if('GER' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_deu_", sce, sep=''), lst_loglike_agg_deu) }
 # if('SWN' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_swe_", sce, sep=''), lst_loglike_agg_swe) }
 # if('DEN' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_den_", sce, sep=''), lst_loglike_agg_den) }
 # if('ITA' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_ita_", sce, sep=''), lst_loglike_agg_ita) }
 assign(paste("lst_loglike_agg_",what,"_vid_", sce, sep=''), lst_loglike_agg_vid)
 assign(paste("lst_loglike_agg_",what,"_vid_port_", sce, sep=''), lst_loglike_agg_vid_port)
 if(general$case_study=="CelticSea") assign(paste("lst_loglike_agg_",what,"_met_", sce, sep=''), lst_loglike_agg_met)

 
 # save for later use....
 save(list=c(paste("lst_loglike_agg_",what,"_all_", sce, sep=''),
       
       # if('GER' %in% general$a.country){paste("lst_loglike_agg_",what,"_ita_", sce, sep='')},
       # if('DEN' %in% general$a.country){paste("lst_loglike_agg_",what,"_den_", sce, sep='')},
       # if('DEU' %in% general$a.country){paste("lst_loglike_agg_",what,"_deu_", sce, sep='')},
       # if('SWE' %in% general$a.country){paste("lst_loglike_agg_",what,"_swe_", sce, sep='')},
       # if('ITA' %in% general$a.country){paste("lst_loglike_agg_",what,"_ita_", sce, sep='')},
         
       if(!is.null(selected_vessels_set1)){ paste("lst_loglike_agg_",what,"_selected_set1_", sce, sep='')},
       if(!is.null(selected_vessels_set2)){ paste("lst_loglike_agg_",what,"_selected_set2_", sce, sep='')},
       if(!is.null(selected_vessels_set3)){ paste("lst_loglike_agg_",what,"_selected_set3_", sce, sep='')},
         
          paste("lst_loglike_agg_",what,"_vid_", sce, sep=''),
          paste("lst_loglike_agg_",what,"_vid_port_", sce, sep=''),
       if(general$case_study=="CelticSea") {paste("lst_loglike_agg_",what,"_met_", sce, sep='')}),
         
          file=file.path(general$main.path, general$namefolderinput,
                 sce, paste("lst_loglike_",what,"_agg_", sce,".RData", sep='') )  )

 

 } # end for sce

 
 
return()
}






