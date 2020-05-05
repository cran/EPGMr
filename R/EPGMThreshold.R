#' Threshold Evaluation
#'
#' This function evaluates results of the EPGM model over a simulated period relative to ecological or regulatory thresholds.
#'
#' As described in the original documentation, the model is designed to simulate marsh enrichment (responses to increasing P load), not recovery (responses to decreasing in load).
#'
#' @param time.dat a `data.frame`, the raw output from EPGMTime (when `raw.time.output=TRUE`).
#' @param WaterColumn.Thresholds a list of three water column total phosphorus thresholds to evaluate time output.
#' @param Soil.Thresholds a list of three soil total phosphorus thresholds to evaluate time output.
#' @param cattail.Thresholds a list of three cattail density thresholds to evaluate time output.
#' @param plot.exceed If `TRUE` base plot will be generate with water column, soil and cattail area exceeded plots.
#' @param raw.area.output If `TRUE` a `data.frame` will be printed with all area exceedances calculated for each time step.Default is set to `FALSE`.
#' @param results.area.table If `TRUE` a summary results table will be printed in the console. Default is set to `TRUE`.
#' @keywords "water quality"
#' @export
#' @return This function computes and plots the distance profile along the gradient based on input values
#' @examples
#' example<-EPGMTime(case.no=11,raw.time.output=TRUE)
#' EPGMThreshold(example)
#'
#' #END

EPGMThreshold=function(time.dat,
                       WaterColumn.Thresholds=c(10,15,20),
                       Soil.Thresholds=c(500,600,1000),
                       cattail.Thresholds=c(5,20,90),
                       plot.exceed=TRUE,
                       raw.area.output=FALSE,
                       results.area.table=TRUE

){
  if(nrow(time.dat)==0){
    stop("Check 'time.dat',the precious needs data to works.")
  }
  if(sum(names(time.dat)=="time.step")!=1){
    stop("Double check 'time.dat'. did you run EPGMTime first?")
  }
  if(length(WaterColumn.Thresholds)!=3){
    stop("Input up to three thresholds to evaluate modelled water quality.")
  }
  if(length(Soil.Thresholds)!=3){
    stop("Input up to three thresholds to evaluate modelled soil TP.")
  }
  if(length(cattail.Thresholds)!=3){
    stop("Input up to three thresholds to evaluate modelled cattail density.")
  }
  if(raw.area.output==TRUE&results.area.table==TRUE){
    warning("Can't have raw.area.output and results.area.table, You can't have your cake and eat it too.")
    raw.area.output=FALSE
  }
  #Calculate values
  Time.increment.yr=diff(unique(time.dat$time.step))[1]
  Max.Dist=max(time.dat$dist)
  Dist.increment.km=diff(unique(time.dat$dist))[1]
  path.width.km=max(time.dat$area)/max(time.dat$dist)
  start.year=min(time.dat$Year)+1
  Max.Yrs=max(time.dat$time.step)

  cell.area=path.width.km*Dist.increment.km
  total.area=Max.Dist*path.width.km
  wghts<-ifelse(time.dat$dist==0|time.dat$dist==Max.Dist,0.5,1)

  #Threshold Evaluation
  WCthres.1<-WaterColumn.Thresholds[1]
  WCthres.2<-if(length(WaterColumn.Thresholds)>=2){WaterColumn.Thresholds[2]}else{NA}
  WCthres.3<-if(length(WaterColumn.Thresholds)==3){WaterColumn.Thresholds[3]}else{NA}

  WCthres.1.exceed<-aggregate(ifelse(time.dat$TP.Time.ugL>WCthres.1,1,0)*wghts,by=list(time.dat$time.step),"sum")*Dist.increment.km
  WCthres.1.exceed.km<-(WCthres.1.exceed[,2]*path.width.km)
  WCthres.2.exceed<-aggregate(ifelse(time.dat$TP.Time.ugL>WCthres.2,1,0)*wghts,by=list(time.dat$time.step),"sum")*Dist.increment.km
  WCthres.2.exceed.km<-(WCthres.2.exceed[,2]*path.width.km)
  WCthres.3.exceed<-aggregate(ifelse(time.dat$TP.Time.ugL>WCthres.3,1,0)*wghts,by=list(time.dat$time.step),"sum")*Dist.increment.km
  WCthres.3.exceed.km<-(WCthres.3.exceed[,2]*path.width.km)

  Soilthres.1<-Soil.Thresholds[1]
  Soilthres.2<-if(length(Soil.Thresholds)>=2){Soil.Thresholds[2]}else{NA}
  Soilthres.3<-if(length(Soil.Thresholds)==3){Soil.Thresholds[3]}else{NA}

  soilthres.1.exceed<-aggregate(ifelse(time.dat$Avg.SoilP.mgkg>Soilthres.1,1,0)*wghts,by=list(time.dat$time.step),"sum")
  soilthres.1.exceed.km<-(soilthres.1.exceed[,2]*Dist.increment.km)*path.width.km
  soilthres.2.exceed<-aggregate(ifelse(time.dat$Avg.SoilP.mgkg>Soilthres.2,1,0)*wghts,by=list(time.dat$time.step),"sum")
  soilthres.2.exceed.km<-(soilthres.2.exceed[,2]*Dist.increment.km)*path.width.km
  soilthres.3.exceed<-aggregate(ifelse(time.dat$Avg.SoilP.mgkg>Soilthres.3,1,0)*wghts,by=list(time.dat$time.step),"sum")
  soilthres.3.exceed.km<-(soilthres.3.exceed[,2]*Dist.increment.km)*path.width.km

  cattailthres.1<-cattail.Thresholds[1]
  cattailthres.2<-if(length(cattail.Thresholds)>=2){cattail.Thresholds[2]}else{NA}
  cattailthres.3<-if(length(cattail.Thresholds)==3){cattail.Thresholds[3]}else{NA}

  cattailthres.1.exceed<-aggregate(ifelse(time.dat$cattail.time.per>cattailthres.1,1,0)*wghts,by=list(time.dat$time.step),"sum")[,2]*cell.area
  cattailthres.2.exceed<-aggregate(ifelse(time.dat$cattail.time.per>cattailthres.2,1,0)*wghts,by=list(time.dat$time.step),"sum")[,2]*cell.area
  cattailthres.3.exceed<-aggregate(ifelse(time.dat$cattail.time.per>cattailthres.3,1,0)*wghts,by=list(time.dat$time.step),"sum")[,2]*cell.area

  #output table
  tmp<-cbind(WCthres.1.exceed.km,WCthres.2.exceed.km,WCthres.3.exceed.km,
             soilthres.1.exceed.km,soilthres.2.exceed.km,soilthres.3.exceed.km,
             cattailthres.1.exceed,cattailthres.2.exceed,cattailthres.3.exceed)
  colnames(tmp)<-c(paste0("WC.",WaterColumn.Thresholds),paste0("Soil.",Soil.Thresholds),paste0("Cattail.",cattail.Thresholds))

  areas.table<-cbind(data.frame(Time.Yrs=unique(time.dat$time.step),Year=unique(time.dat$Year)),tmp)

  if(results.area.table==TRUE){
  #report table
  rpt.time.step<-if(Time.increment.yr==1){c(0,1,2,3,10,15,20,25,200)}else{c(seq(0,Max.Yrs,Time.increment.yr)[1:8],Max.Yrs)}


  threshold.summary=data.frame(Thresholds=c("Water Column (ug/L)","Soil (mg/kg)","Cattail Density (%)"),
                               Value1=c(WaterColumn.Thresholds[1],Soil.Thresholds[1],cattail.Thresholds[1]),
                               Value2=c(WaterColumn.Thresholds[2],Soil.Thresholds[2],cattail.Thresholds[2]),
                               Value3=c(WaterColumn.Thresholds[3],Soil.Thresholds[3],cattail.Thresholds[3]))
    #watercolumn
  wc.area.summary=cbind(data.frame(Time.Step=rpt.time.step,Year=(start.year+rpt.time.step)-1),
                        areas.table[areas.table$Time.Yrs%in%rpt.time.step,c(paste0("WC.",WaterColumn.Thresholds))])
  #Soil
  soil.area.summary=cbind(data.frame(Time.Step=rpt.time.step,Year=(start.year+rpt.time.step)-1),
                          areas.table[areas.table$Time.Yrs%in%rpt.time.step,c(paste0("Soil.",Soil.Thresholds))])
  #Cattail
  cattail.area.summary=cbind(data.frame(Time.Step=rpt.time.step,Year=(start.year+rpt.time.step)-1),
                             areas.table[areas.table$Time.Yrs%in%rpt.time.step,c(paste0("Cattail.",cattail.Thresholds))])

  out.sum<-list("TotalArea.km2"=total.area,
                "Thresholds" = threshold.summary,
                "WaterColumn"=wc.area.summary,
                "Soil"= soil.area.summary,
                "Cattail"=cattail.area.summary)
  }

  if(plot.exceed==TRUE){
    cols<-colorRampPalette(c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"))(3)

    oldpar<- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    par(family="serif",mar=c(1.75,4,2,2),oma=c(2,1,1,1),mgp=c(2.25,0.5,0));
    layout(matrix(1:6,3,2,byrow=T),widths=c(1,0.5))

    plot(areas.table[,c(paste0("WC.",WaterColumn.Thresholds[1]))]~Time.Yrs,areas.table,type="n",las=1,xlab=NA,ylab=NA,yaxs="i",xaxs="i",xlim=c(0,Max.Yrs),ylim=c(0,ceiling(total.area+total.area*0.1)))
    lines(areas.table$Time.Yrs,areas.table[,c(paste0("WC.",WaterColumn.Thresholds[1]))],col=cols[1],lwd=2)
    lines(areas.table$Time.Yrs,areas.table[,c(paste0("WC.",WaterColumn.Thresholds[2]))],col=cols[2],lwd=2)
    lines(areas.table$Time.Yrs,areas.table[,c(paste0("WC.",WaterColumn.Thresholds[3]))],col=cols[3],lwd=2)
    mtext(side=3,"Water Column")
    plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
    legend(0.5,0.5,legend=c(paste(WaterColumn.Thresholds,"\u03BCg/L")),
           pch=NA,lwd=c(2),lty=c(1),
           col=cols,cex=1,
           pt.cex=1.25,ncol=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title="Water Column TP Thresholds",title.adj = 0)
    plot(areas.table[,c(paste0("Soil.",Soil.Thresholds[1]))]~Time.Yrs,areas.table,type="n",las=1,xlab=NA,ylab=NA,yaxs="i",xaxs="i",xlim=c(0,Max.Yrs),ylim=c(0,ceiling(total.area+total.area*0.1)))
    lines(areas.table$Time.Yrs,areas.table[,c(paste0("Soil.",Soil.Thresholds[1]))],col=cols[1],lwd=2)
    lines(areas.table$Time.Yrs,areas.table[,c(paste0("Soil.",Soil.Thresholds[2]))],col=cols[2],lwd=2)
    lines(areas.table$Time.Yrs,areas.table[,c(paste0("Soil.",Soil.Thresholds[3]))],col=cols[3],lwd=2)
    mtext(side=3,"Soil")
    mtext(side=2,line=2.5,"Area Exceeded (km\u00B2)")
    plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
    legend(0.5,0.5,legend=c(paste(Soil.Thresholds,"mg/kg")),
           pch=NA,lwd=c(2),lty=c(1),
           col=cols,cex=1,
           pt.cex=1.25,ncol=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title="Soil TP Thresholds",title.adj = 0)

    plot(areas.table[,c(paste0("Cattail.",cattail.Thresholds[1]))]~Time.Yrs,areas.table,type="n",las=1,xlab=NA,ylab=NA,yaxs="i",xaxs="i",xlim=c(0,Max.Yrs),ylim=c(0,ceiling(total.area+total.area*0.1)))
    lines(areas.table$Time.Yrs,areas.table[,c(paste0("Cattail.",cattail.Thresholds[1]))],col=cols[1],lwd=2)
    lines(areas.table$Time.Yrs,areas.table[,c(paste0("Cattail.",cattail.Thresholds[2]))],col=cols[2],lwd=2)
    lines(areas.table$Time.Yrs,areas.table[,c(paste0("Cattail.",cattail.Thresholds[3]))],col=cols[3],lwd=2)
    mtext(side=3,"Cattail Density")
    mtext(side=1,line=2,"Time (Years)")
    plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
    legend(0.5,0.5,legend=c(paste(cattail.Thresholds,"%")),
           pch=NA,lwd=c(2),lty=c(1),
           col=cols,cex=1,
           pt.cex=1.25,ncol=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title="Cattail Density Thresholds",title.adj = 0)
  }

  if(results.area.table==TRUE){return(out.sum)}
  if(raw.area.output==TRUE){return(areas.table)}
}
