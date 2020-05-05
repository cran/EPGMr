#' Time Profile
#'
#' This function runs the EPGM model over a simulated period. The model is based primarily upon data collected in the early 1990's along the phosphorus gradient in WCA-2A. Substantial additional data collected since then in WCA-2A and other locations indicate a need to recalibrate the model and potentially revise its structure. Recent data suggest, for example, that the relationship between cattail density and soil P needs recalibration and that actual soil P thresholds for biological impacts are probably lower than reflected in the original calibrations.  There are also issues relating to interpretation of and potential anomalies in the historical soil P calibration data attributed to variations in soil core collection method and definition of the soil/water interface (inclusion vs. exclusion of floc layer). There are also indications in the recent data of biologically-mediated vertical transport and/or mixing that are not reflected in the current model structure.
#'
#' As described in the original documentation, the model is designed to simulate marsh enrichment (responses to increasing P load), not recovery (responses to decreasing in load).
#'
#'
#' @param case.no Case number from the pre-loaded example data (values ranges from 1 to 12)
#' @param Start.Discharge The year of discharge started
#' @param Start.Discharge The year which this particular STA began discharge operations.
#' @param STA.outflow.TPconc Outflow total phosphorus concentration (in ug L-1; micrograms per liter) for this STA.
#' @param STA.outflow.vol Annual outflow discharge volume (in x1000 Acre-Feet Year-1) for this STA.
#' @param FlowPath.width The width of the downstream flow path (in kilometers).
#' @param Hydroperiod Average hydroperiod (time above ground surface) of the downstream system (in percent).
#' @param Soil.Depth Depth of soil (in centimeters).
#' @param Soil.BulkDensity.initial The initial bulk density prior to dicharge of the soil downstream of the system (in g cm-3).
#' @param Soil.TPConc.initial The initial total phosphorus concentration of soil prior to discharge downstream of the system (in mg kg-1).
#' @param Vertical.SoilTPGradient.initial The soil total phosphorus concentration gradient prior to dischage downstream of the system (in mg cm-3 cm-1).
#' @param Soil.BulkDensity.final The final bulk density after dischage of the soil downstream of the system (in g cm-3).
#' @param PSettlingRate The phosphorus settling rate estimated from steady-state conditions (m Year-1).
#' @param P.AtmoDep Phosphorus atmospheric depostition loading rate (in mg m-2 Year-1).
#' @param Rainfall Annual accumulated rainfall estimate (m Year-1).
#' @param ET Annual evapotranspiration estimate (m Year-1).
#' @param Dist.Display Output display result for this distance
#' @param Dist.slice A list of distances to disply parameters in a time series plot if `plot.profile` is `TRUE`.
#' @param Max.Yrs Maximum number of years simulated
#' @param Max.Dist Maximum ditance plotted, default is 50 km
#' @param Time.increment.yr Year increment to be modeled
#' @param Dist.increment.km Distance increment modeled
#' @param plot.profile If `TRUE` base plot will be generate with water column distance, soil distance and cattail distance profiles.
#' @param raw.time.output If `TRUE` a `data.frame` will be printed with all calculations used to estimate various parameters.Default is set to `FALSE`.
#' @param results.time.table If `TRUE` a summary results table will be printed in the console. Default is set to `TRUE`.
#' @keywords "water quality"
#' @export
#' @return This function computes and plots the distance profile along the gradient based on input values
#' @examples
#' EPGMTime(case.no=11)

EPGMTime=function(case.no=NA,
                  Start.Discharge=NA,
                  STA.outflow.TPconc=NA,
                  STA.outflow.vol=NA,
                  FlowPath.width=NA,
                  Hydroperiod=NA,
                  Soil.Depth=NA,
                  Soil.BulkDensity.initial=NA,
                  Soil.TPConc.initial=NA,
                  Vertical.SoilTPGradient.initial=NA,
                  Soil.BulkDensity.final=NA,
                  PSettlingRate=NA,
                  P.AtmoDep=NA,
                  Rainfall=NA,
                  ET=NA,
                  Dist.Display=12,
                  Dist.slice=c(0,0.5,1,2,5,10),
                  Max.Yrs=200,
                  Max.Dist=15,
                  Time.increment.yr=5,
                  Dist.increment.km=0.1,
                  plot.profile=TRUE,
                  raw.time.output=FALSE,
                  results.time.table=TRUE

){

  ## Stop/warning section of the function
  input.val.na<-sum(c(is.na(Start.Discharge),is.na(STA.outflow.TPconc),is.na(STA.outflow.vol),is.na(FlowPath.width),
                      is.na(Hydroperiod),is.na(Soil.Depth),is.na(Soil.BulkDensity.initial),is.na(Soil.TPConc.initial),
                      is.na(Vertical.SoilTPGradient.initial),is.na(Soil.BulkDensity.final),is.na(PSettlingRate),
                      is.na(P.AtmoDep),is.na(ET)))
  if(is.na(case.no)==TRUE & input.val.na>1){
    stop("Missing inputs, either input a 'case.no' or all individual model parameters.")
  }
  if(is.na(case.no)==FALSE & case.no>12){
    stop("'case.no' range from 1 to 12.")
  }
  if(Dist.increment.km>Max.Dist){
    stop("Distance increment is greater than the maximum gradient distance.")
  }

  ## Data handling
  if(is.na(case.no)==F){
    cases.dat<-data.frame(case.number=1:12,
                        STA.Name=c("STA2","STA34","STA5","STA6","STA2",'STA34',"STA5","STA6","STA2GDR","STA2GDR","S10s","S10s"),
                        Receiving.Area=c("NW 2A","NE 3A","Rotenb","NW 3A","NW 2A","NE 3A","Rotenb","NW 3A","NW 2A","NW 2A","NE 2A","NE 2A"),
                        Start.Discharge=c(1999,2003,1999,1999,1999,2003,1999,1999,1999,1999,1962,1962),
                        STA.outflow.TPconc=c(50,50,100,50,50,50,50,50,40,40,122,122),
                        STA.outflow.vol=c(205.8,422.0,60.0,64.4,205.8,422.0,30.7,64.4,246.7,246.7,281.3,281.3),
                        FlowPath.width=c(12.1,14.2,3,6,12.1,14.2,3,6,12.1,12.1,10.5,10.5),
                        Hydroperiod=c(90,88,69,61,92,88,69,61,92,92,91.4,91.4),
                        Soil.Depth=c(10,10,10,10,20,20,20,20,10,20,10,20),
                        Soil.BulkDensity.initial=c(0.080,0.179,0.197,0.222,0.071,0.176,0.197,0.232,0.080,0.071,0.102,0.102),
                        Soil.TPConc.initial=c(500,463,508,467,366,358,376,330,442,366,198,198),
                        Vertical.SoilTPGradient.initial=c(-0.0018,-0.0039,-0.0052,-0.0054,-0.0018,-0.0039,-0.0052,-0.0054,-0.0018,-0.0018,-0.0018,-0.0018),
                        Soil.BulkDensity.final=c(0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08,0.08),
                        PSettlingRate=c(10.2,10.2,10.2,10.2,10.2,10.2,10.2,10.2,10.2,10.2,10.2,10.2),
                        P.AtmoDep=c(42.9,42.9,42.9,42.9,42.9,42.9,42.9,42.9,42.9,42.9,42.9,42.9),
                        Rainfall=c(1.23,1.23,1.23,1.23,1.23,1.23,1.23,1.23,1.23,1.23,1.16,1.16),
                        ET=c(1.38,1.38,1.38,1.38,1.38,1.38,1.38,1.38,1.38,1.38,1.38,1.38))

    }

  if(is.na(case.no)==T){
    start.year<-Start.Discharge
    outflow.c.ugL<-STA.outflow.TPconc
    outflow.q.kacft<-STA.outflow.vol
    path.width.km<-FlowPath.width
    hydroperiod.per<-Hydroperiod
    soil.z.cm<-Soil.Depth
    bd.i.gcc<-Soil.BulkDensity.initial
    soilP.i.mgkg<-Soil.TPConc.initial
    soilPgrad.i.mgcccm<-Vertical.SoilTPGradient.initial
    bd.f.gcc<-Soil.BulkDensity.final
    Psettle.myr<-PSettlingRate
    atmoP.mgm2yr<-P.AtmoDep
    RF.myr<-Rainfall
    ET.myr<-ET
  }else{
    start.year<-cases.dat[cases.dat$case.number==case.no,"Start.Discharge"]
    outflow.c.ugL<-cases.dat[cases.dat$case.number==case.no,"STA.outflow.TPconc"]
    outflow.q.kacft<-cases.dat[cases.dat$case.number==case.no,"STA.outflow.vol"]
    path.width.km<-cases.dat[cases.dat$case.number==case.no,"FlowPath.width"]
    hydroperiod.per<-cases.dat[cases.dat$case.number==case.no,"Hydroperiod"]
    soil.z.cm<-cases.dat[cases.dat$case.number==case.no,"Soil.Depth"]
    bd.i.gcc<-cases.dat[cases.dat$case.number==case.no,"Soil.BulkDensity.initial"]
    soilP.i.mgkg<-cases.dat[cases.dat$case.number==case.no,"Soil.TPConc.initial"]
    soilPgrad.i.mgcccm<-cases.dat[cases.dat$case.number==case.no,"Vertical.SoilTPGradient.initial"]
    bd.f.gcc<-cases.dat[cases.dat$case.number==case.no,"Soil.BulkDensity.final"]
    Psettle.myr<-cases.dat[cases.dat$case.number==case.no,"PSettlingRate"]
    atmoP.mgm2yr<-cases.dat[cases.dat$case.number==case.no,"P.AtmoDep"]
    RF.myr<-cases.dat[cases.dat$case.number==case.no,"Rainfall"]
    ET.myr<-cases.dat[cases.dat$case.number==case.no,"ET"]
  }

  time<-seq(0,Max.Yrs,Time.increment.yr)
  time<-time[time<=Max.Yrs]

  maxdist.km<-min(c(50,Max.Dist),na.rm=T)

  Dist.slice<-c(Dist.slice,Max.Dist)

  time.dat<-data.frame()
  for(i in 1:length(time)){
    if(is.na(case.no)==T){tmp<-EPGMProfile(Start.Discharge=start.year,
                                           STA.outflow.TPconc=outflow.c.ugL,
                                           STA.outflow.vol=outflow.q.kacft,
                                           FlowPath.width=path.width.km,
                                           Hydroperiod=hydroperiod.per,
                                           Soil.Depth=soil.z.cm,
                                           Soil.BulkDensity.initial=bd.i.gcc,
                                           Soil.TPConc.initial=soilP.i.mgkg,
                                           Vertical.SoilTPGradient.initial=soilPgrad.i.mgcccm,
                                           Soil.BulkDensity.final=bd.f.gcc,
                                           PSettlingRate=Psettle.myr,
                                           P.AtmoDep=atmoP.mgm2yr,
                                           Rainfall=RF.myr,
                                           ET=ET.myr,
                                           raw.output=T,results.table=F,plot.profile = F,Yr.Display =time[i])}
    else{tmp<-EPGMProfile(case.no=case.no,raw.output=T,results.table=F,plot.profile = F,Yr.Display =time[i])}
    tmp$time.step<-time[i]
    tmp$Year<-(start.year+time[i])-1
    time.dat<-rbind(tmp,time.dat)
  }
  time.dat<-time.dat[order(time.dat$time.step,time.dat$dist),]

  #wghts<-ifelse(time.dat$dist==0|time.dat$dist==maxdist.km,0.5,1)
  #cattail.flowpath<-aggregate(time.dat$cattail.time.per*wghts*(path.width.km*Dist.increment.km),by=list(time.dat$time.step),"sum")
  #Commented out may include in future iterations.

  if(plot.profile==TRUE){
    #Graphs_Profile plots
    oldpar<- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    par(family="serif",mar=c(1.75,4,2,2),oma=c(1,1,1,1),mgp=c(2.5,0.5,0));
    layout(matrix(1:6,2,3,byrow=T),widths=c(1,0.4,1))
    time.dat.plot<-time.dat[time.dat$dist==Dist.Display,]
    plot(Avg.SoilP.mgkg~time.step,time.dat.plot,type="n",las=1,xlab=NA,ylab="Concentration (mg kg\u207B\u00B9)",yaxs="i",xaxs="i",xlim=c(0,Max.Yrs),ylim=c(0,ceiling(max(time.dat.plot$Avg.SoilP.mgkg)+max(time.dat.plot$Avg.SoilP.mgkg)*0.1)))
    lines(time.dat.plot$time.step,time.dat.plot$Avg.SoilP.mgkg,col="black",lty=1,lwd=2)
    lines(time.dat.plot$time.step,time.dat.plot$SoilP.SS.mgkg,col="black",lty=2,lwd=1)
    mtext(side=3,"Soil Phosphorus")
    plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
    legend(0.5,0.85,legend=c("Integrated","Steady State"),
           pch=NA,lwd=c(2,1),lty=c(1,2),
           col=c("black"),cex=0.8,
           pt.cex=1.25,ncol=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title=paste("Soil Z:",soil.z.cm,"cm\n Distance:",Dist.Display,"km"),title.adj = 0)
    cols<-colorRampPalette(c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"))(length(Dist.slice))
    legend(0.5,0.5,legend=c(paste("Distance:",format(Dist.slice),"km")),
           pch=NA,lwd=c(2),lty=c(1),
           col=cols,cex=0.8,
           ncol=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title=paste("Soil Z:",soil.z.cm,"cm"),title.adj = 0)
    time.dat.plot2<-time.dat[time.dat$dist%in%Dist.slice,]
    plot(Avg.SoilP.mgkg~time.step,time.dat.plot,type="n",las=1,xlab=NA,ylab=NA,yaxs="i",xaxs="i",xlim=c(0,Max.Yrs),ylim=c(0,ceiling(max(time.dat.plot2$Avg.SoilP.mgkg)+max(time.dat.plot2$Avg.SoilP.mgkg)*0.1)))
    for(i in 1:length(Dist.slice)){
      with(time.dat.plot2[time.dat.plot2$dist==Dist.slice[i],],lines(time.step,Avg.SoilP.mgkg,lwd=2,col=cols[i]))
    }
    mtext(side=3,"Soil Phosphorus")

    plot(cattail.time.per~time.step,time.dat.plot,type="n",las=1,xlab=NA,ylab="Cattail Density (% Area)",yaxs="i",xaxs="i",xlim=c(0,Max.Yrs),ylim=c(0,105))
    lines(time.dat.plot$time.step,time.dat.plot$cattail.time.per,col="black",lty=1,lwd=2)
    lines(time.dat.plot$time.step,time.dat.plot$cattail.density.SS.per,col="black",lty=2,lwd=1)
    mtext(side=3,"Cattail Density")
    mtext(side=1,line=1.5,"Time (Yrs)")
    plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
    legend(0.5,0.85,legend=c("Integrated","Steady State"),
           pch=NA,lwd=c(2,1),lty=c(1,2),
           col=c("black"),cex=0.8,
           pt.cex=1.25,ncol=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title=paste("Cattail Density\n Distance:",Dist.Display,"km"),title.adj = 0)
    legend(0.5,0.5,legend=c(paste("Distance:",format(Dist.slice),"km")),
           pch=NA,lwd=c(2),lty=c(1),
           col=cols,cex=0.8,
           ncol=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title="Cattail Density",title.adj = 0)
    if(is.na(case.no)==F){
      text(x=-1,y=0,paste("Case:",cases.dat[cases.dat$case.number==case.no,"case.number"],"\nSTA:",cases.dat[cases.dat$case.number==case.no,"STA.Name"],"\nRecieving Area:",cases.dat[cases.dat$case.number==case.no,"Receiving.Area"],"\nSoil Depth (cm):",cases.dat[cases.dat$case.number==case.no,"Soil.Depth"],"\nStart Year:",cases.dat[cases.dat$case.number==case.no,"Start.Discharge"]),adj=0,xpd=NA,cex=0.85)
    }

    time.dat.plot2<-time.dat[time.dat$dist%in%Dist.slice,]
    plot(cattail.time.per~time.step,time.dat.plot,type="n",las=1,xlab=NA,ylab=NA,yaxs="i",xaxs="i",xlim=c(0,Max.Yrs),ylim=c(0,105))
    for(i in 1:length(Dist.slice)){
      with(time.dat.plot2[time.dat.plot2$dist==Dist.slice[i],],lines(time.step,cattail.time.per,lwd=2,col=cols[i]))
    }
    mtext(side=3,"Cattail Density")
    mtext(side=1,line=1.5,"Time (Yrs)")

  }

  if(results.time.table==TRUE){
    cell.area=path.width.km*Dist.increment.km
    total.area=Max.Dist*path.width.km
    wghts<-ifelse(time.dat$dist==0|time.dat$dist==maxdist.km,0.5,1)

    soil.ss.time<-aggregate(time.dat$SoilP.SS.mgkg*wghts*(cell.area/total.area),by=list(time.dat$time.step),"sum")
    BD<-aggregate(time.dat$BulkDensity.gcc*wghts*(cell.area/total.area),by=list(time.dat$time.step),"sum")
    avg.vol.soilP<-aggregate((time.dat$Avg.SoilP.mgkg*time.dat$BulkDensity.gcc/1000)*wghts*(cell.area/total.area),by=list(time.dat$time.step),"sum")
    avg.soilP<-data.frame(Group.1=avg.vol.soilP[,1],x=avg.vol.soilP[,2]/BD[,2]*1000)
    cattail.ha<-aggregate(time.dat$cattail.time.per*wghts*cell.area,by=list(time.dat$time.step),"sum")
    time.val<-time.dat$time.step

    rpt.time.step<-if(Time.increment.yr==1){c(0,1,2,3,10,15,20,25,200)}else{c(seq(0,Max.Yrs,Time.increment.yr)[1:8],Max.Yrs)}

    # Simulation information
    sim.zone<-data.frame(Parameter=c("Distance.km","Width.km","Area.km2","STA.outflow.volume.kAcftyr","Hydroperiod.pct","Soil.Depth.cm","P.Settle.Rate.myr","STA.outflow.Conc.ugL","STA.outflow.Load.mtyr"),
                         Value=c(Max.Dist,
                                 path.width.km,
                                 total.area,
                                 outflow.q.kacft,
                                 hydroperiod.per*100,
                                 soil.z.cm,
                                 Psettle.myr,
                                 outflow.c.ugL,
                                 round((outflow.q.kacft*43560*0.001/3.28^3)*outflow.c.ugL/1000,1)))

    #Time series summary
    TimeProfile.summary=data.frame(Time.Step=rpt.time.step,
                                   Year=(start.year+rpt.time.step)-1,
                                   SoilP.mgkg=round(avg.soilP[avg.soilP$Group.1%in%rpt.time.step,2],0),
                                   CattailDensity.ha=round(cattail.ha[cattail.ha$Group.1%in%rpt.time.step,2],0))



    out.time.sum<-list("Time.yrs" = Max.Yrs, "Time.increment.yrs"=Time.increment.yr,
                       "Simulated.Zone"= sim.zone,
                       "TimeProfile"=TimeProfile.summary)
  }

  #class(time.dat)<-"EPGMr"
  if(raw.time.output==T){return(time.dat)}
  if(results.time.table==TRUE){return(out.time.sum)}

}

