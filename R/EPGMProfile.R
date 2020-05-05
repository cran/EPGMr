#' Distance Profile
#'
#' This function runs the EPGM model for a specific simulated period. The model is based primarily upon data collected in the early 1990's along the phosphorus gradient in WCA-2A. Substantial additional data collected since then in WCA-2A and other locations indicate a need to recalibrate the model and potentially revise its structure. Recent data suggest, for example, that the relationship between cattail density and soil P needs recalibration and that actual soil P thresholds for biological impacts are probably lower than reflected in the original calibrations.  There are also issues relating to interpretation of and potential anomalies in the historical soil P calibration data attributed to variations in soil core collection method and definition of the soil/water interface (inclusion vs. exclusion of floc layer). There are also indications in the recent data of biologically-mediated vertical transport and/or mixing that are not reflected in the current model structure.
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
#' @param Yr.Display Output displays results for this time (years)
#' @param Max.Yrs Maximum number of years simulated
#' @param Max.Dist Maximum ditance plotted, default is 50 km
#' @param Dist.increment.km Distance increment modeled
#' @param plot.profile If `TRUE` base plot will be generate with water column distance, soil distance and cattail distance profiles.
#' @param raw.output If `TRUE` a `data.frame` will be printed with all calculations used to estimate various parameters.Default is set to `FALSE`.
#' @param results.table if `TRUE` summary results table will be printed in the console. Default is set to `TRUE`.
#' @param summary.distance Default is `c(0,0.5,1,2,4,8,10)` but can be changed. Values determine what distances will be included in the summary table.
#' @keywords "water quality"
#' @export
#' @return This function computes and plots the distance profile along the gradient based on input values

#' @importFrom grDevices colorRampPalette
#' @importFrom graphics abline layout legend lines mtext par plot text
#' @importFrom stats aggregate
#' @importFrom utils data

#' @examples
#' EPGMProfile(case.no=11)
#'
#' EPGMProfile(NA,1991,38,526,15.3,50,10,0.05,257,-0.004,0.04,15.2,45,1.3,1.4)


EPGMProfile=function(case.no=NA,
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
Yr.Display=30,
Max.Yrs=200,
Max.Dist=15,
Dist.increment.km=0.1,
plot.profile=TRUE,
raw.output=FALSE,
results.table=TRUE,
summary.distance=c(0,0.5,1,2,4,8,10)
){

  ## Stop/warning section of the function
  input.val.na<-sum(c(is.na(Start.Discharge),is.na(STA.outflow.TPconc),is.na(STA.outflow.vol),is.na(FlowPath.width),
                     is.na(Hydroperiod),is.na(Soil.Depth),is.na(Soil.BulkDensity.initial),is.na(Soil.TPConc.initial),
                     is.na(Vertical.SoilTPGradient.initial),is.na(Soil.BulkDensity.final),is.na(PSettlingRate),
                     is.na(P.AtmoDep),is.na(ET)))
  summary.distance<-summary.distance
  if(is.na(case.no)==TRUE & input.val.na>1){
    stop("Missing inputs, either input a 'case.no' or all individual model parameters.")
  }
  if(is.na(case.no)==FALSE & case.no>12){
    stop("'case.no' range from 1 to 12.")
  }
  if(Dist.increment.km>Max.Dist){
    stop("Distance increment is greater than the maximum gradient distance.")
  }
  if(results.table==TRUE & (sum(is.na(summary.distance))>0)==TRUE){
    stop("Distance values for summary table empty, please input data.")
  }
  if(min(summary.distance)<0){
    stop("Distance values much be equal to or greater than zero")
  }
  if(raw.output==TRUE&results.table==TRUE){
    warning("Can't have raw.output and results.table, You can't have your cake and eat it too.")
    raw.output=FALSE
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
    hydroperiod.per<-Hydroperiod/100
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
    hydroperiod.per<-cases.dat[cases.dat$case.number==case.no,"Hydroperiod"]/100
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

  ## Default values based on empirical studies
  SoilP_Accret.slope<-1.467
  SoilP_Accret.int<-462.8865
  spread.mgkg<-if(soil.z.cm==10){144.05071892624}else{727.675986572509}
  midpoint.mgkg<-if(soil.z.cm==10){1034.4142631602}else{71.2079211802927}

  maxdist.km<-min(c(50,Max.Dist),na.rm=T)

  ## Intermediate Calcs
  vol.soilP.mgcm3<-(soilP.i.mgkg*bd.i.gcc/1000)
  cattail.i.per<-1/(1+exp(-(soilP.i.mgkg-midpoint.mgkg)/spread.mgkg))*100

  b.myr<-RF.myr-ET.myr;
  Rinflow.myr<-Psettle.myr*hydroperiod.per+b.myr
  CbInflow.ugL<-atmoP.mgm2yr/Rinflow.myr
  Rss.myr<-Psettle.myr*hydroperiod.per+b.myr
  Cbss.ugL<-atmoP.mgm2yr/Rss.myr
  Chalf.ugL<-1
  Me.kgm2<-10*soil.z.cm*bd.f.gcc
  Mi.kgm2<-10*soil.z.cm*bd.i.gcc
  Xi.mgcm3<-soilP.i.mgkg*bd.i.gcc/1000
  TArea.km2<-path.width.km*maxdist.km
  CellArea.ha<-path.width.km*Dist.increment.km*100
  End.Yr<-Yr.Display+start.year-1

  HalfLife.settle.yrs<-0.1
  Ke.relax.perYr<-log(2)/HalfLife.settle.yrs
  Current.Ke<-Psettle.myr+(Psettle.myr-Psettle.myr)*exp(-Yr.Display*Ke.relax.perYr)
  aa<-Psettle.myr
  bb<-Psettle.myr-Psettle.myr
  CumAvg.Ke.myr<-if(Yr.Display==0){Psettle.myr}else{(aa*Yr.Display+bb/Ke.relax.perYr*(1-exp(-Ke.relax.perYr*Yr.Display)))/Yr.Display}

  ## Profile Calc
  dist.seq<-seq(0,maxdist.km,Dist.increment.km)
  area.seq<-dist.seq*path.width.km

  #Steady State
  q.seq<-b.myr*area.seq+outflow.q.kacft*43560*0.001/3.28^3
  TP.SS<-if(Yr.Display==0){Cbss.ugL}else{outflow.c.ugL}

  for(i in 2:length(dist.seq)){
    TP.SS[i]<-if(Yr.Display==0){Cbss.ugL}else{Cbss.ugL+(TP.SS[i-1]-Cbss.ugL)*if(b.myr==0){exp(-Psettle.myr*hydroperiod.per*(area.seq[i]-area.seq[i-1])/q.seq[i])}else{(q.seq[i]/q.seq[i-1])^(-Rss.myr/b.myr)}}
  }
  SoilP.SS<-SoilP_Accret.int+SoilP_Accret.slope*Psettle.myr*hydroperiod.per*TP.SS
  SSTime.yrs<-10*bd.f.gcc*soil.z.cm*SoilP.SS/(Psettle.myr*hydroperiod.per*TP.SS)
  cattail.SS.per<-(1/(1+exp(-(SoilP.SS-midpoint.mgkg)/spread.mgkg)))*100

  #Current Time
  Ke.myr<-Psettle.myr
  R.myr<-Ke.myr*hydroperiod.per+b.myr
  Cb.ugL<-atmoP.mgm2yr/R.myr
  TP.Time.ugL<-if(Yr.Display==0){Cbss.ugL}else{outflow.c.ugL}

  for(i in 2:length(dist.seq)){
    Ke.myr[i]<-Psettle.myr+max(c(0,(Ke.myr[i-1]-Psettle.myr)*(TP.Time.ugL[i-1]-Cbss.ugL+Chalf.ugL)))
    R.myr[i]<-Ke.myr[i]*hydroperiod.per+b.myr
    Cb.ugL[i]<-atmoP.mgm2yr/R.myr[i]
    TP.Time.ugL[i]<-if(Yr.Display==0){Cbss.ugL}else{Cb.ugL[i]+(TP.Time.ugL[i-1]-Cb.ugL[i])*if(b.myr==0){exp(-hydroperiod.per*Ke.myr[i]*(area.seq[i]-area.seq[i-1])/q.seq[i])}else{(q.seq[i]/q.seq[i-1])^(-R.myr[i]/b.myr)}}
  }

  #Integration
  Ke.int.myr<-CumAvg.Ke.myr
  R.int.myr<-Ke.int.myr*hydroperiod.per+b.myr
  Cb.int.ugL<-atmoP.mgm2yr/R.int.myr
  TP.int.ugL<-if(Yr.Display==0){Cbss.ugL}else{outflow.c.ugL}
  for(i in 2:length(dist.seq)){
    Ke.int.myr[i]<-Psettle.myr+max(c(0,(Ke.int.myr[i-1]-Psettle.myr)*(TP.int.ugL[i-1]-Cbss.ugL+Chalf.ugL)))
    R.int.myr[i]<-Ke.int.myr[i]*hydroperiod.per+b.myr
    Cb.int.ugL[i]<-atmoP.mgm2yr/R.int.myr[i]
    TP.int.ugL[i]<-Cb.int.ugL[i]+(TP.int.ugL[i-1]-Cb.int.ugL[i])*if(b.myr==0){exp(-hydroperiod.per*Ke.int.myr[i]*(area.seq[i]-area.seq[i-1])/q.seq[i])}else{(q.seq[i]/q.seq[i-1])^(-R.int.myr[i]/b.myr)}
  }
  P.accret.mgm2yr<-TP.int.ugL*hydroperiod.per*Ke.int.myr
  NewSoilP.mgkg<-SoilP_Accret.int*Ke.int.myr/Psettle.myr+SoilP_Accret.slope*P.accret.mgm2yr
  Soil.accret.kgm2yr<-P.accret.mgm2yr/NewSoilP.mgkg
  Soil.accret.cmyr<-Soil.accret.kgm2yr/(10*bd.f.gcc)
  NewSoil.z.cm<-pmin(soil.z.cm,c(Soil.accret.cmyr*Yr.Display))
  EquilTime.yrs<-soil.z.cm/Soil.accret.cmyr
  SoilMass.kgm2<-ifelse(Yr.Display<EquilTime.yrs,10*soil.z.cm*bd.i.gcc+Soil.accret.kgm2yr*(1-bd.i.gcc/bd.f.gcc)*Yr.Display,Me.kgm2)
  BulkDensity.gcc<-SoilMass.kgm2/10/soil.z.cm

  coef1<-P.accret.mgm2yr-10000*Soil.accret.cmyr*(vol.soilP.mgcm3+soilPgrad.i.mgcccm*soil.z.cm/2)
  coef2<-10000*Soil.accret.cmyr^2*soilPgrad.i.mgcccm/2
  Avg.SoilP.mgkg<-ifelse(Yr.Display>EquilTime.yrs,NewSoilP.mgkg,(Mi.kgm2*soilP.i.mgkg+coef1*Yr.Display+coef2*Yr.Display^2)/SoilMass.kgm2)
  Avg.SoilP.mgcm3<-Avg.SoilP.mgkg*BulkDensity.gcc/1000
  B.SoilP.mgcm3<-pmax(0,c(ifelse(NewSoil.z.cm>=soil.z.cm,Avg.SoilP.mgcm3,vol.soilP.mgcm3-soilPgrad.i.mgcccm*(NewSoil.z.cm-soil.z.cm/2))))
  cattail.time.per<-1/(1+exp(-(Avg.SoilP.mgkg-midpoint.mgkg)/spread.mgkg))*100

  profile.dat<-data.frame(dist=dist.seq,
                          area=area.seq,
                          q.hm3yr=q.seq,
                          TP.SS.ugL=TP.SS,
                          SoilP.SS.mgkg=SoilP.SS,
                          SSTime.yrs=SSTime.yrs,
                          cattail.density.SS.per=cattail.SS.per,
                          Ke.myr=Ke.myr,
                          R.myr=R.myr,
                          Cb.ugL=Cb.ugL,
                          TP.Time.ugL=TP.Time.ugL,
                          Ke.int.myr=Ke.int.myr,
                          R.int.myr=R.int.myr,
                          Cb.int.ugL=Cb.int.ugL,
                          TP.int.ugL=TP.int.ugL,
                          Soil.accret.cmyr=Soil.accret.cmyr,
                          P.accret.mgm2yr=P.accret.mgm2yr,
                          NewSoilP.mgkg=NewSoilP.mgkg,
                          NewSoil.z.cm=NewSoil.z.cm,
                          EquilTime.yrs=EquilTime.yrs,
                          SoilMass.kgm2=SoilMass.kgm2,
                          BulkDensity.gcc=BulkDensity.gcc,
                          Avg.SoilP.mgkg=Avg.SoilP.mgkg,
                          cattail.time.per=cattail.time.per)
  if(plot.profile==TRUE){
    #Graphs_Profile plots
    oldpar<- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    par(family="serif",mar=c(3.25,4,2,2),oma=c(1,1,1,1),mgp=c(2.25,0.5,0));
    layout(matrix(1:4,2,2))

    plot(TP.SS.ugL~dist,profile.dat,type="n",las=1,xlab="Distance (km)",ylab="Concentration (\u03BC g L\u207B\u00B9)",yaxs="i",xaxs="i",xlim=c(0,Max.Dist),ylim=c(0,ceiling(outflow.c.ugL+outflow.c.ugL*0.1)))
    lines(profile.dat$dist,profile.dat$TP.SS.ugL,col="forestgreen",lty=2,lwd=2)
    lines(profile.dat$dist,profile.dat$TP.Time.ugL,col="red",lty=1,lwd=2)
    lines(profile.dat$dist,profile.dat$Cb.int.ugL,col="black",lty=3)
    mtext(side=3,"Water Column Distance Profile")
    legend("topright",legend=c("Steady State",paste("Time =",Yr.Display,'Yrs'),"Initial"),
           pch=NA,lwd=c(2,2,1),lty=c(2,1,3),
           col=c("forestgreen","red","black"),
           pt.cex=1.25,ncol=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

    plot(SoilP.SS.mgkg~dist,profile.dat,type="n",las=1,xlab="Distance (km)",ylab="Concentration (mg kg\u207B\u00B9)",yaxs="i",xaxs="i",xlim=c(0,Max.Dist),ylim=c(0,ceiling(max(SoilP.SS)+max(SoilP.SS)*0.1)))
    lines(profile.dat$dist,profile.dat$SoilP.SS.mgkg,col="red ",lty=2,lwd=2)
    lines(profile.dat$dist,profile.dat$Avg.SoilP.mgkg,col="red",lty=1,lwd=2)
    abline(h=soilP.i.mgkg,col="black",lty=3)
    mtext(side=3,"Soil Distance Profile")
    legend("topright",legend=c("Steady State",paste("Time =",Yr.Display,'Yrs'),"Initial"),
           pch=NA,lwd=c(2,2,1),lty=c(2,1,3),
           col=c("red","red","black"),
           pt.cex=1.25,ncol=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

    plot(cattail.density.SS.per~dist,profile.dat,type="n",las=1,xlab="Distance (km)",ylab="Cattail Density (%)",yaxs="i",xaxs="i",xlim=c(0,Max.Dist),ylim=c(0,105))
    lines(profile.dat$dist,profile.dat$cattail.density.SS.per,lty=2,lwd=2)
    lines(profile.dat$dist,profile.dat$cattail.time.per,col="black",lty=1,lwd=2)
    abline(h=cattail.i.per,col="black",lty=3)
    mtext(side=3,"Cattail Density")
    legend("topright",legend=c("Steady State",paste("Time =",Yr.Display,'Yrs'),"Initial"),
           pch=NA,lwd=c(2,2,1),lty=c(2,1,3),
           col=c("black","black","black"),
           pt.cex=1.25,ncol=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

    plot(0:1,0:1,axes=F,ylab=NA,xlab=NA,type="n")
    if(is.na(case.no)==F){
      text(0,0.5,paste("Case:",cases.dat[cases.dat$case.number==case.no,"case.number"],"\nSTA:",cases.dat[cases.dat$case.number==case.no,"STA.Name"],"\nRecieving Area:",cases.dat[cases.dat$case.number==case.no,"Receiving.Area"],"\nSoil Depth (cm):",cases.dat[cases.dat$case.number==case.no,"Soil.Depth"],"\nTime (Years):",Yr.Display,"\nStart Year:",cases.dat[cases.dat$case.number==case.no,"Start.Discharge"],"\nEnd Year:",End.Yr),adj=0)
    }

  }

  if(results.table==TRUE){
  # Results Report
  summary.distance<-summary.distance[summary.distance<Max.Dist]

  # Simulation information
  sim.zone<-data.frame(Parameter=c("Distance.km","Width.km","Area.km2","STA.outflow.volume.kAcftyr","Hydroperiod.pct","Soil.Depth.cm","P.Settle.Rate.myr","STA.outflow.Conc.ugL","STA.outflow.Load.mtyr"),
                       Value=c(max(profile.dat$dist,na.rm=T),
                               path.width.km,
                               round(max(profile.dat$dist,na.rm=T)*path.width.km,2),
                               outflow.q.kacft,
                               hydroperiod.per*100,
                               soil.z.cm,
                               Psettle.myr,
                               outflow.c.ugL,
                               round((outflow.q.kacft*43560*0.001/3.28^3)*outflow.c.ugL/1000,1)))
  #Distance Profile
  DistanceProfile<-rbind(t(data.frame(WaterCol.Pconc.ugL=round(profile.dat[profile.dat$dist%in%summary.distance,"TP.Time.ugL"],1))),
                         t(data.frame(SteadyState.WC.Conc.ugL=round(profile.dat[profile.dat$dist%in%summary.distance,"TP.SS.ugL"],1))),
                         t(data.frame(SteadyState.Soil.Conc.mgkg=round(profile.dat[profile.dat$dist%in%summary.distance,"SoilP.SS.mgkg"],0))),
                         t(data.frame(Time.to.Steady.State.yrs=round(profile.dat[profile.dat$dist%in%summary.distance,"SSTime.yrs"],1))),
                         t(data.frame(NewSoil.Depth.cm=round(profile.dat[profile.dat$dist%in%summary.distance,"NewSoil.z.cm"],1))),
                         t(data.frame(Soil.Mass.Accret.kgm2yr=round(profile.dat[profile.dat$dist%in%summary.distance,"Soil.accret.cmyr"],2))),
                         t(data.frame(Cattail.Density.pct=round(profile.dat[profile.dat$dist%in%summary.distance,"cattail.time.per"],0))),
                         t(data.frame(SteadyState.Cattail.Density.pct=round(profile.dat[profile.dat$dist%in%summary.distance,"cattail.density.SS.per"],0)))
  )
  colnames(DistanceProfile)<-c(summary.distance)

  #Water Budget
  Flow.m<-data.frame(Total.Flow.m=round(c((outflow.q.kacft*43560*0.001/3.28^3)/TArea.km2*Yr.Display,
                                          RF.myr*Yr.Display,ET.myr*Yr.Display,
                                          ((outflow.q.kacft*43560*0.001/3.28^3)/TArea.km2*Yr.Display)+(RF.myr-ET.myr)*Yr.Display),2))
  Flow.hm3<-data.frame(Total.Flow.hm3=round(Flow.m[,1]*TArea.km2,0))
  Flow.myr<-data.frame(Sim.Avg.Flow.myr=if(Yr.Display==0){NA}else{Flow.m[,1]/Yr.Display})

  Water.Budget<-cbind(Flow.m,Flow.hm3,round(Flow.myr,2))
  rownames(Water.Budget)<-c("Inflow","Rainfall","ET","Outflow")

  #
  wghts=ifelse(profile.dat$dist==0|profile.dat$dist==maxdist.km,0.5,1)
  #P Budget
  PMass.mgm2.inflow<-Flow.m[1,]*outflow.c.ugL
  PMass.mgm2.RF<-Flow.m[2,]*atmoP.mgm2yr
  PMass.mgm2.removal<-sum(profile.dat$P.accret.mgm2yr*wghts)*CellArea.ha/TArea.km2/100*Yr.Display
  PMass.mgm2.outflow<-PMass.mgm2.inflow+PMass.mgm2.RF-PMass.mgm2.removal
  PMass.mgm2<-data.frame(PMass.mgm2=c(PMass.mgm2.inflow,PMass.mgm2.RF,PMass.mgm2.removal,PMass.mgm2.outflow))
  PMass.mtons<-data.frame(PMass.mtons=PMass.mgm2[,1]*TArea.km2/1000)
  Sim.Avg.Load.mgm2yr<-data.frame(Sim.Avg.Load.mgm2yr=round(if(Yr.Display==0){NA}else{PMass.mgm2[,1]/Yr.Display},1))

  P.Budget<-cbind(round(PMass.mgm2,0),round(PMass.mtons,1),round(Sim.Avg.Load.mgm2yr,1))
  rownames(P.Budget)<-c("Inflow","Rainfall","Removal","Outflow")

  #Soils
  BD.istore<-sum(bd.i.gcc*ifelse(profile.dat$dist==0|profile.dat$dist==maxdist.km,0.5,1))*CellArea.ha/TArea.km2/100
  BD.curstore<-sum(profile.dat$BulkDensity.gcc*ifelse(profile.dat$dist==0|profile.dat$dist==maxdist.km,0.5,1))*CellArea.ha/TArea.km2/100
  BD.accret<-bd.f.gcc

  soilmass.i<-soil.z.cm*10*BD.istore
  soilmass.cur<-soil.z.cm*10*BD.curstore
  soilmass.accret<-(sum((profile.dat$P.accret.mgm2yr/profile.dat$NewSoilP.mgkg)*wghts)*CellArea.ha/TArea.km2/100)*Yr.Display
  soilmass.burial<-soilmass.accret+soilmass.i-soilmass.cur

  BD.burial<-if(Yr.Display==0){0}else{soilmass.burial/10/((sum(profile.dat$Soil.accret.cmyr*wghts)*CellArea.ha/TArea.km2/100)*Yr.Display)}

  Pvol.i<-sum((soilP.i.mgkg*bd.i.gcc/1000)*wghts)*CellArea.ha/TArea.km2/100
  Pvol.c<-sum((profile.dat$Avg.SoilP.mgkg*profile.dat$BulkDensity.gcc/1000)*wghts)*CellArea.ha/TArea.km2/100
  Pconc.i<-Pvol.i/bd.i.gcc*1000
  Pconc.c<-Pvol.c/BD.curstore*1000

  Pmass.i<-soil.z.cm*10000*Pvol.i
  Pmass.c<-soil.z.cm*10000*Pvol.c
  Pmass.accret<-(sum(profile.dat$P.accret.mgm2yr*wghts)*CellArea.ha/TArea.km2/100)*Yr.Display
  Pmass.burial<-Pmass.accret+Pmass.i-Pmass.c

  Pconc.accret<-if(soilmass.accret==0){0}else{Pmass.accret/soilmass.accret}
  Pconc.burial<-if(soilmass.burial==0){0}else{Pmass.burial/soilmass.burial}
  Pvol.accret<-BD.accret*Pconc.accret/1000
  Pvol.burial<-BD.burial*Pconc.burial/1000

  soilmass<-data.frame(SoilMass.kgm2=c(soilmass.i,soilmass.cur,soilmass.accret,soilmass.burial))
  PMass.mgm2<-data.frame(PMass.mgm2=c(Pmass.i,Pmass.c,Pmass.accret,Pmass.burial))
  PConc.mgkg<-data.frame(PConc.mgkg=c(Pconc.i,Pconc.c,Pconc.accret,Pconc.burial))
  BD.gcm3<-data.frame(BulkDensity.gcm3=c(BD.istore,BD.curstore,BD.accret,BD.burial))
  PVol.mgcm3<-data.frame(PVol.mgcm3=c(Pvol.i,Pvol.c,Pvol.accret,Pvol.burial))

  soils<-cbind(round(soilmass,2),round(PMass.mgm2,0),round(PConc.mgkg,0),round(BD.gcm3,3),round(PVol.mgcm3,3))
  rownames(soils)<-c("Initial Storage","Current Storage","Accretion","Burial")

  # Final table
  out.sum<-list("Time.yrs" = Yr.Display,
                "Simulated.Zone"= sim.zone,
                "DistanceProfile"=DistanceProfile,
                "Water.Budget"=Water.Budget,
                "P.MassBalance"=P.Budget,
                "Soils"=soils)
}
  #class(results.table)<-"EPGMr"
  #class(raw.output)<-"EPGMr"

  if(raw.output==TRUE){return(profile.dat)}
  if(results.table==TRUE){return(out.sum)}
}
#
