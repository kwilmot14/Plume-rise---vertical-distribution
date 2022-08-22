library(rioja) # used for interpolation to finer vertical resolution


# need to set thermo_file equal to some output thermodynamic profile from the Freitas model 
# can put the below in a loop or make it into a function 
thermo_profile<-"some_file.txt"

prof<-read.table(thermo_file, header=F, stringsAsFactors = F)
prof<-prof[,c(1:3, 12:15)]

prof[,c(1,4)]<-prof[,c(1,4)]*1000 # km to meters
prof[,2]<-prof[,2]*100 # mb to Pa
prof[,6:7]<-prof[,6:7]+273.15 # C to K 

colnames(prof)<-c("Z", "pres", "W","rad","entr","Tplume","Tenv")   #not actually entrainment yet

prof$entr<-prof$entr*-1*prof$W
prof<-prof[prof$Z <= PTH*1000, ] #they take the first level of W < 1 as plume top and add 50m to get top of level

# going to assume constant density in a layer of the plume
prof$rho_plume<-prof$pres/(prof$Tplume*287 )
prof$area<-(prof$rad**2)*pi
prof$mass<-100*prof$area*prof$rho_plume

prof$entr<-prof$entr*prof$mass  #entrained mass in kg/s at the steady solution

### Net vertical mass flux

# interpolating to values at interface between layers
# this section is kind of ugly, but it works
prof$avg_rho<-c(diff(prof$rho_plume)*0.5+prof$rho_plume[1:(length(prof$rho_plume)-1)],prof$rho_plume[length(prof$rho_plume)])
prof$avg_W<-c(diff(prof$W)*0.5+prof$W[1:(length(prof$W)-1)],prof$W[length(prof$W)])
prof$avg_area<-c(diff(prof$area)*0.5+prof$area[1:(length(prof$area)-1)],prof$area[length(prof$area)])

prof$mass_flux_out<-prof$avg_rho*prof$avg_area*prof$avg_W #vertical mass flux out
prof$mass_flux_in<-c(0,prof$mass_flux_out[1:(length(prof$mass_flux_out)-1)]) #vertical mass flux in
prof$mass_flux_out[length(prof$mass_flux_out)]<-0  # last layer ejects all remaining mass
prof$net_flux<-prof$mass_flux_in-prof$mass_flux_out

prof<-prof[,c(1,3,5,16)]
prof[1,]<-0 # no surface detrainment (literally the surface)

prof$detr<-(prof$entr+prof$net_flux) # detrainment profile
prof<-rbind(prof, prof[length(prof[,1]),]) # add a layer to be the actual plume top for interpolation
prof$Z[length(prof$Z)]<-prof$Z[length(prof$Z)-1]+50 #top of the top layer
prof$detr[length(prof$detr)]<-0 #no detrainment at exact plume top

#  Interpolate profile

prof<-interp.dataset(prof,x=prof$Z,xout=seq(0,max(prof$Z),10)) #interpolate to 10m intervals
prof<-as.data.frame(prof)
prof$detr<-prof$detr/sum(prof$detr) # normalizing again so we are not creating emissions via the interpolation

# prof$detr is now the vertical distribution of emissions below plume top at 10 m resolution