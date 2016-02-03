setwd("/Users/appletree/Documents")
data<-read.csv("realtime.csv",stringsAsFactors =FALSE)
setwd("/Users/appletree/Documents/gtfs_nyct_bus_20150103")
trip<-read.csv("trips.txt",stringsAsFactors =FALSE)
route<-read.csv("routes.txt",stringsAsFactors =FALSE)
stops<-read.csv("stops.txt",stringsAsFactors =FALSE)
stop_times<-read.csv("stop_times.txt",stringsAsFactors =FALSE)
library("dplyr")

#1
ve_t<-unique(paste(data[,3],data[,9]))
ve_t2<-unlist(strsplit(ve_t," "))
ve<-rep(0,length(ve_t2)/2)
n=1
while(2*n<=length(ve)){
	ve[n]<-ve_t2[2*n-1]
	n=n+1
}
median(table(ve))
##5

##the highest total number of trips made by a Manhattan bus route (for our purposes all routes starting with "M")?
Mroutes<-trip[grepl("^M",trip$route_id ),c(1,3)]
Mroutes2<-group_by(Mroutes,route_id)
Mroutes3<-summarise(Mroutes2,count=n())
head(arrange(Mroutes3,desc(count)))
#M14D  2138




M_trip_id<-intersect(data$trip_id,Mroutes$trip_id)
M_trip<-data[is.element(data$trip_id,M_trip_id),]

##M_trip$trip_id<-as.factor(as.character(M_trip$trip_id))
M_trip$dist_along_route<-as.numeric(M_trip$dist_along_route)
M_trip<-M_trip[!is.na(M_trip$dist_along_route),]
M_trip$timestamp<-strftime(M_trip$timestamp,"%Y-%m-%d %H:%M:%S")

M_move<-M_trip[M_trip$progress==0,]
M_move2<-group_by(M_move,trip_id)
M_move3<-summarise(M_move2,dis=(max(dist_along_route)-min(dist_along_route)),time=difftime(max(timestamp),min(timestamp),units="hours"))
M_move4<-mutate(M_move3,velocity=dis/as.numeric(time))

M_vel<-merge(M_move4,Mroutes,by="trip_id")
M_vel2<-group_by(M_vel,route_id)
M_vel3<-summarise(M_vel2,velocity=mean(velocity))
arrange(M_vel3,desc(velocity))
#M60+ 17154.641632
#M116  7937.832916 dist_from_stop
0.000621371192*17154.641632
#10.65940012

#4
subtrip<-trip[trip$route_id=="M116"&trip$direction_id=="1",]
M116<-M_trip[is.element(M_trip$trip_id,subtrip$trip_id),]

#average speed of M116 meter per mins
v_116<-M_vel3$velocity[M_vel3$route_id=="M116"]/60
#132.2972153

headways<-c()
m116_stops<-unique(M116$next_stop_id)
m116_stops<-m116_stops[-1]

for(s in 1:length(m116_stops)){
	a<-M116[M116$next_stop_id==m116_stops[s],c(2,3,11,13)]
	b<-a
	n=1
	for(i in 1:(nrow(a)-1)){
		if (a$vehicle_id[i]!=a$vehicle_id[i+1]){
			b[n,]<-a[i,]
			n=n+1
		}	
	}
	b[n,]<-a[i+1,]
	b<-b[1:n,]
	b$dist_from_stop<-as.numeric(b$dist_from_stop)
	b<-mutate(b,time=b$dist_from_stop/v_116)
	diff_time=difftime(tail(b$timestamp,-1),head(b$timestamp,-1),units="mins")
	headway<-as.numeric(diff_time)+head(b$time,-1)-tail(b$time,-1)
	headways<-c(headways,headway)
}
var(headways)
#163.7918388


##M_stops<-stops[is.element(stops$stop_id,m116_stops),c(1,2)]
##
schedule<-stop_times[is.element(stop_times$trip_id,subtrip$trip_id)&stop_times$stop_id=="401998",c(1,3)]
depart<-M116[M116$next_stop_id=="405315",c(2,3,9)]

diff<-merge(depart,schedule,by="trip_id",all=FALSE)
a<-paste("2015-01-28",diff$departure_time[1:72])
a<-c(a,"2015-01-29 00:35:00","2015-01-29 00:55:00","2015-01-29 01:25:00","2015-01-29 01:25:00")
diff_time<-difftime(a,diff$timestamp,units="mins")
var(diff_time)
#1.969555812

