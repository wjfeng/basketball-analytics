library(ggplot2)
library(grid)
library(jpeg)
library(RCurl)
library(hexbin)

shots.data.raw<-read.csv('complete_data.csv',header=T)
#excluding free throws
fieldgoals.raw<-subset(shots.data.raw,!(((shot_x<=25&shot_x>=18)|(shot_x<=76&shot_x>=69))&(shot_y<=31&shot_y>=19)&(shot_shot_clock=="NA"|shot_shot_clock==24)))
#selecting only action in the frountcourt
data.frontcourt<-subset(fieldgoals.raw,!((pass_x<0|pass_x>94|pass_y<0|pass_y>50)|abs(pass_x-shot_x)>=47))
#transforming data so that all action happens on one side
data.frontcourt.new<-within(data.frontcourt,{
  shot_y<-ifelse(shot_x>47,50-shot_y,shot_y)
  shot_x<-ifelse(shot_x>47,94-shot_x,shot_x)
  pass_y<-ifelse(pass_x>47,50-pass_y,pass_y)
  pass_x<-ifelse(pass_x>47,94-pass_x,pass_x)
  poss_y<-ifelse(poss_x>47,50-poss_y,poss_y)
  poss_x<-ifelse(poss_x>47,94-poss_x,poss_x)
})
#uploading King's court as background
courtImg3.URL <- "https://pbs.twimg.com/media/DDNlC3CXgAAXhbM.jpg"
court3 <- rasterGrob(readJPEG(getURLContent(courtImg3.URL)),width=unit(1,"npc"), height=unit(1,"npc"))
#selecting Willie-Cauley Stein's data as a passer
wcs.passer.data<-subset(data.frontcourt.new,passer==699950,select=c(9,10,11,17,18,21,22,29))
#selecting Willie-Cauley Stein's data as a shooter
wcs.shooter.data<-subset(data.frontcourt.new,shooter==699950,select=c(9,10,11,17,18,21,22,29))
#mapping WCS's passes
wcs.passer <- ggplot(wcs.passer.data,aes(x=pass_x,y=pass_y))+
  ggtitle("Willie Cauley-Stein's Frontcourt Passes", subtitle = "The hexagon is where Cauley-Stein is when he passes, “B” is where the player who he passes to receives, and the point is where the player who receives the pass shoots")+
  annotation_custom(court3,0,47,0,50)+
  stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  geom_segment(aes(x=pass_x,y=pass_y,xend=poss_x,yend=poss_y))+
  geom_curve(aes(x=poss_x,y=poss_y,xend=shot_x,yend=shot_y))+
  geom_text(aes(x=poss_x,y=poss_y,label="B"))+
  geom_point(aes(x=shot_x,y=shot_y,colour=made))+
  xlim(0,47) +
  ylim(0,50)
wcs.passer
#mapping WCS's shots
wcs.shooter <- ggplot(wcs.shooter.data,aes(x=poss_x,y=poss_y))+
  ggtitle("Willie Cauley-Stein's Frontcourt Shots", subtitle = "“A” represents the player who passes Cauley-Stein the ball, the hexagon is where the ball is caught, and the point is the shot")+
  annotation_custom(court3,0,47,0,50)+
  stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  geom_segment(aes(x=pass_x,y=pass_y,xend=poss_x,yend=poss_y))+
  geom_curve(aes(x=poss_x,y=poss_y,xend=shot_x,yend=shot_y))+
  geom_text(aes(x=pass_x,y=pass_y,label="A"))+
  geom_point(aes(x=shot_x,y=shot_y,colour=made))+
  xlim(0,47) +
  ylim(0,50)
wcs.shooter
#selecting Kosta Koufos's data as a passer
koufos.passer.data<-subset(data.frontcourt.new,passer==398066,select=c(9,10,11,17,18,21,22,29))
#selecting Kosta Koufos's data as a shooter
koufos.shooter.data<-subset(data.frontcourt.new,shooter==398066,select=c(9,10,11,17,18,21,22,29))
koufos.passer <- ggplot(koufos.passer.data,aes(x=pass_x,y=pass_y))+
  ggtitle("Kosta Koufos's Pass Map", subtitle = "Areas where Kosta Koufos tends to pass from (hexagons) and to (points)")+
  annotation_custom(court3,0,47,0,50)+
  stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  geom_point(aes(x=poss_x,y=poss_y,colour=made))+
  xlim(-1,48) +
  ylim(-1,51)
koufos.passer
#calculating efficiencies from Koufos passes depending on area
#koufos.passer.data.ADG<-subset(koufos.passer.data,pass_x<18)
#koufos.block <- sum(koufos.passer.data.ADG$made==1)/nrow(koufos.passer.data.ADG)*100
#koufos.passer.data.BEH<-subset(koufos.passer.data,pass_x>=18&pass_x<36)
#koufos.perimeter <- sum(koufos.passer.data.BEH$made==1)/nrow(koufos.passer.data.BEH)*100
koufos.shooter <- ggplot(koufos.shooter.data,aes(x=poss_x,y=poss_y))+
  ggtitle("Kosta Koufos's Shot Map", subtitle = "Areas from which Kosta Koufos tends to catch (hexagons) and shoot (points)")+
  annotation_custom(court3,0,47,0,50)+
  stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  geom_point(aes(x=shot_x,y=shot_y,colour=made),alpha=0.7)+
  xlim(0,47) +
  ylim(0,50)
koufos.shooter
#koufos.shooter.data.D<-subset(koufos.shooter.data,poss_x<18&poss_y<34&pass_y>=17)
#sum(koufos.shooter.data.D$made==1)/nrow(koufos.shooter.data.D)*100
#koufos.shooter.data.A<-subset(koufos.shooter.data,poss_x<18&poss_y>=34)
#sum(koufos.shooter.data.A$made==1)/nrow(koufos.shooter.data.A)*100
#koufos.shooter.data.G<-subset(koufos.shooter.data,poss_x<18&poss_y<17)
#sum(koufos.shooter.data.G$made==1)/nrow(koufos.shooter.data.G)*100
temple.passer.data<-subset(data.frontcourt.new,passer==263903,select=c(9,10,11,17,18,21,22,29))
temple.shooter.data<-subset(data.frontcourt.new,shooter==263903,select=c(9,10,11,17,18,21,22,29))
temple.passer <- ggplot(temple.passer.data,aes(x=pass_x,y=pass_y))+
  ggtitle("Garrett Temple's Pass Map", subtitle = "Areas where Garrett Temple tends to pass from (hexagons) and to (points)")+
  annotation_custom(court3,0,47,0,50)+
  stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  geom_point(aes(x=poss_x,y=poss_y,colour=made))+
  xlim(-1,48) +
  ylim(-1,51)
temple.passer
#temple.passer.data.B<-subset(temple.passer.data,pass_x>=18&pass_x<36&pass_y>=34)
#sum(temple.passer.data.B$made==1)/nrow(temple.passer.data.B)*100
#temple.passer.data.A<-subset(temple.passer.data,poss_x<18&poss_y>=34)
#sum(temple.passer.data.A$made==1)/nrow(temple.passer.data.A)*100
#temple.passer.data.H<-subset(temple.passer.data,poss_x>=18&poss_x<36&poss_y<17)
#sum(temple.passer.data.H$made==1)/nrow(temple.passer.data.H)*100
temple.shooter <- ggplot(temple.shooter.data,aes(x=poss_x,y=poss_y))+
  ggtitle("Garrett Temple's Shot Map", subtitle = "Areas from which Garrett Temple tends to catch (hexagons) and shoot (points)")+
  annotation_custom(court3,0,47,0,50)+
  stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  geom_point(aes(x=shot_x,y=shot_y,colour=made),alpha=0.7)+
  xlim(-1,48) +
  ylim(-1,51)
temple.shooter
temple.shooter.data.A<-subset(temple.shooter.data,poss_x<18&poss_y>=34)
sum(temple.shooter.data.A$made==1)/nrow(temple.shooter.data.A)*100
temple.shooter.data.G<-subset(temple.shooter.data,poss_x<18&poss_y<17)
sum(temple.shooter.data.G$made==1)/nrow(temple.shooter.data.G)*100
#mclemore.passer.data<-subset(data.frontcourt.new,passer==604898,select=c(9,10,11,17,18,21,22,29))
#mclemore.shooter.data<-subset(data.frontcourt.new,shooter==604898,select=c(9,10,11,17,18,21,22,29))
#mclemore.passer <- ggplot(mclemore.passer.data,aes(x=pass_x,y=pass_y))+
#  ggtitle("Ben McLemore's Pass Map", subtitle = "Areas where Ben McLemore tends to pass from (hexagons) and to (points)")+
 # annotation_custom(court3,0,47,0,50)+
  #stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
#  scale_fill_gradientn(colours = c("yellow","orange","red")) +
#  geom_point(aes(x=poss_x,y=poss_y,colour=made),alpha=0.7)+
#  xlim(-1,48) +
#  ylim(-1,51)
#mclemore.passer
#mclemore.passer.data.B<-subset(mclemore.passer.data,pass_x>=18&pass_x<36&pass_y>=34)
#sum(mclemore.passer.data.B$made==1)/nrow(mclemore.passer.data.B)*100
#mclemore.passer.data.H<-subset(mclemore.passer.data,pass_x>=18&pass_x<36&pass_y<17)
#sum(mclemore.passer.data.H$made==1)/nrow(mclemore.passer.data.H)*100
#mclemore.shooter <- ggplot(mclemore.shooter.data,aes(x=poss_x,y=poss_y))+
 # ggtitle("McLemore's Shot Map", subtitle = "Areas from which Ben McLemore tends to catch (hexagons) and shoot (points)")+
#  annotation_custom(court3,0,47,0,50)+
#  stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
#  scale_fill_gradientn(colours = c("yellow","orange","red")) +
#  geom_point(aes(x=shot_x,y=shot_y,colour=made),alpha=0.7)+
#  xlim(-1,48) +
#  ylim(-1,51)
#mclemore.shooter
#mclemore.shooter.data.G<-subset(mclemore.shooter.data,poss_x<18&poss_y<17)
#sum(mclemore.shooter.data.G$made==1)/nrow(mclemore.shooter.data.G)*100
#mclemore.shooter.data.B<-subset(mclemore.shooter.data,poss_x>=18&poss_x<36&poss_y>=34)
#sum(mclemore.shooter.data.B$made==1)/nrow(mclemore.shooter.data.B)*100
#mclemore.shooter.data.H<-subset(mclemore.shooter.data,poss_x>=18&poss_x<36&poss_y<17)
#sum(mclemore.shooter.data.H$made==1)/nrow(mclemore.shooter.data.H)*100
hill.passer.data<-subset(data.frontcourt.new,passer==277552,select=c(9,10,11,17,18,21,22,29))
hill.shooter.data<-subset(data.frontcourt.new,shooter==277552,select=c(9,10,11,17,18,21,22,29))
hill.passer <- ggplot(hill.passer.data,aes(x=pass_x,y=pass_y))+
  ggtitle("George Hill's Pass Map", subtitle = "Areas where George Hill tends to pass from (hexagons) and to (points)")+
  annotation_custom(court3,0,47,0,50)+
  stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  geom_point(aes(x=poss_x,y=poss_y,colour=made),alpha=0.7)+
  xlim(-1,48) +
  ylim(-1,51)
hill.passer
#hill.passer.data.B<-subset(hill.passer.data,pass_x>=18&pass_x<36&pass_y>=34)
#sum(hill.passer.data.B$made==1)/nrow(hill.passer.data.B)*100
#hill.passer.data.E<-subset(hill.passer.data,pass_x>=18&pass_x<36&pass_y>=17&pass_y<34)
#sum(hill.passer.data.E$made==1)/nrow(hill.passer.data.E)*100
#hill.passer.data.H<-subset(hill.passer.data,pass_x>=18&pass_x<36&pass_y<17)
#sum(hill.passer.data.H$made==1)/nrow(hill.passer.data.H)*100
hill.shooter <- ggplot(hill.shooter.data,aes(x=poss_x,y=poss_y))+
  ggtitle("George Hill's Shot Map", subtitle = "Areas from which George Hill tends to catch (hexagons) and shoot (points)")+
  annotation_custom(court3,0,47,0,50)+
  stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  geom_point(aes(x=shot_x,y=shot_y,colour=made),alpha=0.7)+
  xlim(-1,48) +
  ylim(-1,51)
hill.shooter
#hill.shooter.data.A<-subset(hill.shooter.data,poss_x<18&poss_y>=34)
#sum(hill.shooter.data.A$made==1)/nrow(hill.shooter.data.A)*100
#hill.shooter.data.G<-subset(hill.shooter.data,poss_x<18&poss_y<17)
#sum(hill.shooter.data.G$made==1)/nrow(hill.shooter.data.G)*100
zbo.passer.data<-subset(data.frontcourt.new,passer==3512,select=c(9,10,11,17,18,21,22,29))
zbo.shooter.data<-subset(data.frontcourt.new,shooter==3512,select=c(9,10,11,17,18,21,22,29))
zbo.passer <- ggplot(zbo.passer.data,aes(x=pass_x,y=pass_y))+
  ggtitle("Zach Randolph's Pass Map", subtitle = "Areas where Zach Randolph tends to pass from (hexagons) and to (points)")+
  annotation_custom(court3,0,47,0,50)+
  stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  geom_point(aes(x=poss_x,y=poss_y,colour=made),alpha=0.7)+
  xlim(-1,48) +
  ylim(-1,51)
zbo.passer
#zbo.passer.data.A<-subset(zbo.passer.data,pass_x<18&pass_y>=34)
#sum(zbo.passer.data.A$made==1)/nrow(zbo.passer.data.A)*100
#zbo.passer.data.G<-subset(zbo.passer.data,pass_x<18&pass_y<17)
#sum(zbo.passer.data.G$made==1)/nrow(zbo.passer.data.G)*100
#zbo.passer.data.H<-subset(zbo.passer.data,pass_x>=18&pass_x<36&pass_y<17)
#sum(zbo.passer.data.H$made==1)/nrow(zbo.passer.data.H)*100
zbo.shooter <- ggplot(zbo.shooter.data,aes(x=poss_x,y=poss_y))+
  ggtitle("Zach Randolph's Shot Map", subtitle = "Areas from which Zach Randolph tends to catch (hexagons) and shoot (points)")+
  annotation_custom(court3,0,47,0,50)+
  stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  geom_point(aes(x=shot_x,y=shot_y,colour=made),alpha=0.7)+
  xlim(-1,48) +
  ylim(-1,51)
zbo.shooter
#zbo.shooter.data.A<-subset(zbo.shooter.data,poss_x<18&poss_y>=34)
#sum(zbo.shooter.data.A$made==1)/nrow(zbo.shooter.data.A)*100
#zbo.shooter.data.G<-subset(zbo.shooter.data,poss_x<18&poss_y<17)
#sum(zbo.shooter.data.G$made==1)/nrow(zbo.shooter.data.G)*100
#zbo.shooter.data.D<-subset(zbo.shooter.data,poss_x<18&poss_y<34&poss_y>=17)
#sum(zbo.shooter.data.D$made==1)/nrow(zbo.shooter.data.D)*100
#zbo.shooter.data.E<-subset(zbo.shooter.data,poss_x>=18&poss_x<36&poss_y<34&poss_y>=17)
#sum(zbo.shooter.data.E$made==1)/nrow(zbo.shooter.data.E)*100
vince.passer.data<-subset(data.frontcourt.new,passer==3230,select=c(9,10,11,17,18,21,22,29))
vince.shooter.data<-subset(data.frontcourt.new,shooter==3230,select=c(9,10,11,17,18,21,22,29))
vince.passer <- ggplot(vince.passer.data,aes(x=pass_x,y=pass_y))+
  ggtitle("Vince Carter's Pass Map", subtitle = "Areas where Vince Carter tends to pass from (hexagons) and to (points)")+
  annotation_custom(court3,0,47,0,50)+
  stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  geom_point(aes(x=poss_x,y=poss_y,colour=made),alpha=0.7)+
  xlim(-1,48) +
  ylim(-1,51)
vince.passer
vince.passer.data.A<-subset(vince.passer.data,pass_x<18&pass_y>=34)
sum(vince.passer.data.A$made==1)/nrow(vince.passer.data.A)*100
vince.passer.data.E<-subset(vince.passer.data,pass_x>=18&pass_x<36&pass_y>=17&pass_y<34)
sum(vince.passer.data.E$made==1)/nrow(vince.passer.data.E)*100
vince.shooter <- ggplot(vince.shooter.data,aes(x=poss_x,y=poss_y))+
  ggtitle("Vince Carter's Shot Map", subtitle = "Areas from which Vince Carter tends to catch (hexagons) and shoot (points)")+
  annotation_custom(court3,0,47,0,50)+
  stat_binhex(bins = 20, colour = "white", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  geom_point(aes(x=shot_x,y=shot_y,colour=made),alpha=0.7)+
  xlim(-1,48) +
  ylim(-1,51)
vince.shooter
#vince.shooter.data.A<-subset(vince.shooter.data,poss_x<18&poss_y>=34)
#sum(vince.shooter.data.A$made==1)/nrow(vince.shooter.data.A)*100
#vince.shooter.data.G<-subset(vince.shooter.data,poss_x<18&poss_y<17)
#sum(vince.shooter.data.G$made==1)/nrow(vince.shooter.data.G)*100
#vince.shooter.data.B<-subset(vince.shooter.data,poss_x>=18&poss_x<36&poss_y>=34)
#sum(vince.shooter.data.B$made==1)/nrow(vince.shooter.data.B)*100
#vince.shooter.data.E<-subset(vince.shooter.data,poss_x>=18&poss_x<36&poss_y<34&poss_y>=17)
#sum(vince.shooter.data.E$made==1)/nrow(vince.shooter.data.E)*100
#vince.shooter.data.H<-subset(vince.shooter.data,poss_x>=18&poss_x<36&poss_y<17)
#sum(vince.shooter.data.H$made==1)/nrow(vince.shooter.data.H)*100