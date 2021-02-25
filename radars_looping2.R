write.csv(gw, "C:\\Users\\Jonathan\\Desktop\\gw.csv")

library(fplscrapR)
library(readxl)
library(dplyr)
library(reshape)
library(ggplot2)

#prebuilt functions from the fplscrapR package help to gather gameweek data and 
#player info for club and position info (needed for scoring system)
gw <- get_player_details() #gw by gw data
players<-get_player_info() #player info for club/position

#Import the xlsm with data scraped via the VBA code from understat. 
x<- read_excel("xG and xA macro data.xlsm", sheet = "Players", range = "A1:D800")
head(x)

####CLEANING###
##Making all the stats per 90, and subsetting by position
clean_data <- {
  gw<-merge(x = gw, y = players[ , c("playername", "element_type", "team")],
            by = "playername", all.x=TRUE)
  gw<-merge(x=gw, y=x[ ,c("element", "xG90", "xA90")], by = "element", all.x=TRUE)
  gks<-subset(gw, element_type==1)
  defs<-subset(gw, element_type==2)
  mids<-subset(gw, element_type==3)
  fwds<-subset(gw, element_type==4)

#goalkeepers  
gk2<- gks %>%
    group_by(playername) %>%
    summarize( "Minutes" = sum(minutes), "BPS" =(sum(bps))/(sum(minutes)/90),
               "Owned" = mean(selected), "Saves" = (sum(saves))/(sum(minutes)/90),
               "Bonus" = (sum(bonus))/(sum(minutes)/90), "Team" = mean(team),
               "CS" = (sum(clean_sheets))/(sum(minutes)/90),
               "PPM" = ((sum(total_points)/mean(value)*10))/(sum(minutes)/90))
  gk3<-subset(gk2, gk2$Minutes >900)
  gk4<- mutate(gk3, BPS90=round((rank(BPS)/length(BPS)*100), digits=0), Minutes = round((rank(Minutes)/length(Minutes)*100), digits=0),
               Owned = round((rank(Owned)/length(Owned)*100), digits=0),Bonus90 = round((rank(Bonus)/length(Bonus)*100), digits=0),
               Saves90 =round((rank(Saves)/length(Saves)*100), digits=0),
               CS90 = round((rank(CS)/length(CS)*100), digits=0),
               PPM90 = round((rank(PPM)/length(PPM)*100), digits=0))
  gk4<-subset(gk4, select = -c(BPS, Saves, Bonus, CS, PPM))
  gk4$Team<-as.factor(gk4$Team)
  gk4 <- gk4[, c(1, 2, 3, 5, 6, 7, 8, 9,4)]
  
#defenders
  defs2<- defs %>%
    group_by(playername) %>%
    summarize( "Minutes" = sum(minutes), "BPS" =(sum(bps))/(sum(minutes)/90),
               "Owned" = mean(selected), "Assists" = (sum(assists))/(sum(minutes)/90),
               "Bonus" = (sum(bonus))/(sum(minutes)/90), "Team" = mean(team),
               "CS" = (sum(clean_sheets))/(sum(minutes)/90),
               "PPM" = ((sum(total_points)/mean(value)*10))/(sum(minutes)/90),
               "xA90" = mean(xA90))
  defs3<-subset(defs2, defs2$Minutes >900)
  defs4<- mutate(defs3, BPS90=round((rank(BPS)/length(BPS)*100), digits=0), Minutes = round((rank(Minutes)/length(Minutes)*100), digits=0),
                 Owned = round((rank(Owned)/length(Owned)*100), digits=0),Bonus90 = round((rank(Bonus)/length(Bonus)*100), digits=0),
                 Assists90 =round((rank(Assists)/length(Assists)*100), digits=0),
                 CS90 = round((rank(CS)/length(CS)*100), digits=0),
                 PPM90 = round((rank(PPM)/length(PPM)*100), digits=0), xA90 = round((rank(xA90)/length(xA90)*100), digits=0))
  defs4<-subset(defs4, select = -c(BPS, Assists, Bonus, CS, PPM))
  defs4$Team<-as.factor(defs4$Team)
  defs4 <- defs4[, c(1, 2, 3, 6, 7, 5, 8, 9, 10, 4)]
  
#midfielders
mids2<- mids %>%
    group_by(playername) %>%
    summarize( "Minutes" = sum(minutes), "BPS" =(sum(bps))/(sum(minutes)/90),
               "Owned" = mean(selected), "Assists" = (sum(assists))/(sum(minutes)/90),
               "Bonus" = (sum(bonus))/(sum(minutes)/90), "Team" = mean(team),
               "CS" = (sum(clean_sheets))/(sum(minutes)/90), "Goals"=(sum(goals_scored))/(sum(minutes)/90),
               "PPM" = ((sum(total_points)/mean(value)*10))/(sum(minutes)/90),
               "xG90"= mean(xG90), "xA90" = mean(xA90))
  mids3<-subset(mids2, mids2$Minutes >900)
  mids4<- mutate(mids3, BPS90=round((rank(BPS)/length(BPS)*100), digits=0), Minutes = round((rank(Minutes)/length(Minutes)*100), digits=0),
                 Owned = round((rank(Owned)/length(Owned)*100), digits=0),Bonus90 = round((rank(Bonus)/length(Bonus)*100), digits=0),
                 Assists90 =round((rank(Assists)/length(Assists)*100), digits=0), Goals90 =round((rank(Goals)/length(Goals)*100), digits=0),
                 CS90 = round((rank(CS)/length(CS)*100), digits=0),
                 PPM90 = round((rank(PPM)/length(PPM)*100), digits=0),
                 xG90 = round((rank(xG90)/length(xG90)*100), digits=0),
                 xA90 = round((rank(xA90)/length(xA90)*100), digits=0))
  mids4<-subset(mids4, select = -c(BPS, Assists, Bonus, CS, PPM, Goals))
  mids4$Team<-as.factor(mids4$Team)
  mids4 <- mids4[, c(1, 2, 3, 7, 8, 5, 10, 6, 9, 11,12,4)]

#forwards
fwds2<- fwds %>%
    group_by(playername) %>%
    summarize( "Minutes" = sum(minutes), "BPS" =(sum(bps))/(sum(minutes)/90),
               "Owned" = mean(selected), "Assists" = (sum(assists))/(sum(minutes)/90),
               "Bonus" = (sum(bonus))/(sum(minutes)/90), "Team" = mean(team),
               "Goals"=(sum(goals_scored))/(sum(minutes)/90),
               "PPM" = ((sum(total_points)/mean(value)*10))/(sum(minutes)/90),
               "xG90"= mean(xG90), "xA90" = mean(xA90))
  fwds3<-subset(fwds2, fwds2$Minutes >900)
  fwds4<- mutate(fwds3, BPS90=round((rank(BPS)/length(BPS)*100), digits=0), Minutes = round((rank(Minutes)/length(Minutes)*100), digits=0),
                 Owned = round((rank(Owned)/length(Owned)*100), digits=0),Bonus90 = round((rank(Bonus)/length(Bonus)*100), digits=0),
                 Assists90 =round((rank(Assists)/length(Assists)*100), digits=0), Goals90 =round((rank(Goals)/length(Goals)*100), digits=0),
                 PPM90 = round((rank(PPM)/length(PPM)*100), digits=0),
                 xG90 = round((rank(xG90)/length(xG90)*100), digits=0),
                 xA90 = round((rank(xA90)/length(xA90)*100), digits=0))
  fwds4<-subset(fwds4, select = -c(BPS, Assists, Bonus, PPM, Goals))
  fwds4$Team<-as.factor(fwds4$Team)
  fwds4 <- fwds4[, c(1, 2, 3, 7, 8, 5, 10, 6, 9, 11, 4)]
}

#radars have different metrics per position, so we need to select which position the player
#we want to plot plays in
mdata<-fwds4
mdata <- melt(as.data.frame(mdata, id="playername"))
uniq = unique(mdata$playername)

#gks
for (i in uniq) {
  
  # Start the plot
  p1 = ggplot(data=subset(mdata, playername== i), aes(x=as.factor(variable), y=value, fill = Team)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    # This add the bars with a blue color
    geom_hline(yintercept = 100, color = "darkgrey", size=1) + 
    geom_hline(yintercept = 50, color = "darkgrey", size = 1) + 
    geom_hline(yintercept = 25, color = "grey", size=0.2) + 
    geom_hline(yintercept = 75, color = "grey", size = 0.2) + 
    geom_bar(stat="identity") +
    
    # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
    ylim(-10,150) +
    
    # Custom the theme: no axis title and no cartesian grid
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank()    # Adjust the margin to make in sort labels are not truncated!
    ) +
    
    # This makes the coordinate polar instead of cartesian.
    coord_polar(start = 0) +
    
    # Add the labels, using the label_data dataframe that we have created before
    geom_text(data=subset(mdata, playername== i), aes(x=variable, y=135, label=variable), color="black",alpha=1, size=3, inherit.aes = FALSE ) +
    
    
    geom_line() +
    labs(title = i) + 
    theme(plot.title = element_text(size= 20, vjust=0, hjust = 0.5)) +
    geom_text(data=subset(mdata, playername== i),aes(x=variable,y=value-10,label= value), size=4, color="white") +
    labs(subtitle = "FPL percentile data from 19/20 (up to GW38+)\nMeasures are per90 where marked\n@fpl_radar") +
    theme(plot.subtitle = element_text(size= 12, vjust=0, hjust = 0.5)) + theme(legend.position = "none") + theme(plot.title = element_text(face="bold")) +
    labs (caption = "SP = save points per 90 \nCS = clean sheet points per 90 \nC2G = points lost for conceding 2 goals per 90") + scale_fill_manual(values = c("1" = "#EF0107",
                                                                                                                                                                "2"= "#a3c5e9",
                                                                                                                                                                "3" = "#B50E12",
                                                                                                                                                                "4" = "#0057B8",
                                                                                                                                                                "5" = "#6C1D45",
                                                                                                                                                                "6" = "#034694",
                                                                                                                                                                "7" = "#1B458F",
                                                                                                                                                                "8" = "#003399", 
                                                                                                                                                                "9" = "#003090",
                                                                                                                                                                "10" = "#C8102E",
                                                                                                                                                                "11" = "#6CABDD",
                                                                                                                                                                "12" = "#DA291C",
                                                                                                                                                                "13" = "#241F20",
                                                                                                                                                                "14" = "#00a650",
                                                                                                                                                                "15" = "#EE2737",
                                                                                                                                                                "16" = "#D71920",
                                                                                                                                                                "17" = "#132257",
                                                                                                                                                                "18" = "#ED2127",
                                                                                                                                                                "19" = "#7A263A",
                                                                                                                                                                "20" = "#FDB913")) 
   
  ggsave(p1, file=paste0("plot_", i,".png"), width=5, height=5, dpi=300)
}

#defenders
for (i in uniq) {
  p1 = ggplot(data=subset(mdata, playername == i), aes(x=as.factor(variable), y=value, fill = Team)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    # This add the bars with a blue color
    geom_hline(yintercept = 100, color = "darkgrey", size=1) + 
    geom_hline(yintercept = 50, color = "darkgrey", size = 1) + 
    geom_hline(yintercept = 25, color = "grey", size=0.2) + 
    geom_hline(yintercept = 75, color = "grey", size = 0.2) + 
    geom_bar(stat="identity") +
    
    # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
    ylim(-10,150) +
    
    # Custom the theme: no axis title and no cartesian grid
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank()    # Adjust the margin to make in sort labels are not truncated!
    ) +
    
    # This makes the coordinate polar instead of cartesian.
    coord_polar(start = 0) +
    
    # Add the labels, using the label_data dataframe that we have created before
    geom_text(data=subset(mdata, playername == i), aes(x=variable, y=135, label=variable), color="black",alpha=1, size=3, inherit.aes = FALSE ) +
    
    
    geom_line() +
    labs(title = i) + 
    theme(plot.title = element_text(size= 20, vjust=0, hjust = 0.5)) +
    geom_text(data=subset(mdata, playername == i),aes(x=variable,y=value-10,label= value), size=4, color="white") +
    labs(subtitle = "FPL percentile data from 19/20 (up to GW38+)\nMeasures are per90 where marked\n@fpl_radar") +
    theme(plot.subtitle = element_text(size= 12, vjust=0, hjust = 0.5)) + theme(legend.position = "none") + theme(plot.title = element_text(face="bold")) +
    labs (caption = "CS = clean sheet points per 90 \nC2G = points lost for conceding 2 goals per 90") + scale_fill_manual(values = c("1" = "#EF0107",
                                                                                                                                      "2" = "#a3c5e9",
                                                                                                                                      "3" = "#B50E12",
                                                                                                                                      "4" = "#0057B8",
                                                                                                                                      "5" = "#6C1D45",
                                                                                                                                      "6" = "#034694",
                                                                                                                                      "7" = "#1B458F",
                                                                                                                                      "8" = "#003399", 
                                                                                                                                      "9" = "#003090",
                                                                                                                                      "10" = "#C8102E",
                                                                                                                                      "11" = "#6CABDD",
                                                                                                                                      "12" = "#DA291C",
                                                                                                                                      "13" = "#241F20",
                                                                                                                                      "14" = "#00a650",
                                                                                                                                      "15" = "#EE2737",
                                                                                                                                      "16" = "#D71920",
                                                                                                                                      "17" = "#132257",
                                                                                                                                      "18" = "#ED2127",
                                                                                                                                      "19" = "#7A263A",
                                                                                                                                      "20" = "#FDB913"))

  ggsave(p1, file=paste0("plot_", i,".png"), width=5, height=5, dpi=300)
}

#midfielders
for (i in uniq) {
  
  # Start the plot
  p1 = ggplot(data=subset(mdata, playername == i), aes(x=as.factor(variable), y=value, fill = Team)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar

        # This add the bars with a blue color
    geom_hline(yintercept = 100, color = "darkgrey", size=1) + 
    geom_hline(yintercept = 50, color = "darkgrey", size = 1) + 
    geom_hline(yintercept = 25, color = "grey", size=0.2) + 
    geom_hline(yintercept = 75, color = "grey", size = 0.2) + 
    geom_bar(stat="identity") +
    
    # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
    ylim(-10,150) +
    
    # Custom the theme: no axis title and no cartesian grid
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank()    # Adjust the margin to make in sort labels are not truncated!
    ) +
    
    # This makes the coordinate polar instead of cartesian.
    coord_polar(start = 0) +
    
    # Add the labels, using the label_data dataframe that we have created before
    geom_text(data=subset(mdata, playername == i), aes(x=variable, y=135, label=variable), color="black",alpha=1, size=3, inherit.aes = FALSE ) +
    
    
    geom_line() +
    labs(title = i) + 
    theme(plot.title = element_text(size= 20, vjust=0, hjust = 0.5)) +
    geom_text(data=subset(mdata, playername == i),aes(x=variable,y=value-10,label= value), size=4, color="white") +
    labs(subtitle = "FPL percentile data from 19/20 (up to GW38+)\nMeasures are per90 where marked\n@fpl_radar") +
    theme(plot.subtitle = element_text(size= 12, vjust=0, hjust = 0.5)) + theme(legend.position = "none") + theme(plot.title = element_text(face="bold")) +
    labs(caption = "CS = clean sheet points per 90") + scale_fill_manual(values = c("1" = "#EF0107",
                                                                                     "2" = "#a3c5e9",
                                                                                     "3" = "#B50E12",
                                                                                     "4" = "#0057B8",
                                                                                     "5" = "#6C1D45",
                                                                                     "6" = "#034694",
                                                                                     "7" = "#1B458F",
                                                                                     "8" = "#003399", 
                                                                                     "9" = "#003090",
                                                                                     "10" = "#C8102E",
                                                                                     "11" = "#6CABDD",
                                                                                     "12" = "#DA291C",
                                                                                     "13" = "#241F20",
                                                                                     "14" = "#00a650",
                                                                                     "15" = "#EE2737",
                                                                                     "16" = "#D71920",
                                                                                     "17" = "#132257",
                                                                                     "18" = "#ED2127",
                                                                                     "19" = "#7A263A",
                                                                                     "20" = "#FDB913"))
  ggsave(p1, file=paste0("plot_", i,".png"), width=5, height=5, dpi=300)
}

#forwards
for (i in uniq) {
  
  # Start the plot
  p1 = ggplot(data=subset(mdata, playername == i), aes(x=as.factor(variable), y=value, fill = Team)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    # This add the bars with a blue color
    geom_hline(yintercept = 100, color = "darkgrey", size=1) + 
    geom_hline(yintercept = 50, color = "darkgrey", size = 1) + 
    geom_hline(yintercept = 25, color = "grey", size=0.2) + 
    geom_hline(yintercept = 75, color = "grey", size = 0.2) + 
    geom_bar(stat="identity") +
    
    # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
    ylim(-10,150) +
    
    # Custom the theme: no axis title and no cartesian grid
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank()    # Adjust the margin to make in sort labels are not truncated!
    ) +
    
    # This makes the coordinate polar instead of cartesian.
    coord_polar(start = 0) +
    
    # Add the labels, using the label_data dataframe that we have created before
    geom_text(data=subset(mdata, playername == i), aes(x=variable, y=135, label=variable), color="black",alpha=1, size=3, inherit.aes = FALSE ) +
    
    
    geom_line() +
    labs(title = i) + 
    theme(plot.title = element_text(size= 20, vjust=0, hjust = 0.5)) +
    geom_text(data=subset(mdata, playername== i),aes(x=variable,y=value-10,label= value), size=4, color="white") +
    labs(subtitle = "FPL percentile data from 19/20 (up to GW38+)\nMeasures are per90 where marked\n@fpl_radar") +
    theme(plot.subtitle = element_text(size= 12, vjust=0, hjust = 0.5)) + theme(legend.position = "none") + theme(plot.title = element_text(face="bold")) +
    scale_fill_manual(values = c("1" = "#EF0107",
                                 "2" = "#a3c5e9",
                                 "3" = "#B50E12",
                                 "4" = "#0057B8",
                                 "5" = "#6C1D45",
                                 "6" = "#034694",
                                 "7" = "#1B458F",
                                 "8" = "#003399", 
                                 "9" = "#003090",
                                 "10" = "#C8102E",
                                 "11" = "#6CABDD",
                                 "12" = "#DA291C",
                                 "13" = "#241F20",
                                 "14" = "#00a650",
                                 "15" = "#EE2737",
                                 "16" = "#D71920",
                                 "17" = "#132257",
                                 "18" = "#ED2127",
                                 "19" = "#7A263A",
                                 "20" = "#FDB913")) 

  ggsave(p1, file=paste0("plot_", i,".png"), width=5, height=5, dpi=300)
}

