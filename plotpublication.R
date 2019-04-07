# pubs<-read.table("publications_day_record.tsv",header=T)

#####

nicewrap<-function(){
      theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))
}


#####
library(dplyr)
library(ggplot2)
library(cowplot)

sortvec<-function(){
    c(
  "1" = "rejection",
  "2" = "bioRxiv",
  "3" = "submission",
  "4" = "full_submission",
  "5" =  "reviews",
  "6" =  "reviews_minor",
  "7" =  "accepted",
  "8" =  "press"
  )
}

translatoraction<-function(x){
  sapply(x,function(i){
    which(sortvec() %in% i)
  })
}


pubs=read.table("publications_day_record.tsv",header=TRUE,fill=TRUE) %>%
  mutate(time=as.Date(time),
         action.num=translatoraction(action)) %>%
  mutate(
          publicationdate= publicationdate[paper,2],
          startdate= startdate[paper,2]
         ) %>%
  mutate(
          timescaled= time-startdate,
          timescaledend=publicationdate-startdate
          )
pubs


accepteddate=group_by(pubs, paper)%>%
                   summarise(
                             isaccepted=any(action =="accepted"),
                             lasttime=tail(time,1)
                             ) %>% 
                    mutate(accepteddate=ifelse(isaccepted ==T, lasttime, NA)) %>%
                   data.frame()
accepteddate

publicationdate=(group_by(pubs, paper)%>%summarise(lasttime=tail(time,1)) ) %>% data.frame()
  rownames(publicationdate)=publicationdate$paper
  
startdate=(group_by(pubs, paper)%>%summarise(lasttime=head(time,1)) ) %>% data.frame()
  rownames(startdate)=startdate$paper

  

# p<-ggplot(pubs) +
#   geom_line(aes(y=action.num,x=time,group=paper,color=paper)) +
#   labs(y="",x="")+
#   geom_point(aes(y=action.num,x=time,group=paper,color=paper)) +
#   labs(y="",x="")+
#   scale_y_continuous(breaks=as.numeric(names(sortvec())),labels=as.character(sortvec() ) )+
#   scale_x_date(date_breaks="1 month")+
#   # theme_bw()+
#   theme(axis.text.x = element_text(angle = 45 ,hjust = TRUE)) +
#   # geom_point(data=publicationdate,aes( y=paper,x = lasttime ))+
#   facet_wrap(~paper, ncol=1)
#   
# p
# save_plot(filename = "paperrec.jpg",p,base_height = 5,base_width = 15)


p<-ggplot(pubs) +
  geom_line(aes(y=action.num,x=timescaled,group=paper,color=paper)) +
  geom_point(aes(y=action.num,x=timescaled,group=paper,color=paper)) +
  labs(y="",x="days since first submission")+
  scale_y_continuous(breaks=as.numeric(names(sortvec())),labels=as.character(sortvec() ) )+
  scale_x_continuous(breaks = seq(0,700,by=30))+
  # theme_bw()+
  theme(axis.text.x = element_text(angle = 45 ,hjust = TRUE)) +
  geom_segment(data=dplyr::filter(pubs, action=="accepted"),aes( y=1,yend= 8, x = timescaledend,xend=timescaledend )) +
  facet_wrap(~paper, ncol=1) + 
  theme_bw() + theme(panel.grid.minor=element_blank()) +      
        theme(
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))

save_plot(filename = "paperrec_separated.jpg",p,base_height = 1*length(unique(pubs$paper)),base_width = 8)

  


  
