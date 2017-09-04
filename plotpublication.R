
library(dplyr)
library(ggplot2)
library(cowplot)

sortvec<-function(){
    c(
  "1" = "rejection",
  "2" = "submission",
  "3" = "full_submission",
  "4" =  "reviews",
  "5" =  "reviews_minor",
  "6" =  "accepted",
  "7" =  "press"
  )
}

translatoraction<-function(x){
  sapply(x,function(i){
    which(sortvec() %in% i)
  })
}

pubs=read.table("publications_day_record.tsv",header=TRUE,fill=TRUE) %>%
  mutate(time=as.Date(time),
         action.num=translatoraction(action))


p<-ggplot(pubs) +
  geom_line(aes(y=action.num,x=time,group=paper,color=paper)) +
  labs(y="",x="")+
  geom_point(aes(y=action.num,x=time,group=paper,color=paper)) +
  labs(y="",x="")+
  scale_y_continuous(breaks=as.numeric(names(sortvec())),labels=as.character(sortvec() ) )+
  scale_x_date(date_breaks="1 month")+
  # theme_bw()+
  theme(axis.text.x = element_text(angle = 45 ,hjust = TRUE))
p

# save_plot(filename = "paperrec.pdf",p,base_height = 5,base_width = 15)
save_plot(filename = "paperrec.jpg",p,base_height = 5,base_width = 15)
