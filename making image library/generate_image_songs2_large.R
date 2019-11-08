rm(list=ls())
library(tuneR)
library(seewave)

path_recs='recordings/'
pic_width=300
pic_height=150
fmin=1500
fmax=10000

db=read.csv('positions.csv',header=T, stringsAsFactors = F)

## sampling
db_samples=data.frame()
sample_length=5 #in sec  
sample_step=3 #in sec

files=unique(db$file)

# making samples with at least a small part of song on it
for(file_n in 1:length(files)){#:length(files)
  db2=db[which(db$file==files[file_n]),]
  rows=1:nrow(db2)
  
  for(row_n in rows){
    t_start=db2$song_start[row_n]-runif(1)*sample_length*0.8
    t_end=db2$song_end[row_n]+runif(1)*sample_length*0.8
    
    if((t_end-t_start)>sample_length){
      sample_starts=seq(t_start,t_end-sample_length,sample_step)}
    else{
      sample_starts=t_start
    }
    sample_ends=sample_starts+sample_length
    db_sample1=data.frame(file=files[file_n],sample_starts,sample_ends)
    db_samples=rbind(db_samples,db_sample1)
  }
}

# generating coordinates on image

db_samples$n_i=1:nrow(db_samples)
for(i in 1:nrow(db_samples)){#1:nrow(db_samples)
  n_i=db_samples$n_i[i]
  t1=db_samples$sample_starts[i]
  t2=db_samples$sample_ends[i]
  file1=as.character(db_samples$file[i])
  db3=db[which(db$file==file1),]
  rows=which((db3$song_start>=t1 & db3$song_start<t2) | 
               (db3$song_end>=t1 & db3$song_end<t2))
  print(c(i,rows))
  if(length(rows)>0){
   
    sink(paste("image_library\\",n_i,".txt",sep=""))
    
    for(j in 1:length(rows)){
      
      # time
      s1=db3$song_start[rows[j]]-t1
      if(s1<0){s1=0}
      s1=s1/sample_length
      
      s2=db3$song_end[rows[j]]-t1
      if(s2>sample_length){s2=sample_length}
      s2=s2/sample_length
      
      # frequency
      songID=as.character(db3$songID[rows[j]])
      
      s3=0
      s4=1
      
      z1=(s1+s2)/2
      z2=1-(s3+s4)/2
      z3=s2-s1
      z4=s4-s3
      
    
      cat(paste("song",z1,z2,z3,z4,"\n"))
    }
    
    sink()

  }
}


db_samples$file=as.character(db_samples$file)

# generating the images

res=data.frame()

for(sample1 in 1:nrow(db_samples)){ #:nrow(db_samples)
  
  print(paste(sample1,"/",nrow(db_samples)))
  n_i=db_samples$n_i[sample1]
  jpeg(filename = paste("image_library\\",n_i,".jpg",sep=""),
       width = pic_width, height = pic_height, units = "px", pointsize = 30,
       quality = 100, bg = "white")
  par(mar = c(0,0,0,0))
  wav1=readWave(paste0(path_recs,db_samples$file[sample1]), from = db_samples$sample_start[sample1], to = db_samples$sample_end[sample1], 
                units = "seconds", header = FALSE, toWaveMC = NULL)
  

  # making pictures ##
  fft=512
  
  wav1@left=wav1@left/10
  S=spectro(wav1,f=wav1@samp.rate,wl=fft,ovl=50,plot=F,norm=F) # norm=F !!!
  
  S2=S[['amp']]
  res=rbind(res,data.frame(max(S2),min(S2)))
  
  row_min=fft/2/(wav1@samp.rate/2)*fmin
  row_max=fft/2/(wav1@samp.rate/2)*fmax
  S3=S2[row_min:row_max,]
  minlim=-30
  maxlim=50
  S3[S3>maxlim]=maxlim
  S3[S3<minlim]=minlim
  S4=(S3-minlim)/(maxlim-minlim)*255
  
  #S5 <- resize(S4, w = pic_height, h = pic_width)
  
  breaks1=seq(0,255,1)
  image(t(S4),col = heat.colors(length(breaks1)-1),breaks=breaks1,
        xaxt="n",yaxt="n")
  
  dev.off()
}



# generate train_songs.txt and test_songs.txt
files=dir("image_library/",pattern="*.jpg")
order=sample(length(files))
n_tests=ceiling(length(files)/10)
files_test=paste("data/image_library/",files[order][1:n_tests],sep="")
files_train=paste("data/image_library/",files[order][(n_tests+1):length(files)],sep="")

sink("train_songs.txt")
for(i in 1:length(files_train)){
  cat(files_train[i]);cat("\n")
};sink()

sink("test_songs.txt")
for(i in 1:length(files_test)){
  cat(files_test[i]);cat("\n")
};sink()
