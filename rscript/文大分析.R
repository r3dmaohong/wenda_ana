rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
path<-"D:\\abc\\wjhong\\projects\\wenda_ana"
setwd(path)

trim <-function(x){
  gsub("^\\(+|\\)+$", "", x)
}
library(ggplot2)

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

filename='文大畢業流向調查分析結果.csv'
people = read.csv('103學年度執行畢業生流向調查-data.csv',stringsAsFactors=F)

##unique(unlist(c(people[,20:38])))
for(i in 20:38){
  people[which(grepl('非常',people[,i])),i] = '非常重要(5分)'
  people[which(grepl('完全',people[,i])),i] = '完全不重要(1分)'
  people[which(grepl('普通',people[,i])),i] = '普通(3分)'
  people[which(grepl('4分',people[,i])),i] = '重要(4分)'
  people[which(grepl('2分',people[,i])),i] = '不太重要(2分)'
  people[which(grepl('不太重要',people[,i])),i] = '不太重要(2分)'
}

drops = c()
for(i in 1:ncol(people)){
  if(length(which(is.na(people[,i])))==length(people[,i])){
    drops = c(drops,i)
  }
}
people = people[,!(1:ncol(people) %in% drops)]

##整體分析
if(T){
  for(i in 1:ncol(people)){
    if(i %in% which(grepl('複選',colnames(people)))){
      tmp = people[,i]
      tmp = trim(tmp)
      tmp = unlist(strsplit(tmp,','))
      
      tmp = as.data.frame(table(tmp),stringsAsFactors=F)
      colnames(tmp)[1] = colnames(people)[i]
      tmp = tmp[order(-tmp[,2]),]
      tmp[which(tmp[,1]==''),1] = '未填寫'
      n_row = 1:nrow(tmp) 
      tmp = tmp[c(n_row[which(!(n_row %in% which(tmp[,1]=='未填寫')))],which(tmp[,1]=='未填寫')),]
      tmp$percentage = tmp[,2]/tmp_sum 
      ###
      # Summarize to get counts and percentages
      
      if(toString(which(grepl('[0-9]',tmp[,1])))!=''){
        ##如有量尺，就用其作排序
        #q_num = unique(na.omit(as.numeric(unlist(strsplit(unlist(tmp[,1]), "[^0-9]+")))))
        row_order = c()
        for(nrow_order in 1:nrow(tmp)){
          row_order = c(row_order,paste0(as.numeric(unlist(strsplit(unlist(tmp[nrow_order,1]), "[^0-9]+")))[which(!is.na(as.numeric(unlist(strsplit(unlist(tmp[nrow_order,1]), "[^0-9]+")))))],collapse=''))
          
        }
        ##tmp = tmp[order(row_order),]
        tmp = tmp[order(as.numeric(row_order)),]
        tmp = rbind(tmp[which(!grepl('以上$',tmp[,1])),],tmp[which(grepl('以上$',tmp[,1])),])
        tmp = rbind(tmp[which(grepl('[0-9]',tmp[,1])),],tmp[which(!grepl('[0-9]',tmp[,1])),])
      }
      
      ##塞入level讓他繪圖時順序不會亂掉
      tmp[,1]=factor(tmp[,1],levels=tmp[,1])
      
      
    }else{
      tmp = as.data.frame(table(people[,i]),stringsAsFactors=F)
      colnames(tmp)[1] = colnames(people)[i]
      tmp = tmp[order(-tmp[,2]),]
      tmp[which(tmp[,1]==''),1] = '未填寫'
      n_row = 1:nrow(tmp) 
      tmp = tmp[c(n_row[which(!(n_row %in% which(tmp[,1]=='未填寫')))],which(tmp[,1]=='未填寫')),]
      tmp$percentage = tmp[,2]/sum(tmp[,2])
      
      tmp_sum = sum(tmp[,2])
      ###
      if(toString(which(grepl('[0-9]',tmp[,1])))!=''){
        ##如有量尺，就用其作排序
        #q_num = unique(na.omit(as.numeric(unlist(strsplit(unlist(tmp[,1]), "[^0-9]+")))))
        row_order = c()
        for(nrow_order in 1:nrow(tmp)){
          row_order = c(row_order,paste0(as.numeric(unlist(strsplit(unlist(tmp[nrow_order,1]), "[^0-9]+")))[which(!is.na(as.numeric(unlist(strsplit(unlist(tmp[nrow_order,1]), "[^0-9]+")))))],collapse=''))
          
        }
        ##tmp = tmp[order(row_order),]
        tmp = tmp[order(as.numeric(row_order)),]
        tmp = rbind(tmp[which(!grepl('以上$',tmp[,1])),],tmp[which(grepl('以上$',tmp[,1])),])
        tmp = rbind(tmp[which(grepl('[0-9]',tmp[,1])),],tmp[which(!grepl('[0-9]',tmp[,1])),])
      }
      
      # Summarize to get counts and percentages
      ##塞入level讓他繪圖時順序不會亂掉
      tmp[,1]=factor(tmp[,1],levels=tmp[,1])
    }
    
    ##筆記
    if(F){
      ##匯入windos字體庫?
      library(extrafont)
      loadfonts(device="win")
      
      #列出字體名稱
      x = list.files(file.path(Sys.getenv('SystemRoot'), 'Fonts'))
      grep('\\.ttf$', x, ignore.case=TRUE, value=TRUE)
      #
      
      #中文字測試
      library(Cairo);
      CairoPNG(file = "mtcars_plot.png");
      plot(1:10,1:10,type="n");
      text(2,10,"宋體",family="SimSun");
      text(2,8,"黑體",family="SimHei");
      text(2,6,"楷體",family="KaiTi_GB2312");
      text(2,4,"隸書",family="LiSu");
      text(2,2,"幼圓",family="YouYuan");
      text(6,10,"Arial",family="Arial");
      text(6,8,"Times New Roman",family="Times New Roman");
      text(6,6,"Courier New",family="Courier New");
      text(6,4,"Consolas",family="Consolas");
      text(6,2,"Symbol",family="Symbol");
      dev.off();
    }
    
    rownames(tmp)=1:nrow(tmp)
    
    if(nrow(tmp)<=6){
      myPlot <- ggplot(data=tmp, aes(x=tmp[,1], y=tmp[,2])) +
        geom_bar(stat="identity") + 
        ##paste0(format(100*percentage, digits=2, drop0trailing=TRUE)
        geom_text(aes(label = paste0(format(round(100*percentage, 2), nsmall=2),' %'),
                      y= Freq ), vjust = -.5, size=5)+
        scale_y_continuous(breaks = round(seq(0, roundUpNice(max(tmp$Freq)), by = roundUpNice(roundUpNice(max(tmp$Freq))/8)),1))+
        labs(y = "次數", x = "類別")+
        theme(text = element_text(size=15))
      ##直接轉成躺的bar chart  
      ##  + coord_flip()
    }else{
      if(F){
        myPlot <- ggplot(data=tmp, aes(x=tmp[,1], y=tmp[,2])) +
          geom_bar(stat="identity") + 
          geom_text(aes(label = paste0(format(100*percentage, digits=2, drop0trailing=TRUE),' %'),
                        y= Freq ), hjust = .0005, size=5)+
          scale_y_continuous(breaks = round(seq(roundUpNice(min(tmp$Freq)), roundUpNice(max(tmp$Freq)), by = roundUpNice((roundUpNice(max(tmp$Freq))-roundUpNice(min(tmp$Freq)))/8)),1))+
          labs(y = "次數", x = "類別")+
          theme(text = element_text(size=15)) +
          coord_flip()
      }
      
      if(T){
        
        ##調整班轉後bar chart順序
        tmp = tmp[order(-as.numeric(rownames(tmp))),]
        tmp[,1]=factor(tmp[,1],levels=tmp[,1])
        myPlot <- ggplot(data=tmp, aes(x=tmp[,1], y=tmp[,2])) +
          geom_bar(stat="identity",position = "dodge") + 
          geom_text(aes(label = paste0(format(round(100*percentage, 2), nsmall=2),' %')
          ), hjust = .0001, size=5)+
          scale_y_continuous(limits = c(0,roundUpNice(max(tmp$Freq)))) +
          labs(y = "次數", x = "類別")+
          theme(text = element_text(size=15))+
          coord_flip()
        
      }
    }
    png_name = paste0('png//png',i,'.png')
    #ggsave(filename=png_name, plot=myPlot)
    
    ##if(nrow())
    if(nrow(tmp)<=6){
      width=600
      height=400
    }else if(nrow(tmp)>30){
      width=600
      height=800
    }else{
      width = 700 ##+ (nrow(tmp)-6)*25
      height = 400 + (nrow(tmp)-6)*25
    }
    png(png_name,width=width,height=height)
    print(myPlot)
    dev.off()
    
    tmp = tmp[order(as.numeric(rownames(tmp))),]
    ##
    tmp[,1] <- unlist(lapply(tmp[,1], as.character))
    tmp = rbind(tmp,c('總計',sum(tmp[,2]),1))
    #ifelse(i==1,write.table(tmp, filename, col.names=TRUE, sep=","),write.table(tmp, filename, col.names=TRUE, sep=",",append=T))
    if(i==1){
      write.table(tmp, filename, col.names=TRUE, row.names=F, sep=",")
    }else{
      write.table(tmp, filename, col.names=TRUE, row.names=F, sep=",",append=T)
    }
  }
  
}

##交叉分析測試
if(T){
  library(plyr)
  filename = '文大畢業流向調查交叉分析結果.csv'
  backgournd_v_index = c(1)
  for(bvi_l in 1:length(backgournd_v_index)){
    for(i in (1:ncol(people))[!((1:ncol(people)) %in% backgournd_v_index)]){
      if(i %in% which(grepl('複選',colnames(people)))){
        
        for(tmp_div_n in 1:length(unique(people[,backgournd_v_index[bvi_l]]))){
          tmp_div = people[which(people[,1]==unique(people[tmp_div_n,backgournd_v_index[bvi_l]])),i]
          tmp_div = unlist(strsplit(trim(tmp_div),','))
          tmp_div = as.data.frame(table(tmp_div),stringsAsFactors=F)
          tmp_div = tmp_div[order(-tmp_div$Freq),]
          tmp_div = cbind(unique(people[,backgournd_v_index[bvi_l]])[tmp_div_n],tmp_div)
          colnames(tmp_div) = c(colnames(people)[backgournd_v_index[bvi_l]], colnames(people)[i],'Freq')
          tmp_div = tmp_div
          tmp_div[which(tmp_div[,2]==''), 2] = '未填寫'
          
          n_row = 1:nrow(tmp_div) 
          tmp_div = tmp_div[c(n_row[which(!(n_row %in% which(tmp_div[,1]=='未填寫')))],which(tmp_div[,1]=='未填寫')),]
          tmp_div$percentage = tmp_div[,3]/sum(tmp_div[,3]) 
          
          if(toString(which(grepl('[0-9]',tmp_div[,2])))!=''){
            ##如有量尺，就用其作排序
            #q_num = unique(na.omit(as.numeric(unlist(strsplit(unlist(tmp_div[,1]), "[^0-9]+")))))
            row_order = c()
            for(nrow_order in 1:nrow(tmp_div)){
              row_order = c(row_order,paste0(as.numeric(unlist(strsplit(unlist(tmp_div[nrow_order,2]), "[^0-9]+")))[which(!is.na(as.numeric(unlist(strsplit(unlist(tmp_div[nrow_order,2]), "[^0-9]+")))))],collapse=''))
              
            }
            ##tmp_div = tmp_div[order(row_order),]
            tmp_div = tmp_div[order(as.numeric(row_order)),]
            tmp_div = rbind(tmp_div[which(!grepl('以上$',tmp_div[,2])),],tmp_div[which(grepl('以上$',tmp_div[,2])),])
            tmp_div = rbind(tmp_div[which(grepl('[0-9]',tmp_div[,2])),],tmp_div[which(!grepl('[0-9]',tmp_div[,2])),])
            
          }
          if(tmp_div_n==1){
            tmp = tmp_div
          }else{
            tmp = rbind(tmp,tmp_div)
          }
        }
        rownames(tmp)=1:nrow(tmp)
        
        ##塞入level讓他繪圖時順序不會亂掉
        ##tmp[,1]=factor(tmp[,1],levels=tmp[,1])
        
        
      }else{
        tmp = ddply(people, c(backgournd_v_index[bvi_l], i), nrow)
        
        tmp = tmp[order(tmp[,1],-tmp[,3]),]
        tmp[which(tmp[,2]==''),2] = '未填寫'
        n_row = 1:nrow(tmp) 
        tmp = tmp[c(n_row[which(!(n_row %in% which(tmp[,2]=='未填寫')))],which(tmp[,2]=='未填寫')),]
        #tmp = tmp[order(tmp[,1]),]
        
        tmp[,3] = as.numeric(tmp[,3])
        tmp$percentage = 0
        for(tmp_div in 1:nrow(tmp)){
          tmp[tmp_div,4] = tmp[tmp_div,3]/sum(tmp[which(tmp[,1]==tmp[tmp_div,1]),3])
        }
        ###
        if(toString(which(grepl('[0-9]',tmp[,2])))!=''){
          ##如有量尺，就用其作排序
          #q_num = unique(na.omit(as.numeric(unlist(strsplit(unlist(tmp[,1]), "[^0-9]+")))))
          row_order = c()
          for(nrow_order in 1:nrow(tmp)){
            row_order = c(row_order,paste0(as.numeric(unlist(strsplit(unlist(tmp[nrow_order,2]), "[^0-9]+")))[which(!is.na(as.numeric(unlist(strsplit(unlist(tmp[nrow_order,2]), "[^0-9]+")))))],collapse=''))
            
          }
          ##tmp = tmp[order(row_order),]
          tmp = tmp[order(as.numeric(row_order)),]
          tmp = rbind(tmp[which(!grepl('以上$',tmp[,2])),],tmp[which(grepl('以上$',tmp[,2])),])
          tmp = rbind(tmp[which(grepl('[0-9]',tmp[,2])),],tmp[which(!grepl('[0-9]',tmp[,2])),])
        }
        rownames(tmp)=1:nrow(tmp)
        colnames(tmp)[3] = 'Freq'
        
      }
        ##共同從這開始好了
        ##調整班轉後bar chart順序
        tmp = tmp[order(-as.numeric(rownames(tmp))),]
        for(tmp_div_n in 1:length(unique(tmp[,1]))){
          tmp_div = tmp[which(tmp[,1]==unique(tmp[,1])[tmp_div_n]),]
          tmp_div_name = unique(tmp[,1])[tmp_div_n]
          tmp_div = tmp_div[,2:4]
          tmp_div[,1]=factor(tmp_div[,1],levels=tmp_div[,1])
          myPlot <- ggplot(data=tmp_div, aes(x=tmp_div[,1], y=tmp_div[,2])) +
            geom_bar(stat="identity",position = "dodge") + 
            geom_text(aes(label = paste0(format(round(100*percentage, 2), nsmall=2),' %')
            ), hjust = .0001, size=5)+
            scale_y_continuous(limits = c(0,roundUpNice(max(tmp_div$Freq)+1))) +
            labs(y = "次數", x = "類別")+
            theme(text = element_text(size=15))+
            coord_flip()
          
          png_name = paste0('png//交叉//',tmp_div_name,'_',i,'.png')
          #ggsave(filename=png_name, plot=myPlot)
          
          ##if(nrow())
          if(nrow(tmp_div)<=6){
            width=600
            height=400
          }else if(nrow(tmp_div)>30){
            width=600
            height=800
          }else{
            width = 700 ##+ (nrow(tmp_div)-6)*25
            height = 400 + (nrow(tmp_div)-6)*25
          }
          png(png_name,width=width,height=height)
          print(myPlot)
          dev.off()
        }
        
        tmp = tmp[order(as.numeric(rownames(tmp))),]
        
        ##
        for(tmp_div_n in 1:length(unique(tmp[,1]))){
          # tmp[tmp_div,4] = tmp[tmp_div,3]/sum(tmp[which(tmp[,1]==tmp[tmp_div,1]),3])
          tmp = rbind(tmp,c(unique(tmp[,1])[tmp_div_n],'總計',sum(as.numeric(tmp[which(tmp[,1]==unique(tmp[,1])[tmp_div_n]),3])),1))
        }
        tmp = tmp[order(tmp[,1]),]
        
        #ifelse(i==1,write.table(tmp, filename, col.names=TRUE, sep=","),write.table(tmp, filename, col.names=TRUE, sep=",",append=T))
        if(i==1){
          write.table(tmp, filename, col.names=TRUE, row.names=F, sep=",")
        }else{
          write.table(tmp, filename, col.names=TRUE, row.names=F, sep=",",append=T)
        }
      }
    }
}
