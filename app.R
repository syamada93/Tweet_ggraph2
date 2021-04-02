#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

if(!require(data.table))
    install.packages("data.table")
library(data.table)

if(!require(dplyr))
    install.packages("dplyr")
library(dplyr)

if(!require(tidyr))
    install.packages("tidyr")
library(tidyr)

if(!require(stringi))
    install.packages("stringi")
library(stringi)

if(!require(ggplot2))
    install.packages("ggplot2")
library(ggplot2)

if(!require(ggraph))
    install.packages("ggraph")
library(ggraph)

if(!require(tidygraph))
    install.packages("tidygraph")
library(tidygraph)

if(!require(rtweet))
    install.packages("rtweet")
library(rtweet)

if(!require(leaflet))
    install.packages("leaflet")
library(leaflet)

if(!require(leafletCN))
    install.packages("leafletCN")
library(leafletCN)

if(!require(maptools))
    install.packages("maptools")
library(maptools)

if(!require(sf))
    install.packages("sf")
library(sf)

if(!require(rsconnect))
    install.packages("rsconnect")
library(rsconnect)

if(!require(dygraphs))
    install.packages("dygraphs")
library(dygraphs)

if(!require(RMeCab))
    install.packages("RMeCab", repos = "https://rmecab.jp/R") 
library(RMeCab)


# write.csv(emojis,"Emojis.csv",row.names = F)
ED <-
    fread("Emojis.csv") %>%
    rename(Unicode=code) %>%
    mutate(Uni=gsub("U\\+","U\\\\+",Unicode)) %>%
    left_join(emojis) %>%
    filter(grepl("<U",Unicode))

ggColorHue <- function(n, l=65) {
    hues <- seq(15, 375, length=n+1)
    hcl(h=hues, l=l, c=100)[1:n]
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("ツイート数の推移"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            column(6,textInput("wd",
                      "抽出する単語",
                      "コロナ"),
            actionButton("button1","抽出開始")),
            column(6,
                   column(6,dateInput("ymd",
                      "共起ネットワークの日付",
                      Sys.Date())),
                   column(6,selectInput("h",
                        "時間",
                        1:23,
                        12)),
            actionButton("button2","共起ネットワーク作成")),
            width = 12
            # submitButton()
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            column(6,dygraphOutput("Mdy"),
                   dygraphOutput("Hdy")),
                   # plotOutput("Dline0")),
            
            column(6,tabsetPanel(
                tabPanel("最新",plotOutput("ggraph",height = "800px")),
                tabPanel("選択",plotOutput("ggraph2",height = "800px")))),
            width = 12
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    refreshPlot0 <- reactiveTimer(intervalMs = 1)
    # refreshPlot <- reactiveTimer(intervalMs = 60000)
    
    TDC <- data.frame()
    dc=""
    wd="コロナ"
    wd="雨"
    
    WD <- eventReactive(input$button1,{
        input$wd
    })
    observeEvent(input$button1, {
        if(file.exists("TDC.csv"))
            file.remove("TDC.csv")
        if(file.exists("dc.txt"))
            file.remove("dc.txt")
    })
    
    observe({
        wd=WD()
        refreshPlot0()
        if(file.exists("TDC.csv"))
            TDC <- fread("TDC.csv") %>%
            data.frame()
        if(file.exists("dc.txt"))
            dc <- as.character(fread("dc.txt")$V1)
        # print(Sys.time())
        if(as.numeric(format(Sys.time(),"%S"))>5)
            return()
        print(Sys.time())
        # refreshPlot()
        td <- search_tweets(wd,lang = "ja",n = 1000,include_rts = T)#,retryonratelimit = T)
        
        if(nrow(td)==0)
            return()
        
        tds <-
            td %>%
            mutate(JTime=as.POSIXct(format(created_at, tz="Japan"))) %>%
            mutate(Tweet=gsub("　","",text)) %>%
            mutate(Tweet=stri_trans_nfkc(Tweet)) %>%
            # mutate(Tweet=gsub("\"","",Tweet)) %>%
            mutate(Tweet=gsub("http[[:print:]]{,18}","",Tweet)) %>%
            mutate(Tweet=gsub("@[[:alnum:][:punct:]]+","",Tweet)) %>%
            # mutate(Tweet=gsub("<U\\+0001","<U\\+1",Tweet)) %>%
            mutate(Tweet=gsub("!+","!",Tweet)) %>%
            mutate(Tweet=gsub("\\?+","\\?",Tweet)) %>%
            mutate(Tweet=gsub("-+","-",Tweet)) %>%
            mutate(Tweet=gsub("\\(+","\\(",Tweet)) %>%
            mutate(Tweet=gsub(")+",")",Tweet)) %>%
            mutate(Tweet=gsub("\"+","",Tweet)) %>%
            mutate(Year=year(JTime)) %>%
            mutate(Month=month(JTime)) %>%
            mutate(Day=mday(JTime)) %>%
            mutate(Hour=hour(JTime)) %>%
            mutate(Minute=minute(JTime)) %>%
            mutate(M=floor(Minute/10)*10) %>%
            arrange(desc(status_id)) %>%
            mutate(ID=paste0("Row",1:n())) %>%
            # mutate(Tweet3=ifelse(Encoding(Tweet)=="UTF-8",iconv(Tweet,from="UTF-8",to="CP932"),Tweet)) %>%
            data.frame()
        
        day=format(max(tds$JTime),"%Y%m%d_%H%M") #-24*60*60
        mid=max(tds$status_id)
        print(paste(min(tds$JTime),max(tds$JTime),nrow(tds)))
        
        write_as_csv(tds,paste0("Tweet_data/Tweet_",wd,"_",day,"_",mid,".csv"),fileEncoding = "CP932")
        
        tdc <-
            tds %>%
            filter(!status_id %in% dc) %>%
            count(Year,Month,Day,Hour,Minute,RT=is_retweet) %>%
            mutate(RT=as.logical(RT)) %>%
            complete(Year,Month,Day,Hour,Minute,RT,fill=list(n=0)) %>%
            # mutate(M=floor(Minute/10)*10) %>%
            group_by(Year,Month,Day,Hour,Minute) %>%
            mutate(total=sum(n)) %>%
            ungroup() %>%
            mutate(JTime=as.POSIXct(paste(Year,Month,Day,Hour,Minute),format="%Y %m %d %H %M"))
        
        TDC <-
            TDC %>%
            rbind(tdc) %>%
            group_by(Year,Month,Day,Hour,Minute,RT) %>%
            summarise(n=sum(n)) %>%
            ungroup() %>%
            complete(Year,Month,Day,Hour,Minute,RT,fill=list(n=0)) %>%
            group_by(Year,Month,Day,Hour,Minute) %>%
            mutate(total=sum(n)) %>%
            ungroup() %>%
            mutate(JTime=as.POSIXct(paste(Year,Month,Day,Hour,Minute),format="%Y %m %d %H %M")) %>%
            filter(floor(as.numeric(JTime)/60)<floor(as.numeric(Sys.time())/60)) %>%
            mutate(cm=cumsum(n)) %>%
            filter(cm>0) %>%
            select(-cm)
        
        
        print(head(TDC %>% arrange(desc(JTime))))
        
        write.csv(TDC,"TDC.csv",row.names = F)
        dc <- unique(c(tds$status_id,sort(dc,decreasing = T)))
        dc <- dc[1:min(length(dc),3000)]
        write(dc,"dc.txt")
        
        # print(list.files("Tweet_data"))
    
        output$Mdy <- renderDygraph({
            Comp <- 
                # data.frame(JTime=(max(TDC$JTime)-60*60):(max(TDC$JTime))) %>%
                data.frame(JTime=rep(seq(min(TDC$JTime),max(TDC$JTime),60),each=2),
                           RT=c(F,T))
            
            TDCS <-
                Comp %>%
                left_join(TDC) %>%
                complete(JTime,RT,fill=list(n=0)) %>%
                mutate(RTs=factor(RT,labels = c("Origin","Retweet"))) %>%
                select(JTime,RTs,n) %>%
                spread(RTs,n) %>%
                select(Retweet,Origin)
            
            rownames(TDCS) <- unique(Comp$JTime)
            
            dygraph(TDCS,main = paste0(max(TDC$JTime)-3*60*60,"～",max(TDC$JTime))) %>%
                dyOptions(stackedGraph = T, drawPoints = T, pointSize = 1, strokeWidth = 2,fillAlpha = 0.5,colors = c("red","blue"),
                          axisLabelFontSize = 30,axisLabelWidth = 100,titleHeight = 50,labelsKMB = T) %>%
                dyRangeSelector(height = 100,keepMouseZoom = T,dateWindow = c(max(TDC$JTime)-3*60*60,max(TDC$JTime))) %>%
                dyLegend(width = 175)
            })
        
        output$Hdy <- renderDygraph({
            TDCH <-
                TDC %>%
                mutate(M=floor(Minute/10)*10) %>%
                group_by(Year,Month,Day,Hour,M,RT) %>%
                summarise(n=sum(n)) %>%
                ungroup() %>%
                complete(Year,Month,Day,Hour,M,RT,fill=list(n=0)) %>%
                group_by(Year,Month,Day,Hour,M) %>%
                mutate(total=sum(n)) %>%
                ungroup() %>%
                mutate(JTime=as.POSIXct(paste(Year,Month,Day,Hour,M),format="%Y %m %d %H %M")) %>%
                filter(JTime<Sys.time()) %>%
                mutate(cm=cumsum(n)) %>%
                filter(cm>0) %>%
                select(-cm)
            
            Comp <- 
                data.frame(JTime=rep(seq(min(TDCH$JTime),max(TDCH$JTime),60*10),each=2),
                           RT=rep(c(F,T)))
            TDC2 <-
                Comp %>%
                left_join(TDCH) %>%
                mutate(n=ifelse(is.na(n),0,n)) %>%
                mutate(total=ifelse(is.na(total),0,total))
            
            TDCS <-
                Comp %>%
                left_join(TDC2) %>%
                complete(JTime,RT,fill=list(n=0)) %>%
                mutate(RTs=factor(RT,labels = c("Origin","Retweet"))) %>%
                select(JTime,RTs,n) %>%
                spread(RTs,n) %>%
                select(Retweet,Origin)
            
            rownames(TDCS) <- unique(Comp$JTime)
            
            dygraph(TDCS,main = paste0(max(TDC$JTime)-24*60*60,"～",max(TDC$JTime))) %>%
                dyOptions(stackedGraph = T, drawPoints = T, pointSize = 1, strokeWidth = 2,fillAlpha = 0.5,colors = c("red","blue"),
                          axisLabelFontSize = 30,axisLabelWidth = 100,titleHeight = 50,labelsKMB = T) %>%
                dyRangeSelector(height = 100,keepMouseZoom = T,dateWindow = c(max(TDC$JTime)-12*60*60,max(TDC$JTime))) %>%
                dyLegend(width = 175)
            
        })
    })
    
    YMD <- eventReactive(input$button2,{
        input$ymd
    })
    Ho <- eventReactive(input$button2,{
        input$h
    })
    
    output$ggraph <- renderPlot({
            ymd <- YMD()
            wd=input$wd
            
            path <- "Tweet_data"
            Dn <-  
                list.files(path) %>%
                data.frame() %>%
                rename(csv=".") %>%
                mutate(csv=as.character(csv)) %>%
                filter(grepl(paste0("_",wd,"_"),csv)) %>%
                arrange(desc(csv))
            
            print(Dn$csv[1])
        
            TDS <-
                # fread(paste0("Tweet_data/Tweet_",wd,"_",day,"_",mid,".csv")) %>%
                # fread("Tweet_data/Tweet_クラスター_20210322_0955_1373800378660052992.csv") %>%
                fread(paste0("Tweet_data/",Dn$csv[1])) %>%
                data.frame() %>%
                # filter(!is_retweet) %>%
                # sample_n(1000) %>%
                mutate(ID=paste0("Row",1:n())) %>%
                mutate(Tweet2=as.character(Tweet)) %>%
                mutate(Tweet2=gsub("<"," <",Tweet2)) %>%
                mutate(Tweet2=gsub(">","> ",Tweet2)) %>%
                # mutate(EF=Encoding(Tweet2)=="UTF-8") %>%
                mutate(Tweet2=ifelse(Encoding(Tweet2)=="UTF-8",iconv(Tweet2,from="UTF-8",to="CP932"),Tweet2)) %>%
                data.frame()
            
            TF1 <- docDF(TDS, col = 100, type = 1, N = 1, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic")
            
            TF1S <-
                TF1 %>%
                gather(ID,n,starts_with("Row")) %>%
                filter(n>0) %>%
                group_by(ID,N1) %>%
                mutate(n=sum(n)) %>%
                ungroup() %>%
                left_join(TDS %>% select(ID,Tweet2,status_id,JTime))
            
            TF1C <-
                TF1S %>%
                distinct(ID,N1,.keep_all = T) %>%
                group_by(word=N1) %>%
                summarise(Freq=sum(n)) %>%
                ungroup() %>%
                mutate(CF=word>=0|word %in% ED$Unicode) %>%
                filter(CF) %>%
                select(-CF)
            
            TF2 <- docDF(TDS, col = 100, type = 1, N = 2, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic",
                         pos = c("感動詞","形容詞","接頭詞","動詞","副詞","名詞","連体詞"))
            
            TF2S <-
                TF2 %>%
                gather(ID,n,starts_with("Row")) %>%
                filter(n>0) %>%
                filter(N1 %in% TF1C$word) %>%
                filter(N2 %in% TF1C$word) %>%
                group_by(ID,N1,N2) %>%
                mutate(n=sum(n)) %>%
                ungroup() %>%
                left_join(TDS %>% select(ID,Tweet2,status_id,JTime)) %>%
                mutate(word=ifelse(grepl("^接頭詞-名詞",POS1) & !grepl("<U",N2),paste0(N1,N2),NA)) %>%
                mutate(word=ifelse(grepl("名詞-名詞",POS1) & grepl("-接尾",POS2),paste0(N1,N2),word))
            
            TF2SS <-
                TF2S %>%
                filter(!is.na(word))
            
            TF2C <-
                TF2S %>%
                filter(is.na(word)) %>%
                distinct(ID,N1,N2,.keep_all = T) %>%
                group_by(word1=N1,word2=N2) %>%
                # mutate(Freq=sum(n)) %>%
                summarise(Freq=sum(n)) %>%
                ungroup() %>%
                arrange(-Freq,word1,word2)
            
            TF3 <- docDF(TDS, col = 100, type = 1, N = 3, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic",
                         pos = c("感動詞","形容詞","接頭詞","動詞","副詞","名詞","連体詞"))
            
            TF3S <-
                TF3 %>%
                gather(ID,n,starts_with("Row")) %>%
                filter(n>0) %>%
                filter(N1 %in% TF1C$word) %>%
                filter(N2 %in% TF1C$word) %>%
                filter(N3 %in% TF1C$word) %>%
                group_by(ID,N1,N2,N3) %>%
                mutate(n=sum(n)) %>%
                ungroup() %>%
                left_join(TDS %>% select(ID,Tweet2,status_id,JTime)) %>%
                left_join(TF2SS %>% select(-POS1,-POS2,-n) %>% rename(word1=word)) %>%
                left_join(TF2SS %>% select(-POS1,-POS2,-n) %>% rename(word2=word,N2=N1,N3=N2)) %>%
                mutate(word=ifelse(!is.na(word1)&!is.na(word2),paste0(N1,N2,N3),NA))
            
            TF3SS <-
                TF3S %>%
                filter(!is.na(word))
            
            TF3C1 <-
                TF3S %>%
                filter(is.na(word)) %>%
                filter(!is.na(word1)) %>%
                distinct(ID,N1,N2,N3,.keep_all = T) %>%
                group_by(wd1=word1,wd2=N3) %>%
                summarise(Freq3=sum(n)) %>%
                ungroup()
            
            TF3C2 <-
                TF3S %>%
                filter(is.na(word)) %>%
                filter(!is.na(word2)) %>%
                distinct(ID,N1,N2,N3,.keep_all = T) %>%
                group_by(wd1=N1,wd2=word2) %>%
                summarise(Freq3=sum(n)) %>%
                ungroup()
            
            TF3C <- 
                TF3C1 %>%
                rbind(TF3C2) %>%
                left_join(TF2C %>% select(-Freq) %>% rename(wd2=word2)) %>%
                left_join(TF2C %>% rename(wd1=word1)) %>%
                group_by(word1,word2) %>%
                filter(grepl(paste0(word1,"$"),wd1)|is.na(word1)) %>%
                filter(grepl(paste0("^",word2),wd2)|is.na(word2)) %>%
                mutate(Freq3=ifelse(word1==wd1&word2==wd2,Freq3+Freq,Freq3)) %>%
                group_by(wd1,wd2) %>%
                filter(Freq3==max(Freq3)) %>%
                ungroup() %>%
                select(-Freq) %>%
                mutate(word1=ifelse(is.na(word1),wd1,word1)) %>%
                mutate(word2=ifelse(is.na(word2),wd2,word2))
            
            TF2C3 <-
                TF2C %>%
                left_join(TF3C) %>%
                group_by(word1,word2) %>%
                mutate(n=n()) %>%
                mutate(Freq3=ifelse(Freq>sum(Freq3),NA,Freq3)) %>%
                mutate(wd1=ifelse(is.na(Freq3),NA,wd1)) %>%
                mutate(wd2=ifelse(is.na(Freq3),NA,wd2)) %>%
                select(-n) %>%
                ungroup() %>%
                distinct() %>%
                arrange(-Freq,word1,word2,wd1,wd2)
            
            TF4 <- docDF(TDS, col = 100, type = 1, N = 4, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic",
                         pos = c("感動詞","形容詞","接頭詞","動詞","副詞","名詞","連体詞"))
            TF4S <-
                TF4 %>%
                gather(ID,n,starts_with("Row")) %>%
                filter(n>0) %>%
                filter(N1 %in% TF1C$word) %>%
                filter(N2 %in% TF1C$word) %>%
                filter(N3 %in% TF1C$word) %>%
                filter(N4 %in% TF1C$word) %>%
                group_by(ID,N1,N2,N3,N4) %>%
                mutate(n=sum(n)) %>%
                ungroup() %>%
                left_join(TDS %>% select(ID,Tweet2,status_id,JTime)) %>%
                left_join(TF3SS %>% select(-POS1,-POS2,-n,-word1,-word2) %>% rename(word1=word)) %>%
                left_join(TF3SS %>% select(-POS1,-POS2,-n,-word1,-word2) %>% rename(word2=word,N2=N1,N3=N2,N4=N3)) %>%
                mutate(word=ifelse(!is.na(word1)&!is.na(word2),paste0(N1,N2,N3,N4),NA))
            
            TF4C1 <-
                TF4S %>%
                filter(is.na(word)) %>%
                filter(!is.na(word1)) %>%
                distinct(ID,N1,N2,N3,N4,.keep_all = T) %>%
                group_by(wd1=word1,wd2=N4) %>%
                summarise(Freq4=sum(n)) %>%
                ungroup()
            
            TF4C2 <-
                TF4S %>%
                filter(is.na(word)) %>%
                filter(!is.na(word2)) %>%
                distinct(ID,N1,N2,N3,N4,.keep_all = T) %>%
                group_by(wd1=N1,wd2=word2) %>%
                summarise(Freq4=sum(n)) %>%
                ungroup()
            
            TF4S2 <-
                TF4 %>%
                gather(ID,n,starts_with("Row")) %>%
                filter(n>0) %>%
                filter(N1 %in% TF1C$word) %>%
                filter(N2 %in% TF1C$word) %>%
                filter(N3 %in% TF1C$word) %>%
                filter(N4 %in% TF1C$word) %>%
                group_by(ID,N1,N2,N3,N4) %>%
                mutate(n=sum(n)) %>%
                ungroup() %>%
                left_join(TDS %>% select(ID,Tweet2,status_id,JTime)) %>%
                left_join(TF2SS %>% select(-POS1,-POS2,-n) %>% rename(word1=word)) %>%
                left_join(TF2SS %>% select(-POS1,-POS2,-n) %>% rename(word2=word,N3=N1,N4=N2))
            
            TF4C3 <-
                TF4S2 %>%
                filter(!is.na(word1)) %>%
                filter(!is.na(word2)) %>%
                distinct(ID,N1,N2,N3,N4,.keep_all = T) %>%
                group_by(wd1=word1,wd2=word2) %>%
                summarise(Freq4=sum(n)) %>%
                ungroup()
            
            TF4C <- 
                TF4C1 %>%
                rbind(TF4C2) %>%
                rbind(TF4C3) %>%
                left_join(TF2C3 %>% select(-wd1,-word2,-Freq,-Freq3)) %>%
                left_join(TF2C3 %>% select(-wd2,-word1)) %>%
                group_by(word1,word2) %>%
                filter(grepl(paste0(word1,"$"),wd1)|is.na(word1)) %>%
                filter(grepl(paste0("^",word2),wd2)|is.na(word2)) %>%
                ungroup() %>%
                select(-Freq,-Freq3) %>%
                mutate(word1=ifelse(is.na(word1),wd1,word1)) %>%
                mutate(word2=ifelse(is.na(word2),wd2,word2))
            
            TF2C4 <-
                TF2C3 %>%
                left_join(TF4C %>% rename(wd01=wd1,wd02=wd2)) %>%
                group_by(wd1,wd2) %>%
                mutate(n=n()) %>%
                mutate(Freq4=ifelse(Freq3!=sum(Freq4),NA,Freq4)) %>%
                mutate(wd01=ifelse(is.na(Freq4),NA,wd01)) %>%
                mutate(wd02=ifelse(is.na(Freq4),NA,wd02)) %>%
                ungroup() %>%
                mutate(wd1=ifelse(!is.na(wd01),wd01,wd1)) %>%
                mutate(wd2=ifelse(!is.na(wd02),wd02,wd2)) %>%
                select(-wd01,-wd02,-n) %>%
                distinct() %>%
                arrange(-Freq,word1,word2,wd1,wd2) %>%
                group_by(word1,word2,wd1) %>%
                mutate(Freq3=ifelse(grepl(wd2,lead(wd2)),Freq3-lead(Freq3),Freq3)) %>%
                ungroup()
            
            
            TF5 <- docDF(TDS, col = 100, type = 1, N = 5, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic",
                         pos = c("感動詞","形容詞","接頭詞","動詞","副詞","名詞","連体詞"))
            TF5S <-
                TF5 %>%
                gather(ID,n,starts_with("Row")) %>%
                filter(n>0) %>%
                filter(N1 %in% TF1C$word) %>%
                filter(N2 %in% TF1C$word) %>%
                filter(N3 %in% TF1C$word) %>%
                filter(N4 %in% TF1C$word) %>%
                filter(N5 %in% TF1C$word) %>%
                group_by(ID,N1,N2,N3,N4,N5) %>%
                mutate(n=sum(n)) %>%
                ungroup() %>%
                left_join(TDS %>% select(ID,Tweet2,status_id,JTime)) %>%
                left_join(TF3SS %>% select(-POS1,-POS2,-n,-word1,-word2) %>% rename(word1=word)) %>%
                left_join(TF2SS %>% select(-POS1,-POS2,-n) %>% rename(word2=word,N4=N1,N5=N2))
            
            TF5S2 <-
                TF5 %>%
                gather(ID,n,starts_with("Row")) %>%
                filter(n>0) %>%
                filter(N1 %in% TF1C$word) %>%
                filter(N2 %in% TF1C$word) %>%
                filter(N3 %in% TF1C$word) %>%
                filter(N4 %in% TF1C$word) %>%
                filter(N5 %in% TF1C$word) %>%
                group_by(ID,N1,N2,N3,N4,N5) %>%
                mutate(n=sum(n)) %>%
                ungroup() %>%
                left_join(TDS %>% select(ID,Tweet2,status_id,JTime)) %>%
                left_join(TF2SS %>% select(-POS1,-POS2,-n) %>% rename(word1=word)) %>%
                left_join(TF3SS %>% select(-POS1,-POS2,-n,-word1,-word2) %>% rename(word2=word,N3=N1,N4=N2,N5=N3))
            
            TF5C1 <-
                TF5S %>%
                filter(!is.na(word1)) %>%
                filter(!is.na(word2)) %>%
                distinct(ID,N1,N2,N3,N4,N5,.keep_all = T) %>%
                group_by(wd1=word1,wd2=word2) %>%
                summarise(Freq5=sum(n)) %>%
                ungroup()
            
            TF5C2 <-
                TF5S2 %>%
                filter(!is.na(word1)) %>%
                filter(!is.na(word2)) %>%
                distinct(ID,N1,N2,N3,N4,N5,.keep_all = T) %>%
                group_by(wd1=word1,wd2=word2) %>%
                summarise(Freq5=sum(n)) %>%
                ungroup()
            
            TF5C <- 
                TF5C1 %>%
                rbind(TF5C2) %>%
                left_join(TF2C4 %>% select(-wd1,-word2,-Freq,-Freq3,-Freq4)) %>%
                left_join(TF2C4 %>% select(-wd2,-word1)) %>%
                group_by(word1,word2) %>%
                filter(grepl(paste0(word1,"$"),wd1)|is.na(word1)) %>%
                filter(grepl(paste0("^",word2),wd2)|is.na(word2)) %>%
                ungroup() %>%
                select(-Freq,-Freq3,-Freq4) %>%
                mutate(word1=ifelse(is.na(word1),wd1,word1)) %>%
                mutate(word2=ifelse(is.na(word2),wd2,word2))
            
            if(nrow(TF5C)==0)
                TF2C5 <- TF2C4 %>% mutate(Freq5=NA)
                
            if(nrow(TF5C)>0)
            TF2C5 <-
                TF2C4 %>%
                # select(-wd1,-wd2) %>%
                left_join(TF5C %>% rename(wd01=wd1,wd02=wd2)) %>%
                mutate(wd1=ifelse(!is.na(wd01),wd01,wd1)) %>%
                mutate(wd2=ifelse(!is.na(wd02),wd02,wd2)) %>%
                select(-wd01,-wd02) %>%
                distinct() %>%
                ungroup()
            
            TF1CS <-
                TF1C %>%
                rbind(TF2SS %>% group_by(word) %>% summarise(Freq=sum(n))) %>%
                rbind(TF3SS %>% group_by(word) %>% summarise(Freq=sum(n))) %>%
                group_by(word) %>%
                summarise(freq=sum(Freq))
            
            TF2CS <-
                TF2C5 %>%
                mutate(word1=ifelse(!is.na(wd1),wd1,word1)) %>%
                mutate(word2=ifelse(!is.na(wd2),wd2,word2)) %>%
                mutate(Freq=ifelse(!is.na(Freq5),Freq5,ifelse(!is.na(Freq4),Freq4,ifelse(!is.na(Freq3),Freq3,Freq)))) %>%
                select(word1,word2,Freq) %>%
                distinct() %>%
                group_by(word1,word2,Freq) %>%
                summarise(Freq=sum(Freq)) %>%
                ungroup() %>%
                mutate(rate=Freq/sum(Freq)) %>%
                filter(word1!=word2) %>%
                mutate(Ranks=frank(-Freq,ties.method="min")) %>%
                arrange(Ranks) %>%
                inner_join(TF1CS,by=c("word1"="word")) %>%
                inner_join(TF1CS,by=c("word2"="word")) %>%
                mutate(rate1=Freq/freq.x) %>%
                mutate(rate2=Freq/freq.y) %>%
                mutate(word=paste(word1,word2,sep = "")) %>%
                arrange(Ranks,desc(rate1),desc(rate2))
            
            TFS <-
                TF2C5 %>%
                mutate(wd1=ifelse(!is.na(wd1),wd1,word1)) %>%
                mutate(wd2=ifelse(!is.na(wd2),wd2,word2)) %>%
                arrange(-Freq) %>%
                distinct(word1,word2,wd1,wd2) %>%
                inner_join(TF2CS,by=c("wd1"="word1","wd2"="word2")) %>%
                inner_join(TF2S %>% distinct(N1,N2,ID,status_id,Tweet=Tweet2,JTime),by=c("word1"="N1","word2"="N2"))
            
            rk=100
            cln=10
            
            TFS0 <-
                TFS %>%
                distinct(wd1,wd2,Freq,rate,Ranks,rate1,rate2,word) %>%
                arrange(Ranks,desc(rate1),desc(rate2)) %>%
                filter(Ranks<=rk) %>%
                filter(Freq>min(Freq)) %>%
                filter(wd1 %in% wd2 | wd2 %in% wd1) %>%
                mutate(num=1:n())
            
            TFSS <-
                TFS %>%
                filter(word %in% TFS0$word)
            
            k=length(unique(TFSS$Tweet))
            
            TFS1 <-
                rbind(TFSS %>%
                          count(word=wd1,Tweet)) %>%
                rbind(TFSS %>%
                          count(word=wd2,Tweet)) %>%
                arrange(desc(n)) %>%
                distinct(word,Tweet,.keep_all = T) %>%
                spread(word,n,fill=0)
            
            TFS2 <-
                TFS1 %>%
                select(-Tweet) %>%
                distinct() %>%
                t()
            
            TFS2_d <- dist(TFS2)
            TFS2_hc <- hclust(TFS2_d, method = "ward.D2") 
            # plot(TFS2_hc)
            
            TFS2_cl <- cutree(TFS2_hc, k=min(k,cln)) %>%
                data.frame() %>%
                rename(Cluster=".") %>%
                add_rownames("word")
            
            TFS3 <-
                TFS %>%
                filter(word %in% TFS0$word) %>%
                arrange(JTime,Tweet) %>%
                left_join(TFS2_cl,by=c("wd1"="word")) %>%
                left_join(TFS2_cl,by=c("wd2"="word")) %>%
                mutate(Year=year(JTime)) %>%
                mutate(Month=month(JTime)) %>%
                mutate(Day=mday(JTime)) %>%
                mutate(Hour=hour(JTime)) %>%
                mutate(Stime=as.POSIXct(paste(Year,Month,Day,Hour),format="%Y %m %d %H"))
            
            k=length(unique(TFS3$Tweet))
            
            TFS00 <-
                TFS3 %>%
                distinct(wd1,wd2,Freq,rate,Ranks,rate1,rate2,word) %>%
                arrange(Ranks)
            
            TFS3_cl <-
                data.frame() %>%
                rbind(TFS3 %>%
                          select(word=wd1,Freq=freq.x,Cluster=Cluster.x)) %>%
                rbind(TFS3 %>%
                          select(word=wd2,Freq=freq.y,Cluster=Cluster.y)) %>%
                distinct() %>%
                mutate(Cluster=factor(Cluster))
            
            g <- as_tbl_graph(TFS00, directed = T) %>%
                left_join(TFS3_cl,by=c("name"="word")) %>%
                left_join(ED,by=c("name"="Unicode")) %>%
                mutate(name=ifelse(!is.na(code),code,name))
            
            G <- data.frame(g)
            MN=max(G$Freq)
            keta=nchar(MN)-1
            br<- seq(10^keta,floor(MN/10^keta)*10^keta,10^keta)
            if(length(br)==1){
                keta=keta-1
                br<- seq(10^keta,floor(MN/10^keta)*10^keta,10^keta)
            }
            
            p<-
                g %>%
                ggraph(layout ="nicely") +
                geom_edge_link(aes(width = rate, alpha = rate1),color="royalblue", #
                               arrow = arrow(length = unit(3,'mm')), end_cap = circle(10,'mm'),force_flip = F) +
                geom_node_point(aes(col = Cluster, size = Freq)) +
                geom_node_text(aes(label = name), repel = F, size=7.5) +
                ggtitle(paste0("",min(TFS3$JTime),"~",max(TFS3$JTime),"\n",nrow(TFS00),"ルール")) + #,k,"ツイート　"
                theme_graph(title_size = 30) +
                scale_edge_alpha(range = c(0.1,1)) +
                scale_size_continuous(range = c(5,30),breaks = c(min(br),max(br))) + #,breaks = seq(0,floor(mn/keta)*keta,by = keta)
                theme( legend.text =  element_text(size = 20), # 凡例
                       legend.title = element_text(face = "bold", size = 20, hjust = 0)) +
                guides(alpha = guide_legend(title = "LEFT", title.position = "left")) +
                guides(colour = guide_legend(order=1 , override.aes = list(size=10)),
                       size   = guide_legend(order=2),
                       edge_width = F,
                       edge_alpha = F) +
                scale_colour_manual(values = ggColorHue(max(TFS3$Cluster.x,TFS3$Cluster.y)),drop=F,breaks=unique(sort(G$Cluster)))
            
            plot(p)
            })
    
    output$ggraph2 <- renderPlot({
        ymd <- YMD()
        h <- Ho()

        ymdh <- paste0(format(ymd,"%Y%m%d"),"_",formatC(h,width = 2,flag = 0))

        path <- "Tweet_data"
        # ymdh <- "20210201_16"
        Dn <-  list.files(path) %>%
            data.frame() %>%
            rename(csv=".") %>%
            mutate(csv=as.character(csv)) %>%
            filter(grepl(ymdh,csv)) %>%
            filter(grepl(paste0("_",wd,"_"),csv))

        # dn <- Dn$csv[which(grepl(ymdh,Dn$csv))]
        TD <- data.frame()
        for (dn in Dn$csv) {
            td <- fread(paste0(path,"/",dn))
            TD <-
                TD %>%
                rbind(td) %>%
                distinct(status_id,.keep_all = T)
        }

        TDS <-
            TD %>%
            filter(!is_retweet) %>%
            sample_n(min(n(),1000)) %>%
            arrange(desc(status_id)) %>%
            mutate(ID=paste0("Row",1:n())) %>%
            # mutate(Tweet2=Tweet)
            mutate(Tweet2=gsub("<"," <",Tweet)) %>%
            mutate(Tweet2=gsub(">","> ",Tweet2)) %>%
            mutate(Tweet2=ifelse(Encoding(Tweet2)=="UTF-8",iconv(Tweet2,from="UTF-8",to="CP932"),Tweet2))
    
    TF1 <- docDF(TDS, col = 100, type = 1, N = 1, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic")
    
    TF1S <-
        TF1 %>%
        gather(ID,n,starts_with("Row")) %>%
        filter(n>0) %>%
        group_by(ID,N1) %>%
        mutate(n=sum(n)) %>%
        ungroup() %>%
        left_join(TDS %>% select(ID,Tweet2,status_id,JTime))
    
    TF1C <-
        TF1S %>%
        distinct(ID,N1,.keep_all = T) %>%
        group_by(word=N1) %>%
        summarise(Freq=sum(n)) %>%
        ungroup() %>%
        mutate(CF=word>=0|word %in% ED$Unicode) %>%
        filter(CF) %>%
        select(-CF)
    
    TF2 <- docDF(TDS, col = 100, type = 1, N = 2, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic",
                 pos = c("感動詞","形容詞","接頭詞","動詞","副詞","名詞","連体詞"))
    
    TF2S <-
        TF2 %>%
        gather(ID,n,starts_with("Row")) %>%
        filter(n>0) %>%
        filter(N1 %in% TF1C$word) %>%
        filter(N2 %in% TF1C$word) %>%
        group_by(ID,N1,N2) %>%
        mutate(n=sum(n)) %>%
        ungroup() %>%
        left_join(TDS %>% select(ID,Tweet2,status_id,JTime)) %>%
        mutate(word=ifelse(grepl("^接頭詞-名詞",POS1) & !grepl("<U",N2),paste0(N1,N2),NA)) %>%
        mutate(word=ifelse(grepl("名詞-名詞",POS1) & grepl("-接尾",POS2),paste0(N1,N2),word))
    
    TF2SS <-
        TF2S %>%
        filter(!is.na(word))
    
    TF2C <-
        TF2S %>%
        filter(is.na(word)) %>%
        distinct(ID,N1,N2,.keep_all = T) %>%
        group_by(word1=N1,word2=N2) %>%
        # mutate(Freq=sum(n)) %>%
        summarise(Freq=sum(n)) %>%
        ungroup() %>%
        arrange(-Freq,word1,word2)
    
    TF3 <- docDF(TDS, col = 100, type = 1, N = 3, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic",
                 pos = c("感動詞","形容詞","接頭詞","動詞","副詞","名詞","連体詞"))
    
    TF3S <-
        TF3 %>%
        gather(ID,n,starts_with("Row")) %>%
        filter(n>0) %>%
        filter(N1 %in% TF1C$word) %>%
        filter(N2 %in% TF1C$word) %>%
        filter(N3 %in% TF1C$word) %>%
        group_by(ID,N1,N2,N3) %>%
        mutate(n=sum(n)) %>%
        ungroup() %>%
        left_join(TDS %>% select(ID,Tweet2,status_id,JTime)) %>%
        left_join(TF2SS %>% select(-POS1,-POS2,-n) %>% rename(word1=word)) %>%
        left_join(TF2SS %>% select(-POS1,-POS2,-n) %>% rename(word2=word,N2=N1,N3=N2)) %>%
        mutate(word=ifelse(!is.na(word1)&!is.na(word2),paste0(N1,N2,N3),NA))
    
    TF3SS <-
        TF3S %>%
        filter(!is.na(word))
    
    TF3C1 <-
        TF3S %>%
        filter(is.na(word)) %>%
        filter(!is.na(word1)) %>%
        distinct(ID,N1,N2,N3,.keep_all = T) %>%
        group_by(wd1=word1,wd2=N3) %>%
        summarise(Freq3=sum(n)) %>%
        ungroup()
    
    TF3C2 <-
        TF3S %>%
        filter(is.na(word)) %>%
        filter(!is.na(word2)) %>%
        distinct(ID,N1,N2,N3,.keep_all = T) %>%
        group_by(wd1=N1,wd2=word2) %>%
        summarise(Freq3=sum(n)) %>%
        ungroup()
    
    TF3C <- 
        TF3C1 %>%
        rbind(TF3C2) %>%
        left_join(TF2C %>% select(-Freq) %>% rename(wd2=word2)) %>%
        left_join(TF2C %>% rename(wd1=word1)) %>%
        group_by(word1,word2) %>%
        filter(grepl(paste0(word1,"$"),wd1)|is.na(word1)) %>%
        filter(grepl(paste0("^",word2),wd2)|is.na(word2)) %>%
        mutate(Freq3=ifelse(word1==wd1&word2==wd2,Freq3+Freq,Freq3)) %>%
        group_by(wd1,wd2) %>%
        filter(Freq3==max(Freq3)) %>%
        ungroup() %>%
        select(-Freq) %>%
        mutate(word1=ifelse(is.na(word1),wd1,word1)) %>%
        mutate(word2=ifelse(is.na(word2),wd2,word2))
    
    TF2C3 <-
        TF2C %>%
        left_join(TF3C) %>%
        group_by(word1,word2) %>%
        mutate(n=n()) %>%
        mutate(Freq3=ifelse(Freq>sum(Freq3),NA,Freq3)) %>%
        mutate(wd1=ifelse(is.na(Freq3),NA,wd1)) %>%
        mutate(wd2=ifelse(is.na(Freq3),NA,wd2)) %>%
        select(-n) %>%
        ungroup() %>%
        distinct() %>%
        arrange(-Freq,word1,word2,wd1,wd2)
    
    TF4 <- docDF(TDS, col = 100, type = 1, N = 4, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic",
                 pos = c("感動詞","形容詞","接頭詞","動詞","副詞","名詞","連体詞"))
    TF4S <-
        TF4 %>%
        gather(ID,n,starts_with("Row")) %>%
        filter(n>0) %>%
        filter(N1 %in% TF1C$word) %>%
        filter(N2 %in% TF1C$word) %>%
        filter(N3 %in% TF1C$word) %>%
        filter(N4 %in% TF1C$word) %>%
        group_by(ID,N1,N2,N3,N4) %>%
        mutate(n=sum(n)) %>%
        ungroup() %>%
        left_join(TDS %>% select(ID,Tweet2,status_id,JTime)) %>%
        left_join(TF3SS %>% select(-POS1,-POS2,-n,-word1,-word2) %>% rename(word1=word)) %>%
        left_join(TF3SS %>% select(-POS1,-POS2,-n,-word1,-word2) %>% rename(word2=word,N2=N1,N3=N2,N4=N3)) %>%
        mutate(word=ifelse(!is.na(word1)&!is.na(word2),paste0(N1,N2,N3,N4),NA))
    
    TF4C1 <-
        TF4S %>%
        filter(is.na(word)) %>%
        filter(!is.na(word1)) %>%
        distinct(ID,N1,N2,N3,N4,.keep_all = T) %>%
        group_by(wd1=word1,wd2=N4) %>%
        summarise(Freq4=sum(n)) %>%
        ungroup()
    
    TF4C2 <-
        TF4S %>%
        filter(is.na(word)) %>%
        filter(!is.na(word2)) %>%
        distinct(ID,N1,N2,N3,N4,.keep_all = T) %>%
        group_by(wd1=N1,wd2=word2) %>%
        summarise(Freq4=sum(n)) %>%
        ungroup()
    
    TF4S2 <-
        TF4 %>%
        gather(ID,n,starts_with("Row")) %>%
        filter(n>0) %>%
        filter(N1 %in% TF1C$word) %>%
        filter(N2 %in% TF1C$word) %>%
        filter(N3 %in% TF1C$word) %>%
        filter(N4 %in% TF1C$word) %>%
        group_by(ID,N1,N2,N3,N4) %>%
        mutate(n=sum(n)) %>%
        ungroup() %>%
        left_join(TDS %>% select(ID,Tweet2,status_id,JTime)) %>%
        left_join(TF2SS %>% select(-POS1,-POS2,-n) %>% rename(word1=word)) %>%
        left_join(TF2SS %>% select(-POS1,-POS2,-n) %>% rename(word2=word,N3=N1,N4=N2))
    
    TF4C3 <-
        TF4S2 %>%
        filter(!is.na(word1)) %>%
        filter(!is.na(word2)) %>%
        distinct(ID,N1,N2,N3,N4,.keep_all = T) %>%
        group_by(wd1=word1,wd2=word2) %>%
        summarise(Freq4=sum(n)) %>%
        ungroup()
    
    TF4C <- 
        TF4C1 %>%
        rbind(TF4C2) %>%
        rbind(TF4C3) %>%
        left_join(TF2C3 %>% select(-wd1,-word2,-Freq,-Freq3)) %>%
        left_join(TF2C3 %>% select(-wd2,-word1)) %>%
        group_by(word1,word2) %>%
        filter(grepl(paste0(word1,"$"),wd1)|is.na(word1)) %>%
        filter(grepl(paste0("^",word2),wd2)|is.na(word2)) %>%
        ungroup() %>%
        select(-Freq,-Freq3) %>%
        mutate(word1=ifelse(is.na(word1),wd1,word1)) %>%
        mutate(word2=ifelse(is.na(word2),wd2,word2))
    
    TF2C4 <-
        TF2C3 %>%
        left_join(TF4C %>% rename(wd01=wd1,wd02=wd2)) %>%
        group_by(wd1,wd2) %>%
        mutate(n=n()) %>%
        mutate(Freq4=ifelse(Freq3!=sum(Freq4),NA,Freq4)) %>%
        mutate(wd01=ifelse(is.na(Freq4),NA,wd01)) %>%
        mutate(wd02=ifelse(is.na(Freq4),NA,wd02)) %>%
        ungroup() %>%
        mutate(wd1=ifelse(!is.na(wd01),wd01,wd1)) %>%
        mutate(wd2=ifelse(!is.na(wd02),wd02,wd2)) %>%
        select(-wd01,-wd02,-n) %>%
        distinct() %>%
        arrange(-Freq,word1,word2,wd1,wd2) %>%
        group_by(word1,word2,wd1) %>%
        mutate(Freq3=ifelse(grepl(wd2,lead(wd2)),Freq3-lead(Freq3),Freq3)) %>%
        ungroup()
    
    
    TF5 <- docDF(TDS, col = 100, type = 1, N = 5, minFreq = 1, nDF = 1, dic = "Tweet_data/user.dic",
                 pos = c("感動詞","形容詞","接頭詞","動詞","副詞","名詞","連体詞"))
    TF5S <-
        TF5 %>%
        gather(ID,n,starts_with("Row")) %>%
        filter(n>0) %>%
        filter(N1 %in% TF1C$word) %>%
        filter(N2 %in% TF1C$word) %>%
        filter(N3 %in% TF1C$word) %>%
        filter(N4 %in% TF1C$word) %>%
        filter(N5 %in% TF1C$word) %>%
        group_by(ID,N1,N2,N3,N4,N5) %>%
        mutate(n=sum(n)) %>%
        ungroup() %>%
        left_join(TDS %>% select(ID,Tweet2,status_id,JTime)) %>%
        left_join(TF3SS %>% select(-POS1,-POS2,-n,-word1,-word2) %>% rename(word1=word)) %>%
        left_join(TF2SS %>% select(-POS1,-POS2,-n) %>% rename(word2=word,N4=N1,N5=N2))
    
    TF5S2 <-
        TF5 %>%
        gather(ID,n,starts_with("Row")) %>%
        filter(n>0) %>%
        filter(N1 %in% TF1C$word) %>%
        filter(N2 %in% TF1C$word) %>%
        filter(N3 %in% TF1C$word) %>%
        filter(N4 %in% TF1C$word) %>%
        filter(N5 %in% TF1C$word) %>%
        group_by(ID,N1,N2,N3,N4,N5) %>%
        mutate(n=sum(n)) %>%
        ungroup() %>%
        left_join(TDS %>% select(ID,Tweet2,status_id,JTime)) %>%
        left_join(TF2SS %>% select(-POS1,-POS2,-n) %>% rename(word1=word)) %>%
        left_join(TF3SS %>% select(-POS1,-POS2,-n,-word1,-word2) %>% rename(word2=word,N3=N1,N4=N2,N5=N3))
    
    TF5C1 <-
        TF5S %>%
        filter(!is.na(word1)) %>%
        filter(!is.na(word2)) %>%
        distinct(ID,N1,N2,N3,N4,N5,.keep_all = T) %>%
        group_by(wd1=word1,wd2=word2) %>%
        summarise(Freq5=sum(n)) %>%
        ungroup()
    
    TF5C2 <-
        TF5S2 %>%
        filter(!is.na(word1)) %>%
        filter(!is.na(word2)) %>%
        distinct(ID,N1,N2,N3,N4,N5,.keep_all = T) %>%
        group_by(wd1=word1,wd2=word2) %>%
        summarise(Freq5=sum(n)) %>%
        ungroup()
    
    TF5C <- 
        TF5C1 %>%
        rbind(TF5C2) %>%
        left_join(TF2C4 %>% select(-wd1,-word2,-Freq,-Freq3,-Freq4)) %>%
        left_join(TF2C4 %>% select(-wd2,-word1)) %>%
        group_by(word1,word2) %>%
        filter(grepl(paste0(word1,"$"),wd1)|is.na(word1)) %>%
        filter(grepl(paste0("^",word2),wd2)|is.na(word2)) %>%
        ungroup() %>%
        select(-Freq,-Freq3,-Freq4) %>%
        mutate(word1=ifelse(is.na(word1),wd1,word1)) %>%
        mutate(word2=ifelse(is.na(word2),wd2,word2))
    
    if(nrow(TF5C)==0)
        TF2C5 <- TF2C4 %>% mutate(Freq5=NA)
    
    if(nrow(TF5C)>0)
        TF2C5 <-
        TF2C4 %>%
        # select(-wd1,-wd2) %>%
        left_join(TF5C %>% rename(wd01=wd1,wd02=wd2)) %>%
        mutate(wd1=ifelse(!is.na(wd01),wd01,wd1)) %>%
        mutate(wd2=ifelse(!is.na(wd02),wd02,wd2)) %>%
        select(-wd01,-wd02) %>%
        distinct() %>%
        ungroup()
    
    TF1CS <-
        TF1C %>%
        rbind(TF2SS %>% group_by(word) %>% summarise(Freq=sum(n))) %>%
        rbind(TF3SS %>% group_by(word) %>% summarise(Freq=sum(n))) %>%
        group_by(word) %>%
        summarise(freq=sum(Freq))
    
    TF2CS <-
        TF2C5 %>%
        mutate(word1=ifelse(!is.na(wd1),wd1,word1)) %>%
        mutate(word2=ifelse(!is.na(wd2),wd2,word2)) %>%
        mutate(Freq=ifelse(!is.na(Freq5),Freq5,ifelse(!is.na(Freq4),Freq4,ifelse(!is.na(Freq3),Freq3,Freq)))) %>%
        select(word1,word2,Freq) %>%
        distinct() %>%
        group_by(word1,word2,Freq) %>%
        summarise(Freq=sum(Freq)) %>%
        ungroup() %>%
        mutate(rate=Freq/sum(Freq)) %>%
        filter(word1!=word2) %>%
        mutate(Ranks=frank(-Freq,ties.method="min")) %>%
        arrange(Ranks) %>%
        inner_join(TF1CS,by=c("word1"="word")) %>%
        inner_join(TF1CS,by=c("word2"="word")) %>%
        mutate(rate1=Freq/freq.x) %>%
        mutate(rate2=Freq/freq.y) %>%
        mutate(word=paste(word1,word2,sep = "")) %>%
        arrange(Ranks,desc(rate1),desc(rate2))
    
    TFS <-
        TF2C5 %>%
        mutate(wd1=ifelse(!is.na(wd1),wd1,word1)) %>%
        mutate(wd2=ifelse(!is.na(wd2),wd2,word2)) %>%
        arrange(-Freq) %>%
        distinct(word1,word2,wd1,wd2) %>%
        inner_join(TF2CS,by=c("wd1"="word1","wd2"="word2")) %>%
        inner_join(TF2S %>% distinct(N1,N2,ID,status_id,Tweet=Tweet2,JTime),by=c("word1"="N1","word2"="N2"))
    
    rk=100
    cln=10
    
    TFS0 <-
        TFS %>%
        distinct(wd1,wd2,Freq,rate,Ranks,rate1,rate2,word) %>%
        arrange(Ranks,desc(rate1),desc(rate2)) %>%
        filter(Ranks<=rk) %>%
        filter(Freq>min(Freq)) %>%
        filter(wd1 %in% wd2 | wd2 %in% wd1) %>%
        mutate(num=1:n())
    
    TFSS <-
        TFS %>%
        filter(word %in% TFS0$word)
    
    k=length(unique(TFSS$Tweet))
    
    TFS1 <-
        rbind(TFSS %>%
                  count(word=wd1,Tweet)) %>%
        rbind(TFSS %>%
                  count(word=wd2,Tweet)) %>%
        arrange(desc(n)) %>%
        distinct(word,Tweet,.keep_all = T) %>%
        spread(word,n,fill=0)
    
    TFS2 <-
        TFS1 %>%
        select(-Tweet) %>%
        distinct() %>%
        t()
    
    TFS2_d <- dist(TFS2)
    TFS2_hc <- hclust(TFS2_d, method = "ward.D2") 
    # plot(TFS2_hc)
    
    TFS2_cl <- cutree(TFS2_hc, k=min(k,cln)) %>%
        data.frame() %>%
        rename(Cluster=".") %>%
        add_rownames("word")
    
    TFS3 <-
        TFS %>%
        filter(word %in% TFS0$word) %>%
        arrange(JTime,Tweet) %>%
        left_join(TFS2_cl,by=c("wd1"="word")) %>%
        left_join(TFS2_cl,by=c("wd2"="word")) %>%
        mutate(Year=year(JTime)) %>%
        mutate(Month=month(JTime)) %>%
        mutate(Day=mday(JTime)) %>%
        mutate(Hour=hour(JTime)) %>%
        mutate(Stime=as.POSIXct(paste(Year,Month,Day,Hour),format="%Y %m %d %H"))
    
    k=length(unique(TFS3$Tweet))
    
    TFS00 <-
        TFS3 %>%
        distinct(wd1,wd2,Freq,rate,Ranks,rate1,rate2,word) %>%
        arrange(Ranks)
    
    TFS3_cl <-
        data.frame() %>%
        rbind(TFS3 %>%
                  select(word=wd1,Freq=freq.x,Cluster=Cluster.x)) %>%
        rbind(TFS3 %>%
                  select(word=wd2,Freq=freq.y,Cluster=Cluster.y)) %>%
        distinct() %>%
        mutate(Cluster=factor(Cluster))
    
    g <- as_tbl_graph(TFS00, directed = T) %>%
        left_join(TFS3_cl,by=c("name"="word")) %>%
        left_join(ED,by=c("name"="Unicode")) %>%
        mutate(name=ifelse(!is.na(code),code,name))
    
    G <- data.frame(g)
    MN=max(G$Freq)
    keta=nchar(MN)-1
    br<- seq(10^keta,floor(MN/10^keta)*10^keta,10^keta)
    if(length(br)==1){
        keta=keta-1
        br<- seq(10^keta,floor(MN/10^keta)*10^keta,10^keta)
    }
    
    p<-
        g %>%
        ggraph(layout ="nicely") +
        geom_edge_link(aes(width = rate, alpha = rate1),color="royalblue", #
                       arrow = arrow(length = unit(3,'mm')), end_cap = circle(10,'mm'),force_flip = F) +
        geom_node_point(aes(col = Cluster, size = Freq)) +
        geom_node_text(aes(label = name), repel = F, size=7.5) +
        ggtitle(paste0("",min(TFS3$JTime),"~",max(TFS3$JTime),"\n",nrow(TFS00),"ルール")) + #,k,"ツイート　"
        theme_graph(title_size = 30) +
        scale_edge_alpha(range = c(0.1,1)) +
        scale_size_continuous(range = c(5,30),breaks = c(min(br),max(br))) + #,breaks = seq(0,floor(mn/keta)*keta,by = keta)
        theme( legend.text =  element_text(size = 20), # 凡例
               legend.title = element_text(face = "bold", size = 20, hjust = 0)) +
        guides(alpha = guide_legend(title = "LEFT", title.position = "left")) +
        guides(colour = guide_legend(order=1 , override.aes = list(size=10)),
               size   = guide_legend(order=2),
               edge_width = F,
               edge_alpha = F) +
        scale_colour_manual(values = ggColorHue(max(TFS3$Cluster.x,TFS3$Cluster.y)),drop=F,breaks=unique(sort(G$Cluster)))
    
    plot(p)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
