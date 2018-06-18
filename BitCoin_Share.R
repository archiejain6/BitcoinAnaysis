setwd("/Users/archiejain/Aly6015_Intermediate Analytics/Week-6")
getwd()
#Reading into R
bitcoin<-read.csv("bitcoin_price.csv",header = T)
bitcoin_cash<-read.csv("bitcoin_cash_price.csv",header = T)
ripple<-read.csv("ripple_price.csv",header = T)
dash<-read.csv("dash_price.csv",header = T)
ethereum<-read.csv("ethereum_price.csv",header = T)
neo<-read.csv("neo_price.csv",header = T)
nem<-read.csv("nem_price.csv",header = T)
monero<-read.csv("monero_price.csv",header = T)
lite_coin<-read.csv("litecoin_price.csv",header = T)

bitcoinV<-data.frame(date=bitcoin$Date,bitcoin$Volume)
bitcoin_cashV<-data.frame(date=bitcoin_cash$Date,bitcoin_cash$Volume,stringsAsFactors=FALSE)
rippleV<-data.frame(date=ripple$Date,ripple$Volume)
dashV<-data.frame(date=dash$Date,dash$Volume)
ethereumV<-data.frame(date=ethereum$Date,ethereum$Volume)
neoV<-data.frame(date=neo$Date,neo$Volume)
nemV<-data.frame(date=nem$Date,nem$Volume)
moneroV<-data.frame(date=monero$Date,monero$Volume)
litecoinV<-data.frame(date=lite_coin$Date,lite_coin$Volume)

#joining tables
library(dplyr)
joinsV <- left_join(bitcoinV,bitcoin_cashV, by="date")
joinsV <- left_join(joinsV,rippleV, by="date")
joinsV <- left_join(joinsV,dashV, by="date")
joinsV <- left_join(joinsV,ethereumV, by="date")
joinsV <- left_join(joinsV,neoV, by="date")
joinsV <- left_join(joinsV,moneroV, by="date")
joinsV <- left_join(joinsV,litecoinV, by="date")
joinsV <- left_join(joinsV,nemV, by="date")

colnames(joinsV)<-c("Date","Bitcoin","Bitcoin_cash","Ripple","Dash","Ethereum","NEO","Monero","Litecoin","NEM")
joinsV$Date<-as.Date(joinsV$Date,format = " %dd %B , %Y")

write.csv(joinsV, file = "Volume.csv",row.names=TRUE, na="0")

joinedV<-read.csv("Volume.csv")
class(joinedV$Ethereum)

joinedV$Bitcoin<-as.numeric(as.character(joinedV$Bitcoin))
joinedV$Bitcoin_cash<-as.numeric(as.character(joinedV$Bitcoin_cash))
joinedV$Ripple<-as.numeric(as.character(joinedV$Ripple))
joinedV$Dash<-as.numeric(as.character(joinedV$Dash))
joinedV$Ethereum<-as.numeric(as.character(joinedV$Ethereum))
joinedV$NEO<-as.numeric(as.character(joinedV$NEO))
joinedV$NEM<-as.numeric(as.character(joinedV$NEM))
joinedV$Litecoin<-as.numeric(as.character(joinedV$Litecoin))
joinedV$Monero<-as.numeric(as.character(joinedV$Monero))

attach(joinedV)
 joinedV["TotalVolume"]=Bitcoin+Bitcoin_cash+Ripple+Dash+Ethereum+NEO+Monero+Litecoin+NEM
 joinedV$TotalVolume<-as.numeric(paste((joinedV$TotalVolume)))
 joinedV["BitCoinPercentage"]=(Bitcoin*100)/TotalVolume
joinedV$BitCoinPercentage<-as.numeric(paste((joinedV$BitCoinPercentage)))
joinedV["OtherCryptocurrencies"]=(100-BitCoin.Percentage)
 joinedV$OtherCryptocurrencies<-as.numeric(paste((joinedV$OtherCryptocurrencies)))

colnames(joinedV)=c("Year","Date","Bitcoin","Bitcoin_cash","Ripple","Dash","Ethereum","NEO","Monero","Litecoin","NEM","TotalVolume","BitCoinPercentage","OtherCryptocurrencies")

share<-aggregate(joinedV[, 13:14], list(joinedV$Year), mean)
colnames(share)<-c("Year","BitCoinPercentage","OtherCryptocurrencies")

#plotting the change of volume of shares
plot(share$Year,share$BitCoinPercentage,type = "l",col="red",ylim = range(0,100));par(new=T)
plot(share$Year,share$OtherCryptocurrencies,type = "l",col="blue",ylim = range(0,100))

otc<-cbind(share$BitCoinPercentage,share$OtherCryptocurrencies)

#bar plot comparing volume of shares for bitcoin and other crypto currencies
barplot(t(otc), beside=T, ylab="Percentage", 
        cex.names=0.8, las=2, ylim=c(0,100), col=c("darkblue","red"))
axis(1, at = seq(3,13,by=2.5), labels = c("2013","2014","2015","2016","2017"))
legend("topright",c("BitCoin","Other Cryptocurrencies"),col=c("darkblue","red"), lty=1, cex=.65)


#Code for animation plot
# install.packages("cowplot")  # a gganimate dependency
# devtools::install_github("dgrtwo/gganimate")
library(ggplot2)
library(gganimate)

# install.packages("gapminder")
library(gapminder)
theme_set(theme_bw())  # pre-set the bw theme.

#install.package("gminder")
library(gminder)
p2 <- ggplot(joinedV, aes(joinedV$Bitcoin,joinedV$BitCoinPercentage, frame = Year)) +
  geom_point()+scale_x_log10()
  
gganimate(p2)

p3<-ggplot(joinedV, aes(joinedV$Bitcoin,joinedV$OtherCryptocurrencies, frame = Year,col="red")) +
  geom_point()+scale_x_log10()
gganimate(p3)

Sys.setenv(PATH = paste("C:/Program Files/ImageMagick-7.0.7-Q16",Sys.getenv("PATH"), sep = ";"))
#install.packages("animation")
library(animation)
ani.options(convert = "C:/Program Files/ImageMagick-7.0.7-Q16/convert.exe")

saveGIF({
  ggplot(joinedV, aes(joinedV$Bitcoin,joinedV$OtherCryptocurrencies, frame = Year,col="red")) +
    geom_point()+scale_x_log10()
})

library(ggplot2)
library(gganimate)

# bp<-ggplot(share, aes(x=, y=cnt_thisyeartype, fill=current_type, frame = Year, cumulative = FALSE))+
#   geom_bar(width = 1, stat = "identity")+coord_polar("y")+
#   #geom_text(aes(label = cnt_thisyeartype), position = position_stack(vjust = 0.5)) +
#   geom_col() +
#   labs(y = "Quantity", title = "Total count of acc", subtitle = "Year by year stat", fill="Type of acc") +
#   scale_fill_brewer(palette = "Set2")+
#   theme_minimal()
# 
# ani.options(interval=2)
# gganimate(bp,"pie.gif")


png("myPlot.png")

dev.off()
install.packages("png")
library(png)
P1 <- readPNG("myPlot.png")
library(caTools)
write.gif(P1,"myPlot.gif")
showGIF <- function(fn) system(paste("display",fn))
showGIF("myPlot.gif")
unlink("myPlot.gif")

ggsave(filename = paste0("./fig_output/ndwi/hgm_ndwi_",Yr,".png"),
       width = 8,height=8,dpi = 150)