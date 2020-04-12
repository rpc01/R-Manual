####
# Download financial data and calculate returns
####
library(quantmod)
library(dplyr)
from<-"2018-01-01"
till<-"2020-04-09"
getSymbols("GOOG",src="yahoo",from=from, to=till)%>%get() # from yahoo 
getSymbols("F",src="yahoo",from=from, to=till)%>%get() # from yahoo 
getSymbols("DEXJPUS",src="FRED") #FX from FRED

setSymbolLookup(GOOG ='yahoo') 
setSymbolLookup(F ='yahoo') 
setSymbolLookup(DEXJPUS='FRED') 
setSymbolLookup(XPTUSD=list(name="XPT/USD",src="oanda")) 
saveSymbolLookup(file="mysymbols.rda") 
# new sessions call loadSymbolLookup(file="mysymbols.rda") 

getSymbols(c("F","GOOG","DEXJPUS","XPTUSD"))

F %>%chartSeries(TA='addBBands();
                     addBBands(draw="p");
                    addVo();
                    addMACD()', 
                subset=c("2020"),
                theme="white")

tail(allReturns(F),100)
dret<-(dailyReturn(F["2020"]))  

F_dret <- F %>% 
#  subset="2020" %>%
  Ad() %>% 
  dailyReturn(type="log")


F_dret %>%
  ggplot(aes(x=F_dret))+
  geom_histogram(bins=100)+
  geom_density()+
  geom_rug(alpha=0.5)

probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_log_returns <- F_dret %>% 
  quantile(probs = probs, na.rm = TRUE)
dist_log_returns





