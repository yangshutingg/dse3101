# install.packages("RSelenium")
library(RSelenium)

url = "https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/real-time-data-set-full-time-series-history"

file_path <- getwd()
fprof <- makeFirefoxProfile(list(browser.download.dir = file_path,
                                 browser.download.folderList = 2L,
                                 browser.download.manager.showWhenStarting = FALSE,
                                 browser.helperApps.neverAsk.openFile = "text/csv",
                                 browser.helperApps.neverAsk.saveToDisk = "text/csv")
)

rD <- rsDriver(browser="firefox", port=4555L, verbose=F, chromever = NULL, extraCapabilities = fprof)
remDr <- rD[["client"]]
remDr$navigate(url)

webElem <- remDr$findElement(using = "xpath", "/html/body/div[1]/main/section[1]/div[2]/div[1]/ul[1]/li[1]/a")
webElem$clickElement()

getgdp = remDr$findElement(using = "xpath", "/html/body/div[1]/main/section[1]/div[2]/div[1]/ul[1]/li/a")
getgdp$clickElement()

remDr$goBack()

rcon = remDr$findElement(using = "xpath", "/html/body/div[1]/main/section[1]/div[2]/div[1]/ul[1]/li[2]/ul/li[1]/a")
rcon$clickElement()

getrcon = remDr$findElement(using = "xpath", "/html/body/div[1]/main/section[1]/div[2]/div[1]/ul[1]/li/a")
getrcon$clickElement()

remDr$closeWindow()

# stop the selenium server
rD[["server"]]$stop()
