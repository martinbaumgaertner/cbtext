get_bis_countries<-function(){
  #get central banks in bis
  countries_cb<-xml2::read_html("https://www.bis.org/cbanks.htm") %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill = T) %>% 
    .[[1]] %>% 
    dplyr::as_tibble(.,.name_repair = c("unique")) %>% 
    dplyr::rename("country"=1,"cb"=2) %>% 
    dplyr::filter(cb!="") %>% 
    dplyr::mutate(country=if_else(country=="", "United States" ,country))
  
  countries_cb[8,"cb"]<-paste("Reserve Bank of Australia","Australian Reserve Bank",sep="|")
  countries_cb[9,"cb"]<-paste("National Bank of the Republic of Austria","Oesterreichische Nationalbank",sep="|")
  countries_cb[37,"cb"]<-paste("People's Bank of China")
  countries_cb[38,"cb"]<-paste("Central Bank of Colombia","Bank of the Republic",sep="|")
  countries_cb[42,"cb"]<-paste("Croatian National Bank","Croation National Bank",sep="|")
  countries_cb[44,"cb"]<-paste("Central Bank of Curaçao and Sint Maarten","Central Bank of Curacao and Sint Maarten",sep="|")
  countries_cb[47,"cb"]<-paste("Danmarks Nationalbank","National Bank of Denmark","Danmarks National Bank",sep="|")
  countries_cb[56,"cb"]<-paste("European Central Bank","ECB","Mario Draghi","European Central, Bank","Europ. Central Bank",sep="|")
  countries_cb[59,"cb"]<-paste("Bank of France","Banque de France","BANK OF FRANCE",sep="|")
  countries_cb[72,"cb"]<-paste("Hong Kong Monetary Authority","Hong Kong Monetary","Hong Kong Mon. Authority",sep="|")
  countries_cb[75,"cb"]<-paste("Reserve Bank of India","Reserve Bank of india",sep="|")
  countries_cb[76,"cb"]<-paste("Bank Indonesia","Bank of Indonesia",sep="|")
  countries_cb[84,"cb"]<-paste("BANK OF JAPAN","Bank of Japan",sep="|")
  countries_cb[100,"cb"]<-paste("Monetary Authority of Macao","Mon. Authority of Macao",sep="|")
  countries_cb[117,"cb"]<-paste("Netherlands Bank","Nederlandsche Bank",sep="|")
  countries_cb[122,"cb"]<-paste("National Bank of the Republic of North Macedonia","National Bank of North Macedonia",sep="|")
  countries_cb[122,"country"]<-paste("North Macedonia, Republic of","Macedonia",sep="|")
  countries_cb[123,"cb"]<-paste("Central Bank of Norway","Norges Bank",sep="|")
  countries_cb[132,"cb"]<-paste("Central Bank of the Philippines (Bangko Sentral ng Pilipinas)","Central Bank of the Philippines",
                                "Bangko Sentral ng Pilipinas",sep="|")
  countries_cb[137,"cb"]<-paste("Central Bank of the Russian Federation","Bank of Russia",sep="|")
  countries_cb[150,"cb"]<-paste("Reserve Bank of South Africa","South African Reserve Bank",sep="|")
  countries_cb[151,"cb"]<-paste("Bank of Spain","Banco de España",sep="|")
  countries_cb[155,"cb"]<-paste("Sveriges Riksbank","Sveriges Risksbank","Sveriges Riskbank","Bank of Sweden","Swedish Central Bank",sep="|")
  countries_cb[156,"cb"]<-paste("Banque nationale suisse","Swiss National Bank",sep="|")
  countries_cb[163,"cb"]<-paste("Central Bank of Trinidad and Tobago","Central Bank of Trinidad &amp; Tobago",sep="|")
  countries_cb[178,"cb"]<-paste("Federal Reserve Bank of New York","Federal\rReserve Bank of New York","Fed. Res. Bank of New York",sep="|")
  countries_cb[183,"cb"]<-paste("Board of Governors of the Federal Reserve System","US Fed. Reserve System","US Federal Reserve",
                                "Federal Reserve System","Board of Governors of the Federal Reserve","US Fed. Res. System","Federal Reserve Board",sep="|")
  countries_cb[192,"cb"]<-paste("BIS","Committee on Payments and Market Infrastructures","Markets Committee","Bank for International Settlements",sep="|")
  countries_cb[192,"country"]<-paste("world",sep="|")
  countries_cb[193,"country"]<-paste("world",sep="|")
  countries_cb[193,"cb"]<-paste("UN",sep="|")
  countries_cb[194,"cb"]<-paste("OECD",sep="|")
  countries_cb[194,"country"]<-paste("world",sep="|")
  countries_cb[195,"cb"]<-paste("Financial Services Authority",sep="|")
  countries_cb[195,"country"]<-paste("world",sep="|")
  countries_cb[196,"cb"]<-paste("International Monetary Fund","IMF",sep="|")
  countries_cb[196,"country"]<-paste("world",sep="|")
  countries_cb[197,"cb"]<-paste("Superintendent of Financial Institutions, Canada",sep="|")
  countries_cb[197,"country"]<-paste("Canada",sep="|")
  countries_cb[198,"country"]<-paste("world",sep="|")
  countries_cb[198,"cb"]<-paste("Inter-American Development Bank",sep="|")
  countries_cb[199,"country"]<-paste("Switzerland",sep="|")
  countries_cb[199,"cb"]<-paste("Swiss Federal Banking Commission",sep="|")
  countries_cb[200,"country"]<-paste("world",sep="|")
  countries_cb[200,"cb"]<-paste("European Monetary Institute",sep="|")
  
  return(countries_cb)
}
