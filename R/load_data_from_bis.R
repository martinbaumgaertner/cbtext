get_bis_features <- function(column, countries_cb) {
  
  # Check if column is a Tibble with 3 columns
  if (!is_tibble(column) || ncol(column) != 3) {
    stop("Invalid column input. It should be a Tibble with 3 columns.")
  }
  
  # Check if column elements are of character type
  if (!is.character(column[[1]]) || !is.character(column[[2]]) || !is.character(column[[3]])) {
    stop("Invalid column input. Elements should be of character type.")
  }
  
  # Define site URL
  url_base <- column[1]
  html_site <- xml2::read_html(paste0(url_base, ".htm"))
  
  # Get PDF link if it exists
  pdf_link <- if(!is.na(column[2])) paste0(url_base, ".pdf") else NA
  
  # Extract elements
  extract_element <- function(html, css) {
    html %>% rvest::html_element(css) %>% rvest::html_text() %>% stringr::str_remove_all("\n|\t")
  }
  
  title <- extract_element(html_site, "h1")
  speaker <- extract_element(html_site, ".authorlnk")
  description <- extract_element(html_site, "#extratitle-div")
  release_date <- extract_element(html_site, ".date") %>% as.Date(c("%d %B %Y"))
  
  # Extract dates
  dates <- get_date_from_text(tibble(texts = description), "bis", type = "speeches")
  start_date <- dates$start_date
  end_date <- dates$end_date
  
  # Extract speaker position, event, cb, country
  speaker_position <- extract_position(description)
  event <- extract_event(description)
  cb_country <- search_cb(speaker_position, description, countries_cb)
  cb <- cb_country$cb
  country <- cb_country$country
  
  # Extract text and link
  if (!is.na(pdf_link)) {
    link <- pdf_link
    text <- suppressMessages(readtext::readtext(pdf_link)) %>% 
      dplyr::pull(text) %>% 
      readr::read_lines()
    text <- text[!stringr::str_detect(text, "BIS Review|BIS central bankers’ speeches")]
  } else {
    link <- paste0(url_base, ".htm")
    text <- html_site %>% 
      rvest::html_nodes(xpath = "//div[@id='cmsContent']") %>%
      rvest::html_text() %>%
      readr::read_lines()
  }
  text <- list(text)
  
  # Get access time and language
  access_time <- Sys.time()
  language <- names(which.max(table(cld3::detect_language(text[[1]]))))
  type <- "speech"
  
  # Check for NA in start_date and speaker
  if (is.na(start_date)) {
    dates <- get_date_from_text(paste(text[[1]], collapse = " "), "bis", "speech")
    start_date <- dates$start_date
    end_date <- dates$end_date
  }
  if(is.na(speaker)){
    speaker<-html_site %>% 
      rvest::html_element("h1")%>% 
      rvest::html_text() %>% 
      stringr::str_split(":")
    speaker<-sapply( speaker, utils::head, 1 )
  }
  
  return(dplyr::tibble(title,speaker,start_date,end_date,release_date,speaker_position,event,cb,country,type,text,link,access_time,language))
}

search_cb <- function(strings, aux_string, countrys) {
  
  # Initialize empty data frame
  dat <- data.frame(country = rep(NA, length(strings)), 
                    cb = rep(NA, length(strings)), 
                    stringsAsFactors = FALSE)
  
  # Loop over strings
  for (i in seq_along(strings)) {
    
    # Attempt to find match in 'cb' column
    dat_temp <- countrys %>% 
      filter(str_detect(strings[i], countrys %>% pull(cb)))
    if (nrow(dat_temp) == 0) {
      # if central bank cannot be found, use country names
      dat_temp <- countrys %>% 
        filter(str_detect(strings[i], countrys %>% pull(country)))
    } else {
      # else use cb names
      if (nrow(dat_temp) > 1) {
        dat_temp <- dat_temp[1, , drop = FALSE]
      }
    }
    
    # If speaker position gives no central bank or country, use total description
    if (nrow(dat_temp) == 0) {
      dat_temp <- countrys %>% 
        filter(str_detect(aux_string[i], pull(cb)))
      if (nrow(dat_temp) > 0) {
        dat_temp <- dat_temp[1, , drop = FALSE]
      }
    }
    if (nrow(dat_temp) == 0) {
      print(aux_string[i])
      dat_temp <- data.frame(country = NA, cb = NA, stringsAsFactors = FALSE)
    }
    
    dat[i, ] <- dat_temp
  }
  
  return(dat)
}


extract_date<-function(x){
  Sys.setlocale("LC_ALL","en_US.UTF-8")
  x %>% 
    stringr::str_replace_all("March,","March") %>% 
    stringr::str_replace_all("Sept\\.","September") %>% 
    stringr::str_replace_all("Hel...","Helsinki, 9 October 2003") %>% 
    stringr::str_replace_all("at the Swiss-American Chamber ...","at the Swiss-American Chamber of Commerce, Zurich, 8 October 2003") %>% 
    stringr::str_replace_all("on the occasion of the signing of...","Moscow, 13 October 2003") %>% 
    stringr::str_replace_all("Committee on Finance, Stockholm, 16 October...","Committee on Finance, Stockholm, 16 October 2003") %>% 
    stringr::str_replace_all("at the 2003 IMF","at the 2003 IMF, 24 September 2003") %>% 
    stringr::str_replace_all("/"," ") %>% 
    stringr::str_remove_all("Claude Trichet President of the European Central Bank Frankfurt am Main") %>% 
    stringr::str_remove_all("Communication") %>%
    stringr::str_remove_all("on the occasion of a special ECOFIN di...") %>%
    stringr::str_remove_all("workshop on ¿Managing Non Performing Asset¿ hosted") %>% 
    stringr::str_remove_all("The references for the speech can be found on the Board of Governors of the Federal Reserve System¿s website.") %>%
    stringr::str_remove_all("Basel Committee on Banking") %>% 
    stringr::str_remove_all("on the occasion") %>% 
    stringr::str_remove_all("Seminar on his") %>% 
    stringr::str_replace_all("on the European Commission's proposal of 29 January 2014 for a regulation on the separation of certain trading activities from credit institutions.","29 January 2014") %>% 
    stringr::str_split(.,",") %>% 
    sapply( ., utils::tail, 1 )%>% 
    dplyr::tibble(date=.) %>% 
    dplyr::mutate(date=ifelse(stringr::str_detect(date," on "),stringr::str_split(date," on (?=\\d{1,2})",simplify=T)[,2],date)) %>% 
    dplyr::mutate(date=ifelse(stringr::str_detect(date,"-"),get_last_date(date)%>% 
                                stringr::str_remove(.,"\\.$") %>% 
                                stringr::str_trim(),date%>% 
                                stringr::str_remove(.,"\\.$") %>% 
                                stringr::str_trim())) %>% 
    dplyr::mutate(date=ifelse(stringr::str_detect(date,"^\\d{1,2}",negate=T),paste0("1 ",date),date))%>% 
    dplyr::mutate(date=as.POSIXlt(as.Date(ifelse(grepl("/", date ),
                                                 as.Date(date , format = c("%d/%m/%y")),
                                                 as.Date(date , format = c("%d %B %Y"))), origin = "1970-01-01"))) %>% 
    dplyr::pull(date)
}

get_last_date<-function(x){
  x%>%
    stringr::str_split(.,"-",simplify = T) %>% 
    dplyr::as_tibble(.,.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>% 
    dplyr::select(2) %>% dplyr::pull() %>% stringr::str_remove(.,"^_") 
}

extract_position <- function(x) {
  out <- stringr::str_split(x, ",", simplify = TRUE)
  
  if (ncol(out) > 1) {
    out <- dplyr::tibble(position = trimws(out[, 2]))
    out <- dplyr::pull(out, position)
    return(out)
  } else {
    return(NA)
  }
}

extract_event<-function(x){
  x1<-dplyr::tibble(spacyr::entity_extract(spacyr::spacy_parse(x)))
  x1<-dplyr::filter(x1,entity_type=="GPE")
  x1<-paste(dplyr::pull(x1,entity),collapse = ", ")
  
  return(x1)
}


load_data_from_bis<-function(links,countries_cb){
  #loop over all links and collect results in tibble
  bis_data<-list()
  pb<-utils::txtProgressBar(0,nrow(links),style=3)
  for(i in 1:nrow(links)){
    bis_data[[i]]<-get_bis_features(links[i,],countries_cb)
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  bis_data<-bis_data %>% 
    dplyr::bind_rows()%>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(cb=head(unlist(stringr::str_split(cb,
                                    "\\|")),1))%>% 
    ungroup()
  
  return(bis_data)
}
