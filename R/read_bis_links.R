#' Funktion zum Lesen und Umwandeln einer XML-Datei
#'
#' Diese Funktion liest eine XML-Datei von einer gegebenen URL und konvertiert sie in ein tibble.
#'
#' @param url Eine Zeichenkette, die die URL der zu lesenden XML-Datei darstellt.
#' @return Ein tibble, das aus der XML-Datei erstellt wurde. Bei Fehlern beim Lesen der Datei gibt die Funktion NULL zurück.
#' @examples
#' \dontrun{
#' read_xml_link("https://www.bis.org/sitemap.xml?documents=2005,2006,2007,2008,2009")
#' }
read_xml_link <- function(url) {
  tryCatch({
    xml2::read_xml(url) %>% 
      xml2::as_list() %>% 
      tibble::as_tibble() %>%
      tidyr::unnest_wider(urlset) %>%
      tidyr::unnest(cols = names(.)) %>%
      tidyr::unnest(cols = names(.)) %>%
      suppressMessages(readr::type_convert())
  }, error = function(e) {
    message("Fehler beim Lesen von ", url, ": ", e)
    return(NULL)
  })
}

#' Hauptfunktion zum Lesen von XML-Links
#'
#' Diese Funktion liest XML-Dateien von einer Liste von URLs und konvertiert sie in ein tibble.
#' Sie filtert dann nach Links, die "/review/" enthalten und klassifiziert sie nach ihrem Typ (PDF oder HTML).
#'
#' @param urls Eine Vektor von Zeichenketten, die die URLs der zu lesenden XML-Dateien darstellen.
#' @return Ein tibble, das alle Links aus den XML-Dateien enthält, die "/review/" enthalten, klassifiziert nach ihrem Typ.
#' @examples
#' \dontrun{
#' read_bis_links(c("https://www.bis.org/sitemap.xml?documents=2005,2006,2007,2008,2009"))
#' }
read_bis_links <- function(urls=c("https://www.bis.org/sitemap.xml?documents=2005,2006,2007,2008,2009",
                                  "https://www.bis.org/sitemap.xml?documents=2010,2011,2012,2013,2014",
                                  "https://www.bis.org/sitemap.xml?documents=2015,2016,2017,2018,2019",
                                  "https://www.bis.org/sitemap.xml?documents=2020,2021,2022,2023")) {
  links <- urls %>% 
    purrr::map(read_xml_link) %>% 
    dplyr::bind_rows() %>% 
    dplyr::rename(link=loc) %>%
    dplyr::select(link) %>% 
    dplyr::filter(stringr::str_detect(link,"/review/")) %>% 
    dplyr::mutate(type = ifelse(stringr::str_detect(link,".pdf"), "pdf", "html"),
                  link = stringr::str_remove(link, ".pdf|.htm")) %>% 
    tidyr::pivot_wider(names_from = type, values_from = type)
  
  return(links)
}
