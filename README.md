library(httr)
library(rvest)
get_wikipedia_covid19_page <- function() {

    wikipedia_base_url <-  "https://en.wikipedia.org/w/index.php"
    wikipedia_parameter <- list(title = "Template:COVID-19_testing_by_country")
    wikipedia_response <- GET(wikipedia_base_url, query = wikipedia_parameter)
return(wikipedia_response)
}
wikipedia_covid19_page_response <- get_wikipedia_covid19_page()
print(wikipedia_covid19_page_response)
root_html_node <- read_html(wikipedia_covid19_page_response)
table_node <- html_nodes(root_html_node,"table")
getdata_frame <- html_table(table_node[2], fill = TRUE)
data_frame <- as.data.frame(getdata_frame[[1]])
head(data_frame)
tail(data_frame)
summary(data_frame)
preprocess_covid_data_frame <- function(data_frame) {
    
    shape <- dim(data_frame)

    # Remove the World row
    data_frame<-data_frame[!(data_frame$`Country.or.region`=="World"),]
    # Remove the last row
    data_frame <- data_frame[1:172, ]
    
    # We dont need the Units and Ref columns, so can be removed
    data_frame["Ref."] <- NULL
    data_frame["Units.b."] <- NULL
    
    # Renaming the columns
    names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
    
    # Convert column data types
    data_frame$country <- as.factor(data_frame$country)
    data_frame$date <- as.factor(data_frame$date)
    data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
    data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
    data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
    data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
    data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
    
    return(data_frame)
}
new_data_frame <- preprocess_covid_data_frame(data_frame)
summary(new_data_frame)
head(data_frame)
tail(data_frame)
write.csv(new_data_frame,"output.csv",row.names = FALSE)
