client <- function(){
    con <- socketConnection(host="localhost", port = 6011, blocking=TRUE,
                            server=FALSE, open="r+")
    num <- runif (100, 0, 1) 
    
    for (curr_number in 1:length (num)) {
        text <- as.character (num[curr_number])
        read_resp <- 0
        
        while (read_resp != "1") {
                writeLines (text)
                write_resp <- writeLines(text, con)
                read_resp <- readLines (con, 1)
                writeLines (paste0 ("response is: ", read_resp))
        }

        Sys.sleep (0.01)
        }
  
    close(con)
}

client()
