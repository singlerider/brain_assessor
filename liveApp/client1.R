client <- function(){
    con <- socketConnection(host="localhost", port = 6011, blocking=TRUE,
                            server=FALSE, open="r+")
    num <- runif (100, 0, 1) 
    
    for (curr_number in 1:length (num)) {
        text <- as.character (num[curr_number])
        writeLines (text)
        write_resp <- writeLines(text, con)
        Sys.sleep (0.3)
        }
  
    close(con)
}

client()
