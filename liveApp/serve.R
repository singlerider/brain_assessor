server <- function(){


       writeLines("Listening...")

       while(TRUE){

		con  <- socketConnection(host="", port = 6011, blocking=TRUE,
                               server=TRUE, open="r+")

                data <- readLines(con, -1)
                writeLines (data)
                response <- data
		close (con)

        }



}
server()
