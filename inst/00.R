library(homodatum)
# library(dspins)

# user_id <- "wikitables3"
# user_board_create(user_id)


data <- data.frame(book = c("Black", "Red"), value = 1:2)
ff <- fringe(data, name = "Nice booksX2", description = "I just made this files up",
            license = "CC-BY", date_created = Sys.time())
# fringe_write(f, path = "tmp")

pins::pin(ff, board = "local")

fringes <- pin_find(board = "local")
nm <- fringes$name[nrow(fringes)]

f1 <- pin_get(nm, files = TRUE)



pin_list("wikitables3")



pin_get("Nice booksX2")

pins::pin(ff, board = "dskt.ch.wikitables3")

board_list()

user_board_list_remote()

board_
board_connect("dskt.ch.wikitables3")

board_register_s3(name = "dskt.ch.wikitables3", bucket = "dskt.ch.wikitables3")

pins::pin(ff, board = "dskt.ch.wikitables3")


pin_find(board = "local")
pin_get("f", board = "local")


pins::pin(f, "dskt.ch.wikitables3")


local_board <- "/Users/jpmarindiaz/Library/Caches/pins/local"
list.files(local_board, recursive = TRUE)


list.files("", recursive = TRUE)


x <- pin_find("boston housing")


d <- pin_get("GSE/boston")

pin("http://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_master.csv")


genero_es <- "https://github.com/datasketch/genero/blob/master/data/names_gender_es.rda?raw=true"
pin1 <- pin(genero_es)


#pins::board_register(board = "packages")
local_board <- "/Users/jpmarindiaz/Library/Caches/pins/local"
list.files(local_board, recursive = TRUE)

pin(mtcars, board = "mypins")

board_list()


pin(mtcars, name = "mtcars2", board = "local")
tibble::tibble(paths = dir(board_local_storage(), full.names = TRUE))

pin_get()
pin_find()

board_get()
board_register_local("mypins", cache = "mypins")




