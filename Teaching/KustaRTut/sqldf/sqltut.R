require(sqldf)
require(dplyr)

crashes <- read.csv("./crashes.csv")
roads <- read.csv("./roads.csv")


# Left join ---------------------------------------------------------------


join_string <- "select  crashes.*,
                        roads.District,
                        roads.Length
                from    crashes
                        left join roads
                        on crashes.Road = roads.Road"

(crashes_join_roads <- sqldf(join_string,stringsAsFactors = FALSE))


# Inner join --------------------------------------------------------------


join_string2 <- "select crashes.*,
                        roads.District,
                        roads.Length
                from    crashes
                        inner join roads
                        on crashes.Road = roads.Road"

(crashes_join_roads2 <- sqldf(join_string2, stringsAsFactors = FALSE))



# Full conditions ---------------------------------------------------------


join_string2 <- "select crashes.*,
                        roads.District,
                        roads.Length
                from    crashes
                        inner join roads
                        on crashes.Road = roads.Road
                where   crashes.Road = 'US-40'"
crashes_join_roads4 <- sqldf(join_string2,stringsAsFactors = FALSE)


# Merge -------------------------------------------------------------------


authors <- data.frame(
  ## I(*) : use character columns of names to get sensible sort order
  surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  deceased = c("yes", rep("no", 4)))
authorN <- within(authors, { name <- surname; rm(surname) })
books <- data.frame(
  name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
  title = c("Exploratory Data Analysis",
            "Modern Applied Statistics ...",
            "LISP-STAT",
            "Spatial Statistics", "Stochastic Simulation",
            "Interactive Data Analysis",
            "An Introduction to R"),
  other.author = c(NA, "Ripley", NA, NA, NA, NA,
                   "Venables & Smith"))

(m0 <- merge(authorN, books, by = intersect(names(authorN), names(books))))
(m1 <- merge(authors, books, by.x = "surname", by.y = "name"))
(m2 <- merge(books, authors, by.x = "name", by.y = "surname"))