# For the current git blame author list (amount of lines per author)
# git ls-files | xargs -n1 git blame --line-porcelain | sed -n 's/^author //p'

# For commits by authors per date.
# git log --pretty=format:"%an%x09%ad%x09%T" --date=short --shortstat --no-merges

#ppd
#    (
#        git ls-files |
#        xargs -n1 git blame --line-porcelain |
#        sed -n 's/^author //p' >  ~/projects/author-graphing/input/blames/$DIR.txt
#    );
#    (
#        git log --pretty=format:"%an%x09%ad" --date=short --shortstat --no-merges >
#        ~/projects/author-graphing/input/commits/$DIR.txt
#    );
#dpp

# potential add to the git log: --use-mailmap, might fix some issues
require(dplyr)
require(stringr)
require(data.table)
require(ggplot2)

FullNameToLdap <- function(author.name, edgecases) {
    # If our name is in the provided edgecases, use the new name instead.
    filtered.cases = edgecases %>% filter(OriginalName == author.name)
    if(nrow(filtered.cases) > 0) {
        author.name = filtered.cases$NewName[1]
    }

    author.name <- tolower(author.name)
    if(str_detect(author.name, " ") == FALSE) {
        return(author.name)
    }
    first.name    <- str_match(author.name, "^[a-z]+ ") %>% trimws()
    first.initial <- substring(first.name, 1, 1)
    last.name <- gsub(first.name, "", author.name) %>%
        trimws() %>%
        gsub(" ", "", .)
    ldap <- paste(first.initial, last.name, sep = "")
    return(ldap)
}

RetrieveFromMap <- function(key, map) {
    value <- map[names(map) == key]
    return(value)
}

NamesToLdap <- function(name.list) {
    list           <- unique(name.list)
    edgecase.names <- read.csv(
        file             = './input/author_edge_cases.csv',
        header           = TRUE,
        sep              = ',',
        stringsAsFactors = FALSE,
        strip.white      = TRUE
    )
    name.map <- lapply(list, FullNameToLdap, edgecases=edgecase.names)
    names(name.map) <- list

    return(unlist(lapply(name.list, RetrieveFromMap, map=name.map)))
}

CreateBlameDataFromFile <- function(file.name) {
    blame.authors <- readLines(paste("./input/blames/", file.name, sep=""))
    repo.name <- gsub(".txt", "", file.name) %>%
        gsub("blame_", "", .)
    blame.data <- data.frame(
        Author = blame.authors,
        Repository = rep_len(repo.name, length(blame.authors))
    )
    return(blame.data)
}

CreateHistoryDataFromFile <- function(file.name) {
    repo.name   <- gsub(".txt", "", file.name)
    history.raw <- readLines(paste("./input/commits/", file.name, sep = ""))

    # Use paste to collapse the array back into a single string.
    history.raw <- paste(history.raw, collapse = "\n")

    # Filter out unwanted data, namely any commits that are empty (don't have changes).
    history.raw <- str_match_all(
        history.raw,
        regex("^[a-zA-Z]+[a-zA-Z ]*\t[0-9]{4}-[0-9]{2}-[0-9]{2}\t[a-zA-Z0-9]{40}\n.*", multiline = TRUE)
    )[[1]]

    # Get specific data from the raw format.
    # - Authors
    # - Dates
    # - Files changed
    # - Insertions
    # - Deletions
    history.authors <- str_match(history.raw, regex('^.*?\t')) %>% gsub('\t', '', .)
    history.date    <- str_match(history.raw, regex('\t[0-9]{4}-[0-9]{2}-[0-9]{2}\t')) %>%
        gsub('\t', '', .)  %>%
        as.Date()
    history.hash <- str_match(history.raw, regex('\t.{40}\n')) %>%
        gsub('\n', '', .) %>%
        gsub('\t', '', .)
    history.files.changed <- str_match(history.raw, '[0-9]+ files? changed') %>%
        gsub(' files? changed', '', .) %>%
        as.numeric()

    history.insertions <- str_match(history.raw, '[0-9]+ insertions?\\(\\+\\)') %>%
        gsub(' insertions?\\(\\+\\)', '', .) %>%
        as.numeric()
    # If there are any NA values, it means there are 0 insertions.
    history.insertions[is.na(history.insertions)] <- 0

    history.deletions  <-str_match(history.raw, '[0-9]+ deletions?\\(\\-\\)') %>%
        gsub(' deletions?\\(\\-\\)', '', .) %>%
        as.numeric()
    # Same as insertions, NA means 0 deletions.
    history.deletions[is.na(history.deletions)] <- 0

    repository.data <- data.frame(
        Author        = history.authors,
        Date          = history.date,
        FilesChanged  = history.files.changed,
        Insertions    = history.insertions,
        Deletions     = history.deletions,
        NetLineChange = (history.insertions - history.deletions),
        Repository    = rep_len(repo.name, length(history.authors)),
        Hash          = history.hash
    )
    repository.data <- repository.data %>% filter(!(Hash %in% existing.hashes))
    existing.hashes <<- c(existing.hashes, as.vector(repository.data$Hash))
    return(repository.data)
}
existing.hashes <<- c()

# Blame files are read directly from their input dir.
blame.files <-list.files("./input/blames")
blame.data <- lapply(blame.files, CreateBlameDataFromFile) %>% rbindlist()
blame.data$Author <- NamesToLdap(blame.data$Author)

# For commits history, we want to use a file with orderings
# to prevent duplicate commit hashes from appearing in our data.
# Hashes are registered in ascending order, so the oldest hash should go first.
history.ordering <- read.csv(
    "./input/repo_orderings.csv",
    header = TRUE,
    stringsAsFactors = FALSE,
    sep = ","
)
history.ordering <- history.ordering %>% arrange(Ordering)

history.data <- lapply(history.ordering$FileName, CreateHistoryDataFromFile) %>% rbindlist()
history.data$Author <- NamesToLdap(history.data$Author)

write.csv(blame.data, "./output/blames.csv")
write.csv(blame.data, "./output/commits.csv")
