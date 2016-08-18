#query for all facts based of of min/max of dates in infile/refsfile



db_user <- ""
db_password <- ""
infile <- "/Users/allenbross/Desktop/R_working_directory/DQC-2016_new.csv"
refsfile <- "/Users/allenbross/Desktop/R_working_directory/DQC-2016_refs.csv"

# creates files:
# - infile-awk.csv - the "properties" in infile are joined to the preceding line
# - infile-sed.csv - ^M characters and tabs are removed from the awk file
# - infile-tabular.csv - above plus extra columns from research base
# - infile-w-refs.csv - with refs jammed in

# parsing strategy:
# use awk to join lines that do not start with an accession number to the
# previous line.  This makes the last column a very long message that includes
# the message as well as all the properties.  Read in that csv and then process
# the long message column so that the properties are extracted from the long
# message afterwards.

# data.frame for the parsed data set
output <- data.frame()

# names of columns in a "regular row"
rowheader <- c("accession_number", "company_name", "form", "filing_date",
               "message_id", "exact_rule", "general_rule", "severity", "long_message")

# awk, print a newline and then line contents if line starts with accession
# number.  Otherwise, just pring the line contents without a newline
awkfile <- paste0(unlist(strsplit(infile, "\\.csv"))[1], "-awk.csv")
awkcommand <- paste0("cat ", infile, " | awk '/^\"[0-9]{10}-[0-9]{2}-[0-9]{6}/ {printf(\"\\n%s\", $0); next} {printf(\"  %s\", $0);}' > ", awkfile)
system(awkcommand)

# There can also be ^M (windows style new lines) that mess things up
# get rid of tabs as well to be safe
sedfile <- paste0(unlist(strsplit(infile, "\\.csv"))[1], "-sed.csv")
sedcommand <- paste0("cat ", awkfile, " | sed -E 's/\r|\t/ /g' > ", sedfile)
system(sedcommand)

output <- read.csv(sedfile, col.names = rowheader)

# extract the properties from long_message.  If long_message does not include
# the property, then for some reason the whole long_message is repeated.  So,
# afterwards go in and check and set NA where the property is not actually
# there.
output$property_1 <- sub(".* The properties of this (.*) fact are.*", "\\1", output$long_message)
output$period <- sub(".* Period: (.*?) (Dimensions|Unit|Rule version): .*", "\\1", output$long_message)
output$dimensions <- sub(".* Dimensions: (.*?) (Period|Unit|Rule version): .*", "\\1", output$long_message)
output$unit <- sub(".* Unit: (.*?) (Period|Dimensions|Rule version): .*", "\\1", output$long_message)
output$rule_id <- sub(".* Rule version: (.*)", "\\1", output$long_message)

output$property_1[!grepl("The properties of this", output$long_message)] <- NA
output$period[!grepl("Period:", output$long_message)] <- NA
output$dimensions[!grepl("Dimensions:", output$long_message)] <- NA
output$unit[!grepl("Unit:", output$long_message)] <- NA
output$rule_id[!grepl("Rule version:", output$long_message)] <- NA

# get message from long message
output$message <- sub("(.*) The properties of this.*", "\\1", output$long_message)
output <- output[ , - which(colnames(output) == "long_message")]

# if message is giant table text block, just say so without contents
output$message[grepl("\\[Table Text Block\\]", output$message)] <- "Table Text Block"
output$message[grepl("\\[Text Block\\]", output$message)] <- "Text Block"

# Subset to only DQC.US rule violations
output <- subset(output, grepl("DQC\\.US\\.", exact_rule))

# # subset 0005 to not include some specific forms
# no05 <- which(output$general_rule == 'DQC.US.0005' &
#               (output$form == 'S-1' | output$form == 'S-1/A' |
#                output$form == 'S-11' | output$form == 'S-11/A'))
# output <- output[-no05, ]

# Remove duplicates (ignore dimensions in duplicate because they might be
# reported in a different order on an otherwise duplicate)
combos <- output[ , -which(colnames(output) == "dimensions")]
output <- output[!duplicated(combos), ]

# grab additional columns from database
library(RPostgreSQL)
db_server <- "rltest.markv.com"
db_port <- 8084
db_name <- "debug3_db"
con <- dbConnect(PostgreSQL(), host=db_server, port=db_port,
              user=db_user, password=db_password, dbname=db_name)

query <- "
SELECT DISTINCT
    rb.accession_number,
    rb.cik,
    rb.sic,
    rb.taxonomy,
    CASE
     WHEN rb.creation_software LIKE '%XBRL Document Created with Wdesk from Workiva%'::TEXT THEN 'Workiva'::TEXT
     ELSE rb.creation_software
END AS creation_software,
    rb.entry_url,
    dc.count AS fact_count,
    rb.filer_status,
    rb.document_period_end_date 
FROM
    _mv_research_base_2014_ongoing rb
    JOIN _mv_datapoint_counts dc ON dc.report_id = rb.report_id
"

cs.data <- dbGetQuery(con, query)

# merge in extra columns
output <- merge(output, cs.data, by = "accession_number", all.x = TRUE)

# write
midway <- paste0(unlist(strsplit(infile, "\\.csv"))[1], "-tabular.csv")
write.csv(output, file = midway, row.names = FALSE)


# add message references

# A single message can have multiple references.
# Each reference corresponds to a single row in the reference data set, but in
# our final data set we want a single row for each message with extra columns
# tacked on for extra message references.
# strategy will be to group refs by accession_number * message_id while
# concatenating the respective ref columns with a separator of '|||'.
# Following, we split that single column into multiple column based on the
# separator.

refs <- read.csv(refsfile, col.names = c("accession_number", "message_id", "ref1", "ref2", "ref3", "ref4", "ref5"))
# only keep refs with no NA's
refs <- refs[complete.cases(refs),  ]

# convert into a data.table and concatenate ref1, ref2, and ref3 columns for
# rows that share the same accession_number and message_id
library(data.table)
refsDT <- as.data.table(refs, keys = keys)
refsDT2 <- refsDT[ , list(ref1all = paste0(ref1, collapse = "|||"),
                          ref2all = paste0(ref2, collapse = "|||"),
                          ref3all = paste0(ref3, collapse = "|||")), by = c("accession_number", "message_id")]

# function to split the concatenated ref1/2/3 into multiple columns
splitfunc <- function(x)
{
    # Allow for up to five rows of refs all put onto single row
    y <- strsplit(x, "\\|\\|\\|")
    a <- unlist(lapply(y, function(x) c(x, rep(NA, max(0, 5 - length(x))))))
    list(a[1], a[2], a[3], a[4], a[5])
}

# split into multiple columns
refsDT2[ , c("ref1A", "ref1B", "ref1C", "ref1D", "ref1E") := splitfunc(ref1all)]
refsDT2[ , c("ref2A", "ref2B", "ref2C", "ref2D", "ref2E") := splitfunc(ref2all)]
refsDT2[ , c("ref3A", "ref3B", "ref3C", "ref3D", "ref3E") := splitfunc(ref3all)]

# drop the concatenated columns
refsDT2[ , ref1all := NULL]
refsDT2[ , ref2all := NULL]
refsDT2[ , ref3all := NULL]

# merge refs with messages
output <- merge(output, refsDT2, by = c("accession_number", "message_id"), all.x = TRUE)

# write
finished <- paste0(unlist(strsplit(infile, "\\.csv"))[1], "-w-refs.csv")
write.csv(output, file = finished, row.names = FALSE)
View(output)
