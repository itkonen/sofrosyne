library(RPostgres)
## library(pool)

## sofrosyne:::connect_db()
## con <- sofrosyne:::.env$con
## con <<- pool::dbPool( # Not supported for pool objects, use DBI directly
con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  host = getOption("sofrosyne.db_host"),
  user = getOption("sofrosyne.db_user"),
  db = "sofrosyne",
  password = getOption("sofrosyne.db_password")
)

## Test connection
dbGetQuery(con, "SELECT NOW();")


## PERSONAL DATA
dbCreateTable(
  con,
  "personal_data",
  fields = c(
    uid = "varchar(48)",
    time = "timestamp",
    variable = "varchar(32)",
    value = "numeric",
    updated_at = "timestamp"
  )
)

dbSendQuery(
  con,
  "CREATE UNIQUE INDEX personal_data_idx ON personal_data (uid, time, variable)"
)

### Delete personal data
delete_personal_data_table <- function() {
  if (menu(
    "Are you sure you want to delete the personal_data table?",
    c("Yes", "No"),
    title = "Delete personal_data table"
  ) == 1) {
    dbRemoveTable(con, "personal_data")
  }
}

## FITBIT TOKENS
con |>
  dbCreateTable(
    "fitbit_tokens",
    fields = c(
      uid = "varchar(48)",
      user_id = "varchar(16)",
      access_token = "varchar(255)",
      refresh_token = "varchar(255)",
      expires_at = "timestamp",
      scope = "varchar(255)"
    )
  )

dbGetQuery(
  con,
  "ALTER TABLE fitbit_tokens
   ADD CONSTRAINT unique_uid UNIQUE (uid);"
)
## dbGetQuery(
##   con,
##   "CREATE INDEX idx_uid ON fitbit_tokens (uid);"
## )

## FITBIT SUBSCRIPTIONS
con |>
  dbCreateTable(
    "fitbit_subscriptions",
    fields = c(
      uid = "varchar(48)",
      collection_type = "varchar(17)",
      owner_id = "varchar(32)",
      owner_type = "varchar(16)",
      subscriber_id = "varchar(32)",
      subscription_id = "varchar(50)"
    )
  )

dbGetQuery(
  con,
  "ALTER TABLE fitbit_subscriptions
   ADD CONSTRAINT unique_uid_subscription_id UNIQUE (uid, subscription_id);"
)

## Test tables created
dbListTables(con)

## Should show "personal_data", "fitbit_tokens", "fitbit_subscriptions"
