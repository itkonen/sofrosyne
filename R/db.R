#' @importFrom R6 R6Class
#' @importFrom RPostgres dbConnect dbDisconnect dbSendQuery dbBind dbClearResult dbCreateTable dbGetQuery dbExecute dbRemoveTable
#' @importFrom lubridate now
#' @importFrom glue glue_sql glue_sql_collapse
SofrosyneDB <- R6Class(
  "SofrosyneDB",
  portable = FALSE,
  public = list(
    con = NULL,
    initialize = function() {
      system_message("Connecting to database")
      con <<- pool::dbPool(
        drv = RPostgres::Postgres(),
        host = getOption("sofrosyne.db_host"),
        user = getOption("sofrosyne.db_user"),
        db = "sofrosyne",
        password = getOption("sofrosyne.db_password"),
        maxSize = getOption("sofrosyne.db_max_connections", 5)
      ) |> with_errors("SofrosyneDB failed to connect to database")
    },
    upsert_personal_data = function(uid, data) {
      if (nrow(data) == 0) {
        return(0)
      }
      values <- glue_sql(
        "({uid}, {time}, {variable}, {value}, NOW())",
        .con = con,
        uid = uid,
        time = data$time,
        variable = data$variable,
        value = data$value
      ) |>
        glue_sql_collapse(sep = ", ")
      statement <- glue_sql(
        "INSERT INTO personal_data (uid, time, variable, value, updated_at)
         VALUES {values}
         ON CONFLICT (uid, time, variable)
         DO UPDATE SET value = EXCLUDED.value, updated_at = NOW();",
        .con = con,
        values = values
      )
      dbExecute(con, statement)
    },
    latest_value_time = function(uid, variable) {
      query <- glue_sql(
        "SELECT max(time) FROM personal_data
         WHERE uid = {uid} AND variable = {variable}",
        .con = con,
        uid = uid,
        variable = variable
      )
      dbGetQuery(con, query)$max
    },
    get_personal_data = function(uid) {
      tbl(con, "personal_data") |>
        filter(uid == !!uid)
    },
    delete_personal_data = function(uid) {
      statement <- glue_sql(
        "DELETE FROM personal_data WHERE uid = {uid}",
        .con = con,
        uid = uid
      )
      dbExecute(con, statement)
    },
    ## FITBIT TOKENS
    upsert_fitbit_token = function(token) {
      glue_sql(
        "INSERT INTO fitbit_tokens (uid, user_id, access_token, refresh_token, expires_at, scope)
         VALUES ({uid}, {user_id}, {access_token}, {refresh_token}, {expires_at}, {scope})
         ON CONFLICT (uid)
         DO UPDATE SET access_token = {access_token}, refresh_token = {refresh_token}, expires_at = {expires_at}, scope = {scope}",
        .con = con,
        uid = token$uid,
        user_id = token$user_id,
        access_token = token$access_token,
        refresh_token = token$refresh_token,
        expires_at = token$expires_at,
        scope = token$scope
      ) |>
        dbExecute(conn = con)
    },
    get_fitbit_token = function(uid = NULL) {
      glue_sql(
        "SELECT * FROM fitbit_tokens WHERE uid = {uid}",
        .con = con,
        uid = uid
      ) |>
        dbGetQuery(conn = con)
    },
    is_fitbit_authorized = function(uid) {
      query <- glue_sql(
        "SELECT count(*) FROM fitbit_tokens WHERE uid = {uid}",
        .con = con,
        uid = uid
      )
      dbGetQuery(con, query)$count > 0
    },
    ## FITBIT SUBSCRIPTIONS
    upsert_fitbit_subscription = function(uid, subscription) {
      glue_sql(
        "INSERT INTO fitbit_subscriptions (uid, collection_type, owner_id, owner_type, subscriber_id, subscription_id)
         VALUES ({uid}, {collectionType}, {ownerId}, {ownerType}, {subscriberId}, {subscriptionId})
         ON CONFLICT (uid, subscription_id)
         DO UPDATE SET collection_type = {collectionType}, owner_id = {ownerId}, owner_type = {ownerType}, subscriber_id = {subscriberId}, subscription_id = {subscriptionId}",
        .con = con,
        uid = uid,
        collectionType = subscription$collectionType,
        ownerId = subscription$ownerId,
        ownerType = subscription$ownerType,
        subscriberId = subscription$subscriberId,
        subscriptionId = subscription$subscriptionId,
      ) |>
        dbExecute(conn = con)
    },
    get_fitbit_subscriptions = function(uid) {
      glue_sql(
        "SELECT * FROM fitbit_subscriptions WHERE uid = {uid}",
        .con = con,
        uid = uid
      ) |>
        dbGetQuery(conn = con)
    }
  ),
  private = list(
    finalize = function() {
      cat("Closing database connection\n")
      pool::poolClose(self$con)
    }
  )
)
