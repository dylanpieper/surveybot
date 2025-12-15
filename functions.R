import("DBI")
import("RSQLite")
import("coro")
import("ellmer")
import("promises")
import("cli")

# Database Setup ----

#' Initialize SQLite database with schema
#' @param db_path Path to database file
#' @param db_driver Database driver (default RSQLite::SQLite())
init_database <- \(db_path = "survey.db", db_driver = RSQLite::SQLite()) {
  con <- dbConnect(db_driver, db_path)

  dbExecute(con, "PRAGMA foreign_keys = ON")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sessions (
      session_id INTEGER PRIMARY KEY AUTOINCREMENT,
      started_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      completed_at TIMESTAMP,
      completed BOOLEAN NOT NULL DEFAULT FALSE,
      retry_count INTEGER DEFAULT 0,
      question_set_version TEXT DEFAULT '1.0'
    )
  ")

  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sessions_completed ON sessions(completed)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sessions_started ON sessions(started_at)")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS responses (
      response_id INTEGER PRIMARY KEY AUTOINCREMENT,
      session_id INTEGER NOT NULL,
      question_id TEXT NOT NULL,
      question_order INTEGER NOT NULL,
      question_text TEXT,
      raw_input TEXT NOT NULL,
      extracted_value TEXT,
      answered_clearly BOOLEAN,
      responded_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      retry_attempt INTEGER DEFAULT 0,

      FOREIGN KEY (session_id) REFERENCES sessions(session_id) ON DELETE CASCADE
    )
  ")

  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_responses_session ON responses(session_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_responses_question ON responses(question_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_responses_order ON responses(session_id, question_order)")

  dbDisconnect(con)
  cli_alert_success("Database initialized at {.file {db_path}}")
}

# Database Operations ----

#' Start a new survey session
#' @param con Database connection
#' @param version Question set version
#' @return session_id
start_session <- \(con, version = "1.0") {
  dbExecute(
    con,
    "INSERT INTO sessions (question_set_version) VALUES (?)",
    params = list(version)
  )

  dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
}

#' Save a question response
#' @param con Database connection
#' @param session_id Integer session ID
#' @param question_id Question identifier
#' @param question_order Order in survey
#' @param question_text Actual question text shown
#' @param raw_input User's raw input
#' @param extracted_value Cleaned/extracted value
#' @param answered_clearly Boolean quality flag
#' @param retry_attempt Which attempt (0 = first)
save_response <- \(con, session_id, question_id, question_order, question_text,
  raw_input, extracted_value = NULL, answered_clearly = NULL,
  retry_attempt = 0) {
  dbExecute(
    con,
    "INSERT INTO responses
     (session_id, question_id, question_order, question_text, raw_input,
      extracted_value, answered_clearly, retry_attempt)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      as.integer(session_id),
      as.character(question_id),
      as.integer(question_order),
      as.character(question_text),
      as.character(raw_input),
      if (is.null(extracted_value)) NULL else as.character(extracted_value),
      if (is.null(answered_clearly)) NULL else as.logical(answered_clearly),
      as.integer(retry_attempt)
    )
  )
}

#' Mark session as completed
#' @param con Database connection
#' @param session_id Integer session ID
complete_session <- \(con, session_id) {
  dbExecute(
    con,
    "UPDATE sessions SET completed = TRUE, completed_at = CURRENT_TIMESTAMP WHERE session_id = ?",
    params = list(session_id)
  )
}

#' Increment retry count for session
#' @param con Database connection
#' @param session_id Integer session ID
increment_retry <- \(con, session_id) {
  dbExecute(
    con,
    "UPDATE sessions SET retry_count = retry_count + 1 WHERE session_id = ?",
    params = list(session_id)
  )
}

# Async Chat Operations ----

#' Extract structured response from user input (synchronous)
#' @param chat Chat object
#' @param user_response User's text input
#' @param schema Extraction schema
#' @return Extracted data
extract_response <- \(chat, user_response, schema) {
  chat$clone()$set_turns(list())$chat_structured(user_response, type = schema)
}

#' Generate content using templates
#' @param chat Chat object
#' @param template_config Template configuration with prompt, schema, response_field
#' @param context_data List of values for template placeholders
#' @return Generated content
generate_content <- \(chat, template_config, context_data = list()) {
  prompt <- do.call(sprintf, c(list(template_config$prompt), context_data))
  result <- chat$clone()$set_turns(list())$chat_structured(prompt, type = template_config$schema)
  result[[template_config$response_field]]
}

#' Build context string from template and field mapping
#' @param template Template string with %s placeholders
#' @param field_mapping List of field names to extract from responses
#' @param responses Response data
#' @return Formatted context string
build_context <- \(template, field_mapping, responses) {
  context_values <- lapply(field_mapping, \(field) responses[[field]])
  do.call(sprintf, c(list(template), context_values))
}

#' Generate adaptive follow-up question (synchronous)
#' @param chat Chat object
#' @param responses Current response data
#' @param context_template Template for building context from responses
#' @param prompt Prompt template for question generation
#' @return Question text
#' @deprecated Use generate_content with follow_up template instead
generate_adaptive_question_async <- \(chat, responses, context_template, prompt) {
  context <- sprintf(
    context_template,
    responses$name,
    responses$ice_cream,
    responses$why_favorite
  )

  full_prompt <- paste(context, prompt, sep = "\n")

  chat$clone()$set_turns(list())$chat(full_prompt)
}

# Utility Functions ----

#' Personalize question text with user responses
#' @param text Question text with {placeholders}
#' @param responses List of response values
#' @return Personalized text
personalize_text <- \(text, responses) {
  result <- text
  for (key in names(responses)) {
    if (key %in% c("adaptive_question_text", "adaptive_question_response", "answered_clearly")) next
    if (grepl("_raw$", key)) next
    if (grepl("_answered_clearly$", key)) next
    pattern <- sprintf("\\{%s\\}", key)
    result <- gsub(pattern, responses[[key]], result)
  }
  result
}

#' Create streaming bot response generator
#' @param message Message to stream
#' @param response_delay Initial delay before streaming (default from config)
#' @param character_delay Delay between characters (default from config)
#' @return Async generator
bot_response <- async_generator(function(message, response_delay = NULL, character_delay = NULL) {
  # Use provided delays or defaults from parent environment
  if (is.null(response_delay)) response_delay <- 0.3
  if (is.null(character_delay)) character_delay <- 0.02
  
  await(async_sleep(response_delay))
  chars <- strsplit(as.character(message), "", useBytes = FALSE)[[1]]
  for (char in chars) {
    yield(char)
    await(async_sleep(character_delay))
  }
})

