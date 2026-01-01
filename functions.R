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
      question_set_version TEXT DEFAULT '1.0',
      duration_seconds INTEGER
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
      input_raw TEXT NOT NULL,
      input_extracted TEXT,
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
#' @param input_raw User's raw input
#' @param input_extracted Cleaned/extracted value
#' @param answered_clearly Boolean quality flag
#' @param retry_attempt Which attempt (0 = first)
save_response <- \(con, session_id, question_id, question_order, question_text,
  input_raw, input_extracted = NULL, answered_clearly = NULL,
  retry_attempt = 0) {
  dbExecute(
    con,
    "INSERT INTO responses
     (session_id, question_id, question_order, question_text, input_raw,
      input_extracted, answered_clearly, retry_attempt)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      as.integer(session_id),
      as.character(question_id),
      as.integer(question_order),
      as.character(question_text),
      as.character(input_raw),
      if (is.null(input_extracted)) NULL else as.character(input_extracted),
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
    "UPDATE sessions SET 
       completed = TRUE, 
       completed_at = CURRENT_TIMESTAMP,
       duration_seconds = CAST((julianday(CURRENT_TIMESTAMP) - julianday(started_at)) * 86400 AS INTEGER)
     WHERE session_id = ?",
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
#' @param context_data Named list of values for template placeholders
#' @return Generated content
generate_content <- \(chat, template_config, context_data = list()) {
  prompt <- interpolate(template_config$prompt, context_data)
  schema <- create_generic_schema(template_config$response_field)
  result <- chat$clone()$set_turns(list())$chat_structured(prompt, type = schema)
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

#' String interpolation with context-aware capitalization
#' @param template String with {variable} placeholders
#' @param data Named list or environment with variable values
#' @param fallback_value Value to use for missing variables (default: variable name)
#' @return Interpolated string with proper capitalization
interpolate_with_context <- \(template, data, fallback_value = NULL) {
  result <- template
  if (is.null(template)) {
    return(template)
  }

  # Extract all {variable} patterns
  matches <- gregexpr("\\{([^}]+)\\}", template)[[1]]
  if (matches[1] == -1) {
    return(template)
  }

  # Process matches in reverse order to preserve positions
  match_starts <- as.numeric(matches)
  match_lengths <- attr(matches, "match.length")

  for (i in length(match_starts):1) {
    start <- match_starts[i]
    length <- match_lengths[i]

    # Extract variable name
    var_text <- substr(template, start, start + length - 1)
    var_name <- gsub("\\{|\\}", "", var_text)

    # Get replacement value
    replacement <- if (!is.null(data[[var_name]])) {
      as.character(data[[var_name]])
    } else if (!is.null(fallback_value)) {
      as.character(fallback_value)
    } else {
      var_name
    }

    # Check if variable starts a sentence (at beginning or after ". ", "! ", "? ")
    is_sentence_start <- start == 1 || 
      grepl("[\\.\\!\\?]\\s*$", substr(result, 1, start - 1))

    # Capitalize if at sentence start
    if (is_sentence_start && !is.null(data[[var_name]])) {
      replacement <- capitalize_first(replacement)
    }

    # Replace in template
    result <- paste0(
      substr(result, 1, start - 1),
      replacement,
      substr(result, start + length, nchar(result))
    )
  }

  result
}

#' String interpolation with named placeholders
#' @param template String with {variable} placeholders
#' @param data Named list or environment with variable values
#' @param fallback_value Value to use for missing variables (default: variable name)
#' @return Interpolated string
interpolate <- \(template, data, fallback_value = NULL) {
  result <- template
  if (is.null(template)) {
    return(template)
  }

  # Extract all {variable} patterns
  matches <- gregexpr("\\{([^}]+)\\}", template)[[1]]
  if (matches[1] == -1) {
    return(template)
  }

  # Process matches in reverse order to preserve positions
  match_starts <- as.numeric(matches)
  match_lengths <- attr(matches, "match.length")

  for (i in length(match_starts):1) {
    start <- match_starts[i]
    length <- match_lengths[i]

    # Extract variable name
    var_text <- substr(template, start, start + length - 1)
    var_name <- gsub("\\{|\\}", "", var_text)

    # Get replacement value
    replacement <- if (!is.null(data[[var_name]])) {
      as.character(data[[var_name]])
    } else if (!is.null(fallback_value)) {
      as.character(fallback_value)
    } else {
      var_name
    }

    # Replace in template
    result <- paste0(
      substr(result, 1, start - 1),
      replacement,
      substr(result, start + length, nchar(result))
    )
  }

  result
}

#' Extract variable names from template strings
#' @param template Template string with {variable} placeholders
#' @return Character vector of variable names
extract_variables <- \(template) {
  if (is.null(template)) {
    return(character(0))
  }

  matches <- regmatches(template, gregexpr("\\{([^}]+)\\}", template))[[1]]
  if (length(matches) == 0) {
    return(character(0))
  }

  # Extract variable names without braces
  gsub("\\{|\\}", "", matches)
}

#' Generate generic schema for content extraction
#' @param response_field Name of the response field to extract
#' @return Schema object for structured data
create_generic_schema <- \(response_field) {
  schema_list <- list()
  schema_list[[response_field]] <- type_string(paste("Extract", response_field, "content"))
  do.call(type_object, schema_list)
}

#' Capitalize first letter of a string
#' @param text String to capitalize
#' @return String with first letter capitalized
capitalize_first <- \(text) {
  if (is.null(text) || nchar(text) == 0) {
    return(text)
  }
  paste0(toupper(substr(text, 1, 1)), substr(text, 2, nchar(text)))
}

#' Personalize question text with user responses
#' @param text Question text with {placeholders}
#' @param responses List of response values
#' @return Personalized text
personalize_text <- \(text, responses) {
  if (is.null(text)) {
    return(text)
  }

  # Filter out internal fields
  filtered_responses <- responses[!names(responses) %in% c(
    "adaptive_question_text", "adaptive_question_response", "answered_clearly"
  )]

  interpolate_with_context(text, filtered_responses)
}

#' Create streaming bot response generator
#' @param message Message to stream
#' @param response_delay Initial delay before streaming (default from config)
#' @param character_delay Delay between characters (default from config)
#' @return Async generator
bot_response <- async_generator(function(message, response_delay = NULL, character_delay = NULL) {
  # Use provided delays or defaults from parent environment
  if (is.null(response_delay)) response_delay <- 0
  if (is.null(character_delay)) character_delay <- 0.02

  await(async_sleep(response_delay))
  chars <- strsplit(as.character(message), "", useBytes = FALSE)[[1]]
  for (char in chars) {
    yield(char)
    await(async_sleep(character_delay))
  }
})
