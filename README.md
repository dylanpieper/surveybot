# SurveyBot

SurveyBot demonstrates conversational data collection using [shinychat](https://posit-dev.github.io/shinychat/) and Claude. Expanding on traditional surveys and interviews, users answer questions through natural conversation while the LLM extracts structured data, generates content, and asks adaptive follow-up questions.

## Usage 🍦✨

``` r
shiny::runApp("app.R")
```

The demo collects ice cream preferences through a chat interface, storing responses in a SQLite database.

## Key Features

-   **LLM data extraction** with structured schemas and validation
-   **Adaptive questioning** based on user responses
-   **Automatic retry logic** for unclear answers
-   **SQLite storage** with session tracking

## Files

-   `app.R` - Main Shiny application
-   `functions.R` - Database operations and chat utilities
-   `analyze.R` - Database analysis script
