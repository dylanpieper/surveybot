# surveybot

surveybot enables conversational data collection using [shinychat](https://posit-dev.github.io/shinychat/). Expanding on traditional surveys and interviews, users answer questions through natural dialogue while the LLM extracts structured data, generates content, and asks adaptive follow-up questions.

## Usage 🍦✨

``` r
shiny::runApp("app.R")
```

The demo collects ice cream preferences through a shinychat UI. The files are modular to make your own surveybot (package to come).

## Key Features

-   **LLM data extraction** with structured schemas and validation
-   **Adaptive questioning** based on user responses
-   **Automatic retry logic** for unclear answers
-   **SQL storage** with tables "sessions" and "responses" (default is SQLite)

## Files

-   `app.R` - Main Shiny application
-   `functions.R` - Database operations and chat utilities
-   `analyze.R` - Database analysis script
