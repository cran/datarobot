context("Test Lint Free Code")
library(lintr)
library(rex)

skip_lint <- TRUE

if (!skip_lint) {
  test_that("Code Lint", {
    #Skip de-linting on CRAN
    skip_on_cran()

    unquote <- function(str, q="`") {
      # Remove surrounding quotes (select either single, double or backtick) from
      # given character vector and unescape special characters.
      str <- re_substitutes(str, rex(start, q, capture(anything), q, end), "\\1")
      unescape(str, q)
    }

    escapeChars <- c(
      "\\\\" = "\\",  # backslash
      "\\n"  = "\n",  # newline
      "\\r"  = "\r",  # carriage return
      "\\t"  = "\t",  # tab
      "\\b"  = "\b",  # backspace
      "\\a"  = "\a",  # alert (bell)
      "\\f"  = "\f",  # form feed
      "\\v"  = "\v"   # vertical tab
      # dynamically-added:
      #"\\'"  = "'",  # ASCII apostrophe
      #"\\\"" = "\"", # ASCII quotation mark
      #"\\`"  = "`"   # ASCII grave accent (backtick)
    )

    unescape <- function(str, q="`") {
      names(q) <- paste0("\\", q)
      myEscapeChars <- c(escapeChars, q)
      res <- gregexpr(text=str, pattern=rex(or(names(myEscapeChars))))
      all_matches <- regmatches(str, res)
      regmatches(str, res) <- lapply(
        all_matches,
        function(string_matches) {
          myEscapeChars[string_matches]
        }
      )
      str
    }

    # Define a custom linter to require "function(x){" instead of "function(x) {"
    parenBracketLinter <- function(sourceFile) {
      lapply(lintr:::ids_with_token(sourceFile, "'{'"), function(id) {
        lineNumber <- sourceFile$parsed_content[id, ]$line1
        lineContent <- sourceFile$lines[[which(names(sourceFile$lines) == lineNumber)[[1]]]]
        if (grepl("){", lineContent, fixed = TRUE) &&
            !grepl('"){"', lineContent, fixed = TRUE)) {
          Lint(filename = sourceFile$filename,
               line_number = lineNumber,
               column_number = re_matches(lineContent, rex(")"), locations = TRUE)$start,
               type = "style",
               message = "A space should be added between ) and {.",
               line = lineContent,
               linter = "parenBracketLinter")
        }
      })
    }

    # Define a custom linter that allows either camelCase or CamelCase.
    IsCamelCase <- function(name) {
      vapply(rex(start, zero_or_more(alnum), end),
             re_matches,
             logical(1L),
             data = name)
    }
    camelCaseLinter <- lintr:::make_object_linter(
      function(sourceFile, token) {
        name <- unquote(token[["text"]])
        if (!IsCamelCase(name)) {
          lintr:::object_lint(sourceFile,
                              token,
                              "Variable or function name should be camel case.",
                              "camelCaseLinter")
        }})

    # Define custom list of lintrs
    myLinters <- list(
      line_length_linter = line_length_linter(100),
      absolute_path_linter = absolute_paths_linter,
      # camel_case_linter = camelCaseLinter, TODO: Broken... also very slow!
      paren_bracket_linter = parenBracketLinter,
      trailing_whitespace_linter = trailing_whitespace_linter,
      trailing_blank_lines_linter = trailing_blank_lines_linter,
      # infix_spaces_linter = infix_spaces_linter, TODO: Broken
      assignment_linter = assignment_linter,
      spaces_left_parentheses_linter = spaces_left_parentheses_linter,
      commas_linter = commas_linter,
      closed_curly_linter = closed_curly_linter,
      no_tab_linter = no_tab_linter,
      object_length_linter = object_length_linter,
      open_curly_linter = open_curly_linter,
      spaces_inside_linter = spaces_inside_linter
    )

    # Run lintrs
    suppressWarnings(expect_lint_free(linters = myLinters))
  })
}
