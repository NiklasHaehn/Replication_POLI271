library(tidyverse)
library(glue)
library(ollamar)
library(progressr)
library(R.utils)

set.seed(1234)
options(error = NULL)

model_name <- "llama3.2:latest"

prompt_death_place <- function(txt) {
  glue(
    "You extract the place of death from biographies of members of the U.S. House of Representatives.

TASK
Extract the place where the person died, if it is explicitly stated in the biography.

SCOPE
Only extract the place of death of the person described in the biography.
Do not extract:
- place of birth
- place of burial or interment
- place of residence
- place of work
- place of education
- any other location

DECISION RULE
Return the death place only if the biography explicitly states where the person died.

Examples of valid evidence:
- 'died in Washington, D.C.'
- 'died near Lebanon, Wilson County, Tenn.'
- 'died in Kansas City, Mo.'
- 'died on May 13, 2007, in Charleston, S.C.'

Return NA if:
- no death place is stated
- only the burial place or cemetery is given
- the location is ambiguous or unclear
- city or state is missing

OUTPUT
Return exactly one value and nothing else:
- format: {City}, {State}
- {State} = full U.S. state name (e.g., South Carolina, Missouri, Texas); use District of Columbia for Washington, D.C.
- or NA

EXAMPLES

Example 1
Biography: 'died in Washington, D.C., April 10, 1907; interment in Lakeside Cemetery'
Answer: Washington, District of Columbia

Example 2
Biography: 'died near Lebanon, Wilson County, Tenn., August 19, 1867; interment in Cedar Grove Cemetery'
Answer: Lebanon, Tennessee

Example 3
Biography: 'died in St. Louis, Mo., November 20, 1886; interment in Hazelwood Cemetery'
Answer: St. Louis, Missouri

Example 4
Biography: 'elected to the One Hundred Second and to the seventeen succeeding Congresses (January 3, 1991-present)'
Answer: NA

Example 5
Biography: 'died October 1, 1954; interment in Laurel Springs Baptist Church Cemetery'
Answer: NA

BIOGRAPHY
\"\"\"
<<txt>>
\"\"\""
    ,
    .open = "<<",
    .close = ">>"
  )
}

normalize_death_place <- function(x) {
  out <- x |>
    coalesce("") |>
    str_replace_all("[\r\n]+", " ") |>
    str_squish()
  
  if (out == "") return(NA_character_)
  
  out_lower <- str_to_lower(out)
  
  if (out_lower %in% c("na", "n/a", "none", "null", "unknown", "not mentioned")) {
    return(NA_character_)
  }
  
  out |>
    str_remove_all("^['\"]|['\"]$") |>
    str_squish() |>
    na_if("")
}

death_primary_keywords <- c(
  "died",
  "passed away",
  "was killed",
  "killed",
  "slain",
  "deceased"
)

death_context_keywords <- c(
  "until his death",
  "until her death",
  "his death in",
  "her death in",
  "death in",
  "death at",
  "death near",
  "death on"
)

death_keyword_regex <- regex(
  str_c(
    "\\b(",
    str_c(c(death_primary_keywords, death_context_keywords), collapse = "|"),
    ")\\b"
  ),
  ignore_case = TRUE
)

death_primary_regex <- regex(
  str_c("\\b(", str_c(death_primary_keywords, collapse = "|"), ")\\b"),
  ignore_case = TRUE
)

death_exclude_regex <- regex(
  str_c(
    c(
      "vacancy caused by the death of",
      "fill the vacancy caused by the death of",
      "presumptive death certificate",
      "death of (the )?(governor|representative|senator|president|speaker)"
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

extract_death_sentence <- \(txt) {
  txt_clean <- txt |>
    coalesce("") |>
    str_replace_all("[\r\n]+", " ") |>
    str_squish()
  
  if (txt_clean == "") return(NA_character_)
  
  segments <- txt_clean |>
    str_split(";", simplify = FALSE) |>
    pluck(1) |>
    str_squish() |>
    discard(~ .x == "")
  
  if (length(segments) == 0) return(NA_character_)
  
  candidates <- segments |>
    keep(~ str_detect(.x, death_keyword_regex) && !str_detect(.x, death_exclude_regex))
  
  if (length(candidates) == 0) return(NA_character_)
  
  primary_hits <- candidates |> keep(~ str_detect(.x, death_primary_regex))
  
  (if (length(primary_hits) > 0) primary_hits[[1]] else candidates[[1]]) |>
    na_if("")
}

extract_death_place <- function(txt) {
  if (txt |> coalesce("") |> str_squish() == "") return(NA_character_)
  
  resp <- generate(
    model_name,
    prompt_death_place(txt),
    temperature = 0
  )
  
  resp |>
    resp_process("text") |>
    normalize_death_place()
}

llm_prompt_echo_regex <- regex(
  str_c(
    c(
      "^\\s*#\\s*extracting place of death",
      "extracting place of death from u\\.s\\. house of representatives biographies",
      "^\\s*##\\s*task\\b",
      "you extract the place of death from biographies",
      "def extract_death_place\\(biography\\)\\: if "
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

safe_extract_death_place <- \(txt, max_tries = 3, timeout_sec = 9) {
  last_error <- NA_character_
  
  for (attempt in seq_len(max_tries)) {
    base::setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    
    out <- tryCatch(
      {
        value <- withTimeout(
          extract_death_place(txt),
          timeout = timeout_sec,
          elapsed = timeout_sec,
          onTimeout = "silent"
        )
        list(
          value = if (is.null(value)) NA_character_ else value,
          error = if (is.null(value)) "timeout" else NA_character_
        )
      },
      interrupt = \(e) list(value = NA_character_, error = conditionMessage(e)),
      error = \(e) list(value = NA_character_, error = conditionMessage(e)),
      finally = base::setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    )
    
    if (!is.list(out) || !all(c("value", "error") %in% names(out))) {
      last_error <- "invalid result structure"
      next
    }
    
    value <- out$value
    value <- if (is.character(value) && length(value) == 1) value else NA_character_
    
    error <- out$error
    error <- if (
      is.character(error) &&
        length(error) == 1 &&
        !is.na(error) &&
        str_squish(error) != ""
    ) error else NA_character_
    
    is_prompt_echo <- !is.na(value) && str_detect(value, llm_prompt_echo_regex)
    
    if (is_prompt_echo) {
      last_error <- "prompt_echo"
      next
    }
    
    if (!is.na(value)) return(list(value = value, error = NA_character_))
    
    last_error <- if (!is.na(error)) error else "NA_result"
  }
  
  list(value = NA_character_, error = glue("retry_exhausted: {last_error}"))
}


# Load Data ---------------------------------------------------------------

mp_data <- read_csv("data/raw/MP_data/MP_data.csv", show_col_types = FALSE)

mp_profiles <- mp_data |>
  filter(
    !is.na(bio_deathday) &
    as.Date(bio_deathday) > as.Date("1970-01-01")
  ) |>
  distinct(id_bioguide, .keep_all = TRUE) |>
  transmute(
    id_bioguide,
    bio_profile_text,
    bio_profile_death_sentence = map_chr(bio_profile_text, extract_death_sentence)
  ) |> 
  filter(!is.na(bio_profile_death_sentence))

handlers(handler_progress(
  format = "[:bar] :percent | ETA: :eta | Row :current/:total",
  clear = FALSE, width = 60
))

death_place_labels <- with_progress({
  p <- progressor(steps = nrow(mp_profiles))
  mp_profiles |>
    mutate(
      llm_result = map2(bio_profile_death_sentence, row_number(), \(txt, i) {
        res <- tryCatch(
          safe_extract_death_place(txt),
          interrupt = \(e) list(value = NA_character_, error = conditionMessage(e)),
          error = \(e) list(value = NA_character_, error = conditionMessage(e))
        )
        
        err <- res$error |>
          (\(x) if (is.character(x) && length(x) == 1 && !is.na(x) && str_squish(x) != "") x else NA_character_)()
        
        if (!is.na(err)) message(glue("Row {i} ({id_bioguide[i]}) failed: {err}"))
        
        p()
        Sys.sleep(0.1)
        list(value = res$value, error = err)
      }),
      Death_Place_llama3 = map_chr(llm_result, \(x) x$value),
      Death_Place_error = map_chr(llm_result, \(x) x$error)
    ) |>
    select(id_bioguide, Death_Place_llama3, Death_Place_error)
})

mp_data_llama3 <- mp_data |>
  left_join(death_place_labels, by = "id_bioguide")

write_csv(mp_data_llama3, "data/fmt/MP_data_llama3.csv", na = "")
write_csv(death_place_labels, "data/fmt/MP_deathplaces_NH.csv")

#print(death_place_labels, n = Inf)
