library(tidyverse)
library(glue)
library(ollamar)
library(progressr)
library(R.utils)

set.seed(1234)

model_name <- "llama3.2:1b"

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

OUTPUT
Return exactly one value and nothing else:
- the death place as written in the biography
- or NA

EXAMPLES

Example 1
Biography: 'died in Washington, D.C., April 10, 1907; interment in Lakeside Cemetery'
Answer: Washington, D.C.

Example 2
Biography: 'died near Lebanon, Wilson County, Tenn., August 19, 1867; interment in Cedar Grove Cemetery'
Answer: near Lebanon, Wilson County, Tenn.

Example 3
Biography: 'died in St. Louis, Mo., November 20, 1886; interment in Hazelwood Cemetery'
Answer: St. Louis, Mo.

Example 4
Biography: 'elected to the One Hundred Second and to the seventeen succeeding Congresses (January 3, 1991-present)'
Answer: NA

Example 5
Biography: 'died October 1, 1954; interment in Laurel Springs Baptist Church Cemetery'
Answer: NA

BIOGRAPHY
\"\"\"
{txt}
\"\"\""
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

safe_extract_death_place <- purrr::possibly( 
  function(txt) {
    withTimeout(
      extract_death_place(txt),
      timeout = 30,         # Maximum 10 seconds per biography
      onTimeout = "error"   # Throws an error to be caught by possibly() if it hangs
    )
  }, 
  otherwise = NA_character_ 
)


# Load Data ---------------------------------------------------------------

mp_data <- read_csv("data/raw/MP_data/MP_data.csv", show_col_types = FALSE)

mp_profiles <- mp_data |>
  filter(
    !is.na(bio_deathday) | 
  as.Date(bio_deathday) > as.Date("1970-01-01")
  ) |> 
  distinct(id_bioguide, .keep_all = TRUE) |>
  transmute(id_bioguide, bio_profile_text) |> 
  sample_n(50)

handlers(handler_progress(
  format = "[:bar] :percent | ETA: :eta | Row :current/:total",
  clear = FALSE, width = 60
))

death_place_labels <- with_progress({
  p <- progressor(steps = nrow(mp_profiles))
  mp_profiles |>
    mutate(
      Death_Place_llama3 = map_chr(bio_profile_text, \(txt) {
        res <- safe_extract_death_place(txt)
        p()
        Sys.sleep(0.1)
        res
      })
    ) |>
    select(id_bioguide, Death_Place_llama3)
})

mp_data_llama3 <- mp_data |>
  left_join(death_place_labels, by = "id_bioguide")

write_csv(mp_data_llama3, "data/fmt/MP_data_llama3.csv", na = "")

print(death_place_labels, n = Inf)
