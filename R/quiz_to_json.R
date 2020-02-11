#' Convert FDS quizzes to json structure.
#'
#' @param quiz_string A string containing one quiz question (be careful to defining it as a string with either `'` or `"` depending on which one is used inside the quiz itself.
#' @return A string containing the quiz in json structure
quiz_to_json <- function(quiz_string){

  quiz_string <- str_replace_all(quiz_string, '"', "'")

  question <-
    str_extract(quiz_string, "##\\s+Question[\\s\\w\\d\\n\`><[:punct:]=%\\+]*##\\s+Multiple") %>%
    str_remove("^##\\s+Question[^\\w]+") %>%
    str_remove("##[-\\n\\t]*##\\s+Multiple$") %>%
    str_trim() %>%
    str_replace_all("```r", "\n```r\n") %>%
    str_replace_all("```[^rR]", "\n```\n") %>%
    str_replace_all("```$", "\n```\n")

  status <- str_extract_all(quiz_string, "Status: [:upper:]{4,5}") %>%
    map(~ str_remove(., "Status: ")) %>%
    pluck(1)
  value_ix <- str_locate_all(quiz_string, "Value: ")[[1]]
  hint_ix <- str_locate_all(quiz_string, "Hint: ")[[1]]
  hash_ix <- str_locate_all(quiz_string, "##--")[[1]]
  hash_ix <- hash_ix[map_int(hint_ix[,1], ~which(hash_ix[,1] > .)[1]), ]

  value <- str_sub(quiz_string, value_ix[,2], hint_ix[,1] - 1) %>%
    str_trim() %>%
    str_replace_all("```r", "\n```r\n") %>%
    str_replace_all("```[^rR]", "\n```\n") %>%
    str_replace_all("```$", "\n```\n")
  hint <- str_sub(quiz_string, hint_ix[,2], hash_ix[,1] - 1) %>%
    str_trim() %>%
    str_replace_all("```r", "\n```r\n") %>%
    str_replace_all("```[^rR]", "\n```\n") %>%
    str_replace_all("```$", "\n```\n")

  choices <- rep("", nrow(value_ix))
  for (ii in 1:nrow(value_ix)){

    choices[ii] <- str_glue(
'
  {{text: "{value[ii]}",
   hint: "{hint[ii]}",
   is_correct: {str_to_lower(status[ii])}}}
'
    )
  }
  choices <- glue::glue_collapse(choices, sep = ",\n")

  success_message <-
    str_extract(quiz_string, "##\\s+Answer[\\s\\w\\d\\n\`><[:punct:]=%\\+]*") %>%
    str_remove("^##\\s+Answer[^\\w]+")

  question_json <-
    str_glue(
      '{{
question: "{question}",
answers: [
{choices}
],
success_message: "{success_message}"
}}'
    )

  question_json

}
