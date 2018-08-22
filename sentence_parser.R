# This is a function built off this [answer][1] for a Python solution. It allows some flexibility in that the lists of prefixes, suffixes, etc. can be modified to your specific text. 
# Definitely not perfect, but could be useful. 
# [1]: https://stackoverflow.com/a/31505798/3058123
# https://stackoverflow.com/questions/18712878/r-break-corpus-into-sentences


split_into_sentences <- function(text){
  caps = "([A-Z])"
  digits = "([0-9])"
  prefixes = "(Mr|St|Mrs|Ms|Dr|Prof|Capt|Cpt|Lt|Mt)\\."
  suffixes = "(Inc|Ltd|Jr|Sr|Co)"
  acronyms = "([A-Z][.][A-Z][.](?:[A-Z][.])?)"
  starters = "(Mr|Mrs|Ms|Dr|He\\s|She\\s|It\\s|They\\s|Their\\s|Our\\s|We\\s|But\\s|However\\s|That\\s|This\\s|Wherever)"
  websites = "\\.(com|edu|gov|io|me|net|org)"
  
  text = gsub("\n|\r\n"," ", text)
  text = gsub(prefixes, "\\1<prd>", text)
  text = gsub(websites, "<prd>\\1", text)
  text = gsub('www\\.', "www<prd>", text)
  text = gsub("Ph.D.","Ph<prd>D<prd>", text)
  text = gsub(paste0("\\s", caps, "\\. "), " \\1<prd> ", text)
  text = gsub(paste0(acronyms, " ", starters), "\\1<stop> \\2", text)
  text = gsub(paste0(caps, "\\.", caps, "\\.", caps, "\\."), "\\1<prd>\\2<prd>\\3<prd>", text)
  text = gsub(paste0(caps, "\\.", caps, "\\."), "\\1<prd>\\2<prd>", text)
  text = gsub(paste0(" ", suffixes, "\\. ", starters), " \\1<stop> \\2", text)
  text = gsub(paste0(" ", suffixes, "\\."), " \\1<prd>", text)
  text = gsub(paste0(" ", caps, "\\."), " \\1<prd>",text)
  text = gsub(paste0(digits, "\\.", digits), "\\1<prd>\\2", text)
  text = gsub("...", "<prd><prd><prd>", text, fixed = TRUE)
  text = gsub('\\.”', '”.', text)
  text = gsub('\\."', '\".', text)
  text = gsub('\\!"', '"!', text)
  text = gsub('\\?"', '"?', text)
  text = gsub('\\.', '.<stop>', text)
  text = gsub('\\?', '?<stop>', text)
  text = gsub('\\!', '!<stop>', text)
  text = gsub('<prd>', '.', text)
  sentence = strsplit(text, "<stop>\\s*")
  names(sentence) <- 'sentence'
  return(sentence)
}

# sentences <- split_into_sentences(txt_clean)
# df_sentences <- dplyr::bind_rows(sentences)
# df_sentences <- purrr::map_df(sentences, ~.x)

# samples
# small sample:
test_text <- 'Dr. John Johnson, Ph.D. worked for X.Y.Z. Inc. for 4.5 years. He earned $2.5 million when it sold! Now he works at www.website.com.'
sentences <- split_into_sentences(test_text)
df_sentences <- dplyr::bind_rows(sentences) 
dplyr::bind_rows(sentences)
df_sentences

# larger sample:
# James Joyce's Dubliners
txt_url <- 'https://www.gutenberg.org/files/2814/2814-0.txt'
txt_raw <- readr::read_file(txt_url)
txt_clean <- gsub('End of the Project Gutenberg EBook of Dubliners.+$', '', txt_raw)
txt_clean <- gsub('^.+Produced by David Reed, Karol Pietrzak, and David Widger', '', txt_clean)
txt_clean <- gsub('\r\n', ' ', txt_clean)
txt_clean <- gsub('\\s+', ' ', txt_clean)
txt_clean <- gsub('“|”', '"', txt_clean)
txt_clean <- gsub('’', "'", txt_clean)

sentences <- split_into_sentences(txt_clean)

# create a data frame using either of the following:
df_sentences <- purrr::map_df(sentences, ~.x)
df_sentences <- dplyr::bind_rows(sentences) 

# https://github.com/opetchey/RREEBES/wiki/Reading-data-and-code-from-an-online-github-repository






