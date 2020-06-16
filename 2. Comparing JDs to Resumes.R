current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

#======================================================================================================================
# Text Mining
#======================================================================================================================

library(readtext)
library(quanteda)

# Increase the number of threads from default 2 threads to speed up computations
quanteda_options("threads" = 4)

# 1. Job Descriptions
# Import Job Description text file
JD <- readtext("Job Description.txt")

# Generate tokens from JD & token cleaning
JD.corpus <- corpus(JD)
JD.tokens1 <- tokens(JD.corpus, remove_punct = T, remove_numbers = T)
JD.tokens2 <- tokens_remove(JD.tokens1, pattern = stopwords('en'))
JD.tokens3 <- tokens_wordstem(JD.tokens2)
JD.tokens4 <- tokens_tolower(JD.tokens3)

JD.DFM <- dfm(JD.tokens4)

# A quick look at JD.DFM
dim(JD.DFM)
table(as.data.table(t(JD.DFM))$'Job Description.txt') # Too many tokens (77) that only appear 1 time

# Remove 'noise' tokens that only appear once
JD.DFM <- dfm_trim(JD.DFM,min_termfreq = 2, verbose = FALSE)

# Visualising the keywords in the job description
textplot_wordcloud(JD.DFM, max_words = 100)

# Convert to a data.table & allocate a score to each word, calculated by the word's frequency as a percentage in the JD
JD.DFM <- as.data.table(t(JD.DFM))
names(JD.DFM) <- c("keyword", "freq")
JD.DFM[, score := freq/sum(freq)]
View(JD.DFM)

# 2. Resume
# Import text files from a folder
resume.data <- readtext("Resumes/*.txt",
                        docvarsfrom = "filenames",
                        encoding = "UTF-8")

# Generate tokens from resume.data & clean tokens
resume.corpus <- corpus(resume.data)
resume.tokens1 <- tokens(resume.corpus, remove_punct = T, remove_numbers = T)
resume.tokens2 <- tokens_remove(resume.tokens1, pattern = stopwords('en'))
resume.tokens3 <- tokens_wordstem(resume.tokens2)
resume.tokens4 <- tokens_tolower(resume.tokens3)

# Iterate through the resumes & generate a score for each applicant
i = 1
candidate_scores = c()
for (applicant in names(resume.tokens4))
{
  common_words <- intersect(JD.DFM$keyword, resume.tokens4[[applicant]])
  candidate_score <- JD.DFM[keyword %in% common_words, sum(score)]
  candidate_scores[i] <- candidate_score
  i = i + 1
}

# Create a table to store candidate names & their scores
result <- data.table(candidate_name = names(resume.tokens4), score = candidate_scores * 100)
result <- result[order(-candidate_scores)]
View(result)

#======================================================== END =========================================================