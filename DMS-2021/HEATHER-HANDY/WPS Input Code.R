setwd("C:/Users/heath/Downloads/WPS Assignment")
library(tidyverse)
supersub <- read_csv("supersub-homework-input.csv")

###########################################################################################
###COMMENT: col1 through col7 use the separate() functions to split by the colon, 
###then by the first comma to separate the sub-questions from the rating scores
###the first split using colon causes the rating to split together with the next sub-question, which facilitates the need to split again by the first comma (which directly follows the rating)
###Each col group corresponds to the question groups that are being split to respective sub questions and their ratings
col1 <- separate(supersub,
                 'Access/Setting/Overall Experience',
                 into = 
                   c(
                     "1",
                     "2",
                     "3",
                     "4",
                     "5",
                     "6")
                 ,
                 sep = (":"), extra = "drop" ) 

col1 <- separate(col1,
                 '2',
                 into =
                   c( "Access/Setting/Overall Experience: Overall educational experience",
                      "10"),
                 sep = (",") )

col1 <- separate(col1,
                 '3',
                 into =
                   c( "Access/Setting/Overall Experience:  Accessibility of platforms/materials",
                      "11"),
                 sep = (","))

col1 <- separate(col1,
                 '4',
                 into =
                   c( "Access/Setting/Overall Experience: Technical quality (e.g., audio/visual experience)",
                      "12"),
                 sep = (","))

col1 <- separate(col1,
                 '5',
                 into =
                   c( "Access/Setting/Overall Experience: Accessing the webinar (e.g., purchasing/registering, logging in)",
                      "13"),
                 sep = (","))


col1 <- rename(col1, 'Access/Setting/Overall Experience: Clarity and timeliness of information/materials received (e.g., emails from WPS, course materials)' = '6')


#################################################################################################################
col2 <- separate(supersub,
                 'Quality of Instruction - Instructor Ratings - Somer Bishop, PhD',
                 into = 
                   c(
                     "1",
                     "2",
                     "3",
                     "4",
                     "5",
                     "6")
                 ,
                 sep = (":"), extra = "drop" ) 

col2 <- separate(col2,
                 '2',
                 into =
                   c( "Quality of Instruction - Instructor Ratings - Somer Bishop, PhD: Provided useful historical and current information about autism spectrum disorder",
                      "10"),
                 sep = (",") )

col2 <- separate(col2,
                 '3',
                 into =
                   c( "Quality of Instruction - Instructor Ratings - Somer Bishop, PhD:  Clarified content in response to questions from participants",
                      "11"),
                 sep = (","))

col2 <- separate(col2,
                 '4',
                 into =
                   c( "Quality of Instruction - Instructor Ratings - Somer Bishop, PhD: Presented content using methods appropriate to the content",
                      "12"),
                 sep = (","))

col2 <- separate(col2,
                 '5',
                 into =
                   c( "Quality of Instruction - Instructor Ratings - Somer Bishop, PhD: Presented content consistent with the learning objectives",
                      "13"),
                 sep = (","))


col2 <- rename(col2, 'Quality of Instruction - Instructor Ratings - Somer Bishop, PhD: Knowledgeable in relevant content areas)' = '6')


##################################################################################################
col3 <- separate(supersub,
                 'Quality of Instruction - Instructor Ratings - So Hyun "Sophy" Kim, PhD',
                 into = 
                   c(
                     "1",
                     "2",
                     "3",
                     "4",
                     "5",
                     "6")
                 ,
                 sep = (":") ) 

col3 <- separate(col3,
                 '2',
                 into =
                   c( "Quality of Instruction - Instructor Ratings - So Hyun "Sophy" Kim, PhD: Provided useful historical and current information about autism spectrum disorders",
                      "10"),
                 sep = (",") )

col3 <- separate(col3,
                 '3',
                 into =
                   c( "Quality of Instruction - Instructor Ratings - So Hyun "Sophy" Kim, PhD:  Clarified content in response to questions from participants",
                      "11"),
                 sep = (","))

col3 <- separate(col3,
                 '4',
                 into =
                   c( "Quality of Instruction - Instructor Ratings - So Hyun "Sophy" Kim, PhD: Presented content using methods appropriate to the content",
                      "12"),
                 sep = (","))

col3 <- separate(col3,
                 '5',
                 into =
                   c( "Quality of Instruction - Instructor Ratings - So Hyun "Sophy" Kim, PhD: Presented content consistent with the learning objectives",
                      "13"),
                 sep = (","))


col3 <- rename(col3, 'Quality of Instruction - Instructor Ratings - So Hyun "Sophy" Kim, PhD: Knowledgeable in relevant content areas' = '6')

#################################################################################
col4 <- separate(supersub,
                 'Quality of Instruction - Course Features',
                 into = 
                   c(
                     "1",
                     "2",
                     "3",
                     "4",
                     "5")
                 ,
                 sep = (":") ) 

col4 <- separate(col4,
                 '2',
                 into =
                   c( "Quality of Instruction - Course Features: Other interactive features (e.g., polling, chat/Q&A)",
                      "10"),
                 sep = (",") )

col4 <- separate(col4,
                 '3',
                 into =
                   c( "Quality of Instruction - Course Features:  Interactive content (e.g., group discussion of administration and coding)",
                      "11"),
                 sep = (","))

col4 <- separate(col4,
                 '4',
                 into =
                   c( "Quality of Instruction - Course Features: Demonstrations (e.g., video-based case examples)",
                      "12"),
                 sep = (","))


col4 <- rename(col4, 'Quality of Instruction - Course Features: Lecture(s)' = '5')


###########################################################################################################
col5 <- separate(supersub,
                    'Learning Objectives',
                    into = 
                      c(
                        "1",
                        "2",
                        "3",
                        "4",
                        "5",
                        "6",
                        "7",
                        "8",
                        "9")
                    ,
                    sep = (":") ) 

col5 <- separate(col5,
                    '2',
                    into =
                      c( "Learning Objectives: Explain issues in the clinical application of ADOS-2 results",
                         "10"),
                    sep = (",") )

col5 <- separate(col5,
                    '3',
                    into =
                      c( "Learning Objectives:  Identify the basic psychometric support for the ADOS-2",
                         "11"),
                    sep = (","))

col5 <- separate(col5,
                    '4',
                    into =
                      c( "Learning Objectives: Demonstrate a basic understanding of how to score ADOS-2 algorithms",
                         "12"),
                    sep = (","))
col5 <- separate(col5,
                 '5',
                 into =
                   c( "Learning Objectives: Demonstrate a basic understanding of how to apply ADOS-2 codes",
                      "13"),
                 sep = (","))

col5 <- separate(col5,
                 '6',
                 into =
                   c( "Learning Objectives: Identify administration procedures for the ADOS-2",
                      "14"),
                 sep = (","))

col5 <- separate(col5,
                 '7',
                 into =
                   c( "Learning Objectives: Explain how to select the most appropriate ADOS-2 module",
                      "15"),
                 sep = (","))

col5 <- separate(col5,
                 '8',
                 into =
                   c( "Learning Objectives: Describe use of the ADOS-2 for operationalizing diagnostic criteria for autism spectrum disorders",
                      "16"),
                 sep = (","))

col5 <- rename(col5, 'Learning Objectives: Identify the key features of the ADOS-2 approach, including the rationale for the approach and how it enhances autism assessment' = '9')


########################################################################################
col6 <- separate(supersub, 
                 'Learning Self-Evaluation',
                 into = 
                   c("1", "Learning Self-Evaluation: How much did you learn as a result of this CE program?")
                 ,
                 sep = ":")
#######################################################################################
col7 <- separate(supersub, 
  'Usefulness of Content',
  into = 
    c("1", "Usefulness of Content: How useful was the content of this CE program for your practice or other professional development?")
  ,
  sep = ":")

######################################################################################
######################################################################################
##COMMENT: 'combine' binds all 7 datasets together that were created from the separate ratings and 'super_q' is the dataset
##with all unnecessary columns removed, leaving only columns containing the ratings
combine <- bind_cols(col1, col2, col3, col4, col5, col6, col7)
super_q <- subset(combine, select = -c(1, 3, 5, 7, 9, 11:18, 20, 22, 24, 26, 28:35, 37, 39, 41, 43, 45:52, 
                                    54, 56, 58, 60:67, 69, 71, 73, 75, 77, 79, 81, 83:90, 92:99))

##This line replaces all spaces with underscores in the column names
colnames(super_q) %<>% str_replace_all("\\s", "_")

##Exporting final clean data to .csv file
write.csv(super_q, "C:/Users/heath/Downloads/WPS Assignment/My Work/supersub-clean-input.csv", row.names = FALSE)
