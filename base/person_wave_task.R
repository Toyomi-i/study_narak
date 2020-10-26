# �팱�҂̖��O
PERSON <- c("Koga", "Takemoto", "Morita", "Taya", "Siga")
# �g�`�̎��
WAVE <- c("ua", "ub", "uba", "da", "db", "dba")
# �^�X�N�̏���
Koga <- c("task2","task3","task1","task4","task16","task5","task15","task6","task14","task7","task13","task8","task12","task9","task11","task10")
Takemoto <- c("task13","task14","task12","task15","task1","task16","task10","task11","task9","task2","task8","task3","task7","task4","task6","task5")
Morita <- c("task4","task5","task3","task6","task2","task7","task1","task8","task16","task9","task15","task10","task14","task11","task13","task12")
Taya <- c("task7","task8","task6","task9","task5","task10","task4","task11","task3","task12","task2","task13","task1","task14","task16","task15")
Siga <- c("task5","task6","task4","task7","task3","task8","task2","task9","task1","task10","task16","task11","task15","task12","task14","task13")
TASKS <- cbind(Koga, Takemoto, Morita, Taya, Siga)
#�����p�^�X�N����
eKoga <- c("task2","task3","task1","task4","task16","task5","task15","task6","task14","task7","task13","task8","task12","task9","task11","task10")
eTakemoto <- c("task13","task14","task12","task15","task1","task16","task10","task11","task9","task2","task8","task3","task7","task4","task6","task5")
eMorita <- c("task4","task5","task3","task6","task2","task7","task1","task8","task16","task9","task15","task10","task14","task11","task13","task12")
eTaya <- c("task7","task8","task6","task9","task5","task10","task4","task11","task3","task12","task2","task13","task1","task14","task16","task15")
eSiga <- c("task5","task6","task4","task7","task3","task8","task2","task9","task1","task10","task16","task11","task15","task12","task14","task13")
eyeTASKS <- cbind(Koga, Takemoto, Morita, Taya, Siga)
# ���̓t�@�C����(�팱�҂̉񓚏�񂪊܂܂�Ă���)
#ansFileName <- paste(storage_name, "/Reseach/analysis/work_directory/Boxplot_inputfile.csv", sep="")

library(stringr)

############################# subfunction ######################################
################################################################################
# �팱�Җ���Ԃ�
getPerson <- function(pIndex){
  return(PERSON[pIndex])
}

# �]�g�̎�ނ�Ԃ�
getWave <- function(wIndex){
  return(WAVE[wIndex])
}

# task�̎��{����Ԃ�
getTask <- function(pIndex){
  tasks <- TASKS[,colnames(TASKS) == PERSON[pIndex]]
  return(tasks)
}

getEyeTask <- function(pIndex){
  tasks <- eyeTASKS[,colnames(TASKS) == PERSON[pIndex]]
  return(tasks)
}

# �팱�҂̉񓚏���Ԃ�
getAns <- function(pIndex) {
  ## read file 1,2���character  3,4���numeric�œǂݍ���
  answerFile <- read.csv(ansFileName, header = T, stringsAsFactors=F, colClasses=c(rep("character", 2),rep("numeric",2)))
  ##���ۏ���3,4���(3:und, 4:diff)
  u_ansdata <- answerFile[answerFile$sbj == PERSON[pIndex], 3]
  d_ansdata <- answerFile[answerFile$sbj == PERSON[pIndex], 4]
  return(cbind(u_ansdata, d_ansdata))
}

# Difficulty
getDifficulty <- function(taskName){
  # Extract task number as numeric
  num <- as.numeric(str_extract_all(taskName, "[0-9.]+"))
  
  # classify difficulty
  if(num < 8){
    return("Easy")
  }else{
    return("Difficult")
  }
}

# und�t�F�C�Y����
isUndPhase <- function(wIndex){
  if(wIndex <= 3){
    result <- TRUE
  }
  else{
    result <- FALSE
  }
  return(result)
}

# ���g:1�C���g:2�C��/��:3
whichWave <- function(wIndex){
  if(wIndex %% 3 == 1){
    # alpha
    result <- 1
  }else if(wIndex %% 3 == 2){
    # beta
    result <- 2
  }else{
    # beta / alpha
    result <- 3
  }
  return(result)
}

# ���� n(�c)�̋��datafram�����
makeErowdataframe <- function(n) {
  result <- data.frame()
  result <- data.frame(matrix(rep(NA, n), nrow=1))[numeric(0), ]
  return( result )
}
# ���� n(��)�̋��datafram�����
makeEcoldataframe <- function(n) {
  result <- data.frame()
  result <- data.frame(matrix(rep(NA, n), ncol=1))[numeric(0), ]
  return( result )
}

###�������牺�ʃt�@�C���̕�����������

#���ۏ���t�^
addCorrect <- function(input, pIndex, wIndex) {
  result <- NULL
  if(isUndPhase(wIndex)){
    # und phase
    result <- transform(input, task = TASKS[,pIndex], ans = getAns(pIndex)[,1])
  }else{
    # und phase
    result <- transform(input, task = TASKS[,pIndex], ans = getAns(pIndex)[,2])
  }
  colnames(result) <- c(1:ncol(input), "task", "ans")
  result$ans[result$ans != 1] <- "failure"
  result$ans[result$ans == 1] <- "success"
  return( result )
}
#�팱�ҏ���t�^
addPerson <- function(input, person) {
  tmp <- NULL
  for (i in 1:16) {
    tmp[i] <- person
  }
  result <- transform(input, person = tmp)
  return ( result )
}
