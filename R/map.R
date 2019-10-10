library(leaflet)
getIDS <- function()
{
  require(RMariaDB)
  con <- dbConnect(
    drv = RMariaDB::MariaDB(),
    db = 'Opencpu'
  )

  res <- dbSendQuery(con, "SELECT SensorID FROM Sensor;")
  res2 <-dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
  res2
}
mapMake <- function(latP = 50.87959, lngP = 4.70093){
  require(magrittr)
  require(leaflet)
  require(RMariaDB)
  require(htmltools)
  ma <<- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    setView(zoom=4, lat = latP, lng = lngP)

  con <- dbConnect(
    drv = RMariaDB::MariaDB(),
    db = 'Opencpu'
  )

  res <- dbSendQuery(con, "SELECT SensorID, Latitude, Longitude from Sensor;")
  res2 <-dbFetch(res)

  for (row in 1:nrow(res2))
  {
    sid <- res2[row, "SensorID"]
    latid <- res2[row, "Latitude"]
    longid <- res2[row, "Longitude"]
    mapAdd(latid, longid, sid)
  }

  dbClearResult(res)
  dbDisconnect(con)

  mapGet()
}

mapAdd <- function(latP = 48.874065, lngP = 9.596336, sid = 929){
  require(magrittr)
  require(leaflet)
  require(htmlwidgets)
  require(widgetframe)
  sidString <- toString(sid)
  ma <<- ma %>% addMarkers(lat = latP, lng = lngP, popup = sidString)
  mapGet()
}


mapGet <- function(){
  require(htmlwidgets)
  require(widgetframe)

  ma

  htmlwidgets::saveWidget(widgetframe::frameableWidget(ma),
                          "mapfoto.html", selfcontained = TRUE)
}


lime <- function(varID = 698148){
  knitr::opts_chunk$set(echo = FALSE, fig.width = 40, fig.height = 30)
  Rizivnr2process <- '56172403601';
  #Rizivnr2process <- '00000000';
  #Rizivnr2process <- '000000';

  suppressPackageStartupMessages(require(extrafont))
  suppressPackageStartupMessages(require(extrafontdb))
  suppressPackageStartupMessages(extrafont::loadfonts(device="win"))

  suppressPackageStartupMessages(require(LimeRick, warn.conflicts = FALSE, quietly=TRUE))
  suppressPackageStartupMessages(require(dplyr))
  require(stringi)
  require(devtools)
  require(tibble)
  require(scales)
  require(ggplot2)
  require(ggradar)
  require(grDevices)
  suppressPackageStartupMessages(require(reshape2))
  require(knitr)
  require(mailR)
  require(viridis)

  #############################################################
  options(lsAPIurl = 'https://udas.ucll.be/limesurvey/index.php/admin/remotecontrol/')


  options(lsUser = 'ReadOnly')
  options(lsPass = 'Kcirtap!')
  options(LimeRickStats = NULL)

  surveyID <- varID # VAdvies
  # first get a session access key
  key <- lsSessionKey(action = "set", lsAPIurl = getOption("lsAPIurl"),
                      user = getOption("lsUser"), pass = getOption("lsPass"), verbose = TRUE,
                      sessionType = "global")
  questionList <- lsList(action = "questions", surveyID = surveyID, lsAPIurl = getOption("lsAPIurl"), sessionKey = key, usageStats = getOption("LimeRickStats"))
  xcol = array(data = NA, dim = c(dim(questionList)[1], 1));
  for(i in 1:dim(questionList)[1]) {
    if(questionList[i, "parent_qid"] == 0) {
      xcol[i] <- questionList[i, "title"];
    } else {
      xcol[i] <- paste0(questionList[questionList[,"qid"] == questionList[i, "parent_qid"], "title"], ".", questionList[i, "title"], ".");
    }
  }
  qL2 <- cbind(questionList, response_title=xcol);

  MainQuests <- qL2[qL2$parent_qid == 0,];
  SubQuests <- qL2[qL2$parent_qid != 0,];
  MainQIDs <- sort(as.numeric(unique(MainQuests[,"qid"])));
  MainQIDs_havingSubQ <- sort(as.numeric(unique(SubQuests[, "parent_qid"])));
  SubQuestionIDs <- sort(as.numeric(unique(SubQuests[, "qid"])));


  AllAnswers <- lsGetResponses(surveyID, documentType = "csv", languageCode = "nl", completionStatus = "complete", headingType = "code", responseType = "long", lsAPIurl = getOption("lsAPIurl"), sessionKey = key, usageStats = getOption("LimeRickStats"))
  CleanResponses <- as.data.frame(AllAnswers[AllAnswers[,"profiel4Rizivnr"] == Rizivnr2process, -(3:8)]);
  remove(AllAnswers)


  cns <- colnames(CleanResponses);
  answers <- cns[!grepl("Score", cns)];
  feedback <- cns[grepl("Score", cns)];
  rm(cns)

  maxScale <- max(as.numeric(qL2[as.numeric(qL2[, "parent_qid"]) != 0,"question_order"]))
  lbreaks <- 50;
  dispIDs <- c(CleanResponses[,"id"])

  rowID <- 1;
  colsAllData <- c("rowID", "QType", "qid", "parent_qid", "response_title", "final_question", "ordered_answers", "MaxScale");
  AllData <- matrix(data = NA, byrow = TRUE, ncol = length(colsAllData), nrow = 0);
  colnames(AllData) <- colsAllData;


  Lastquest <- 0;
  Newquest <- Lastquest;
  for(ID in MainQIDs) {
    selector <- (qL2[,"qid"] == ID);
    Newquest <- as.character(qL2[selector, "response_title"]);
    if(!grepl("Score", Newquest)) {
      # We will process only real answers (thus not feedback) with possibly subquestions
      if(Newquest != Lastquest) {
        FMTQuest <- gsub(paste0("(.{",lbreaks,",}?)\\s"), "\\1\n", gsub("<.*?>", " ", (qL2[selector, "question"])));
      }
      props <- lsGetProperties(action = 'question', surveyID, ID, languageCode = 'nl', lsAPIurl = getOption("lsAPIurl"), sessionKey = key, usageStats = getOption("LimeRickStats"))
      if(ID %in% MainQIDs_havingSubQ) {
        # We will process only real answers (thus not feedback) with subquestions, so we will loop across all subquestions
        if(!("answer" %in% names(props$answeroptions))) {
          # no real subquestions, only kind of answers
          o_a <- as.array(props$available_answers);
          AllData <- rbind(AllData, c(rowID, "MultiChoice", ID, 0, as.character(qL2[selector, "response_title"]), FMTQuest, jsonlite::toJSON(o_a), maxScale));
        }
        else {
          # real subquestions, each having same set of possible answers !
          o_a <- as.array(props$answeroptions$answer);
          RelatedSQs <- as.numeric(qL2[qL2[, "parent_qid"] == ID, "qid"]);
          for(sqID in RelatedSQs) {
            sselector <- (qL2[,"qid"] == sqID);
            FMTSQuest <- paste0(FMTQuest, gsub(paste0("(.{",lbreaks,",}?)\\s"), "\\1\n", gsub("<.*?>", " ", (qL2[sselector, "question"]))));
            AllData <- rbind(AllData, c(rowID, "SQs", sqID, ID, as.character(qL2[sselector, "response_title"]), FMTSQuest, jsonlite::toJSON(o_a), maxScale));
            rowID <- rowID + 1;
          }
        }
      }
      else {
        # We will process only real answers (thus not feedback) without subquestions
        if(is.list(props$answeroptions) && !is.null(props$answeroptions)) {
          o_a <- as.character(as.matrix(as.data.frame(props$answeroptions)[grepl("answer", colnames(as.data.frame(props$answeroptions)))]))
          AllData <- rbind(AllData, c(rowID, "SimpleQ", ID, 0, as.character(qL2[selector, "response_title"]), FMTQuest, jsonlite::toJSON(o_a), maxScale));
          rowID <- rowID + 1;
        }
      }
    }
    Lastquest <- Newquest;
  }


  # The "0" column will contain the reference dietist
  Responses <- array(data = NA, dim = c(dim(AllData)[1], length(dispIDs)+1), dimnames = list(AllData[, "response_title"], c(as.character(dispIDs), '0')));
  for (cname in colnames(CleanResponses)[-1]) {
    if(cname %in% AllData[, "response_title"]) {
      for(respondent in dispIDs) {
        pos <- which(jsonlite::fromJSON(AllData[AllData[, "response_title"] == cname, "ordered_answers"]) == CleanResponses[CleanResponses[, "id"] == respondent, cname]);
        if(length(pos) > 0)
          Responses[cname, which(respondent == colnames(Responses))] <- as.numeric(pos);

      }
    }
  }


  # Input of Reference ("Gold standard")
  Responses['KVerwervenVaklit.K1.', '0'] <- 3.4;
  Responses['KVerwervenVaklit.K2.', '0'] <- 2.0;
  Responses['KVerwervenVaklit.K3.', '0'] <- 2.5;

  Responses['KVerwervenNavorming.K4.', '0'] <- 2.8;
  Responses['KVerwervenNavorming.K5.', '0'] <- 2.6;
  Responses['KVerwervenNavorming.K6.', '0'] <- 1.5;
  Responses['KVerwervenNavorming.K7.', '0'] <- 1.5;
  Responses['KVerwervenEval', '0'] <- 4.0;

  Responses['KDelenDeelname.KD1.', '0'] <- 2.6;
  Responses['KDelenDeelname.KD2.', '0'] <- 2.5;
  Responses['KDelenDeelname.KD3.', '0'] <- 1.4;
  Responses['KDelenDeelname.KD4.', '0'] <- 1.2;
  Responses['KDelenPresentatie.KD5.', '0'] <- 1.6;
  Responses['KDelenPresentatie.KD6.', '0'] <- 1.1;
  Responses['KDelenAuteur.KD7.', '0'] <- 1.2;
  Responses['KDelenAuteur.KD8.', '0'] <- 1.2;
  Responses['KD9andere', '0'] <- 1.0;

  Responses['Begeleiden.KD10.', '0'] <- 2.0;
  Responses['Begeleiden.KD11.', '0'] <- 2.0;
  Responses['Begeleiden.KD12.', '0'] <- 2.4;
  Responses['Begeleiden.KD13.', '0'] <- 1.9;

  Responses['Consultaties1.C2.', '0'] <- 4.0;
  Responses['Consultaties1.C3.', '0'] <- 5.0;
  Responses['Consultaties1.C4.', '0'] <- 5.3;
  Responses['Consultaties1.C5.', '0'] <- 5.0;
  Responses['Consultaties1.C6.', '0'] <- 4.6;
  Responses['Consultaties1.C7.', '0'] <- 5.2;
  Responses['Consultaties1.C8.', '0'] <- 5.1;
  Responses['Consultaties1.C9.', '0'] <- 5.0;
  Responses['Consultaties1.C10.', '0'] <- 5.0;
  Responses['Consultaties2.C11.', '0'] <- 5.4;
  Responses['Consultaties2.C12.', '0'] <- 5.1;
  Responses['Consultaties2.C13.', '0'] <- 5.2;
  Responses['Consultaties3.C14.', '0'] <- 5.0;
  Responses['Consultaties3.C15.', '0'] <- 5.7;
  Responses['Consultaties3.C16.', '0'] <- 5.6;
  Responses['Consultaties3.C17.', '0'] <- 5.0;
  Responses['Consultaties3.C18.', '0'] <- 5.0;

  Responses['Consultaties5.C19huisarts.', '0'] <- 3.7;
  Responses['Consultaties5.C19artsspecialist.', '0'] <- 3.6;
  Responses['Consultaties5.C19psycholoog.', '0'] <- 2.8;
  Responses['Consultaties5.C19verpleegkundige.', '0'] <- 3.2;
  Responses['Consultaties5.C19kinesist.', '0'] <- 1.9;
  Responses['Consultaties5.C19ergotherapeut.', '0'] <- 1.6;
  Responses['Consultaties5.C19tandarts.', '0'] <- 1.1;
  Responses['Consultaties5.C19gynaecoloog.', '0'] <- 1.0;
  Responses['Consultaties5.C19andere.', '0'] <- 1.0;

  Responses['Praktijkinrichting.P1.', '0'] <- 4.4;
  Responses['Praktijkinrichting.P2.', '0'] <- 5.2;

  Responses['G1', '0'] <- 1.0;

  Responses['G2.G2.', '0'] <- 4.8;
  Responses['G2.G3.', '0'] <- 4.5;
  Responses['G2.G4.', '0'] <- 5.4;
  Responses['G2.G5.', '0'] <- 4.8;
  Responses['G2.G6.', '0'] <- 4.5;
  Responses['G2.G7.', '0'] <- 5.0;
  Responses['G2.G8.', '0'] <- 4.0;
  Responses['G2.G9.', '0'] <- 5.4;
  Responses['G2.G10.', '0'] <- 4.7;

  Responses['Zelfreflectie1.E1.', '0'] <- 4.9;
  Responses['Zelfreflectie1.E2.', '0'] <- 4.4;
  Responses['Zelfreflectie1.E3.', '0'] <- 4.2;
  Responses['Zelfreflectie1.E4.', '0'] <- 4.4;

  Responses['Management1.M1.', '0'] <- 4.7;
  Responses['Management1.M2.', '0'] <- 5.0;
  Responses['M3', '0'] <- 6.0;
  Responses['M4', '0'] <- 1.6;
  Responses['M5', '0'] <- 4.9;
  dispIDs <- c(dispIDs, 0);




  # Remove Profile info
  BaseColSelect <- !grepl("profiel", rownames(Responses))

  # Find items that can be clustered
  ResponsesF <- as.data.frame(Responses[BaseColSelect,]);
  rn_resp <- rownames(ResponsesF);
  rn_clustresp <- rn_resp[grep("\\.", rownames(ResponsesF))];
  rn_nonclustresp <- setdiff(rn_resp, rn_clustresp);
  cluster_titles <- unique(substr(rn_clustresp, 1, regexpr(".", rn_clustresp, fixed = TRUE)-1));

  ResponsesClust <- as.data.frame(matrix(data = NA, nrow = length(rn_nonclustresp) + length(cluster_titles),
                                         ncol = dim(ResponsesF)[2], dimnames = list(c(rn_nonclustresp, cluster_titles), colnames(ResponsesF))));

  FQuestions <- as.data.frame(unlist(AllData[,'final_question'][BaseColSelect]));
  rownames(FQuestions) <- rn_resp;
  colnames(FQuestions) <- 'Label';

  lbreaks <- 30;
  ResponsesClust[rn_nonclustresp, ] <- ResponsesF[rn_nonclustresp, ]
  Labels4Plot <- data.frame(row.names = c(rn_nonclustresp, cluster_titles), stringsAsFactors = FALSE);
  Labels4Plot[rn_nonclustresp, 'Label'] <- gsub(paste0("(.{",lbreaks,",}?)\\s"), "\\1\n", gsub("<.*?>", " ", (gsub("\r?\n|\r", " ", as.character(FQuestions[rn_nonclustresp, 'Label'])))));

  if(dim(Responses)[2] > 1) {
    for (r in cluster_titles){
      ResponsesClust[r,] <- colMeans(ResponsesF[regexpr(paste0(r, "."), rownames(ResponsesF), fixed = TRUE) == TRUE,]);
      phrases <- as.character(FQuestions[regexpr(paste0(r, "."), rownames(ResponsesF), fixed = TRUE) == TRUE,]);
      qs <- strsplit(phrases, '');
      qs <- lapply(qs, `length<-`, max(nchar(phrases)))
      qsm <- do.call(rbind, qs);
      csl <- which.max(apply(qsm, 2, function(col) !length(unique(col)) == 1)) - 1;
      Labels4Plot[r, 'Label'] <- gsub(paste0("(.{",lbreaks,",}?)\\s"), "\\1\n", gsub("<.*?>", " ", (gsub("\r?\n|\r", " ", substr(phrases[1], 1, csl)))));
    }
  } else {
    for (r in cluster_titles){
      ResponsesClust[r,] <- mean(Responses[regexpr(paste0(r, "."), rownames(Responses), fixed = TRUE) == TRUE,]);
      phrases <- as.character(FQuestions[regexpr(paste0(r, "."), rownames(ResponsesF), fixed = TRUE) == TRUE,]);
      qs <- strsplit(phrases, '');
      qs <- lapply(qs, `length<-`, max(nchar(phrases)))
      qsm <- do.call(rbind, qs);
      csl <- which.max(apply(qsm, 2, function(col) !length(unique(col)) == 1)) - 1;
      Labels4Plot[r, 'Label'] <- gsub(paste0("(.{",lbreaks,",}?)\\s"), "\\1\n", gsub("<.*?>", " ", (gsub("\r?\n|\r", " ", substr(phrases[1], 1, csl)))));
    }
  }


  ##############################################################################
  #
  # Clustered Plot Stuff
  #
  ##############################################################################
  ClData4Plot <-  as.data.frame(t(ResponsesClust));
  rownames(ClData4Plot) <- c(format.Date(CleanResponses[,'submitdate'], "%d-%b-%Y"), "Referentie");
  ColSelect <- colSums(is.na(ClData4Plot))<nrow(ClData4Plot); # Remove columns with all NA
  ColSelect <- ColSelect & (colSums(is.na(ClData4Plot)) == 0) # Remove columns with one or more  NA
  ClData4Plot <- ClData4Plot[, ColSelect]/maxScale;

  naxes <- dim(ClData4Plot)[2];
  ClData4PlotRadar <- as.data.frame(ClData4Plot);
  rownames_to_column(ClData4PlotRadar, "Participant") -> ClData4PlotRadar
  spinneweb <- ggradar(ClData4PlotRadar, grid.min = 0, grid.max = 1,
                       axis.labels = Labels4Plot[ColSelect,'Label'],
                       font.radar = "sans", axis.label.size = 9, axis.label.offset = 1.05,
                       values.radar = seq(0,1,0.5),
                       group.line.width = 1,
                       group.point.size = 4,
                       group.colours = viridis::plasma(dim(ClData4PlotRadar)[1]+1),
                       grid.label.size = 30,
                       legend.text.size = 25,
                       legend.title = "");
  return(spinneweb)
}

