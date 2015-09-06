reportPrep = function(reportType,users,notes,noteResponses,questions,answers,qaMap){
  noteResponses = subset(noteResponses,noteResponses$question_id !=0)
  numCrashes = length(unique(subset(noteResponses,noteResponses$question_id == 28)$note_id))
  numIssues = length(unique(subset(noteResponses,noteResponses$question_id == 31)$note_id))
  numReports = length(unique(noteResponses$note_id))
  if(reportType==0){
    crashList = unique(subset(noteResponses, noteResponses$question_id==28)$note_id)
    crashResponses = subset(noteResponses,noteResponses$note_id %in% crashList)
    crashes = subset(notes, notes$id %in% crashList)
    
    cols = c("note_id","severity","conflict_NA","conflict_1","conflict_2","conflict_3",
             "conflict_4","conflict_5","conflict_6","conflict_7",
             "conflict_8","conflict_9","conflict_10",
             "action_NA","action_1","action_2","action_3","action_4",
             "action_5","action_6","action_7","action_8",
             "action_9","action_10",
             "reason_NA","reason_1","reason_2","reason_3","reason_4",
             "reason_5","reason_6","reason_7","reason_8",
             "reason_9","reason_10","reason_11")
    crashModelTab = data.frame(matrix(nrow=length(crashList),ncol = length(cols)))
    colnames(crashModelTab)=cols
    crashList = crashes$id
    crashModelTab$note_id=crashList
    m_qids=c(29,32,33)
    multiple=cols[3:length(cols)]
    offsets = c(4,15,26)
    
    ##Severity
    qid = 28
    possibleAnswers = subset(qaMap,qaMap$question_id == qid)$answer_id
    levels = rev(subset(answers,answers$id %in% possibleAnswers)$text)
    levels = levels[2:length(levels)]
    
    for (i in 1:nrow(crashModelTab)){
      nid = crashModelTab$note_id[i]
      if (qid %in% subset(crashResponses,crashResponses$note_id == nid)$question_id){
        response = subset(crashResponses, crashResponses$note_id == nid & crashResponses$question_id == qid)
        text = answers$text[answers$id == response$answer_id]
        if(identical(text,"No severity level indicated")){
          crashModelTab[i,"severity"] = NA
        }else{
          crashModelTab[i,"severity"] = text
        } 
      }else{
        crashModelTab[i,"severity"] = NA
      }
    }
    crashModelTab[,"severity"] = factor(crashModelTab[,"severity"],levels = levels, ordered = TRUE)
    
    ###Multiple Choice
    crashModelTab[,multiple]=FALSE
    
    for (j in 1:length(m_qids)){
      qid = m_qids[j]
      off = offsets[j]
      ansInds = qaMap[qaMap$question_id == qid,"answer_id"]
      possibleAnswers = answers[answers$id %in% ansInds,"text"]
      for (i in 1:nrow(crashModelTab)){
        nid = crashModelTab$note_id[i]
        response = subset(noteResponses, noteResponses$note_id == nid & noteResponses$question_id == qid)
        if (nrow(response)>0){
          selected = which(ansInds %in% response$answer_id)+off-1
          crashModelTab[i,selected]=TRUE
        }else{
          if(qid ==29){
            crashModelTab[i,3]=TRUE
          }else if(qid==32){
            crashModelTab[i,14]=TRUE
          }else if(qid==33){
            crashModelTab[i,25]=TRUE
          }
        }
      }
    }
    
    return(crashModelTab)
    
  }else if (reportType==1){
    issueList = unique(subset(noteResponses, noteResponses$question_id==31)$note_id)
    issueResponses = subset(noteResponses,noteResponses$note_id %in% issueList)
    issues = subset(notes,notes$id %in% issueList)
    
    cols = c("note_id","urgency","issueType_NA","issueType_1","issueType_2","issueType_3",
             "issueType_4","issueType_5","issueType_6","issueType_7",
             "issueType_8","issueType_9","issueType_10","issueType_11",
             "issueType_12","issueType_13","issueType_14")
    issueModelTab = data.frame(matrix(nrow=length(issueList),ncol = length(cols)))
    colnames(issueModelTab)=cols
    issueList = issues$id
    issueModelTab$note_id=issueList
    
    ##Urgency
    qid = 31
    possibleAnswers = subset(qaMap,qaMap$question_id == qid)$answer_id
    levels = subset(answers,answers$id %in% possibleAnswers)$text
    levels = levels[1:length(levels)-1]
    
    for (i in 1:nrow(issueModelTab)){
      nid = issueModelTab$note_id[i]
      if (qid %in% subset(issueResponses,issueResponses$note_id == nid)$question_id){
        response = subset(issueResponses, issueResponses$note_id == nid & issueResponses$question_id == qid)
        text = answers$text[answers$id == response$answer_id]
        if(identical(text,"No urgency level indicated")){
          issueModelTab[i,"urgency"] = NA
        }else{
          issueModelTab[i,"urgency"] = text
        } 
      }else{
        issueModelTab[i,"urgency"] = NA
      }
    }
    issueModelTab[,"urgency"] = factor(issueModelTab[,"urgency"],levels = levels, ordered = TRUE)
    
    ###Multiple Choice
    issueModelTab[,3:ncol(issueModelTab)]=FALSE
    qid = 30
    off = 4
    ansInds = qaMap[qaMap$question_id == qid,"answer_id"]
    possibleAnswers = answers[answers$id %in% ansInds,"text"]
    for (i in 1:nrow(issueModelTab)){
      nid = issueModelTab$note_id[i]
      response = subset(noteResponses, noteResponses$note_id == nid & noteResponses$question_id == qid)
      if (nrow(response)>0){
        selected = which(ansInds %in% response$answer_id)+off-1
        issueModelTab[i,selected]=TRUE
      }else{
        issueModelTab[i,3]=TRUE
      }
    }
    return(issueModelTab)
  }
}














