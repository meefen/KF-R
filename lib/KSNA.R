
kf.sna.getdata = function(){
  url = "http://localhost:8080/kforum/"
    
  username = readline("username? ");
  if(nchar(username) <= 0){
    return();
  }  
  password = readline("password? ");
  if(nchar(password) <= 0){
    return();
  }    

  cookie = CreateCurlHandle()
  registrations = Authenticate(url, username, password, cookie)
  if(length(registrations) <= 0){
    print("login failed or the user has no registration")
    return();
  }
  print(data.frame("title"=registrations$sectionTitle))
  selectedNo = readline("Registration No.? ");
  if(nchar(selectedNo) <= 0){
    return();
  }
  registrationId = registrations[selectedNo, "guid"]
  communityId = registrations[selectedNo, "sectionId"]
  SelectCommunity(url, registrationId, cookie)
  views <<- GetSectionViews(url, communityId, cookie)
  print(data.frame("title"=views$title))
  selectedNo = readline("View No.? ");
  if(nchar(selectedNo) <= 0){
    return();
  }
  viewId = views[selectedNo, "guid"];
  posts <<- GetSectionPosts(url, communityId, cookie)
  authors <<- GetAllAuthors(url, communityId, cookie)
  logs <<- GetLogs(url, viewId, cookie)
}

kf.sna.show = function(){
  primaryAuthorIds <- posts$primaryAuthorId
  names(primaryAuthorIds) = unique(posts$guid)
  authornames <- authors$userName
  names(authornames) = unique(authors$guid)
  #posttitles <- posts$title
  #names(posttitles) = unique(posts$guid)
  
  readlogs = subset(logs, operationType=="READ" & entityType=="POST")
  #logs3 = data.frame(logs2$userName, logs2$accessTime, authornames[primaryAuthorIds[logs2$entityId]])
  relations = data.frame(readlogs$userName, authornames[primaryAuthorIds[readlogs$entityId]])
  g <- graph.data.frame(relations, directed=TRUE)
  #plot(g,layout=layout.fruchterman.reingold)
  plot(g, layout=layout.circle, vertex.size=15, vertex.label.color="black", vertex.color="red", edge.arrow.size=0.5, edge.curved=F)
}