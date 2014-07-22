
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

kf.sna.show.old = function(){
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

kf.sna.shiny.install = function(){
  install.packages("rCharts")
  install.packages("markdown")
  install.packages("shiny")
}

kf.sna.shiny = function(){
  library(markdown)
  library(rCharts)
  library(shiny)
  runApp()
}

kf.sna.show = function(){
  g <- kf.sna.createigraph()
  plot(g, layout=layout.circle, vertex.size=15, vertex.label.color="black", vertex.color="red", edge.arrow.size=0.5, edge.curved=F)
}

kf.sna.createigraph = function(){
  primaryAuthorIds <- posts$primaryAuthorId
  names(primaryAuthorIds) = unique(posts$guid)
  authornames <- authors$userName
  names(authornames) = unique(authors$guid)
  
  readlogs = subset(logs, operationType=="READ" & entityType=="POST")
  relations = data.frame(readlogs$userName, authornames[primaryAuthorIds[readlogs$entityId]])
  g <- graph.data.frame(relations, directed=TRUE)
  return(g);
}

kf.sna.gephi.static = function(){
  library(rgexf)
  gexf <- igraph.to.gexf(kf.sna.createigraph())
  print(gexf, "kf.gexf")
}

kf.sna.gephi.dynamic = function(){
  library(rgexf)
  
  primaryAuthorIds <- posts$primaryAuthorId
  names(primaryAuthorIds) = unique(posts$guid)
  authornames <- authors$userName
  names(authornames) = unique(authors$guid)
  
  readlogs = subset(logs, operationType=="READ" & entityType=="POST")  
  authors = data.frame("id"=authors$userName, "name"=authors$userName)
  edges = data.frame("source"=readlogs$userName, "target"=authornames[primaryAuthorIds[readlogs$entityId]])
  edges_dynamic = data.frame("start"=kf.sna.gephi.converttime(readlogs$accessTime), "end"=NA)
  write.gexf(nodes = authors, edges = edges, edgeDynamic = edges_dynamic, tFormat="dateTime", defaultedgetype = "directed", output = "kfdynamic.gexf")
  #write.gexf(nodes = nodes_df, edges = edges_df, edgesAtt = edges_att, nodesVizAtt = nodes_att_viz, edgesVizAtt = edges_att_viz, defaultedgetype = "undirected", output = "lesmis.gexf")}
}

kf.sna.gephi.converttime = function(time){
  datetime = strptime(time, "%b %d, %Y %I:%M:%S %p")
  converted = as.character(datetime, "%Y-%m-%dT%H:%M:%S")
  return(converted)
}

kf.sna.condor3 = function(){
  primaryAuthorIds <- posts$primaryAuthorId
  names(primaryAuthorIds) = unique(posts$guid)
  authornames <- authors$userName
  names(authornames) = unique(authors$guid)
  
  readlogs = subset(logs, operationType=="READ" & entityType=="POST")    
  relations = data.frame("source"=readlogs$userName, "target"=authornames[primaryAuthorIds[readlogs$entityId]], "timestamp"=kf.sna.condor3.converttime(readlogs$accessTime))
  write.table(relations, file="link.csv", sep=",", row.names= FALSE)
  authors = data.frame("id"=authors$userName, "name"=authors$userName, "timestamp"="NA")
  write.table(authors, file="actor.csv", sep=",", row.names=FALSE)
}

kf.sna.condor3.converttime = function(time){
  datetime = strptime(time, "%b %d, %Y %I:%M:%S %p")
  converted = as.character(datetime, "%d/%m/%Y %H:%M")
  return(converted)
}