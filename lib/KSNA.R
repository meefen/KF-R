kfservers = c("http://localhost:8080/kforum/", "http://132.203.154.41:8080/kforum/")

kf.sna.init = function(){
  return (kf.sna.getdata())
}

kf.sna.getdata = function(){
  print(data.frame(kfservers))
  server = readline("server? ");
  if(nchar(server) <= 0){
    return();
  } 
  serverNo = strtoi(server)
  url = kfservers[serverNo]
    
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
  print("Retrieving...")
  posts <<- GetSectionPosts(url, communityId, cookie)
  authors <<- GetAllAuthors(url, communityId, cookie)
  logs <<- GetLogs(url, viewId, cookie)
  
  return ("OK you can check SNA by 'kf.sna.show()'")
}

kf.sna.show = function(){
  library(igraph)
  g <- kf.sna.createigraph()
  plot(g, layout=layout.circle, vertex.size=15, vertex.label.color="black", vertex.color="red", edge.arrow.size=0.5, edge.curved=F)
}

kf.sna.createigraph = function(){
  readlogs = kf.sna.readlogs()
  df <- data.frame("source"=readlogs$from, "target"=readlogs$to)
  g <- graph.data.frame(df, directed=TRUE)
  return(g);
}

kf.sna.gephi.static = function(){
  library(rgexf)
  gexf <- igraph.to.gexf(kf.sna.createigraph())
  print(gexf, "kf.static.gexf")
}

kf.sna.gephi = function(){ #dynamic
  library(rgexf)  
  authors = data.frame("id"=authors$userName, "name"=authors$userName)
  readlogs = kf.sna.readlogs.aggregated()
  edges = data.frame("source"=readlogs$from, "target"=readlogs$to)
  edges_dynamic = data.frame("start"=kf.sna.to.gephitime(readlogs$timestamp), "end"=NA)
  write.gexf(nodes = authors, edges = edges, edgeDynamic = edges_dynamic, tFormat="dateTime", defaultedgetype = "directed", output = "kf.gexf")
  #write.gexf(nodes = authors, edges = edges, edgeDynamic = edges_dynamic, tFormat="date", defaultedgetype = "directed", output = "kf.gexf")
}

kf.sna.to.gephitime = function(time){
   return (as.character(time, "%Y-%m-%dT%H:%M:%S"))
}

# kf.sna.to.gephitime = function(time){
#   return (as.character(time, "%Y-%m-%d"))
# }

kf.sna.condor3 = function(){
  authors = data.frame("id"=authors$userName, "name"=authors$userName, "timestamp"=NA)
  write.table(authors, file="actor.csv", sep=",", row.names=FALSE)  
  readlogs = kf.sna.readlogs()
  edges = data.frame("source"=readlogs$from, "target"=readlogs$to, "timestamp"=kf.sna.to.condortime(readlogs$timestamp))
  write.table(edges, file="link.csv", sep=",", row.names= FALSE)
}

kf.sna.to.condortime = function(time){
  return (as.character(time, "%d/%m/%Y %H:%M"))
}

kf.sna.readlogs = function(){
  primaryAuthorIds <- posts$primaryAuthorId
  names(primaryAuthorIds) = unique(posts$guid)
  authornames <- authors$userName
  names(authornames) = unique(authors$guid)
  
  readlogs = subset(logs, operationType=="READ" & entityType=="POST")
  readlogs = data.frame("from"=readlogs$userName, "to"=authornames[primaryAuthorIds[readlogs$entityId]], timestamp=kf.sna.time(readlogs$accessTime))
  return (readlogs)
}

kf.sna.readlogs.aggregated = function(){
  readlogs = kf.sna.readlogs()
  time = aggregate(list("timestamp"=readlogs$timestamp), list("from"=readlogs$from, "to"=readlogs$to), FUN=kf.sna.first)
  count = aggregate(list("count"=readlogs$timestamp), list("from"=readlogs$from, "to"=readlogs$to), FUN=length)
  merged = merge(time, count)
  sorted = merged[order(merged$timestamp), ]
  return(sorted)
}

kf.sna.first = function(x){
  return(x[1])
}

kf.sna.time = function(time){
  return(strptime(time, "%b %d, %Y %I:%M:%S %p"))
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

print("loading program finished. please run 'kf.sna.init()'")

# kf.sna.show.old = function(){
#   primaryAuthorIds <- posts$primaryAuthorId
#   names(primaryAuthorIds) = unique(posts$guid)
#   authornames <- authors$userName
#   names(authornames) = unique(authors$guid)
#   #posttitles <- posts$title
#   #names(posttitles) = unique(posts$guid)
#   
#   readlogs = subset(logs, operationType=="READ" & entityType=="POST")
#   #logs3 = data.frame(logs2$userName, logs2$accessTime, authornames[primaryAuthorIds[logs2$entityId]])
#   relations = data.frame(readlogs$userName, authornames[primaryAuthorIds[readlogs$entityId]])
#   g <- graph.data.frame(relations, directed=TRUE)
#   #plot(g,layout=layout.fruchterman.reingold)
#   plot(g, layout=layout.circle, vertex.size=15, vertex.label.color="black", vertex.color="red", edge.arrow.size=0.5, edge.curved=F)
# }


