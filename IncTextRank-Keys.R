library(NLP)
library(tm)
library(openNLP)
library(graph)
library(microbenchmark)
library(igraph)
library(fastmatch)

'%!in%' <- function(x,y)!('%in%'(x,y))

# --- Pré-processing (PP) FUNCTIONS
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}
###### illustrate usage of tagPOS
#str <- "this is a the first sentence."
#tagged_str <-  tagPOS(str)
#tagged_str

## $POStagged
## [1] "this/DT is/VBZ a/DT the/DT first/JJ sentence/NN ./."
## 
## $POStags
## [1] "DT"  "VBZ" "DT"  "DT"  "JJ"  "NN"  "."

###### Other utility functions - PP #####
SplitText <- function(Phrase) { 
  unlist(strsplit(Phrase," "))
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

IsPunctuated <- function(Phrase) {
  length(grep("\\.|,|!|\\?|;|:|\\)|]|}\\Z",Phrase,perl=TRUE))>0 # punctuation: . , ! ? ; : ) ] }
}

SelectTaggedWords <- function(Words,tagID) {
  Words[ grep(tagID,Words) ]
}

RemoveTags <- function(Words) {
  sub("/[A-Z]{2,3}","",Words)
}


### Stream Functions ####

IsSelectedWord <- function(Word, selected_words) {
  ifelse(length(which(selected_words == Word))>0, TRUE, FALSE)
}

GetWordLinks <- function(position,scope,words,selected_words) {
  scope <- ifelse(position+scope>length(words),length(words),position+scope)
  links <- ""
  for (i in (position+1):scope) {
    if ( IsSelectedWord(words[i], selected_words) ) links <- c(links,words[i])
  }
  
  if (length(links)>1) {
    links[2:length(links)]
  }
  else {
    links <- ""
  }
}

ConstructTextGraph <- function(n, words, selected_words) { 
  
  i <- 1
  while (i < length(words) ) {
    if ( IsSelectedWord(words[i], selected_words) ) {                                   
      links <- GetWordLinks(i,n,words,selected_words)                                
      if (links[1] != "") {                                     
        #cat(i," ",words[i]," - ",paste(c(links),collapse=" "),"\n")
        if ( length(which(nodes(word_graph)==words[i]))==0  ) {
          
          my.addNode(word_graph, words[i])
          word_graph <- addNode(paste(words[i]),word_graph)
        }                                               
        
        for (j in 1:length(links)) {
          if ( length(which(nodes(word_graph)==links[j]))==0 ) {
            my.addNode(word_graph, links[j])
            word_graph <- addNode(paste(links[j]),word_graph)
            #if(links[j]=="mixed") browser()
            word_graph <- addEdge(paste(words[i]),paste(links[j]),word_graph,1)
          } 
          else {
            #browser()
            if ( length(which(inEdges(links[j],word_graph)[[1]]==words[i]))>0 ) { 
              prev_edge_weight <- as.numeric(edgeData(word_graph,words[i],links[j],"weight"))
              edgeData(word_graph,words[i],links[j],"weight") <- prev_edge_weight+1
            }
            else {
              
              word_graph <- addEdge(paste(words[i]),paste(links[j]),word_graph,1)
            }
          } 
        }
      }
    }
    i <- i+1
  }
  number.of.nodes <<- numNodes(word_graph)
  word_graph <<- word_graph
}





## 1   compatibility  -  systems 
## 3   systems  -  linear 
## 5   linear  -  constraints 
## 9   set  -  natural 
## 11   natural  -  numbers criteria 
## 12   numbers  -  criteria 
## 13   criteria  -  compatibility 
## 18   system  -  linear 
## 20   linear  -  diophantine equations 
## 21   diophantine  -  equations strict 
## 22   equations  -  strict inequations 
## 23   strict  -  inequations 
## 24   inequations  -  nonstrict 
## 26   nonstrict  -  inequations 
## 30   upper  -  bounds 
## 31   bounds  -  components 
## 36   minimal  -  set 
## 37   set  -  solutions 
## 39   solutions  -  algorithms 
## 41   algorithms  -  construction 
## 43   construction  -  minimal 
## 45   minimal  -  sets 
## 47   sets  -  solutions 
## 52   types  -  systems 
## 61   corresponding  -  algorithms 
## 66   minimal  -  supporting set 
## 67   supporting  -  set 
## 68   set  -  solutions 
## 79   types  -  systems 
## 80   systems  -  systems 
## 82   systems  -  mixed 
## 84   mixed  -  types


## Visualize obtained text graph
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

#library("Rgraphviz")
#plot(text_graph, attrs = list(node = list(fillcolor = "lightblue", fontsize = 20),edge = list(arrowsize=0.5)))

#Mudar para directed networks (usar igraph)
PR <- function(d,threshold,i.graph, text_nodes,nodes_num){
  k <- 0                                  # iterations
  convergence_reached <- FALSE
  repeat {
    for (i in 1:nodes_num) {
      #incoming_link <- adj(text_graph,text_nodes[i])[[1]]
      incoming_link <- as.numeric(unlist(adjacent_vertices(i.graph, text_nodes[i], mode = c("in")))) #adj(graph,text_nodes[i])[[1]]
      incoming_num <- length(incoming_link)
      
      if(length(incoming_link)==0){
        nodes_rank[i,1] <<- d/nrow(nodes_rank)
        next
      }
      
      
      tmp <- 0
      for (j in 1:incoming_num) {
        link_num <- incoming_link[j]
        #outgoing_num <- length(adj(text_graph,text_nodes[link_num])[[1]])
        #browser()
        outgoing_num <- length(as.numeric(unlist(adjacent_vertices(i.graph, text_nodes[link_num], mode = c("out")))))
        tmp <- tmp + nodes_rank[link_num,1] / outgoing_num
      }
      nodes_rank[i,1] <<- (1-d)+d*tmp
    }
    k <- k+1
    for (i in 1:nodes_num) {
      if (abs(nodes_rank[i,1]-nodes_rank[i,2])<threshold) convergence_reached <- TRUE
    }
    if (convergence_reached) break
    nodes_rank[,2] <<- nodes_rank[,1]
  }
  nodes_rank <<- nodes_rank
}

incrementalPR <- function(d,threshold,i.graph,text_nodes, affected_nodes){
  #browser()
  k <- 0                                  # iterations
  convergence_reached <- FALSE
  rn <- rownames(nodes_rank)
  repeat {
    for (node in affected_nodes) {
      incoming_link <- as.numeric(unlist(adjacent_vertices(i.graph, node, mode = c("in")))) #adj(graph,text_nodes[i])[[1]]
      
      incoming_num <- length(incoming_link)
      
      if(length(incoming_link)==0){
        nodes_rank[paste(node),1] <<- d/nrow(nodes_rank)
        next
      }
      
      tmp <- 0.0
      
      if(incoming_num != 0){
        for (j in 1:incoming_num) {
          link_num <- incoming_link[j]
          outgoing_num <- length(adjacent_vertices(i.graph, text_nodes[link_num], mode = c("out")))#adj(text_graph,text_nodes[link_num])[[1]])
          tmp <- tmp + nodes_rank[link_num,1] / outgoing_num
        }
      }
      
      #browser()
      
      nodes_rank[which(fmatch(rn,node)==1),1] <<- (1-d) + d*tmp
    }
    
    #browser()
    
    k <- k+1
    
    for (node in affected_nodes) {
      if (abs(nodes_rank[which(fmatch(rn,node)==1),1]-nodes_rank[which(fmatch(rn,node)==1),2])<threshold) convergence_reached <- TRUE
    }
    if (convergence_reached) break
    nodes_rank[,2] <<- nodes_rank[,1]
  }
  
  nodes_rank <<- nodes_rank
  
}


#change this to addNode
my.addNode <- function(graph, node1){
  
  
  #Adds nodes if they do not exist in the graph
  is.node.in.graph <-fmatch(V(igraph.from.graphNEL(graph, name = TRUE, weight = TRUE,unlist.attrs = TRUE))$name, node1)
  
  #browser()
  
  if(1 %!in% is.node.in.graph){
    #graph <<- graph + vertices(node1.origin)
    #Update nodes_rank
    nodes_rank <<- rbind(nodes_rank, matrix(1,1,2, dimnames=list(node1,c("rank.new","rank.old"))), deparse.level = 0)
  }
  
  #if(2 %!in% is.node.in.graph){
    #graph <<- graph + vertices(node2.destination)
    #Update nodes_rank
  #  nodes_rank <<- rbind(nodes_rank, matrix(1,1,2, dimnames=list(node2.destination,c("rank.new","rank.old"))), deparse.level = 0)
  #}
  
  #adds connection between those nodes
  #graph <<- graph + edge(node1.origin, node2.destination)
  
  
}

#Update PageRank
updatePR <- function(changed.nodes.vector, nodes_num, new.nodes_num){
  
  rn <- rownames(nodes_rank)
  unchanged.nodes <- c()
  #breadth-first search for the new nodes by using changed nodes
  
  #browser()
  
  i.graph <- igraph.from.graphNEL(text_graph, name = TRUE, weight = TRUE,
                                unlist.attrs = TRUE)
  
  affected <- dfs(i.graph,root= changed.nodes.vector, neimode ="out", unreachable = FALSE, order  = TRUE, dist = FALSE, extra = NULL, rho = parent.frame())$"order"
  
  #find the subgraph of affected nodes
  affected.nodes <<- as.character(affected[which(as.character(affected) != "NA")]$name)
  
  
  
  
  #affected <- bfs(i.graph, V(i.graph)$name[fmatch(changed.nodes.vector,V(i.graph)$name)], neimode = c("out"),
  #                unreachable = TRUE, restricted = NULL, order = TRUE, rank = FALSE,
  #                father = FALSE, pred = FALSE, succ = FALSE, dist = FALSE,
  #                callback = NULL, extra = NULL, rho = parent.frame())
  
  #find the subgraph of affected nodes
  #affected.nodes <<- names(affected$"order")
  
  
  
  
  #find unchanged nodes that point to the changed nodes
  #unchanged.nodes <- adjacent_vertices(i.graph, affected.nodes, mode = c("in")) %!in% affected.nodes
  for(i in 1:length(affected.nodes)){
    vec.in.nodes <- V(i.graph)$name[as.numeric(unlist(eval(adjacent_vertices(i.graph, affected.nodes[i], mode = c("in")))))] 
    unchanged.nodes <- c(unchanged.nodes, vec.in.nodes[vec.in.nodes %!in% affected.nodes])
  }
  
  # verificar ->>>>> paste(adjacent_vertices(i.graph, affected.nodes, mode = c("in")))
  vec.graph <- as.character(V(i.graph)$name) 
  
  other.nodes <- vec.graph[vec.graph %!in% affected.nodes]  
  
  all.unchanged.nodes <- unique(c(unchanged.nodes, other.nodes)) 
  #update PR for those nodes with a simple scaling
  #number of nodes in previous Graph/number of nodes of new graph
  new.cardinality <- new.nodes_num
  old.cardinality <- nodes_num
  for(node in all.unchanged.nodes){
    #browser()
    nodes_rank[which(fmatch(rn,node)==1),1] <<- nodes_rank[which(fmatch(rn,node)==1),1]*old.cardinality/new.cardinality 
  }
  
  #calculate updated PR for the changed nodes
  incrementalPR(d,threshold,i.graph, as.character(V(i.graph)$name), affected.nodes)
  
}

post.proc <- function(words, text_nodes, nodes_num, selected_words,words_with_punctuation){
  
  # ---Incremental POST-PROCESSING
  
  keywords_num <- round(nodes_num/3) # a third of the number of vertices in the graph.
  ranked_words <- data.frame(text_nodes,nodes_rank[,1])
  names(ranked_words) <- c("word","rank")
  strong_words <- ranked_words[order(ranked_words$rank,decreasing=TRUE),]
  strong_words <- as.character(strong_words$word[1:keywords_num])
  keywords <- ""
  keywords_scores <- 0
  for (i in 1:keywords_num) {
    keyword_positions <- which(words==strong_words[i])
    for (j in 1:length(keyword_positions)) {
      keyword <- ""
      keyword_score <- 0
      k <- keyword_positions[j]                                       
      repeat {
        if (IsSelectedWord(words[k], selected_words)) { 
          keyword <- trim(paste(c(keyword,words[k]),collapse=" "))
          keyword_score <- keyword_score + ranked_words[which(ranked_words$word==words[k]),2]
        }
        else break                                                    
        
        if (IsPunctuated(words_with_punctuation[k])) break
        if (k==length(words)) break                               
        k <- k+1
      }
      k <- keyword_positions[j]-1                                 
      repeat {
        if (k<1) break
        
        if (IsSelectedWord(words[k], selected_words)) { 
          keyword <- paste(c(words[k],trim(keyword)),collapse=" ")
          keyword_score <- keyword_score + ranked_words[which(ranked_words$word==words[k]),2]
        }
        else break
        
        if (k>1) {            
          if (IsPunctuated(words_with_punctuation[k-1])) break
        } 
        k <- k-1
      }
      if (keyword!=strong_words[i]) { 
        keywords <- c(keywords,keyword)
        keywords_scores <- c(keywords_scores,keyword_score)
      }   
    }
  }
  keywords_df <- data.frame(keywords,keywords_scores)
  #keywords_list <- keywords_df[order(keywords_df$keywords_scores,decreasing=TRUE),] 
  keywords_list <- keywords_df
  keywords_list <- unique(as.character(keywords_list$keywords[1:nrow(keywords_list)]))  
  #sort(keywords_list)
  
  ##  [1] ""                             "corresponding algorithms"    
  ##  [3] "linear constraints"           "linear diophantine equations"
  ##  [5] "minimal set"                  "minimal supporting set"      
  ##  [7] "nonstrict inequations"        "strict inequations"          
  ##  [9] "types systems"                "upper bounds"
  
  #keywords_list
  
  ##  [1] "linear diophantine equations" "minimal supporting set"      
  ##  [3] "minimal set"                  "types systems"               
  ##  [5] "linear constraints"           "strict inequations"          
  ##  [7] "upper bounds"                 "corresponding algorithms"    
  ##  [9] "nonstrict inequations"        ""
  
  
}

#ver esta função melhor!!!
inc.post.proc <- function(words, text_nodes, nodes_num, selected_words,words_with_punctuation, affected.nodes){
  
  # ---Incremental POST-PROCESSING
  
  keywords_num <- round(nodes_num/3) # a third of the number of vertices in the graph.
  #keywords_num <- length(affected.nodes)
  ranked_words <- data.frame(text_nodes,nodes_rank[,1])
  names(ranked_words) <- c("word","rank")
  strong_words <- ranked_words[order(ranked_words$rank,decreasing=TRUE),]
  strong_words <- as.character(strong_words$word[1:keywords_num])
  #strong_words <- affected.nodes
  keywords <- ""
  keywords_scores <- 0
  for (i in 1:keywords_num) {
    keyword_positions <- which(words==strong_words[i])
    for (j in 1:length(keyword_positions)) {
      keyword <- ""
      keyword_score <- 0
      k <- keyword_positions[j]                                       
      repeat {
        if (IsSelectedWord(words[k], selected_words)) { 
          keyword <- trim(paste(c(keyword,words[k]),collapse=" "))
          keyword_score <- keyword_score + ifelse(length(ranked_words[which(ranked_words$word==words[k]),2])==0,0,ranked_words[which(ranked_words$word==words[k]),2])
          #if(length(keyword_score)==0)browser()
        }
        else break                                                    
        
        if (IsPunctuated(words_with_punctuation[k])) break
        if (k==length(words)) break                               
        k <- k+1
      }
      k <- keyword_positions[j]-1
      #cat(k,"\n")
      repeat {
        if (k<1) break
        
        if (IsSelectedWord(words[k], selected_words)) { 
          keyword <- paste(c(words[k],trim(keyword)),collapse=" ")
          keyword_score <- keyword_score + ifelse(length(ranked_words[which(ranked_words$word==words[k]),2])==0,0,ranked_words[which(ranked_words$word==words[k]),2])
          #if(length(keyword_score)==0)browser()
        }
        else break
        
        if (k>1) {            
          if (IsPunctuated(words_with_punctuation[k-1])) break
        } 
        k <- k-1
      }
      if (keyword!=strong_words[i]) { 
        keywords <- c(keywords,keyword)
        keywords_scores <- c(keywords_scores,keyword_score)
      } 
    }
  }
  #browser()
  keywords_df <- data.frame(keywords,keywords_scores)
  #keywords_list <- keywords_df[order(keywords_df$keywords_scores,decreasing=FALSE),] 
  keywords_list <- keywords_df
  keywords_list <- unique(as.character(keywords_list$keywords[1:nrow(keywords_list)]))  
  #sort(keywords_list)
  
  ##  [1] ""                             "corresponding algorithms"    
  ##  [3] "linear constraints"           "linear diophantine equations"
  ##  [5] "minimal set"                  "minimal supporting set"      
  ##  [7] "nonstrict inequations"        "strict inequations"          
  ##  [9] "types systems"                "upper bounds"
  
  #keywords_list
  
  ##  [1] "linear diophantine equations" "minimal supporting set"      
  ##  [3] "minimal set"                  "types systems"               
  ##  [5] "linear constraints"           "strict inequations"          
  ##  [7] "upper bounds"                 "corresponding algorithms"    
  ##  [9] "nonstrict inequations"        ""
  
  
}

#Top-k updates a structure of 10xKeywords
topk <- function(new.keys){
  
  
  sapply(new.keys, FUN = function(x){
    
    rn <- names(topk.list)
    
    j <- which(fmatch(rn,x)==1)
    
    if(length(j)!=0){
      
      # se número está dentro dos números monitorizados
      # adiciona um ao valor do novo nó - space saving
      topk.list[j]  <<-  topk.list[j] + 1
      y <- max(which(topk.list[j]>topk.list[]))
      
      
    }else{
      # se número não está dentro dos números monitorizados
      # procura número com minimo de contagem e adiciona um ao valor do novo nó - space saving
      if(length(topk.list) < window_size){#preenche a lista de keywords
        topk.list <<- setNames(c(as.numeric(topk.list), 1), c(names(topk.list), paste(x)))
        y <- NULL
        j <- NULL
      }else{
        index.node.out <- max(which(topk.list[]==min(topk.list[])))
        aux <- topk.list[index.node.out]
        topk.list[index.node.out]  <<-  aux + 1
        y <- min(which(topk.list[index.node.out]>topk.list[]))
        j <- index.node.out
        names(topk.list)[index.node.out] <- paste(x)
      }
      
    }
    
    
    if(length(topk.list)>=window_size){
      
      if(length(y)!=0){
        if(j!=y){
          topk.list <<- sort(topk.list,decreasing = TRUE)
        }
      }
    }
  })
  topk.list
}

nodes_rank <- matrix(,nrow = 0,ncol = 2)
word_graph <- new("graphNEL",edgemode="directed")
topk.list <- c()
window_size=1000
k=1000
#batch and Incremental PageRank settings
d <- 0.85                               # damping factor
threshold <- 1e-4               # convergence threshold
words <- c()
words_with_punctuation <- c()
selected_words <- c()
number.of.nodes <- 0
number.of.words <- 0

main <- function(stream.doc, iter){
  
  # --- MAIN CODE
 
  
  ### Pre-process Text ###
  ### Pré-processing Stream Functions ### 
  
  corp <- Corpus(VectorSource(stream.doc))
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, tolower)
  words_with_punctuation <<- c(words_with_punctuation,SplitText(as.character(corp[[1]])))
  corp <- tm_map(corp, removePunctuation)
  
  ### Stream functions ###
  #--- Initial and Stream GRAPH CONSTRUCTION
  
  words <<- c(words,SplitText(as.character(corp[[1]])))
  number.of.words <<- length(words)
  tagged_text <- tagPOS(corp[[1]])
  tagged_words_aux <- SplitText(as.character(tagged_text))
  tagged_words_aux <- c(SelectTaggedWords(tagged_words_aux,"/NN"),SelectTaggedWords(tagged_words_aux,"/JJ")) # keep only NN & JJ tagged words 
  tagged_words_aux <- RemoveTags(tagged_words_aux)                                                      # remove un-used tag POS
  selected_words <<- unique(c(selected_words,tagged_words_aux))                                                         
  
  ### ---  Incremental PAGE RANK Implementation
  
  #if iteration 1 then start with batch PR
  if(iter == 1){
    time.gen.graph <- microbenchmark(
      text_graph <<- ConstructTextGraph(2, words, selected_words)  # co-occurrence of window size 2
      ,list = NULL, times = 1, check = NULL,
      control = list())
    text_nodes <- nodes(text_graph)
    nodes_num <- length(text_nodes)
    #nodes_rank <<- matrix(1,nodes_num,2)
    i.graph <- igraph.from.graphNEL(text_graph, name = TRUE, weight = TRUE,
                                    unlist.attrs = TRUE)
    PR(d,threshold,i.graph, text_nodes,nodes_num)
    ### Post-Processing ###
    
    new.keys <- post.proc(words, text_nodes, nodes_num, selected_words,words_with_punctuation)
    #print(new.keys)
    ### Top-K Keywords Update ###
    final.key.list <- topk(new.keys)
   
  }else{#else use incremental PR
    #browser()
    time.gen.graph <- microbenchmark(
      text_graph <<- ConstructTextGraph(2, words, unique(tagged_words_aux))  # co-occurrence of window size 2
      ,list = NULL, times = 1, check = NULL,
      control = list())
    text_nodes <- nodes(text_graph)
    nodes_num <- length(text_nodes)
    #Counting number of nodes in graph to scale
    new.nodes_num <- length(nodes(text_graph))
    
    #if(iter==3)browser()
    
    changed.nodes.vector <- tagged_words_aux[tagged_words_aux %in% nodes(text_graph)]
    #Update PR
    updatePR(changed.nodes.vector, nodes_num, new.nodes_num)
    ### Post-Processing ###
    
    new.keys <- inc.post.proc(words, text_nodes, nodes_num, selected_words,words_with_punctuation, changed.nodes.vector)
    #print(new.keys)
    ### Top-K Keywords Update ###
    final.key.list <- topk(new.keys)
  }
  
  
  
  ### Output Top-K Keywords ###
  #print(final.key.list)
  assign(paste("iter.",iter,".key.list",sep=""), final.key.list, envir = .GlobalEnv) 
}

#Function to stream content incrementally
go <- function(){
  
  doc1 <- c("Compatibility of systems of linear constraints over the set of natural numbers. 
         Criteria of compatibility of a system of linear Diophantine equations, strict inequations, 
         and nonstrict inequations are considered.")
  doc2 <- c("Upper bounds for components of a minimal set of solutions and algorithms of construction of 
         minimal generating sets of solutions for all types of systems are given. 
         These criteria and the corresponding algorithms for constructing a minimal supporting set of solutions 
         can be used in solving all the considered  types systems and systems of mixed types.")
  doc3 <- c("Upper bounds provide a solution for expected values of the algorithm. These mixed types allow 
            us to provide a proof of concept with the criteria, and solve the problem of optimization of the
            solutions for the inequations.")
  
  
  
  #Open Text Stream
  #stream.doc <- c(doc1,doc2, doc3)
  
  time.main2 <- c()
  print(Sys.time())
  
  load("RD_Data_2013_Confs_Month.RData")
  
  
  for(iter in 1:length(stream.doc)){
    #browser()
    #stream <- paste(stream.doc[1:iter], collapse = " ")
    stream <- stream.doc[iter]
    time.main.value <-  microbenchmark(main(stream, iter),list = NULL, times = 1, check = NULL,control = list())$time * 10^-9
    time.main2 <- c(time.main2, time.main.value)
    assign(paste("nnodes.iter.",iter,sep=""), number.of.nodes)
    assign(paste("nwords.iter.",iter,sep=""), number.of.words)
  }
  
  print(Sys.time())
  
  names(time.main2) <- paste("iter", 1:length(stream.doc),sep=" ")
  time.main2 <<- time.main2
  save(list=c("time.main2",paste("iter.",1:12,".key.list",sep=""),paste("nnodes.iter.",1:12,sep=""),paste("nwords.iter.",1:12,sep="")),file="IncTextRankResults-RD-12-2.RData")
  #save(list=c("time.main3",paste("iter.",1:15,".key.list",sep="")),file="IncTextRankResults3.RData")
  
}
