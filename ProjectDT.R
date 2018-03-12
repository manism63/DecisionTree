library(data.tree)
mydata = read.table("mushrooms.csv", header=TRUE, sep=",")

calEntropy = function(dataset){
  posits = sum(dataset[,"class"] == "e")
  negs = sum(dataset[,"class"] == "p")
  t = posits + negs
  if (posits == 0){
    entropy = (-(negs/t)*log2(negs/t))
  }
  else if (negs == 0){
    entropy = (-(posits/t)*log2(posits/t))
  }
  else{
    entropy = (-(posits/t)*log2(posits/t)) + (-(negs/t)*log2(negs/t))
  }
  #entropy = (-(posits/t)*log2(posits/t)) + (-(negs/t)*log2(negs/t))
  return (entropy)
}

calcGain = function(col_name, dataset){
  ent = calEntropy(dataset)
  #print("parent Entropy")
  #print(ent)
  #cat = levels(as.factor(dataset[,col_name]))
  cat = as.character(unique(dataset[,col_name]))
  #print(length(cat))
  children_entropy = 0
  for(i in 1 : length(cat)){
    L = dataset[,col_name] == cat[i]
    #print(L)
    cDataSet = dataset[L,]
    cDataSet = subset(cDataSet, select = -eval(parse(text = col_name)))
    x = (nrow(cDataSet)/nrow(dataset))*calEntropy(cDataSet)
    #print(x)
    children_entropy = children_entropy +  x #(nrow(cDataSet)/nrow(dataset))*calEntropy(cDataSet)
    #stopp
  }
  #print(children_entropy)
  gain = ent - children_entropy
  # print(gain)
  # print(ent)
  return (gain)
}

selectCategory = function(dataset){
  #catName = ''
  cols = colnames(dataset)[2:ncol(dataset)]#, select = -class)
  gain = -99
  c = ncol(dataset)-1
  for(i in 1 : c){
    # print('---------------------')
    # print(cols[i])
    tempGain = calcGain(cols[i], dataset)
    # print(tempGain)
    # print('---------------------')
    if(gain < tempGain){
      catName = cols[i]
      gain = tempGain
    }
    #gain = max(gain, calcGain(colnames(dataset)[i], dataset))
  }
  return(catName)
}

constructTree = function(parentNode, parent_cat_val, col_name, dataset){
  L = dataset[,col_name] == parent_cat_val
  
  if(length(unique(dataset[L,'class'])) == 1){
    nodename = paste(parent_cat_val, ':y=', as.character(unique(dataset[L,'class'])), sep = '') #pure split will give the asnwer as y:poisonous or edible
    parentNode$AddChild(nodename)
  }else{
    newDataSet = subset(dataset[L,], select = -eval(parse(text = col_name)))
    select_col = selectCategory(newDataSet);
    #print(select_col)
    nodename = paste(parent_cat_val, select_col, sep=':')
    child = parentNode$AddChild(nodename)
    #catLevels = levels(as.factor(newDataSet[,select_col]))
    catLevels = as.character(unique(newDataSet[,select_col]))
    for(i in 1 : length(catLevels)){
      constructTree(child, catLevels[i], select_col, newDataSet);
    }
  }
}
 
smp_size = floor(0.8 * nrow(mydata))
train_ind = sample(seq_len(nrow(mydata)), size = smp_size)
traindata = mydata[train_ind,]
testinds = sample(seq_len(nrow(mydata)), size = nrow(mydata) - nrow(traindata) + 10)
testdata = mydata[testinds,]
colnames(traindata) = colnames(mydata)
colnames(testdata) = colnames(mydata)

col_name = selectCategory(traindata);
dt <- Node$new(col_name)
catLevels = as.character(unique(traindata[,col_name]))
#catLevels = c('n')
for(i in 1:length(catLevels)){
  constructTree(dt, catLevels[i], col_name, traindata)
}
#print(dt)



answers = c()
classname = function(query, dtree){
  children = dtree$children
  for(i in 1:length(children)){
    child_lvl_name = children[[i]]$levelName
    # print ("child lvl name"); print(child_lvl_name)
    child_lvl_name = sub('^\\s+.*--', '', child_lvl_name, perl = TRUE)
    # print(child_lvl_name)
    cname = ""
    if(dtree$isRoot == TRUE){
      # print("this is root")
      cname = dtree$levelName;
    }else{
      # print("I ama not root")
      cname = unlist(strsplit(dtree$levelName, ":"))[2]
    }
    
    cname_value = as.character(query[1,cname])
    #cname_value = paste(cname_value, ':', sep='')
    # print(child_lvl_name)
    # print(cname_value)
    #grepVal = grep(cname_value, c(child_lvl_name), perl=TRUE, value=FALSE)
    grepVal = unlist(strsplit(child_lvl_name, ":"))[1]
    # print(grepVal)
    #cname2 = sub('^\\s+.*--', '', child_lvl_name, perl = TRUE)
    #if(grep(cname_value, child_lvl_name, perl=TRUE, value=TRUE) == child_lvl_name){
    if(grepVal == cname_value){
      # print ("got it")
      if(children[[i]]$isLeaf == TRUE){
        # print ("this is leaf")
        class = sub('.*=', '', child_lvl_name, perl= TRUE)
        return(class)
      }
      else{
        # print("Not a leaf")
        # print(i)
        return(classname(query, children[[i]]))
      }
    }
    else{
      # print ("continue")
      #continue
      next;
    }
  }
}
# colnames(query) = colnames(testdata)
# query = mydata[2000,]
# class = classname(query,dt)

test = function(testdata, dt){
  for (i in 1:nrow(testdata)){
    query = testdata[i,]
    colnames(query) = colnames(testdata)
    answers = c(answers, classname(query, dt))
  }
  return(answers)
}

answers = c()
answers = test(testdata, dt)
accuracy = sum(answers == testdata[,'class'])*100/nrow(testdata)
accuracy

############################################################################################################


mush = read.table("mushrooms.csv", header=TRUE, sep=",")
samp_size = floor(0.8 * nrow(mush))
tr_inds = sample(seq_len(nrow(mush)), size = samp_size)
mushtrain = mush[tr_inds,]
test_inds = sample(seq_len(nrow(mush)), size = nrow(mush) - nrow(mushtrain))
mushtest = mush[test_inds,]

mushtest = as.data.frame(mushtest)
mushtest[,"class"] = NA
colnames(mushtest) = colnames(mushtrain)
p = mushtrain[which(mushtrain[,'class'] == 'p'),]
e = mushtrain[which(mushtrain[,'class'] == 'e'),]
p = as.data.frame(p)
e = as.data.frame(e)

prior = list(p = nrow(p)/nrow(mushtrain), e = nrow(e)/nrow(mushtrain))
probs = list(p = prior[['p']], e = prior[['e']])
mushtest = data.frame(lapply(mushtest, as.character), stringsAsFactors=FALSE)

for (i in 1:nrow(mushtest)){
  point = mushtest[i,]
  #print(point)
  #print(typeof(point))
  
  probs = list(p = prior[['p']], e = prior[['e']])
  for (j in 2:(ncol(mushtest))){
    
    probs[['p']] = probs[['p']] * sum(p[,j] == point[1,j])/nrow(p)
    probs[['e']] = probs[['e']] * sum(e[,j] == point[1,j])/nrow(e)
    
  }
  if (probs[['p']] > probs[['e']]){
    mushtest[i,'class'] = 'p'
  }
  else{
    mushtest[i,'class'] = 'e'
  }
}

#print(mushtest)
accuracy2 = sum(mushtest[,'class'] == mush[test_inds,'class'])/nrow(mushtest) * 100
#print(accuracy2)

print(accuracy)
print(accuracy2)
#plotter = c(plotter, accuracy2 - accuracy)
#print(dt) to see the decision tree
