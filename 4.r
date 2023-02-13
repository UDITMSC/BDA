library ( 'arules' )
library( 'arulesViz' )
data("Groceries")
View(Groceries)
summary(Groceries)
class(Groceries)
Groceries@itemInfo[1:20,]
Groceries@itemsetInfo[1:20,]
g=Groceries@itemsetInfo[1:20,]
apply(Groceries@data[,1:30],2,
 function(r) paste(Groceries@itemInfo[r,"labels"], collapse = ", "))
# Generating frequent itemset of size 1
itemsets = apriori(Groceries)
itemsets = apriori(Groceries, parameter =
 list(minlen=1,
 maxlen=1,
 support=0.02,
 target="frequent itemsets"))
inspect(itemsets)
summary(itemsets)
# The top 10 most frequent 1-itemsets
inspect(head(sort(itemsets, by = "support"),10))
# Generating frequent itemset of size 2
itemsets = apriori(Groceries, parameter =
list(minlen=2,
     maxlen=2,
     support=0.02,
     target="frequent itemsets"))
inspect(itemsets)
summary(itemsets)
# The top 10 most frequent 2-itemsets
inspect(head(sort(itemsets, by = "support"),10))
# Generating frequent itemset of size 3
itemsets = apriori(Groceries, parameter =
 list(minlen=3,
 maxlen=3,
 support=0.02,
 target="frequent itemsets"))
inspect(itemsets)
summary(itemsets)
# The top 10 most frequent 3-itemsets
inspect(head(sort(itemsets, by = "support"),10))
# Generating frequent itemset of size 4
itemsets = apriori(Groceries, parameter =
 list(minlen=4,
 maxlen=4,
 support=0.02,
 target="frequent itemsets"))
inspect(itemsets)
summary(itemsets)
#Rule Generation
rules = apriori(Groceries,
 parameter=list(support=0.001,
 confidence=0.6,
 target="rules"))
inspect(rules)
summary(rules)
inspect(head(sort(rules, by = "support"),10))
# Rule Visualization
plot(rules)
plot(rules@quality)

inspect(head(sort(rules, by = "lift"),10))
confidentRules = rules[quality(rules)$confidence>0.9]
confidentRules
###
plot(confidentRules,
 method="matrix",
 measure = c("lift", "confidence"),
 control=list(reorder=TRUE)
  )
highLiftRules = head(sort (rules, by= "lift " ) , 5)
highLiftRules = head(sort(rules, by = "lift"))
highLiftRules
plot(highLiftRules, method = "graph", control = list(type="items"))
