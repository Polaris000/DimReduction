data(iris)

y <- iris[, 5]
mypca <- princomp(iris[, -5], cor=TRUE, score=TRUE)

print(summary(mypca))

model__ <- mypca$scores

model__ = data.frame(mypca$scores)

# selecting best 2 components
model__ = model__[, c(1, 2)]
model__$Species = y

# plotting our data
plot(model__[1:2], col=model__$Species)

# training svm
model_ <- svm(Species ~., data=model__)
print(model)

# plotting svm...
# plot(model_, model__)
