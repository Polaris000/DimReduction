# print(combn(letters[1:4], 2))
print(combn(1:10, 2))  # minimum value in each combination
# Different way of encoding points:
# print(combn(c(1,1,1,1,2,2,2,3,3,4), 3, tabulate, nbins = 4))
#Compute support points and (scaled) probabilities for a
#Multivariate-Hypergeometric(n = 3, N = c(4,3,2,1)) p.f.:
# table.mat(t(combn(c(1,1,1,1,2,2,2,3,3,4), 3, tabulate,nbins=4)))