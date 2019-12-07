#########################################
############Create datasets##############
#########################################

#the observed data A77
data_raw_matrix = matrix(c(66, 87, 25, 22, 4,
                           13, 14, 15, 9, 4,
                           0, 4, 4, 9, 1,
                           0, 0, 4, 3, 1,
                           0, 0, 0, 1, 1,
                           0, 0, 0, 0, 0), ncol = 5, byrow = TRUE)
data_A77 = prop.table(data_raw_matrix,2)

#the observed data A80
data_raw_matrix = matrix(c(44, 62, 47, 38, 9,
                          10, 13, 8, 11, 5,
                           0, 9, 2, 7, 3,
                           0, 0, 3, 5, 1,
                           0, 0, 0, 1, 0,
                           0, 0, 0, 0, 1), ncol = 5, byrow = TRUE)
data_A80 = prop.table(data_raw_matrix,2)

#the observed data ASE
data_raw_matrix = matrix(c(15, 12, 4,
                          11, 17, 4,
                           0, 21, 4,
                           0, 0, 5), ncol = 3, byrow = TRUE)
data_ASE = prop.table(data_raw_matrix,2)

#the observed data BSE
data_raw_matrix = matrix(c(9, 12, 18, 9, 4,
                          1, 6, 6, 4, 3,
                           0, 2, 3, 4, 0,
                           0, 0, 1, 3, 2,
                           0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0), ncol = 5, byrow = TRUE)
data_BSE = prop.table(data_raw_matrix,2)


#########################################
###########Create ABC code###############
#########################################

#the data generating function
generate_probabilities = function(theta, n_col_data = 5)
{
	q_c = theta[1]
	q_h = theta[2]
	w_0 = sapply(seq(1, n_col_data, by = 1), function(x){return(`^`(q_c,x))}) #w_0s components (1st row components)
	prob_matrix = matrix(rep(0, n_col_data^2), nrow = n_col_data)
	
	for (i in 1 : (n_col_data - 1)){
		prob_matrix[i, i] = 1 - w_0[i] - sum(prob_matrix[1:i, i]) #w_jj components (diagonal components)
		for (j in (i+1):n_col_data)
		{
			prob_matrix[i, j] = choose(j,i) * prob_matrix[i,i] * (q_c * q_h^i)^(j-i) #w_js components
		}
	}
	prob_matrix[n_col_data, n_col_data] = 1 - w_0[n_col_data] - sum(prob_matrix[1:n_col_data, n_col_data]) #last elements of the matrix
	probabilities = rbind(w_0, prob_matrix) #W matrix (data matrix)
	return(probabilities)
}



#generate the first abc sample
generate_abc_sample_1 = function(observed_data, data_generating_function, epsilon)
{
	while(TRUE)
	{
		q_c_h = runif(2) #q_c and q_h
		generated_data = data_generating_function(theta = q_c_h)
		if(sum((generated_data - observed_data)^2) < epsilon^2) #Squared Euclidean distance
			return(q_c_h)
	}
}

#generate the sequential abc sample
generate_abc_sample_2 = function(observed_data, population, data_generating_function, epsilon)
{
	while(TRUE)
	{
	  N_sample = dim(population)[2]
		sample_index = floor(runif(1, min = 1, max = N_sample + 0.999))
		q_c_h = population[ , sample_index] #population = previous posterior_sample
		q_c_h[1] = rnorm(1, mean = q_c_h[1], sd = 0.05) #perturbation kernel using Gaussian dist
		q_c_h[2] = rnorm(1, mean = q_c_h[2], sd = 0.05) #perturbation kernel using Gaussian dist
		generated_data = data_generating_function(theta = q_c_h)
		if(sum((generated_data - observed_data)^2) < epsilon^2) #Squared Euclidean distance
			return(q_c_h)
	}
}


#the main function
generate_abc_smc_sample = function(dataset)
{
	generate_prob = function(theta){generate_probabilities(theta, n_col_data = ncol(dataset))}
	epsilon_vector = seq(1, 0.35, by = -0.05) #different epsilons
	n_sequential = length(epsilon_vector)
	N_sample = 1000
	for (i in 1:n_sequential)
	{
		if (i == 1)
		{
			posterior_sample = replicate(generate_abc_sample_1(observed_data = dataset,
															   data_generating_function = generate_prob,
															   epsilon = epsilon_vector[i]), n = N_sample)
		}
		else
		{
			posterior_sample = replicate(generate_abc_sample_2(observed_data = dataset,
										population = posterior_sample,
										data_generating_function = generate_prob,
										epsilon = epsilon_vector[i]), n = N_sample)
		}
		#print(i) #to check the number of iteration to find the best epsilon
	}
	return(posterior_sample)
}



posterior_sample_A77 = generate_abc_smc_sample(data_A77)
posterior_sample_A80 = generate_abc_smc_sample(data_A80)
posterior_sample_ASE = generate_abc_smc_sample(data_ASE)
posterior_sample_BSE = generate_abc_smc_sample(data_BSE)

par(mfrow=c(1,2))
plot(x =posterior_sample_A77[2, ], y = posterior_sample_A77[1, ], type = "p", col = "red", xlim = c(0,1), ylim = c(0,1), xlab = "q_h", ylab = "q_c")
points(x = posterior_sample_A80[2, ], y = posterior_sample_A80[1, ], type = "p", col = "blue", xlim = c(0,1), ylim = c(0,1))

plot(x = posterior_sample_ASE[2, ], y = posterior_sample_ASE[1, ], type = "p", col = "blue", xlim = c(0,1), ylim = c(0,1), xlab = "q_h", ylab = "q_c")
points(x = posterior_sample_BSE[2, ], y = posterior_sample_BSE[1, ], type = "p", col = "red", xlim = c(0,1), ylim = c(0,1))
par(mfrow=c(1,1))




