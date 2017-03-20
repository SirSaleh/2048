#	2 
#	0
#	4
#	8
#   Sides: c(1,0) for right, (-1,0) for left, (0,1) for top, (0,-1) for down 


init_2014 <- function (dimention){
	a <- matrix (rep(0,(dimention^2)),nrow=dimention)
	first2s = sample (c(1:(dimention^2)),2)
	a[first2s[1]] = 2 ; a[first2s[2]] = 2;
	return (a)
}


clear <- function() cat(c("\033[2J","\033[0;0H"))


show_hint <- function(current_matrix){
	print ("the current matrix is:")
	print (current_matrix)
	cat ("\t 8^ \n 4< \t\t 6> \n \t 2v\n");
}
is_full <- function (current_matrix){
	if (length(current_matrix[current_matrix!=0]) == 0){
		return (TRUE)
	}
	return	(FALSE)
}

can_save <- function(current_matrix){
	#just for full square
	for (i in c(1:dim(current_matrix)[1])){
		for (j in c(1:(dim(current_matrix)[2]-1))){
			if (i!=dim(current_matrix)[1]){
				if (current_matrix[i,j]==current_matrix[i,j+1] || current_matrix[i,j]==current_matrix[i+1,j]){
					return (TRUE)
				}
			}else{
				if (current_matrix[i,j]==current_matrix[i,j+1]){
					return (TRUE)
				}
			}
		}
	}
	return (FALSE)
}
get_random_empty <- function(current_matrix){
	l = length(current_matrix[current_matrix==0])
	if (l>0){
		indices = which (current_matrix==0)
		random_order = sample (l,1)
		return (indices[random_order])
	}
	return (NULL)
}
generate_new_tile_value <- function(){
	if (rbinom(1,1,0.1)==1){
		return (4)
	}
	return (2)
}
set_new_tile_value <- function(current_matrix){
	if (length(current_matrix[current_matrix==0]!=0)){
		#so there room for new value
		current_matrix[get_random_empty(current_matrix)] = generate_new_tile_value()
	}
	return (current_matrix)
}
clean_row_col <- function(current_matrix,rows,side){
	d = dim(current_matrix)[2]
	if (side == "left"){
		for (row in rows){
			for (j in c(1:d)){
				if (current_matrix[row,j] == 0){
					for(i in c(j:d)){
						if (current_matrix[row,i]!=0 && i!=j){
							current_matrix[row,j] = current_matrix[row,i]
							current_matrix[row,i] = 0
							break;
						}
					}
				}
			}
		}
	}else if(side == "right"){
		for (row in rows){
			for (j in c(d:1)){
				if (current_matrix[row,j] == 0){
					for(i in c(j:1)){
						if (current_matrix[row,i]!=0 && i!=j){
							current_matrix[row,j] = current_matrix[row,i]
							current_matrix[row,i] = 0
							break;
						}
					}
				}
			}
		}
	}else if (side == "top"){
		for (row in rows){
			for (j in c(1:d)){
				if (current_matrix[j,row] == 0){
					for(i in c(j:d)){
						if (current_matrix[i,row]!=0 && i!=j){
							current_matrix[j,row] = current_matrix[i,row]
							current_matrix[i,row] = 0
							break;
						}
					}
				}
			}
		}
	}else{
		for (row in rows){
			for (j in c(d:1)){
				if (current_matrix[j,row] == 0){
					for(i in c(j:1)){
						if (current_matrix[i,row]!=0 && i!=j){
							current_matrix[j,row] = current_matrix[i,row]
							current_matrix[i,row] = 0
							break;
						}
					}
				}
			}
		}
	}

	return (current_matrix)
}
# join neighbour same numbers in each action

same_joiner <- function(current_matrix,side){
	d = dim(current_matrix)[2]
	if (side == "left"){
		for (row_col in c(1:4)){
			for (j in c(1:(d-1))){
				if (current_matrix[row_col,j] == 0 || (current_matrix[row_col,j] != 0 && current_matrix[row_col,j]!= current_matrix[row_col,j+1])){
					next
				}else{
					current_matrix[row_col,j] = current_matrix[row_col,j]*2
					current_matrix[row_col,j+1] = 0
				}
			}
		}
	}else if(side == "right"){
		for (row_col in c(1:4)){
			for (j in c(d:2)){
				if (current_matrix[row_col,j] == 0 || (current_matrix[row_col,j] != 0 && current_matrix[row_col,j]!= current_matrix[row_col,j-1])){
					next
				}else{
					current_matrix[row_col,j] = current_matrix[row_col,j]*2
					current_matrix[row_col,j-1] = 0
				}
			}
		}
	}else if (side == "top"){
		for (row_col in c(1:4)){
			for (j in c(1:(d-1))){
				if (current_matrix[j,row_col] == 0 || (current_matrix[j,row_col] != 0 && current_matrix[j,row_col]!= current_matrix[j+1,row_col])){
					next
				}else{
					current_matrix[j,row_col] = current_matrix[j,row_col]*2
					current_matrix[j+1,row_col] = 0
				}
			}
		}
	}else{
		for (row_col in c(1:4)){
			for (j in c(d:2)){
				if (current_matrix[j,row_col] == 0 || (current_matrix[j,row_col] != 0 && current_matrix[j,row_col]!= current_matrix[j-1,row_col])){
					next
				}else{
					current_matrix[j,row_col] = current_matrix[j,row_col]*2
					current_matrix[j-1,row_col] = 0
				}
			}
		}
	}
	return (current_matrix)
}

is_there_possible_join <- function(current_matrix,side){
	posibility = FALSE
	d = dim(current_matrix)[2]
	if (side == "left"){
		for (row_col in c(1:4)){
			for (j in c(1:(d-1))){
				if (current_matrix[row_col,j] == 0 || (current_matrix[row_col,j] != 0 && current_matrix[row_col,j]!= current_matrix[row_col,j+1])){
					next
				}else{
					posibility  = posibility || TRUE
				}
			}
		}
		for (row in c(1:4)){
			for (j in c(1:d)){
				if (current_matrix[row,j] == 0){
					for(i in c(j:d)){
						if (current_matrix[row,i]!=0 && i!=j){
							posibility  = posibility || TRUE
						}
					}
				}
			}
		}
	}else if(side == "right"){
		for (row_col in c(1:4)){
			for (j in c(d:2)){
				if (current_matrix[row_col,j] == 0 || (current_matrix[row_col,j] != 0 && current_matrix[row_col,j]!= current_matrix[row_col,j-1])){
					next
				}else{
					posibility  = posibility || TRUE
				}
			}
		}
		for (row in rows){
			for (j in c(d:1)){
				if (current_matrix[row,j] == 0){
					for(i in c(j:1)){
						if (current_matrix[row,i]!=0 && i!=j){
							posibility  = posibility || TRUE
						}
					}
				}
			}
		}
	}else if (side == "top"){
		for (row_col in c(1:4)){
			for (j in c(1:(d-1))){
				if (current_matrix[j,row_col] == 0 || (current_matrix[j,row_col] != 0 && current_matrix[j,row_col]!= current_matrix[j+1,row_col])){
					next
				}else{
					posibility  = posibility || TRUE
				}
			}
		}
		for (row in c(1:4)){
			for (j in c(1:d)){
				if (current_matrix[j,row] == 0){
					for(i in c(j:d)){
						if (current_matrix[i,row]!=0 && i!=j){
							posibility  = posibility || TRUE
						}
					}
				}
			}
		}
	}else{
		print ("here to check down possible")
		for (row_col in c(1:4)){
			for (j in c(d:2)){
				if (current_matrix[j,row_col] == 0 || (current_matrix[j,row_col] != 0 && current_matrix[j,row_col]!= current_matrix[j-1,row_col])){
					next
				}else{
					posibility  = posibility || TRUE
				}
			}
		}
		for (row in c(1:4)){
			for (j in c(d:1)){
				if (current_matrix[j,row] == 0){
					for(i in c(j:1)){
						if (current_matrix[i,row]!=0 && i!=j){
							posibility  = posibility || TRUE
						}
					}
				}
			}
		}
	}
	return (posibility)
}

nextmove_result <- function (current_matrix,side) {
	if (length(side[side==0]) == 0 || length(side[side!=0]) == 0) {
		stop("Logical Error! Side vector should have a zero and a non-zero member");
	}
	d1 = dim(current_matrix)[1]; d2 = dim(current_matrix)[2]
	
	temp = current_matrix;
	if (all(side == c(-1,0))){
		# TODO: when there is no side move new tile will not create! 
		print ("you press left")
		current_matrix = clean_row_col(current_matrix,c(1:4),"left")
		current_matrix = same_joiner(current_matrix,"left")	
		current_matrix = clean_ro[w_col(current_matrix,c(1:4),"left")
		if (temp!=current_matrix){current_matrix = set_new_tile_value(current_matrix)}

	}else if (all(side == c(1,0))){
		print ("you press right")

		current_matrix = clean_row_col(current_matrix,c(1:4),"right")
		current_matrix = same_joiner(current_matrix,"right")
		current_matrix = clean_row_col(current_matrix,c(1:4),"right")
		if (temp!=current_matrix){current_matrix = set_new_tile_value(current_matrix)}

	}else if (all(side == c(0,1))){
		print ("you press top")
		current_matrix = clean_row_col(current_matrix,c(1:4),"top")
		current_matrix = same_joiner(current_matrix,"top")
		current_matrix = clean_row_col(current_matrix,c(1:4),"top")
		if (temp!=current_matrix){current_matrix = set_new_tile_value(current_matrix)}
	}else {
		print ("you press down")
		current_matrix = clean_row_col(current_matrix,c(1:4),"down")
		current_matrix = same_joiner(current_matrix,"down")
		current_matrix = clean_row_col(current_matrix,c(1:4),"down")
		if (temp!=current_matrix){current_matrix = set_new_tile_value(current_matrix)}	
	}
	return(current_matrix)
	#Check win-or-lose before end
}

play <- function(dimention = 4){
	current_matrix = init_2014(dimention)
	#current_matrix = matrix(c(rep(0,12),2,0,2,2),nrow=4,byrow=FALSE)
	not_ended_yet = TRUE
	while(not_ended_yet){
		#clear()
		show_hint(current_matrix);
		sidecode = list ("4" = c(-1,0), "6" = c(1,0),"8"=c(0,1),"2"=c(0,-1))
		side = NULL
		while (is.null(side)){
			cat("Please enter correct value!")
			side = sidecode[readline()][[1]]
		}
		current_matrix = nextmove_result(current_matrix,side)
		not_ended_yet= !(is_full(current_matrix) && !can_save(current_matrix))
	}
}
play()
