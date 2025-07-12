# Function to compute the length of the longest common substring of two strings
longestCommonSubstringLengthCalculator <- function(str_1, str_2){
  m_lcs <- matrix(0L, nrow = nchar(str_1), ncol = nchar(str_2))
  for (c1 in 1:nchar(str_1)){
    for (c2 in 1:nchar(str_2)){
      if (substr(str_1, c1, c1) == substr(str_2, c2, c2)){
        m_lcs[c1, c2] <- 1
      }  
    }
  }
  
  #Determine longest substring by iterating over diagonals
  longest <- 0
  for (r in 1:nrow(m_lcs)){
    for (c in 1:ncol(m_lcs)){
      if (m_lcs[r,c] == 1){
        curr_len <- 1
        curr_r <- r
        curr_c <- c
        while((curr_r+1 <= nrow(m_lcs) && curr_c+1 <= ncol(m_lcs)) && m_lcs[curr_r+1, curr_c+1] == 1){
          curr_len <- curr_len + 1
          curr_r <- curr_r + 1
          curr_c <- curr_c + 1
        }
        if (curr_len > longest){
          longest <- curr_len
        }
      }  
    }
  }
  return(longest)
}

#Function to turn hex into rgb string
hexToRbgStringConverter <- function(hex){
  str <- paste0("rgb(",paste0(as.vector(col2rgb(hex)), collapse = ","), ")")
  return(str)
}
hexToRbgaStringConverter <- function(hex, a = 0.5){
  str <- paste0("rgba(",paste0(as.vector(col2rgb(hex)), collapse = ","),",",a,")")
  return(str)
}
