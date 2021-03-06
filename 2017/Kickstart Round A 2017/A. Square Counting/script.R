###############################################################################
###############################################################################
###############################################################################

## I am loading input data ----------------------------------------------------

my_input <- file.choose()
my_data <- readLines(my_input)


## ----------------------------------------------------------------------------

###############################################################################

## I am changing a working directory ------------------------------------------

setwd(
    gsub("(.*\\\\)(.*)", "\\1", my_input)
)


## ----------------------------------------------------------------------------

###############################################################################

## helper functions -----------------------------------------------------------

solveTheCase <- function(
    
    my_case,
    modulus = 1e9 + 7
    
){
    
    # '''
    # Returns a solution for one case of the input data.
    # '''
    
    R <- as.integer(strsplit(my_case, split = " ")[[1]][1])
    C <- as.integer(strsplit(my_case, split = " ")[[1]][2])
    
    M <- max(R, C)
    N <- min(R, C)
    
    ## We have a matrix made up of $R \times C$ dots. Let M be maximum
    ## of (R, C) and N let be minimum of (R, C). We can prove that
    ## number of squares that can be found by joining any 4 dots in
    ## the matrix is given by formula
    ##
    ## $$
    ##   \sum_{i = 1}^{N - 1} (N - i) \cdot (M - i) \cdot i = \\
    ##   \sum_{i = 1}^{N - 1} N \cdot M \cdot i - (N + M) \cdot i ^ 2 + i ^ 3
    ## $$,
    ##
    ## where $(N − i) \cdot (M − i)$ counts all squares of sidelength $i$
    ## and sides parallel to the rows and columns and multiplies it by
    ## the amount of squares we get when we slide their corners along
    ## those sides $(\cdot i)$.
    
    ## addend $ \sum_{i = 1}^{N - 1} N \cdot M \cdot i $ ----------------------
    
    part_1 <- N * (N + 1) / 2 * (N * M)
    
    
    ## addend $ \sum_{i = 1}^{N - 1} (N + M) \cdot \cdot i ^ 2 $ --------------
    
    part_2 <- (N + 1) * N * (2 * N + 1) / 6 * (N + M)
    
    
    ## addend $ \sum_{i = 1}^{N - 1} i ^ 3 $ ----------------------------------
    
    part_3 <- (N * (N + 1) / 2) ^ 2   
    
    
    return((part_1 - part_2 + part_3) %% modulus)
            
}


## ----------------------------------------------------------------------------

###############################################################################

## flow control ---------------------------------------------------------------

if(
    as.integer(my_data[1]) != length(my_data) - 1
){
    stop("Number of cases is fishy.", call. = FALSE)
}


## ----------------------------------------------------------------------------

###############################################################################

## core computations ----------------------------------------------------------

my_output <- NULL

for(i in 1:as.integer(my_data[1])){

    my_case <- my_data[i + 1]

    my_output <- c(
        my_output,
        paste(
            "Case #",
            i,
            ": ",
            solveTheCase(my_case),
            sep = ""
        )
    )
    
    flush.console()
    print(
        paste(
            "Process is ",
            format(
                round(
                    i / as.integer(my_data[1]) * 100,
                    digits = 2
                ),
                nsmall = 2
            ),
            " % complete.",
            sep = ""            
        )
    )
    
}


## ----------------------------------------------------------------------------

###############################################################################

## I am giving an output ------------------------------------------------------

writeLines(
    my_output,
    con = gsub(
        ".in",
        "-out.in",
        gsub("(.*\\\\)(.*)", "\\2", my_input)
    )
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





