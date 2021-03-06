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
    
    my_case
    
){
    
    # '''
    # Returns a solution for one case of the input data.
    # '''
    
    N <- as.integer(strsplit(my_case, split = " ")[[1]][1])
    M <- as.integer(strsplit(my_case, split = " ")[[1]][2])
    
    ## We can prove that number of possible ways A stays in the lead
    ## the entire time is the $(N - 1, M)$-th term of Catalan's triangle.
    ## Let $c_{N - 1, M}$ be the $(N - 1, M)$-th term of Catalan's triangle.
    ## Then $c_{N - 1, M} = \frac{(N + M - 1)! \cdot (N - M)}{N! \cdot M!}$.
    ##
    ## The number of all the ways the election could go is equal to
    ## ${{N + M}\choose{N}} = \frac{(N + M)!}{N! \cdot M!}$.
    ##
    ## The probability that A will always be winning after every vote is
    ## a result of division of the two previous numbers, so that
    ## $\frac{c_{N - 1, M}}{{{N + M}\choose{N}}} = \frac{N - M}{N + M}$.
    
    return((N - M) / (N + M))
            
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
            format(round(
                solveTheCase(my_case),
                digits = 8), nsmall = 8),
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






