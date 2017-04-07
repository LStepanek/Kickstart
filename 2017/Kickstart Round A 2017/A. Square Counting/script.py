###############################################################################
###############################################################################
###############################################################################

## I am loading packages ------------------------------------------------------

import os


## ----------------------------------------------------------------------------

###############################################################################

## I am changing a working directory ------------------------------------------

os.chdir("U:/Kickstart/2017/Kickstart Round A 2017/A. Square Counting")


## ----------------------------------------------------------------------------

###############################################################################

## I am loading input data ----------------------------------------------------

file_name = "A-large-practice.in"    ## or "A-small-practice.in"

file_pointer = open(file_name, "r")
my_data = file_pointer.readlines()
file_pointer.close()


## ----------------------------------------------------------------------------

###############################################################################

## helper functions -----------------------------------------------------------

def solveTheCase(my_case, modulus):
    
    '''
    Returns a solution for one case of the input data.
    '''
    
    R = int(my_case.strip("\n").split(" ")[0])
    C = int(my_case.strip("\n").split(" ")[1])
    
    M = max(R, C)
    N = min(R, C) 
    
    
    ## addend  sum_{i = 1}^{N - 1} N cdot M cdot i ----------------------------
    
    part_1 = N * (N + 1) / 2 * (N * M)
    
    
    ## addend sum_{i = 1}^{N - 1} (N + M) cdot i ^ 2 --------------------------
  
    part_2 = (N + 1) * N * (2 * N + 1) / 6 * (N + M)
    
    
    ## addend sum_{i = 1}^{N - 1} i ^ 3 ---------------------------------------
    
    part_3 = (N * (N + 1) / 2) ** 2   
    
    
    return((part_1 - part_2 + part_3) % modulus)


## ----------------------------------------------------------------------------

###############################################################################

## core computations ----------------------------------------------------------

my_output = []

for i in range(1, len(my_data)):

    my_case = my_data[i]
    
    my_row = ("Case #" + str(i) + ": " +
              str(int(solveTheCase(my_case, modulus = (10 ** 9 + 7)))) +
              "\n")

    my_output.append(my_row)


## ----------------------------------------------------------------------------

###############################################################################

## I am giving an output ------------------------------------------------------

file_name = "A-large-practice-out.in"    ## or "A-small-practice.in"

file_pointer = open(file_name, "w")
file_pointer.writelines(my_output)
file_pointer.close()


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################




