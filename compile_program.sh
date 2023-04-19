#!/bin/bash

gfortran -o runner program.f90 -O3 -std=f2008 -Wall -Wextra -pedantic-errors
