! Copyright (C) 2022 Ibzan Hyena
! See http://factorcode.org/license.txt for BSD license.
USING:
  accessors
  advent-of-code-2019.intcode
  arrays
  io io.encodings.utf8 io.files
  kernel
  lists
  math math.combinatorics math.functions math.order math.parser
  sequences sequences.deep
  vectors
  ;
IN: advent-of-code-2019

<PRIVATE

: read ( path -- lines ) utf8 file-lines ;


: aoc01g ( path quot -- )
  [ read ] dip                     ! lines quot
  '[ string>number _ [ 0 ] if* + ] ! lines quot
  0                                ! lines quot 0
  swap                             ! lines 0 quot
  reduce                           ! sum
  number>string                    ! str
  print
  ; inline

: aoc01bfuel ( weight -- fuel )
  0 swap
  [ dup 0 > ]
  [ 3 / floor 2 - 0 max dup [ + ] dip ]
  while
  +
  ;

PRIVATE>

: aoc01a ( path -- ) [ 3 / floor 2 - ] aoc01g ;

: aoc01b ( path -- ) [ aoc01bfuel ] aoc01g ;

: aoc02a ( path -- )
  program-from-file    ! program
  dup buffer>>         ! program buffer
  dup 12 1 rot set-nth ! program buffer
  2 2 rot set-nth      ! program
  run-program          ! program
  buffer>>             ! buffer
  0 swap nth           ! value
  number>string
  print
  ;

: aoc05a ( path -- ) program-from-file 1 run-with-input ;

: aoc05b ( path -- ) program-from-file 5 run-with-input ;

:: aoc07a ( path -- )
  path program-from-file :> prog
  [ prog [ clone ] deep-map ] :> new-prog
  { 0 1 2 3 4 } all-permutations
  [
    >list ! permutation
    0     ! permutation 0
    [                    ! prev elt
      2array >vector     ! inputs
      new-prog call swap ! prog inputs
      >>input            ! prog
      run-program        ! prog
      output>>           ! vec
      last               ! next
    ]     ! permutation 0 quot
    foldl ! result
  ]
  map supremum number>string print
  ;
