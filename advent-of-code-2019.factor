! Copyright (C) 2022 Ibzan Hyena
! See http://factorcode.org/license.txt for BSD license.
USING:
  assocs
  io io.encodings.utf8 io.files
  kernel
  math math.functions math.order math.parser
  namespaces
  sequences
  splitting
  ;
IN: advent-of-code-2019
SYMBOL: instructions

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

: aoc01a ( path -- )
  [ 3 / floor 2 - ] aoc01g
  ;

: aoc01b ( path -- )
  [ aoc01bfuel ] aoc01g
  ;

: create ( str -- 0 buffer ) "," split [ string>number ] map 0 swap ;

: fetch ( pc buffer -- pc buffer op ) 2dup nth ;

: decode ( op -- quot ) instructions get-global at* [ "unknown opcode" throw ] unless ;

: execute ( pc buffer quot -- pc buffer ) call( pc buffer -- pc buffer ) ;

:: op-arithmetic ( pc buffer quot -- pc buffer )
  pc 1 + buffer nth buffer nth
  pc 2 + buffer nth buffer nth
  quot call
  pc 3 + buffer nth
  buffer set-nth
  pc 4 + buffer
  ; inline

: op-add ( pc buffer -- pc buffer ) [ + ] op-arithmetic ;

: op-mul ( pc buffer -- pc buffer ) [ * ] op-arithmetic ;

: op-halt ( pc buffer -- -1 buffer ) [ drop -1 ] dip ;

H{
  {  1 [ op-add  ] }
  {  2 [ op-mul  ] }
  { 99 [ op-halt ] }
} instructions set-global 

: run-program ( pc buffer -- buffer )
  [ [ dup 0 >= ] dip swap ]
  [ fetch decode execute ]
  while
  [ drop ] dip
  ;

: aoc02a ( path -- )
  utf8 file-contents create
  dup 12 1 rot set-nth
  dup 2 2 rot set-nth
  run-program
  0 swap nth
  number>string
  print
  ;



