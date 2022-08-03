! Copyright (C) 2022 Ibzan Hyena
! See http://factorcode.org/license.txt for BSD license.
USING:
  accessors
  arrays
  assocs
  io io.encodings.utf8 io.files
  kernel
  lists
  math math.functions math.order math.parser
  namespaces
  sequences
  splitting
  ;
IN: advent-of-code-2019.intcode
SYMBOL: instructions

: boolean>fixnum ( ? -- n ) [ 1 ] [ 0 ] if ;

TUPLE: program
  { pc integer initial: 0 }
  { buffer array }
  { input integer initial: 0 }
  { output integer initial: 0 }
  ;

: create ( str -- program )
  "," split [ string>number ] map ! buffer
  program new                     ! buffer program
  dup [ buffer<< ] dip            ! program
  ;

: antibase ( bases num -- digits )
  [ sequence>list ] dip
  1list
  [                   ! acc elt
    swap              ! elt acc
    uncons            ! elt car cdr
    [ swap /mod ] dip ! div rem cdr
    cons cons         ! acc
  ]
  foldr
  cdr
  list>array
  ;

TUPLE: opcode
  { modes array } 
  { code integer }
  ;

C: <opcode> opcode

: parse-opcode ( arr -- opcode )
  { 10 10 10 100 } swap antibase ! arr
  dup but-last                   ! arr modes
  [ 1 = ] map reverse            ! arr modes
  swap last                      ! modes code
  <opcode>
  ;

: fetch ( program  -- program op ) dup [ pc>> ] [ buffer>> ] bi nth parse-opcode ;

: decode ( op -- op quot ) dup code>> instructions get-global at* [ "unknown opcode" throw ] unless ;

: execute ( program op quot -- program ) call( program op -- program ) ;

:: get-position ( index buffer -- value ) index buffer nth buffer nth ;
: get-immediate ( index buffer -- value ) nth ;
: get ( index buffer mode -- value ) [ get-immediate ] [ get-position ] if ;

:: increment ( program n -- program ) program program pc>> n + >>pc ;
: jump ( program n -- program ) >>pc ;

:: param ( program op n -- value )
  program pc>>     :> pc
  program buffer>> :> buffer

  pc n +               ! index
  buffer               ! index buffer
  n 1 - op modes>> nth ! index buffer mode
  get
  ;

:: op-arithmetic ( program op quot -- program )
  program pc>>     :> pc
  program buffer>> :> buffer

  program op 1 param  ! val1
  program op 2 param  ! val2
  quot call           ! result
  pc 3 + buffer nth   ! result destination
  buffer set-nth      !
  program 4 increment ! program
  ; inline

: op-add ( program op -- program ) [ + ] op-arithmetic ;

: op-mul ( program op -- program ) [ * ] op-arithmetic ;

:: op-input ( program op -- program )
  program pc>>   :> pc
  program buffer>> :> buffer

  program input>> ! input
  pc 1 +          ! input n
  buffer          ! input n buffer
  nth             ! input index
  buffer          ! input index buffer
  set-nth         !

  program 2 increment
  ;

:: op-output ( program op -- program ) 
  program            ! program
  program op 1 param ! program value
  >>output           ! program

  2 increment
  ;

:: op-jump-cond ( program op quot -- program )
  program              ! program
  program op 1 param   ! program val
  quot call            ! program jump?
  [ program op 2 param jump ]
  [ 3 increment ]
  if
  ; inline

: op-jump-true ( program op -- program ) [ zero? not ] op-jump-cond ;

: op-jump-false ( program op -- program ) [ zero? ] op-jump-cond ;

: op-lt ( program op -- program ) [ < boolean>fixnum ] op-arithmetic ;

: op-eq ( program op -- program ) [ = boolean>fixnum ] op-arithmetic ;

: op-halt ( program op -- program ) drop -1 >>pc ;

H{
  {  1 [ op-add        ] }
  {  2 [ op-mul        ] }
  {  3 [ op-input      ] }
  {  4 [ op-output     ] }
  {  5 [ op-jump-true  ] }
  {  6 [ op-jump-false ] }
  {  7 [ op-lt         ] }
  {  8 [ op-eq         ] }
  { 99 [ op-halt       ] }
} instructions set-global 

: program-from-file ( path -- program )  utf8 file-contents "\n" "" replace create ;

: run-program ( program -- program )
  [ dup pc>> 0 >= ]
  [ fetch decode execute ]
  while
  ;

: run-with-input ( program n -- )
  >>input       ! program
  run-program   ! program
  output>>      ! output
  number>string ! str
  print         !
  ;


