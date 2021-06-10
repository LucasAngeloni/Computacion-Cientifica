G95 module created on Wed Nov 25 11:08:04 2020 from se_nolineales.f90
If you edit this, you'll get what you deserve.
module-version 9
(() () () () () () () () () () () () () () () () () () () () ())

()

()

()

()

(2 'deriv_parciales' 'my_funcs' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE DIMENSION FUNCTION INVOKED) (REAL 8) 0 0 (3 NONE) (2 EXPLICIT
(CONSTANT (INTEGER 4) 0 '1') (CONSTANT (INTEGER 4) 0 '6') (CONSTANT (
INTEGER 4) 0 '1') (CONSTANT (INTEGER 4) 0 '6')) () '' () ())
4 'es_divergente' 'se_nolineales' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE FUNCTION) (LOGICAL 4) 0 0 (5 NONE) () () '' () ())
6 'f' 'my_funcs' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL NONE NONE
DIMENSION FUNCTION INVOKED) (REAL 8) 0 0 (7 NONE 8 NONE) (1 EXPLICIT (
CONSTANT (INTEGER 4) 0 '1') (CONSTANT (INTEGER 4) 0 '6')) () '' () ())
9 'g' 'my_funcs' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL NONE NONE
DIMENSION FUNCTION INVOKED) (REAL 8) 0 0 (10 NONE) (1 EXPLICIT (
CONSTANT (INTEGER 4) 0 '1') (CONSTANT (INTEGER 4) 0 '6')) () '' () ())
11 'g_gauss_seidel' 'my_funcs' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE DIMENSION FUNCTION INVOKED) (REAL 8) 0 0 (12 NONE) (1 EXPLICIT
(CONSTANT (INTEGER 4) 0 '1') (CONSTANT (INTEGER 4) 0 '6')) () '' () ())
13 'ikind1' 'kinds' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN NONE NONE) (
INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '1') () () '' () ())
14 'ikind16' 'kinds' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN NONE NONE) (
INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '-1') () () '' () ())
15 'ikind2' 'kinds' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN NONE NONE) (
INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '2') () () '' () ())
16 'ikind4' 'kinds' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN NONE NONE) (
INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '4') () () '' () ())
17 'ikind8' 'kinds' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN NONE NONE) (
INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '8') () () '' () ())
18 'jacobiano' 'my_funcs' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL NONE
NONE DIMENSION FUNCTION INVOKED) (REAL 8) 0 0 (19 NONE 20 NONE) (2
EXPLICIT (CONSTANT (INTEGER 4) 0 '1') (CONSTANT (INTEGER 4) 0 '6') (
CONSTANT (INTEGER 4) 0 '1') (CONSTANT (INTEGER 4) 0 '6')) () '' () ())
21 'kinds' 'kinds' 1 ((MODULE UNKNOWN UNKNOWN UNKNOWN NONE NONE) (
UNKNOWN) 0 0 () () () '' () ())
22 'matriz_inversa' 'se_nolineales' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (23 NONE 24 NONE) ()
() '' () ())
25 'metodo_gauss_seidel' 'se_nolineales' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (26 NONE 27
NONE 28 NONE) () () '' () ())
29 'metodo_newton_raphson' 'se_nolineales' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (30 NONE 31
NONE 32 NONE) () () '' () ())
33 'metodo_punto_fijo' 'se_nolineales' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (34 NONE 35 NONE 36 NONE) ()
() '' () ())
37 'my_funcs' 'my_funcs' 1 ((MODULE UNKNOWN UNKNOWN UNKNOWN NONE NONE) (
UNKNOWN) 0 0 () () () '' () ())
38 'norma' 'se_nolineales' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL NONE
NONE FUNCTION INVOKED) (REAL 8) 0 0 (39 NONE) () () '' () ())
40 'order' 'my_funcs' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN NONE NONE) (
INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '6') () () '' () ())
41 'permutacion_filas' 'se_nolineales' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (42 NONE 43 NONE) ()
() '' () ())
44 'rkind10' 'kinds' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN NONE NONE) (
INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '10') () () '' () ())
45 'rkind16' 'kinds' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN NONE NONE) (
INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '16') () () '' () ())
46 'rkind4' 'kinds' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN NONE NONE) (
INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '4') () () '' () ())
47 'rkind8' 'kinds' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN NONE NONE) (
INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '8') () () '' () ())
48 'se_nolineales' 'se_nolineales' 1 ((MODULE UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' () ())
49 'selected_int_kind' '(intrinsic)' 1 ((PROCEDURE UNKNOWN INTRINSIC
UNKNOWN NONE NONE FUNCTION) (UNKNOWN) 0 0 () () () '' () ())
50 'selected_real_kind' '(intrinsic)' 1 ((PROCEDURE UNKNOWN INTRINSIC
UNKNOWN NONE NONE FUNCTION) (UNKNOWN) 0 0 () () () '' () ())
43 'fila' '' 51 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (INTEGER
4) 0 0 () () () '' () ())
42 'a_amp' '' 51 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DIMENSION
DUMMY) (REAL 8) 0 0 () (2 ASSUMED_SHAPE () () () ()) () '' () ())
39 'vector' '' 52 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION
DUMMY) (REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
36 'tol' '' 53 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (REAL 8) 0
0 () () () '' () ())
35 'num_iteraciones' '' 53 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY)
(INTEGER 4) 0 0 () () () '' () ())
34 'p' '' 53 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
32 'tol' '' 54 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (REAL 8) 0
0 () () () '' () ())
31 'num_iteraciones' '' 54 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY)
(INTEGER 4) 0 0 () () () '' () ())
30 'x' '' 54 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
28 'tol' '' 55 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (REAL 8) 0
0 () () () '' () ())
27 'num_iteraciones' '' 55 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY)
(INTEGER 4) 0 0 () () () '' () ())
26 'p' '' 55 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
24 'inv' '' 56 ((VARIABLE OUT UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 8) 0 0 () (2 ASSUMED_SHAPE () () () ()) () '' () ())
23 'a' '' 56 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 8) 0 0 () (2 ASSUMED_SHAPE () () () ()) () '' () ())
20 'o' '' 57 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
19 't' '' 57 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
12 'p' '' 58 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
10 'p' '' 59 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
8 'o' '' 60 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
7 't' '' 60 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
5 'p' '' 61 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
3 'p' '' 62 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
)

('deriv_parciales' 0 2 'es_divergente' 0 4 'f' 0 6 'g' 0 9
'g_gauss_seidel' 0 11 'ikind1' 0 13 'ikind16' 0 14 'ikind2' 0 15 'ikind4'
0 16 'ikind8' 0 17 'jacobiano' 0 18 'kinds' 0 21 'matriz_inversa' 0 22
'metodo_gauss_seidel' 0 25 'metodo_newton_raphson' 0 29
'metodo_punto_fijo' 0 33 'my_funcs' 0 37 'norma' 0 38 'order' 0 40
'permutacion_filas' 0 41 'rkind10' 0 44 'rkind16' 0 45 'rkind4' 0 46
'rkind8' 0 47 'se_nolineales' 0 48 'selected_int_kind' 0 49
'selected_real_kind' 0 50)
