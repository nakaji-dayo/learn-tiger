{-# OPTIONS_GHC -w #-}
module Parser where

import Prelude hiding (GT, LT)
import qualified AbSyn as A
import Lexer
import Text.Pretty.Simple (pPrint)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21
	= HappyTerminal (TigToken)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,443) ([0,224,22560,57345,0,0,64,0,0,0,100,0,0,0,0,0,0,0,4096,0,0,0,1,0,2,4096,0,65408,3,0,0,0,0,57344,0,344,224,49152,1,688,448,32768,32775,1376,896,0,7,2752,1792,0,0,0,512,0,0,0,0,0,0,0,0,0,1344,0,2048,0,0,0,0,0,0,0,0,0,896,24576,32773,3,0,0,0,1,3584,32768,21,14,0,0,0,4,0,16384,0,0,8192,0,65344,7,0,128,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,8196,57344,255,0,28,11008,7168,0,56,22016,14336,0,112,44032,28672,0,224,22528,57345,0,448,45056,49154,1,896,24576,32773,3,1792,49152,10,7,3584,32768,21,14,7168,0,43,28,14336,0,86,56,28672,0,172,112,57344,0,344,224,49152,1,688,448,0,0,0,128,0,7,2752,1792,0,0,4,0,0,8192,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,32768,0,0,0,0,1,2048,0,0,0,0,8200,0,0,0,0,1024,0,0,0,0,0,57344,8192,344,224,32768,0,64512,31,0,0,0,0,0,130,61440,127,0,4,57344,0,0,8,49152,1,0,16,32768,771,0,32,0,1543,0,64,0,3086,0,128,0,6172,0,256,0,12344,0,512,0,24688,0,0,0,0,0,0,0,0,0,0,0,768,0,0,0,1536,0,57344,8192,344,224,0,0,0,0,32768,32771,1376,896,0,7,2753,1792,0,14,5504,3584,0,0,0,0,0,1040,32768,1023,0,512,0,0,0,0,0,16,0,16512,0,8188,0,1024,0,0,0,0,0,0,0,3584,32768,21,14,7168,0,43,28,0,0,0,0,0,0,2,0,16384,0,65056,15,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,16,0,0,128,128,2048,0,0,0,4096,0,224,22560,57345,0,0,0,16384,0,1024,0,0,0,0,2,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,8,0,0,0,0,0,0,0,0,0,0,14,5506,3584,0,28,11008,7168,0,0,0,0,0,112,44032,28672,0,8256,0,4094,0,0,0,0,0,0,0,32768,0,512,0,32752,0,1024,0,65512,0,0,0,0,0,0,0,0,8,0,2,0,0,57344,8192,344,224,0,0,0,64,0,256,0,0,0,0,32768,0,0,0,0,512,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,448,45120,49154,1,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,14336,2048,86,56,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Exp","Decs","Dec","TyDec","TyId","Ty","TyFs","VarDec","OptTyAnn","FunDec","Seq","LVal","LVal1","Exp1","Vals","Atom","Arr1","RecFs","if","'-'","'('","')'","'{'","'}'","'['","']'","','","':'","';'","type","then","else","var","function","let","in","end","':='","array","of","while","for","to","break","do","nil","'+'","'*'","'/'","'='","'<>'","'<'","'<='","'>'","'>='","'&'","'|'","'.'","id","num","str","%eof"]
        bit_start = st Prelude.* 65
        bit_end = (st Prelude.+ 1) Prelude.* 65
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..64]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (22) = happyShift action_8
action_0 (23) = happyShift action_9
action_0 (24) = happyShift action_10
action_0 (38) = happyShift action_2
action_0 (44) = happyShift action_11
action_0 (45) = happyShift action_12
action_0 (47) = happyShift action_13
action_0 (49) = happyShift action_14
action_0 (62) = happyShift action_15
action_0 (63) = happyShift action_16
action_0 (64) = happyShift action_17
action_0 (4) = happyGoto action_3
action_0 (15) = happyGoto action_4
action_0 (16) = happyGoto action_5
action_0 (17) = happyGoto action_6
action_0 (19) = happyGoto action_7
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (38) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (33) = happyShift action_49
action_2 (36) = happyShift action_50
action_2 (37) = happyShift action_51
action_2 (5) = happyGoto action_44
action_2 (6) = happyGoto action_45
action_2 (7) = happyGoto action_46
action_2 (11) = happyGoto action_47
action_2 (13) = happyGoto action_48
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (65) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (41) = happyShift action_43
action_4 _ = happyReduce_59

action_5 (28) = happyShift action_41
action_5 (61) = happyShift action_42
action_5 _ = happyReduce_24

action_6 (23) = happyShift action_29
action_6 (50) = happyShift action_30
action_6 (51) = happyShift action_31
action_6 (52) = happyShift action_32
action_6 (53) = happyShift action_33
action_6 (54) = happyShift action_34
action_6 (55) = happyShift action_35
action_6 (56) = happyShift action_36
action_6 (57) = happyShift action_37
action_6 (58) = happyShift action_38
action_6 (59) = happyShift action_39
action_6 (60) = happyShift action_40
action_6 _ = happyReduce_2

action_7 _ = happyReduce_42

action_8 (22) = happyShift action_8
action_8 (23) = happyShift action_9
action_8 (24) = happyShift action_10
action_8 (44) = happyShift action_11
action_8 (45) = happyShift action_12
action_8 (47) = happyShift action_13
action_8 (49) = happyShift action_14
action_8 (62) = happyShift action_15
action_8 (63) = happyShift action_16
action_8 (64) = happyShift action_17
action_8 (15) = happyGoto action_4
action_8 (16) = happyGoto action_5
action_8 (17) = happyGoto action_28
action_8 (19) = happyGoto action_7
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (22) = happyShift action_8
action_9 (23) = happyShift action_9
action_9 (24) = happyShift action_10
action_9 (44) = happyShift action_11
action_9 (45) = happyShift action_12
action_9 (47) = happyShift action_13
action_9 (49) = happyShift action_14
action_9 (62) = happyShift action_15
action_9 (63) = happyShift action_16
action_9 (64) = happyShift action_17
action_9 (15) = happyGoto action_4
action_9 (16) = happyGoto action_5
action_9 (17) = happyGoto action_27
action_9 (19) = happyGoto action_7
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (22) = happyShift action_8
action_10 (23) = happyShift action_9
action_10 (24) = happyShift action_10
action_10 (25) = happyShift action_26
action_10 (38) = happyShift action_2
action_10 (44) = happyShift action_11
action_10 (45) = happyShift action_12
action_10 (47) = happyShift action_13
action_10 (49) = happyShift action_14
action_10 (62) = happyShift action_15
action_10 (63) = happyShift action_16
action_10 (64) = happyShift action_17
action_10 (4) = happyGoto action_24
action_10 (14) = happyGoto action_25
action_10 (15) = happyGoto action_4
action_10 (16) = happyGoto action_5
action_10 (17) = happyGoto action_6
action_10 (19) = happyGoto action_7
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (22) = happyShift action_8
action_11 (23) = happyShift action_9
action_11 (24) = happyShift action_10
action_11 (44) = happyShift action_11
action_11 (45) = happyShift action_12
action_11 (47) = happyShift action_13
action_11 (49) = happyShift action_14
action_11 (62) = happyShift action_15
action_11 (63) = happyShift action_16
action_11 (64) = happyShift action_17
action_11 (15) = happyGoto action_4
action_11 (16) = happyGoto action_5
action_11 (17) = happyGoto action_23
action_11 (19) = happyGoto action_7
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (62) = happyShift action_22
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_50

action_14 _ = happyReduce_56

action_15 (24) = happyShift action_18
action_15 (26) = happyShift action_19
action_15 (28) = happyShift action_20
action_15 (61) = happyShift action_21
action_15 _ = happyReduce_23

action_16 _ = happyReduce_54

action_17 _ = happyReduce_55

action_18 (22) = happyShift action_8
action_18 (23) = happyShift action_9
action_18 (24) = happyShift action_10
action_18 (44) = happyShift action_11
action_18 (45) = happyShift action_12
action_18 (47) = happyShift action_13
action_18 (49) = happyShift action_14
action_18 (62) = happyShift action_15
action_18 (63) = happyShift action_16
action_18 (64) = happyShift action_17
action_18 (15) = happyGoto action_4
action_18 (16) = happyGoto action_5
action_18 (17) = happyGoto action_81
action_18 (18) = happyGoto action_82
action_18 (19) = happyGoto action_7
action_18 _ = happyReduce_53

action_19 (62) = happyShift action_80
action_19 (21) = happyGoto action_79
action_19 _ = happyReduce_64

action_20 (22) = happyShift action_8
action_20 (23) = happyShift action_9
action_20 (24) = happyShift action_10
action_20 (44) = happyShift action_11
action_20 (45) = happyShift action_12
action_20 (47) = happyShift action_13
action_20 (49) = happyShift action_14
action_20 (62) = happyShift action_15
action_20 (63) = happyShift action_16
action_20 (64) = happyShift action_17
action_20 (15) = happyGoto action_4
action_20 (16) = happyGoto action_5
action_20 (17) = happyGoto action_78
action_20 (19) = happyGoto action_7
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (62) = happyShift action_77
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (41) = happyShift action_76
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (23) = happyShift action_29
action_23 (48) = happyShift action_75
action_23 (50) = happyShift action_30
action_23 (51) = happyShift action_31
action_23 (52) = happyShift action_32
action_23 (53) = happyShift action_33
action_23 (54) = happyShift action_34
action_23 (55) = happyShift action_35
action_23 (56) = happyShift action_36
action_23 (57) = happyShift action_37
action_23 (58) = happyShift action_38
action_23 (59) = happyShift action_39
action_23 (60) = happyShift action_40
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (32) = happyShift action_74
action_24 _ = happyReduce_21

action_25 (25) = happyShift action_73
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_45

action_27 _ = happyReduce_60

action_28 (23) = happyShift action_29
action_28 (34) = happyShift action_72
action_28 (50) = happyShift action_30
action_28 (51) = happyShift action_31
action_28 (52) = happyShift action_32
action_28 (53) = happyShift action_33
action_28 (54) = happyShift action_34
action_28 (55) = happyShift action_35
action_28 (56) = happyShift action_36
action_28 (57) = happyShift action_37
action_28 (58) = happyShift action_38
action_28 (59) = happyShift action_39
action_28 (60) = happyShift action_40
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (22) = happyShift action_8
action_29 (23) = happyShift action_9
action_29 (24) = happyShift action_10
action_29 (44) = happyShift action_11
action_29 (45) = happyShift action_12
action_29 (47) = happyShift action_13
action_29 (49) = happyShift action_14
action_29 (62) = happyShift action_15
action_29 (63) = happyShift action_16
action_29 (64) = happyShift action_17
action_29 (15) = happyGoto action_4
action_29 (16) = happyGoto action_5
action_29 (17) = happyGoto action_71
action_29 (19) = happyGoto action_7
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (22) = happyShift action_8
action_30 (23) = happyShift action_9
action_30 (24) = happyShift action_10
action_30 (44) = happyShift action_11
action_30 (45) = happyShift action_12
action_30 (47) = happyShift action_13
action_30 (49) = happyShift action_14
action_30 (62) = happyShift action_15
action_30 (63) = happyShift action_16
action_30 (64) = happyShift action_17
action_30 (15) = happyGoto action_4
action_30 (16) = happyGoto action_5
action_30 (17) = happyGoto action_70
action_30 (19) = happyGoto action_7
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (22) = happyShift action_8
action_31 (23) = happyShift action_9
action_31 (24) = happyShift action_10
action_31 (44) = happyShift action_11
action_31 (45) = happyShift action_12
action_31 (47) = happyShift action_13
action_31 (49) = happyShift action_14
action_31 (62) = happyShift action_15
action_31 (63) = happyShift action_16
action_31 (64) = happyShift action_17
action_31 (15) = happyGoto action_4
action_31 (16) = happyGoto action_5
action_31 (17) = happyGoto action_69
action_31 (19) = happyGoto action_7
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (22) = happyShift action_8
action_32 (23) = happyShift action_9
action_32 (24) = happyShift action_10
action_32 (44) = happyShift action_11
action_32 (45) = happyShift action_12
action_32 (47) = happyShift action_13
action_32 (49) = happyShift action_14
action_32 (62) = happyShift action_15
action_32 (63) = happyShift action_16
action_32 (64) = happyShift action_17
action_32 (15) = happyGoto action_4
action_32 (16) = happyGoto action_5
action_32 (17) = happyGoto action_68
action_32 (19) = happyGoto action_7
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (22) = happyShift action_8
action_33 (23) = happyShift action_9
action_33 (24) = happyShift action_10
action_33 (44) = happyShift action_11
action_33 (45) = happyShift action_12
action_33 (47) = happyShift action_13
action_33 (49) = happyShift action_14
action_33 (62) = happyShift action_15
action_33 (63) = happyShift action_16
action_33 (64) = happyShift action_17
action_33 (15) = happyGoto action_4
action_33 (16) = happyGoto action_5
action_33 (17) = happyGoto action_67
action_33 (19) = happyGoto action_7
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (22) = happyShift action_8
action_34 (23) = happyShift action_9
action_34 (24) = happyShift action_10
action_34 (44) = happyShift action_11
action_34 (45) = happyShift action_12
action_34 (47) = happyShift action_13
action_34 (49) = happyShift action_14
action_34 (62) = happyShift action_15
action_34 (63) = happyShift action_16
action_34 (64) = happyShift action_17
action_34 (15) = happyGoto action_4
action_34 (16) = happyGoto action_5
action_34 (17) = happyGoto action_66
action_34 (19) = happyGoto action_7
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (22) = happyShift action_8
action_35 (23) = happyShift action_9
action_35 (24) = happyShift action_10
action_35 (44) = happyShift action_11
action_35 (45) = happyShift action_12
action_35 (47) = happyShift action_13
action_35 (49) = happyShift action_14
action_35 (62) = happyShift action_15
action_35 (63) = happyShift action_16
action_35 (64) = happyShift action_17
action_35 (15) = happyGoto action_4
action_35 (16) = happyGoto action_5
action_35 (17) = happyGoto action_65
action_35 (19) = happyGoto action_7
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (22) = happyShift action_8
action_36 (23) = happyShift action_9
action_36 (24) = happyShift action_10
action_36 (44) = happyShift action_11
action_36 (45) = happyShift action_12
action_36 (47) = happyShift action_13
action_36 (49) = happyShift action_14
action_36 (62) = happyShift action_15
action_36 (63) = happyShift action_16
action_36 (64) = happyShift action_17
action_36 (15) = happyGoto action_4
action_36 (16) = happyGoto action_5
action_36 (17) = happyGoto action_64
action_36 (19) = happyGoto action_7
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (22) = happyShift action_8
action_37 (23) = happyShift action_9
action_37 (24) = happyShift action_10
action_37 (44) = happyShift action_11
action_37 (45) = happyShift action_12
action_37 (47) = happyShift action_13
action_37 (49) = happyShift action_14
action_37 (62) = happyShift action_15
action_37 (63) = happyShift action_16
action_37 (64) = happyShift action_17
action_37 (15) = happyGoto action_4
action_37 (16) = happyGoto action_5
action_37 (17) = happyGoto action_63
action_37 (19) = happyGoto action_7
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (22) = happyShift action_8
action_38 (23) = happyShift action_9
action_38 (24) = happyShift action_10
action_38 (44) = happyShift action_11
action_38 (45) = happyShift action_12
action_38 (47) = happyShift action_13
action_38 (49) = happyShift action_14
action_38 (62) = happyShift action_15
action_38 (63) = happyShift action_16
action_38 (64) = happyShift action_17
action_38 (15) = happyGoto action_4
action_38 (16) = happyGoto action_5
action_38 (17) = happyGoto action_62
action_38 (19) = happyGoto action_7
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (22) = happyShift action_8
action_39 (23) = happyShift action_9
action_39 (24) = happyShift action_10
action_39 (44) = happyShift action_11
action_39 (45) = happyShift action_12
action_39 (47) = happyShift action_13
action_39 (49) = happyShift action_14
action_39 (62) = happyShift action_15
action_39 (63) = happyShift action_16
action_39 (64) = happyShift action_17
action_39 (15) = happyGoto action_4
action_39 (16) = happyGoto action_5
action_39 (17) = happyGoto action_61
action_39 (19) = happyGoto action_7
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (22) = happyShift action_8
action_40 (23) = happyShift action_9
action_40 (24) = happyShift action_10
action_40 (44) = happyShift action_11
action_40 (45) = happyShift action_12
action_40 (47) = happyShift action_13
action_40 (49) = happyShift action_14
action_40 (62) = happyShift action_15
action_40 (63) = happyShift action_16
action_40 (64) = happyShift action_17
action_40 (15) = happyGoto action_4
action_40 (16) = happyGoto action_5
action_40 (17) = happyGoto action_60
action_40 (19) = happyGoto action_7
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (22) = happyShift action_8
action_41 (23) = happyShift action_9
action_41 (24) = happyShift action_10
action_41 (44) = happyShift action_11
action_41 (45) = happyShift action_12
action_41 (47) = happyShift action_13
action_41 (49) = happyShift action_14
action_41 (62) = happyShift action_15
action_41 (63) = happyShift action_16
action_41 (64) = happyShift action_17
action_41 (15) = happyGoto action_4
action_41 (16) = happyGoto action_5
action_41 (17) = happyGoto action_59
action_41 (19) = happyGoto action_7
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (62) = happyShift action_58
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (22) = happyShift action_8
action_43 (23) = happyShift action_9
action_43 (24) = happyShift action_10
action_43 (44) = happyShift action_11
action_43 (45) = happyShift action_12
action_43 (47) = happyShift action_13
action_43 (49) = happyShift action_14
action_43 (62) = happyShift action_15
action_43 (63) = happyShift action_16
action_43 (64) = happyShift action_17
action_43 (15) = happyGoto action_4
action_43 (16) = happyGoto action_5
action_43 (17) = happyGoto action_57
action_43 (19) = happyGoto action_7
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (39) = happyShift action_56
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (33) = happyShift action_49
action_45 (36) = happyShift action_50
action_45 (37) = happyShift action_51
action_45 (5) = happyGoto action_55
action_45 (6) = happyGoto action_45
action_45 (7) = happyGoto action_46
action_45 (11) = happyGoto action_47
action_45 (13) = happyGoto action_48
action_45 _ = happyReduce_4

action_46 _ = happyReduce_5

action_47 _ = happyReduce_6

action_48 _ = happyReduce_7

action_49 (62) = happyShift action_54
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (62) = happyShift action_53
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (62) = happyShift action_52
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (24) = happyShift action_97
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (31) = happyShift action_95
action_53 (41) = happyShift action_96
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (53) = happyShift action_94
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_3

action_56 (22) = happyShift action_8
action_56 (23) = happyShift action_9
action_56 (24) = happyShift action_10
action_56 (38) = happyShift action_2
action_56 (44) = happyShift action_11
action_56 (45) = happyShift action_12
action_56 (47) = happyShift action_13
action_56 (49) = happyShift action_14
action_56 (62) = happyShift action_15
action_56 (63) = happyShift action_16
action_56 (64) = happyShift action_17
action_56 (4) = happyGoto action_24
action_56 (14) = happyGoto action_93
action_56 (15) = happyGoto action_4
action_56 (16) = happyGoto action_5
action_56 (17) = happyGoto action_6
action_56 (19) = happyGoto action_7
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (23) = happyShift action_29
action_57 (50) = happyShift action_30
action_57 (51) = happyShift action_31
action_57 (52) = happyShift action_32
action_57 (53) = happyShift action_33
action_57 (54) = happyShift action_34
action_57 (55) = happyShift action_35
action_57 (56) = happyShift action_36
action_57 (57) = happyShift action_37
action_57 (58) = happyShift action_38
action_57 (59) = happyShift action_39
action_57 (60) = happyShift action_40
action_57 _ = happyReduce_41

action_58 _ = happyReduce_28

action_59 (23) = happyShift action_29
action_59 (29) = happyShift action_92
action_59 (50) = happyShift action_30
action_59 (51) = happyShift action_31
action_59 (52) = happyShift action_32
action_59 (53) = happyShift action_33
action_59 (54) = happyShift action_34
action_59 (55) = happyShift action_35
action_59 (56) = happyShift action_36
action_59 (57) = happyShift action_37
action_59 (58) = happyShift action_38
action_59 (59) = happyShift action_39
action_59 (60) = happyShift action_40
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (23) = happyShift action_29
action_60 (50) = happyShift action_30
action_60 (51) = happyShift action_31
action_60 (52) = happyShift action_32
action_60 _ = happyReduce_34

action_61 (23) = happyShift action_29
action_61 (50) = happyShift action_30
action_61 (51) = happyShift action_31
action_61 (52) = happyShift action_32
action_61 _ = happyReduce_33

action_62 (23) = happyShift action_29
action_62 (50) = happyShift action_30
action_62 (51) = happyShift action_31
action_62 (52) = happyShift action_32
action_62 (59) = happyShift action_39
action_62 (60) = happyShift action_40
action_62 _ = happyReduce_39

action_63 (23) = happyShift action_29
action_63 (50) = happyShift action_30
action_63 (51) = happyShift action_31
action_63 (52) = happyShift action_32
action_63 (59) = happyShift action_39
action_63 (60) = happyShift action_40
action_63 _ = happyReduce_37

action_64 (23) = happyShift action_29
action_64 (50) = happyShift action_30
action_64 (51) = happyShift action_31
action_64 (52) = happyShift action_32
action_64 (59) = happyShift action_39
action_64 (60) = happyShift action_40
action_64 _ = happyReduce_40

action_65 (23) = happyShift action_29
action_65 (50) = happyShift action_30
action_65 (51) = happyShift action_31
action_65 (52) = happyShift action_32
action_65 (59) = happyShift action_39
action_65 (60) = happyShift action_40
action_65 _ = happyReduce_38

action_66 (23) = happyShift action_29
action_66 (50) = happyShift action_30
action_66 (51) = happyShift action_31
action_66 (52) = happyShift action_32
action_66 (59) = happyShift action_39
action_66 (60) = happyShift action_40
action_66 _ = happyReduce_36

action_67 (23) = happyShift action_29
action_67 (50) = happyShift action_30
action_67 (51) = happyShift action_31
action_67 (52) = happyShift action_32
action_67 (59) = happyShift action_39
action_67 (60) = happyShift action_40
action_67 _ = happyReduce_35

action_68 _ = happyReduce_32

action_69 _ = happyReduce_31

action_70 (51) = happyShift action_31
action_70 (52) = happyShift action_32
action_70 _ = happyReduce_29

action_71 (51) = happyShift action_31
action_71 (52) = happyShift action_32
action_71 _ = happyReduce_30

action_72 (22) = happyShift action_8
action_72 (23) = happyShift action_9
action_72 (24) = happyShift action_10
action_72 (38) = happyShift action_2
action_72 (44) = happyShift action_11
action_72 (45) = happyShift action_12
action_72 (47) = happyShift action_13
action_72 (49) = happyShift action_14
action_72 (62) = happyShift action_15
action_72 (63) = happyShift action_16
action_72 (64) = happyShift action_17
action_72 (4) = happyGoto action_91
action_72 (15) = happyGoto action_4
action_72 (16) = happyGoto action_5
action_72 (17) = happyGoto action_6
action_72 (19) = happyGoto action_7
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_44

action_74 (22) = happyShift action_8
action_74 (23) = happyShift action_9
action_74 (24) = happyShift action_10
action_74 (38) = happyShift action_2
action_74 (44) = happyShift action_11
action_74 (45) = happyShift action_12
action_74 (47) = happyShift action_13
action_74 (49) = happyShift action_14
action_74 (62) = happyShift action_15
action_74 (63) = happyShift action_16
action_74 (64) = happyShift action_17
action_74 (4) = happyGoto action_24
action_74 (14) = happyGoto action_90
action_74 (15) = happyGoto action_4
action_74 (16) = happyGoto action_5
action_74 (17) = happyGoto action_6
action_74 (19) = happyGoto action_7
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (22) = happyShift action_8
action_75 (23) = happyShift action_9
action_75 (24) = happyShift action_10
action_75 (38) = happyShift action_2
action_75 (44) = happyShift action_11
action_75 (45) = happyShift action_12
action_75 (47) = happyShift action_13
action_75 (49) = happyShift action_14
action_75 (62) = happyShift action_15
action_75 (63) = happyShift action_16
action_75 (64) = happyShift action_17
action_75 (4) = happyGoto action_89
action_75 (15) = happyGoto action_4
action_75 (16) = happyGoto action_5
action_75 (17) = happyGoto action_6
action_75 (19) = happyGoto action_7
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (22) = happyShift action_8
action_76 (23) = happyShift action_9
action_76 (24) = happyShift action_10
action_76 (44) = happyShift action_11
action_76 (45) = happyShift action_12
action_76 (47) = happyShift action_13
action_76 (49) = happyShift action_14
action_76 (62) = happyShift action_15
action_76 (63) = happyShift action_16
action_76 (64) = happyShift action_17
action_76 (15) = happyGoto action_4
action_76 (16) = happyGoto action_5
action_76 (17) = happyGoto action_88
action_76 (19) = happyGoto action_7
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_27

action_78 (23) = happyShift action_29
action_78 (29) = happyShift action_87
action_78 (50) = happyShift action_30
action_78 (51) = happyShift action_31
action_78 (52) = happyShift action_32
action_78 (53) = happyShift action_33
action_78 (54) = happyShift action_34
action_78 (55) = happyShift action_35
action_78 (56) = happyShift action_36
action_78 (57) = happyShift action_37
action_78 (58) = happyShift action_38
action_78 (59) = happyShift action_39
action_78 (60) = happyShift action_40
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (27) = happyShift action_86
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (53) = happyShift action_85
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (23) = happyShift action_29
action_81 (30) = happyShift action_84
action_81 (50) = happyShift action_30
action_81 (51) = happyShift action_31
action_81 (52) = happyShift action_32
action_81 (53) = happyShift action_33
action_81 (54) = happyShift action_34
action_81 (55) = happyShift action_35
action_81 (56) = happyShift action_36
action_81 (57) = happyShift action_37
action_81 (58) = happyShift action_38
action_81 (59) = happyShift action_39
action_81 (60) = happyShift action_40
action_81 _ = happyReduce_51

action_82 (25) = happyShift action_83
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_43

action_84 (22) = happyShift action_8
action_84 (23) = happyShift action_9
action_84 (24) = happyShift action_10
action_84 (44) = happyShift action_11
action_84 (45) = happyShift action_12
action_84 (47) = happyShift action_13
action_84 (49) = happyShift action_14
action_84 (62) = happyShift action_15
action_84 (63) = happyShift action_16
action_84 (64) = happyShift action_17
action_84 (15) = happyGoto action_4
action_84 (16) = happyGoto action_5
action_84 (17) = happyGoto action_81
action_84 (18) = happyGoto action_113
action_84 (19) = happyGoto action_7
action_84 _ = happyReduce_53

action_85 (22) = happyShift action_8
action_85 (23) = happyShift action_9
action_85 (24) = happyShift action_10
action_85 (44) = happyShift action_11
action_85 (45) = happyShift action_12
action_85 (47) = happyShift action_13
action_85 (49) = happyShift action_14
action_85 (62) = happyShift action_15
action_85 (63) = happyShift action_16
action_85 (64) = happyShift action_17
action_85 (15) = happyGoto action_4
action_85 (16) = happyGoto action_5
action_85 (17) = happyGoto action_112
action_85 (19) = happyGoto action_7
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_57

action_87 (43) = happyShift action_111
action_87 (20) = happyGoto action_110
action_87 _ = happyReduce_25

action_88 (23) = happyShift action_29
action_88 (46) = happyShift action_109
action_88 (50) = happyShift action_30
action_88 (51) = happyShift action_31
action_88 (52) = happyShift action_32
action_88 (53) = happyShift action_33
action_88 (54) = happyShift action_34
action_88 (55) = happyShift action_35
action_88 (56) = happyShift action_36
action_88 (57) = happyShift action_37
action_88 (58) = happyShift action_38
action_88 (59) = happyShift action_39
action_88 (60) = happyShift action_40
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_46

action_90 _ = happyReduce_22

action_91 (35) = happyShift action_108
action_91 _ = happyReduce_48

action_92 _ = happyReduce_26

action_93 (40) = happyShift action_107
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (26) = happyShift action_104
action_94 (42) = happyShift action_105
action_94 (62) = happyShift action_106
action_94 (9) = happyGoto action_103
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (62) = happyShift action_102
action_95 (8) = happyGoto action_101
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (22) = happyShift action_8
action_96 (23) = happyShift action_9
action_96 (24) = happyShift action_10
action_96 (38) = happyShift action_2
action_96 (44) = happyShift action_11
action_96 (45) = happyShift action_12
action_96 (47) = happyShift action_13
action_96 (49) = happyShift action_14
action_96 (62) = happyShift action_15
action_96 (63) = happyShift action_16
action_96 (64) = happyShift action_17
action_96 (4) = happyGoto action_100
action_96 (15) = happyGoto action_4
action_96 (16) = happyGoto action_5
action_96 (17) = happyGoto action_6
action_96 (19) = happyGoto action_7
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (62) = happyShift action_99
action_97 (10) = happyGoto action_98
action_97 _ = happyReduce_15

action_98 (25) = happyShift action_122
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (31) = happyShift action_121
action_99 _ = happyFail (happyExpListPerState 99)

action_100 _ = happyReduce_16

action_101 (41) = happyShift action_120
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_9

action_103 _ = happyReduce_8

action_104 (62) = happyShift action_99
action_104 (10) = happyGoto action_119
action_104 _ = happyReduce_15

action_105 (43) = happyShift action_118
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_10

action_107 _ = happyReduce_1

action_108 (22) = happyShift action_8
action_108 (23) = happyShift action_9
action_108 (24) = happyShift action_10
action_108 (38) = happyShift action_2
action_108 (44) = happyShift action_11
action_108 (45) = happyShift action_12
action_108 (47) = happyShift action_13
action_108 (49) = happyShift action_14
action_108 (62) = happyShift action_15
action_108 (63) = happyShift action_16
action_108 (64) = happyShift action_17
action_108 (4) = happyGoto action_117
action_108 (15) = happyGoto action_4
action_108 (16) = happyGoto action_5
action_108 (17) = happyGoto action_6
action_108 (19) = happyGoto action_7
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (22) = happyShift action_8
action_109 (23) = happyShift action_9
action_109 (24) = happyShift action_10
action_109 (44) = happyShift action_11
action_109 (45) = happyShift action_12
action_109 (47) = happyShift action_13
action_109 (49) = happyShift action_14
action_109 (62) = happyShift action_15
action_109 (63) = happyShift action_16
action_109 (64) = happyShift action_17
action_109 (15) = happyGoto action_4
action_109 (16) = happyGoto action_5
action_109 (17) = happyGoto action_116
action_109 (19) = happyGoto action_7
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_58

action_111 (22) = happyShift action_8
action_111 (23) = happyShift action_9
action_111 (24) = happyShift action_10
action_111 (44) = happyShift action_11
action_111 (45) = happyShift action_12
action_111 (47) = happyShift action_13
action_111 (49) = happyShift action_14
action_111 (62) = happyShift action_15
action_111 (63) = happyShift action_16
action_111 (64) = happyShift action_17
action_111 (15) = happyGoto action_4
action_111 (16) = happyGoto action_5
action_111 (17) = happyGoto action_115
action_111 (19) = happyGoto action_7
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (23) = happyShift action_29
action_112 (30) = happyShift action_114
action_112 (50) = happyShift action_30
action_112 (51) = happyShift action_31
action_112 (52) = happyShift action_32
action_112 (53) = happyShift action_33
action_112 (54) = happyShift action_34
action_112 (55) = happyShift action_35
action_112 (56) = happyShift action_36
action_112 (57) = happyShift action_37
action_112 (58) = happyShift action_38
action_112 (59) = happyShift action_39
action_112 (60) = happyShift action_40
action_112 _ = happyReduce_62

action_113 _ = happyReduce_52

action_114 (62) = happyShift action_80
action_114 (21) = happyGoto action_130
action_114 _ = happyReduce_64

action_115 (23) = happyShift action_29
action_115 (50) = happyShift action_30
action_115 (51) = happyShift action_31
action_115 (52) = happyShift action_32
action_115 (53) = happyShift action_33
action_115 (54) = happyShift action_34
action_115 (55) = happyShift action_35
action_115 (56) = happyShift action_36
action_115 (57) = happyShift action_37
action_115 (58) = happyShift action_38
action_115 (59) = happyShift action_39
action_115 (60) = happyShift action_40
action_115 _ = happyReduce_61

action_116 (23) = happyShift action_29
action_116 (48) = happyShift action_129
action_116 (50) = happyShift action_30
action_116 (51) = happyShift action_31
action_116 (52) = happyShift action_32
action_116 (53) = happyShift action_33
action_116 (54) = happyShift action_34
action_116 (55) = happyShift action_35
action_116 (56) = happyShift action_36
action_116 (57) = happyShift action_37
action_116 (58) = happyShift action_38
action_116 (59) = happyShift action_39
action_116 (60) = happyShift action_40
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_49

action_118 (62) = happyShift action_128
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (27) = happyShift action_127
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (22) = happyShift action_8
action_120 (23) = happyShift action_9
action_120 (24) = happyShift action_10
action_120 (38) = happyShift action_2
action_120 (44) = happyShift action_11
action_120 (45) = happyShift action_12
action_120 (47) = happyShift action_13
action_120 (49) = happyShift action_14
action_120 (62) = happyShift action_15
action_120 (63) = happyShift action_16
action_120 (64) = happyShift action_17
action_120 (4) = happyGoto action_126
action_120 (15) = happyGoto action_4
action_120 (16) = happyGoto action_5
action_120 (17) = happyGoto action_6
action_120 (19) = happyGoto action_7
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (62) = happyShift action_125
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (31) = happyShift action_124
action_122 (12) = happyGoto action_123
action_122 _ = happyReduce_19

action_123 (53) = happyShift action_134
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (62) = happyShift action_102
action_124 (8) = happyGoto action_133
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (30) = happyShift action_132
action_125 _ = happyReduce_14

action_126 _ = happyReduce_17

action_127 _ = happyReduce_12

action_128 _ = happyReduce_11

action_129 (22) = happyShift action_8
action_129 (23) = happyShift action_9
action_129 (24) = happyShift action_10
action_129 (38) = happyShift action_2
action_129 (44) = happyShift action_11
action_129 (45) = happyShift action_12
action_129 (47) = happyShift action_13
action_129 (49) = happyShift action_14
action_129 (62) = happyShift action_15
action_129 (63) = happyShift action_16
action_129 (64) = happyShift action_17
action_129 (4) = happyGoto action_131
action_129 (15) = happyGoto action_4
action_129 (16) = happyGoto action_5
action_129 (17) = happyGoto action_6
action_129 (19) = happyGoto action_7
action_129 _ = happyFail (happyExpListPerState 129)

action_130 _ = happyReduce_63

action_131 _ = happyReduce_47

action_132 (62) = happyShift action_99
action_132 (10) = happyGoto action_136
action_132 _ = happyReduce_15

action_133 _ = happyReduce_18

action_134 (22) = happyShift action_8
action_134 (23) = happyShift action_9
action_134 (24) = happyShift action_10
action_134 (38) = happyShift action_2
action_134 (44) = happyShift action_11
action_134 (45) = happyShift action_12
action_134 (47) = happyShift action_13
action_134 (49) = happyShift action_14
action_134 (62) = happyShift action_15
action_134 (63) = happyShift action_16
action_134 (64) = happyShift action_17
action_134 (4) = happyGoto action_135
action_134 (15) = happyGoto action_4
action_134 (16) = happyGoto action_5
action_134 (17) = happyGoto action_6
action_134 (19) = happyGoto action_7
action_134 _ = happyFail (happyExpListPerState 134)

action_135 _ = happyReduce_20

action_136 _ = happyReduce_13

happyReduce_1 = happyMonadReduce 5 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.LetExp (reduceDecs happy_var_2) happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn4 r))

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1:happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyMonadReduce 4 7 happyReduction_8
happyReduction_8 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( fmap (\x -> A.TyDecs [x]) $ pos (A.TyDec happy_var_2 happy_var_4)))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_9 = happyMonadReduce 1 8 happyReduction_9
happyReduction_9 ((HappyTerminal ((_, Id happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos (\p -> (happy_var_1, p))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_10 = happyMonadReduce 1 9 happyReduction_10
happyReduction_10 ((HappyTerminal ((_, Id happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.NameTy happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_11 = happyMonadReduce 3 9 happyReduction_11
happyReduction_11 ((HappyTerminal ((_, Id happy_var_3))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.ArrTy happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (A.RecordTy happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyMonadReduce 5 10 happyReduction_13
happyReduction_13 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ \p -> (A.Field happy_var_1 happy_var_3 p):happy_var_5))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_14 = happyMonadReduce 3 10 happyReduction_14
happyReduction_14 ((HappyTerminal ((_, Id happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ \p -> [A.Field happy_var_1 happy_var_3 p]))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_15 = happySpecReduce_0  10 happyReduction_15
happyReduction_15  =  HappyAbsSyn10
		 ([]
	)

happyReduce_16 = happyMonadReduce 4 11 happyReduction_16
happyReduction_16 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.VarDec happy_var_2 Nothing happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn11 r))

happyReduce_17 = happyMonadReduce 6 11 happyReduction_17
happyReduction_17 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.VarDec happy_var_2 (Just happy_var_4) happy_var_6))
	) (\r -> happyReturn (HappyAbsSyn11 r))

happyReduce_18 = happySpecReduce_2  12 happyReduction_18
happyReduction_18 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Just happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_0  12 happyReduction_19
happyReduction_19  =  HappyAbsSyn12
		 (Nothing
	)

happyReduce_20 = happyMonadReduce 8 13 happyReduction_20
happyReduction_20 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ \p -> A.FunDecs [A.FunDec happy_var_2 happy_var_4 happy_var_6 happy_var_8 p]))
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_21 = happyMonadReduce 1 14 happyReduction_21
happyReduction_21 ((HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ \p -> A.SeqExp [(happy_var_1, p)]))
	) (\r -> happyReturn (HappyAbsSyn14 r))

happyReduce_22 = happyMonadReduce 3 14 happyReduction_22
happyReduction_22 ((HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ (\(A.SeqExp xs) p -> A.SeqExp ((happy_var_1,p):xs)) happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn14 r))

happyReduce_23 = happyMonadReduce 1 15 happyReduction_23
happyReduction_23 ((HappyTerminal ((_, Id happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.SimpleVar happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_24 = happySpecReduce_1  15 happyReduction_24
happyReduction_24 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happyMonadReduce 4 16 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ \p -> A.SubscriptVar (A.SimpleVar happy_var_1 p) happy_var_3 p))
	) (\r -> happyReturn (HappyAbsSyn16 r))

happyReduce_26 = happyMonadReduce 4 16 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.SubscriptVar happy_var_1 happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn16 r))

happyReduce_27 = happyMonadReduce 3 16 happyReduction_27
happyReduction_27 ((HappyTerminal ((_, Id happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ \p -> A.FieldVar (A.SimpleVar happy_var_1 p) happy_var_3 p))
	) (\r -> happyReturn (HappyAbsSyn16 r))

happyReduce_28 = happyMonadReduce 3 16 happyReduction_28
happyReduction_28 ((HappyTerminal ((_, Id happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.FieldVar happy_var_1 happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn16 r))

happyReduce_29 = happyMonadReduce 3 17 happyReduction_29
happyReduction_29 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.OpExp happy_var_1 A.Plus happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_30 = happyMonadReduce 3 17 happyReduction_30
happyReduction_30 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.OpExp happy_var_1 A.Minus happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_31 = happyMonadReduce 3 17 happyReduction_31
happyReduction_31 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.OpExp happy_var_1 A.Times happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_32 = happyMonadReduce 3 17 happyReduction_32
happyReduction_32 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.OpExp happy_var_1 A.Divide happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_33 = happyMonadReduce 3 17 happyReduction_33
happyReduction_33 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.IfExp happy_var_1 happy_var_3 (Just happy_var_1)))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_34 = happyMonadReduce 3 17 happyReduction_34
happyReduction_34 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.IfExp happy_var_1 happy_var_1 (Just happy_var_3)))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_35 = happyMonadReduce 3 17 happyReduction_35
happyReduction_35 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.OpExp happy_var_1 A.Eq happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_36 = happyMonadReduce 3 17 happyReduction_36
happyReduction_36 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.OpExp happy_var_1 A.Neq happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_37 = happyMonadReduce 3 17 happyReduction_37
happyReduction_37 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.OpExp happy_var_1 A.Gt happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_38 = happyMonadReduce 3 17 happyReduction_38
happyReduction_38 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.OpExp happy_var_1 A.Lt happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_39 = happyMonadReduce 3 17 happyReduction_39
happyReduction_39 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.OpExp happy_var_1 A.Gte happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_40 = happyMonadReduce 3 17 happyReduction_40
happyReduction_40 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.OpExp happy_var_1 A.Lte happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_41 = happyMonadReduce 3 17 happyReduction_41
happyReduction_41 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.AssignExp happy_var_1 happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_42 = happySpecReduce_1  17 happyReduction_42
happyReduction_42 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happyMonadReduce 4 17 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.CallExp happy_var_1 happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_44 = happySpecReduce_3  17 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  17 happyReduction_45
happyReduction_45 _
	_
	 =  HappyAbsSyn17
		 (A.SeqExp []
	)

happyReduce_46 = happyMonadReduce 4 17 happyReduction_46
happyReduction_46 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.WhileExp happy_var_2 happy_var_4))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_47 = happyMonadReduce 8 17 happyReduction_47
happyReduction_47 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.ForExp happy_var_2 happy_var_4 happy_var_6 happy_var_8))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_48 = happyMonadReduce 4 17 happyReduction_48
happyReduction_48 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.IfExp happy_var_2 happy_var_4 Nothing))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_49 = happyMonadReduce 6 17 happyReduction_49
happyReduction_49 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.IfExp happy_var_2 happy_var_4 (Just happy_var_6)))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_50 = happyMonadReduce 1 17 happyReduction_50
happyReduction_50 (_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pos A.BreakExp))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_51 = happySpecReduce_1  18 happyReduction_51
happyReduction_51 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  18 happyReduction_52
happyReduction_52 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1: happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_0  18 happyReduction_53
happyReduction_53  =  HappyAbsSyn18
		 ([]
	)

happyReduce_54 = happySpecReduce_1  19 happyReduction_54
happyReduction_54 (HappyTerminal ((_, Num happy_var_1)))
	 =  HappyAbsSyn19
		 (A.IntExp happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happyMonadReduce 1 19 happyReduction_55
happyReduction_55 ((HappyTerminal ((_, StringL happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.StringExp happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn19 r))

happyReduce_56 = happySpecReduce_1  19 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn19
		 (A.NilExp
	)

happyReduce_57 = happyMonadReduce 4 19 happyReduction_57
happyReduction_57 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.RecExp happy_var_3 happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn19 r))

happyReduce_58 = happyMonadReduce 5 19 happyReduction_58
happyReduction_58 ((HappyAbsSyn20  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.ArrExp happy_var_1 happy_var_3 happy_var_5))
	) (\r -> happyReturn (HappyAbsSyn19 r))

happyReduce_59 = happySpecReduce_1  19 happyReduction_59
happyReduction_59 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn19
		 (A.VarExp happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happyMonadReduce 2 19 happyReduction_60
happyReduction_60 ((HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( pos $ A.OpExp (A.IntExp 0) A.Minus happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn19 r))

happyReduce_61 = happySpecReduce_2  20 happyReduction_61
happyReduction_61 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happyMonadReduce 3 21 happyReduction_62
happyReduction_62 ((HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen ((( pos (\x -> [(happy_var_1, happy_var_3, x)])))
	) (\r -> happyReturn (HappyAbsSyn21 r))

happyReduce_63 = happyMonadReduce 5 21 happyReduction_63
happyReduction_63 ((HappyAbsSyn21  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_, Id happy_var_1))) `HappyStk`
	happyRest) tk
	 = happyThen ((( fmap (\x -> x:happy_var_5) $ pos (\x -> (happy_var_1, happy_var_3, x))))
	) (\r -> happyReturn (HappyAbsSyn21 r))

happyReduce_64 = happySpecReduce_0  21 happyReduction_64
happyReduction_64  =  HappyAbsSyn21
		 ([]
	)

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	(_, EOF) -> action 65 65 tk (HappyState action) sts stk;
	(_, happy_dollar_dollar@If) -> cont 22;
	(_, happy_dollar_dollar@Negation) -> cont 23;
	(_, happy_dollar_dollar@LParen) -> cont 24;
	(_, happy_dollar_dollar@RParen) -> cont 25;
	(_, happy_dollar_dollar@LBrace) -> cont 26;
	(_, happy_dollar_dollar@RBrace) -> cont 27;
	(_, happy_dollar_dollar@LBracket) -> cont 28;
	(_, happy_dollar_dollar@RBracket) -> cont 29;
	(_, happy_dollar_dollar@Comma) -> cont 30;
	(_, happy_dollar_dollar@Colon) -> cont 31;
	(_, happy_dollar_dollar@Semicolon) -> cont 32;
	(_, happy_dollar_dollar@Type) -> cont 33;
	(_, happy_dollar_dollar@Then) -> cont 34;
	(_, happy_dollar_dollar@Else) -> cont 35;
	(_, happy_dollar_dollar@Var) -> cont 36;
	(_, happy_dollar_dollar@Function) -> cont 37;
	(_, happy_dollar_dollar@Let) -> cont 38;
	(_, happy_dollar_dollar@In) -> cont 39;
	(_, happy_dollar_dollar@End) -> cont 40;
	(_, happy_dollar_dollar@Bind) -> cont 41;
	(_, happy_dollar_dollar@Array) -> cont 42;
	(_, happy_dollar_dollar@Of) -> cont 43;
	(_, happy_dollar_dollar@While) -> cont 44;
	(_, happy_dollar_dollar@For) -> cont 45;
	(_, happy_dollar_dollar@To) -> cont 46;
	(_, happy_dollar_dollar@Break) -> cont 47;
	(_, happy_dollar_dollar@Do) -> cont 48;
	(_, happy_dollar_dollar@Nil) -> cont 49;
	(_, happy_dollar_dollar@Plus) -> cont 50;
	(_, happy_dollar_dollar@Times) -> cont 51;
	(_, happy_dollar_dollar@Div) -> cont 52;
	(_, happy_dollar_dollar@Eq) -> cont 53;
	(_, happy_dollar_dollar@NotEq) -> cont 54;
	(_, happy_dollar_dollar@LT) -> cont 55;
	(_, happy_dollar_dollar@LTE) -> cont 56;
	(_, happy_dollar_dollar@GT) -> cont 57;
	(_, happy_dollar_dollar@GTE) -> cont 58;
	(_, happy_dollar_dollar@And) -> cont 59;
	(_, happy_dollar_dollar@Or) -> cont 60;
	(_, happy_dollar_dollar@Dot) -> cont 61;
	(_, Id happy_dollar_dollar) -> cont 62;
	(_, Num happy_dollar_dollar) -> cont 63;
	(_, StringL happy_dollar_dollar) -> cont 64;
	_ -> happyError' (tk, [])
	})

happyError_ explist 65 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Alex a
happyReturn = (Prelude.return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((TigToken), [Prelude.String]) -> Alex a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
calc = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- stubPos = AlexPn 0 0 0

-- 出題からは、FunDec, VarDec, FunDecはまとめなさそうな気がするが、意図としてはまとめて良さそうなのでそうする。
-- TODO: Decの型キモい。全体見えてきたら直す
reduceDecs :: [A.Dec] -> [A.Dec]
reduceDecs xs =
  let (fs, vs, ts) = foldr f ([], [], []) xs
  in concat[w ts A.TyDecs, vs, w fs A.FunDecs]
  where
    f (A.FunDecs f) (fs, vs, ts) = (f ++ fs, vs, ts)
    f v@(A.VarDec _ _ _ _) (fs, vs, ts) = (fs, v:vs, ts)
    f (A.TyDecs d) (fs, vs, ts) = (fs, vs, d ++ ts)
    w xs c = if null xs then [] else [c xs]

pos c = Alex $ \s -> Right (s, c (alex_pos s))

parseError :: TigToken -> Alex a
parseError x = error $ "Parse error: " ++ show x

parse :: String -> Either String A.Exp
parse s = do
  runAlex s calc
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
