{-# OPTIONS_GHC -w #-}
module Main where

import Prelude hiding (GT, LT)
import qualified AbSyn as A
import Lexer
import Text.Pretty.Simple (pPrint)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,533) ([0,112,44048,61440,0,0,32,0,0,0,50,0,0,0,0,0,0,4096,0,0,0,0,4096,0,0,2048,0,65472,1,0,0,0,0,28672,0,172,240,57344,0,344,480,49152,16387,688,960,32768,3,1376,1920,0,0,0,256,0,0,0,0,0,0,0,0,0,544,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3840,49152,10,15,3584,32768,21,30,0,0,0,4,0,16384,0,0,8192,0,65344,7,0,128,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,8196,57344,255,0,28,11008,15360,0,56,22016,30720,0,112,44032,61440,0,224,22528,57345,1,448,45056,49154,3,896,24576,32773,7,1792,49152,10,15,3584,32768,21,30,7168,0,43,60,14336,0,86,120,28672,0,172,240,57344,0,344,480,49152,1,688,960,0,0,0,128,0,0,2,0,0,36864,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,16384,0,0,0,32768,0,1024,0,0,0,0,4100,0,0,0,0,512,0,0,0,0,0,0,0,0,0,57344,8192,344,480,0,8,0,0,0,0,16384,0,0,0,0,0,0,4,57344,0,0,8,49152,1,0,16,32768,771,0,32,0,1543,0,64,0,3086,0,128,0,6172,0,256,0,12344,0,512,0,24688,0,0,0,0,0,0,0,0,0,0,0,768,0,0,0,1536,0,57344,8192,344,480,0,0,0,0,32768,32771,1376,1920,0,7,2753,3840,0,14,5504,7680,0,256,0,512,0,1040,32768,1023,0,4128,0,2047,0,256,0,0,0,0,0,0,0,0,0,0,0,1792,49152,10,15,32768,16384,0,1,0,0,0,0,14336,0,86,120,0,0,0,16,16384,0,65056,15,0,0,0,0,0,0,0,0,0,8192,0,0,0,14,5504,7680,0,0,0,0,0,0,32,0,0,256,256,4096,0,0,0,8192,0,448,45120,49154,3,0,0,32768,0,2048,0,0,0,0,4,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,16,0,0,0,0,0,0,516,57344,255,0,28,11012,15360,0,56,22016,30720,0,1024,0,2048,0,4160,0,4094,0,0,0,0,0,0,0,0,0,1792,49152,10,15,0,0,0,0,0,0,0,0,0,2,0,4,8192,0,65344,7,0,0,0,0,0,0,0,64,0,0,0,128,0,32,0,0,0,14,5506,7680,0,0,0,1024,0,4096,0,4,0,0,0,4096,0,224,22528,57345,1,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14336,2048,86,120,0,0,0,0,0,0,0,32,32768,0,64512,31,0,0,16384,0,0,7,2752,3840,0,0,0,0,0,8,49152,511,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Exp","Decs","Dec","TyDec","TyId","Ty","TyFs","VarDec","FunDec","Seq","LVal","ValAcc","Exp1","Vals","Atom","Arr1","RecFs","if","'-'","'('","')'","'{'","'}'","'['","']'","','","':'","';'","type","then","else","var","function","let","in","end","':='","array","of","while","for","to","break","do","nil","'+'","'*'","'/'","'='","'<>'","'<'","'<='","'>'","'>='","'&'","'|'","'.'","id","num","real","str","%eof"]
        bit_start = st Prelude.* 65
        bit_end = (st Prelude.+ 1) Prelude.* 65
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..64]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (21) = happyShift action_8
action_0 (22) = happyShift action_9
action_0 (23) = happyShift action_10
action_0 (37) = happyShift action_2
action_0 (43) = happyShift action_11
action_0 (44) = happyShift action_12
action_0 (46) = happyShift action_13
action_0 (48) = happyShift action_14
action_0 (61) = happyShift action_15
action_0 (62) = happyShift action_16
action_0 (63) = happyShift action_17
action_0 (64) = happyShift action_18
action_0 (4) = happyGoto action_3
action_0 (8) = happyGoto action_4
action_0 (14) = happyGoto action_5
action_0 (16) = happyGoto action_6
action_0 (18) = happyGoto action_7
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (37) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (32) = happyShift action_49
action_2 (35) = happyShift action_50
action_2 (36) = happyShift action_51
action_2 (5) = happyGoto action_44
action_2 (6) = happyGoto action_45
action_2 (7) = happyGoto action_46
action_2 (11) = happyGoto action_47
action_2 (12) = happyGoto action_48
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (65) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (25) = happyShift action_43
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (40) = happyShift action_42
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (22) = happyShift action_30
action_6 (49) = happyShift action_31
action_6 (50) = happyShift action_32
action_6 (51) = happyShift action_33
action_6 (52) = happyShift action_34
action_6 (53) = happyShift action_35
action_6 (54) = happyShift action_36
action_6 (55) = happyShift action_37
action_6 (56) = happyShift action_38
action_6 (57) = happyShift action_39
action_6 (58) = happyShift action_40
action_6 (59) = happyShift action_41
action_6 _ = happyReduce_2

action_7 _ = happyReduce_38

action_8 (21) = happyShift action_8
action_8 (22) = happyShift action_9
action_8 (23) = happyShift action_10
action_8 (43) = happyShift action_11
action_8 (44) = happyShift action_12
action_8 (46) = happyShift action_13
action_8 (48) = happyShift action_14
action_8 (61) = happyShift action_15
action_8 (62) = happyShift action_16
action_8 (63) = happyShift action_17
action_8 (64) = happyShift action_18
action_8 (8) = happyGoto action_4
action_8 (14) = happyGoto action_5
action_8 (16) = happyGoto action_29
action_8 (18) = happyGoto action_7
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (21) = happyShift action_8
action_9 (22) = happyShift action_9
action_9 (23) = happyShift action_10
action_9 (43) = happyShift action_11
action_9 (44) = happyShift action_12
action_9 (46) = happyShift action_13
action_9 (48) = happyShift action_14
action_9 (61) = happyShift action_15
action_9 (62) = happyShift action_16
action_9 (63) = happyShift action_17
action_9 (64) = happyShift action_18
action_9 (8) = happyGoto action_4
action_9 (14) = happyGoto action_5
action_9 (16) = happyGoto action_28
action_9 (18) = happyGoto action_7
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (21) = happyShift action_8
action_10 (22) = happyShift action_9
action_10 (23) = happyShift action_10
action_10 (24) = happyShift action_27
action_10 (37) = happyShift action_2
action_10 (43) = happyShift action_11
action_10 (44) = happyShift action_12
action_10 (46) = happyShift action_13
action_10 (48) = happyShift action_14
action_10 (61) = happyShift action_15
action_10 (62) = happyShift action_16
action_10 (63) = happyShift action_17
action_10 (64) = happyShift action_18
action_10 (4) = happyGoto action_25
action_10 (8) = happyGoto action_4
action_10 (13) = happyGoto action_26
action_10 (14) = happyGoto action_5
action_10 (16) = happyGoto action_6
action_10 (18) = happyGoto action_7
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (21) = happyShift action_8
action_11 (22) = happyShift action_9
action_11 (23) = happyShift action_10
action_11 (43) = happyShift action_11
action_11 (44) = happyShift action_12
action_11 (46) = happyShift action_13
action_11 (48) = happyShift action_14
action_11 (61) = happyShift action_15
action_11 (62) = happyShift action_16
action_11 (63) = happyShift action_17
action_11 (64) = happyShift action_18
action_11 (8) = happyGoto action_4
action_11 (14) = happyGoto action_5
action_11 (16) = happyGoto action_24
action_11 (18) = happyGoto action_7
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (61) = happyShift action_23
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_47

action_14 _ = happyReduce_54

action_15 (23) = happyShift action_20
action_15 (25) = happyReduce_9
action_15 (27) = happyShift action_21
action_15 (40) = happyReduce_24
action_15 (60) = happyShift action_22
action_15 (15) = happyGoto action_19
action_15 _ = happyReduce_53

action_16 _ = happyReduce_50

action_17 _ = happyReduce_51

action_18 _ = happyReduce_52

action_19 _ = happyReduce_21

action_20 (21) = happyShift action_8
action_20 (22) = happyShift action_9
action_20 (23) = happyShift action_10
action_20 (24) = happyShift action_82
action_20 (43) = happyShift action_11
action_20 (44) = happyShift action_12
action_20 (46) = happyShift action_13
action_20 (48) = happyShift action_14
action_20 (61) = happyShift action_15
action_20 (62) = happyShift action_16
action_20 (63) = happyShift action_17
action_20 (64) = happyShift action_18
action_20 (8) = happyGoto action_4
action_20 (14) = happyGoto action_5
action_20 (16) = happyGoto action_80
action_20 (17) = happyGoto action_81
action_20 (18) = happyGoto action_7
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (21) = happyShift action_8
action_21 (22) = happyShift action_9
action_21 (23) = happyShift action_10
action_21 (43) = happyShift action_11
action_21 (44) = happyShift action_12
action_21 (46) = happyShift action_13
action_21 (48) = happyShift action_14
action_21 (61) = happyShift action_15
action_21 (62) = happyShift action_16
action_21 (63) = happyShift action_17
action_21 (64) = happyShift action_18
action_21 (8) = happyGoto action_4
action_21 (14) = happyGoto action_5
action_21 (16) = happyGoto action_79
action_21 (18) = happyGoto action_7
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (61) = happyShift action_78
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (40) = happyShift action_77
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (22) = happyShift action_30
action_24 (47) = happyShift action_76
action_24 (49) = happyShift action_31
action_24 (50) = happyShift action_32
action_24 (51) = happyShift action_33
action_24 (52) = happyShift action_34
action_24 (53) = happyShift action_35
action_24 (54) = happyShift action_36
action_24 (55) = happyShift action_37
action_24 (56) = happyShift action_38
action_24 (57) = happyShift action_39
action_24 (58) = happyShift action_40
action_24 (59) = happyShift action_41
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (31) = happyShift action_75
action_25 _ = happyReduce_19

action_26 (24) = happyShift action_74
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_42

action_28 _ = happyReduce_58

action_29 (22) = happyShift action_30
action_29 (33) = happyShift action_73
action_29 (49) = happyShift action_31
action_29 (50) = happyShift action_32
action_29 (51) = happyShift action_33
action_29 (52) = happyShift action_34
action_29 (53) = happyShift action_35
action_29 (54) = happyShift action_36
action_29 (55) = happyShift action_37
action_29 (56) = happyShift action_38
action_29 (57) = happyShift action_39
action_29 (58) = happyShift action_40
action_29 (59) = happyShift action_41
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (21) = happyShift action_8
action_30 (22) = happyShift action_9
action_30 (23) = happyShift action_10
action_30 (43) = happyShift action_11
action_30 (44) = happyShift action_12
action_30 (46) = happyShift action_13
action_30 (48) = happyShift action_14
action_30 (61) = happyShift action_15
action_30 (62) = happyShift action_16
action_30 (63) = happyShift action_17
action_30 (64) = happyShift action_18
action_30 (8) = happyGoto action_4
action_30 (14) = happyGoto action_5
action_30 (16) = happyGoto action_72
action_30 (18) = happyGoto action_7
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (21) = happyShift action_8
action_31 (22) = happyShift action_9
action_31 (23) = happyShift action_10
action_31 (43) = happyShift action_11
action_31 (44) = happyShift action_12
action_31 (46) = happyShift action_13
action_31 (48) = happyShift action_14
action_31 (61) = happyShift action_15
action_31 (62) = happyShift action_16
action_31 (63) = happyShift action_17
action_31 (64) = happyShift action_18
action_31 (8) = happyGoto action_4
action_31 (14) = happyGoto action_5
action_31 (16) = happyGoto action_71
action_31 (18) = happyGoto action_7
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (21) = happyShift action_8
action_32 (22) = happyShift action_9
action_32 (23) = happyShift action_10
action_32 (43) = happyShift action_11
action_32 (44) = happyShift action_12
action_32 (46) = happyShift action_13
action_32 (48) = happyShift action_14
action_32 (61) = happyShift action_15
action_32 (62) = happyShift action_16
action_32 (63) = happyShift action_17
action_32 (64) = happyShift action_18
action_32 (8) = happyGoto action_4
action_32 (14) = happyGoto action_5
action_32 (16) = happyGoto action_70
action_32 (18) = happyGoto action_7
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (21) = happyShift action_8
action_33 (22) = happyShift action_9
action_33 (23) = happyShift action_10
action_33 (43) = happyShift action_11
action_33 (44) = happyShift action_12
action_33 (46) = happyShift action_13
action_33 (48) = happyShift action_14
action_33 (61) = happyShift action_15
action_33 (62) = happyShift action_16
action_33 (63) = happyShift action_17
action_33 (64) = happyShift action_18
action_33 (8) = happyGoto action_4
action_33 (14) = happyGoto action_5
action_33 (16) = happyGoto action_69
action_33 (18) = happyGoto action_7
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (21) = happyShift action_8
action_34 (22) = happyShift action_9
action_34 (23) = happyShift action_10
action_34 (43) = happyShift action_11
action_34 (44) = happyShift action_12
action_34 (46) = happyShift action_13
action_34 (48) = happyShift action_14
action_34 (61) = happyShift action_15
action_34 (62) = happyShift action_16
action_34 (63) = happyShift action_17
action_34 (64) = happyShift action_18
action_34 (8) = happyGoto action_4
action_34 (14) = happyGoto action_5
action_34 (16) = happyGoto action_68
action_34 (18) = happyGoto action_7
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (21) = happyShift action_8
action_35 (22) = happyShift action_9
action_35 (23) = happyShift action_10
action_35 (43) = happyShift action_11
action_35 (44) = happyShift action_12
action_35 (46) = happyShift action_13
action_35 (48) = happyShift action_14
action_35 (61) = happyShift action_15
action_35 (62) = happyShift action_16
action_35 (63) = happyShift action_17
action_35 (64) = happyShift action_18
action_35 (8) = happyGoto action_4
action_35 (14) = happyGoto action_5
action_35 (16) = happyGoto action_67
action_35 (18) = happyGoto action_7
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (21) = happyShift action_8
action_36 (22) = happyShift action_9
action_36 (23) = happyShift action_10
action_36 (43) = happyShift action_11
action_36 (44) = happyShift action_12
action_36 (46) = happyShift action_13
action_36 (48) = happyShift action_14
action_36 (61) = happyShift action_15
action_36 (62) = happyShift action_16
action_36 (63) = happyShift action_17
action_36 (64) = happyShift action_18
action_36 (8) = happyGoto action_4
action_36 (14) = happyGoto action_5
action_36 (16) = happyGoto action_66
action_36 (18) = happyGoto action_7
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (21) = happyShift action_8
action_37 (22) = happyShift action_9
action_37 (23) = happyShift action_10
action_37 (43) = happyShift action_11
action_37 (44) = happyShift action_12
action_37 (46) = happyShift action_13
action_37 (48) = happyShift action_14
action_37 (61) = happyShift action_15
action_37 (62) = happyShift action_16
action_37 (63) = happyShift action_17
action_37 (64) = happyShift action_18
action_37 (8) = happyGoto action_4
action_37 (14) = happyGoto action_5
action_37 (16) = happyGoto action_65
action_37 (18) = happyGoto action_7
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (21) = happyShift action_8
action_38 (22) = happyShift action_9
action_38 (23) = happyShift action_10
action_38 (43) = happyShift action_11
action_38 (44) = happyShift action_12
action_38 (46) = happyShift action_13
action_38 (48) = happyShift action_14
action_38 (61) = happyShift action_15
action_38 (62) = happyShift action_16
action_38 (63) = happyShift action_17
action_38 (64) = happyShift action_18
action_38 (8) = happyGoto action_4
action_38 (14) = happyGoto action_5
action_38 (16) = happyGoto action_64
action_38 (18) = happyGoto action_7
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (21) = happyShift action_8
action_39 (22) = happyShift action_9
action_39 (23) = happyShift action_10
action_39 (43) = happyShift action_11
action_39 (44) = happyShift action_12
action_39 (46) = happyShift action_13
action_39 (48) = happyShift action_14
action_39 (61) = happyShift action_15
action_39 (62) = happyShift action_16
action_39 (63) = happyShift action_17
action_39 (64) = happyShift action_18
action_39 (8) = happyGoto action_4
action_39 (14) = happyGoto action_5
action_39 (16) = happyGoto action_63
action_39 (18) = happyGoto action_7
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (21) = happyShift action_8
action_40 (22) = happyShift action_9
action_40 (23) = happyShift action_10
action_40 (43) = happyShift action_11
action_40 (44) = happyShift action_12
action_40 (46) = happyShift action_13
action_40 (48) = happyShift action_14
action_40 (61) = happyShift action_15
action_40 (62) = happyShift action_16
action_40 (63) = happyShift action_17
action_40 (64) = happyShift action_18
action_40 (8) = happyGoto action_4
action_40 (14) = happyGoto action_5
action_40 (16) = happyGoto action_62
action_40 (18) = happyGoto action_7
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (21) = happyShift action_8
action_41 (22) = happyShift action_9
action_41 (23) = happyShift action_10
action_41 (43) = happyShift action_11
action_41 (44) = happyShift action_12
action_41 (46) = happyShift action_13
action_41 (48) = happyShift action_14
action_41 (61) = happyShift action_15
action_41 (62) = happyShift action_16
action_41 (63) = happyShift action_17
action_41 (64) = happyShift action_18
action_41 (8) = happyGoto action_4
action_41 (14) = happyGoto action_5
action_41 (16) = happyGoto action_61
action_41 (18) = happyGoto action_7
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (21) = happyShift action_8
action_42 (22) = happyShift action_9
action_42 (23) = happyShift action_10
action_42 (43) = happyShift action_11
action_42 (44) = happyShift action_12
action_42 (46) = happyShift action_13
action_42 (48) = happyShift action_14
action_42 (61) = happyShift action_15
action_42 (62) = happyShift action_16
action_42 (63) = happyShift action_17
action_42 (64) = happyShift action_18
action_42 (8) = happyGoto action_4
action_42 (14) = happyGoto action_5
action_42 (16) = happyGoto action_60
action_42 (18) = happyGoto action_7
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (61) = happyShift action_59
action_43 (20) = happyGoto action_58
action_43 _ = happyReduce_63

action_44 (38) = happyShift action_57
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (32) = happyShift action_49
action_45 (35) = happyShift action_50
action_45 (36) = happyShift action_51
action_45 (5) = happyGoto action_56
action_45 (6) = happyGoto action_45
action_45 (7) = happyGoto action_46
action_45 (11) = happyGoto action_47
action_45 (12) = happyGoto action_48
action_45 _ = happyReduce_4

action_46 _ = happyReduce_5

action_47 _ = happyReduce_6

action_48 _ = happyReduce_7

action_49 (61) = happyShift action_55
action_49 (8) = happyGoto action_54
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (61) = happyShift action_53
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (61) = happyShift action_52
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (23) = happyShift action_99
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (30) = happyShift action_97
action_53 (40) = happyShift action_98
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (52) = happyShift action_96
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_9

action_56 _ = happyReduce_3

action_57 (21) = happyShift action_8
action_57 (22) = happyShift action_9
action_57 (23) = happyShift action_10
action_57 (37) = happyShift action_2
action_57 (43) = happyShift action_11
action_57 (44) = happyShift action_12
action_57 (46) = happyShift action_13
action_57 (48) = happyShift action_14
action_57 (61) = happyShift action_15
action_57 (62) = happyShift action_16
action_57 (63) = happyShift action_17
action_57 (64) = happyShift action_18
action_57 (4) = happyGoto action_25
action_57 (8) = happyGoto action_4
action_57 (13) = happyGoto action_95
action_57 (14) = happyGoto action_5
action_57 (16) = happyGoto action_6
action_57 (18) = happyGoto action_7
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (26) = happyShift action_94
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (52) = happyShift action_93
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_37

action_61 (22) = happyShift action_30
action_61 (49) = happyShift action_31
action_61 (50) = happyShift action_32
action_61 (51) = happyShift action_33
action_61 _ = happyReduce_30

action_62 (22) = happyShift action_30
action_62 (49) = happyShift action_31
action_62 (50) = happyShift action_32
action_62 (51) = happyShift action_33
action_62 _ = happyReduce_29

action_63 (22) = happyShift action_30
action_63 (49) = happyShift action_31
action_63 (50) = happyShift action_32
action_63 (51) = happyShift action_33
action_63 (58) = happyShift action_40
action_63 (59) = happyShift action_41
action_63 _ = happyReduce_35

action_64 (22) = happyShift action_30
action_64 (49) = happyShift action_31
action_64 (50) = happyShift action_32
action_64 (51) = happyShift action_33
action_64 (58) = happyShift action_40
action_64 (59) = happyShift action_41
action_64 _ = happyReduce_33

action_65 (22) = happyShift action_30
action_65 (49) = happyShift action_31
action_65 (50) = happyShift action_32
action_65 (51) = happyShift action_33
action_65 (58) = happyShift action_40
action_65 (59) = happyShift action_41
action_65 _ = happyReduce_36

action_66 (22) = happyShift action_30
action_66 (49) = happyShift action_31
action_66 (50) = happyShift action_32
action_66 (51) = happyShift action_33
action_66 (58) = happyShift action_40
action_66 (59) = happyShift action_41
action_66 _ = happyReduce_34

action_67 (22) = happyShift action_30
action_67 (49) = happyShift action_31
action_67 (50) = happyShift action_32
action_67 (51) = happyShift action_33
action_67 (58) = happyShift action_40
action_67 (59) = happyShift action_41
action_67 _ = happyReduce_32

action_68 (22) = happyShift action_30
action_68 (49) = happyShift action_31
action_68 (50) = happyShift action_32
action_68 (51) = happyShift action_33
action_68 (58) = happyShift action_40
action_68 (59) = happyShift action_41
action_68 _ = happyReduce_31

action_69 _ = happyReduce_28

action_70 _ = happyReduce_27

action_71 (50) = happyShift action_32
action_71 (51) = happyShift action_33
action_71 _ = happyReduce_25

action_72 (50) = happyShift action_32
action_72 (51) = happyShift action_33
action_72 _ = happyReduce_26

action_73 (21) = happyShift action_8
action_73 (22) = happyShift action_9
action_73 (23) = happyShift action_10
action_73 (37) = happyShift action_2
action_73 (43) = happyShift action_11
action_73 (44) = happyShift action_12
action_73 (46) = happyShift action_13
action_73 (48) = happyShift action_14
action_73 (61) = happyShift action_15
action_73 (62) = happyShift action_16
action_73 (63) = happyShift action_17
action_73 (64) = happyShift action_18
action_73 (4) = happyGoto action_92
action_73 (8) = happyGoto action_4
action_73 (14) = happyGoto action_5
action_73 (16) = happyGoto action_6
action_73 (18) = happyGoto action_7
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_41

action_75 (21) = happyShift action_8
action_75 (22) = happyShift action_9
action_75 (23) = happyShift action_10
action_75 (37) = happyShift action_2
action_75 (43) = happyShift action_11
action_75 (44) = happyShift action_12
action_75 (46) = happyShift action_13
action_75 (48) = happyShift action_14
action_75 (61) = happyShift action_15
action_75 (62) = happyShift action_16
action_75 (63) = happyShift action_17
action_75 (64) = happyShift action_18
action_75 (4) = happyGoto action_25
action_75 (8) = happyGoto action_4
action_75 (13) = happyGoto action_91
action_75 (14) = happyGoto action_5
action_75 (16) = happyGoto action_6
action_75 (18) = happyGoto action_7
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (21) = happyShift action_8
action_76 (22) = happyShift action_9
action_76 (23) = happyShift action_10
action_76 (37) = happyShift action_2
action_76 (43) = happyShift action_11
action_76 (44) = happyShift action_12
action_76 (46) = happyShift action_13
action_76 (48) = happyShift action_14
action_76 (61) = happyShift action_15
action_76 (62) = happyShift action_16
action_76 (63) = happyShift action_17
action_76 (64) = happyShift action_18
action_76 (4) = happyGoto action_90
action_76 (8) = happyGoto action_4
action_76 (14) = happyGoto action_5
action_76 (16) = happyGoto action_6
action_76 (18) = happyGoto action_7
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (21) = happyShift action_8
action_77 (22) = happyShift action_9
action_77 (23) = happyShift action_10
action_77 (43) = happyShift action_11
action_77 (44) = happyShift action_12
action_77 (46) = happyShift action_13
action_77 (48) = happyShift action_14
action_77 (61) = happyShift action_15
action_77 (62) = happyShift action_16
action_77 (63) = happyShift action_17
action_77 (64) = happyShift action_18
action_77 (8) = happyGoto action_4
action_77 (14) = happyGoto action_5
action_77 (16) = happyGoto action_89
action_77 (18) = happyGoto action_7
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (27) = happyShift action_87
action_78 (40) = happyReduce_24
action_78 (60) = happyShift action_88
action_78 (15) = happyGoto action_86
action_78 _ = happyReduce_56

action_79 (22) = happyShift action_30
action_79 (28) = happyShift action_85
action_79 (49) = happyShift action_31
action_79 (50) = happyShift action_32
action_79 (51) = happyShift action_33
action_79 (52) = happyShift action_34
action_79 (53) = happyShift action_35
action_79 (54) = happyShift action_36
action_79 (55) = happyShift action_37
action_79 (56) = happyShift action_38
action_79 (57) = happyShift action_39
action_79 (58) = happyShift action_40
action_79 (59) = happyShift action_41
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (22) = happyShift action_30
action_80 (29) = happyShift action_84
action_80 (49) = happyShift action_31
action_80 (50) = happyShift action_32
action_80 (51) = happyShift action_33
action_80 (52) = happyShift action_34
action_80 (53) = happyShift action_35
action_80 (54) = happyShift action_36
action_80 (55) = happyShift action_37
action_80 (56) = happyShift action_38
action_80 (57) = happyShift action_39
action_80 (58) = happyShift action_40
action_80 (59) = happyShift action_41
action_80 _ = happyReduce_48

action_81 (24) = happyShift action_83
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_40

action_83 _ = happyReduce_39

action_84 (21) = happyShift action_8
action_84 (22) = happyShift action_9
action_84 (23) = happyShift action_10
action_84 (43) = happyShift action_11
action_84 (44) = happyShift action_12
action_84 (46) = happyShift action_13
action_84 (48) = happyShift action_14
action_84 (61) = happyShift action_15
action_84 (62) = happyShift action_16
action_84 (63) = happyShift action_17
action_84 (64) = happyShift action_18
action_84 (8) = happyGoto action_4
action_84 (14) = happyGoto action_5
action_84 (16) = happyGoto action_80
action_84 (17) = happyGoto action_117
action_84 (18) = happyGoto action_7
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (27) = happyShift action_87
action_85 (40) = happyReduce_24
action_85 (42) = happyShift action_116
action_85 (60) = happyShift action_88
action_85 (15) = happyGoto action_114
action_85 (19) = happyGoto action_115
action_85 _ = happyReduce_60

action_86 _ = happyReduce_22

action_87 (21) = happyShift action_8
action_87 (22) = happyShift action_9
action_87 (23) = happyShift action_10
action_87 (43) = happyShift action_11
action_87 (44) = happyShift action_12
action_87 (46) = happyShift action_13
action_87 (48) = happyShift action_14
action_87 (61) = happyShift action_15
action_87 (62) = happyShift action_16
action_87 (63) = happyShift action_17
action_87 (64) = happyShift action_18
action_87 (8) = happyGoto action_4
action_87 (14) = happyGoto action_5
action_87 (16) = happyGoto action_113
action_87 (18) = happyGoto action_7
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (61) = happyShift action_112
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (22) = happyShift action_30
action_89 (45) = happyShift action_111
action_89 (49) = happyShift action_31
action_89 (50) = happyShift action_32
action_89 (51) = happyShift action_33
action_89 (52) = happyShift action_34
action_89 (53) = happyShift action_35
action_89 (54) = happyShift action_36
action_89 (55) = happyShift action_37
action_89 (56) = happyShift action_38
action_89 (57) = happyShift action_39
action_89 (58) = happyShift action_40
action_89 (59) = happyShift action_41
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_43

action_91 _ = happyReduce_20

action_92 (34) = happyShift action_110
action_92 _ = happyReduce_45

action_93 (21) = happyShift action_8
action_93 (22) = happyShift action_9
action_93 (23) = happyShift action_10
action_93 (43) = happyShift action_11
action_93 (44) = happyShift action_12
action_93 (46) = happyShift action_13
action_93 (48) = happyShift action_14
action_93 (61) = happyShift action_15
action_93 (62) = happyShift action_16
action_93 (63) = happyShift action_17
action_93 (64) = happyShift action_18
action_93 (8) = happyGoto action_4
action_93 (14) = happyGoto action_5
action_93 (16) = happyGoto action_109
action_93 (18) = happyGoto action_7
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_55

action_95 (39) = happyShift action_108
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (25) = happyShift action_106
action_96 (41) = happyShift action_107
action_96 (61) = happyShift action_55
action_96 (8) = happyGoto action_104
action_96 (9) = happyGoto action_105
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (61) = happyShift action_103
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (21) = happyShift action_8
action_98 (22) = happyShift action_9
action_98 (23) = happyShift action_10
action_98 (37) = happyShift action_2
action_98 (43) = happyShift action_11
action_98 (44) = happyShift action_12
action_98 (46) = happyShift action_13
action_98 (48) = happyShift action_14
action_98 (61) = happyShift action_15
action_98 (62) = happyShift action_16
action_98 (63) = happyShift action_17
action_98 (64) = happyShift action_18
action_98 (4) = happyGoto action_102
action_98 (8) = happyGoto action_4
action_98 (14) = happyGoto action_5
action_98 (16) = happyGoto action_6
action_98 (18) = happyGoto action_7
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (61) = happyShift action_101
action_99 (10) = happyGoto action_100
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (24) = happyShift action_127
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (30) = happyShift action_126
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_15

action_103 (40) = happyShift action_125
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_10

action_105 _ = happyReduce_8

action_106 (61) = happyShift action_101
action_106 (10) = happyGoto action_124
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (42) = happyShift action_123
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_1

action_109 (22) = happyShift action_30
action_109 (29) = happyShift action_122
action_109 (49) = happyShift action_31
action_109 (50) = happyShift action_32
action_109 (51) = happyShift action_33
action_109 (52) = happyShift action_34
action_109 (53) = happyShift action_35
action_109 (54) = happyShift action_36
action_109 (55) = happyShift action_37
action_109 (56) = happyShift action_38
action_109 (57) = happyShift action_39
action_109 (58) = happyShift action_40
action_109 (59) = happyShift action_41
action_109 _ = happyReduce_61

action_110 (21) = happyShift action_8
action_110 (22) = happyShift action_9
action_110 (23) = happyShift action_10
action_110 (37) = happyShift action_2
action_110 (43) = happyShift action_11
action_110 (44) = happyShift action_12
action_110 (46) = happyShift action_13
action_110 (48) = happyShift action_14
action_110 (61) = happyShift action_15
action_110 (62) = happyShift action_16
action_110 (63) = happyShift action_17
action_110 (64) = happyShift action_18
action_110 (4) = happyGoto action_121
action_110 (8) = happyGoto action_4
action_110 (14) = happyGoto action_5
action_110 (16) = happyGoto action_6
action_110 (18) = happyGoto action_7
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (21) = happyShift action_8
action_111 (22) = happyShift action_9
action_111 (23) = happyShift action_10
action_111 (43) = happyShift action_11
action_111 (44) = happyShift action_12
action_111 (46) = happyShift action_13
action_111 (48) = happyShift action_14
action_111 (61) = happyShift action_15
action_111 (62) = happyShift action_16
action_111 (63) = happyShift action_17
action_111 (64) = happyShift action_18
action_111 (8) = happyGoto action_4
action_111 (14) = happyGoto action_5
action_111 (16) = happyGoto action_120
action_111 (18) = happyGoto action_7
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (27) = happyShift action_87
action_112 (60) = happyShift action_88
action_112 (15) = happyGoto action_86
action_112 _ = happyReduce_24

action_113 (22) = happyShift action_30
action_113 (28) = happyShift action_119
action_113 (49) = happyShift action_31
action_113 (50) = happyShift action_32
action_113 (51) = happyShift action_33
action_113 (52) = happyShift action_34
action_113 (53) = happyShift action_35
action_113 (54) = happyShift action_36
action_113 (55) = happyShift action_37
action_113 (56) = happyShift action_38
action_113 (57) = happyShift action_39
action_113 (58) = happyShift action_40
action_113 (59) = happyShift action_41
action_113 _ = happyFail (happyExpListPerState 113)

action_114 _ = happyReduce_23

action_115 _ = happyReduce_57

action_116 (21) = happyShift action_8
action_116 (22) = happyShift action_9
action_116 (23) = happyShift action_10
action_116 (43) = happyShift action_11
action_116 (44) = happyShift action_12
action_116 (46) = happyShift action_13
action_116 (48) = happyShift action_14
action_116 (61) = happyShift action_15
action_116 (62) = happyShift action_16
action_116 (63) = happyShift action_17
action_116 (64) = happyShift action_18
action_116 (8) = happyGoto action_4
action_116 (14) = happyGoto action_5
action_116 (16) = happyGoto action_118
action_116 (18) = happyGoto action_7
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_49

action_118 _ = happyReduce_59

action_119 (27) = happyShift action_87
action_119 (60) = happyShift action_88
action_119 (15) = happyGoto action_114
action_119 _ = happyReduce_24

action_120 (22) = happyShift action_30
action_120 (47) = happyShift action_135
action_120 (49) = happyShift action_31
action_120 (50) = happyShift action_32
action_120 (51) = happyShift action_33
action_120 (52) = happyShift action_34
action_120 (53) = happyShift action_35
action_120 (54) = happyShift action_36
action_120 (55) = happyShift action_37
action_120 (56) = happyShift action_38
action_120 (57) = happyShift action_39
action_120 (58) = happyShift action_40
action_120 (59) = happyShift action_41
action_120 _ = happyFail (happyExpListPerState 120)

action_121 _ = happyReduce_46

action_122 (61) = happyShift action_59
action_122 (20) = happyGoto action_134
action_122 _ = happyReduce_63

action_123 (61) = happyShift action_55
action_123 (8) = happyGoto action_133
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (26) = happyShift action_132
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (21) = happyShift action_8
action_125 (22) = happyShift action_9
action_125 (23) = happyShift action_10
action_125 (37) = happyShift action_2
action_125 (43) = happyShift action_11
action_125 (44) = happyShift action_12
action_125 (46) = happyShift action_13
action_125 (48) = happyShift action_14
action_125 (61) = happyShift action_15
action_125 (62) = happyShift action_16
action_125 (63) = happyShift action_17
action_125 (64) = happyShift action_18
action_125 (4) = happyGoto action_131
action_125 (8) = happyGoto action_4
action_125 (14) = happyGoto action_5
action_125 (16) = happyGoto action_6
action_125 (18) = happyGoto action_7
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (61) = happyShift action_55
action_126 (8) = happyGoto action_130
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (30) = happyShift action_128
action_127 (52) = happyShift action_129
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (61) = happyShift action_55
action_128 (8) = happyGoto action_139
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (21) = happyShift action_8
action_129 (22) = happyShift action_9
action_129 (23) = happyShift action_10
action_129 (43) = happyShift action_11
action_129 (44) = happyShift action_12
action_129 (46) = happyShift action_13
action_129 (48) = happyShift action_14
action_129 (61) = happyShift action_15
action_129 (62) = happyShift action_16
action_129 (63) = happyShift action_17
action_129 (64) = happyShift action_18
action_129 (8) = happyGoto action_4
action_129 (14) = happyGoto action_5
action_129 (16) = happyGoto action_138
action_129 (18) = happyGoto action_7
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (29) = happyShift action_137
action_130 _ = happyReduce_14

action_131 _ = happyReduce_16

action_132 _ = happyReduce_12

action_133 _ = happyReduce_11

action_134 _ = happyReduce_62

action_135 (21) = happyShift action_8
action_135 (22) = happyShift action_9
action_135 (23) = happyShift action_10
action_135 (37) = happyShift action_2
action_135 (43) = happyShift action_11
action_135 (44) = happyShift action_12
action_135 (46) = happyShift action_13
action_135 (48) = happyShift action_14
action_135 (61) = happyShift action_15
action_135 (62) = happyShift action_16
action_135 (63) = happyShift action_17
action_135 (64) = happyShift action_18
action_135 (4) = happyGoto action_136
action_135 (8) = happyGoto action_4
action_135 (14) = happyGoto action_5
action_135 (16) = happyGoto action_6
action_135 (18) = happyGoto action_7
action_135 _ = happyFail (happyExpListPerState 135)

action_136 _ = happyReduce_44

action_137 (61) = happyShift action_101
action_137 (10) = happyGoto action_141
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (22) = happyShift action_30
action_138 (49) = happyShift action_31
action_138 (50) = happyShift action_32
action_138 (51) = happyShift action_33
action_138 (52) = happyShift action_34
action_138 (53) = happyShift action_35
action_138 (54) = happyShift action_36
action_138 (55) = happyShift action_37
action_138 (56) = happyShift action_38
action_138 (57) = happyShift action_39
action_138 (58) = happyShift action_40
action_138 (59) = happyShift action_41
action_138 _ = happyReduce_17

action_139 (52) = happyShift action_140
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (21) = happyShift action_8
action_140 (22) = happyShift action_9
action_140 (23) = happyShift action_10
action_140 (43) = happyShift action_11
action_140 (44) = happyShift action_12
action_140 (46) = happyShift action_13
action_140 (48) = happyShift action_14
action_140 (61) = happyShift action_15
action_140 (62) = happyShift action_16
action_140 (63) = happyShift action_17
action_140 (64) = happyShift action_18
action_140 (8) = happyGoto action_4
action_140 (14) = happyGoto action_5
action_140 (16) = happyGoto action_142
action_140 (18) = happyGoto action_7
action_140 _ = happyFail (happyExpListPerState 140)

action_141 _ = happyReduce_13

action_142 (22) = happyShift action_30
action_142 (49) = happyShift action_31
action_142 (50) = happyShift action_32
action_142 (51) = happyShift action_33
action_142 (52) = happyShift action_34
action_142 (53) = happyShift action_35
action_142 (54) = happyShift action_36
action_142 (55) = happyShift action_37
action_142 (56) = happyShift action_38
action_142 (57) = happyShift action_39
action_142 (58) = happyShift action_40
action_142 (59) = happyShift action_41
action_142 _ = happyReduce_18

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (A.LetExp (snd happy_var_2) happy_var_4 (fst happy_var_1)
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 _
	_
	 =  HappyAbsSyn5
		 ((error "0")
	)

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn5
		 ((error "1")
	)

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn6
		 ((error "2")
	)

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn6
		 ((error "3")
	)

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn6
		 ((error "4")
	)

happyReduce_8 = happyReduce 4 7 happyReduction_8
happyReduction_8 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((error "5")
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn8
		 ((error "6")
	)

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn9
		 ((error "7")
	)

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 _
	_
	_
	 =  HappyAbsSyn9
		 ((error "8")
	)

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 _
	_
	_
	 =  HappyAbsSyn9
		 ((error "9")
	)

happyReduce_13 = happyReduce 5 10 happyReduction_13
happyReduction_13 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ((error "10")
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_3  10 happyReduction_14
happyReduction_14 _
	_
	_
	 =  HappyAbsSyn10
		 ((error "11")
	)

happyReduce_15 = happyReduce 4 11 happyReduction_15
happyReduction_15 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((error "12")
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 11 happyReduction_16
happyReduction_16 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((error "13")
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 7 12 happyReduction_17
happyReduction_17 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ((error "14")
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 9 12 happyReduction_18
happyReduction_18 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ((error "15")
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn13
		 (A.SeqExp [(happy_var_1, stubPos)]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  13 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn13
		 (A.SeqExp ((\(A.SeqExp xs) -> (happy_var_1, fst happy_var_2):xs) happy_var_3)
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  14 happyReduction_21
happyReduction_21 _
	(HappyTerminal ((_, Id happy_var_1)))
	 =  HappyAbsSyn14
		 (A.SimpleVar happy_var_1 stubPos
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  15 happyReduction_22
happyReduction_22 _
	_
	_
	 =  HappyAbsSyn15
		 ((error "18")
	)

happyReduce_23 = happyReduce 4 15 happyReduction_23
happyReduction_23 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 ((error "19")
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_0  15 happyReduction_24
happyReduction_24  =  HappyAbsSyn15
		 (A.NilExp
	)

happyReduce_25 = happySpecReduce_3  16 happyReduction_25
happyReduction_25 (HappyAbsSyn16  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (A.OpExp happy_var_1 A.Plus happy_var_3 (fst happy_var_2)
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  16 happyReduction_26
happyReduction_26 _
	_
	_
	 =  HappyAbsSyn16
		 ((error "20")
	)

happyReduce_27 = happySpecReduce_3  16 happyReduction_27
happyReduction_27 _
	_
	_
	 =  HappyAbsSyn16
		 ((error "21")
	)

happyReduce_28 = happySpecReduce_3  16 happyReduction_28
happyReduction_28 _
	_
	_
	 =  HappyAbsSyn16
		 ((error "22")
	)

happyReduce_29 = happySpecReduce_3  16 happyReduction_29
happyReduction_29 _
	_
	_
	 =  HappyAbsSyn16
		 ((error "23")
	)

happyReduce_30 = happySpecReduce_3  16 happyReduction_30
happyReduction_30 _
	_
	_
	 =  HappyAbsSyn16
		 ((error "24")
	)

happyReduce_31 = happySpecReduce_3  16 happyReduction_31
happyReduction_31 _
	_
	_
	 =  HappyAbsSyn16
		 ((error "25")
	)

happyReduce_32 = happySpecReduce_3  16 happyReduction_32
happyReduction_32 _
	_
	_
	 =  HappyAbsSyn16
		 ((error "26")
	)

happyReduce_33 = happySpecReduce_3  16 happyReduction_33
happyReduction_33 _
	_
	_
	 =  HappyAbsSyn16
		 ((error "27")
	)

happyReduce_34 = happySpecReduce_3  16 happyReduction_34
happyReduction_34 _
	_
	_
	 =  HappyAbsSyn16
		 ((error "28")
	)

happyReduce_35 = happySpecReduce_3  16 happyReduction_35
happyReduction_35 _
	_
	_
	 =  HappyAbsSyn16
		 ((error "29")
	)

happyReduce_36 = happySpecReduce_3  16 happyReduction_36
happyReduction_36 _
	_
	_
	 =  HappyAbsSyn16
		 ((error "30")
	)

happyReduce_37 = happySpecReduce_3  16 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn16
		 (A.AssignExp happy_var_1 happy_var_3 (fst happy_var_2)
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  16 happyReduction_38
happyReduction_38 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 16 happyReduction_39
happyReduction_39 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((error "32")
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_3  16 happyReduction_40
happyReduction_40 _
	_
	_
	 =  HappyAbsSyn16
		 ((error "33")
	)

happyReduce_41 = happySpecReduce_3  16 happyReduction_41
happyReduction_41 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  16 happyReduction_42
happyReduction_42 _
	_
	 =  HappyAbsSyn16
		 ((error "34")
	)

happyReduce_43 = happyReduce 4 16 happyReduction_43
happyReduction_43 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((error "35")
	) `HappyStk` happyRest

happyReduce_44 = happyReduce 8 16 happyReduction_44
happyReduction_44 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((error "36")
	) `HappyStk` happyRest

happyReduce_45 = happyReduce 4 16 happyReduction_45
happyReduction_45 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((error "37")
	) `HappyStk` happyRest

happyReduce_46 = happyReduce 6 16 happyReduction_46
happyReduction_46 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((error "38")
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_1  16 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn16
		 ((error "39")
	)

happyReduce_48 = happySpecReduce_1  17 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn17
		 ((error "40")
	)

happyReduce_49 = happySpecReduce_3  17 happyReduction_49
happyReduction_49 _
	_
	_
	 =  HappyAbsSyn17
		 ((error "41")
	)

happyReduce_50 = happySpecReduce_1  18 happyReduction_50
happyReduction_50 (HappyTerminal ((_, Num happy_var_1)))
	 =  HappyAbsSyn18
		 (A.IntExp happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  18 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn18
		 ((error "43")
	)

happyReduce_52 = happySpecReduce_1  18 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn18
		 ((error "44")
	)

happyReduce_53 = happySpecReduce_1  18 happyReduction_53
happyReduction_53 (HappyTerminal ((_, Id happy_var_1)))
	 =  HappyAbsSyn18
		 (A.VarExp (A.SimpleVar happy_var_1 stubPos)
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  18 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn18
		 ((error "46")
	)

happyReduce_55 = happyReduce 4 18 happyReduction_55
happyReduction_55 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((error "47")
	) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_3  18 happyReduction_56
happyReduction_56 _
	_
	_
	 =  HappyAbsSyn18
		 ((error "48")
	)

happyReduce_57 = happyReduce 5 18 happyReduction_57
happyReduction_57 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((error "49")
	) `HappyStk` happyRest

happyReduce_58 = happySpecReduce_2  18 happyReduction_58
happyReduction_58 _
	_
	 =  HappyAbsSyn18
		 ((error "50")
	)

happyReduce_59 = happySpecReduce_2  19 happyReduction_59
happyReduction_59 _
	_
	 =  HappyAbsSyn19
		 ((error "51")
	)

happyReduce_60 = happySpecReduce_0  19 happyReduction_60
happyReduction_60  =  HappyAbsSyn19
		 ((error "52")
	)

happyReduce_61 = happySpecReduce_3  20 happyReduction_61
happyReduction_61 _
	_
	_
	 =  HappyAbsSyn20
		 ((error "53")
	)

happyReduce_62 = happyReduce 5 20 happyReduction_62
happyReduction_62 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 ((error "54")
	) `HappyStk` happyRest

happyReduce_63 = happySpecReduce_0  20 happyReduction_63
happyReduction_63  =  HappyAbsSyn20
		 ((error "55")
	)

happyNewToken action sts stk [] =
	action 65 65 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	(_, If) -> cont 21;
	(_, Negation) -> cont 22;
	(_, LParen) -> cont 23;
	(_, RParen) -> cont 24;
	(_, LBrace) -> cont 25;
	(_, RBrace) -> cont 26;
	(_, LBracket) -> cont 27;
	(_, RBracket) -> cont 28;
	(_, Comma) -> cont 29;
	(_, Colon) -> cont 30;
	(_, Semicolon) -> cont 31;
	(_, Type) -> cont 32;
	(_, Then) -> cont 33;
	(_, Else) -> cont 34;
	(_, Var) -> cont 35;
	(_, Function) -> cont 36;
	(_, Let) -> cont 37;
	(_, In) -> cont 38;
	(_, End) -> cont 39;
	(_, Bind) -> cont 40;
	(_, Array) -> cont 41;
	(_, Of) -> cont 42;
	(_, While) -> cont 43;
	(_, For) -> cont 44;
	(_, To) -> cont 45;
	(_, Break) -> cont 46;
	(_, Do) -> cont 47;
	(_, Nil) -> cont 48;
	(_, Plus) -> cont 49;
	(_, Times) -> cont 50;
	(_, Div) -> cont 51;
	(_, Eq) -> cont 52;
	(_, NotEq) -> cont 53;
	(_, LT) -> cont 54;
	(_, LTE) -> cont 55;
	(_, GT) -> cont 56;
	(_, GTE) -> cont 57;
	(_, And) -> cont 58;
	(_, Or) -> cont 59;
	(_, Dot) -> cont 60;
	(_, Id happy_dollar_dollar) -> cont 61;
	(_, Num happy_dollar_dollar) -> cont 62;
	(_, Real happy_dollar_dollar) -> cont 63;
	(_, StringL happy_dollar_dollar) -> cont 64;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 65 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(TigToken)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
calc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


stubPos = AlexPn 0 0 0

parseError :: [TigToken] -> a
parseError xs = error $ "Parse error: " ++ show xs

main = getContents >>= pPrint . calc . lexer

-- 意味動作なしで構文解析だけ作るの難しくないか？
-- -> -iと-a -dオプション
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
