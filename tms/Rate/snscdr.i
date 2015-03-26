/* remove a-number prefix  */


 ASSIGN
 pos1  =     TRIM(ENTRY( 1,callrec,lcSep)).
 pos2  = INT(TRIM(ENTRY( 2,callrec,lcSep))).
 pos3  =     TRIM(ENTRY( 3,callrec,lcSep)).
 pos4  = INT(TRIM(ENTRY( 4,callrec,lcSep))).
 pos5  = INT(TRIM(ENTRY( 5,callrec,lcSep))).
 pos6  =  INT(TRIM(ENTRY( 6,callrec,lcSep))).
 pos7  =  IF(TRIM(ENTRY( 7,callrec,lcSep))) =  "1" THEN TRUE ELSE FALSE.
 pos8  = TRIM(ENTRY( 8,callrec,lcSep)).
 pos9  =     TRIM(ENTRY( 9,callrec,lcSep)).
 pos10 =     TRIM(ENTRY(10,callrec,lcSep)).
 pos11 =     TRIM(ENTRY(11,callrec,lcSep)).
 pos12 =     TRIM(ENTRY(12,callrec,lcSep)).
 pos13 =     TRIM(ENTRY(13,callrec,lcSep)).
 pos14 =     TRIM(ENTRY(14 ,callrec,lcSep)).
 pos15 =     TRIM(ENTRY(15 ,callrec,lcSep)).
 pos16 =     TRIM(ENTRY(16 ,callrec,lcSep)).
 pos17 =     TRIM(ENTRY(17 ,callrec,lcSep)).
 pos18 =     TRIM(ENTRY(18, callrec,lcSep)).
 pos19 =     TRIM(ENTRY(19 ,callrec,lcSep)).
 pos20 =     TRIM(ENTRY(20,callrec,lcSep)).
 pos21 =     TRIM(ENTRY(21,callrec,lcSep)).
 pos22 =     TRIM(ENTRY(22,callrec,lcSep)).
 pos23 =     TRIM(ENTRY(23,callrec,lcSep)).
 pos24 =     TRIM(ENTRY(24,callrec,lcSep)).
 pos25 = DEC(TRIM(ENTRY(25,callrec,lcSep))).
 pos26 = DEC(TRIM(ENTRY(26,callrec,lcSep))).
 pos27 = DEC(TRIM(ENTRY(27,callrec,lcSep))).
 pos28 =     TRIM(ENTRY(28,callrec,lcSep)).
 pos29 = DEC(TRIM(ENTRY(29,callrec,lcSep))).
 pos30 = DEC(TRIM(ENTRY(30,callrec,lcSep))).
 pos31 = DEC(TRIM(ENTRY(31,callrec,lcSep))).
 pos32 =     TRIM(ENTRY(32,callrec,lcSep)).
 pos33 =     TRIM(ENTRY(33,callrec,lcSep)).
 pos34 =  INT(TRIM(ENTRY(34,callrec,lcSep))).
 pos35 =  INT(TRIM(ENTRY(35,callrec,lcSep))).
 pos36 =     TRIM(ENTRY(36,callrec,lcSep)).
 pos37 =     TRIM(ENTRY(37,callrec,lcSep)).
 pos38 = INT(TRIM(ENTRY(38,callrec,lcSep))).
 pos39 =     TRIM(ENTRY(39 ,callrec,lcSep)).
 pos40 =     TRIM(ENTRY(40,callrec,lcSep)).
 pos41 = INT(TRIM(ENTRY(41 ,callrec,lcSep))).
 pos42 = INT(TRIM(ENTRY(42,callrec,lcSep))).
 pos43 =     TRIM(ENTRY(43,callrec,lcSep)).
 pos44 =     TRIM(ENTRY(44,callrec,lcSep)).
 pos45 =     TRIM(ENTRY(45,callrec,lcSep)).
 pos46 =     TRIM(ENTRY(46,callrec,lcSep)).
 pos47 = IF(TRIM(ENTRY( 47,callrec,lcSep))) =  "1" THEN TRUE ELSE FALSE.
 pos48 =    TRIM(ENTRY(48,callrec,lcSep)).
 pos49 = IF(TRIM(ENTRY( 49,callrec,lcSep))) =  "1" THEN TRUE ELSE FALSE.
 pos50 =    TRIM(ENTRY(50,callrec,lcSep)).
 pos51 =    TRIM(ENTRY(51,callrec,lcSep)).
 pos52 = TRIM(ENTRY(52,callrec,lcSep)).
 pos53 =    TRIM(ENTRY(53,callrec,lcSep)).
 pos54 =    TRIM(ENTRY(54,callrec,lcSep)).
 pos55 = INT(TRIM(ENTRY(55,callrec,lcSep))).
 pos56 = INT(TRIM(ENTRY(56,callrec,lcSep))).
 pos57 = INT(TRIM(ENTRY(57,callrec,lcSep))).
 pos58 = INT(TRIM(ENTRY(58,callrec,lcSep))).
 pos59 = INT(TRIM(ENTRY(59,callrec,lcSep))).
 pos60 = TRIM(ENTRY(60,callrec,lcSep)).
 pos61 = TRIM(ENTRY(61,callrec,lcSep)).
 pos62 = INT(TRIM(ENTRY(62,callrec,lcSep))).
 pos63 = IF(TRIM(ENTRY( 63,callrec,lcSep))) =  "0" THEN FALSe ELSE TRUE.
 pos64 = TRIM(ENTRY(64,callrec,lcSep)).
 pos65 = IF(TRIM(ENTRY( 65,callrec,lcSep))) =  "0" THEN FALSe ELSE TRUE.
 pos66 = TRIM(ENTRY(66,callrec,lcSep)). 
 pos67 = TRIM(ENTRY(67,callrec,lcSep)).
 pos68 = TRIM(ENTRY(68,callrec,lcSep)).
 pos69 = TRIM(ENTRY(69,callrec,lcSep)).
 pos70 = TRIM(ENTRY(70,callrec,lcSep)).
 pos71 = TRIM(ENTRY(71,callrec,lcSep)).
 pos72 = TRIM(ENTRY(72,callrec,lcSep)).
 pos73 = TRIM(ENTRY(73,callrec,lcSep)).
 pos74 = TRIM(ENTRY(74,callrec,lcSep)).
 pos75 = TRIM(ENTRY(75,callrec,lcSep)).
 pos76 = TRIM(ENTRY(76,callrec,lcSep)).
 pos77 = TRIM(ENTRY(77,callrec,lcSep)).
 pos78 = TRIM(ENTRY(78,callrec,lcSep)).
 pos79 = TRIM(ENTRY(79,callrec,lcSep)).
 pos80 = TRIM(ENTRY(80,callrec,lcSep)).
 pos81 = TRIM(ENTRY(81,callrec,lcSep)).
 pos82 = TRIM(ENTRY(82,callrec,lcSep)).
 pos83 = TRIM(ENTRY(83,callrec,lcSep)).
 pos84 = TRIM(ENTRY(84,callrec,lcSep)).
 pos85 = TRIM(ENTRY(85,callrec,lcSep)).
 pos86 = TRIM(ENTRY(86,callrec,lcSep)).
 pos87 = TRIM(ENTRY(87,callrec,lcSep)).
 pos88 = TRIM(ENTRY(88,callrec,lcSep)).
 pos89 = TRIM(ENTRY(89,callrec,lcSep)).
 pos90 = TRIM(ENTRY(90,callrec,lcSep)).
 pos91 = TRIM(ENTRY(91,callrec,lcSep)).
 pos92 = TRIM(ENTRY(92,callrec,lcSep)).
 pos93 = TRIM(ENTRY(93,callrec,lcSep)).
 pos94 = TRIM(ENTRY(94,callrec,lcSep)).
 pos95 = TRIM(ENTRY(95,callrec,lcSep)).
 pos96 = TRIM(ENTRY(96,callrec,lcSep)).
 pos97 = TRIM(ENTRY(97,callrec,lcSep)).
 pos98 = TRIM(ENTRY(98,callrec,lcSep)). 

 
 
 
