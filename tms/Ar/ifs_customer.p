/* ----------------------------------------------------------------------
  MODULE .......: ifs_customer.p
  TASK .........: Create a dump file for IFS from customers
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 28.05.09 (using sap_business_partner)
  Version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
gcBrand = "1".

{commali.i}
{cparam2.i}
{dumpfile_run.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEFINE VARIABLE AKTYP       AS CHARACTER NO-UNDO. /*  Activity category  */
DEFINE VARIABLE RLTP1       AS CHARACTER NO-UNDO. /*  BDT: Object part  */
DEFINE VARIABLE PARTNER     AS INTEGER   NO-UNDO. /*  BP number  */
DEFINE VARIABLE VALDT       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE TYPE        AS CHARACTER NO-UNDO. /*  BP category  */
DEFINE VARIABLE BPKIND      AS CHARACTER NO-UNDO. /*  BP type  */
DEFINE VARIABLE BU_GROUP    AS CHARACTER NO-UNDO. /*  BP grouping  */
DEFINE VARIABLE BPEXT       AS CHARACTER NO-UNDO. /*  BP number in ext.syst */
DEFINE VARIABLE FIBUKRS     AS CHARACTER NO-UNDO. /*  Direct Input Company 
                                                      Code in Initial Screen */
DEFINE VARIABLE BU_SORT1    AS CHARACTER NO-UNDO. /*  Search term 1 */
DEFINE VARIABLE BU_SORT2    AS CHARACTER NO-UNDO. /*  Search term 2 */
DEFINE VARIABLE SOURCE      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcTITLE     AS CHARACTER NO-UNDO. /*  Title key  */
DEFINE VARIABLE NAME_ORG1   AS CHARACTER NO-UNDO. /*  Name 1 of organization */
DEFINE VARIABLE NAME_ORG2   AS CHARACTER NO-UNDO. /*  Name 2 of organization */
DEFINE VARIABLE NAME_ORG3   AS CHARACTER NO-UNDO. /*  Name 3 of organization */
DEFINE VARIABLE NAME_ORG4   AS CHARACTER NO-UNDO. /*  Name 4 of organization */
DEFINE VARIABLE NAME_LAST   AS CHARACTER NO-UNDO. /*  Last name */
DEFINE VARIABLE NAME_FIRST  AS CHARACTER NO-UNDO. /*  First name */
DEFINE VARIABLE NAME_LST2   AS CHARACTER NO-UNDO. /*  Other last name */
DEFINE VARIABLE NAME_LAST2  AS CHARACTER NO-UNDO. /*  Name at birth */
DEFINE VARIABLE NATIO       AS CHARACTER NO-UNDO. /*  Nationality */
DEFINE VARIABLE LGTYP       AS CHARACTER NO-UNDO. /*  Legitimation type  */
DEFINE VARIABLE LGNUM       AS CHARACTER NO-UNDO. /*  Id of legitimation doc */
DEFINE VARIABLE ADEXT_ADDR  AS CHARACTER NO-UNDO. /*  Address number */
DEFINE VARIABLE CHIND_ADDR  AS CHARACTER NO-UNDO. /*  Type of change */
DEFINE VARIABLE XDFADR      AS CHARACTER NO-UNDO. /*  Indicator: Address is 
                                                      standard address  */
DEFINE VARIABLE POST_CODE1  AS CHARACTER NO-UNDO. /*  City postal code  */
DEFINE VARIABLE CITY1       AS CHARACTER NO-UNDO. /*  Post office */
DEFINE VARIABLE STREET      AS CHARACTER NO-UNDO. /*  Street  */
DEFINE VARIABLE HOUSE_NUM1  AS CHARACTER NO-UNDO. /*  House number  */
DEFINE VARIABLE COUNTRY     AS CHARACTER NO-UNDO. /*  Country key  */
DEFINE VARIABLE LANGU       AS CHARACTER NO-UNDO. /*  Language key  */
DEFINE VARIABLE REGION      AS CHARACTER NO-UNDO. /*  Region (State, Province,
                                                      County)  */
DEFINE VARIABLE CHIND_TEL   AS CHARACTER NO-UNDO. /*  Type of change */
DEFINE VARIABLE TEL_NUMBER  AS CHARACTER NO-UNDO. /*  dialling code+number */
DEFINE VARIABLE CHIND_FAX   AS CHARACTER NO-UNDO. /*  Type of change */
DEFINE VARIABLE CHIND_TTX   AS CHARACTER NO-UNDO. /*  Type of change */
DEFINE VARIABLE CHIND_TLX   AS CHARACTER NO-UNDO. /*  Type of change */
DEFINE VARIABLE CHIND_SMTP  AS CHARACTER NO-UNDO. /*  Type of change */
DEFINE VARIABLE CHIND_BANK  AS CHARACTER NO-UNDO. /*  Type of change */
DEFINE VARIABLE SEPA        AS CHARACTER NO-UNDO. /*  SEPA code, example ES23 */
DEFINE VARIABLE BANKS       AS CHARACTER NO-UNDO. /*  Bank country key  */
DEFINE VARIABLE BANKL       AS CHARACTER NO-UNDO. /*  Bank key  */
DEFINE VARIABLE BANKN       AS CHARACTER NO-UNDO. /*  Bank account number  */
DEFINE VARIABLE BKONT       AS CHARACTER NO-UNDO. /*  Bank control key  */
DEFINE VARIABLE BKEXT       AS CHARACTER NO-UNDO. /*  Bank details ID */
DEFINE VARIABLE ZEZAWE      AS CHARACTER NO-UNDO. /* Incoming Payment Method */
DEFINE VARIABLE ZAZAWE      AS CHARACTER NO-UNDO. /* Outgoing Payment Methods */
DEFINE VARIABLE ZZSLCS      AS CHARACTER NO-UNDO. /* Contract Account Segment */
DEFINE VARIABLE TEL_NUMBER2 AS CHARACTER NO-UNDO. /* Second phone number */
DEFINE VARIABLE SMTP_ADDR   AS CHARACTER NO-UNDO. /* Email               */ 

DEF VAR ldaModified   AS DATE   NO-UNDO.
DEF VAR liCnt         AS INT    NO-UNDO.
DEF VAR lhTable       AS HANDLE NO-UNDO.
DEF VAR ldtLastDump   AS DATETIME NO-UNDO.
DEF VAR lcCustDenied  AS CHAR   NO-UNDO.
DEF VAR lcModFields   AS CHAR   NO-UNDO.
DEF VAR lcCustIDTypeDenied  AS CHAR   NO-UNDO.

DEFINE STREAM sLog.


/* ********************     MAIN     *******************************/

ASSIGN /* constant values */
   AKTYP         = "04"    /*  Activity category  04 = CREATE/MODIFY BP*/
   RLTP1         = "MKK"   /*  BDT: Object part  constant MKK */
   VALDT         = ""      /*  Validity data for changes (direct input)  */
   BU_GROUP      = "0002"  /*  BP grouping  */
   FIBUKRS       = "X001"  /*  Direct Input Company Code in Initial Screen  */
   BU_SORT2      = ""      /*  Search term 2 for BP  */
   SOURCE        = "0001"  /* Data origin */
   ADEXT_ADDR    = ""      /*  Address number in external system  */
   CHIND_ADDR    = "M"     /*  BP: Type of change during direct input  */
   XDFADR        = "X"     /*  Indicator: Address is standard address  */
   HOUSE_NUM1    = ""      /*  House number  */
   BANKS         = "ES"    /*  Bank country key  */
   CHIND_FAX     = ""      /*  BP: Type of change during direct input  */
   CHIND_TTX     = ""      /*  BP: Type of change during direct input  */
   CHIND_TLX     = ""      /*  BP: Type of change during direct input  */
   CHIND_SMTP    = ""      /*  BP: Type of change during direct input  */
   .
   
OUTPUT STREAM sLog TO VALUE(icFile).

fSplitTS(idLastDump,    
         OUTPUT ldaModified,
         OUTPUT liCnt).

ASSIGN
   ldtLastDump = fTimeStamp2DateTime(idLastDump)
   lhTable     = BUFFER CUSTOMER:HANDLE
   /* customers that are not transferred to sap */
   lcCustDenied = fCParamC("AgrCustNoTransfer")
   lcCustIDTypeDenied = fCParamC("CustIDTypeNoTransfer").


FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN lcModFields = DumpFile.EventLogFields.

FOR EACH Customer NO-LOCK WHERE
         Customer.Brand = gcBrand
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   /* make sure that ctrl-c doesn't quit */
   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   IF LOOKUP(STRING(Customer.CustNum),lcCustDenied) > 0 THEN NEXT. 
   
   IF LOOKUP(Customer.CustIdType,lcCustIDTypeDenied) > 0 THEN NEXT.
   
   /* dump only modified ones */
   IF icDumpMode = "modified" THEN DO:

      IF NOT fWasRecordModified(lhTable,
                                icEventSource,
                                icEventFields,
                                idLastDump,
                                ldaModified,
                                ldtLastDump,
                                lcModFields)
      THEN NEXT. 
   END.
 
   oiEvents = oiEvents + 1.
   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
       PAUSE 0.
       DISP oiEvents LABEL "Customers" 
       WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
          TITLE " Collecting " FRAME fQty.
   END.
       
   CASE Customer.CustIDType:
      WHEN "CIF" THEN ASSIGN
         TYPE       = "2"
         NAME_ORG1  = Customer.CompanyName /*  Name 1 of organization  */
         NAME_ORG2  = Customer.COName      /*  Name 2 of organization  */
         NAME_ORG3  = ""                   /*  Name 3 of organization  */
         NAME_ORG4  = "".                  /*  Name 4 of organization  */
      OTHERWISE ASSIGN
         TYPE       = "1"                  /*  BP category  */
         NAME_LAST  = Customer.CustName    /* Last name of bp (person)  */
         NAME_FIRST = Customer.FirstName   /* First name of bp (person)  */
         NAME_LST2  = Customer.SurName2    /* Other last name of a person  */
         NAME_LAST2 = "".                  /* Name at birth of BP  */
   END CASE.

   /* custcat 31 VIP -> 0002, 30 EMPLOY -> 0004, GENERAL -> 0001 */
   CASE Customer.category:
      WHEN "31" THEN BPKIND = "0002". 
      WHEN "30" THEN BPKIND = "0004". 
      OTHERWISE BPKIND = "0001".   /*  BP type  */
   END CASE.

   IF TYPE = "1" THEN DO:
      IF Customer.hontitle = "Sr." THEN 
         lcTITLE = "0001". 
      ELSE IF Customer.hontitle = "Sra." THEN 
         lcTITLE = "0002".
   END.      
   ELSE lcTITLE = "0003". /*  Title key  */
   
   /*  Legitimation type  */
   CASE Customer.CustIdType:
   WHEN "NIF"      THEN LGTYP = "01". 
   WHEN "CIF"      THEN LGTYP = "02".
   WHEN "Passport" THEN LGTYP = "03".
   WHEN "NIE"      THEN LGTYP = "06".
   OTHERWISE LGTYP = "01".
   END CASE. 

   /* language */
   CASE Customer.Language:
   WHEN 1 THEN LANGU = "S".
   OTHERWISE LANGU = "S".
   END CASE. 
   
   ASSIGN
      BPEXT       = STRING(Customer.CustNum)    /* nbr in external system */
      BU_SORT1    = Customer.OrgId              /* Search term 1 for BP  */
      LGNUM       = Customer.OrgID              /* Identification nbr   */
      PARTNER     = Customer.CustNum            /* BP number  */
      POST_CODE1  = Customer.ZipCode            /* City postal code  */
      CITY1       = Customer.PostOffice
      STREET      = Customer.Address            /* Street  */
      REGION      = SUBSTRING(TRIM(Customer.ZipCode),1,2)  
      COUNTRY     = IF Customer.Country > "" 
                    THEN Customer.Country
                    ELSE "ES"                   /* Country key  */
      TEL_NUMBER  = IF Customer.SMSNumber > ""
                    THEN Customer.SMSNumber
                    ELSE ""         /* Telephone no. */
      CHIND_BANK  = "M"
      SEPA        = SUBSTRING(Customer.BankAcct,1,4) /* SEPA key  */
      BANKL       = SUBSTRING(Customer.BankAcct,5,8) /* Bank key  */
      BKONT       = SUBSTRING(Customer.BankAcct,13,2) /* Bank control key  */
      BANKN       = SUBSTRING(Customer.BankAcct,15) /* Bank account number  */
      NATIO       = Customer.Nationality
      TEL_NUMBER2 = Customer.Phone
      SMTP_ADDR   = Customer.Email.
   
   IF TEL_NUMBER EQ "" THEN CHIND_TEL = "".
   ELSE CHIND_TEL = "M". /*  BP: Type of change during direct input  */

   /* fixed length for bank data */
   ASSIGN
      SEPA  = FILL("0",4 - LENGTH(SEPA)) + SEPA
      BANKL = FILL("0",8 - LENGTH(BANKL)) + BANKL
      BANKN = FILL("0",10 - LENGTH(BANKN)) + BANKN
      BKONT = FILL("0",2 - LENGTH(BKONT)) + BKONT
      BKEXT = SEPA + BANKL + BKONT + BANKN. 

   /* incoming payment method */
   CASE Customer.ChargeType:
   WHEN 2 THEN ZEZAWE = "1".  /* direct debit */
   WHEN 5 THEN ZEZAWE = "3".  /* Bank Transfer - No direct debit */
   WHEN 6 THEN ZEZAWE = "3".  /* Bank Transfer - Next invoice not direct debit */
   OTHERWISE ZEZAWE = "2".    /* cash, over the counter */
   END CASE. 

   /* outgoing payment method */
   ZAZAWE = "".

   ZZSLCS = "99".

   PUT STREAM sLog UNFORMATTED                                /* Positions */     
   /*   1 */ STRING(AKTYP,"X(2)")                             /*   1 -  2  */    
   /*   2 */ STRING(RLTP1,"X(6)")                             /*   3 -  8  */
   /*   3 */   /* RLTP2          - NOT USED */ FILL(" ",6)    /*   9 - 14  */
   /*   4 */   /* RLTP3          - NOT USED */ FILL(" ",6)    /*  15 - 20  */
   /*   5 */   /* RLTP4          - NOT USED */ FILL(" ",6)    /*  21 - 26  */
   /*   6 */   /* RLTP5          - NOT USED */ FILL(" ",6)    /*  27 - 32  */
   /*   7 */   /* RLTP6          - NOT USED */ FILL(" ",6)    /*  33 - 38  */
   /*   8 */   /* RLTP7          - NOT USED */ FILL(" ",6)    /*  39 - 44  */
   /*   9 */   /* RLTP8          - NOT USED */ FILL(" ",6)    /*  45 - 50  */
   /*  10 */   /* RLTP9          - NOT USED */ FILL(" ",6)    /*  51 - 56  */
   /*  11 */ STRING(PARTNER,"9999999999")                     /*  57 - 66  */                  
   /*  12 */ STRING(VALDT,"X(8)")                             /*  67 - 74  */  
   /*  13 */ STRING(TYPE,"X(1)")                              /*  75 - 75  */
   /*  14 */ STRING(BPKIND,"X(4)")                            /*  76 - 79  */
   /*  15 */ STRING(BU_GROUP,"X(4)")                          /*  80 - 83  */
   /*  16 */ STRING(BPEXT,"X(20)")                            /*  84 - 103 */
   /*  17 */ STRING(FIBUKRS,"X(4)")                           /* 104 - 107 */
   /*  18 */   /* MUSTER_KUN     - NOT USED */ FILL(" ",10)   /* 108 - 117 */
   /*  19 */ STRING(BU_SORT1,"X(20)")                         /* 118 - 137 */
   /*  20 */ STRING(BU_SORT2,"X(20)")                         /* 138 - 157 */
   /*  21 */ STRING(SOURCE,"X(4)")                            /* 158 - 161 */  
   /*  22 */ STRING(lcTITLE,"X(4)")                           /* 162 - 165 */
   /*  23 */   /* XDELE          - NOT USED */ FILL(" ",1)    /* 166 - 166 */
   /*  24 */   /* XBLCK          - NOT USED */ FILL(" ",1)    /* 167 - 167 */
   /*  25 */   /* AUGRP          - NOT USED */ FILL(" ",4)    /* 168 - 171 */
   /*  26 */   /* TITLE_LET      - NOT USED */ FILL(" ",50)   /* 172 - 221 */
   /*  27 */   /* BU_LOGSYS      - NOT USED */ FILL(" ",10)   /* 222 - 231 */
   /*  28 */   /* CONTACT        - NOT USED */ FILL(" ",1)    /* 232 - 232 */
   /*  29 */ STRING(NAME_ORG1,"X(40)")                        /* 233 - 272 */
   /*  30 */ STRING(NAME_ORG2,"X(40)")                        /* 273 - 312 */  
   /*  31 */ STRING(NAME_ORG3,"X(40)")                        /* 313 - 352 */  
   /*  32 */ STRING(NAME_ORG4,"X(40)")                        /* 353 - 392 */
   /*  33 */   /* LEGAL_ENTY     - NOT USED */ FILL(" ",2)    /* 393 - 394 */
   /*  34 */   /* IND_SECTOR     - NOT USED */ FILL(" ",10)   /* 395 - 404 */
   /*  35 */   /* LEGAL_ORG      - NOT USED */ FILL(" ",2)    /* 405 - 406 */
   /*  36 */   /* FOUND_DAT      - NOT USED */ FILL(" ",8)    /* 407 - 414 */
   /*  37 */   /* LIQUID_DAT     - NOT USED */ FILL(" ",8)    /* 415 - 422 */
   /*  38 */   /* LOCATION_1     - NOT USED */ FILL(" ",7)    /* 423 - 429 */
   /*  39 */   /* LOCATION_2     - NOT USED */ FILL(" ",5)    /* 430 - 434 */
   /*  40 */   /* LOCATION_3     - NOT USED */ FILL(" ",1)    /* 435 - 435 */
   /*  41 */ STRING(NAME_LAST,"X(40)")                        /* 436 - 475 */
   /*  42 */ STRING(NAME_FIRST,"X(40)")                       /* 476 - 515 */
   /*  43 */ STRING(NAME_LST2,"X(40)")                        /* 516 - 555 */  
   /*  44 */ STRING(NAME_LAST2,"X(40)")                       /* 556 - 595 */  
   /*  45 */   /* NAMEMIDDLE     - NOT USED */ FILL(" ",40)   /* 596 - 635 */
   /*  46 */   /* TITLE_ACA1     - NOT USED */ FILL(" ",4)    /* 636 - 639 */
   /*  47 */   /* TITLE_ACA2     - NOT USED */ FILL(" ",4)    /* 640 - 643 */
   /*  48 */   /* TITLE_ROYL     - NOT USED */ FILL(" ",4)    /* 644 - 647 */
   /*  49 */   /* PREFIX1        - NOT USED */ FILL(" ",4)    /* 648 - 651 */
   /*  50 */   /* PREFIX2        - NOT USED */ FILL(" ",4)    /* 652 - 655 */
   /*  51 */   /* NAME1_TEXT     - NOT USED */ FILL(" ",80)   /* 656 - 735 */
   /*  52 */   /* NICKNAME       - NOT USED */ FILL(" ",40)   /* 736 - 775 */
   /*  53 */   /* INITIALS       - NOT USED */ FILL(" ",10)   /* 776 - 785 */
   /*  54 */   /* NAMEFORMAT     - NOT USED */ FILL(" ",2)    /* 786 - 787 */
   /*  55 */   /* NAMCOUNTRY     - NOT USED */ FILL(" ",3)    /* 788 - 790 */
   /*  56 */   /* LANGU_CORR     - NOT USED */ FILL(" ",1)    /* 791 - 791 */
   /*  57 */   /* XSEXM          - NOT USED */ FILL(" ",1)    /* 792 - 792 */
   /*  58 */   /* XSEXF          - NOT USED */ FILL(" ",1)    /* 793 - 793 */
   /*  59 */   /* BIRTHPL        - NOT USED */ FILL(" ",40)   /* 794 - 833 */
   /*  60 */   /* MARST          - NOT USED */ FILL(" ",1)    /* 834 - 834 */
   /*  61 */   /* EMPLO          - NOT USED */ FILL(" ",35)   /* 835 - 869 */
   /*  62 */   /* JOBGR          - NOT USED */ FILL(" ",4)    /* 870 - 873 */
   /*  63 */ STRING(NATIO, "X(3)")                            /* 874 - 876 */  
   /*  64 */   /* CNTAX          - NOT USED */ FILL(" ",3)    /* 877 - 879 */
   /*  65 */   /* CNDSC          - NOT USED */ FILL(" ",3)    /* 880 - 882 */
   /*  66 */   /* PERSNUMBER     - NOT USED */ FILL(" ",10)   /* 883 - 892 */
   /*  67 */   /* XSEXU          - NOT USED */ FILL(" ",1)    /* 893 - 893 */
   /*  68 */   /* XUBNAME        - NOT USED */ FILL(" ",12)   /* 894 - 905 */
   /*  69 */   /* BU_LANGU       - NOT USED */ FILL(" ",1)    /* 906 - 906 */
   /*  70 */   /* BIRTHDT        - NOT USED */ FILL(" ",8)    /* 907 - 914 */
   /*  71 */   /* DEATHDT        - NOT USED */ FILL(" ",8)    /* 915 - 922 */   
   /*  72 */   /* PERNO          - NOT USED */ FILL(" ",8)    /* 923 - 930 */
   /*  73 */   /* CHILDREN       - NOT USED */ FILL(" ",2)    /* 931 - 932 */
   /*  74 */   /* MEM_HOUSE      - NOT USED */ FILL(" ",2)    /* 933 - 934 */
   /*  75 */   /* PARTGRPTYP     - NOT USED */ FILL(" ",4)    /* 935 - 938 */
   /*  76 */   /* NAME_GRP1      - NOT USED */ FILL(" ",40)   /* 939 - 978 */
   /*  77 */   /* NAME_GRP2      - NOT USED */ FILL(" ",40)   /* 979  - 1018 */
   /*  78 */ STRING(LGTYP,"X(4)")                             /* 1019 - 1022 */ 
   /*  79 */ STRING(LGNUM,"X(16)")                            /* 1023 - 1038 */  
   /*  80 */   /* INFCT          - NOT USED */ FILL(" ",30)   /* 1039 - 1068 */
   /*  81 */   /* COMRG          - NOT USED */ FILL(" ",30)   /* 1069 - 1098 */
   /*  82 */   /* CLBRG          - NOT USED */ FILL(" ",30)   /* 1099 - 1128 */
   /*  83 */   /* COPRG          - NOT USED */ FILL(" ",30)   /* 1129 - 1158 */
   /*  84 */   /* COMREG_DAT     - NOT USED */ FILL(" ",8)    /* 1159 - 1166 */
   /*  85 */   /* ADINT_ADDR     - NOT USED */ FILL(" ",10)   /* 1167 - 1176 */
   /*  86 */ STRING(ADEXT_ADDR,"X(20)")                       /* 1177 - 1196 */  
   /*  87 */ STRING(CHIND_ADDR,"X(1)")                        /* 1197 - 1197 */  
   /*  88 */ STRING(XDFADR,"X(1)")                            /* 1198 - 1198 */  
   /*  89 */   /* GUID           - NOT USED */ FILL(" ",32)   /* 1199 - 1230 */ 
   /*  90 */   /* NAME_CO        - NOT USED */ FILL(" ",40)   /* 1231 - 1270 */
   /*  91 */ STRING(CITY1,"X(40)")                            /* 1271 - 1310 */  
   /*  92 */   /* CITY2          - NOT USED */ FILL(" ",40)   /* 1311 - 1350 */
   /*  93 */   /* CITY_CODE      - NOT USED */ FILL(" ",12)   /* 1351 - 1362 */
   /*  94 */   /* CITYP_CODE     - NOT USED */ FILL(" ",8)    /* 1363 - 1370 */
   /*  95 */   /* HOME_CITY      - NOT USED */ FILL(" ",40)   /* 1371 - 1410 */
   /*  96 */   /* CITYH_CODE     - NOT USED */ FILL(" ",12)   /* 1411 - 1422 */
   /*  97 */   /* CHCKSTATUS     - NOT USED */ FILL(" ",1)    /* 1423 - 1423 */
   /*  98 */   /* REGIOGROUP     - NOT USED */ FILL(" ",8)    /* 1424 - 1431 */
   /*  99 */ STRING(POST_CODE1,"X(10)")                       /* 1432 - 1441 */  
   /* 100 */   /* POST_CODE2     - NOT USED */ FILL(" ",10)   /* 1442 - 1451 */
   /* 101 */   /* POST_CODE3     - NOT USED */ FILL(" ",10)   /* 1452 - 1461 */
   /* 102 */   /* PCODE1_EXT     - NOT USED */ FILL(" ",10)   /* 1462 - 1471 */
   /* 103 */   /* PCODE2_EXT     - NOT USED */ FILL(" ",10)   /* 1472 - 1481 */
   /* 104 */   /* PCODE3_EXT     - NOT USED */ FILL(" ",10)   /* 1482 - 1491 */
   /* 105 */   /* PO_BOX         - NOT USED */ FILL(" ",10)   /* 1492 - 1501 */
   /* 106 */   /* PO_BOX_NUM     - NOT USED */ FILL(" ",1)    /* 1502 - 1502 */
   /* 107 */   /* PO_BOX_LOC     - NOT USED */ FILL(" ",40)   /* 1503 - 1542 */
   /* 108 */   /* CITY_CODE2     - NOT USED */ FILL(" ",12)   /* 1543 - 1554 */
   /* 109 */   /* PO_BOX_REG     - NOT USED */ FILL(" ",3)    /* 1555 - 1557 */
   /* 110 */   /* PO_BOX_CTY     - NOT USED */ FILL(" ",3)    /* 1558 - 1560 */   
   /* 111 */   /* POSTALAREA     - NOT USED */ FILL(" ",15)   /* 1561 - 1575 */
   /* 112 */   /* TRANSPZONE     - NOT USED */ FILL(" ",10)   /* 1576 - 1585 */
   /* 113 */ STRING(STREET,"X(60)")                           /* 1586 - 1645 */  
   /* 114 */   /* STREETCODE     - NOT USED */ FILL(" ",12)   /* 1646 - 1657 */
   /* 115 */   /* STREETABBR     - NOT USED */ FILL(" ",2)    /* 1658 - 1659 */
   /* 116 */ STRING(HOUSE_NUM1,"X(10)")                       /* 1660 - 1669 */  
   /* 117 */   /* HOUSE_NUM2     - NOT USED */ FILL(" ",10)   /* 1670 - 1679 */
   /* 118 */   /* HOUSE_NUM3     - NOT USED */ FILL(" ",10)   /* 1680 - 1689 */
   /* 119 */   /* STR_SUPPL1     - NOT USED */ FILL(" ",40)   /* 1690 - 1729 */
   /* 120 */   /* STR_SUPPL2     - NOT USED */ FILL(" ",40)   /* 1730 - 1769 */
   /* 121 */   /* STR_SUPPL3     - NOT USED */ FILL(" ",40)   /* 1770 - 1809 */
   /* 122 */   /* LOCATION       - NOT USED */ FILL(" ",40)   /* 1810 - 1849 */
   /* 123 */   /* BUILDING       - NOT USED */ FILL(" ",20)   /* 1850 - 1869 */
   /* 124 */   /* FLOOR          - NOT USED */ FILL(" ",10)   /* 1870 - 1879 */
   /* 125 */   /* ROOMNUMBER     - NOT USED */ FILL(" ",10)   /* 1880 - 1889 */
   /* 126 */ STRING(COUNTRY,"X(3)")                           /* 1890 - 1892 */  
   /* 127 */ STRING(LANGU,"X(1)")                             /* 1893 - 1893 */  
   /* 128 */ STRING(REGION,"X(3)")                            /* 1894 - 1896 */  
   /* 129 */   /* SORT1          - NOT USED */ FILL(" ",20)   /* 1897 - 1916 */
   /* 130 */   /* SORT2          - NOT USED */ FILL(" ",20)   /* 1917 - 1936 */
   /* 131 */   /* SORT_PHN       - NOT USED */ FILL(" ",20)   /* 1937 - 1956 */
   /* 132 */   /* ADDRORIGIN     - NOT USED */ FILL(" ",4)    /* 1957 - 1960 */
   /* 133 */   /* EXTENSION1     - NOT USED */ FILL(" ",40)   /* 1961 - 2000 */
   /* 134 */   /* EXTENSION2     - NOT USED */ FILL(" ",40)   /* 2001 - 2040 */
   /* 135 */   /* TIME_ZONE      - NOT USED */ FILL(" ",6)    /* 2041 - 2046 */
   /* 136 */   /* TAXJURCODE     - NOT USED */ FILL(" ",15)   /* 2047 - 2061 */
   /* 137 */   /* ADDRESS_ID     - NOT USED */ FILL(" ",10)   /* 2062 - 2071 */
   /* 138 */   /* REMARK         - NOT USED */ FILL(" ",50)   /* 2072 - 2121 */
   /* 139 */   /* LANGU_CREA     - NOT USED */ FILL(" ",1)    /* 2122 - 2122 */
   /* 140 */   /* DEFLT_COMM     - NOT USED */ FILL(" ",3)    /* 2123 - 2125 */
   /* 141 */ STRING(CHIND_TEL,"X(1)")                         /* 2126 - 2126 */  
   /* 142 */   /* TEL_CONSNR     - NOT USED */ FILL(" ",3)    /* 2127 - 2129 */
   /* 143 */   /* TEL_CNTRY      - NOT USED */ FILL(" ",3)    /* 2130 - 2132 */
   /* 144 */ STRING(TEL_NUMBER,"X(30)")                       /* 2133 - 2162 */  
   /* 145 */   /* TEL_EXTENS     - NOT USED */ FILL(" ",10)   /* 2163 - 2172 */
   /* 146 */   /* TEL_DEFLT      - NOT USED */ FILL(" ",1)    /* 2173 - 2173 */
   /* 147 */   /* TEL_REMARK     - NOT USED */ FILL(" ",50)   /* 2174 - 2223 */
   /* 148 */   /* TEL_HOME       - NOT USED */ FILL(" ",1)    /* 2224 - 2224 */
   /* 149 */ STRING(CHIND_FAX,"X(1)")                         /* 2225 - 2225 */  
   /* 150 */   /* FAX_CONSNR     - NOT USED */ FILL(" ",3)    /* 2226 - 2228 */
   /* 151 */   /* FAX_CNTRY      - NOT USED */ FILL(" ",3)    /* 2229 - 2231 */
   /* 152 */   /* FAX_NUMBER     - NOT USED */ FILL(" ",30)   /* 2232 - 2261 */
   /* 153 */   /* FAX_EXTENS     - NOT USED */ FILL(" ",10)   /* 2262 - 2271 */
   /* 154 */   /* FAX_DEFLT      - NOT USED */ FILL(" ",1)    /* 2272 - 2272 */
   /* 155 */   /* FAX_REMARK     - NOT USED */ FILL(" ",50)   /* 2273 - 2322 */
   /* 156 */   /* FAX_HOME       - NOT USED */ FILL(" ",1)    /* 2323 - 2323 */
   /* 157 */ STRING(CHIND_TTX,"X(1)")                         /* 2324 - 2324 */
   /* 158 */   /* TTX_CONSNR     - NOT USED */ FILL(" ",3)    /* 2325 - 2327 */
   /* 159 */   /* TTX_CNTRY      - NOT USED */ FILL(" ",3)    /* 2328 - 2330 */
   /* 160 */   /* TTX_NUMBER     - NOT USED */ FILL(" ",30)   /* 2331 - 2360 */
   /* 161 */   /* TTX_DEFLT      - NOT USED */ FILL(" ",1)    /* 2361 - 2361 */
   /* 162 */   /* TTX_REMARK     - NOT USED */ FILL(" ",50)   /* 2362 - 2411 */
   /* 163 */   /* TTX_HOME       - NOT USED */ FILL(" ",1)    /* 2412 - 2412 */
   /* 164 */ STRING(CHIND_TLX,"X(1)")                         /* 2413 - 2413 */    
   /* 165 */   /* TLX_CONSNR     - NOT USED */ FILL(" ",3)    /* 2416 - 2416 */
   /* 166 */   /* TLX_CNTRY      - NOT USED */ FILL(" ",3)    /* 2419 - 2419 */
   /* 167 */   /* TLX_NUMBER     - NOT USED */ FILL(" ",30)   /* 2449 - 2449 */
   /* 168 */   /* TLX_DEFLT      - NOT USED */ FILL(" ",1)    /* 2450 - 2450 */
   /* 169 */   /* TLX_REMARK     - NOT USED */ FILL(" ",50)   /* 2451 - 2500 */
   /* 170 */   /* TLX_HOME       - NOT USED */ FILL(" ",1)    /* 2501 - 2501 */
   /* 171 */ STRING(CHIND_SMTP,"X(1)")                        /* 2502 - 2502 */  
   /* 172 */   /* SMTP_CONSNR    - NOT USED */ FILL(" ",3)    /* 2503 - 2505 */  
   /* 173 */ STRING(SMTP_ADDR,"X(241)")                       /* 2506 - 2746 */ 

   /* 253 */   /* BKVID          - NOT USED */ FILL(" ",4)    /* 2747 - 2750 */
   /* 254 */ STRING(CHIND_BANK,"X(1)")                        /* 2751 - 2751 */  
   /* 255 */ STRING(BANKS,"X(3)")                             /* 2752 - 2754 */
   /* 256 */ STRING(BANKL,"X(15)")                            /* 2755 - 2769 */  
   /* 257 */ STRING(BANKN,"X(18)")                            /* 2770 - 2787 */  
   /* 258 */ STRING(BKONT,"99")                               /* 2788 - 2789 */
   
   /* 259 */   /* BKREF          - NOT USED */ FILL(" ",20)   /* 2790 - 2809 */
   /* 260 */   /* KOINH          - NOT USED */ FILL(" ",56)   /* 2810 - 2865 */
   /* 261 */ STRING(BKEXT,"X(24)")                            /* 2866 - 2889 */
   /* 262 */   /* XEZER          - NOT USED */ FILL(" ",1)    /* 2890 - 2890 */
   /* 263 */   /* ACCNAME        - NOT USED */ FILL(" ",40)   /* 2891 - 2930 */
   /* 264 */   /* IBAN           - NOT USED */ FILL(" ",34)   /* 2931 - 2964 */
   /* 265 */   /* CCARD_ID       - NOT USED */ FILL(" ",6)    /* 2965 - 2970 */
   /* 266 */   /* CHIND_CCARD    - NOT USED */ FILL(" ",1)    /* 2971 - 2971 */
   /* 267 */   /* CCINS          - NOT USED */ FILL(" ",4)    /* 2972 - 2975 */
   /* 268 */   /* CCNUM          - NOT USED */ FILL(" ",25)   /* 2976 - 3000 */ 
   /* 269 */   /* CCDEF          - NOT USED */ FILL(" ",1)    /* 3001 - 3001 */
   /* 270 */   /* CCACCNAME      - NOT USED */ FILL(" ",40)   /* 3002 - 3041 */
   /* 271 */   /* CCNAME         - NOT USED */ FILL(" ",40)   /* 3042 - 3081 */
   /* 272 */   /* ISSBANK        - NOT USED */ FILL(" ",40)   /* 3082 - 3121 */
   /* 273 */   /* CCTYP          - NOT USED */ FILL(" ",2)    /* 3122 - 3123 */
   /* 274 */   /* CCLOCK         - NOT USED */ FILL(" ",2)    /* 3124 - 3125 */
   /* 275 */   /* DATAB          - NOT USED */ FILL(" ",8)    /* 3126 - 3133 */
   /* 276 */   /* DATBI          - NOT USED */ FILL(" ",8)    /* 3134 - 3141 */
   /* 277 */   /* AUSGDAT        - NOT USED */ FILL(" ",8)    /* 3142 - 3149 */
   /* 278 */   /* DUMMY          - NOT USED */ FILL(" ",1)    /* 3150 - 3150 */
   /* 279 */   /* TITLE_LET_TR   - NOT USED */ FILL(" ",2)    /* 3151 - 3152 */
   /* 280 */   /* GROUP_D        - NOT USED */ FILL(" ",4)    /* 3153 - 3156 */
   /* 281 */   /* PAR_REL        - NOT USED */ FILL(" ",1)    /* 3157 - 3157 */
   /* 282 */   /* VIP            - NOT USED */ FILL(" ",1)    /* 3158 - 3158 */
   /* 283 */   /* SORT_PHON      - NOT USED */ FILL(" ",20)   /* 3159 - 3178 */
   /* 284 */   /* CALENDARID     - NOT USED */ FILL(" ",2)    /* 3179 - 3180 */
   /* 285 */   /* CNTRY_COMP     - NOT USED */ FILL(" ",3)    /* 3181 - 3183 */
   /* 286 */   /* REGIO          - NOT USED */ FILL(" ",3)    /* 3184 - 3186 */
   /* 287 */   /* COMP_HEAD      - NOT USED */ FILL(" ",35)   /* 3187 - 3221 */
   /* 288 */   /* MIN_RESERV     - NOT USED */ FILL(" ",1)    /* 3222 - 3222 */
   /* 289 */   /* BAL_SH_CUR     - NOT USED */ FILL(" ",5)    /* 3223 - 3227 */
   /* 290 */   /* CAP_INCR_A     - NOT USED */ FILL(" ",21)   /* 3228 - 3248 */
   /* 291 */   /* CAP_INCR_Y     - NOT USED */ FILL(" ",4)    /* 3249 - 3252 */
   /* 292 */   /* BALANCE_FL     - NOT USED */ FILL(" ",1)    /* 3253 - 3253 */
   /* 293 */   /* BANKL_TR       - NOT USED */ FILL(" ",15)   /* 3254 - 3268 */  
   /* 294 */   /* BANKS_TR       - NOT USED */ FILL(" ",3)    /* 3268 - 3271 */
   /* 295 */   /* STATE          - NOT USED */ FILL(" ",3)    /* 3272 - 3274 */
   /* 296 */   /* PROPRTY_ST     - NOT USED */ FILL(" ",2)    /* 3275 - 3276 */
   /* 297 */   /* INCOME_CUR     - NOT USED */ FILL(" ",5)    /* 3277 - 3281 */
   /* 298 */   /* NET_INCOME     - NOT USED */ FILL(" ",21)   /* 3282 - 3302 */
   /* 299 */   /* MO_NET_INC     - NOT USED */ FILL(" ",21)   /* 3303 - 3323 */
   /* 300 */   /* NET_INC_Y      - NOT USED */ FILL(" ",4)    /* 3324 - 3327 */
   /* 301 */   /* MO_INC_M       - NOT USED */ FILL(" ",2)    /* 3328 - 3329 */
   /* 302 */   /* MO_INC_Y       - NOT USED */ FILL(" ",4)    /* 3330 - 3333 */
   /* 303 */   /* STAFF_GRP      - NOT USED */ FILL(" ",4)    /* 3334 - 3337 */
   /* 304 */   /* HR_ORG_REF     - NOT USED */ FILL(" ",1)    /* 3338 - 3338 */
   /* 305 */   /* XUBNAME_TR     - NOT USED */ FILL(" ",12)   /* 3339 - 3350 */
   /* 306 */   /* STAFF_REF      - NOT USED */ FILL(" ",1)    /* 3351 - 3351 */
   /* 307 */   /* CHIND_ED       - NOT USED */ FILL(" ",1)    /* 3352 - 3352 */
   /* 308 */   /* EMPL_SINCE     - NOT USED */ FILL(" ",10)   /* 3353 - 3362 */
   /* 309 */   /* EMPL_UNTIL     - NOT USED */ FILL(" ",10)   /* 3363 - 3372 */
   /* 310 */   /* EMPLOYMENT     - NOT USED */ FILL(" ",2)    /* 3373 - 3374 */
   /* 311 */   /* EMP_SECTOR     - NOT USED */ FILL(" ",4)    /* 3375 - 3378 */
   /* 312 */   /* EMPLOYER       - NOT USED */ FILL(" ",35)   /* 3379 - 3413 */
   /* 313 */   /* PROFESSION     - NOT USED */ FILL(" ",35)   /* 3414 - 3448 */
   /* 314 */   /* CHIND_BY       - NOT USED */ FILL(" ",1)    /* 3449 - 3449 */

   /* 399 */ STRING(ZEZAWE,"X(1)")                            /* 3450 - 3450 */  
   /* 400 */ STRING(ZAZAWE,"X(5)")                            /* 3451 - 3455 */
   /* 401 */ STRING(ZZSLCS,"X(2)")                            /* 3456 - 3457 */
   /* 402 */ FILL(" ",1)                                      /* 3458 - 3458 */           
   /* 403 */ STRING(TEL_NUMBER2,"X(30)")                      /* 3459 - 3488 */  
   SKIP.

END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
   
OUTPUT STREAM sLog CLOSE.


