/*-----------------------------------------------------------------------------
  MODULE .......: mobsubmore.p
  FUNCTION .....: More Mobsub
  SOVELLUTUS ...: 
  AUTHOR .......: 
  CREATED ......: 
  changePVM ....: 07.11.03 jp contsig/stvmpwd
                  23.01.04 jp Send sms to subscription
                  23.01.04/aam call specification (mclispec)
                  11.02.04/aam print infotxt (prininfo)
                  17.03.04/tk  input parameter mobsub.custnum for callalarm
                  22.09.04/aam create fat to "M" (order confirmation removed)
                  15.04.05/aam only one parameter to mspnpgrp
                  25.05.05/mvi Changed menu H -> "Supervisor actions" 
                               Details in Copernicus Task 6737
                  05.10.05/aam fonecta (numberinq)
                  19.12.05 jp  new layout
                  17.01.06/aam solog moved to allogs
                  08.02.06/aam periodical contract, dccli
                  05.02.07 kl  Q) ppreqbr
                  08.03.07/aam lock and unlock removed
                  18.04.07/aam odi request N
                  29.05.07 kl  .p removed from RUN protop/lib/commands.p
                  27.06.07 vk  for testing purposes only
                  06.02.08 jt  new barring handling (HLR-action menu)

  Version ......: M15
  SHARED .......: INPUT: msseq
                  OUTPUT Kille
  -------------------------------------------------------------------------- */

{Syst/commali.i} 
{Syst/tmsconst.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'OrdStat'}
{Syst/eventval.i}
{Func/matrix.i}
{Func/cparam2.i}
{Func/fdss.i}
{Func/barrfunc.i}
IF llDoEvent THEN DO:

  &GLOBAL-DEFINE STAR_EVENT_USER katun
   
  {Func/lib/eventlog.i}
      
  DEFINE VARIABLE lhMobsub AS HANDLE NO-UNDO.
  lhMobsub = BUFFER Mobsub:HANDLE.
  RUN StarEventInitialize(lhMobsub).
               
  ON F12 ANYWHERE DO:
     RUN Mc/eventview2.p(lhMobsub).
   END.
                           
END.

FUNCTION fIsPermittedModule RETURNS LOGICAL  
 ( icCliType AS CHAR,
   icModule AS CHAR):

   DEFINE VARIABLE ocResult AS CHARACTER NO-UNDO. 
   
   IF fMatrixAnalyse("1",
                     "DENIED",
                     "SubsTypeFrom;Module",
                     icCliType + ";" + icModule,
                     OUTPUT ocResult) = ? THEN DO:
      RETURN TRUE.
   END.   
   MESSAGE
      SUBST("This function is not allowed with clitype &1!",icCliType)
      VIEW-AS ALERT-BOX.
   RETURN FALSE.

END FUNCTION. 

DEF  INPUT  PARAMETER  msseq  AS INT . 
DEF  OUTPUT PARAMETER   noMobile AS lo .

DEF NEW SHARED variable siirto as c .
DEF VAR menuc      AS C  EXTENT 30               NO-UNDO.
DEF VAR inv-rep    AS LO FORMAT "Inv/Rep"        NO-UNDO.
DEF VAR ok         AS LO                         NO-UNDO.
DEF VAR Fromperiod AS INT                        NO-UNDO FORMAT "999999".
DEF VAR EndPeriod  AS INT                        NO-UNDO FORMAT "999999".
DEF VAR lcUsername AS CHAR                       No-UNDO.

DEF VAR lcCmdList  AS CHAR                       NO-UNDO.
DEF VAR llDSSActive AS LOG                       NO-UNDO.
DEF VAR lcDSSBundleId AS CHAR                    NO-UNDO.
DEF VAR lcAllowedDSS2SubsType  AS CHAR           NO-UNDO.
DEF VAR lgOngoing AS LOGICAL                     NO-UNDO.
DEF VAR lrBarring AS ROWID                       NO-UNDO.
DEF VAR ldeFirstSecond AS DEC NO-UNDO. 
DEF VAR ldaDate AS DATE NO-UNDO. 
DEF VAR ldeLastSecond AS DEC NO-UNDO. 
DEF VAR llPartial AS LOG NO-UNDO.
DEF VAR llkilled AS LOG NO-UNDO.

DEF VAR lhSub AS HANDLE NO-UNDO. 

PAUSE 0.


FIND MobSub  WHERE MobSub.Msseq = msseq NO-LOCK NO-ERROR.

IF avail mobsub then do:
   FIND Customer where 
        Customer.CustNum = MobSub.CustNum no-lock no-error.
   ASSIGN lhSub = BUFFER MobSub:HANDLE.
END.
ELSE DO:
   FIND TermMobsub WHERE TermMobsub.Msseq = msseq NO-LOCK NO-ERROR.
   IF avail TermMobsub then do:
      FIND Customer where 
           Customer.CustNum = TermMobsub.CustNum no-lock no-error.
      ASSIGN lhSub = BUFFER TermMobsub:HANDLE.
   END.
   ELSE RETURN.
END.

IF Avail Customer THEN lcUserName = Func.Common:mDispCustName(BUFFER Customer).
ELSE                    lcUserName = "".


DO WHILE TRUE:
   ASSIGN 
      noMobile = (not avail mobsub) 
      ufk = 0 
      ufk[8] = 8 
      Syst.CUICommon:ehto = 3. 
   IF AVAIL MobSub AND Mobsub.msStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE} THEN DO:
      noMobile = TRUE.
      llPartial = TRUE.
      FIND TermMobsub WHERE TermMobsub.Msseq = msseq NO-LOCK NO-ERROR.
      IF avail TermMobsub then
         ASSIGN lhSub = BUFFER TermMobsub:HANDLE
         llkilled = TRUE.
   END.
   RUN Syst/ufkey.p.
 DISPLAY
 "A) Subscription active masks            " WHEN NOT noMobile @ menuc[1] 
 "P) Counters and Limits          "       @ menuc[16] SKIP

 "B) Change subscription type             " WHEN NOT noMobile  @ menuc[2] 
 "Q) Prepaid functions            "       WHEN NOT noMobile  @ menuc[17] SKIP

 "C) Change SIM/ICC                       " WHEN NOT noMobile  @ menuc[3]
 "R) Periodical Contracts         "          @ menuc[18] SKIP

 "D) Change MSISDN                        "  WHEN NOT noMobile @ menuc[4]
 "S) Timestamp History            "          @ menuc[19] SKIP

 "E) Customer Data                        " WHEN (NOT noMobile OR llPartial) 
                                               @ menuc[5]
 "T) Re-Rate Subscription       "        WHEN (NOT noMobile OR llPartial)
                                            @ menuc[20] SKIP

 "F) Order View                           " WHEN (NOT noMobile OR llPartial)
                                               @ menuc[6]
 "U) Send SMS                  "         WHEN NOT noMobile  @ menuc[21] SKIP

 "G) Single Fee                           " WHEN (NOT noMobile OR llPartial)
                                               @ menuc[7]
 "V) Third Party Service(s)      "         @ menuc[22] SKIP

 "H) Change Data Bundle                   " WHEN NOT noMobile  @ menuc[8]
 "W) Print Itemized List of Calls"          WHEN NOT noMobile  @ menuc[23] SKIP

 "I) Terminate Subscription               " WHEN (NOT noMobile OR llpartial)
                                               @ menuc[ 9]    
 "X) TMT (Ticket Management Tool)"          WHEN NOT noMobile  @ menuc[24] SKIP

 "J) FAT (Free Air Time)                  " WHEN NOT noMobile  @ menuc[10]   
 "Y) Commissions              "             WHEN NOT noMobile  @ menuc[25] SKIP
 
 "K) Create FAT                           " WHEN NOT noMobile  @ menuc[11]
 "Z) Terminals                "             WHEN NOT noMobile  @ menuc[26] SKIP
 
 "L) Discount                             "                  @ menuc[12]
 "1) Reactivate Subscription  "             WHEN (noMobile AND NOT llPartial OR
                                            (llPartial AND llkilled))
                                               @ menuc[27] SKIP

 "M)                                      " WHEN NOT noMobile  @ menuc[13]
 "2) All Logs                 "                              @ menuc[28] SKIP
 
 "N)                                      " WHEN NOT noMobile  @ menuc[14]
 "3) Supervisor Actions       "             WHEN NOT noMobile  @ menuc[29] SKIP
 
 "O) Billing                              "                  @ menuc[15]
 "4) Admin Actions "                        WHEN NOT noMobile  @ menuc[30] SKIP

   WITH OVERLAY WIDTH 78 FRAME choices NO-LABELS.
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE " " +  lhSub::CLI + " " + lcUserName 
   CENTERED WITH COL 1 ROW 3.
   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"F8") > 0  THEN LEAVE.

   IF FRAME-INDEX EQ 1 AND NOT noMobile THEN DO:
      lgOngoing = fCheckBarrStatus(MsSeq, OUTPUT lcCmdList, OUTPUT lrBarring). 
      IF lgOngoing EQ TRUE THEN DO:
         MESSAGE "This subscription has unfinished barring commands in HLR!"
         VIEW-AS ALERT-BOX.
         LEAVE.
      END.
      RUN Mm/barrbrowser1.p(MsSeq).
   END.

   ELSE IF FRAME-INDEX = 2 AND NOT noMobile THEN DO :
      IF NOT fIsPermittedModule(MobSub.CliType, "mobtypech") THEN NEXT.
      RUN Mm/mobtypech.p(msseq).
   END.
           
   ELSE IF FRAME-INDEX = 3 AND NOT noMobile THEN DO TRANSACTION:
      IF NOT fIsPermittedModule(MobSub.CliType, "simch") THEN NEXT.
       /*YPR-4777*/
      /*Operation is not allowed if fixed line provisioning is pending*/
      IF (MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} OR /*16*/ 
          MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) /*17*/ THEN
         MESSAGE "Mobile line provisioning is not complete or " +
                 "no active mobile line" VIEW-AS ALERT-BOX.
      ELSE     
         RUN Mm/simch.p(MsSeq).
   END.

   ELSE IF FRAME-INDEX = 4 AND NOT noMobile THEN DO TRANSACTION:
      IF NOT fIsPermittedModule(MobSub.CliType, "msisdnch") THEN NEXT.
      /*YPR-4776*/
      /*Operation is not allowed if fixed line provisioning is pending*/
      IF (MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} OR /*16*/
          MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) /*17*/ THEN
         MESSAGE "Mobile line provisioning is not complete or" +
                 "no active mobile line" VIEW-AS ALERT-BOX.
      ELSE 
         RUN Mm/msisdnch.p(MsSeq).
   END.
            
   ELSE IF FRAME-INDEX = 5 AND (NOT noMobile OR llPartial) THEN  DO :
      RUN Mm/persondata.p(MsSeq).
   END.
            
   ELSE IF FRAME-INDEX = 6 AND (NOT noMobile OR llPartial) THEN DO:
      FIND FIRST Order WHERE 
                 Order.Msseq = MSSeq AND
                 Order.OrderType < 2 NO-LOCK NO-ERROR.
                
      IF AVAIL order THEN RUN Mc/order.p(2,8,"",Order.OrderID).
      ELSE 
      MESSAGE
      "UNKNOWN ORDER INFORMATION"
      VIEW-AS ALERT-BOX.
   END.         
   
   ELSE IF FRAME-INDEX = 7 AND (NOT noMobile OR llPartial) THEN DO:
      IF MobSub.PayType = FALSE THEN  
              RUN Mc/bitemcu.p(MobSub.CustNum,
                            STRING(MsSeq)).
      ELSE MESSAGE "Function not allowed for Prepaid " VIEW-AS ALERT-BOX.
   END.
   
   ELSE IF FRAME-INDEX = 8 AND NOT noMobile THEN DO:
      /*YPR-4775*/
      /*Operation is not allowed if fixed line provisioning is pending*/
      IF (MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} OR /*16*/
          MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) /*17*/ THEN
         MESSAGE "Mobile line provisioning is not complete or " +
                 "no active mobile line" VIEW-AS ALERT-BOX.
         
      ELSE RUN Mm/bundle_change_ui.p (MobSub.MsSeq).
   END.
            
   ELSE IF FRAME-INDEX = 9 AND (NOT noMobile OR llPartial) AND Avail mobsub 
   THEN DO TRANSACTION:  /* KILL subscription */

      IF Mobsub.msstatus = 2 OR 
         (Mobsub.Msstatus > 10 AND 
          Mobsub.Msstatus < 15) THEN DO:
         MESSAGE
         "Kill subscription function not allowed!"
         VIEW-AS ALERT-BOX.
         LEAVE.
      END.    
      RUN Mm/deletems1.p(INPUT MsSeq).

      /* RETURN immediately IF noMobile */
      IF NOT CAN-FIND(MobSub WHERE MobSub.MsSeq = MsSeq) THEN DO:
         noMobile = TRUE.
         LEAVE.
      END.
   END.
          
   ELSE IF  FRAME-INDEX = 10 AND NOT noMobile AND Avail mobsub THEN DO :
      IF NOT fIsPermittedModule(MobSub.CliType, "fatime") THEN NEXT.
      RUN Mm/fatime.p("", 0, mobsub.cli, MobSub.MsSeq).
   END.
                                    

   ELSE IF FRAME-INDEX = 11 AND NOT noMobile AND Avail mobsub THEN DO:
      IF NOT fIsPermittedModule(MobSub.CliType, "creafatui") THEN NEXT.
      RUN Mc/creafatui.p (MobSub.CustNum,
                     MobSub.MsSeq).
   END.
 
   ELSE IF FRAME-INDEX = 12 THEN DO:
      IF NOT fIsPermittedModule(lhsub::CliType, "dpmember") THEN NEXT.
      RUN Mc/dpmember.p (0,
                      "MobSub",
                      STRING(lhsub::MsSeq)).
   END.
                         
   ELSE IF  FRAME-INDEX = 15 THEN DO :
      /* Postpaid cli */

      
      IF lhSub::PayType = FALSE THEN RUN Mm/callmenu.p(msseq).
                                ELSE RUN Mm/prcallcalc.p(msseq).
            
   END.
                        
   /* second column */
   ELSE IF  FRAME-INDEX = 16 THEN DO :
      IF NOT fIsPermittedModule(lhSub::CliType, "countermenu") THEN NEXT.
      RUN Mm/countermenu.p(msseq).
   END.

   ELSE IF FRAME-INDEX = 17 AND NOT noMobile  AND avail mobsub THEN DO:  
      IF NOT fIsPermittedModule(MobSub.CliType, "ppreqbr") THEN NEXT.
      RUN Gwy/ppreqbr.p(MobSub.MsSeq).
   END.
   
   ELSE IF FRAME-INDEX  = 18 THEN DO :
      IF NOT fIsPermittedModule(lhSub::CLIType, "dccli") THEN NEXT.
      RUN Mm/pclist.p("mobsub",MsSeq).
   END.
                                    
   ELSE IF FRAME-INDEX = 19 THEN DO:
      RUN Mm/msowner.p(msseq). 
   END.

   ELSE IF FRAME-INDEX = 20 AND (NOT noMobile OR llPartial) THEN DO :
      IF NOT fIsPermittedModule(MobSub.CliType, "dccli") THEN NEXT. 

      lcDSSBundleId = fGetActiveDSSId(INPUT MobSub.CustNum, INPUT Func.Common:mMakeTS()).
      IF lcDSSBundleId = "DSS2" THEN
         lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

      IF lcDSSBundleId = {&DSS} OR (lcDSSBundleId = "DSS2" AND
         LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0) THEN DO:
         MESSAGE "This customer has " + lcDSSBundleId +
                 " active so customer will be re-rated."
                 SKIP
                 "Do You REALLY want to re-rate the customer: " +
                 STRING(MobSub.CustNum) 
         VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE " CONFIRMATION " UPDATE ok.
         IF NOT ok THEN NEXT.
         llDSSActive = TRUE.
      END. /* IF lcDSSBundleId = {&DSS} OR (lcDSSBundleId = "DSS2" AND */

      ASSIGN 
         EndPeriod   = year(today) * 100 + Month(today)
         Fromperiod  = year(today) * 100 + Month(today).

      UPDATE FromPeriod  LABEL  "Period From....:"    SKIP
             EndPeriod   LABEL  "Period To .....:"    SKIP
      
      WITH  OVERLAY ROW 5 centered
      COLOR VALUE(Syst.CUICommon:cfc)
      TITLE COLOR VALUE(Syst.CUICommon:ctc) (IF llDSSActive THEN "RERATE CUSTOMER"
                              ELSE "RERATE SUBSCRIPTION")
      SIDE-LABELS
      FRAME rerate.

      IF NOT llDSSActive THEN DO:
        
        ASSIGN
           ldeFirstSecond = FromPeriod * 100 + 1
           ldaDate = DATE(EndPeriod MOD 100, 1, INT(EndPeriod / 100)) + 32
           ldeLastSecond = YEAR(ldaDate) * 10000 + MONTH(ldaDate) * 100 + 1.

         IF CAN-FIND(FIRST msowner NO-LOCK WHERE
                           msowner.CLI = MobSub.CLI AND
                           msowner.tsbegin < ldeLastSecond AND
                           msowner.tsend >= ldeFirstSecond AND
                           msowner.FixedNumber > "") THEN DO:
         MESSAGE "This subscription has fixed line"
                 " active so customer will be re-rated."
                 SKIP
                 "Do You REALLY want to re-rate the customer: " +
                 STRING(MobSub.CustNum) 
         VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE " CONFIRMATION " UPDATE ok.
         IF NOT ok THEN NEXT.
         llDSSActive = TRUE.
         END.
      END.
      
      IF llDSSActive THEN
         RUN Rate/rerate.p(INPUT "", MobSub.CustNum , INPUT Fromperiod, INPUT Endperiod).
      ELSE
         RUN Rate/rerate.p(INPUT MobSub.CLI, 0 , INPUT Fromperiod, INPUT Endperiod).
      
      MESSAGE 
      "Calls re-rated"
      VIEW-AS ALERT-BOX INFORMATION.
   END.

   ELSE IF FRAME-INDEX =  21 AND NOT noMobile  THEN DO:
      IF NOT fIsPermittedModule(MobSub.CliType, "mobsubsms") THEN NEXT.
      IF lcRight = "R" THEN MESSAGE "Restricted use. Not allowed!" 
                            VIEW-AS ALERT-BOX.
      ELSE IF lcRight = "RW" THEN RUN Mm/mobsubsms.p(INPUT msseq).
   END.
   
   /* Third party service */
   ELSE IF FRAME-INDEX = 22 THEN DO:
      IF NOT fIsPermittedModule(MobSub.CliType, "TPService") THEN NEXT.
      RUN Mm/tpservice_br.p(msseq).
   END.

   /* call specification */
   ELSE IF FRAME-INDEX = 23 AND NOT noMobile  THEN DO:
      IF NOT fIsPermittedModule(MobSub.CliType, "mclispec") THEN NEXT.
      RUN Mm/mclispec.p(mobsub.cli).
   END.
   
   ELSE IF FRAME-INDEX = 24 AND NOT noMobile THEN DO:
      RUN Mm/tmrulesel.p(MobSub.MsSeq,MobSub.CustNum).
   END.
   ELSE IF FRAME-INDEX = 25 AND NOT noMobile THEN DO:
      RUN Ar/cotarg.p(MobSub.MsSeq,"mobsub").
   END.
                  
   ELSE IF FRAME-INDEX = 26 AND NOT noMobile THEN
      RUN Mm/substerminal.p(MobSub.MsSeq,0,0).

   /* Reactivate the terminated Subscription */                 
   ELSE IF FRAME-INDEX = 27 AND ((noMobile OR llPartial) OR 
                                 (llPartial AND llkilled)) THEN DO:
      MESSAGE "Do You REALLY want to reactivate subscription with MSISDN: " TermMobsub.CLI 
         VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE " CONFIRMATION " UPDATE ok .
         
      IF NOT ok THEN NEXT.
      RUN Mm/reacmobsub_cui.p(INPUT TermMobsub.Msseq, INPUT katun).
   END. /* ELSE IF FRAME-INDEX = 27 AND noMobile THEN DO: */

   /* call specification */
   ELSE IF FRAME-INDEX = 28 THEN DO:
      RUN Mm/alllogs.p(msseq).
   END.

   ELSE IF FRAME-INDEX = 29 AND NOT noMobile THEN DO:  
      IF NOT fIsPermittedModule(MobSub.CliType, "mobsubsudo") THEN NEXT.
      /* Supervisor actions here */
      RUN Mm/mobsubsudo.p(MsSeq).
   END.

    
   ELSE IF FRAME-INDEX = 30 AND NOT noMobile THEN DO: 
      IF NOT fIsPermittedModule(MobSub.CliType, "adminactions") THEN NEXT.
      MESSAGE 
      "Not in Use"
      VIEW-AS ALERT-BOX.
   end.
   ELSE IF FRAME-INDEX = 31 OR FRAME-INDEX = 0 THEN LEAVE.

END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.
