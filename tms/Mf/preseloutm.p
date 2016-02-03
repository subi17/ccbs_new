/*===========================================================================
 MODULE ........: preseloutm.p
 APPLICATION ...: Export preselect transaction records manually
 TASK ..........: NN
 CREATED .......: jp 29.03.00
 CHANGED .......: 27.06.00 pt form layout fine-tuning,
                              cparams telia-opcode & PreOutFile
                  13.09.00 ht bBatch-option added
 Version .......: M15
 ============================================================================*/

DEF VAR bBatch AS LO NO-UNDO.

/* Check if started using a unix script */
bBatch = session:batch.

{Syst/commpaa.i} 
{Func/explog.i}

DEF VAR ok           AS LO NO-UNDO FORMAT "Yes/No".
DEF VAR okt          AS LO NO-UNDO FORMAT "Yes/No".
DEF VAR al           AS LO NO-UNDO INIT false.
DEF VAR coun         AS I  NO-UNDO.
DEF VAR coun2        AS I  NO-UNDO.
DEF VAR coun3        AS I  NO-UNDO.
DEF VAR TransLimit   AS I  NO-UNDO FORMAT "99999".
DEF VAR PreOutFile   AS C  NO-UNDO FORMAT "X(30)".
DEF VAR Telia-OpCode AS C  NO-UNDO.

{Func/cparam.i TransLimit  RETURN}. TransLimit   = TMSParam.IntVal.
{Func/cparam.i PreOutFile  RETURN}. PreOutFile   = TMSParam.CharVal.
{Func/cparam.i TeliaOpCode RETURN}. Telia-OpCode = TMSParam.CharVal.

IF NOT bBatch THEN DO:

   FORM 
   SKIP(1)
" Note: This program creates a Carrier Preselect Transaction File" SKIP
"       ('beställningsfil') for Telia"                           SKIP(1)
"       into directory" PreoutFile NO-LABEL                      SKIP(1)
"       Status of Carrier Preselect transactions:"               SKIP(1)
"       Ready for export:          " coun  " transactions"       SKIP
"       Today already exported:    " coun3 " transactions"       SKIP  
"       Over the Limit of " TransLimit
                                ": " coun2 " transactions"       SKIP(3)

   WITH  OVERLAY  ROW 1 WIDTH 80
   COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    " " + ynimi + "    EXPORT PRESELECTIONS " + STRING(pvm,"99.99.9999") + " "
    NO-LABELS 
    /*1 columns*/
    FRAME main.


   FORM 
   SKIP(1)
     " number of pending transactions exceed the daily Limit !"  SKIP(1)
     "    Limit is ......:" TransLimit FORMAT "zz,zz9" NO-LABEL  SKIP
     "    Excess records :" coun3      FORMAT "zz,zz9" NO-LABEL  SKIP(1)

     " Do you want to send all transactions anyway (Y/N) ?" okt  SKIP(1) 
     "   - Yes: ALL transactions are being exported regardless"  SKIP
     "          of daily Limit (may cause problems) "            SKIP
     "   - No:  Some transactions remain unexported due to the"  SKIP
     "          daily Limit (they shall be exported earliest tomorrow) "

   WITH title color value(ctc) " DAILY Limit EXCEEDED "
           color value(cfc) overlay row 3 centered no-labels frame alert.


   PAUSE 0.
   ASSIGN
   coun  = 0
   coun2 = TransLimit
   coun3 = 0.

   /* count no. of records already Sent today */
   FOR EACH Presel NO-LOCK:
      /* RepType 3 'both' generates 2 transaction records ... */
      IF Presel.SentDate = TODAY AND 
         Presel.PsType   = 3 THEN coun3 = coun3 + 2.

      /* other types generate a single record */
      IF Presel.SentDate = TODAY AND
         Presel.PsType  ne 3 THEN coun3 = coun3 + 1.

      /* is this Presel record changed after previous transmission to Telia ? */
      IF Presel.ChStamp > 
      fGetExpLog(Presel.PsSeq,"OUTPUT","PRESEL") THEN DO:
         /* we shall make one or two transactions depending on pstype */
         IF Presel.PsType = 3 then 
         coun = coun + 2.
         ELSE coun = coun + 1.
      END.
   END.

   IF (coun + coun3) >= coun2 THEN coun2 = (coun + coun3) - coun2.
   ELSE coun2 = 0.

   MAIN:
   REPEAT WITH FRAME main:

      ehto = 9. RUN ufkey.

      pause 0.
         DISPLAY
         PreOutFile
         coun
         coun2
         coun3
         TransLimit
      WITH FRAME main.

   Action:
      REPEAT WITH FRAME main:
         ASSIGN
         ufk = 0 ehto = 0
         ufk[1] = 0 
         ufk[5] = 795
         ufk[8] = 8.
         RUN ufkey.

         IF toimi = 1 THEN NEXT  main.
         IF toimi = 8 THEN LEAVE main.
         IF TOIMI = 5 THEN DO:

            ok = false.
            MESSAGE "Do You REALLY want to export (Y/N) ?" UPDATE ok.
            IF NOT ok THEN NEXT Action.

            LEAVE Action.
         END.
      END. /* Action */      

      IF coun3 >= TransLimit THEN DO WITH FRAME alert:
         PAUSE 0.
         DISP TransLimit coun3.

         UPDATE okt WITH FRAME alert.

         HIDE FRAME alert.
      END.

      /* Export preselects */
      RUN preselout(INPUT okt, 
                    INPUT Telia-OpCode).
      LEAVE main.
   END. /* MAIN */
   HIDE FRAME main NO-PAUSE.
   HIDE MESSAGE.
END. /* IF NOT bBatch */

ELSE DO: /* defaul values is used IF bBatch */
   /* Export preselects */
   RUN preselout(INPUT okt, 
                 INPUT Telia-OpCode).
END. /* IF bBatch */
