/*-----------------------------------------------------------------
 * MODULE    : editsolog.p
 * AUTHOR    : mvi
 * CREATED   : 11.10.2005
 * EDITED    : 
 * CHANGELOG : 
 *
 *-----------------------------------------------------------------*/
{commpaa.i}
{timestamp.i}

DEF VAR sid LIKE solog.solog.
DEF VAR sologi AS C NO-UNDO.
DEF VAR timesl AS C NO-UNDO FORMAT "x(20)".
DEF VAR crtime AS C NO-UNDO FORMAT "x(20)".
DEF VAR actime AS C NO-UNDO FORMAT "x(20)".
DEF VAR cotime AS C NO-UNDO FORMAT "x(20)".
DEF VAR commli AS C NO-UNDO FORMAT "x(60)".
DEF VAR timeslt AS DEC NO-UNDO.
DEF VAR createt AS DEC NO-UNDO.
DEF VAR activat AS DEC NO-UNDO.
DEF VAR complet AS DEC NO-UNDO.
DEF VAR stat LIKE solog.stat NO-UNDO.
DEF VAR statname AS C NO-UNDO.
DEF VAR cli LIKE solog.cli NO-UNDO.
DEF VAR compar AS C NO-UNDO.
DEF VAR comm AS C NO-UNDO.
DEF VAR icc AS C NO-UNDO.
DEF VAR res1 AS C NO-UNDO.
DEF VAR res2 AS C NO-UNDO.
DEF VAR res3 AS C NO-UNDO.
DEF VAR statnames AS C NO-UNDO.
statnames = "NEW,FAIL,OK,,,ONGOING,NWERR,HLR".

/* CLI-solog display counters */
DEF VAR amt AS I NO-UNDO.
DEF VAR num AS I NO-UNDO.

/* temps for general tmp use*/
DEF VAR ctmp AS C NO-UNDO.
DEF VAR itmp AS I NO-UNDO.

DEF BUFFER x FOR solog.

DEFINE TEMP-TABLE tt 
FIELD n AS I 
FIELD solog LIKE solog.solog
INDEX solog IS PRIMARY UNIQUE solog.


FORM 
   "solog id:" sid SKIP
WITH NO-LABELS TITLE "FIND SOLOG" FRAME ask.

FORM 
   "Solog id................: " 
      sologi FORMAT "x(20)" SKIP(1)
   "Timeslot for activation : " 
      timesl "(" solog.timeslottms ")" SKIP
   "Create time.............: " 
      crtime "(" solog.createdts ")" SKIP
   "Activation time.........: " 
      actime "(" solog.activationts ")" SKIP
   "Complete time...........: " 
      cotime "(" solog.completedts ")" SKIP(1)
   "Solog command...........: " 
      comm FORMAT "x(40)" SKIP
   "Solog target CLI........: " cli SKIP     
   "Target ICC..............: " icc FORMAT "x(40)"  SKIP
   "Command parameters......: " compar FORMAT "x(40)" SKIP(1)
    
   "Current / Avail for CLI :" num FORMAT ">9" "/" amt FORMAT ">9" 
   "      Status order - fail first" SKIP(1) 
   "Status..................: " stat statname SKIP
   "Response................: " res1 FORMAT "x(50)" SKIP
   "                          " res2 FORMAT "x(50)"
WITH TITLE "EDIT SOLOG" WIDTH 80 NO-LABELS FRAME sog.   

/*-----------------------------------------------------------------*/


main:
REPEAT:
   IF AVAIL solog THEN DO:
      RUN local-disp.
   END.
   ELSE DO:
      RUN local-find(?).
      NEXT.
   END.
  
      
   ASSIGN
     ufk[1] = 816  /* FIND */
     ufk[2] = 9033 /* UPDATE STATUS */
     ufk[5] = 9035 /* PREV BY CLI */
     ufk[6] = 9034 /* NEXT BY CLI */
     ufk[8] = 8. /* RETURN */
   IF num = 1 THEN ufk[5] = 0.
   IF num = amt THEN ufk[6] = 0.
   RUN ufkey.
   ASSIGN nap = keylabel(LASTKEY).

   IF lookup(nap,"f1,1") > 0 THEN DO:
      /* find */
      RUN local-find(?).
      NEXT main.
   END.
   ELSE IF lookup(nap,"f2,2") > 0 THEN DO:
      /* update status */
      UPDATE stat WITH FRAME sog TITLE "EDIT SOLOG".
      
   END.
   ELSE IF lookup(nap,"f5,5") > 0 THEN DO:
      /* prev solog */
      itmp = solog.solog.
      FIND PREV solog NO-LOCK WHERE 
                solog.cli = cli 
                NO-ERROR.
      IF NOT AVAIL solog THEN DO:
         FIND solog NO-LOCK WHERE solog.solog = itmp.
      END.          
      NEXT.
   END.
   ELSE IF lookup(nap,"f6,6") > 0 THEN DO:
      /* next solog */
      itmp = solog.solog.
      FIND NEXT solog NO-LOCK WHERE
                solog.cli = cli
                NO-ERROR.
                
      IF NOT AVAIL solog THEN DO:
         FIND solog NO-LOCK WHERE solog.solog = itmp.
      END.
      NEXT.
   END.
   
END.

/*-----------------------------------------------------------------*/
PROCEDURE local-find:
DEFINE INPUT PARAMETER id LIKE solog.solog.
findsolog:
   REPEAT:
     IF id EQ ? THEN DO:
        UPDATE 
          sid 
        WITH FRAME ask ROW 1.
     
        FIND solog NO-LOCK WHERE
           solog.solog = sid NO-ERROR.
        END.
     ELSE DO:
     
     
     END.
     IF NOT AVAIL solog THEN DO:
        MESSAGE "no solog found with id " sid 
           VIEW-AS ALERT-BOX ERROR.
        /* ask for new solog */
        NEXT findsolog.  
     END.
   
     HIDE FRAME ask.
     RUN local-newcli.
     RETURN.
   END. /* repeat */
     
END.   
/*-----------------------------------------------------------------*/
PROCEDURE local-newcli:
   /* count how many sologs are there for specified cli */
   FOR EACH tt.
      DELETE tt.
   END.
        
     amt = 0.
     FOR EACH x NO-LOCK WHERE
              x.cli = solog.cli.
        amt = amt + 1.         
        FIND tt WHERE tt.solog = x.solog NO-ERROR.
        IF NOT AVAIL tt THEN DO:
           CREATE tt.
~           tt.n = amt.
           tt.solog = x.solog.
        END.   
     END.           
END.     
/*-----------------------------------------------------------------*/
PROCEDURE local-disp:
   ASSIGN
       stat = solog.stat
       timesl = fts2hms(solog.timeslottms)
       crtime = fts2hms(solog.createdts)
       actime = fts2hms(solog.activationts)
       cotime = fts2hms(solog.completedts)
       commli = solog.commline
       cli = solog.cli
       sologi = STRING(solog.solog)
       res1 = SUBSTR(solog.response,1,50) NO-ERROR.
       res2 = SUBSTR(solog.response,51) NO-ERROR.
       
   statname = ENTRY(stat + 1,statnames,",").
   comm = ENTRY(1,commli,",").
   icc = ENTRY(3,commli,",").
   ctmp = ?. /* we must clear this.. or else it is from last solog */
   ctmp = ENTRY(4,commli,",") NO-ERROR.
   IF ctmp NE ? AND ctmp NE "" THEN DO:
      compar = SUBSTR(commli,INDEX(commli,ctmp)).
   END. 
   ELSE DO:
      compar = "".
   END.
       
   FIND tt WHERE tt.solog = solog.solog NO-ERROR.
   IF AVAIL tt THEN num = tt.n.
   DISPLAY 
      timesl
      crtime
      actime
      cotime
      solog.timeslottms
      solog.createdts
      solog.completedts
      solog.activationts
      comm
      icc
      sologi
      stat
      cli
      num 
      amt
      compar
      statname 
      res1 res2
   WITH FRAME sog ROW 1 TITLE "BROWSE Service Order Log".

END.
/*-----------------------------------------------------------------*/
   
