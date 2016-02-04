/*------------------------------------------------------------------
  MODULE .......: NNYRYP.P
  FUNCTION .....: Yritykseen liittyvien tietojen yllApito
  APPLICATION ..: NN
  AUTHOR .......: TT
  CREATED ......: 04.12.1996
  changePVM ....: 05.01.1997 pt yr-lraja, yr-alv
                  06.05.1997 pt yr-kirj
                  14.04.1999 pt in English
                  29.04.2002 tk evenlogging added
                  05.03.2003 tk tokens
                  15.09.2003/aam brand
                  18.03.2004/aam Phone2/3, HomeLocation etc.
                  07.05.2004/aam Address2-4
                  03.08.2004/aam memo
  Version ......: M15
  ------------------------------------------------------------------*/

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'company'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCompany AS HANDLE NO-UNDO.
   lhCompany = BUFFER Company:HANDLE.
   RUN StarEventInitialize(lhCompany).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCompany).
   END.

END.


def var jno1 as int format "ZZZZZZ9".
def var jno2 as int format "ZZZZZZ9".
DEF VAR llOk AS LOG NO-UNDO. 


form
   skip(4)
   Company.CompName     label "Company name .." AT 10 FORMAT "X(40)" SKIP
   Company.Address      label "Address ......." AT 10 FORMAT "X(40)" SKIP
   Company.PostOffice   label "Post Office ..." AT 10 FORMAT "X(40)" SKIP
   Company.Phone        label "Phone 1 ......." AT 10 FORMAT "X(40)" SKIP
   Company.Phone2       label "Phone 2 ......." AT 10 FORMAT "X(40)" SKIP
   Company.Phone3       label "Phone 3 ......." AT 10 FORMAT "X(40)" SKIP
   Company.CompanyID    label "Company ID ...." AT 10 FORMAT "X(40)" SKIP
   Company.HomeLocation label "Home Location ." AT 10 FORMAT "X(40)" SKIP
   skip(1)
   Company.Address2     label "Addit. Address1" AT 10 FORMAT "X(40)" SKIP
   Company.Address3     label "Addit. Address2" AT 10 FORMAT "X(40)" SKIP
   Company.Address4     label "Addit. Address3" AT 10 FORMAT "X(40)" SKIP
   SKIP(1)
   
with title color value(ctc) " " + ynimi + " COMPANY INFORMATION "
     + string(pvm,"99-99-99") + " "
     COLOR value(cfc) ROW 1 col 1 width 80 side-labels
     FRAME yri.

cfc = "yri". RUN Syst/ufcolor.

PAUSE 0 no-message.

OLRefresh:
repeat ON ENDKEY UNDO OLRefresh, NEXT OLRefresh:

   FIND FIRST Company WHERE Company.Brand = gcBrand exclusive-lock no-error.
   IF NOT AVAILABLE Company THEN DO:
      CREATE Company.
      Company.Brand = gcBrand.
   END.

   DISPLAY 
   Company.CompName 
   Company.Address 
   Company.PostOffice 
   Company.Phone 
   Company.Phone2
   Company.Phone3
   Company.CompanyID
   Company.HomeLocation
   Company.Address2
   Company.Address3
   Company.Address4
   WITH FRAME yri.


   toimi:
   repeat:

      ASSIGN
      ufk    = 0 
      ufk[1] = (IF lcRight = "RW" THEN 91 ELSE 0)
      ufk[3] = (IF lcRight = "RW" THEN 927 ELSE 0)
      ufk[5] = (IF lcRight = "RW" THEN 15 ELSE 0) 
      ufk[8] = 8  
      ehto   = 0. 
      
      RUN Syst/ufkey.

      IF toimi = 1 AND lcRight = "RW" THEN DO:
         cfc = "yri". RUN Syst/ufcolor.
         ehto = 9. RUN Syst/ufkey.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCompany).

         UPDATE 
            Company.CompName 
            Company.Address 
            Company.PostOffice 
            Company.Phone 
            Company.Phone2
            Company.Phone3
            Company.CompanyID
            Company.HomeLocation
            Company.Address2
            Company.Address3
            Company.Address4
         WITH FRAME yri.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCompany).

         cfc = "yri". RUN Syst/ufcolor.
         PAUSE 0 no-message.
         NEXT toimi.
      END.

      ELSE IF toimi = 3 AND lcRight = "RW" THEN DO:
      
        RUN Mc/memo(INPUT 0,
                 INPUT "Company",
                 INPUT STRING(Company.UnitCode),
                 INPUT "Company").

      END.

      ELSE IF toimi = 5 AND lcRight = "RW" THEN DO:
         ASSIGN
         ynimi = CompName. /* common-alueelle */
         LEAVE OLRefresh.
      END.

      ELSE IF toimi = 8 THEN DO:
         
         llOk = FALSE.
         MESSAGE "Do You want to leave without saving ?"
         VIEW-AS ALERT-BOX
         QUESTION
         BUTTONS YES-NO
         TITLE " UNDO CHANGES "
         SET llOk.
         
         IF llOk THEN UNDO OLRefresh, LEAVE OLRefresh.
      END.
         
   END. /* toimi */
END. /* OLRefresh */

