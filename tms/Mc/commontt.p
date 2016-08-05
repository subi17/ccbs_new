/*-----------------------------------------------------------------------------
  MODULE .......: commontt.P
  FUNCTION .....: Asiakkaan tarkat tiedot -valikko
  SOVELLUTUS ...: NN
  AUTHOR .......: PT
  CREATED ......: 12.02.1996
  changePVM ....: 05.09.02/aam "fixed disc" removed from B),
                               "Discounts by billcode" removed,
                               "Billing structure" moved from nnastt2
                  19.09.02/aam "Special rates" removed,
                               "Invoice texts" and "Create Invoice"
                               moved from nnastt2
                  28.10.02/aam payments to H 
                  13.11.02 lp  two column (added programs from nnatt2.p)
                  03.03.03/aam BillStr, old PNP and Product package removed
                  03.03.03/jp  fatime added
                  03.03.03 kl  custpnp
                  21.03.03/aam customer prices (tariff.p)
                  23.03.03 kl quit was in wrong column
                  04.04.03 kl RUN Mc/tariff, new parameter
                  15.04.03/aam deposit invoices (display and create)
                  23.05.03/aam view it-text send log (itsendlo)
                  26.06.03 kl new parameter for tariff
                  04.07.03 kl RUN Mc/tariff, new parameter
                  03.10.03/aam contract added 
                  27.10.03/aam new parameter for bitemcu
                  23.01.04/aam new layout, nnasda&nnasbt&nnpwd removed
                  23.01.04/jp  callalarm
                  10.02.04/jp  Customer scheduled
                  11.02.04/aam print infotxt (prininfo),
                               customer balances (custbalrel)
                  12.03.04/aam payment plan
                  30.03.04/aam refunds 
                  23.04.04/aam invoice type for nnlamu5
                  03.05.04/aam contacts 
                  23.08.04/aam orderid to nnlamu5
                  22.09.04/aam create FAT (creafatui)
                  12.05.05/jp  rerate
                  16.12.05/aam new layout
                  13.04.06/aam payment plan
                  24.11.06/aam terminals (orderaccessory)
                  22.03.07 kl  new param for RUN Ar/payments
                  
  Version ......: M15
  SHARED .......: INPUT: CustNum
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO.

DEF VAR menuc     AS C  EXTENT 30 FORMAT "X(35)"   NO-UNDO.
DEF VAR inv-rep   AS LO FORMAT "Invoiced/Reported" NO-UNDO.
DEF VAR liQty     AS INT                           NO-UNDO. 
DEF VAR lcName    AS CHAR                          NO-UNDO. 
DEF VAR liInvType AS INT                           NO-UNDO.

DO FOR Customer:
   FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK.
   lcName = STRING(Customer.CustNum) + " " +
            DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                             BUFFER Customer).
END. 

/* invoice type to be created */
liInvType = fCParamI("InvCreType").
IF liInvType = 0 OR liInvType = ? THEN liInvType = 3.

PAUSE 0.

DO WHILE TRUE:
   ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN Syst/ufkey. 

      DISPLAY
       " A) Invoices                       "             @ menuc[1]  
        
       " P) Address Change                 "             @ menuc[16]  
       SKIP
       
       " B) Payments                       "             @ menuc[2]  
       " Q)                                "             @ menuc[17]   
       SKIP
       
       " C)                                "             @ menuc[3]  
       " R)                                "             @ menuc[18] 
       SKIP

       " D) Payment Plans                  "             @ menuc[4]  
       " S) Send SMS                       "             @ menuc[19]  
       SKIP

       " E) Claiming History               "             @ menuc[5]  
       " T) Memo                           "             @ menuc[20] 
       SKIP

       " F)                                "             @ menuc[6]
       " U) Requests                       "             @ menuc[21]
       SKIP

       " G) Single Fees                    "             @ menuc[7] 
       " V) Terminals                      "             @ menuc[22] 
       SKIP
            
       " H) Fixed Fees                     "             @ menuc[8] 
       " W)                                "             @ menuc[23] 
       SKIP

       " I) FAT (Free Air Time)            "             @ menuc[9] 
       " X) TMT (Ticket Management Tool)   "             @ menuc[24]
       SKIP

       " J) Create FAT                     "             @ menuc[10] 
       " Y)                                "             @ menuc[25] 
       SKIP

       " K)                                "             @ menuc[11]  
       " Z)                                "             @ menuc[26]
       SKIP

       " L)                                "             @ menuc[12] 
       " 1) Sent SMS Log                   "             @ menuc[27] 
       SKIP

       " M) Marketing Info                 "             @ menuc[13] 
       " 2) Sent Texts Log                 "             @ menuc[28] 
       SKIP
       
       " N) Company Contacts               "             @ menuc[14] 
       " 3) Event Log                      "             @ menuc[29] 
       SKIP
       
       " O) Billing                        "             @ menuc[15] 
       " 4) Admin Actions                  "             @ menuc[30] 
       SKIP

   WITH OVERLAY FRAME choices NO-LABELS.
       
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
      TITLE " " + SUBSTR(lcName,1,50) + " "  CENTERED WITH COL 2 ROW 2.

   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"8,F8") > 0  THEN LEAVE.

   IF FRAME-INDEX EQ 1 THEN DO:
      RUN Mc/nnasla(iiCustNum,
                 0).
   END.

   ELSE IF FRAME-INDEX EQ 2 THEN DO:
      RUN Ar/payments (iiCustNum,0,"").
   END.

   ELSE IF FRAME-INDEX EQ 4 THEN DO:
      RUN Ar/paymplan (iiCustNum,0,0).
   END.

   ELSE IF FRAME-INDEX EQ 5 THEN DO:
      RUN Ar/claimhis (iiCustNum, 
                    0).
   END.

   ELSE IF FRAME-INDEX EQ 7 THEN DO:
      RUN Mc/bitemcu(iiCustNum,
                  "").
   END.

   ELSE IF FRAME-INDEX EQ 8 THEN DO:
      RUN Mc/nncuco(iiCustNum,"").
   END.

   ELSE IF FRAME-INDEX = 9 THEN DO:
      RUN Mm/fatime("",    
                 iiCustNum,
                 "",
                 0).
   END.

   /* create FAT */ 
   ELSE IF FRAME-INDEX EQ 10 THEN DO:
      RUN Mc/creafatui(iiCustNum,
                    0).
   END.

   ELSE IF FRAME-INDEX EQ 13 THEN DO:
      RUN Mc/custmarket (iiCustNum).
   END.
   
   ELSE IF FRAME-INDEX EQ 14 THEN DO:
      MESSAGE "Not in use"
      VIEW-AS ALERT-BOX INFORMATION.
      NEXT.
   END.
  
   ELSE IF FRAME-INDEX EQ 15 THEN DO:
      RUN Mc/custbime(iiCustNum).
   END.
   
   ELSE IF FRAME-INDEX EQ 16 THEN DO:
      RUN Mc/nnasse.p(iiCustNum,"address_chg").
   END.
   
   ELSE IF FRAME-INDEX EQ 19 THEN DO:
      MESSAGE "Not in use"
      VIEW-AS ALERT-BOX INFORMATION.
      NEXT.
   END.
 
   ELSE IF FRAME-INDEX EQ 20 THEN DO:
      RUN Mc/memotype(iiCustNum).
   END.

   ELSE IF FRAME-INDEX EQ 21 THEN DO:
      RUN Mc/custrqme(iiCustNum).
   END.

   ELSE IF FRAME-INDEX EQ 22 THEN DO:
      RUN Mm/substerminal(0,0,iiCustNum).
   END.
   
   ELSE IF FRAME-INDEX EQ 24 THEN DO:
      RUN Mm/tmrulesel(0,iiCustNum).
   END.

   ELSE IF FRAME-INDEX EQ 27 THEN DO:
      RUN Mm/callalarm(iiCustNum,"").
   END.

   ELSE IF FRAME-INDEX EQ 28 THEN DO:
      RUN Mc/itsendlo(iiCustNum, 
                   0,
                   0,
                   0).
   END.

   ELSE IF FRAME-INDEX EQ 29 THEN DO:
       RUN Mc/evbrcust(iiCustNum).
   END.

   ELSE IF FRAME-INDEX EQ 30 THEN DO:
      RUN Mc/custadme (iiCustNum).
   END.

END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.


