/* --------------------------------------------------------------------------
  MODULE .......: nnxorcu.p
  FUNCTION .....: Customer data PaymFile FOR XOR
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 11.11.1998
  MODIFIED  ....: 04.01.1999 kl blanko + Size & ConnType
        09.01.1999 pt CustGroup
  Version ......: M15
  ------------------------------------------------------------------------ */

{Syst/commali.i}

DEF INPUT PARAMETER CustGroup LIKE CustGroup.CustGroup       NO-UNDO.
DEF INPUT PARAMETER asno1   LIKE Customer.CustNum       NO-UNDO.
DEF INPUT PARAMETER asno2   LIKE Customer.CustNum       NO-UNDO.
DEF INPUT PARAMETER myyja1  LIKE Customer.Salesman      NO-UNDO.
DEF INPUT PARAMETER myyja2  LIKE Customer.Salesman      NO-UNDO.
DEF INPUT PARAMETER kateg   LIKE Customer.Category       NO-UNDO.
def input parameter apvm1   as Date format "99-99-99" NO-UNDO.
def input parameter apvm2   as Date format "99-99-99" NO-UNDO.
def input parameter ppvm1   as Date format "99-99-99" NO-UNDO.
def input parameter ppvm2   as Date format "99-99-99" NO-UNDO.
DEF INPUT PARAMETER ConnType    AS lo                     NO-UNDO.
DEF INPUT PARAMETER PriceList AS c                      NO-UNDO.
DEF INPUT PARAMETER InvGroup AS c                      NO-UNDO.
DEF INPUT PARAMETER Reseller AS c                      NO-UNDO.
DEF INPUT PARAMETER exPaymFile  AS c                      NO-UNDO.

DEF NEW shared STREAM excel.

DEF VAR tab     AS c                                 NO-UNDO.
DEF VAR mynimi  AS c   NO-UNDO.
DEF VAR rsname  AS c   NO-UNDO.
DEF VAR fake    AS DA  NO-UNDO EXTENT 4.
DEF VAR pr-code AS c   NO-UNDO.
DEF VAR blanko  AS c   NO-UNDO.
DEF VAR i       AS i   NO-UNDO.
DEF VAR Size    AS c   NO-UNDO.

tab    = chr(9).
DO i = 1 TO 11:
   blanko = blanko + tab.
END.

FIND FIRST Company no-lock no-error.

message "Printing ...".
OUTPUT STREAM excel TO value(exPaymFile) page-size 0.
PUT STREAM excel UNFORMATTED
"CustNo" tab
"Customer's Name" tab
"Size"            tab
"Conn".
RUN Syst/uexskip.p(1).

FOR
    EACH Customer no-lock            where
   (if CustGroup ne "" THEN can-find(CGMember where
               CGMember.CustGroup = CustGroup  AND
               CGMember.CustNum  = Customer.CustNum)
           ELSE TRUE)                                         AND
    Customer.CustNum   >= asno1  AND
    Customer.CustNum   <= asno2  AND
    Customer.Salesman >= myyja1 AND
    Customer.Salesman <= myyja2 AND
   (if kateg ne ""         THEN Customer.Category   = kateg   ELSE TRUE) AND
   (IF apvm1 NE 1/1/1900   THEN Customer.ContrBeg >= apvm1   ELSE TRUE) AND
   (IF apvm2 NE 12/31/9999 THEN Customer.ContrBeg <= apvm2   ELSE TRUE) AND
   (IF ppvm1 NE 1/1/1900   THEN Customer.ContrEnd >= ppvm1   ELSE TRUE) AND
   (IF ppvm2 NE 12/31/9999 THEN Customer.ContrEnd <= ppvm2   ELSE TRUE) AND
   (IF ConnType    NE ?        THEN Customer.ConnType     = ConnType    ELSE TRUE) AND
   (if PriceList ne ""       THEN Customer.PriceList  = PriceList ELSE TRUE) AND
   (if InvGroup ne ""       THEN Customer.InvGroup  = InvGroup ELSE TRUE) AND
   (if Reseller ne ""       THEN Customer.Reseller  = Reseller ELSE TRUE).

      case Customer.Size:
    when "XL" then Size = "0".
    when "L"  then Size = "1".
    when "M"  then Size = "2".
    when "S"  then Size = "3".
      END.

      PUT STREAM excel UNFORMATTED
      Customer.CustNum          tab
      Customer.CustName         tab
      blanko                  tab  /* 11 empty columns */
      Size                    tab
      Customer.ConnType format "0/5".

      RUN Syst/uexskip.p(1).
END.

OUTPUT STREAM excel CLOSE.
PAUSE 0 no-message.
message "File '" + exPaymFile + "' is ready - hit ENTER !".
PAUSE no-message.

