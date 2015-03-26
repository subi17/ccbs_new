/* nnpura2e.i     07.12.2001/aam
*/


IF licalask <= 6 THEN DO:
    {erepgen.i}

    PUT STREAM eKirje UNFORMATTED 
    " I" MY-NL
    " I" 
    space(4)
    otsi[30] format "x(8)"                    /* Pvm     */
    space(1)
    otsi[21] format "x(8)"                     /* Al.klo  */
    space(1)
    otsi[22] format "x(18)"                    /* Soitettu numeroon */
    space(1)
    otsi[29] format "x(12)"                    /* Duration  */
    space(5)
    otsi[33] format "x(12)"                    /* Att betala     */ 
    MY-NL.

    ASSIGN licalask = licalask + 2.
END.

