
/*------------------------------------------------------------------------
    File        : YCO-250-config.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon March 26 11:07:37 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

blk:
DO TRANSACTION ON ERROR UNDO BLK, LEAVE BLK
               ON STOP UNDO BLK, LEAVE BLK:

MESSAGE "This program will create records in InvText table for:" SKIP(1) 
        "  - SMS / ConPckEnd_AddLin  " SKIP  
        "  - SMS / ConPckEnd_ExtraLin" SKIP(1) 
        "in the Spanish and English languages." SKIP(1)
        "(Note: this program works only with brand 1)" SKIP(1)    
        "Do you want to continue?" 
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lCont AS LOGICAL.
IF lCont THEN 
DO:
    /* Spanish - text for additional line */     
    CREATE InvText.
    ASSIGN InvText.Brand    = "1"
           InvText.Target   = "SMS"
           InvText.KeyValue = "ConPckEnd_AddLint"
           InvText.ITNum    = NEXT-VALUE(it-seq)
           InvText.FromDate = TODAY
           InvText.ToDate   = DATE(12,31,2049)
           InvText.InvText  = 'Hola. Ya no tienes un paquete FIJO+MOVIL con nosotros por lo que se ha cancelado el descuento “Línea Adicional” de esta línea. +info en el 622.'
           InvText.Language = 1   /* Spanish */
           InvText.UseMman  = FALSE. /* Do not use message manager */
    
    MESSAGE "YCO-250 - Invtext record created for" InvText.Target "/" InvText.KeyValue " and language" InvText.Language VIEW-AS ALERT-BOX.

    /* English - text for additional line */     
    CREATE InvText.
    ASSIGN InvText.Brand    = "1"
           InvText.Target   = "SMS"
           InvText.KeyValue = "ConPckEnd_AddLint"
           InvText.ITNum    = NEXT-VALUE(it-seq)
           InvText.FromDate = TODAY
           InvText.ToDate   = DATE(12,31,2049)
           InvText.InvText  = "Hi. You don't have fixed line + mobile line package anymore, consequently, the discount of this additional line is terminated. More info calling to 622."
           InvText.Language = 5   /* English */
           InvText.UseMman  = FALSE. /* Do not use message manager */
    
    MESSAGE "YCO-250 - Invtext record created for" InvText.Target "/" InvText.KeyValue " and language" InvText.Language VIEW-AS ALERT-BOX.

    /* Spanish - text for extra line */     
    CREATE InvText.
    ASSIGN InvText.Brand    = "1"
           InvText.Target   = "SMS"
           InvText.KeyValue = "ConPckEnd_ExtraLin"
           InvText.ITNum    = NEXT-VALUE(it-seq)
           InvText.FromDate = TODAY
           InvText.ToDate   = DATE(12,31,2049)
           InvText.InvText  = "Ya no tienes un paquete FIJO+MOVIL con nosotros por lo que esta linea DUO pasa a tener un coste de 14 E/mes y se le facturara el uso de internet +info en el 622."
           InvText.Language = 1   /* Spanish */
           InvText.UseMman  = FALSE. /* Do not use message manager */
    
    MESSAGE "YCO-250 - Invtext record created for" InvText.Target "/" InvText.KeyValue " and language" InvText.Language VIEW-AS ALERT-BOX.

    /* English - text for extra line */     
    CREATE InvText.
    ASSIGN InvText.Brand    = "1"
           InvText.Target   = "SMS"
           InvText.KeyValue = "ConPckEnd_ExtraLin"
           InvText.ITNum    = NEXT-VALUE(it-seq)
           InvText.FromDate = TODAY
           InvText.ToDate   = DATE(12,31,2049)
           InvText.InvText  = "You don't have fixed line + mobile line package anymore, therefore this DUO subscription price is 14 E/month and you will pay for the internet use. More info calling to 622."
           InvText.Language = 5   /* English */
           InvText.UseMman  = FALSE. /* Do not use message manager */
    
    MESSAGE "YCO-250 - Invtext record created for" InvText.Target "/" InvText.KeyValue " and language" InvText.Language VIEW-AS ALERT-BOX.

    
END.

END.  /* BLK */
