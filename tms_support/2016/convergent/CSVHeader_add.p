DEF TEMP-TABLE ttCSVHeader NO-UNDO LIKE CSVHeader.
FIND FIRST CSVHeader WHERE
           CSVHeader.version = "0101YF" NO-ERROR.
IF NOT AVAIL CSVHeader THEN DO:
   MESSAGE "CSVHeader not found, creating new" VIEW-AS ALERT-BOX.
   CREATE CSVHeader.
   ASSIGN CSVHeader.CSV = "<Title=Customer name><Format=string>|<Title=Source id><Format=numeric>|<Title=Source name><Format=string>|<Title=Record type><Format=string>|<Title=Format version><Format=string>|<Title=Source file name><Format=string>|<Title=Cdr sequence number><Format=numeric>|<Title=Running index><Format=numeric>|<Title=External running index><Format=string>|<Title=Handling time><Format=string>|<Title=Event type><Format=string>|<Title=Event subtype><Format=string>|<Title=Original cdr type><Format=string>|<Title=Call case number><Format=numeric>|<Title=Nocharge><Format=boolean>|<Title=Start date><Format=string>|<Title=Start time><Format=string>|<Title=Start UTC offset><Format=string>|<Title=Duration><Format=numeric>|<Title=Charged party><Format=numeric>|<Title=Subscription type><Format=numeric>|<Title=Originating address><Format=string>|<Title=Originating address type><Format=numeric>|<Title=Destination address><Format=string>|<Title=Destination address type><Format=numeric>|<Title=Call identification number><Format=numeric>|<Title=Filler><Format=spaces>"
          CSVHeader.version = "0101YF".
END.

