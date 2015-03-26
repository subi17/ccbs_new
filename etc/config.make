# Project configuration (should be under RC)
MPRO                    ?= display_banner=no $(DLC)/bin/mpro -cpinternal ISO8859-15 -cpstream ISO8859-15
APPVERSION		:= 0.1
NAME			:= yoigo
PROVERSION		:= 102b
MODULES			?= tms rpc web
RPCS			:= atm dextra heat newton
PRODUCTION_SERVERS	:= rigel merak propus
FORMAT_PARAMS		:= -numdec 46 -numsep 44 -d dmy
DATABASES		:= common ordercanal mobile star prepcdr roamcdr smsc
MONTHLY_DBS		:= mcdr mcdrdtl
TERM                    := xterm
