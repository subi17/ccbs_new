
DEFINE TEMP-TABLE services
    FIELD id AS CHAR
    FIELD servcom_of_name AS CHAR
    FIELD stype AS CHAR
    INDEX gi IS PRIMARY UNIQUE id.

DEFINE TEMP-TABLE service_options
    FIELD id AS CHAR
    FIELD disporder AS INT
    FIELD limit AS DECIMAL
    INDEX gi IS PRIMARY UNIQUE id.

FUNCTION create_service RETURN LOGICAL
      ( pcServiceid AS CHAR,
        pcNameForm AS CHAR,
        pcType AS CHAR ):
    CREATE services.
    services.id = pcServiceid.
    services.servcom_of_name = pcNameForm.
    services.stype = pcType.
END FUNCTION.

DEFINE TEMP-TABLE service_options_services
    FIELD service AS CHAR
    FIELD service_option AS CHAR
    INDEX gi IS PRIMARY UNIQUE service service_option.


FUNCTION create_sso RETURN LOGICAL
      ( pcserviceid AS CHAR,
        pcserviceoption AS CHAR,
        pilimit AS DECIMAL ):
    IF NOT CAN-FIND(service_options
                    WHERE service_options.id = pcserviceoption) THEN DO:
        CREATE service_options.
        service_options.id = pcserviceoption.
        service_options.limit = pilimit.
        service_options.disporder =
                        LOOKUP(pcserviceoption, "VEA,VES,HES,PES") +
                        LOOKUP(pcserviceoption, "SMA,SMV,SMH,SMP") +
                        LOOKUP(pcserviceoption, "SALDOREM,SALDOAGR") +
                        LOOKUP(pcserviceoption, "SMS50,SMS200").
    END.
    CREATE service_options_services.
    service_options_services.service = pcserviceid.
    service_options_services.service_option = pcserviceoption.
    RETURN TRUE.
END FUNCTION.


create_service("PeriodContract", "PUHEPOTTI", "Contract").
create_sso(services.id, "pc_inactive", ?).
create_sso(services.id, "pc_lettersent", ?).
create_sso(services.id, "PUHEPOTTI", ?).
create_sso(services.id, "SAASTOPOTTI", ?).

create_service("Balance_1", "SALDOREM", "Balance").
create_sso(services.id, "off", 0.0).
create_sso(services.id, "SALDOREM", ?).

create_service("Balance_2", "SALDOAGR", "Balance").
create_sso(services.id, "off", 0.0).
create_sso(services.id, "SALDOREM", ?).
create_sso(services.id, "SALDOAGR", ?).

create_service("PhoneBlock", "-Numeroestot", "Block").
create_sso(services.id, "off", 0.0).
FOR EACH servcom
WHERE servcom.brand = "1"
  AND servcom.service = "1":
    create_sso(services.id, servcom.servcom, ?).
END.

create_service("SmsBlock", "-Tekstiviestiestot", "Block").
create_sso(services.id, "off", 0.0).
FOR EACH servcom
WHERE servcom.brand = "1"
  AND servcom.service = "2":
    create_sso(services.id, servcom.servcom, ?).
END.

create_service("AllSmsBlock", "SMR", "BlockOnOff").
create_service("AllPhoneBlock", "KLL", "BlockOnOff").
create_service("ForeignPhoneBlock", "KLU", "BlockOnOff").
create_service("RoamingBlock", "OWN", "BlockOnOff").
create_service("MMS", "MMS", "OnOff").
create_service("AnswerMachine", "PP2", "OnOff").
create_service("SecretNumber", "NUMBERINQ", "OnOff").
create_service("CallListing", "CALLSPEC", "OnOff").
create_service("RedirIfBusy", "SP2", "OnOff").
create_service("NumberSend", "AES", "OnOff").
create_service("NumberReceive", "ANN", "OnOff").
create_service("GPRS", "GPRS", "OnOff").
create_service("FriendNumbers", "PNP", "Friend").

create_service("SmsPack_1", "", "SmsPack").
create_sso(services.id, "off", 0.0).
FOR EACH servicelimit
WHERE servicelimit.validfrom < TODAY
  AND servicelimit.validto >= TODAY
  AND servicelimit.slcode = "SMSPAKETTI":
    create_sso(services.id, servicelimit.groupcode, servicelimit.InclAmt).
    services.servcom_of_name = "-" + servicelimit.slname.
END.

create_service("SmsPack_2", "", "SmsPack").
FOR EACH servicelimit
WHERE servicelimit.validfrom < TODAY
  AND servicelimit.validto >= TODAY
  AND servicelimit.slcode = "TELEKSI":
    create_sso(services.id, servicelimit.groupcode, servicelimit.InclAmt).
    services.servcom_of_name = "-" + servicelimit.slname.
END.

create_service("VoicePack_1", "PUHEPAK", "VoicePack").
FIND FIRST servicelimit WHERE servicelimit.slcode = "PUHEPAKETTI".
create_sso(services.id, "PUHEPAK", servicelimit.InclAmt).

create_service("VoicePack_2", "PUHEPAK", "VoicePack").
FIND FIRST servicelimit WHERE servicelimit.slcode = "MEGA".
create_sso(services.id, servicelimit.groupcode, servicelimit.InclAmt).

create_service("VoicePack_3", "PUHEPAK", "VoicePack").
FIND FIRST servicelimit WHERE servicelimit.slcode = "KUMPPANI".
create_sso(services.id, servicelimit.groupcode, servicelimit.InclAmt).

create_service("GPRSEXTRA", "GPRSEXTRA", "GPRSPack").
create_sso(services.id, "off", 0.0).
FOR EACH servicelimit
WHERE servicelimit.validfrom < TODAY
  AND servicelimit.validto >= TODAY
  AND servicelimit.groupcode = "GPRSEXTRA":
    create_sso(services.id, "GPRSEXTRA", servicelimit.InclAmt).
END.
