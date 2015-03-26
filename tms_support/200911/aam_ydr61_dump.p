{testpaa.i}
katun = "ari".
{create_eventlog.i}

def var llcut as log no-undo.
def var liqty as int no-undo.
def var lceventsource as char no-undo.
def var lceventfields as char no-undo.
def var licnt as int no-undo.

find first dumpfile where dumpid = 31 no-lock.

run ifs_subscription
                                  (dumpfile.dumpid,
                                   "/store/riftp/ifs/outgoing/spool/" + 
                                       "IFS_SU_FULL_26112009.DAT",
                                   "full",
                                   0.0,
                                   "",
                                   "",
                                   output liqty,
                                   output llcut).

message liqty "dumped" skip
        return-value
        view-as alert-box.