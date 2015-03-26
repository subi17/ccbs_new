/* generate username and password for external rpc api
   username should be same as external rpc server instance name
*/
def input param pcusername AS CHARACTER NO-UNDO. 
def input param pcpasswd AS CHARACTER NO-UNDO. 

def var salt as raw.
salt = GENERATE-PBE-SALT.

CREATE AuthUser.
assign AuthUser.username = pcusername 
       AuthUser.pwhash = SHA1-DIGEST(pcpasswd, salt)
       AuthUser.pwsalt = salt.
