INTERFACE Gwy.ModelInterface:

METHOD PUBLIC LOG create().
/*create model instance*/

METHOD PUBLIC LOG update().
/*update model instance*/

METHOD PUBLIC LOG makeOtherAction(icAction AS CHAR).
/*makes other then create/update action*/

END INTERFACE.
