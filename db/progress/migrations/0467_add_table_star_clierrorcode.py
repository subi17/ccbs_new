from gearbox.migrations import Migration

class AddTableCLIErrorCode(Migration):

    database = "star"

    def up(self):
        t = self.table('CLIErrorCode', area="Schema Area", label="CLIErrorCode", dump_name="clierror", desc="Contains the errorcodes")
        t.column('ErrCode', 'integer', format="999", initial="0", help="Error code", max_width=4, label="Errorcode", column_label="Errorcode", position=2, order=10, description="Errorcodes for the responsefile")
        t.column('ErrMsg', 'character', format="X(50)", initial="", help="Error message", max_width=100, label="Errormessage", column_label="Errormessage", position=3, order=20, description="Explains the errorcode")
        t.column('Action', 'character', format="X(40)", initial="", help="Action", max_width=80, label="Action", column_label="Action", position=4, order=30, description="Action to be taken if error occurs")
        t.index('ErrCode', [['ErrCode']], area="Schema Area", primary=True, unique=True)

    def down(self):
        self.drop_table('CLIErrorCode')
