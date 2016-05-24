from gearbox.migrations import Migration

class AddTableCLIQueue(Migration):

    database = "star"

    def up(self):
        t = self.table('CLIQueue', area="Schema Area", label="CLIQueue", dump_name="cliqueue", desc="Contains all detailrows from the requestfile")
        t.column('RsId', 'character', format="X(8)", initial="", help="Reseller", max_width=16, label="ResellerId", column_label="ResellerId", position=2, order=20, description="ResellerId")
        t.column('OrderId', 'character', format="X(8)", initial="", help="Order id", max_width=16, label="OrderId", column_label="OrderId", position=3, order=10, description="The OrderId of the requestfile")
        t.column('CreditLimit', 'integer', format="->,>>>,>>9", initial="0", help="Credit limit", max_width=4, label="Creditlimit", column_label="Creditlimit", position=4, order=40, description="Creditlimit")
        t.column('CLI', 'character', format="X(16)", initial="", help="CLI", max_width=32, label="Cli", column_label="Cli", position=5, order=30, description="The cli number (A-number)")
        t.column('Date', 'character', format="X(8)", initial="", help="Date", max_width=16, label="Date", column_label="Date", position=6, order=90, description="A customerfield that is used for internal purposes")
        t.column('ErrCode', 'integer', format="999", initial="0", help="Error code", max_width=4, label="Errorcode", column_label="Errorcode", position=7, order=70, description="Errorcodes for the responsefile")
        t.column('Command', 'character', format="X(8)", initial="", help="Command", max_width=16, label="Command", column_label="Command", position=8, order=60, description="Add or Delete")
        t.column('Result', 'character', format="X(8)", initial="", help="Result", max_width=16, label="Result", column_label="Result", position=9, order=50, description="The result of the cli-update. Ok or Error")
        t.column('ErrMsg', 'character', format="X(50)", initial="", help="Error messate", max_width=100, label="Errormessage", column_label="Errormessage", position=10, order=80, description="Explains the errorcode ")
        t.column('ExCode', 'character', format="x(6)", initial="", max_width=12, label="Switch", column_label="Switch", position=11, order=100, help="Name of the switch this queue belongs to")
        t.index('orderid', [['OrderId'], ['CLI'], ['ExCode']], area="Schema Area", primary=True)

    def down(self):
        self.drop_table('CLIQueue')
