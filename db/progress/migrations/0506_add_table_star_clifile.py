from gearbox.migrations import Migration

class AddTableCLIFile(Migration):

    database = "star"

    def up(self):
        t = self.table('CLIFile', area="Schema Area", label="CLIFile", dump_name="clifile", desc="A headertable for the cliqueue")
        t.column('FileName', 'character', format="X(20)", initial="", help="File name", max_width=40, label="Filename", column_label="Filename", position=2, order=30, description="The name of the requestfile")
        t.column('OrderId', 'character', format="X(8)", initial="", help="Order id", max_width=16, label="OrderId", column_label="OrderId", position=3, order=10, description="The OrderId of the requestfile")
        t.column('RsId', 'character', format="X(8)", initial="", help="Reseller", max_width=16, label="ResellerId", column_label="ResellerId", position=4, order=20, description="ResellerId")
        t.column('InStamp', 'character', format="X(14)", initial="", help="Creation date and time", max_width=28, label="Increatedate_time", column_label="Increatedate_time", position=5, order=50, description="The date and time the requestfile was created by the customer")
        t.column('DtlCount', 'integer', format="->,>>>,>>9", initial="0", help="Detail count", max_width=4, label="Detailcount", column_label="Detailcount", position=6, order=40, description="The total numbers of detailrecords")
        t.column('OutStamp', 'character', format="X(14)", initial="", help="Export date and time", max_width=28, label="Outcreatedate_time", column_label="Outcreatedate_time", position=7, order=60, description="The date and time the responsefile was created by Tele1")
        t.index('orderid', [['OrderId']], area="Schema Area", primary=True)
        t.index('filename', [['FileName'], ['OrderId']], area="Schema Area")

    def down(self):
        self.drop_table('CLIFile')
