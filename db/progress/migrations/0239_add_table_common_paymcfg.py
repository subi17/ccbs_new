from gearbox.migrations import Migration

class AddTablePaymCfg(Migration):

    database = "common"

    def up(self):
        t = self.table('PaymCfg', area="Sta_Data_64", label="Payment File Config.", dump_name="paymcfg", desc="Configuration for payment files")
        t.column('PaymCfg', 'character', format="x(8)", initial="", help="Id of the ocr-file's origin", max_width=16, label="Origin Id", column_label="OrigId", position=2, order=10, description="used also as a log prefix")
        t.column('Origin', 'character', format="x(30)", initial="", max_width=60, label="Name", position=3, order=20, help="Name of the ocr-file's origin")
        t.column('PaymAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Acct", column_label="Acct", position=4, order=30, help="Account nbr for posting the payment")
        t.column('PaymFile', 'character', format="x(50)", initial="", max_width=100, label="Ocr-File", column_label="File", position=5, order=40, help="Name of the file containing ocr payments")
        t.column('ConvMod', 'character', format="x(12)", initial="", max_width=24, label="Conversion program", column_label="ConvMod", position=6, order=50, help="Name of the program that performs conversion (without '.p')")
        t.column('Memo', 'character', format="x(40)", initial="", max_width=80, label="Memo", position=7, order=60, help="Memo")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=8, order=70, help="Code Of Brand")
        t.index('PaymCfg', [['Brand'], ['PaymCfg']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Origin', [['Brand'], ['Origin']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PaymCfg')
