from gearbox.migrations import Migration

class AddTablePPBatch(Migration):

    database = "common"

    def up(self):
        t = self.table('PPBatch', area="Sta_Data_256", label="PP Batch", dump_name="ppbatch", desc='''Payment plan's batches
''')
        t.column('PPlanID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Payment Plan ID", column_label="PP ID", position=2, order=10, help="Payment plan ID")
        t.column('PPBatch', 'integer', format=">9", initial="0", max_width=4, label="Batch", position=3, order=20, help="Batch number")
        t.column('DueDate', 'date', format="99-99-99", max_width=4, label="Due Date", column_label="DueDate", position=4, order=30, help="Batche's due date")
        t.column('Amount', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Amount", position=5, order=40, help="Amount of batch")
        t.column('BatchFee', 'decimal', format="->>>>>9.99", decimals=2, initial="0", max_width=17, label="Batch Fee", column_label="Fee", position=6, order=50, help="Batch fee")
        t.column('PBStatus', 'integer', format="9", initial="0", help="Status of batch", max_width=4, label="Status", position=7, order=60, description="e.g. unpaid, partly paid, paid")
        t.column('RefNum', 'character', format="x(20)", initial="", max_width=40, label="Reference Nbr", column_label="RefNum", position=8, order=70, help="Reference Number")
        t.column('PaidAmt', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Paid Amount", column_label="Paid", position=9, order=80, help="Paid amount")
        t.index('PPlanID', [['PPlanID'], ['PPBatch']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('PPBatch')
