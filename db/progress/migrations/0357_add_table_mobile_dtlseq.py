from gearbox.migrations import Migration

class AddTableDtlSeq(Migration):

    database = "mobile"

    def up(self):
        t = self.table('DtlSeq', area="Sta_Data_32", dump_name="dtlseq")
        t.column('SeqDate', 'date', format="99.99.99", max_width=4, label="Date", column_label="Date", position=2, order=10, help="Date")
        t.column('SeqStream', 'integer', format=">9", initial="0", max_width=4, label="Stream", position=3, order=20, help="Stream")
        t.column('SeqVal', 'integer', format="99999999", initial="0", max_width=4, label="Value", position=4, order=30, help="Value")
        t.index('SeqDate', [['SeqDate'], ['SeqStream']], area="Sta_Index_1", primary=True, unique=True)

    def down(self):
        self.drop_table('DtlSeq')
