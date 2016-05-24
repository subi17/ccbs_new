from gearbox.migrations import Migration

class AddTableEDRDtl(Migration):

    database = "prepedr"

    def up(self):
        t = self.table('EDRDtl', area="Dtl_Data_64", dump_name="EDRDtl")
        t.column('DateSt', 'date', format="99.99.99", max_width=4, label="CallDate", column_label="CallDate", position=2, order=10, help="Date When call started")
        t.column('DtlSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, position=3, order=20)
        t.column('Version', 'character', format="x(6)", initial="", max_width=12, label="Version", column_label="Version", position=4, order=30)
        t.column('Detail', 'character', format="x(50)", initial="", max_width=100, label="Detail", column_label="Detail", position=5, order=40)
        t.index('DtlSeq', [['DateSt', 'DESC'], ['DtlSeq']], area="Dtl_Index1", primary=True)

    def down(self):
        self.drop_table('EDRDtl')
