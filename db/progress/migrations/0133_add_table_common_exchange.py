from gearbox.migrations import Migration

class AddTableExchange(Migration):

    database = "common"

    def up(self):
        t = self.table('Exchange', area="Sta_Data_256", label="Exchange", dump_name="exchange", desc="Exchange")
        t.column('ExCode', 'character', format="x(8)", initial="", max_width=16, label="Exchange", column_label="Exchange", position=2, order=10, help="Exchange's code")
        t.column('ExName', 'character', format="x(40)", initial="", max_width=80, label="Exchange's name", column_label="Exchange's name", position=3, order=20, help="Exchange's name")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=610, label="Memo", column_label="Memo", extent=5, position=4, order=30, help="Memo text")
        t.column('AreaCode', 'character', format="x(4)", initial="", max_width=8, label="Area Code", column_label="Area Code", position=5, order=40, help="Area Code")
        t.column('Ident', 'character', format="x(8)", initial="", max_width=16, label="Ident", column_label="Ident", position=6, order=80, help="Ident")
        t.column('ExNum', 'integer', format=">>>9", initial="0", max_width=4, label="Ex-num", column_label="Ex-num", position=7, order=90, help="Exchange number")
        t.column('Local', 'logical', format="Yes/No", initial="no", max_width=1, label="Local", column_label="Local", position=8, order=100, help="Local")
        t.column('Options', 'character', format="x(12)", initial="", max_width=24, label="Options", column_label="Options", position=9, order=110, help="Options")
        t.index('ExCode', [['ExCode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('ExName', [['ExName'], ['ExCode']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Exchange')
