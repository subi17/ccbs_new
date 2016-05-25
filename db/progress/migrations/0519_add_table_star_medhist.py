from gearbox.migrations import Migration

class AddTableMedHist(Migration):

    database = "star"

    def up(self):
        t = self.table('MedHist', area="Sta_Data_256", label="MedHist", dump_name="medhist", desc="CDR preprosessor file history")
        t.column('FileName', 'character', format="x(20)", initial="", max_width=40, label="FileName", column_label="FileName", position=2, order=10, help="Name of the configuration file")
        t.column('Ident', 'character', format="x(8)", initial="", max_width=16, label="Ident", column_label="Ident", position=3, order=80, help="Identification")
        t.column('Date', 'date', format="99-99-9999", max_width=4, label="Date", column_label="Date", position=4, order=90, help="Valid from date")
        t.column('FileTime', 'character', format="x(8)", initial="", max_width=16, label="Time", column_label="Time", position=5, order=100, help="Valid from time")
        t.column('FileExt', 'integer', format=">>>9", initial="0", max_width=4, label="Extension", column_label="Extension", position=6, order=110, help="Extension")
        t.index('file', [['Ident'], ['FileExt']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Date', [['Date'], ['FileTime'], ['Ident'], ['FileName']], area="Sta_Index_2", unique=True)
        t.index('ftam-switch', [['Ident'], ['FileName'], ['Date'], ['FileTime']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('MedHist')
