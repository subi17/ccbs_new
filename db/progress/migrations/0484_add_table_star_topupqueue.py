from gearbox.migrations import Migration

class AddTableTopUpQueue(Migration):

    database = "star"

    def up(self):
        t = self.table('TopUpQueue', area="Sta_Data_256", dump_name="topupqueue")
        t.column('State', 'integer', format=">9", initial="0", max_width=4, position=2, order=10)
        t.column('PPRequest', 'int64', format="->,>>>,>>9", initial="0", max_width=8, position=3, order=20)
        t.column('CLI', 'character', format="x(8)", initial="", max_width=16, position=4, order=30)
        t.column('TopUpAmt', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, position=5, order=40)
        t.column('VatAmt', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, position=6, order=50)
        t.column('Date', 'date', format="99/99/99", max_width=4, position=7, order=60)
        t.column('Source', 'character', format="x(8)", initial="", max_width=16, position=8, order=70)
        t.index('State', [['State']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('TopUpQueue')
