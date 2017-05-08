from gearbox.migrations import Migration

class AddTableMNPSub(Migration):

    database = "common"

    def up(self):
        t = self.table('MNPSub', area="Sta_Data_64", dump_name="mnpsub")
        t.column('CLI', 'character', format="x(8)", initial="", max_width=16, position=3, order=20)
        t.column('MsSeq', 'integer', format="->,>>>,>>9", initial="0", max_width=4, position=4, order=30)
        t.column('ICC', 'character', format="x(8)", initial="", max_width=16, position=5, order=40)
        t.column('NRN', 'character', format="x(8)", initial="", max_width=16, position=6, order=50)
        t.column('MNPSeq', 'integer', format="->,>>>,>>9", initial="0", max_width=4, position=7, order=60)
        t.column('PortingTime', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, position=8, order=70)
        t.column('RetentionPlatform', 'character', format="x(8)", initial="", max_width=16, label="RetentionPlatform", position=9, order=80)
        t.index('MNPSeq', [['MNPSeq']], area="Sta_Index_2", primary=True)
        t.index('CLI', [['CLI']], area="Sta_Index_2")
        t.index('MsSeq', [['MsSeq']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MNPSub')
