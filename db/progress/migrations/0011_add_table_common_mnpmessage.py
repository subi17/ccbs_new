from gearbox.migrations import Migration

class AddTableMNPMessage(Migration):

    database = "common"

    def up(self):
        t = self.table('MNPMessage', area="Sta_Data_64", dump_name="mnpmessa")
        t.column('CreatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=17, position=3, order=20)
        t.column('XMLMessage', 'character', format="x(8)", initial="", max_width=16, position=4, order=30)
        t.column('StatusCode', 'integer', format="->,>>>,>>9", initial="0", max_width=4, position=5, order=40)
        t.column('MNPSeq', 'integer', format="->,>>>,>>9", initial="0", max_width=4, position=6, order=50)
        t.column('Sender', 'integer', format="->,>>>,>>9", initial="0", max_width=4, position=7, order=60)
        t.column('SentTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=17, position=8, order=70)
        t.column('MessageType', 'character', format="x(8)", initial="", max_width=16, position=9, order=80)
        t.column('MsgTurn', 'integer', format=">>9", initial="0", max_width=4, position=10, order=90)
        t.index('MNPSeq', [['MNPSeq']], area="Sta_Index_2", primary=True)
        t.index('Sender', [['Sender'], ['StatusCode']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MNPMessage')
