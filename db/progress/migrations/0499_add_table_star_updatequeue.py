from gearbox.migrations import Migration

class AddTableUpdateQueue(Migration):

    database = "star"

    def up(self):
        t = self.table('UpdateQueue', area="Sta_Data_64", dump_name="updatequ")
        t.column('Seq1', 'integer', format=">>>>>>9", initial="0", max_width=4, position=2, order=10)
        t.column('Value1', 'character', format="x(8)", initial="", max_width=16, position=3, order=20)
        t.column('Seq2', 'integer', format=">>>>>>9", initial="0", max_width=4, position=4, order=30)
        t.column('Value2', 'character', format="x(8)", initial="", max_width=16, position=5, order=40)
        t.column('State', 'integer', format=">9", initial="0", max_width=4, position=6, order=50)
        t.column('TSCreate', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, position=7, order=60)
        t.column('TSUpdate', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, position=8, order=70)
        t.index('Seq1', [['Seq1'], ['State']], area="Sta_Index_2", primary=True)
        t.index('State', [['State']], area="Sta_Index_2")

    def down(self):
        self.drop_table('UpdateQueue')
