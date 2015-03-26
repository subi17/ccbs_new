from gearbox.migrations import Migration

class AddUpdateQueue(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('UpdateQueue', area='Sta_Data_64',
                       dump_name='updatequ')
        t.column('Seq1', 'integer', format='>>>>>>9', initial='0')
        t.column('Value1', 'character', initial='')
        t.column('Seq2', 'integer', format='>>>>>>9', initial='0')
        t.column('Value2', 'character', initial='')
        t.column('State', 'integer', format='>9', initial='0')
        t.column('TSCreate', 'decimal', decimals=5, format='99999999.99999', initial='0')
        t.column('TSUpdate', 'decimal', decimals=5, format='99999999.99999', initial='0')
        t.index('Seq1', ['Seq1', 'State'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('UpdateQueue')

