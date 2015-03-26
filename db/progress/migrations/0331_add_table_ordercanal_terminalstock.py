from gearbox.migrations import Migration

class AddTerminalStock(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('TerminalStock', area='Sta_Data_64',
                       dump_name='terminal')
        t.column('BillCode', 'character', initial='',
                 column_label='BillCode')
        t.column('Amount', 'integer', format='->>>,>>9', initial='0',
                 column_label='Amount')
        t.column('Description', 'character', format='x(20)', initial='',
                 column_label='Description')
        t.column('Colour', 'character', initial='',
                 column_label='Colour')
        t.index('BillCode', ['BillCode'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('TerminalStock')

