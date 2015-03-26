from gearbox.migrations import Migration

class AddCustLetter(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CustLetter', area='Sta_Data_256',
                       label='Customer Letter',
                       dump_name='custlett',
                       desc='Customer letter text')
        t.column('ChgDate', 'date', format='99-99-99',
                 label='Date',
                 column_label='Date',
                 help='Date when text was lastly modified')
        t.column('LtrText', 'character', extent=17, format='x(78)', initial='',
                 label='Text',
                 column_label='Text',
                 help='Customer letter text')
        t.column('LtrMargin', 'integer', format='>9', initial='0',
                 label='Margin',
                 column_label='Margin',
                 help='Left margin (characters)')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('Brand', ['Brand'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('CustLetter')

