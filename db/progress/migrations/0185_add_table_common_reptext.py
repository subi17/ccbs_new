from gearbox.migrations import Migration

class AddRepText(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('RepText', area='Sta_Data_128',
                       label='Report Texts',
                       dump_name='reptext',
                       desc='Report texts')
        t.column('TextType', 'integer', format='>9', initial='0',
                 label='InvType',
                 column_label='InvType',
                 help='Invoice Language Type')
        t.column('LinkValue', 'character', initial='',
                 label='Key',
                 column_label='Key',
                 help='Key Value')
        t.column('Language', 'integer', format='>9', initial='0',
                 column_label='Language',
                 help='Code of Language')
        t.column('RepText', 'character', format='x(35)', initial='',
                 label='Text',
                 column_label='Text')
        t.column('LinkNum', 'integer', format='->>>>>>9', initial='0',
                 label='Int',
                 column_label='Int',
                 help='Integer code value')
        t.column('LinkCode', 'character', format='x(30)', initial='',
                 label='Char',
                 column_label='Char',
                 help='Character code value')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From',
                 help='Date when text becomes effective')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='Date when text expires')
        t.index('Language', ['Brand', 'TextType', 'Language', 'LinkCode'], area='Sta_Index_2')
        t.index('LinkCode', ['Brand', 'TextType', 'LinkCode', 'Language', ('ToDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('RepText')

