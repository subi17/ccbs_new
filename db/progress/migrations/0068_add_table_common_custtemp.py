from gearbox.migrations import Migration

class AddCustTemp(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CustTemp', area='Sta_Data_128',
                       label='Customer Templates',
                       dump_name='custemp',
                       desc='Template definition for Customer creation\
')
        t.column('TemplNum', 'integer', format='>9', initial='0',
                 label='Templ',
                 column_label='Templ',
                 help='Template Number (1 .. 99)')
        t.column('TemplName', 'character', format='x(40)', initial='',
                 label='Description',
                 column_label='Description',
                 help='Description Of Template')
        t.column('CustNum', 'integer', mandatory=True, format='zzzzzz9', initial='0',
                 label='CustNo',
                 column_label='CustNo',
                 help='Number of Template Customer')
        t.column('Memo', 'character', extent=15, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo text')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('TemplName', ['Brand', 'TemplName', 'TemplNum'], area='Sta_Index_2')
        t.index('TemplNum', ['Brand', 'TemplNum'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('CustTemp')

