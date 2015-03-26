from gearbox.migrations import Migration

class AddCustClass(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CustClass', area='Sta_Data_256',
                       dump_name='custclas',
                       desc='\
')
        t.column('CustClass', 'integer', format='9', initial='0',
                 label='Class',
                 column_label='Class',
                 help='Customer Class (depend on avg. amount of invoices)')
        t.column('CCName', 'character', format='x(30)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of the customer class')
        t.column('Amt', 'decimal', decimals=2, format='>>>,>>9.99', initial='0',
                 column_label='Amt',
                 help='Lowest limit')
        t.column('Memo', 'character', format='X(60)', initial='',
                 column_label='Memo')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('Amt', ['Brand', 'Amt'], area='Sta_Index_2')
        t.index('CCName', ['Brand', 'CCName', 'CustClass'], area='Sta_Index_2')
        t.index('Class', ['Brand', 'CustClass', 'Amt'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('CustClass')

