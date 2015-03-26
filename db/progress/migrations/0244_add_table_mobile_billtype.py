from gearbox.migrations import Migration

class AddBillType(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('BillType', area='Sta_Data_128',
                       label='ObiType',
                       dump_name='obitype',
                       desc='Object Of Billing Type')
        t.column('BillType', 'character', initial='',
                 label='Billing Object',
                 column_label='Billing Object',
                 help='Type Of Billing Object')
        t.column('BTName', 'character', format='x(30)', initial='',
                 label='Billing Object Name',
                 column_label='Billing Object Name',
                 help='Name Of The Billing Object')
        t.column('Memo', 'character', extent=10, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo of Billing Object')
        t.index('BillType', ['BillType'], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('BTName', ['BTName', 'BillType'], area='Sta_Index_3')

    def down(self):
        self.drop_table('BillType')

