from gearbox.migrations import Migration

class AddIMEIRegister(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('IMEIRegister', area='Sta_Data_256',
                       label='IMEI Register',
                       dump_name='imeiregister',
                       desc='IMEI register')
        t.column('Brand', 'character', initial='')
        t.column('IMEI', 'character', format='x(17)', initial='',
                 help='IMEI code')
        t.column('BillCode', 'character', mandatory=True, format='x(16)', initial='',
                 label='Billing Item',
                 column_label='BillItem',
                 help='Billing item code')
        t.index('BillCode', ['Brand', 'BillCode'], area='Sta_Index_2')
        t.index('IMEI', ['Brand', 'IMEI'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('IMEIRegister')

