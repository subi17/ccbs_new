from gearbox.migrations import Migration

class AddMSClass(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('MSClass', area='Sta_Data_256',
                       label='MSISDN Classes',
                       dump_name='msclass',
                       desc='\
')
        t.column('McCode', 'integer', format='zz9', initial='0',
                 label='Class',
                 column_label='Class',
                 help='Code of MSISDN Class')
        t.column('McName', 'character', format='x(40)', initial='',
                 label='Class Name',
                 column_label='Class Name',
                 help='Name of MSISDN Class')
        t.column('McAmount', 'integer', format='zz,zz9', initial='0',
                 label='Price',
                 column_label='Price',
                 help='Price of a number of this class')
        t.column('McResAmount', 'integer', format='zz,zz9', initial='0',
                 label='ResPrice',
                 column_label='ResPrice',
                 help='Monthly Reservation Fee for a MSISDN No. of this class')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('McCode', ['Brand', 'McCode', 'McName'], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('McName', ['McName', 'McCode'], area='Sta_Index_3')

    def down(self):
        self.drop_table('MSClass')

