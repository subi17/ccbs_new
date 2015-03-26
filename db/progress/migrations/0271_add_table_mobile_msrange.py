from gearbox.migrations import Migration

class AddMSRange(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('MSRange', area='Sta_Data_256',
                       dump_name='msrange',
                       desc='MSISDN Range\
')
        t.column('CLIFrom', 'character', format='x(12)', initial='',
                 label='From',
                 column_label='From',
                 help='First MSISDN Number in Range')
        t.column('CLITo', 'character', format='x(12)', initial='',
                 label='To',
                 column_label='To',
                 help='Last MSISDN Number in Range')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer that owns this range')
        t.column('ReserveDate', 'date', format='99-99-99',
                 label='ResDate',
                 column_label='ResDate',
                 help='Date when Range was Reserved')
        t.column('ExpireDate', 'date', format='99-99-99',
                 label='ResEnds',
                 column_label='ResEnds',
                 help='Date When Reservation Expires if Not Used')
        t.column('SalesMan', 'character', initial='',
                 label='Salesman',
                 column_label='Salesman',
                 help='Salesman\'s code')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('CLIFrom', ['Brand', 'CLIFrom'], area='Sta_Index_3',
                primary=True)
        t.index('CustNum', ['Brand', 'CustNum', 'CLIFrom'], area='Sta_Index_3',
                unique=True)
        t.index('CustNum_s', ['CustNum', 'CLIFrom'], area='Sta_Index_3',
                unique=True)
        t.index('SalesMan', ['Brand', 'SalesMan', 'CustNum'], area='Sta_Index_3')

    def down(self):
        self.drop_table('MSRange')

