from gearbox.migrations import Migration

class AddClosedCust(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('ClosedCust', area='Sta_Data_256',
                       label='ClosedCust',
                       dump_name='closedcu',
                       desc='Closed customers')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer number, 1 - 999999')
        t.column('Date', 'date', format='99-99-99',
                 label='ClDate',
                 column_label='ClDate',
                 help='Closing date',
                 description='Closing date')
        t.column('DateOpen', 'date', format='99-99-99',
                 label='OpDate',
                 column_label='OpDate',
                 help='Opening date',
                 description='Opening date')
        t.column('State', 'logical', initial='no',
                 label='St.',
                 column_label='St.',
                 help='Deny opening (Y/N)',
                 description='Can this customer be opened ?')
        t.column('Printed', 'logical', initial='no',
                 label='Pr.',
                 column_label='Pr.',
                 help='Is this record printed')
        t.column('Called', 'integer', format='9', initial='0',
                 label='Value',
                 column_label='Value',
                 help='Closing value, 1 = Limit, 2 = Invoice, 3 = Both')
        t.index('CustNum', ['CustNum'], area='Sta_Index_2',
                primary=True)
        t.index('Date', ['Date', 'CustNum'], area='Sta_Index_2')

    def down(self):
        self.drop_table('ClosedCust')

