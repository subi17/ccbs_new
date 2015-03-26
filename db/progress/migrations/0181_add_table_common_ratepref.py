from gearbox.migrations import Migration

class AddRatePref(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('RatePref', area='Sta_Data_256',
                       dump_name='ratepref')
        t.column('Prefix', 'character', format='x(5)', initial='',
                 column_label='Prefix',
                 help='Operator prefix where price list is attached to')
        t.column('DialType', 'integer', format='>9', initial='0',
                 label='Dialling Type',
                 column_label='DialType')
        t.column('RatePref', 'character', format='x(5)', initial='',
                 label='Rating Prefix',
                 column_label='RatePref',
                 help='Prefix that is used for rating')
        t.column('CustRate', 'logical', initial='no',
                 column_label='CustRate',
                 help='Are customer related rates allowed')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('DialType', ['Brand', 'DialType', 'Prefix'], area='Sta_Index_2')
        t.index('Prefix', ['Brand', 'Prefix', 'DialType'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('RatePref')

