from gearbox.migrations import Migration

class AddCustPNPSer(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CustPNPSer', area='Sta_Data_256',
                       label='Customer\'s PNP Numbers',
                       dump_name='custpnp',
                       desc='A-subscribers private b-number series')
        t.column('FromBDest', 'character', mandatory=True, format='x(12)', initial='',
                 label='B-sub FROM',
                 column_label='B-sub FROM',
                 help='B-number series lower limit')
        t.column('ToBDest', 'character', mandatory=True, format='x(12)', initial='',
                 label='B-sub TO',
                 column_label='B-sub TO',
                 help='B-number series\' upper limit')
        t.column('CustNum', 'integer', format='ZZZZZZZZ9', initial='0',
                 label='CustNo',
                 column_label='CustNo',
                 help='A-subscribers customer number')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('b-nr', ['Brand', 'FromBDest', 'ToBDest', 'CustNum'], area='Sta_Index_2',
                primary=True)
        t.index('CustNum', ['Brand', 'CustNum', 'FromBDest'], area='Sta_Index_2')
        t.index('CustNum_s', ['CustNum', 'FromBDest'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CustPNPSer')

