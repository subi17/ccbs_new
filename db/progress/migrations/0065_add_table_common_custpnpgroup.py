from gearbox.migrations import Migration

class AddCustPNPGroup(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CustPNPGroup', area='Sta_Data_256',
                       dump_name='custpnpg')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('PnpGroup', 'character', initial='',
                 label='PnPGroup',
                 column_label='PnPGroup')
        t.column('PnPPrior', 'integer', format='>9', initial='1',
                 label='Priority',
                 column_label='Priority')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('CustNum', ['Brand', 'CustNum', 'PnPPrior', 'PnpGroup'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CustNum_s', ['CustNum', 'PnPPrior', 'PnpGroup'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('CustPNPGroup')

