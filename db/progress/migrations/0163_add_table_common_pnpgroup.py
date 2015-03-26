from gearbox.migrations import Migration

class AddPNPGroup(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PNPGroup', area='Sta_Data_64',
                       dump_name='pnpgroup')
        t.column('PNPGroup', 'character', format='x(10)', initial='',
                 label='GroupCode',
                 column_label='GroupCode')
        t.column('Name', 'character', format='x(16)', initial='',
                 column_label='Name')
        t.column('PNPSeq', 'integer', format='>>>>>>>9', initial='0',
                 label='Seq',
                 column_label='Seq',
                 help='PNP Sequence')
        t.column('dFrom', 'date', format='99-99-99',
                 label='DateFrom',
                 column_label='DateFrom',
                 help='FROM date for PNP Group validation')
        t.column('dTo', 'date', format='99-99-99',
                 label='DateTo',
                 column_label='DateTo',
                 help='TO date for PNP Group validation')
        t.column('CCN', 'integer', mandatory=True, format='zz9', initial='0',
                 column_label='CCN',
                 help='Rating CCN')
        t.column('BDest', 'character', mandatory=True, format='x(25)', initial='',
                 column_label='BDest',
                 help='B-number')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('GroupType', 'integer', format='>9', initial='0',
                 label='PNP Group Type',
                 column_label='PNPGroupType',
                 help='Group type of PNP')
        t.column('RateCCN', 'integer', format='>>9', initial='0',
                 column_label='RateCCN',
                 help='Rate CCN')
        t.column('PriceList', 'character', initial='',
                 label='Price List',
                 column_label='PList',
                 help='Code (identifier) for a Price List')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='Billing Item',
                 column_label='BillItem',
                 help='Billing item code, max 16 characters')
        t.column('Pnpgrouptupe', 'integer', format='>9', initial='0',
                 label='PnpGroup Type',
                 column_label='PnpGroup Type',
                 help='Type of pnp group')
        t.column('PNPListLimit', 'integer', format='>>9', initial='0',
                 label='Member Limit',
                 column_label='Limit',
                 help='Max quantity for members')
        t.index('CCN', ['Brand', 'CCN'], area='Sta_Index_2')
        t.index('Name', ['Brand', 'GroupType', 'Name', 'PNPGroup'], area='Sta_Index_2')
        t.index('PNPGroup', ['Brand', 'GroupType', 'PNPGroup', ('dTo', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('PNPSeq', ['Brand', 'PNPSeq'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('PNPGroup')

