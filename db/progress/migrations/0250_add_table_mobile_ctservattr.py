from gearbox.migrations import Migration

class AddCTServAttr(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('CTServAttr', area='Sta_Data_128',
                       label='CLIType Service Attributes',
                       dump_name='ctservat',
                       desc='Attributes of CLI type\'s service components\
\
')
        t.column('CTServEl', 'integer', format='>>>>>>>9', initial='0',
                 label='Element ID',
                 column_label='ID',
                 help='Unique ID of CTServEl')
        t.column('ServAttr', 'character', format='x(14)', initial='',
                 label='Service Attribute',
                 column_label='Attribute',
                 help='Attribute of a service component')
        t.column('DefValue', 'character', initial='',
                 label='Default Value',
                 column_label='Value',
                 help='Default value')
        t.column('ChgAllowed', 'logical', initial='yes',
                 label='Change Allowed',
                 column_label='Changeable',
                 help='Value can be changed on subscription level')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From Date',
                 column_label='From',
                 help='Valid from date')
        t.index('CTServAttr', ['CTServEl', 'ServAttr', ('FromDate', 'DESCENDING')], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('ServAttr', ['ServAttr', ('FromDate', 'DESCENDING')], area='Sta_Index_3')

    def down(self):
        self.drop_table('CTServAttr')

