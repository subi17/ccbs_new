from gearbox.migrations import Migration

class AddRequestAction(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('RequestAction', area='Sta_Data_64',
                       label='Request Actions',
                       dump_name='requestaction',
                       desc='Request actions')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('ReqType', 'integer', format='>>9', initial='0',
                 label='Request Type',
                 column_label='Type',
                 description='Request type')
        t.column('CLIType', 'character', format='x(12)', initial='',
                 label='CLI Type',
                 column_label='CLIType',
                 help='Type of the subscription')
        t.column('ValidFrom', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Date when configuration becomes effective')
        t.column('ValidTo', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='To',
                 help='Date when configuration expires')
        t.column('ActionType', 'character', format='x(20)', initial='',
                 label='Action Type',
                 column_label='Act.Type',
                 help='Action type')
        t.column('ActionKey', 'character', format='x(12)', initial='',
                 label='Action Key',
                 column_label='Key',
                 help='Key value of the action type')
        t.column('Action', 'integer', format='>9', initial='0',
                 description='1=create,2=terminate,3=move')
        t.column('PayType', 'integer', format='9', initial='0',
                 label='Payment Type',
                 column_label='PayType',
                 help='Payment type',
                 description='postpaid, prepaid')
        t.column('RequestActionID', 'integer', format='>>>>>>>9', initial='0',
                 label='Request Action ID',
                 column_label='ID',
                 help='Request action ID')
        t.index('CLIType', ['Brand', 'CLIType', 'ReqType', ('ValidTo', 'DESCENDING')], area='Sta_Index_2')
        t.index('PayType', ['Brand', 'PayType', 'ReqType', ('ValidTo', 'DESCENDING')], area='Sta_Index_2')
        t.index('ReqType', ['Brand', 'ReqType', ('ValidTo', 'DESCENDING')], area='Sta_Index_2')
        t.index('RequestActionID', ['RequestActionID'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('RequestAction')

