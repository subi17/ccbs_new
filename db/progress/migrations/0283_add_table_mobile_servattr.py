from gearbox.migrations import Migration

class AddServAttr(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('ServAttr', area='Sta_Data_64',
                       label='ServAttr',
                       dump_name='servattr',
                       desc='Service Component')
        t.column('ServAttr', 'character', format='x(14)', initial='',
                 label='Service Attribute',
                 column_label='Attribute',
                 help='Attribute of a service component')
        t.column('ServCom', 'character', format='x(12)', initial='',
                 label='Service Component',
                 column_label='Service Component',
                 help='Code of Service Component')
        t.column('SAName', 'character', format='x(60)', initial='',
                 column_label='SAttrName',
                 help='Name of Service Component attribute')
        t.column('SaParameter', 'logical', initial='no',
                 label='Parameter?',
                 column_label='Parameter?',
                 help='Does this service contain a subscriber-specific parameter ?')
        t.column('ScValueRange', 'integer', extent=2, format='>>>9', initial='0',
                 label='Range',
                 column_label='Range',
                 help='Value Range for a Service Attribute in HLR')
        t.column('ScChgable', 'logical', initial='Yes',
                 label='Changeable',
                 column_label='ChgAble',
                 help='Changeable on subscription level')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.column('FeeModel', 'character', format='x(12)', initial='',
                 label='Fee Model',
                 column_label='FeeModel',
                 help='Fee model for changing attribute value')
        t.column('DefValue', 'character', initial='',
                 label='Default Value',
                 column_label='Value',
                 help='Default value')
        t.index('SaName', ['Brand', 'ServCom', 'SAName'], area='Sta_Index_3')
        t.index('ServAttr', ['Brand', 'ServAttr', 'ServCom'], area='Sta_Index_3')
        t.index('ServCom', ['Brand', 'ServCom', 'ServAttr'], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('ServAttr')

