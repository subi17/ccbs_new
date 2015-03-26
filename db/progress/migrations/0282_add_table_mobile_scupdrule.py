from gearbox.migrations import Migration

class AddSCUpdRule(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('SCUpdRule', area='Sta_Data_128',
                       label='Service Update Rule',
                       dump_name='scupdrul',
                       desc='Service component update rule\
')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('ServCom', 'character', format='x(12)', initial='',
                 label='Service Component',
                 column_label='Service',
                 help='Code of service component')
        t.column('OldValue', 'integer', format='>>>9', initial='0',
                 label='Old Value',
                 column_label='Old',
                 help='Old value')
        t.column('NewValue', 'integer', format='>>>9', initial='0',
                 label='New Value',
                 column_label='New',
                 help='New value')
        t.column('UpdServCom', 'character', format='x(12)', initial='',
                 label='Update Component',
                 column_label='Upd.Service',
                 help='Service component that will be updated')
        t.column('UpdValue', 'integer', format='>>>9', initial='0',
                 label='Update Value',
                 column_label='Update',
                 help='Value for service component that will be updated')
        t.column('ScPriority', 'integer', format='>>>>>>9', initial='0',
                 label='Priority',
                 column_label='Prior')
        t.column('ParamInt', 'integer', format='>>>>9', initial='0',
                 label='Integer Parameter',
                 column_label='ParamI',
                 help='Integer parameter')
        t.column('ParamChar', 'character', format='x(30)', initial='',
                 label='Character Parameter',
                 column_label='ParamC',
                 help='Character parameter')
        t.column('ParamDec', 'decimal', decimals=5, format='->>>>>9.99', initial='0',
                 label='Decimal Parameter',
                 column_label='ParamD',
                 help='Decimal parameter')
        t.index('ServCom', ['Brand', 'ServCom', 'OldValue', 'NewValue'], area='Sta_Index_3',
                primary=True)
        t.index('UpdServCom', ['Brand', 'UpdServCom'], area='Sta_Index_3')

    def down(self):
        self.drop_table('SCUpdRule')

