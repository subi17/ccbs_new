from gearbox.migrations import Migration

class AddCTServEl(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('CTServEl', area='Sta_Data_128',
                       label='CLIType Service Elements',
                       dump_name='ctservel',
                       desc='Service elements of a CLI type')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('CLIType', 'character', initial='',
                 label='CLI Type',
                 column_label='CLIType',
                 help='CLI type')
        t.column('ServPac', 'character', format='x(12)', initial='',
                 label='Service Package',
                 column_label='ServPack',
                 help='Service package code')
        t.column('ServCom', 'character', format='x(12)', initial='',
                 label='Service Component',
                 column_label='Component',
                 help='Code of Service Component')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From Date',
                 column_label='From',
                 help='Valid from date')
        t.column('DefValue', 'integer', format='>>>9', initial='0',
                 label='Default Value',
                 column_label='Default',
                 help='Default value')
        t.column('ChgAllowed', 'logical', initial='yes',
                 label='Change Allowed',
                 column_label='Changeable',
                 help='Value can be changed on subscription level')
        t.column('CTServEl', 'integer', format='>>>>>>>9', initial='0',
                 label='Element ID',
                 column_label='ID',
                 help='Unique ID')
        t.column('ServType', 'integer', format='9', initial='0',
                 label='Service Type',
                 column_label='ServType',
                 help='Service type; basic / additional')
        t.column('DefParam', 'character', format='x(20)', initial='',
                 label='Default Parameter',
                 column_label='Parameter',
                 help='Default value for parameter')
        t.index('CLIType', ['Brand', 'CLIType', 'ServPac', 'ServCom', ('FromDate', 'DESCENDING')], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('CTServEl', ['CTServEl'], area='Sta_Index_3',
                unique=True)
        t.index('ServCom', ['Brand', 'ServCom', 'CLIType', ('FromDate', 'DESCENDING')], area='Sta_Index_3')

    def down(self):
        self.drop_table('CTServEl')

