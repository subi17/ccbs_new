from gearbox.migrations import Migration

class AddScComp(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('ScComp', area='Sta_Data_256',
                       label='ScComp',
                       dump_name='sccomp',
                       desc='\
')
        t.column('ScCode2', 'character', format='x(12)', initial='',
                 label='ComparedWith',
                 column_label='ComparedWith',
                 help='Compared With')
        t.column('ScCondition', 'logical', format='Required/Prohibited', initial='no',
                 label='Condition',
                 column_label='Condition',
                 help='What is Condition of combination ?')
        t.column('Memo', 'character', extent=5, format='x(60)', initial='',
                 column_label='Memo',
                 help='Explanation / memory field for Service Combination')
        t.column('ServCom', 'character', format='x(12)', initial='',
                 label='Comparable',
                 column_label='Comparable')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('ScCode2', ['Brand', 'ScCode2', 'ServCom'], area='Sta_Index_3')
        t.index('ServCom', ['Brand', 'ServCom', 'ScCode2'], area='Sta_Index_3',
                primary=True)

    def down(self):
        self.drop_table('ScComp')

