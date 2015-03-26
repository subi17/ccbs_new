from gearbox.migrations import Migration

class AddMedDefTrunk(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('MedDefTrunk', area='Sta_Data_256',
                       label='MedDefTrunk',
                       dump_name='meddeftr',
                       desc='CDR preprosessing file default CGR values')
        t.column('Categ', 'character', format='x(1)', initial='',
                 label='Category',
                 column_label='Category')
        t.column('DefName', 'character', format='x(12)', initial='',
                 label='Name',
                 column_label='Name')
        t.column('Range', 'character', format='x(9)', initial='',
                 column_label='Range')
        t.column('OpCode', 'character', format='x(4)', initial='',
                 label='Operator',
                 column_label='Operator')
        t.column('Type', 'character', format='x(1)', initial='',
                 column_label='Type')
        t.column('DefFrom', 'character', format='x(4)', initial='0',
                 label='From',
                 column_label='From')
        t.column('DefTo', 'character', format='x(4)', initial='0',
                 label='To',
                 column_label='To')
        t.column('Ident', 'character', initial='',
                 column_label='Ident',
                 help='Identification')
        t.column('ExCode', 'character', initial='0',
                 label='Ex-num',
                 column_label='Ex-num',
                 help='Exchange number')
        t.index('DefName', ['DefName', 'Ident', 'DefFrom', 'DefTo', 'Categ'], area='Sta_Index_2',
                unique=True)
        t.index('Ident', ['Ident', 'DefFrom', 'DefTo', 'DefName', 'Categ'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('MedDefTrunk')

