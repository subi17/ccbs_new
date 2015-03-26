from gearbox.migrations import Migration

class AddRoamOper(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('RoamOper', area='Sta_Data_256',
                       dump_name='roamoper')
        t.column('Country', 'character', format='x(20)', initial='',
                 column_label='Country')
        t.column('CommName', 'character', format='x(12)', initial='',
                 label='Comm.Name',
                 column_label='Comm.Name')
        t.column('Name', 'character', format='x(12)', initial='',
                 column_label='Name')
        t.column('Currency', 'character', format='x(4)', initial='',
                 column_label='Currency')
        t.column('IMSI1', 'character', format='x(3)', initial='',
                 column_label='IMSI1')
        t.column('IMSI2', 'character', format='x(2)', initial='',
                 column_label='IMSI2')
        t.column('FileSeq', 'integer', format='>>>>>>9', initial='0',
                 column_label='FileSeq')
        t.column('PLMN', 'character', format='x(5)', initial='',
                 column_label='PLMN')
        t.column('Active', 'logical', initial='No',
                 column_label='Active',
                 help='Are files sent automatically?')
        t.column('TestFileSeq', 'integer', format='>>>>>>9', initial='0',
                 label='FileSeq',
                 column_label='FileSeq')
        t.column('Production', 'integer', format='9', initial='0',
                 column_label='Production')
        t.column('RoamGroup', 'character', initial='',
                 column_label='RoamGroup')
        t.column('IMSI', 'character', format='x(6)', initial='',
                 column_label='IMSI')
        t.index('CommName', ['CommName'], area='Sta_Index_1',
                primary=True)
        t.index('IMSI', ['IMSI'], area='Sta_Index_1')

    def down(self):
        self.drop_table('RoamOper')

