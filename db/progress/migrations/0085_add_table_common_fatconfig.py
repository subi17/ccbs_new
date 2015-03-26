from gearbox.migrations import Migration

class AddFATConfig(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('FATConfig', area='Sta_Data_128',
                       label='FAT Configuration',
                       dump_name='fatconfi',
                       desc='FAT configuration\
')
        t.column('FtGrp', 'character', initial='',
                 label='FatGroup',
                 column_label='FtGrp',
                 help='Fat group')
        t.column('ConfType', 'integer', format='>9', initial='0',
                 label='Config Type',
                 column_label='Type',
                 help='Configuration type')
        t.column('ConfTarget', 'character', format='x(12)', initial='',
                 label='Configuration Target',
                 column_label='Target',
                 help='Configuration target')
        t.column('ConfRule1', 'character', format='x(30)', initial='',
                 label='Configuration Rule 1',
                 column_label='Rule1',
                 help='Configuration rule 1')
        t.column('ConfRule2', 'character', format='x(30)', initial='',
                 label='Configuration Rule 2',
                 column_label='Rule2',
                 help='Configuration rule 2')
        t.column('ConfRule3', 'character', format='x(30)', initial='',
                 label='Configuration Rule 3',
                 column_label='Rule3',
                 help='Configuration rule 3')
        t.column('ConfRule4', 'character', format='x(30)', initial='',
                 label='Configuration Rule 4',
                 column_label='Rule4',
                 help='Configuration rule 4')
        t.column('ValidFrom', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Valid from')
        t.column('ValidTo', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='To',
                 help='Valid to')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('FTGrp', ['Brand', 'FtGrp', 'ConfType', 'ConfTarget', ('ValidTo', 'DESCENDING')], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('FATConfig')

